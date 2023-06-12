unit Builder.Services.Debug;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Threading,
  System.SyncObjs,
  Builder.Chain,
  Builder.Types,
  Builder.Services,
  Builder.Model.Project,
  Builder.Model.Environment,
  BaseProtocol,
  BaseProtocol.Json,
  BaseProtocol.Types,
  BaseProtocol.Events,
  BaseProtocol.Requests,
  BaseProtocol.Client,
  BaseProtocol.Client.Socket;

type
  TDebugService = class(TInterfacedObject, IDebugServices)
  private
    //Models
    FProjectModule: TProjectModel;
    FEnvironmentModel: TEnvironmentModel;
    //Services
    FProjectServices: IProjectServices;
    FAppServices: IAppServices;
    FAdbServices: IAdbServices;
    //DAP Debugger
    FDebugger: TBaseProtocolClientSocket;
    //Subscriptions
    FInitializedEvent: IUnsubscribable;
    FStoppedEvent: IUnsubscribable;
    FContinuedEvent: IUnsubscribable;
    FTerminatedEvent: IUnsubscribable;
    //Status
    FConnectionStatus: TDebuggerConnectionStatus;
    FStopped: boolean;
    FCurrentStoppedThreadId: integer;
    //We are connected to the debugger, but debugger doesn't proceed with process execution
    //or we ordered disconnection, but debugger doesn't confirm it.
    FMonitoringDebugger: boolean;
    FFrozenDebuggerMonitor: System.Classes.TThread;
    function GetConnectionStatus: TDebuggerConnectionStatus;
    function GetIsDebugging(): boolean;

    procedure InternalConnect(const ATimeOut: Int64);
    procedure StartSession(const AHost: string; const APort: integer;
      const ATimeOut: Int64);
    procedure StopSession();

    function AskForFrozenDebuggerAction(): TDebuggerConnectionFrozenAction;
    function AskForFrozenDebuggerButReconnectBeforeAction(): TDebuggerConnectionFrozenAction;
    function AskForFrozenDebuggerButWaitForRunningAppAction(): TDebuggerConnectionFrozenAction;
    procedure CheckForGhostDebugger();

    procedure MonitorFrozenDebugger(const ATriggerStatus,
      ALeaveStatus: TDebuggerConnectionStatus; const ATimeOut: integer = 10;
      const AAction: TFunc<TDebuggerConnectionFrozenAction> = nil);
    procedure StartFrozenDebuggerMonitor();
    procedure StopFrozenDebuggerMonitor();

    procedure DoStart(const AHost: string; const APort: integer; const ATimeOut: Int64);
    procedure Reconnect(const ATimeOut: Int64 = 5000);
    procedure ForceDisconnection();

    function IsAppRunning(): boolean;
  private
    //Events subscriptions
    procedure SubscribeToEvents();
    procedure UnsubscribeAll();
    //Debug handlers
    procedure InitializeRequest();
    procedure OnInitializeResponse();
    procedure OnInitializedEvent();
    procedure AttachRequest();
    procedure OnAttachResponse();
    procedure OnLastConfiguration();
    procedure ConfigurationDoneRequest();
    procedure OnConfigurationDoneResponse();
  public
    constructor Create();
    destructor Destroy(); override;

    procedure Start(const AHost: string; const APort: integer; const ATimeOut: Int64 = 60);
    procedure Pause();
    procedure Stop();
    procedure StepIn();
    procedure StepOver();
    procedure StepOut();
    procedure Continue();

    function CanStart(): boolean;
    function CanPause(): boolean;
    function CanStop(): boolean;
    function CanStepIn(): boolean;
    function CanStepOver(): boolean;
    function CanStepOut(): boolean;
    function CanContinue(): boolean;
  end;

implementation

uses
  System.DateUtils, System.Net.Socket, System.IOUtils,
  Builder.Exception,
  Builder.Storage.Default,
  Builder.Storage.Environment,
  Builder.Services.Factory;

{ TDebugService }

constructor TDebugService.Create;
begin
  inherited;
  FStopped := false;
  FCurrentStoppedThreadId := 0;
  FConnectionStatus := TDebuggerConnectionStatus.OutOfWork;
  FProjectServices := TServiceSimpleFactory.CreateProject();
  FAppServices := TServiceSimpleFactory.CreateApp();
  FAdbServices := TServiceSimpleFactory.CreateAdb();
  TEventsRegistration.RegisterAll();
  TRequestsRegistration.RegisterAll();
  FDebugger := TBaseProtocolClientSocket.Create();
  StartFrozenDebuggerMonitor();
  SubscribeToEvents();
end;

destructor TDebugService.Destroy;
begin
  UnsubscribeAll();
  StopFrozenDebuggerMonitor;
  FDebugger.Free();
  TEventsRegistration.UnregisterAll();
  TRequestsRegistration.UnregisterAll();
  inherited;
end;

procedure TDebugService.DoStart(const AHost: string; const APort: integer;
  const ATimeOut: Int64);
begin
  StartSession(AHost, APort, ATimeOut);
  if FDebugger.Active then
    InitializeRequest();
end;

function TDebugService.GetConnectionStatus: TDebuggerConnectionStatus;
begin
  Result := FConnectionStatus;
end;

function TDebugService.GetIsDebugging: boolean;
begin
  Result := (FConnectionStatus in [
    TDebuggerConnectionStatus.Connecting,
    TDebuggerConnectionStatus.Started,
    TDebuggerConnectionStatus.Disconnecting]);
end;

procedure TDebugService.InternalConnect(const ATimeOut: Int64);
begin
  var LTimeStarted := Now();
  var LRetry := true;
  while LRetry do begin
    try
      FDebugger.Active := true;
      LRetry := false;
    except
      on E: ESocketError do begin
        LRetry := false;
        if (E.Code = 258) then { TODO : Let's make it multiplat with a const }
          LRetry := (SecondsBetween(Now(), LTimeStarted) < ATimeOut);

        if not LRetry then
          raise;
      end;
      else
        raise;
    end;

    if LRetry then
      Sleep(100);
  end;
end;

function TDebugService.IsAppRunning: boolean;
begin
  Result := FAppServices.IsAppRunning(
    FProjectModule, FEnvironmentModel, FAdbServices.ActiveDevice);
end;

procedure TDebugService.StartSession(const AHost: string; const APort: integer;
  const ATimeOut: Int64);
begin
  try
    FDebugger.Active := false;
  except
    //
  end;
  FDebugger.Host := AHost;
  FDebugger.Port := APort;

  //No refire on reconnection
  if (FConnectionStatus <> TDebuggerConnectionStatus.Connecting) then
    TGlobalBuilderChain.BroadcastEvent(TDebugSessionStartedEvent.Create(FDebugger));

  FConnectionStatus := TDebuggerConnectionStatus.Connecting;
  try
    InternalConnect(ATimeOut);
  except
    on E: Exception do begin
      ForceDisconnection();
      raise;
    end;
  end;
end;

procedure TDebugService.StopSession;
begin
  if (FConnectionStatus <> TDebuggerConnectionStatus.Disconnecting) then
    raise EDebuggerStillConnected.Create('Debugger is still connected.');

  FConnectionStatus := TDebuggerConnectionStatus.Stopped;
  TGlobalBuilderChain.BroadcastEventAsync(TDebugSessionStoppedEvent.Create(FDebugger));
  FDebugger.Active := false;
end;

function TDebugService.CanStart(): boolean;
begin
  Result := not GetIsDebugging();
end;

function TDebugService.CanPause(): boolean;
begin
  Result := (FConnectionStatus = TDebuggerConnectionStatus.Started) and not FStopped;
end;

function TDebugService.CanStop(): boolean;
begin
  Result := (FConnectionStatus = TDebuggerConnectionStatus.Started);
end;

procedure TDebugService.CheckForGhostDebugger;
begin
  if (FConnectionStatus = TDebuggerConnectionStatus.Started) and not IsAppRunning() then
    ForceDisconnection();
end;

function TDebugService.CanStepIn(): boolean;
begin
  Result := GetIsDebugging() and FStopped;
end;

function TDebugService.CanStepOver(): boolean;
begin
  Result := GetIsDebugging() and FStopped;
end;

function TDebugService.CanStepOut(): boolean;
begin
  Result := GetIsDebugging() and FStopped;
end;

function TDebugService.CanContinue(): boolean;
begin
  Result := GetIsDebugging() and FStopped;
end;

procedure TDebugService.Start(const AHost: string; const APort: integer; const ATimeOut: Int64);
begin
  if not (FConnectionStatus in [TDebuggerConnectionStatus.OutOfWork, TDebuggerConnectionStatus.Stopped]) then
    raise EDebuggerIsBusy.Create('Debugger is busy');

  FProjectModule := FProjectServices.GetActiveProject();
  var LEnvironmentStorage := TDefaultStorage<TEnvironmentModel>.Make();
  LEnvironmentStorage.LoadModel(FEnvironmentModel);

  DoStart(AHost, APort, ATimeOut);
end;

procedure TDebugService.Stop;
begin
  if (FConnectionStatus <> TDebuggerConnectionStatus.Started) then
    raise EDebuggerNotStarted.Create('Debugger is not started.');

  FConnectionStatus := TDebuggerConnectionStatus.Disconnecting;

  var LStop := TTerminateRequest.Create();
  try
    FDebugger.SendRequest<TTerminateResponse>(
      LStop,
      procedure(const AArg: TTerminateResponse)
      begin

      end,
      procedure(const AArg: TResponseMessage)
      begin

      end
    );
  finally
    LStop.Free();
  end;
end;

procedure TDebugService.Continue;
begin
  var LContinue := TContinueRequest.Create();
  try
    LContinue.Arguments.SingleThread := false;

    FDebugger.SendRequest<TContinueResponse>(
      LContinue,
      procedure(const AArg: TContinueResponse)
      begin

      end,
      procedure(const AArg: TResponseMessage)
      begin

      end
    );
  finally
    LContinue.Free();
  end;
end;

procedure TDebugService.Pause;
begin
  var LPause := TPauseRequest.Create();
  try
    FDebugger.SendRequest<TPauseResponse>(
      LPause,
      procedure(const AArg: TPauseResponse)
      begin

      end,
      procedure(const AArg: TResponseMessage)
      begin

      end
    );
  finally
    LPause.Free();
  end;
end;

procedure TDebugService.StepIn;
begin
  var LStepIn := TStepInRequest.Create();
  try
    if FCurrentStoppedThreadId = 0 then
      LStepIn.Arguments.SingleThread := true
    else begin
      LStepIn.Arguments.SingleThread := false;
      LStepIn.Arguments.ThreadId := FCurrentStoppedThreadId;
    end;
    LStepIn.Arguments.Granularity := TSteppingGranularity.Statement;

    FDebugger.SendRequest<TStepInResponse>(
      LStepIn,
      procedure(const AArg: TStepInResponse)
      begin

      end,
      procedure(const AArg: TResponseMessage)
      begin
        StepOver();
      end
    );
  finally
    LStepIn.Free();
  end;
end;

procedure TDebugService.StepOver;
begin
  var LNext := TNextRequest.Create();
  try
    if FCurrentStoppedThreadId = 0 then
      LNext.Arguments.SingleThread := true
    else begin
      LNext.Arguments.SingleThread := false;
      LNext.Arguments.ThreadId := FCurrentStoppedThreadId;
    end;
    LNext.Arguments.Granularity := TSteppingGranularity.Line;

    FDebugger.SendRequest<TNextResponse>(
      LNext,
      procedure(const AArg: TNextResponse)
      begin

      end,
      procedure(const AArg: TResponseMessage)
      begin

      end
    );
  finally
    LNext.Free();
  end;
end;


procedure TDebugService.StepOut;
begin
  var LStepOut := TStepOutRequest.Create();
  try
    if FCurrentStoppedThreadId = 0 then
      LStepOut.Arguments.SingleThread := true
    else begin
      LStepOut.Arguments.SingleThread := false;
      LStepOut.Arguments.ThreadId := FCurrentStoppedThreadId;
    end;
    LStepOut.Arguments.Granularity := TSteppingGranularity.Statement;

    FDebugger.SendRequest<TStepOutResponse>(
      LStepOut,
      procedure(const AArg: TStepOutResponse)
      begin

      end,
      procedure(const AArg: TResponseMessage)
      begin
        StepOver();
      end
    );
  finally
    LStepOut.Free();
  end;
end;

procedure TDebugService.Reconnect(const ATimeOut: Int64);
begin
  DoStart(FDebugger.Host, FDebugger.Port, 60);

  TSpinWait.SpinUntil(
    function: boolean
    begin
      Result := (FConnectionStatus = TDebuggerConnectionStatus.Started);
    end, ATimeOut);
end;

procedure TDebugService.ForceDisconnection;
begin
  FConnectionStatus := TDebuggerConnectionStatus.Disconnecting;
  StopSession();
end;

function TDebugService.AskForFrozenDebuggerAction: TDebuggerConnectionFrozenAction;
begin
  var LResult := TDebuggerConnectionFrozenAction.ForceDisconnection;
  //Ask someone to tell us what to do
  TGlobalBuilderChain.SendRequest<TDebuggerConnectionFrozenActionResponse>(
    TDebuggerConnectionFrozenActionRequest.Create(),
    procedure(const AArg: TDebuggerConnectionFrozenActionResponse)
    begin
      LResult := AArg.Body.Action;
    end,
    procedure(const AArg: string)
    begin
      LResult := TDebuggerConnectionFrozenAction.ForceDisconnection;
    end);
  Result := LResult;
end;

function TDebugService.AskForFrozenDebuggerButReconnectBeforeAction: TDebuggerConnectionFrozenAction;
begin
  //Maybe we only lost connection. Let's reconnect.
  if (FConnectionStatus = TDebuggerConnectionStatus.Connecting) then
    Reconnect();

  if (FConnectionStatus = TDebuggerConnectionStatus.Connecting) then
    Exit(AskForFrozenDebuggerAction());

  Result := TDebuggerConnectionFrozenAction.TryAgain
end;

function TDebugService.AskForFrozenDebuggerButWaitForRunningAppAction: TDebuggerConnectionFrozenAction;
begin
  TSpinWait.SpinUntil(
    function(): boolean
    begin
      Result := (FConnectionStatus = TDebuggerConnectionStatus.Started) or not IsAppRunning();

      if not Result and (FConnectionStatus = TDebuggerConnectionStatus.Connecting) then
        Reconnect();
    end, 30000);

  if not IsAppRunning() then
    Exit(TDebuggerConnectionFrozenAction.ForceDisconnection);

  Result := AskForFrozenDebuggerButReconnectBeforeAction();
end;

procedure TDebugService.MonitorFrozenDebugger(
  const ATriggerStatus, ALeaveStatus: TDebuggerConnectionStatus; const ATimeOut: integer;
  const AAction: TFunc<TDebuggerConnectionFrozenAction>);
begin
  var LTryAgain := true;
  while FMonitoringDebugger and (FConnectionStatus = ATriggerStatus) and LTryAgain do begin
    //Let's wait a time interval for debugger response
    TSpinWait.SpinUntil(
      function(): boolean
      begin
        Result := (FConnectionStatus = ALeaveStatus) or not FMonitoringDebugger or not IsAppRunning();
      end, ATimeOut * 1000);

    if not FMonitoringDebugger then
      Exit;

    if (FConnectionStatus = ATriggerStatus) then begin
      if not Assigned(AAction) then
        ForceDisconnection()
      else begin
        case AAction() of
          TDebuggerConnectionFrozenAction.ForceDisconnection: ForceDisconnection();
          TDebuggerConnectionFrozenAction.TryAgain          : LTryAgain := true;
        end;
      end;
    end;
  end;
end;

procedure TDebugService.StartFrozenDebuggerMonitor;
begin
  FMonitoringDebugger := true;
  FFrozenDebuggerMonitor := System.Classes.TThread.CreateAnonymousThread(
    procedure()
    begin
      try
        while FMonitoringDebugger do begin
          try
            MonitorFrozenDebugger(
              TDebuggerConnectionStatus.Connecting,
              TDebuggerConnectionStatus.Started,
              1,
              AskForFrozenDebuggerButWaitForRunningAppAction);

            MonitorFrozenDebugger(
              TDebuggerConnectionStatus.Disconnecting,
              TDebuggerConnectionStatus.Stopped,
              1);

            CheckForGhostDebugger();
          except
            on E: Exception do
              TGlobalBuilderChain.BroadcastEventAsync(TAsyncExceptionEvent.Create());
          end;

          Sleep(100);
        end;
      finally
        System.Classes.TThread.RemoveQueuedEvents(
          System.Classes.TThread.CurrentThread);
      end;
    end);
  FFrozenDebuggerMonitor.FreeOnTerminate := false;
  FFrozenDebuggerMonitor.Start();
end;

procedure TDebugService.StopFrozenDebuggerMonitor;
begin
  FMonitoringDebugger := false;
  FFrozenDebuggerMonitor.WaitFor();
  FFrozenDebuggerMonitor.Free();
end;

procedure TDebugService.SubscribeToEvents;
begin
  FInitializedEvent := FDebugger.SubscribeToEvent<TInitializedEvent>(
    procedure(const AEvent: TInitializedEvent)
    begin
      if (FConnectionStatus <> TDebuggerConnectionStatus.Connecting) then
        raise EDebuggerNotConnected.Create('Debugger is not connected.');
      //The debugger has confirmed we're down
      FConnectionStatus := TDebuggerConnectionStatus.Started;
      FStopped := false;
      FCurrentStoppedThreadId := 0;
      OnInitializedEvent();
    end);

  FStoppedEvent := FDebugger.SubscribeToEvent<TStoppedEvent>(
    procedure(const AEvent: TStoppedEvent)
    begin
      FStopped := true;
      FCurrentStoppedThreadId := AEvent.Body.ThreadId;
    end);

  FContinuedEvent := FDebugger.SubscribeToEvent<TContinuedEvent>(
    procedure(const AEvent: TContinuedEvent)
    begin
      FStopped := false;
      FCurrentStoppedThreadId := 0;
    end);

  FTerminatedEvent := FDebugger.SubscribeToEvent<TTerminatedEvent>(
    procedure(const AEvent: TTerminatedEvent)
    begin
      StopSession();
    end);
end;

procedure TDebugService.UnsubscribeAll;
begin
  FStoppedEvent.Unsubscribe();
  FInitializedEvent.Unsubscribe();
end;

procedure TDebugService.InitializeRequest();
begin
  var LInitialize := TInitializeRequest.Create();
  try
    var LArguments := LInitialize.Arguments;
    //LArguments.ClientId := 'delphi4python';
    //LArguments.ClientName := 'delphi4python4android';
    LArguments.AdapterId := 'python';
    //LArguments.Locale := 'en-US';
    LArguments.LinesStartAt1 := true;
    LArguments.ColumnsStartAt1 := true;
    //LArguments.PathFormat := TPathFormat.None;
    LArguments.SupportsVariableType := true;
    LArguments.SupportsVariablePaging := true;
    LArguments.SupportsRunInTerminalRequest := false;
    LArguments.SupportsMemoryReferences := true;
    LArguments.SupportsProgressReporting := false;
    LArguments.SupportsInvalidatedEvent := true;
    LArguments.SupportsMemoryEvent := true;

    FDebugger.SendRequest<TInitializeResponse>(
      LInitialize,
      procedure(const AArg: TInitializeResponse)
      begin
        OnInitializeResponse();
      end,
      procedure(const AArg: TResponseMessage)
      begin
        raise EFailedToInitializeDebugger.Create(
          'Failed initializing the debugger.'
          + sLineBreak
          + sLineBreak
          + AArg);
      end);
  finally
    LInitialize.Free();
  end;
end;

procedure TDebugService.OnInitializeResponse();
begin
  AttachRequest();
end;

procedure TDebugService.OnInitializedEvent();
begin
  try
    TGlobalBuilderChain.BroadcastEvent(TSetupDebuggerEvent.Create(FDebugger));
  finally
    OnLastConfiguration();
  end;
end;

procedure TDebugService.AttachRequest();
begin
  var LAttach := TAttachRequest.Create();
  try
    FDebugger.SendRequest<TAttachResponse>(
      LAttach,
      procedure(const AArg: TAttachResponse)
      begin
        OnAttachResponse();
      end,
      procedure(const AArg: TResponseMessage)
      begin

      end);
  finally
    LAttach.Free();
  end;
end;

procedure TDebugService.OnAttachResponse();
begin

end;

procedure TDebugService.OnLastConfiguration;
begin
  ConfigurationDoneRequest();
end;

procedure TDebugService.ConfigurationDoneRequest();
begin
  var LConfDone := TConfigurationDoneRequest.Create();
  try
    FDebugger.SendRequest<TConfigurationDoneResponse>(
      LConfDone,
      procedure(const AArg: TConfigurationDoneResponse)
      begin
        OnConfigurationDoneResponse();
      end,
      procedure(const AArg: TResponseMessage)
      begin
      end);
  finally
    LConfDone.Free();
  end;
end;

procedure TDebugService.OnConfigurationDoneResponse();
begin
  TGlobalBuilderChain.BroadcastEvent(TSetupDebuggerDoneEvent.Create(FDebugger));
end;

end.
