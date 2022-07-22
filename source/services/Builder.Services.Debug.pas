unit Builder.Services.Debug;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Threading,
  System.SyncObjs,
  Builder.Chain,
  Builder.Services,
  BaseProtocol,
  BaseProtocol.Types,
  BaseProtocol.Events,
  BaseProtocol.Requests,
  BaseProtocol.Client,
  BaseProtocol.Client.Socket;

type
  TDebugService = class(TInterfacedObject, IDebugServices)
  private
    FDebugger: TBaseProtocolClientSocket;
    FInitializedEvent: IUnsubscribable;
    FStatus: TDebuggerStatus;
    //We are connected to the debugger, but debugger doesn't proceed with process execution
    //or we ordered disconnection, but debugger doesn't confirm it.
    FMonitoringDebugger: boolean;
    FFrozenDebuggerMonitor: System.Classes.TThread;
    function GetStatus: TDebuggerStatus;
    procedure InternalConnect(const ATimeOut: Int64);
    procedure StartSession(const AHost: string; const APort: integer;
      const ATimeOut: Int64);
    procedure StopSession();

    function AskForFrozenDebuggerAction(): TDebuggerConnectionFrozenAction;
    function AskForFrozenDebuggerActionButReconnectBefore(): TDebuggerConnectionFrozenAction;
    procedure MonitorFrozenDebugger(const ATriggerStatus,
      ALeaveStatus: TDebuggerStatus; const ATimeOut: integer = 10;
      const AAction: TFunc<TDebuggerConnectionFrozenAction> = nil);
    procedure StartFrozenDebuggerMonitor();
    procedure StopFrozenDebuggerMonitor();

    procedure DoStart(const AHost: string; const APort: integer; const ATimeOut: Int64);
    procedure Reconnect(const ATimeOut: Int64 = 5000);
    procedure ForceDisconnection();
  public
    constructor Create();
    destructor Destroy(); override;

    procedure Start(const AHost: string; const APort: integer; const ATimeOut: Int64 = 60);
    procedure Pause();
    procedure Stop();

    procedure StepIn();
    procedure StepOut();
    procedure Next();
  end;

implementation

uses
  System.DateUtils, System.Net.Socket;

{ TDebugService }

constructor TDebugService.Create;
begin
  inherited;
  FStatus := TDebuggerStatus.OutOfWork;
  TEventsRegistration.RegisterAll();
  TRequestsRegistration.RegisterAll();
  FDebugger := TBaseProtocolClientSocket.Create();
  StartFrozenDebuggerMonitor();
  FInitializedEvent := FDebugger.SubscribeToEvent<TInitializedEvent>(
    procedure(const AEvent: TInitializedEvent)
    begin
      if (FStatus <> TDebuggerStatus.Connecting) then
        raise Exception.Create('Debugger is not connected.');

      FStatus := TDebuggerStatus.Started;
      //Set breakpoints
      var LConfDone := TConfigurationDoneRequest.Create();
      try
        FDebugger.SendRequest<TConfigurationDoneResponse>(
          LConfDone,
          procedure(const AArg: TConfigurationDoneResponse)
          begin
          end,
          procedure(const AArg: TResponseMessage)
          begin
          end);
      finally
        LConfDone.Free();
      end;
    end);
end;

destructor TDebugService.Destroy;
begin
  FInitializedEvent.Unsubscribe();
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

  var LInitialize := TInitializeRequest.Create();
    try
      var LArguments := LInitialize.Arguments;
      LArguments.AdapterId := 'python';

      FDebugger.SendRequest<TInitializeResponse>(
        LInitialize,
        procedure(const AArg: TInitializeResponse)
        begin
          var LAttach := TAttachRequest.Create();
          try
            FDebugger.SendRequest<TAttachResponse>(
              LAttach,
              procedure(const AArg: TAttachResponse)
              begin

              end,
              procedure(const AArg: TResponseMessage)
              begin

              end);
          finally
            LAttach.Free();
          end;
        end,
        procedure(const AArg: TResponseMessage)
        begin

        end);
    finally
      LInitialize.Free();
    end;
end;

function TDebugService.GetStatus: TDebuggerStatus;
begin
  Result := FStatus;
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

procedure TDebugService.StartSession(const AHost: string; const APort: integer;
  const ATimeOut: Int64);
begin
  FDebugger.Active := false;
  FDebugger.Host := AHost;
  FDebugger.Port := APort;
  FStatus := TDebuggerStatus.Connecting;

  TGlobalBuilderChain.BroadcastEvent(TDebugSessionStartedEvent.Create(FDebugger));
  try
    InternalConnect(ATimeOut);
  except
    on E: Exception do begin
      FStatus := TDebuggerStatus.Stopped;
      TGlobalBuilderChain.BroadcastEventAsync(TDebugSessionStoppedEvent.Create());
      raise;
    end;
  end;
end;

procedure TDebugService.StopSession;
begin
  if (FStatus <> TDebuggerStatus.Disconnecting) then
    raise Exception.Create('Debugger is still connected.');

  FStatus := TDebuggerStatus.Stopped;
  TGlobalBuilderChain.BroadcastEventAsync(TDebugSessionStoppedEvent.Create());
  FDebugger.Active := false;
end;

procedure TDebugService.Start(const AHost: string; const APort: integer; const ATimeOut: Int64);
begin
  if not (FStatus in [TDebuggerStatus.OutOfWork, TDebuggerStatus.Stopped]) then
    raise Exception.Create('Debugger is busy');

  DoStart(AHost, APort, ATimeOut);
end;

procedure TDebugService.Stop;
begin
  if (FStatus <> TDebuggerStatus.Started) then
    raise Exception.Create('Debugger is not started.');

  FStatus := TDebuggerStatus.Disconnecting;
  var LStop := TTerminateRequest.Create();
  try
    FDebugger.SendRequest<TTerminateResponse>(
      LStop,
      procedure(const AArg: TTerminateResponse)
      begin
        StopSession();
      end,
      procedure(const AArg: TResponseMessage)
      begin

      end
    );
  finally
    LStop.Free();
  end;
end;

procedure TDebugService.Next;
begin
  var LNext := TNextRequest.Create();
  try
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

procedure TDebugService.Reconnect(const ATimeOut: Int64);
begin
  DoStart(FDebugger.Host, FDebugger.Port, 60);
  TSpinWait.SpinUntil(
    function: boolean
    begin
      Result := (FStatus = TDebuggerStatus.Started);
    end, ATimeOut);
end;

procedure TDebugService.StepIn;
begin
  var LStepIn := TStepInRequest.Create();
  try
    FDebugger.SendRequest<TStepInResponse>(
      LStepIn,
      procedure(const AArg: TStepInResponse)
      begin

      end,
      procedure(const AArg: TResponseMessage)
      begin

      end
    );
  finally
    LStepIn.Free();
  end;
end;

procedure TDebugService.StepOut;
begin
  var LStepOut := TStepOutRequest.Create();
  try
    FDebugger.SendRequest<TStepOutResponse>(
      LStepOut,
      procedure(const AArg: TStepOutResponse)
      begin

      end,
      procedure(const AArg: TResponseMessage)
      begin

      end
    );
  finally
    LStepOut.Free();
  end;
end;

procedure TDebugService.ForceDisconnection;
begin
  FStatus := TDebuggerStatus.Disconnecting;
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

function TDebugService.AskForFrozenDebuggerActionButReconnectBefore: TDebuggerConnectionFrozenAction;
begin
  //Maybe we only lost connection. Let's reconnect.
  if (FStatus = TDebuggerStatus.Connecting) then begin
    Reconnect();
  end;

  if (FStatus = TDebuggerStatus.Connecting) then begin
    Exit(AskForFrozenDebuggerAction());
  end;

  Result := TDebuggerConnectionFrozenAction.TryAgain
end;

procedure TDebugService.MonitorFrozenDebugger(
  const ATriggerStatus, ALeaveStatus: TDebuggerStatus; const ATimeOut: integer;
  const AAction: TFunc<TDebuggerConnectionFrozenAction>);
begin
  var LTryAgain := true;
  while FMonitoringDebugger and (FStatus = ATriggerStatus) and LTryAgain do begin
    //Let's wait a time interval for debugger response
    TSpinWait.SpinUntil(
      function(): boolean
      begin
        Result := (FStatus = ALeaveStatus) or not FMonitoringDebugger;
      end, ATimeOut * 1000);

    if not FMonitoringDebugger then
      Exit;

    if (FStatus = ATriggerStatus) then begin
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
  FFrozenDebuggerMonitor := System.Classes.TThread.CreateAnonymousThread(procedure() begin
    try
      while FMonitoringDebugger do begin
        MonitorFrozenDebugger(
          TDebuggerStatus.Connecting,
          TDebuggerStatus.Started,
          5,
          AskForFrozenDebuggerActionButReconnectBefore);

        MonitorFrozenDebugger(
          TDebuggerStatus.Disconnecting,
          TDebuggerStatus.Stopped,
          3);

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

end.
