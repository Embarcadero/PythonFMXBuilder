unit Builder.Chain;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Threading,
  System.SyncObjs,
  System.Generics.Collections,
  Builder.Model.Project,
  BaseProtocol.Client;

type
  {$SCOPEDENUMS ON}
  TBuilderRequest = (
    DebuggerConnectionFrozen
  );

  TBuilderEvent = (
    &Message,
    OpenProject,
    CloseProject,
    OpenFile,
    CloseFile,
    DebugSessionStarted,
    DebugSessionStopped,
    //App can keep track of async ops for thread safety
    AsyncOperationStarted,
    AsyncOperationEnded,
    AsyncException);

  TAsyncOperation = (
    OpenProject,
    BuildProject,
    DeployProject,
    RunProject,
    DebugProject);

  TDebuggerConnectionFrozenAction = (
    ForceDisconnection,
    TryAgain
  );
  {$SCOPEDENUMS OFF}

  RequestTypeAttribute = class(TCustomAttribute)
  private
    FRequestType: TBuilderRequest;
  public
    constructor Create(const ARequestType: TBuilderRequest);

    property RequestType: TBuilderRequest read FRequestType;
  end;

  EventTypeAttribute = class(TCustomAttribute)
  private
    FEventType: TBuilderEvent;
  public
    constructor Create(const AEventType: TBuilderEvent);

    property EventType: TBuilderEvent read FEventType;
  end;

  IDisconnectable = interface
    ['{21B1CCF8-D844-4738-BB3E-31C826B33949}']
    procedure Disconnect();
  end;

  TChainRequestClass = class of TChainRequest;

  TChainRequest = class;
  TChainResponse = class;
  TChainEvent = class;

  TChainReverseRequestNotification<T: TChainRequest> = reference to function(const AReverseRequest: T; var AHandled: boolean): TChainResponse;
  TChainReverseRequestNotification = TChainReverseRequestNotification<TChainRequest>;

  TChainResolve<T: TChainResponse> = reference to procedure(const AArg: T);
  TChainReject = reference to procedure(const AArg: string);

  TChainEventNotification<T: TChainEvent> = reference to procedure(const AEvent: T);
  TChainEventNotification = TChainEventNotification<TChainEvent>;

  TEmptyBody = class
  end;

  TEmptyArguments = class
  end;

  TChainMessage = class
  protected
    class function InitializeType<T>(): T;
    class procedure FinalizeType<T>(var AValue);
  public
    constructor Create(); virtual;
  end;

  TChainRequest = class(TChainMessage)
  private
    FRequestType: TBuilderRequest;
  public
    class function GeTChainRequestType(): TBuilderRequest;
  public
    constructor Create(); override;

    property RequestType: TBuilderRequest read FRequestType;
  end;

  TChainRequest<TArguments> = class(TChainRequest)
  private
    FArguments: TArguments;
  public
    constructor Create(); override;
    destructor Destroy(); override;

    property Arguments: TArguments read FArguments write FArguments;
  end;

  TChainResponse = class(TChainMessage)
  private
    FRequestType: TBuilderRequest;
    FSuccess: boolean;
    FMessage: string;
  public
    class function GeTChainRequestType(): TBuilderRequest;
  public
    constructor Create(); override;

    property RequestType: TBuilderRequest read FRequestType write FRequestType;
    property Success: boolean read FSuccess write FSuccess;
    property Message: string read FMessage write FMessage;
  end;

  TChainResponse<TBody> = class(TChainResponse)
  private
    FBody: TBody;
  public
    constructor Create(); override;
    destructor Destroy(); override;

    property Body: TBody read FBody write FBody;
  end;

  TErrorResponseBody = class
  private
    FId: integer;
  public
    property Id: integer read FId write FId;
  end;

  TErrorResponse = class(TChainResponse<TErrorResponseBody>)
  public
    constructor Create(const AMessage: string); reintroduce;
  end;

  TChainEvent = class(TChainMessage)
  private
    FEventType: TBuilderEvent;
  public
    class function GeTChainEventType(): TBuilderEvent;
  public
    constructor Create(); override;
  end;

  TChainEvent<TBody> = class(TChainEvent)
  private
    FBody: TBody;
  public
    constructor Create(); override;
    destructor Destroy(); override;

    property Body: TBody read FBody;
  end;

  TMessageEventBody = class
  private
    FMessage: string;
    FClear: boolean;
  public
    property Message: string read FMessage write FMessage;
    property Clear: boolean read FClear write FClear;
  end;

  [EventType(TBuilderEvent.Message)]
  TMessageEvent = class(TChainEvent<TMessageEventBody>)
  public
    constructor Create(const AMessage: string; const AClear: boolean); reintroduce; overload;
    constructor Create(const AMessage: string); reintroduce; overload;
    constructor Create(const AClear: boolean); reintroduce; overload;
  end;

  TOpenProjectBody = class
  private
    FProject: TProjectModel;
  public
    property Project: TProjectModel read FProject write FProject;
  end;

  [EventType(TBuilderEvent.OpenProject)]
  TOpenProjectEvent = class(TChainEvent<TOpenProjectBody>)
  public
    constructor Create(const AProject: TProjectModel); reintroduce;
  end;

  TCloseProjectBody = class
  private
    FProject: TProjectModel;
  public
    property Project: TProjectModel read FProject write FProject;
  end;

  [EventType(TBuilderEvent.CloseProject)]
  TCloseProjectEvent = class(TChainEvent<TCloseProjectBody>)
  public
    constructor Create(const AProject: TProjectModel); reintroduce;
  end;

  TOpenFileBody = class
  private
    FFilePath: string;
  public
    property FilePath: string read FFilePath write FFilePath;
  end;

  [EventType(TBuilderEvent.OpenFile)]
  TOpenFileEvent = class(TChainEvent<TOpenFileBody>)
  public
    constructor Create(const AFilePath: string); reintroduce;
  end;

  TAsyncOperationStartedBody = class
  private
    FOperation: TAsyncOperation;
  public
    property Operation: TAsyncOperation read FOperation write FOperation;
  end;

  [EventType(TBuilderEvent.AsyncOperationStarted)]
  TAsyncOperationStartedEvent = class(TChainEvent<TAsyncOperationStartedBody>)
  public
    constructor Create(const AOperation: TAsyncOperation); reintroduce;
  end;

  TAsyncOperationEndedBody = class
  private
    FOperation: TAsyncOperation;
  public
    property Operation: TAsyncOperation read FOperation write FOperation;
  end;

  [EventType(TBuilderEvent.AsyncOperationEnded)]
  TAsyncOperationEndedEvent = class(TChainEvent<TAsyncOperationEndedBody>)
  public
    constructor Create(const AOperation: TAsyncOperation); reintroduce;
  end;

  TAsyncExceptionBody = class
  private
    FException: Exception;
  public
    destructor Destroy(); override;

    property Exception: Exception read FException write FException;
  end;

  [EventType(TBuilderEvent.AsyncException)]
  TAsyncExceptionEvent = class(TChainEvent<TAsyncExceptionBody>)
  public
    constructor Create(); override;
  end;

  TDebugSessionStartedEventBoby = class
  private
    FDebugger: TBaseProtocolClient;
  public
    property Debugger: TBaseProtocolClient read FDebugger write FDebugger;
  end;

  [EventType(TBuilderEvent.DebugSessionStarted)]
  TDebugSessionStartedEvent = class(TChainEvent<TDebugSessionStartedEventBoby>)
  public
    constructor Create(const ADebugger: TBaseProtocolClient); reintroduce;
  end;

  [EventType(TBuilderEvent.DebugSessionStopped)]
  TDebugSessionStoppedEvent = class(TChainEvent<TEmptyBody>);

  [RequestType(TBuilderRequest.DebuggerConnectionFrozen)]
  TDebuggerConnectionFrozenActionRequest = class(TChainRequest<TEmptyArguments>);

  TDebuggerConnectionFrozenActionResponseBody = class
  private
    FAction: TDebuggerConnectionFrozenAction;
  public
    property Action: TDebuggerConnectionFrozenAction read FAction write FAction;
  end;

  [RequestType(TBuilderRequest.DebuggerConnectionFrozen)]
  TDebuggerConnectionFrozenActionResponse = class(TChainResponse<TDebuggerConnectionFrozenActionResponseBody>)
  public
    constructor Create(const AAction: TDebuggerConnectionFrozenAction); reintroduce;
  end;

  TBuilderChain = class
  private type
    TDisconnectable = class(TInterfacedObject, IDisconnectable)
    private type
      TOnDisconnect = reference to procedure();
    private
      FOnDisconnect: TOnDisconnect;
    public
      constructor Create(const AOnDisconnect: TOnDisconnect);
      procedure Disconnect();
    end;
    TQueuedEventInfo = record
      Event: TChainEvent;
      Owned: boolean;
      Callback: TProc;
    end;
  private
    FBroadcasting: boolean;
    FBroadcastTask: TThread;
    FEventQueue: TThreadedQueue<TQueuedEventInfo>;
    FEventSubscribers: TThreadList<TChainEventNotification>;
    FRequestHandlers: TThreadList<TChainReverseRequestNotification>;
    procedure StartBroadcasting();
    procedure StopBroadcasting();

    function InternalBroadcastEvent(const AEvent: TChainEvent;
      const AOwned: boolean = true; const ACompletitionCallback: TProc = nil): boolean;
  public
    constructor Create();
    destructor Destroy(); override;

    function SubscribeToReverseRequest<T: TChainRequest>(
      const AReverseRequestNotification: TChainReverseRequestNotification<T>): IDisconnectable;

    procedure SendRequest<T: TChainResponse>(const ARequest: TChainRequest;
      const AResolve: TChainResolve<T>; const AReject: TChainReject;
      const AOwned: boolean = true);
    procedure SendRequestAsync<T: TChainResponse>(const ARequest: TChainRequest;
      const AResolve: TChainResolve<T>; const AReject: TChainReject;
      const AOwned: boolean = true);

    function BroadcastEvent(const AEvent: TChainEvent;
      const AOwned: boolean = true): boolean;
    procedure BroadcastEventAsync(const AEvent: TChainEvent;
      const AOwned: boolean = true; const ARejectedCallback: TProc = nil);

    function SubscribeToEvent(
      const AEventNotification: TChainEventNotification): IDisconnectable; overload;
    function SubscribeToEvent<T: TChainEvent>(
      const AEventNotification: TChainEventNotification<T>): IDisconnectable; overload;
  end;

  TGlobalBuilderChain = class(TBuilderChain)
  private
    class var FInstance: TBuilderChain;
  public
    class constructor Create();
    class destructor Destroy();

    class function SubscribeToReverseRequest<T: TChainRequest>(
      const AReverseRequestNotification: TChainReverseRequestNotification<T>): IDisconnectable;

    class procedure SendRequest<T: TChainResponse>(const ARequest: TChainRequest;
      const AResolve: TChainResolve<T>; const AReject: TChainReject;
      const AOwned: boolean = true);
    class procedure SendRequestAsync<T: TChainResponse>(const ARequest: TChainRequest;
      const AResolve: TChainResolve<T>; const AReject: TChainReject;
      const AOwned: boolean = true);

    class procedure BroadcastEvent(const AEvent: TChainEvent;
      const AOwned: boolean = true); reintroduce;
    class procedure BroadcastEventAsync(const AEvent: TChainEvent;
      const AOwned: boolean = true; const ARejectedCallback: TProc = nil); reintroduce;

    class function SubscribeToEvent<T: TChainEvent>(
      const AEventNotification: TChainEventNotification<T>): IDisconnectable; reintroduce;

    class property Instance: TBuilderChain read FInstance;
  end;

implementation

uses
  System.Rtti;

{ RequestTypeAttribute }

constructor RequestTypeAttribute.Create(const ARequestType: TBuilderRequest);
begin
  inherited Create();
  FRequestType := ARequestType;
end;

{ EventTypeAttribute }

constructor EventTypeAttribute.Create(const AEventType: TBuilderEvent);
begin
  inherited Create();
  FEventType := AEventType;
end;

{ TChainMessage }

constructor TChainMessage.Create;
begin
  inherited;
end;

class function TChainMessage.InitializeType<T>: T;
var
  LRttiCtx: TRttiContext;
begin
  var LRttiType := LRttiCtx.GetType(TypeInfo(T));
  if LRttiType.IsInstance then
    for var LRttiMethod in LRttiType.GetMethods() do
      if LRttiMethod.HasExtendedInfo and LRttiMethod.IsConstructor and (Length(LRttiMethod.GetParameters()) = 0) then
        Exit(LRttiMethod.Invoke(LRttiType.AsInstance.MetaclassType, []).AsType<T>());

  Result := Default(T);
end;

class procedure TChainMessage.FinalizeType<T>(var AValue);
var
  LRttiCtx: TRttiContext;
begin
  var LRttiType := LRttiCtx.GetType(TypeInfo(T));
  if LRttiType.IsInstance then
    TObject(AValue).Free();
end;

{ TChainRequest }

constructor TChainRequest.Create;
begin
  inherited;
  FRequestType := GeTChainRequestType();
end;

class function TChainRequest.GeTChainRequestType: TBuilderRequest;
var
  LRttiCtx: TRttiContext;
begin
  var LRttiType := LRttiCtx.GetType(Self.ClassInfo);
  while Assigned(LRttiType) do begin
    var LAttribute := LRttiType.GetAttribute<RequestTypeAttribute>();
    if Assigned(LAttribute) then
      Exit(LAttribute.RequestType);

    LRttiType := LRttiType.BaseType;
  end;

  raise Exception.Create('Request type attribute not found.');
end;

{ TChainRequest<TArguments> }

constructor TChainRequest<TArguments>.Create;
begin
  inherited;
  FArguments := TChainMessage.InitializeType<TArguments>();
end;

destructor TChainRequest<TArguments>.Destroy;
begin
  TChainMessage.FinalizeType<TArguments>(FArguments);
  inherited;
end;

{ TChainResponse }

constructor TChainResponse.Create;
begin
  inherited;
  FRequestType := GeTChainRequestType();
end;

class function TChainResponse.GeTChainRequestType: TBuilderRequest;
var
  LRttiCtx: TRttiContext;
begin
  var LRttiType := LRttiCtx.GetType(Self.ClassInfo);
  while Assigned(LRttiType) do begin
    var LAttribute := LRttiType.GetAttribute<RequestTypeAttribute>();
    if Assigned(LAttribute) then
      Exit(LAttribute.RequestType);

    LRttiType := LRttiType.BaseType;
  end;

  raise Exception.Create('Request type attribute not found.');
end;

{ TChainResponse<TBody> }

constructor TChainResponse<TBody>.Create;
begin
  inherited;
  FBody := TChainMessage.InitializeType<TBody>();
end;

destructor TChainResponse<TBody>.Destroy;
begin
  TChainMessage.FinalizeType<TBody>(FBody);
  inherited;
end;

{ TChainEvent }

constructor TChainEvent.Create;
begin
  inherited;
  FEventType := GeTChainEventType();
end;

class function TChainEvent.GeTChainEventType: TBuilderEvent;
var
  LRttiCtx: TRttiContext;
begin
  var LRttiType := LRttiCtx.GetType(Self.ClassInfo);
  while Assigned(LRttiType) do begin
    var LAttribute := LRttiType.GetAttribute<EventTypeAttribute>();
    if Assigned(LAttribute) then
      Exit(LAttribute.EventType);

    LRttiType := LRttiType.BaseType;
  end;

  raise Exception.Create('Event type attribute not found.');
end;

{ TChainEvent<TBody> }

constructor TChainEvent<TBody>.Create;
begin
  inherited;
  FBody := TChainMessage.InitializeType<TBody>();
end;

destructor TChainEvent<TBody>.Destroy;
begin
  TChainMessage.FinalizeType<TBody>(FBody);
  inherited;
end;

{ TMessageEvent }

constructor TMessageEvent.Create(const AMessage: string; const AClear: boolean);
begin
  inherited Create();
  FBody.Message := AMessage;
  FBody.Clear := AClear;
end;

constructor TMessageEvent.Create(const AMessage: string);
begin
  Create(AMessage, false);
end;

constructor TMessageEvent.Create(const AClear: boolean);
begin
  Create(String.Empty, AClear);
end;

{ TBuilderChain }

constructor TBuilderChain.Create;
begin
  FBroadcasting := true;
  FEventQueue := TThreadedQueue<TQueuedEventInfo>.Create();
  FRequestHandlers := TThreadList<TChainReverseRequestNotification>.Create();
  FEventSubscribers := TThreadList<TChainEventNotification>.Create();
  StartBroadcasting();
end;

destructor TBuilderChain.Destroy;
begin
  StopBroadcasting();
  FEventSubscribers.Free();
  FRequestHandlers.Free();
  FEventQueue.Free();
end;

function TBuilderChain.SubscribeToReverseRequest<T>(
  const AReverseRequestNotification: TChainReverseRequestNotification<T>): IDisconnectable;
begin
  FRequestHandlers.Add(TChainReverseRequestNotification(AReverseRequestNotification));
  Result := TDisconnectable.Create(
    procedure()
    begin
      FRequestHandlers.Remove(
        TChainReverseRequestNotification(AReverseRequestNotification));
    end);
end;

procedure TBuilderChain.SendRequest<T>(const ARequest: TChainRequest;
  const AResolve: TChainResolve<T>; const AReject: TChainReject;
  const AOwned: boolean);
var
  LHandlers: TArray<TChainReverseRequestNotification>;
begin
  try
    var LList := FRequestHandlers.LockList();
    try
      LHandlers := LList.ToArray();
    finally
      FRequestHandlers.UnlockList();
    end;

    var LHandled := false;
    for var LHandler in LHandlers do begin
      var LResponse := LHandler(ARequest, LHandled);
      if LHandled then
        if LResponse.Success then
          AResolve(LResponse as T)
        else
          AReject(LResponse.Message);
    end;

    if not LHandled then
      AReject('Handler unavailable.');
  finally
    if AOwned then
      ARequest.Free();
  end;
end;

procedure TBuilderChain.SendRequestAsync<T>(const ARequest: TChainRequest;
  const AResolve: TChainResolve<T>; const AReject: TChainReject;
  const AOwned: boolean);
begin
  TTask.Run(
    procedure
    begin
      SendRequest<T>(ARequest, AResolve, AReject, AOwned);
    end);
end;

procedure TBuilderChain.StartBroadcasting;
begin
  FBroadcasting := true;
  FBroadcastTask := TThread.CreateAnonymousThread(
    procedure()
    var
      LCurrent: TQueuedEventInfo;
      LSubscribers: TArray<TChainEventNotification>;
    begin
      try
        while FBroadcasting do begin
          if FEventQueue.ShutDown then
            Break;

          if (FEventQueue.PopItem(LCurrent) <> TWaitResult.wrSignaled) then
            Continue;

          try
            var LList := FEventSubscribers.LockList();
            try
              LSubscribers := LList.ToArray();
            finally
              FEventSubscribers.UnlockList();
            end;

            for var LSubscriber in LSubscribers do begin
              try
                LSubscriber(LCurrent.Event);
              except
                //
              end;
            end;

            if Assigned(LCurrent.Callback) then
              LCurrent.Callback();
          finally
            if LCurrent.Owned then
              LCurrent.Event.Free();
          end;
        end;
      finally
        TThread.RemoveQueuedEvents(TThread.Current);
      end;
    end);
  FBroadcastTask.FreeOnTerminate := false;
  FBroadcastTask.Start();
end;

procedure TBuilderChain.StopBroadcasting;
begin
  FEventQueue.DoShutDown();
  FBroadcasting := false;
  FBroadcastTask.WaitFor();
  FBroadcastTask.Free();
end;

function TBuilderChain.InternalBroadcastEvent(const AEvent: TChainEvent;
  const AOwned: boolean; const ACompletitionCallback: TProc):boolean;
var
  LQueuedEventInfo: TQueuedEventInfo;
begin
  if not FBroadcasting then
    Exit(false);

  LQueuedEventInfo := Default(TQueuedEventInfo);
  LQueuedEventInfo.Event := AEvent;
  LQueuedEventInfo.Owned := AOwned;
  LQueuedEventInfo.Callback := ACompletitionCallback;

  Result := (FEventQueue.PushItem(LQueuedEventInfo) = TWaitResult.wrSignaled)
    and not FEventQueue.ShutDown;
end;

function TBuilderChain.BroadcastEvent(const AEvent: TChainEvent;
  const AOwned: boolean): boolean;
var
  LDone: boolean;
begin
  LDone := false;
  Result := InternalBroadcastEvent(AEvent, AOwned,
    procedure()
    begin
      LDone := true;
    end);

  if Result then
    TSpinWait.SpinUntil(
      function(): boolean
      begin
        Result := LDone or FEventQueue.ShutDown;
      end);
end;

procedure TBuilderChain.BroadcastEventAsync(const AEvent: TChainEvent;
  const AOwned: boolean; const ARejectedCallback: TProc);
begin
  TTask.Run(
    procedure()
    begin
      if not InternalBroadcastEvent(AEvent) then
        if Assigned(ARejectedCallback) then
          ARejectedCallback();
    end);
end;

function TBuilderChain.SubscribeToEvent(
  const AEventNotification: TChainEventNotification): IDisconnectable;
begin
  FEventSubscribers.Add(AEventNotification);
  Result := TDisconnectable.Create(
    procedure()
    begin
      FEventSubscribers.Remove(AEventNotification);
    end);
end;

function TBuilderChain.SubscribeToEvent<T>(
  const AEventNotification: TChainEventNotification<T>): IDisconnectable;
begin
  Result := SubscribeToEvent(
    procedure(const AEvent: TChainEvent)
    var
      LRttiCtx: TRttiContext;
    begin
      var LRttiType := LRttiCtx.GetType(T);
      if (LRttiType.IsInstance
        and (LRttiType.AsInstance.MetaclassType = AEvent.ClassType)) then
          AEventNotification(AEvent as TChainEvent);
    end);
end;

{ TBuilderChain.TDisconnectable }

constructor TBuilderChain.TDisconnectable.Create(const AOnDisconnect: TOnDisconnect);
begin
  inherited Create();
  FOnDisconnect := AOnDisconnect;
end;

procedure TBuilderChain.TDisconnectable.Disconnect;
begin
  if Assigned(FOnDisconnect) then
    FOnDisconnect();
end;

{ TGlobalBuilderChain }

class constructor TGlobalBuilderChain.Create;
begin
  FInstance := TBuilderChain.Create();
end;

class destructor TGlobalBuilderChain.Destroy;
begin
  FInstance.Free();
end;

class procedure TGlobalBuilderChain.BroadcastEvent(const AEvent: TChainEvent;
      const AOwned: boolean);
begin
  TGlobalBuilderChain.Instance.BroadcastEvent(AEvent, AOwned);
end;

class procedure TGlobalBuilderChain.BroadcastEventAsync(
  const AEvent: TChainEvent; const AOwned: boolean;
  const ARejectedCallback: TProc);
begin
  TGlobalBuilderChain.Instance.BroadcastEventAsync(AEvent, AOwned, ARejectedCallback);
end;

class procedure TGlobalBuilderChain.SendRequest<T>(const ARequest: TChainRequest;
  const AResolve: TChainResolve<T>; const AReject: TChainReject;
  const AOwned: boolean);
begin
  TGlobalBuilderChain.Instance.SendRequest<T>(ARequest, AResolve, AReject,
    AOwned);
end;

class procedure TGlobalBuilderChain.SendRequestAsync<T>(
  const ARequest: TChainRequest; const AResolve: TChainResolve<T>;
  const AReject: TChainReject; const AOwned: boolean);
begin
  TGlobalBuilderChain.Instance.SendRequestAsync<T>(ARequest, AResolve, AReject,
    AOwned);
end;

class function TGlobalBuilderChain.SubscribeToEvent<T>(
  const AEventNotification: TChainEventNotification<T>): IDisconnectable;
begin
  Result := TGlobalBuilderChain.Instance.SubscribeToEvent<T>(
    AEventNotification);
end;

class function TGlobalBuilderChain.SubscribeToReverseRequest<T>(
  const AReverseRequestNotification: TChainReverseRequestNotification<T>): IDisconnectable;
begin
  Result := TGlobalBuilderChain.Instance.SubscribeToReverseRequest<T>(
    AReverseRequestNotification);
end;

{ TOpenProjectEvent }

constructor TOpenProjectEvent.Create(const AProject: TProjectModel);
begin
  inherited Create();
  Body.Project := AProject;
end;

{ TCloseProjectEvent }

constructor TCloseProjectEvent.Create(const AProject: TProjectModel);
begin
  inherited Create();
  Body.Project := AProject;
end;

{ TErrorResponse }

constructor TErrorResponse.Create(const AMessage: string);
begin
  inherited Create();
  Success := false;
end;

{ TOpenFileEvent }

constructor TOpenFileEvent.Create(const AFilePath: string);
begin
  inherited Create();
  Body.FilePath := AFilePath;
end;

{ TAsyncOperationStartedEvent }

constructor TAsyncOperationStartedEvent.Create(
  const AOperation: TAsyncOperation);
begin
  inherited Create();
  Body.Operation := AOperation;
end;

{ TAsyncOperationEndedEvent }

constructor TAsyncOperationEndedEvent.Create(const AOperation: TAsyncOperation);
begin
  inherited Create();
  Body.Operation := AOperation;
end;

{ TAsyncExceptionBody }

destructor TAsyncExceptionBody.Destroy;
begin
  FException.Free();
  inherited;
end;

{ TAsyncExceptionEvent }

constructor TAsyncExceptionEvent.Create();
begin
  inherited Create();
  Body.Exception := AcquireExceptionObject() as Exception;
end;

{ TDebugSessionStartedEvent }

constructor TDebugSessionStartedEvent.Create(
  const ADebugger: TBaseProtocolClient);
begin
  inherited Create();
  Body.Debugger := ADebugger;
end;

{ TDebuggerConnectionFrozenActionResponse }

constructor TDebuggerConnectionFrozenActionResponse.Create(
  const AAction: TDebuggerConnectionFrozenAction);
begin
  inherited Create();
  Success := true;
  Body.Action := AAction;
end;

end.
