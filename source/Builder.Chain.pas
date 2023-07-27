unit Builder.Chain;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Threading,
  System.SyncObjs,
  System.Generics.Collections,
  Builder.Types,
  Builder.Model.Project,
  BaseProtocol.Requests,
  BaseProtocol.Client,
  BaseProtocol.Types;

type
  {$SCOPEDENUMS ON}
  TBuilderRequest = (
    DebuggerConnectionFrozen
  );

  TBuilderEvent = (
    Internal,
    &Message,
    OpenProject,
    CloseProject,
    OpenFile,
    CloseFile,
    RenameFile,
    SaveState,
    //Debugger events
    DebugSessionStarted,
    DebugSessionStopped,
    SetupDebugger,
    SetupDebuggerDone,
    DebugAction,
    //Editor events
    EditorChanged,
    //App can keep track of async ops for thread safety
    AsyncOperationStarted,
    AsyncOperationEnded,
    AsyncException);

  TAsyncOperation = (
    OpenProject,
    BuildProject,
    DeployProject,
    RunProject,
    DebugProject,
    StopProject);

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
    class function GetChainRequestType(): TBuilderRequest;
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
    class function GetChainEventType(): TBuilderEvent;
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

  {|| EVENTS ||}
  {$SCOPEDENUMS ON}
  TMessageLevel = (Descriptive, Explanatory);
  {$SCOPEDENUMS OFF}
  TMessageEventBody = class
  private
    FMessage: string;
    FLevel: TMessageLevel;
    FClear: boolean;
  public
    property Message: string read FMessage write FMessage;
    property Level: TMessageLevel read FLevel write FLevel;
    property Clear: boolean read FClear write FClear;
  end;

  [EventType(TBuilderEvent.Message)]
  TMessageEvent = class(TChainEvent<TMessageEventBody>)
  public
    constructor Create(const AMessage: string; const ALevel: TMessageLevel; const AClear: boolean); reintroduce; overload;
    constructor Create(const AMessage: string; const AClear: boolean); reintroduce; overload;
    constructor Create(const AMessage: string); reintroduce; overload;
    constructor Create(const AMessage: string; const ALevel: TMessageLevel); reintroduce; overload;
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
    FFileName: string;
    FNew: boolean;
    FBreakpoints: TArray<integer>;
    FActiveLine: integer;
    FShowActiveLineIndicator: boolean;
  public
    property FilePath: string read FFilePath write FFilePath;
    property FileName: string read FFileName write FFileName;
    property New: boolean read FNew write FNew;
    property Breakpoints: TArray<integer> read FBreakpoints write FBreakpoints;
    property ActiveLine: integer read FActiveLine write FActiveLine;
    property ShowActiveLineIndicator: boolean read FShowActiveLineIndicator write FShowActiveLineIndicator;
  end;

  [EventType(TBuilderEvent.OpenFile)]
  TOpenFileEvent = class(TChainEvent<TOpenFileBody>)
  public
    constructor Create(const AFilePath: string; const AActiveLine: integer;
      const AShowActiveLineIndicator: boolean; const ANew: boolean); reintroduce; overload;
    constructor Create(const AFilePath: string; const AActiveLine: integer;
      const AShowActiveLineIndicator: boolean); reintroduce; overload;
    constructor Create(const AFilePath: string); reintroduce; overload;
    constructor Create(const AFilePath: string; const ANew: boolean); reintroduce; overload;
  end;

  TCloseFileBody = class
  private
    FFilePath: string;
  public
    property FilePath: string read FFilePath write FFilePath;
  end;

  [EventType(TBuilderEvent.CloseFile)]
  TCloseFileEvent = class(TChainEvent<TCloseFileBody>)
  public
    constructor Create(const AFilePath: string); reintroduce;
  end;

  TRenameFileBody = class
  private
    FOldFilePath: string;
    FNewFilePath: string;
  public
    property OldFilePath: string read FOldFilePath write FOldFilePath;
    property NewFilePath: string read FNewFilePath write FNewFilePath;
  end;

  [EventType(TBuilderEvent.RenameFile)]
  TRenameFileEvent = class(TChainEvent<TRenameFileBody>)
  public
    constructor Create(const AOldFilePath, ANewFilePath: string); reintroduce;
  end;

  TEditorChangedBody = class
  private
    FTextEditor: ITextEditor;
    FModified: boolean;
  public
    property TextEditor: ITextEditor read FTextEditor write FTextEditor;
    property Modified: boolean read FModified write FModified;
  end;

  [EventType(TBuilderEvent.EditorChanged)]
  TEditorChangedEvent = class(TChainEvent<TEditorChangedBody>)
  public
    constructor Create(const ATextEditor: ITextEditor;
      const AModified: boolean); reintroduce; overload;
    constructor Create(const ATextEditor: ITextEditor); reintroduce; overload;
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

  TDebugSessionStoppedEventBoby = class
  private
    FDebugger: TBaseProtocolClient;
  public
    property Debugger: TBaseProtocolClient read FDebugger write FDebugger;
  end;

  [EventType(TBuilderEvent.DebugSessionStopped)]
  TDebugSessionStoppedEvent = class(TChainEvent<TDebugSessionStoppedEventBoby>)
  public
    constructor Create(const ADebugger: TBaseProtocolClient); reintroduce;
  end;

  TSetupDebuggerEventBoby = class
  private
    FDebugger: TBaseProtocolClient;
  public
    property Debugger: TBaseProtocolClient read FDebugger write FDebugger;
  end;

  [EventType(TBuilderEvent.SetupDebugger)]
  TSetupDebuggerEvent = class(TChainEvent<TSetupDebuggerEventBoby>)
  public
    constructor Create(const ADebugger: TBaseProtocolClient); reintroduce;
  end;

  TSetupDebuggerDoneEventBoby = class
  private
    FDebugger: TBaseProtocolClient;
  public
    property Debugger: TBaseProtocolClient read FDebugger write FDebugger;
  end;

  [EventType(TBuilderEvent.SetupDebuggerDone)]
  TSetupDebuggerDoneEvent = class(TChainEvent<TSetupDebuggerDoneEventBoby>)
  public
    constructor Create(const ADebugger: TBaseProtocolClient); reintroduce;
  end;

  {|||| Debug Actions ||||}

  {$SCOPEDENUMS ON}
  TDebugAction = (Start, Stop, Pause, StepIn, StepOver, StepOut, Continue);
  {$SCOPEDENUMS OFF}

  TDebugActionEventBody = class
  private
    FAction: TDebugAction;
  public
    property Action: TDebugAction read FAction write FAction;
  end;

  [EventType(TBuilderEvent.DebugAction)]
  TDebugActionEvent = class(TChainEvent<TDebugActionEventBody>)
  public
    constructor Create(const ADebugAction: TDebugAction); reintroduce;
  end;

  {|||| ------------- ||||}

  {$SCOPEDENUMS ON}
  TSaveState = (Save, SaveAll);
  {$SCOPEDENUMS OFF}
  TSaveStateEventBoby = class
  private
    FSaveState: TSaveState;
  public
    property SaveState: TSaveState read FSaveState write FSaveState;
  end;

  [EventType(TBuilderEvent.SaveState)]
  TSaveStateEvent = class(TChainEvent<TSaveStateEventBoby>)
  public
    constructor Create(const ASaveState: TSaveState); reintroduce;
  end;

  {|| REQUESTS ||}

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

  {|| Builder Chain ||}

  TBuilderChain = class
  private type
    TDisconnectable = class(TInterfacedObject, IDisconnectable)
    private type
      TOnDisconnect = reference to procedure();
    private
      FOnDisconnect: TOnDisconnect;
    public
      constructor Create(const AOnDisconnect: TOnDisconnect);
      destructor Destroy(); override;
      procedure Disconnect();
    end;
    TQueuedEventInfo = record
      Event: TChainEvent;
      Owned: boolean;
      Callback: TProc;
    end;
    TRequestHandlerInfo = record
      RequestType: TClass;
      Notification: TChainReverseRequestNotification;
    end;
    [EventType(TBuilderEvent.Internal)]
    TFlushEvent = class(TChainEvent);
  private
    FBroadcasting: boolean;
    FBroadcastTask: System.Classes.TThread;
    FEventQueue: TThreadedQueue<TQueuedEventInfo>;
    FEventSubscribers: TThreadList<TChainEventNotification>;
    FRequestHandlers: TThreadList<TRequestHandlerInfo>;
    FFlushing: boolean;
    procedure StartBroadcasting();
    procedure StopBroadcasting();
    //Broadcaster
    procedure NotifyListeners(const AEventInfo: TQueuedEventInfo);
    function InternalBroadcastEvent(const AEvent: TChainEvent;
      const AAsync, AOwned: boolean; const ACompletitionCallback: TProc = nil;
      APredicate: TPredicate<TChainEvent> = nil): boolean;
    function DefaultBroadcastEventPredicate(AEvent: TChainEvent): boolean;
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

    /// <summary>
    ///   Blocking broadcaster.
    /// </summary>
    function BroadcastEvent(const AEvent: TChainEvent;
      const AOwned: boolean = true): boolean;
    /// <summary>
    ///   Non-blocking broadcaster.
    /// </summary>
    function BroadcastEventAsync(const AEvent: TChainEvent;
      const AOwned: boolean = true): boolean;

    function SubscribeToEvent(
      const AEventNotification: TChainEventNotification): IDisconnectable; overload;
    function SubscribeToEvent<T: TChainEvent>(
      const AEventNotification: TChainEventNotification<T>): IDisconnectable; overload;

    /// <summary>
    ///   Wait for all events queued before this request to broadcast.
    /// </summary>
    procedure Flush();
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

    class function BroadcastEvent(const AEvent: TChainEvent;
      const AOwned: boolean = true): boolean;
    class function BroadcastEventAsync(const AEvent: TChainEvent;
      const AOwned: boolean = true): boolean;

    class function SubscribeToEvent<T: TChainEvent>(
      const AEventNotification: TChainEventNotification<T>): IDisconnectable;

    class procedure Flush();

    class property Instance: TBuilderChain read FInstance;
  end;

implementation

uses
  System.Rtti,
  Builder.Exception;

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

  raise ERequestTypeAttributeNotFound.Create('Request type attribute not found.');
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
  FRequestType := GetChainRequestType();
end;

class function TChainResponse.GetChainRequestType: TBuilderRequest;
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

  raise ERequestTypeAttributeNotFound.Create('Request type attribute not found.');
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
  FEventType := GetChainEventType();
end;

class function TChainEvent.GetChainEventType: TBuilderEvent;
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

  raise EEventTypeAttributeNotFound.Create('Event type attribute not found.');
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

constructor TMessageEvent.Create(const AMessage: string;
  const ALevel: TMessageLevel; const AClear: boolean);
begin
  inherited Create();
  FBody.Message := AMessage;
  FBody.Level := ALevel;
  FBody.Clear := AClear;
end;

constructor TMessageEvent.Create(const AMessage: string; const AClear: boolean);
begin
  Create(AMessage, TMessageLevel.Descriptive, AClear);
end;

constructor TMessageEvent.Create(const AMessage: string);
begin
  Create(AMessage, false);
end;

constructor TMessageEvent.Create(const AClear: boolean);
begin
  Create(String.Empty, AClear);
end;

constructor TMessageEvent.Create(const AMessage: string;
  const ALevel: TMessageLevel);
begin
  Create(AMessage, ALevel, false);
end;

{ TBuilderChain }

constructor TBuilderChain.Create;
begin
  inherited Create();
  FFlushing := false;
  FBroadcasting := true;
  FEventQueue := TThreadedQueue<TQueuedEventInfo>.Create();
  FRequestHandlers := TThreadList<TRequestHandlerInfo>.Create();
  FEventSubscribers := TThreadList<TChainEventNotification>.Create();
  StartBroadcasting();
end;

destructor TBuilderChain.Destroy;
begin
  StopBroadcasting();
  FEventSubscribers.Free();
  FRequestHandlers.Free();
  FEventQueue.Free();
  inherited;
end;

function TBuilderChain.SubscribeToReverseRequest<T>(
  const AReverseRequestNotification: TChainReverseRequestNotification<T>): IDisconnectable;
var
  LRequestHandlerInfo: TRequestHandlerInfo;
begin
  LRequestHandlerInfo.RequestType := T;
  LRequestHandlerInfo.Notification := TChainReverseRequestNotification(AReverseRequestNotification);

  FRequestHandlers.Add(LRequestHandlerInfo);
  Result := TDisconnectable.Create(
    procedure()
    begin
      FRequestHandlers.Remove(LRequestHandlerInfo);
    end);
end;

procedure TBuilderChain.NotifyListeners(const AEventInfo: TQueuedEventInfo);
begin
  try
    var LList := FEventSubscribers.LockList();
    try
      //Listeners can't disconnect while notifications
      //It would cause notifications to invalid references
      for var LSubscriber in LList do
      try
        LSubscriber(AEventInfo.Event);
      except
        //
      end;
    finally
      FEventSubscribers.UnlockList();
    end;

    if Assigned(AEventInfo.Callback) then
      AEventInfo.Callback();
  finally
    if AEventInfo.Owned then begin
      AEventInfo.Event.Free();
    end;
  end;
end;

procedure TBuilderChain.SendRequest<T>(const ARequest: TChainRequest;
  const AResolve: TChainResolve<T>; const AReject: TChainReject;
  const AOwned: boolean);
begin
  var LHandled := false;
  try
    var LList := FRequestHandlers.LockList();
    try
      //Responders can't disconnect while notifications
      //It would cause requests to invalid references
      for var LHandler in LList do begin
        if not (ARequest is LHandler.RequestType) then
          Continue;

        var LResponse := LHandler.Notification(ARequest, LHandled);
        if LHandled then begin
          if LResponse.Success then
            AResolve(LResponse as T)
          else
            AReject(LResponse.Message);

          Break;
        end;
      end;
    finally
      FRequestHandlers.UnlockList();
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
  FBroadcastTask := System.Classes.TThread.CreateAnonymousThread(
    procedure()
    var
      LCurrent: TQueuedEventInfo;
    begin
      try
        while FBroadcasting do begin
          if (FEventQueue.PopItem(LCurrent) <> TWaitResult.wrSignaled) then
            Continue;

          if FEventQueue.ShutDown then
            Break;

          NotifyListeners(LCurrent);
        end;
      finally
        System.Classes.TThread.RemoveQueuedEvents(System.Classes.TThread.Current);
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

function TBuilderChain.DefaultBroadcastEventPredicate(
  AEvent: TChainEvent): boolean;
begin
  Result := FBroadcasting
    and (AEvent.GetChainEventType() <> TBuilderEvent.Internal)
    and not FFlushing;
end;

function TBuilderChain.InternalBroadcastEvent(const AEvent: TChainEvent;
  const AAsync, AOwned: boolean; const ACompletitionCallback: TProc;
  APredicate: TPredicate<TChainEvent>): boolean;
var
  LQueuedEventInfo: TQueuedEventInfo;
begin
  if not Assigned(APredicate) then
    APredicate := DefaultBroadcastEventPredicate;

  if not APredicate(AEvent) then
    Exit(false);

  LQueuedEventInfo := Default(TQueuedEventInfo);
  LQueuedEventInfo.Event := AEvent;
  LQueuedEventInfo.Owned := AOwned;
  LQueuedEventInfo.Callback := ACompletitionCallback;

  if not AAsync then begin
    NotifyListeners(LQueuedEventInfo);
    Result := true;
  end else
    Result := (FEventQueue.PushItem(LQueuedEventInfo) = TWaitResult.wrSignaled)
      and not FEventQueue.ShutDown;
end;

function TBuilderChain.BroadcastEvent(const AEvent: TChainEvent;
  const AOwned: boolean): boolean;
begin
  Result := InternalBroadcastEvent(AEvent, false, AOwned);
end;

function TBuilderChain.BroadcastEventAsync(const AEvent: TChainEvent;
  const AOwned: boolean): boolean;
begin
  Result := InternalBroadcastEvent(AEvent, true, AOwned);
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

procedure TBuilderChain.Flush;
var
  LFlushed: boolean;
begin
  if not FBroadcasting then
    Exit();

  //Flushing prevents new pushes to que event queue
  FFlushing := true;
  try
    LFlushed := false;
    if not InternalBroadcastEvent(TFlushEvent.Create(), true, true,
      procedure() begin
        LFlushed := true;
      end,
      function(AEvent: TChainEvent): boolean begin
        Result := FBroadcasting;
      end) then
        Exit;

    //Once we reach out this message, it's guaranteed we have an empty queue.
    TSpinWait.SpinUntil(function(): boolean begin
      Result := LFlushed or FEventQueue.ShutDown;
    end);
  finally
    FFlushing := false;
  end;
end;

{ TBuilderChain.TDisconnectable }

constructor TBuilderChain.TDisconnectable.Create(const AOnDisconnect: TOnDisconnect);
begin
  inherited Create();
  FOnDisconnect := AOnDisconnect;
end;

destructor TBuilderChain.TDisconnectable.Destroy;
begin
  Disconnect();
  inherited;
end;

procedure TBuilderChain.TDisconnectable.Disconnect;
begin
  if Assigned(FOnDisconnect) then
    FOnDisconnect();
  FOnDisconnect := nil;
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

class function TGlobalBuilderChain.BroadcastEvent(const AEvent: TChainEvent;
  const AOwned: boolean): boolean;
begin
  Result := TGlobalBuilderChain.Instance.BroadcastEvent(AEvent, AOwned);
end;

class function TGlobalBuilderChain.BroadcastEventAsync(
  const AEvent: TChainEvent; const AOwned: boolean): boolean;
begin
  Result := TGlobalBuilderChain.Instance.BroadcastEventAsync(AEvent, AOwned);
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

class procedure TGlobalBuilderChain.Flush;
begin
  TGlobalBuilderChain.Instance.Flush();
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

constructor TOpenFileEvent.Create(const AFilePath: string;
  const AActiveLine: integer; const AShowActiveLineIndicator, ANew: boolean);
begin
  inherited Create();
  Body.FilePath := AFilePath;
  Body.ActiveLine := AActiveLine;
  Body.ShowActiveLineIndicator := AShowActiveLineIndicator;
  Body.New := ANew;
end;

constructor TOpenFileEvent.Create(const AFilePath: string;
  const AActiveLine: integer; const AShowActiveLineIndicator: boolean);
begin
  Create(AFilePath, AActiveLine, AShowActiveLineIndicator, false);
end;

constructor TOpenFileEvent.Create(const AFilePath: string; const ANew: boolean);
begin
  Create(AFilePath, 0, false, ANew);
end;

constructor TOpenFileEvent.Create(const AFilePath: string);
begin
  Create(AFilePath, false);
end;

{ TCloseFileEvent }

constructor TCloseFileEvent.Create(const AFilePath: string);
begin
  inherited Create();
  FBody.FilePath := AFilePath;
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

{ TDebugSessionStoppedEvent }

constructor TDebugSessionStoppedEvent.Create(
  const ADebugger: TBaseProtocolClient);
begin
  inherited Create();
  Body.Debugger := ADebugger;
end;

{ TSetupDebuggerEvent }

constructor TSetupDebuggerEvent.Create(const ADebugger: TBaseProtocolClient);
begin
  inherited Create();
  Body.Debugger := ADebugger;
end;

{ TSetupDebuggerDoneEvent }

constructor TSetupDebuggerDoneEvent.Create(
  const ADebugger: TBaseProtocolClient);
begin
  inherited Create();
  Body.Debugger := ADebugger;
end;

{ TSaveStateEvent }

constructor TSaveStateEvent.Create(const ASaveState: TSaveState);
begin
  inherited Create();
  Body.SaveState := ASaveState;
end;

{ TDebugActionEvent }

constructor TDebugActionEvent.Create(
  const ADebugAction: TDebugAction);
begin
  inherited Create();
  Body.Action := ADebugAction;
end;

{ TRenameFileEvent }

constructor TRenameFileEvent.Create(const AOldFilePath, ANewFilePath: string);
begin
  inherited Create();
  Body.OldFilePath := AOldFilePath;
  Body.NewFilePath := ANewFilePath;
end;

{ TEditorChangedEvent }

constructor TEditorChangedEvent.Create(const ATextEditor: ITextEditor;
  const AModified: boolean);
begin
  inherited Create();
  Body.TextEditor := ATextEditor;
  Body.Modified := AModified;
end;

constructor TEditorChangedEvent.Create(const ATextEditor: ITextEditor);
begin
  Create(ATextEditor, false);
end;

end.
