unit Builder.Message;

interface

uses
  System.Threading,
  System.SyncObjs,
  System.Generics.Collections;

type
  {$SCOPEDENUMS ON}
  TRequestType = (
    EditorSaveRequest,
    DebuggerConnectionFrozen
  );

  TEventType = (
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
  {$SCOPEDENUMS OFF}

  RequestTypeAttribute = class(TCustomAttribute)
  private
    FRequestType: TRequestType;
  public
    constructor Create(const ARequestType: TRequestType);

    property RequestType: TRequestType read FRequestType;
  end;

  EventTypeAttribute = class(TCustomAttribute)
  private
    FEventType: TEventType;
  public
    constructor Create(const AEventType: TEventType);

    property EventType: TEventType read FEventType;
  end;

  IDisconnectable = interface
    ['{21B1CCF8-D844-4738-BB3E-31C826B33949}']
    procedure Disconnect();
  end;

  TRequestClass = class of TRequest;

  TRequest = class;
  TResponse = class;
  TEvent = class;

  TReverseRequestNotification<T: TRequest> = reference to function(const AReverseRequest: T; var AHandled: boolean): TResponse;
  TReverseRequestNotification = TReverseRequestNotification<TRequest>;

  TRequestResolve<T: TResponse> = reference to procedure(const AArg: T);
  TRequestReject = reference to procedure(const AArg: string);

  TEventNotification<T: TEvent> = reference to procedure(const AEvent: T);
  TEventNotification = TEventNotification<TEvent>;

  TEmptyBody = class
  end;

  TEmptyArguments = class
  end;

  TMessage = class
  protected
    class function InitializeType<T>(): T;
    class procedure FinalizeType<T>(var AValue);
  public
    constructor Create(); virtual;
  end;

  TRequest = class(TMessage)
  private
    FRequestType: TRequestType;
  public
    class function GeTRequestType(): TRequestType;
  public
    constructor Create(); override;

    property RequestType: TRequestType read FRequestType;
  end;

  TRequest<TArguments> = class(TRequest)
  private
    FArguments: TArguments;
  public
    constructor Create(); override;
    destructor Destroy(); override;

    property Arguments: TArguments read FArguments write FArguments;
  end;

  TResponse = class(TMessage)
  private
    FRequestType: TRequestType;
    FSuccess: boolean;
    FMessage: string;
  public
    class function GeTRequestType(): TRequestType;
  public
    constructor Create(); override;

    property RequestType: TRequestType read FRequestType write FRequestType;
    property Success: boolean read FSuccess write FSuccess;
    property Message: string read FMessage write FMessage;
  end;

  TResponse<TBody> = class(TResponse)
  private
    FBody: TBody;
  public
    constructor Create(); override;
    destructor Destroy(); override;

    property Body: TBody read FBody write FBody;
  end;

  TResponseErrorBody = class
  private
    FId: integer;
  public
    property Id: integer read FId write FId;
  end;

  TResponseError = class(TResponse<TResponseErrorBody>)
  public
    constructor Create(const AMessage: string); reintroduce;
  end;

  TEvent = class(TMessage)
  private
    FEventType: TEventType;
  public
    class function GeTEventType(): TEventType;
  public
    constructor Create(); override;
  end;

  TEvent<TBody> = class(TEvent)
  private
    FBody: TBody;
  public
    constructor Create(); override;
    destructor Destroy(); override;

    property Body: TBody read FBody;
  end;

implementation

uses
  System.Rtti,
  Builder.Exception;

{ RequestTypeAttribute }

constructor RequestTypeAttribute.Create(const ARequestType: TRequestType);
begin
  inherited Create();
  FRequestType := ARequestType;
end;

{ EventTypeAttribute }

constructor EventTypeAttribute.Create(const AEventType: TEventType);
begin
  inherited Create();
  FEventType := AEventType;
end;

{ TMessage }

constructor TMessage.Create;
begin
  inherited;
end;

class function TMessage.InitializeType<T>: T;
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

class procedure TMessage.FinalizeType<T>(var AValue);
var
  LRttiCtx: TRttiContext;
begin
  var LRttiType := LRttiCtx.GetType(TypeInfo(T));
  if LRttiType.IsInstance then
    TObject(AValue).Free();
end;

{ TRequest }

constructor TRequest.Create;
begin
  inherited;
  FRequestType := GeTRequestType();
end;

class function TRequest.GeTRequestType: TRequestType;
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

{ TRequest<TArguments> }

constructor TRequest<TArguments>.Create;
begin
  inherited;
  FArguments := TMessage.InitializeType<TArguments>();
end;

destructor TRequest<TArguments>.Destroy;
begin
  TMessage.FinalizeType<TArguments>(FArguments);
  inherited;
end;

{ TResponse }

constructor TResponse.Create;
begin
  inherited;
  FRequestType := GeTRequestType();
end;

class function TResponse.GeTRequestType: TRequestType;
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

{ TResponse<TBody> }

constructor TResponse<TBody>.Create;
begin
  inherited;
  FBody := TMessage.InitializeType<TBody>();
end;

destructor TResponse<TBody>.Destroy;
begin
  TMessage.FinalizeType<TBody>(FBody);
  inherited;
end;

{ TResponseError }

constructor TResponseError.Create(const AMessage: string);
begin
  inherited Create();
  Success := false;
end;

{ TEvent }

constructor TEvent.Create;
begin
  inherited;
  FEventType := GeTEventType();
end;

class function TEvent.GeTEventType: TEventType;
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

{ TEvent<TBody> }

constructor TEvent<TBody>.Create;
begin
  inherited;
  FBody := TMessage.InitializeType<TBody>();
end;

destructor TEvent<TBody>.Destroy;
begin
  TMessage.FinalizeType<TBody>(FBody);
  inherited;
end;

end.
