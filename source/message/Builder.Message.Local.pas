unit Builder.Message.Local;

interface

uses
  System.SysUtils, System.Classes, System.Threading, System.Generics.Collections,
  System.SyncObjs, System.Rtti,
  Builder.Message;

type
  TLocalMessagery = class
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
      Event: TEvent;
      Owned: boolean;
      Callback: TProc;
    end;
    TRequestHandlerInfo = record
      RequestType: TClass;
      Notification: TReverseRequestNotification;
    end;
    [EventType(TEventType.Internal)]
    TFlushEvent = class(TEvent);
  private
    FBroadcasting: boolean;
    FBroadcastTask: System.Classes.TThread;
    FEventQueue: TThreadedQueue<TQueuedEventInfo>;
    FEventSubscribers: TThreadList<TEventNotification>;
    FRequestHandlers: TThreadList<TRequestHandlerInfo>;
    FFlushing: boolean;
    procedure StartBroadcasting();
    procedure StopBroadcasting();
    //Broadcaster
    procedure NotifyListeners(const AEventInfo: TQueuedEventInfo);
    function InternalBroadcastEvent(const AEvent: TEvent;
      const AAsync, AOwned: boolean; const ACompletitionCallback: TProc = nil;
      APredicate: TPredicate<TEvent> = nil): boolean;
    function DefaultBroadcastEventPredicate(AEvent: TEvent): boolean;
  public
    constructor Create();
    destructor Destroy(); override;

    function SubscribeToReverseRequest<T: TRequest>(
      const AReverseRequestNotification: TReverseRequestNotification<T>): IDisconnectable;

    procedure SendRequest<T: TResponse>(const ARequest: TRequest;
      const AResolve: TRequestResolve<T>; const AReject: TRequestReject;
      const AOwned: boolean = true);
    procedure SendRequestAsync<T: TResponse>(const ARequest: TRequest;
      const AResolve: TRequestResolve<T>; const AReject: TRequestReject;
      const AOwned: boolean = true);

    /// <summary>
    ///   Blocking broadcaster.
    /// </summary>
    function BroadcastEvent(const AEvent: TEvent;
      const AOwned: boolean = true): boolean;
    /// <summary>
    ///   Non-blocking broadcaster.
    /// </summary>
    function BroadcastEventAsync(const AEvent: TEvent;
      const AOwned: boolean = true): boolean;

    function SubscribeToEvent(
      const AEventNotification: TEventNotification): IDisconnectable; overload;
    function SubscribeToEvent<T: TEvent>(
      const AEventNotification: TEventNotification<T>): IDisconnectable; overload;

    /// <summary>
    ///   Wait for all events queued before this request to broadcast.
    /// </summary>
    procedure Flush();
  end;

  TGlobalLocalMessagery = class(TLocalMessagery)
  private
    class var FInstance: TLocalMessagery;
  private
    class constructor Create();
    class destructor Destroy();
  public
    class function SubscribeToReverseRequest<T: TRequest>(
      const AReverseRequestNotification: TReverseRequestNotification<T>): IDisconnectable;

    class procedure SendRequest<T: TResponse>(const ARequest: TRequest;
      const AResolve: TRequestResolve<T>; const AReject: TRequestReject;
      const AOwned: boolean = true);
    class procedure SendRequestAsync<T: TResponse>(const ARequest: TRequest;
      const AResolve: TRequestResolve<T>; const AReject: TRequestReject;
      const AOwned: boolean = true);

    class function BroadcastEvent(const AEvent: TEvent;
      const AOwned: boolean = true): boolean;
    class function BroadcastEventAsync(const AEvent: TEvent;
      const AOwned: boolean = true): boolean;

    class function SubscribeToEvent<T: TEvent>(
      const AEventNotification: TEventNotification<T>): IDisconnectable;

    class procedure Flush();

    class property Instance: TLocalMessagery read FInstance;
  end;

implementation

{ TLocalMessagery }

constructor TLocalMessagery.Create;
begin
  inherited Create();
  FFlushing := false;
  FBroadcasting := true;
  FEventQueue := TThreadedQueue<TQueuedEventInfo>.Create();
  FRequestHandlers := TThreadList<TRequestHandlerInfo>.Create();
  FEventSubscribers := TThreadList<TEventNotification>.Create();
  StartBroadcasting();
end;

destructor TLocalMessagery.Destroy;
begin
  StopBroadcasting();
  FEventSubscribers.Free();
  FRequestHandlers.Free();
  FEventQueue.Free();
  inherited;
end;

function TLocalMessagery.SubscribeToReverseRequest<T>(
  const AReverseRequestNotification: TReverseRequestNotification<T>): IDisconnectable;
var
  LRequestHandlerInfo: TRequestHandlerInfo;
begin
  LRequestHandlerInfo.RequestType := T;
  LRequestHandlerInfo.Notification := TReverseRequestNotification(AReverseRequestNotification);

  FRequestHandlers.Add(LRequestHandlerInfo);
  Result := TDisconnectable.Create(
    procedure()
    begin
      FRequestHandlers.Remove(LRequestHandlerInfo);
    end);
end;

procedure TLocalMessagery.NotifyListeners(const AEventInfo: TQueuedEventInfo);
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

procedure TLocalMessagery.SendRequest<T>(const ARequest: TRequest;
  const AResolve: TRequestResolve<T>; const AReject: TRequestReject;
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
        try
          if LHandled then
            if LResponse.Success then
              AResolve(LResponse as T)
            else
              AReject(LResponse.Message);

          Break;
        finally
          if AOwned then
            LResponse.Free();
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

procedure TLocalMessagery.SendRequestAsync<T>(const ARequest: TRequest;
  const AResolve: TRequestResolve<T>; const AReject: TRequestReject;
  const AOwned: boolean);
begin
  TTask.Run(
    procedure
    begin
      SendRequest<T>(ARequest, AResolve, AReject, AOwned);
    end);
end;

procedure TLocalMessagery.StartBroadcasting;
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

procedure TLocalMessagery.StopBroadcasting;
begin
  FEventQueue.DoShutDown();
  FBroadcasting := false;
  FBroadcastTask.WaitFor();
  FBroadcastTask.Free();
end;

function TLocalMessagery.DefaultBroadcastEventPredicate(
  AEvent: TEvent): boolean;
begin
  Result := FBroadcasting
    and (AEvent.GeTEventType() <> TEventType.Internal)
    and not FFlushing;
end;

function TLocalMessagery.InternalBroadcastEvent(const AEvent: TEvent;
  const AAsync, AOwned: boolean; const ACompletitionCallback: TProc;
  APredicate: TPredicate<TEvent>): boolean;
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

function TLocalMessagery.BroadcastEvent(const AEvent: TEvent;
  const AOwned: boolean): boolean;
begin
  Result := InternalBroadcastEvent(AEvent, false, AOwned);
end;

function TLocalMessagery.BroadcastEventAsync(const AEvent: TEvent;
  const AOwned: boolean): boolean;
begin
  Result := InternalBroadcastEvent(AEvent, true, AOwned);
end;

function TLocalMessagery.SubscribeToEvent(
  const AEventNotification: TEventNotification): IDisconnectable;
begin
  FEventSubscribers.Add(AEventNotification);
  Result := TDisconnectable.Create(
    procedure()
    begin
      FEventSubscribers.Remove(AEventNotification);
    end);
end;

function TLocalMessagery.SubscribeToEvent<T>(
  const AEventNotification: TEventNotification<T>): IDisconnectable;
begin
  Result := SubscribeToEvent(
    procedure(const AEvent: TEvent)
    var
      LRttiCtx: TRttiContext;
    begin
      var LRttiType := LRttiCtx.GetType(T);
      if (LRttiType.IsInstance
        and (LRttiType.AsInstance.MetaclassType = AEvent.ClassType)) then
          AEventNotification(AEvent as TEvent);
    end);
end;

procedure TLocalMessagery.Flush;
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
      function(AEvent: TEvent): boolean begin
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

{ TLocalMessagery.TDisconnectable }

constructor TLocalMessagery.TDisconnectable.Create(const AOnDisconnect: TOnDisconnect);
begin
  inherited Create();
  FOnDisconnect := AOnDisconnect;
end;

destructor TLocalMessagery.TDisconnectable.Destroy;
begin
  Disconnect();
  inherited;
end;

procedure TLocalMessagery.TDisconnectable.Disconnect;
begin
  if Assigned(FOnDisconnect) then
    FOnDisconnect();
  FOnDisconnect := nil;
end;

{ TGlobalLocalMessagery }

class constructor TGlobalLocalMessagery.Create;
begin
  FInstance := TLocalMessagery.Create();
end;

class destructor TGlobalLocalMessagery.Destroy;
begin
  FInstance.Free();
end;

class function TGlobalLocalMessagery.BroadcastEvent(const AEvent: TEvent;
  const AOwned: boolean): boolean;
begin
  Result := TGlobalLocalMessagery.Instance.BroadcastEvent(AEvent, AOwned);
end;

class function TGlobalLocalMessagery.BroadcastEventAsync(
  const AEvent: TEvent; const AOwned: boolean): boolean;
begin
  Result := TGlobalLocalMessagery.Instance.BroadcastEventAsync(AEvent, AOwned);
end;

class procedure TGlobalLocalMessagery.SendRequest<T>(const ARequest: TRequest;
  const AResolve: TRequestResolve<T>; const AReject: TRequestReject;
  const AOwned: boolean);
begin
  TGlobalLocalMessagery.Instance.SendRequest<T>(ARequest, AResolve, AReject,
    AOwned);
end;

class procedure TGlobalLocalMessagery.SendRequestAsync<T>(
  const ARequest: TRequest; const AResolve: TRequestResolve<T>;
  const AReject: TRequestReject; const AOwned: boolean);
begin
  TGlobalLocalMessagery.Instance.SendRequestAsync<T>(ARequest, AResolve, AReject,
    AOwned);
end;

class function TGlobalLocalMessagery.SubscribeToEvent<T>(
  const AEventNotification: TEventNotification<T>): IDisconnectable;
begin
  Result := TGlobalLocalMessagery.Instance.SubscribeToEvent<T>(
    AEventNotification);
end;

class function TGlobalLocalMessagery.SubscribeToReverseRequest<T>(
  const AReverseRequestNotification: TReverseRequestNotification<T>): IDisconnectable;
begin
  Result := TGlobalLocalMessagery.Instance.SubscribeToReverseRequest<T>(
    AReverseRequestNotification);
end;

class procedure TGlobalLocalMessagery.Flush;
begin
  TGlobalLocalMessagery.Instance.Flush();
end;

end.
