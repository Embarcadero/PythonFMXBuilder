unit Builder.Messagery;

interface

uses
  Builder.Message,
  Builder.Message.Event,
  Builder.Message.Request,
  Builder.Message.Local;

type
  TRequestType = Builder.Message.TRequestType;
  TEventType = Builder.Message.TEventType;
  IDisconnectable = Builder.Message.IDisconnectable;

  //Message type
  TRequest = Builder.Message.TRequest;
  TResponse = Builder.Message.TResponse;
  TEvent = Builder.Message.TEvent;
  TResponseError = Builder.Message.TResponseError;

  //Messagery
  TMessagery = Builder.Message.Local.TGlobalLocalMessagery;

  //Events
  TAsyncOperation = Builder.Message.Event.TAsyncOperation;
  TDebugAction = Builder.Message.Event.TDebugAction;
  TMessageLevel = Builder.Message.Event.TMessageLevel;
  TSaveState = Builder.Message.Event.TSaveState;
  TMessageEvent = Builder.Message.Event.TMessageEvent;
  TOpenProjectEvent = Builder.Message.Event.TOpenProjectEvent;
  TCloseProjectEvent = Builder.Message.Event.TCloseProjectEvent;
  TOpenFileEvent = Builder.Message.Event.TOpenFileEvent;
  TCloseFileEvent = Builder.Message.Event.TCloseFileEvent;
  TRenameFileEvent = Builder.Message.Event.TRenameFileEvent;
  TEditorChangedEvent = Builder.Message.Event.TEditorChangedEvent;
  TAsyncOperationStartedEvent = Builder.Message.Event.TAsyncOperationStartedEvent;
  TAsyncOperationEndedEvent = Builder.Message.Event.TAsyncOperationEndedEvent;
  TAsyncExceptionEvent = Builder.Message.Event.TAsyncExceptionEvent;
  TDebugSessionStartedEvent = Builder.Message.Event.TDebugSessionStartedEvent;
  TDebugSessionStoppedEvent = Builder.Message.Event.TDebugSessionStoppedEvent;
  TSetupDebuggerEvent = Builder.Message.Event.TSetupDebuggerEvent;
  TSetupDebuggerDoneEvent = Builder.Message.Event.TSetupDebuggerDoneEvent;
  TDebugActionEvent = Builder.Message.Event.TDebugActionEvent;
  TSaveStateEvent = Builder.Message.Event.TSaveStateEvent;

  //Requests
  TDebuggerConnectionFrozenAction = Builder.Message.Request.TDebuggerConnectionFrozenAction;
  TDebuggerConnectionFrozenActionRequest = Builder.Message.Request.TDebuggerConnectionFrozenActionRequest;
  TDebuggerConnectionFrozenActionResponse = Builder.Message.Request.TDebuggerConnectionFrozenActionResponse;
  TEditorSaveRequest = Builder.Message.Request.TEditorSaveRequest;
  TEditorSaveResponse = Builder.Message.Request.TEditorSaveResponse;

implementation

end.
