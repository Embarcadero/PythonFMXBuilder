unit Builder.Message.Event;

interface

uses
  System.SysUtils,
  System.Classes,
  Builder.Types,
  Builder.Exception,
  BaseProtocol.Requests,
  BaseProtocol.Client,
  BaseProtocol.Types,
  Builder.Model.Project,
  Builder.Message;

type
  {$SCOPEDENUMS ON}
  TMessageLevel = (Descriptive, Explanatory);
  TDebugAction = (Start, Stop, Pause, StepIn, StepOver, StepOut, Continue);
  TSaveState = (Save, SaveAll);
  TAsyncOperation = (
    OpenProject,
    BuildProject,
    DeployProject,
    RunProject,
    DebugProject,
    StopProject);
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

  [EventType(TEventType.Message)]
  TMessageEvent = class(TEvent<TMessageEventBody>)
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

  [EventType(TEventType.OpenProject)]
  TOpenProjectEvent = class(TEvent<TOpenProjectBody>)
  public
    constructor Create(const AProject: TProjectModel); reintroduce;
  end;

  TCloseProjectBody = class
  private
    FProject: TProjectModel;
  public
    property Project: TProjectModel read FProject write FProject;
  end;

  [EventType(TEventType.CloseProject)]
  TCloseProjectEvent = class(TEvent<TCloseProjectBody>)
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

  [EventType(TEventType.OpenFile)]
  TOpenFileEvent = class(TEvent<TOpenFileBody>)
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
    FCheckEditing: boolean;
  public
    property FilePath: string read FFilePath write FFilePath;
    property CheckEditing: boolean read FCheckEditing write FCheckEditing;
  end;

  [EventType(TEventType.CloseFile)]
  TCloseFileEvent = class(TEvent<TCloseFileBody>)
  public
    constructor Create(const AFilePath: string;
      const ACheckEditing: boolean = true); reintroduce;
  end;

  TRenameFileBody = class
  private
    FOldFilePath: string;
    FNewFilePath: string;
  public
    property OldFilePath: string read FOldFilePath write FOldFilePath;
    property NewFilePath: string read FNewFilePath write FNewFilePath;
  end;

  [EventType(TEventType.RenameFile)]
  TRenameFileEvent = class(TEvent<TRenameFileBody>)
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

  [EventType(TEventType.EditorChanged)]
  TEditorChangedEvent = class(TEvent<TEditorChangedBody>)
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

  [EventType(TEventType.AsyncOperationStarted)]
  TAsyncOperationStartedEvent = class(TEvent<TAsyncOperationStartedBody>)
  public
    constructor Create(const AOperation: TAsyncOperation); reintroduce;
  end;

  TAsyncOperationEndedBody = class
  private
    FOperation: TAsyncOperation;
  public
    property Operation: TAsyncOperation read FOperation write FOperation;
  end;

  [EventType(TEventType.AsyncOperationEnded)]
  TAsyncOperationEndedEvent = class(TEvent<TAsyncOperationEndedBody>)
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

  [EventType(TEventType.AsyncException)]
  TAsyncExceptionEvent = class(TEvent<TAsyncExceptionBody>)
  public
    constructor Create(); override;
  end;

  TDebugSessionStartedEventBoby = class
  private
    FDebugger: TBaseProtocolClient;
  public
    property Debugger: TBaseProtocolClient read FDebugger write FDebugger;
  end;

  [EventType(TEventType.DebugSessionStarted)]
  TDebugSessionStartedEvent = class(TEvent<TDebugSessionStartedEventBoby>)
  public
    constructor Create(const ADebugger: TBaseProtocolClient); reintroduce;
  end;

  TDebugSessionStoppedEventBoby = class
  private
    FDebugger: TBaseProtocolClient;
  public
    property Debugger: TBaseProtocolClient read FDebugger write FDebugger;
  end;

  [EventType(TEventType.DebugSessionStopped)]
  TDebugSessionStoppedEvent = class(TEvent<TDebugSessionStoppedEventBoby>)
  public
    constructor Create(const ADebugger: TBaseProtocolClient); reintroduce;
  end;

  TSetupDebuggerEventBoby = class
  private
    FDebugger: TBaseProtocolClient;
  public
    property Debugger: TBaseProtocolClient read FDebugger write FDebugger;
  end;

  [EventType(TEventType.SetupDebugger)]
  TSetupDebuggerEvent = class(TEvent<TSetupDebuggerEventBoby>)
  public
    constructor Create(const ADebugger: TBaseProtocolClient); reintroduce;
  end;

  TSetupDebuggerDoneEventBoby = class
  private
    FDebugger: TBaseProtocolClient;
  public
    property Debugger: TBaseProtocolClient read FDebugger write FDebugger;
  end;

  [EventType(TEventType.SetupDebuggerDone)]
  TSetupDebuggerDoneEvent = class(TEvent<TSetupDebuggerDoneEventBoby>)
  public
    constructor Create(const ADebugger: TBaseProtocolClient); reintroduce;
  end;

  {|||| Debug Actions ||||}
  TDebugActionEventBody = class
  private
    FAction: TDebugAction;
  public
    property Action: TDebugAction read FAction write FAction;
  end;

  [EventType(TEventType.DebugAction)]
  TDebugActionEvent = class(TEvent<TDebugActionEventBody>)
  public
    constructor Create(const ADebugAction: TDebugAction); reintroduce;
  end;

  {|||| ------------- ||||}

  TSaveStateEventBoby = class
  private
    FSaveState: TSaveState;
  public
    property SaveState: TSaveState read FSaveState write FSaveState;
  end;

  [EventType(TEventType.SaveState)]
  TSaveStateEvent = class(TEvent<TSaveStateEventBoby>)
  public
    constructor Create(const ASaveState: TSaveState); reintroduce;
  end;

implementation

{ TMessageEvent }

constructor TMessageEvent.Create(const AMessage: string;
  const ALevel: TMessageLevel; const AClear: boolean);
begin
  inherited Create();
  Body.Message := AMessage;
  Body.Level := ALevel;
  Body.Clear := AClear;
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

constructor TCloseFileEvent.Create(const AFilePath: string;
  const ACheckEditing: boolean);
begin
  inherited Create();
  Body.FilePath := AFilePath;
  Body.CheckEditing := ACheckEditing;
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
