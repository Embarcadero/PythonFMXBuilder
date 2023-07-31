unit Builder.Message.Request;

interface

uses
  Builder.Message,
  Builder.Types;

type
  {$SCOPEDENUMS ON}
  TDebuggerConnectionFrozenAction = (
    ForceDisconnection,
    TryAgain
  );
  {$SCOPEDENUMS OFF}

  [RequestType(TRequestType.DebuggerConnectionFrozen)]
  TDebuggerConnectionFrozenActionRequest = class(TRequest<TEmptyArguments>);

  TDebuggerConnectionFrozenActionResponseBody = class
  private
    FAction: TDebuggerConnectionFrozenAction;
  public
    property Action: TDebuggerConnectionFrozenAction read FAction write FAction;
  end;

  [RequestType(TRequestType.DebuggerConnectionFrozen)]
  TDebuggerConnectionFrozenActionResponse = class(TResponse<TDebuggerConnectionFrozenActionResponseBody>)
  public
    constructor Create(const AAction: TDebuggerConnectionFrozenAction); reintroduce;
  end;

  TEditorSaveRequesBody = class
  private
    FTextEditor: ITextEditor;
  public
    property TextEditor: ITextEditor read FTextEditor write FTextEditor;
  end;

  [RequestType(TRequestType.EditorSaveRequest)]
  TEditorSaveRequest = class(TRequest<TEditorSaveRequesBody>)
  public
    constructor Create(const ATextEditor: ITextEditor); reintroduce;
  end;

  TEditorSaveResponseBody = class
  private
    FSavedTo: string;
  public
    property SavedTo: string read FSavedTo write FSavedTo;
  end;

  [RequestType(TRequestType.EditorSaveRequest)]
  TEditorSaveResponse = class(TResponse<TEditorSaveResponseBody>)
  public
    constructor Create(const ASavedTo: string); reintroduce;
  end;

implementation

{ TDebuggerConnectionFrozenActionResponse }

constructor TDebuggerConnectionFrozenActionResponse.Create(
  const AAction: TDebuggerConnectionFrozenAction);
begin
  inherited Create();
  Success := true;
  Body.Action := AAction;
end;

{ TEditorSaveRequest }

constructor TEditorSaveRequest.Create(const ATextEditor: ITextEditor);
begin
  inherited Create();
  Arguments.TextEditor := ATextEditor;
end;

{ TEditorSaveResponse }

constructor TEditorSaveResponse.Create(const ASavedTo: string);
begin
  inherited Create();
  Success := true;
  Body.SavedTo := ASavedTo;
end;

end.
