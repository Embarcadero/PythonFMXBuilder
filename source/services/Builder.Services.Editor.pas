unit Builder.Services.Editor;

interface

uses
  System.Classes,
  System.IOUtils,
  System.SysUtils,
  System.Types,
  Builder.Types,
  Builder.Chain,
  Builder.Services;

type
  TEditorService = class(TInterfacedObject, IEditorServices)
  private
    [weak]
    class var FEditorControl: IEditorControl;
    [weak]
    class var FActiveTextEditor: ITextEditor;
  private
    //Services
    FProjectServices: IProjectServices;
    procedure SetEditorControl(AEditorControl: IEditorControl);
    function GetEditorControl(): IEditorControl;
    procedure SetActiveTextEditor(ATextEditor: ITextEditor);
    function GetActiveTextEditor(): ITextEditor;
  public
    procedure AfterConstruction(); override;

    procedure OpenEditor(const AFilePath: string;
      const AEditing: boolean = false);
    procedure CloseEditor(const AFilePath: string;
      const ACheckEditing: boolean = true);
    procedure CloseAll(const ACheckEditing: boolean = true);
  end;

implementation

uses
  Builder.Services.Factory;

{ TEditorService }

procedure TEditorService.AfterConstruction;
begin
  inherited;
  FProjectServices := TServiceSimpleFactory.CreateProject();
end;

function TEditorService.GetActiveTextEditor: ITextEditor;
begin
  Result := FActiveTextEditor;
end;

function TEditorService.GetEditorControl: IEditorControl;
begin
  Result := FEditorControl;
end;

procedure TEditorService.SetActiveTextEditor(ATextEditor: ITextEditor);
begin
  FActiveTextEditor := ATextEditor;
end;

procedure TEditorService.SetEditorControl(AEditorControl: IEditorControl);
begin
  FEditorControl := AEditorControl;
end;

procedure TEditorService.OpenEditor(const AFilePath: string;
  const AEditing: boolean);
begin
  Assert(Assigned(FEditorControl), 'Field "FEditorControl" not assigned.');

  FEditorControl.OpenEditor(AFilePath, AEditing);

  TGlobalBuilderChain.BroadcastEventAsync(
    TOpenFileEvent.Create(AFilePath));
end;

procedure TEditorService.CloseEditor(const AFilePath: string;
  const ACheckEditing: boolean);
begin
  Assert(Assigned(FEditorControl), 'Field "FEditorControl" not assigned.');

  FEditorControl.CloseEditor(AFilePath);

  TGlobalBuilderChain.BroadcastEventAsync(
    TCloseFileEvent.Create(AFilePath));
end;

procedure TEditorService.CloseAll(const ACheckEditing: boolean);
begin
  Assert(Assigned(FEditorControl), 'Field "FEditorControl" not assigned.');

  FEditorControl.CloseAllEditors();
end;

end.
