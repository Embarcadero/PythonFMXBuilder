unit Builder.Services.Editor;

interface

uses
  System.Classes,
  System.IOUtils,
  System.SysUtils,
  System.Types,
  Builder.Types,
  Builder.Services;

type
  TEditorService = class(TInterfacedObject, IEditorServices)
  private
    [weak]
    class var FActiveTextEditor: ITextEditor;
  private
    //Services
    FProjectServices: IProjectServices;
    procedure SetActiveTextEditor(ATextEditor: ITextEditor);
    function GetActiveTextEditor(): ITextEditor;
  public
    procedure AfterConstruction(); override;

    procedure SaveEditor(const ATextEditor: ITextEditor;
      const ASaveRequest: TSaveRequest; const ACheckUntracked: boolean = true);
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

procedure TEditorService.SaveEditor(const ATextEditor: ITextEditor;
  const ASaveRequest: TSaveRequest; const ACheckUntracked: boolean = true);
begin
  Assert(Assigned(ATextEditor), 'Argument "ATextEditor" not assigned.');
  Assert(Assigned(ASaveRequest), 'Argument "ASaveRequest" not assigned.');

  var LProject := FProjectServices.GetActiveProject();
  if not Assigned(LProject) then
    Exit;

  for var LModule in LProject.Files.Modules do begin
    if LModule.Path = ATextEditor.FileName then
      FProjectServices.SaveModule(
        LProject, LModule, ASaveRequest, ACheckUntracked);
  end;
end;

procedure TEditorService.SetActiveTextEditor(ATextEditor: ITextEditor);
begin
  FActiveTextEditor := ATextEditor;
end;

end.
