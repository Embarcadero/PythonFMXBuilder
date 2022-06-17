unit Frame.ProjectButtons;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, Container.Images, Builder.Services,
  Builder.Model.Project, Form.SelectProject, Form.Project.Create;

type
  TProjectButtonsFrame = class(TFrame)
    btnCreate: TButton;
    btnOpen: TButton;
    btnOptions: TButton;
    procedure btnCreateClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
  private
    FProjectRef: PProjectModel;
    FProjectServices: IProjectServices;
    function GetProjectServices: IProjectServices;
  public
    property ProjectRef: PProjectModel read FProjectRef write FProjectRef;
  end;

implementation

uses
  FMX.DialogService, System.UITypes,
  Builder.Services.Factory;

{$R *.fmx}

{ TProjectButtonsFrame }

procedure TProjectButtonsFrame.btnCreateClick(Sender: TObject);
begin
  var LProjectName: string;
  var LCreateMainFile: boolean;
  if TProjectCreateForm.CreateProject(LProjectName, LCreateMainFile) then begin
    FProjectRef^ := GetProjectServices().CreateProject(LProjectName,
      LCreateMainFile);
    GetProjectServices().SaveProject(FProjectRef^);
  end;
end;

procedure TProjectButtonsFrame.btnOpenClick(Sender: TObject);
begin
  var LProjects := GetProjectServices().ListProjects();
  if Length(LProjects) > 0 then begin
    var LSelected := TSelectProjectForm.Select(LProjects);
    if not LSelected.IsEmpty() then
      FProjectRef^ := GetProjectServices().LoadProject(LSelected);
  end else
    raise Exception.Create('Your workspace is empty. Try to create a new project.');
end;

function TProjectButtonsFrame.GetProjectServices: IProjectServices;
begin
  if not Assigned(FProjectServices) then
    FProjectServices := TServiceSimpleFactory.CreateProject();
  Result := FProjectServices;
end;

end.
