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
    FProjectServices: IProjectServices;
  public
    constructor Create(AOwner: TComponent); override;
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
    var LProjectModel := FProjectServices.CreateProject(LProjectName, LCreateMainFile);
    FProjectServices.SaveProject(LProjectModel);
  end;
end;

procedure TProjectButtonsFrame.btnOpenClick(Sender: TObject);
begin
  var LProjects := FProjectServices.ListProjects();
  if Length(LProjects) > 0 then begin
    var LSelected := TSelectProjectForm.Select(LProjects);
    if not LSelected.IsEmpty() then
      FProjectServices.LoadProject(LSelected);
  end else
    raise Exception.Create('Your workspace is empty. Try to create a new project.');
end;

constructor TProjectButtonsFrame.Create(AOwner: TComponent);
begin
  inherited;
  FProjectServices := TServiceSimpleFactory.CreateProject();
end;

end.
