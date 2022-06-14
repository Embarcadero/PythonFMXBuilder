program PythonFMXBuilderGUI;

uses
  System.StartUpCopy,
  FMX.Forms,
  Form.Main in 'source\views\Form.Main.pas' {MainForm},
  From.Project in 'source\views\From.Project.pas' {ProjectForm},
  Form.Environment in 'source\views\Form.Environment.pas' {EnvironmentForm},
  Container.Images in 'source\containers\Container.Images.pas' {ImageContainer: TDataModule},
  Form.Data in 'source\views\Form.Data.pas' {DataForm},
  Form.Factory in 'source\views\Form.Factory.pas',
  Form.Slider in 'source\views\Form.Slider.pas',
  Form.Base in 'source\views\Form.Base.pas' {BaseForm},
  Frame.ProjectFiles in 'source\views\frame\Frame.ProjectFiles.pas' {ProjectFilesFrame: TFrame},
  Frame.ProjectButtons in 'source\views\frame\Frame.ProjectButtons.pas' {ProjectButtonsFrame: TFrame},
  Frame.Loading in 'source\views\frame\Frame.Loading.pas' {LoadingFrame: TFrame},
  Form.SelectProject in 'source\views\Form.SelectProject.pas' {SelectProjectForm},
  Frame.ScriptEditor in 'source\views\frame\Frame.ScriptEditor.pas' {ScriptEditorFrame: TFrame},
  Form.Project.Create in 'source\views\Form.Project.Create.pas' {ProjectCreateForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TImageContainer, ImageContainer);
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSelectProjectForm, SelectProjectForm);
  Application.CreateForm(TProjectCreateForm, ProjectCreateForm);
  Application.Run;
end.
