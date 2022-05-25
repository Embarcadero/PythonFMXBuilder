program PythonFMXBuilder;

uses
  System.StartUpCopy,
  FMX.Forms,
  Form.Main in 'source\views\Form.Main.pas' {MainForm},
  From.Project in 'source\views\From.Project.pas' {ProjectForm},
  Form.Environment in 'source\views\Form.Environment.pas' {EnvironmentForm},
  Container.Images in 'source\containers\Container.Images.pas' {ImageContainer: TDataModule},
  Model.Environment in 'source\models\Model.Environment.pas',
  Storage in 'source\storage\Storage.pas',
  Storage.Json in 'source\storage\Storage.Json.pas',
  Storage.Default in 'source\storage\Storage.Default.pas',
  Form.Data in 'source\views\Form.Data.pas' {DataForm},
  Form.Factory in 'source\views\Form.Factory.pas',
  Form.Slider in 'source\views\Form.Slider.pas',
  Services in 'source\services\Services.pas',
  {$IFDEF MSWINDOWS}
  Services.ADB.Win in 'source\services\Services.ADB.Win.pas',
  {$ELSE}
  Services.ADB.Posix in 'source\services\Services.ADB.Posix.pas',
  {$ENDIF }
  Services.ADB in 'source\services\Services.ADB.pas' {S},
  Model in 'source\models\Model.pas',
  Form.Base in 'source\views\Form.Base.pas' {BaseForm},
  Services.App in 'source\services\Services.App.pas',
  Architecture in 'source\Architecture.pas',
  PythonVersion in 'source\PythonVersion.pas',
  Storage.Environment in 'source\storage\Storage.Environment.pas',
  Storage.Factory in 'source\storage\Storage.Factory.pas',
  Services.Factory in 'source\services\Services.Factory.pas',
  Frame.Loading in 'source\views\Frame.Loading.pas' {LoadingFrame: TFrame},
  Model.Project.Icon in 'source\models\project\Model.Project.Icon.pas',
  Model.Project in 'source\models\project\Model.Project.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TImageContainer, ImageContainer);
  Application.Run;
end.
