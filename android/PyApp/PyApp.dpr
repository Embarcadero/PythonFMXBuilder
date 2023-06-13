program PyApp;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {PyMainForm},
  Dependencies.Setup in 'Dependencies.Setup.pas',
  Dependencies.PipWheel in 'Dependencies.PipWheel.pas',
  Dependencies in 'Dependencies.pas',
  Dependencies.ZipImports in 'Dependencies.ZipImports.pas',
  Dependencies.ZipPackage in 'Dependencies.ZipPackage.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TPyMainForm, PyMainForm);
  Application.Run;
end.

