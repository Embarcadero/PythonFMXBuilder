program PyApp;
uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {PyMainForm},
  Dependencies.Setup in 'Dependencies.Setup.pas',
  Dependencies.Pip in 'Dependencies.Pip.pas',
  Dependencies in 'Dependencies.pas',
  Dependencies.SysPath in 'Dependencies.SysPath.pas';

{$R *.res}
begin
  Application.Initialize;
  Application.CreateForm(TPyMainForm, PyMainForm);
  Application.Run;
end.

