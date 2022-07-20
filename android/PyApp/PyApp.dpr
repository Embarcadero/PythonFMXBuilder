program PyApp;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {PyMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TPyMainForm, PyMainForm);
  Application.Run;
end.
