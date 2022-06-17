program PythonFMXBuilderEntityEditor;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  FMX.Forms,
  VSoft.CommandLine.Options,
  Builder.Services,
  Builder.Services.Factory,
  Cli.Exception,
  Form.Environment in 'source\views\Form.Environment.pas',
  Form.Project in 'source\views\Form.Project.pas';

type
  TProjectOptions = class
  public
    class var
      ProjectNameCommand: string;
  end;

procedure ConfigureEnvironmentOptions();
begin
  var LCmd := TOptionsRegistry.RegisterCommand(
    'environment',
    String.Empty,
    'Edit the environment.',
    String.Empty,
    'environment [options]');

  LCmd.Examples.Add('environment --name my_project');
end;

procedure ConfigureProjectOptions();
begin
  var LCmd := TOptionsRegistry.RegisterCommand(
    'project',
    String.Empty,
    'Edit a project.',
    String.Empty,
    'project [options]');

  var LOption := LCmd.RegisterOption<string>(
    'name',
    String.Empty,
    'Project name.',
    procedure(const AValue: string) begin
      TProjectOptions.ProjectNameCommand := AValue;
    end);
  LOption.Required := true;

  LCmd.Examples.Add('project --name my_project');
end;

procedure ConfigureOptions();
begin
  TOptionsRegistry.DescriptionTab := 35;
  TOptionsRegistry.NameValueSeparator := ' ';

  ConfigureEnvironmentOptions();
  ConfigureProjectOptions();
end;

procedure ConfigureForm(const AForm: TForm);
begin
  AForm.Position := TFormPosition.MainFormCenter;
end;

begin
  ConfigureOptions();
  try
    var LParsed := TOptionsRegistry.Parse();
    if LParsed.HasErrors then begin
      Writeln;
      Writeln(LParsed.ErrorText);
      TOptionsRegistry.PrintUsage(String.Empty,
        procedure(const AValue: string) begin
          WriteLn(AValue);
        end);
      Exit;
    end;

    if (LParsed.Command = 'environment') then begin
      var LForm := TEnvironmentForm.Create(nil);
      try
        ConfigureForm(LForm);
        LForm.ShowModal();
      finally
        LForm.Free();
      end;
    end else if (LParsed.Command = 'project') then begin
      var LService := TServiceSimpleFactory.CreateProject();
      if not LService.HasProject(TProjectOptions.ProjectNameCommand) then
        raise EProjectNotFound.Create(TProjectOptions.ProjectNameCommand);

      var LForm := TProjectForm.Create(nil);
      try
        ConfigureForm(LForm);
        LForm.Id := TProjectOptions.ProjectNameCommand;
        LForm.ShowModal();
      finally
        LForm.Free();
      end;
    end;
  except
    on E: ECliException do begin
      WriteLn(E.Message);
      ExitCode := E.Code;
    end;
    on E: Exception do begin
      Writeln(E.Message);
      ExitCode := 1;
    end;
  end;
end.
