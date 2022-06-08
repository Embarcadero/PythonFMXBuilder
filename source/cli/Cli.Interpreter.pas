unit Cli.Interpreter;

interface

uses
  System.SysUtils, System.StrUtils, System.Classes,
  VSoft.CommandLine.Options,
  Model, Model.Environment, Model.Project;

type
  TCommandInterpreter = class
  private
    class procedure DoHelpCommand(const ACommand: string); static;
    class procedure DoCreateCommand(); static;
    class procedure DoListCommand(); static;
    class procedure DoBuildCommand(); static;
    class procedure DoDeployCommand(); static;
  private
    class procedure PrintUsage(const ACommand: string); static;
    class function ExecuteAction(const AVerbose: boolean;
      const AAction: TFunc<boolean>): boolean;
  private
    class function InternalLoadEnvironment(): TEnvironmentModel;
    class function InternalLoadProject(const AProjectName: string): TProjectModel;
    class function InternalBuildApk(const AEnvironment: TEnvironmentModel;
      const AProject: TProjectModel; const AVerbose: boolean): boolean;
  public
    class procedure Interpret(const AParsed: ICommandLineParseResult); static;
  end;

implementation

uses
  Services, Services.Factory,
  Storage.Default,
  Cli.Commands, Cli.Options;

type
  TCliServices = class(TInterfacedObject, IServices, ILogServices)
  public
    procedure Log(const AString: string);
  end;

{ TCliServices }

procedure TCliServices.Log(const AString: string);
begin
  WriteLn(AString);
end;

{ TCommandInterpreter }

class procedure TCommandInterpreter.DoHelpCommand(const ACommand: string);
begin
  TCommandInterpreter.PrintUsage(ACommand);
end;

class procedure TCommandInterpreter.DoCreateCommand;
begin
  var LService := TServiceSimpleFactory.CreateProject();
  var LProject := LService.CreateProject(TCreateOptions.ProjectNameCommand);
  LService.SaveProject(LProject);
end;

class procedure TCommandInterpreter.DoListCommand;
begin
  var LService := TServiceSimpleFactory.CreateProject();
  for var LProjectName in LService.ListProjects() do begin
    WriteLn(LProjectName);
  end;
end;

class function TCommandInterpreter.ExecuteAction(
  const AVerbose: boolean; const AAction: TFunc<boolean>): boolean;
begin
  if AVerbose then begin
    GlobalServices := TCliServices.Create();
    try
      Result := AAction();
    finally
      GlobalServices := nil;
    end;
  end else
    Result := AAction();
end;

class procedure TCommandInterpreter.DoBuildCommand;
begin
  if not InternalBuildApk(
    InternalLoadEnvironment(),
    InternalLoadProject(TBuildOptions.ProjectNameCommand),
    TBuildOptions.VerboseCommand) then
  begin
    WriteLn('Build process has failed.')
  end else
    WriteLn('Build process has completed.');
end;

class procedure TCommandInterpreter.DoDeployCommand;
begin
  var LEnvironmentModel := InternalLoadEnvironment();
  var LProjectModel := InternalLoadProject(TDeployOptions.ProjectNameCommand);
  var LAdbService := TServiceSimpleFactory.CreateAdb();
  var LDevice := TDeployOptions.DeviceCommand;

  var LDeviceList := TStringList.Create();
  try
    LAdbService.ListDevices(LEnvironmentModel.AdbLocation, LDeviceList);
    if LDeviceList.Count = 0 then
      raise Exception.Create('No devices attached.');

    if not LDevice.IsEmpty() and (LDeviceList.IndexOf(LDevice) < 0) then
      raise Exception.CreateFmt('Device %s not attached.', [LDevice]);

    if LDevice.IsEmpty() then
      LDevice := LDeviceList.KeyNames[0];
  finally
    LDeviceList.Free();
  end;

  if not InternalBuildApk(
    LEnvironmentModel,
    LProjectModel,
    TDeployOptions.VerboseCommand) then
  begin
    WriteLn('Build process has failed.')
  end else begin
    var LSuccess := ExecuteAction(TDeployOptions.VerboseCommand,
      function(): boolean
      begin
        WriteLn('Starting deployment.');
        WriteLn;

        var LAppService := TServiceSimpleFactory.CreateApp();
        if TDeployOptions.UninstallCommand then
          LAppService.UnInstallApk(LProjectModel, LEnvironmentModel, LDevice);
        Result := LAppService.InstallApk(LProjectModel, LEnvironmentModel, LDevice);

        if Result then begin
          WriteLn('Launching...');
          WriteLn;
          var LStrings := TStringList.Create();
          try
            LAdbService.RunApp(
              LEnvironmentModel.AdbLocation, LProjectModel.PackageName,
              LDevice, LStrings);
          finally
            LStrings.Free();
          end;
        end;
      end);

    if not LSuccess then
      WriteLn('Deploy process has failed.');
  end;
end;

class procedure TCommandInterpreter.Interpret(
  const AParsed: ICommandLineParseResult);
const
  COMMANDS: array of string = [
    String.Empty, HELP_CMD, CREATE_CMD, LIST_CMD, BUILD_CMD, DEPLOY_CMD];
begin
  if AParsed.HasErrors then begin
    Writeln;
    Writeln(AParsed.ErrorText);
    TCommandInterpreter.PrintUsage(AParsed.Command);
    Exit;
  end;

  case IndexStr(AParsed.Command, COMMANDS) of
    0,
    1: DoHelpCommand(AParsed.Command);
    2: DoCreateCommand();
    3: DoListCommand();
    4: DoBuildCommand();
    5: DoDeployCommand();
  end;
end;

class procedure TCommandInterpreter.PrintUsage(const ACommand: string);
begin
  if (ACommand = 'help') then begin
    if THelpOptions.HelpCommand = '' then
      THelpOptions.HelpCommand := 'help';
  end else
    THelpOptions.HelpCommand := ACommand;

  TOptionsRegistry.PrintUsage(THelpOptions.HelpCommand,
    procedure(const AValue: string) begin
      if THelpOptions.HelpCommand.IsEmpty() and AValue.Trim().IsEmpty() then
        Exit;
      WriteLn(AValue);
    end);
end;

class function TCommandInterpreter.InternalBuildApk(
  const AEnvironment: TEnvironmentModel; const AProject: TProjectModel;
  const AVerbose: boolean): boolean;
begin
  Result := ExecuteAction(AVerbose, function(): boolean begin
    WriteLn('Starting up the build process. It may take some time...');
    WriteLn;
    var LAppService := TServiceSimpleFactory.CreateApp();
    //Copy Python and other APP files
    LAppService.CopyAppFiles(AProject);
    //Copy icons
    LAppService.CopyIcons(AProject);
    //Save aditional scripts to the APP files
    LAppService.CopyScriptFiles(AProject);
    //Update the manifest with the custom APP settings
    LAppService.UpdateManifest(AProject);
    //Create and sign the APK file
    Result := LAppService.BuildApk(AProject, AEnvironment);
  end);
end;

class function TCommandInterpreter.InternalLoadEnvironment: TEnvironmentModel;
begin
  Result := nil;
  var LEnvironmentStorage := TDefaultStorage<TEnvironmentModel>.Make();
  if not LEnvironmentStorage.LoadModel(Result) then
    raise Exception.Create('The Environment Settings are empty.');

  var LModelErrors := TStringList.Create();
  try
    if not Result.Validate(LModelErrors) then
      raise EModelValidationError.Create('The Environment Settings has invalid arguments:'
        + sLineBreak
        + sLineBreak
        + LModelErrors.Text);
  finally
    LModelErrors.Free();
  end;
end;

class function TCommandInterpreter.InternalLoadProject(
  const AProjectName: string): TProjectModel;
begin
  var LProjectService := TServiceSimpleFactory.CreateProject();
  if not LProjectService.HasProject(AProjectName) then
    raise Exception.Create('The Project Settings are empty.');

  Result := LProjectService.LoadProject(AProjectName);

  var LModelErrors := TStringList.Create();
  try
    if not Result.Validate(LModelErrors) then
      raise EModelValidationError.Create('The Project Settings has invalid arguments:'
        + sLineBreak
        + sLineBreak
        + LModelErrors.Text);
  finally
    LModelErrors.Free();
  end;
end;

end.
