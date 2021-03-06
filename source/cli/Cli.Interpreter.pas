unit Cli.Interpreter;

interface

uses
  System.SysUtils, System.StrUtils, System.Classes,
  VSoft.CommandLine.Options,
  Builder.Model, Builder.Model.Environment, Builder.Model.Project,
  Builder.PythonVersion, Builder.Architecture,
  System.Rtti;

type
  TCommandInterpreter = class
  private
    class procedure DoHelpCommand(const ACommand: string); static;
    class procedure DoCreateCommand(); static;
    class procedure DoListCommand(); static;
    class procedure DoRemoveCommand(); static;
    class procedure DoBuildCommand(); static;
    class procedure DoDeployCommand(); static;
    class procedure DoDeviceCommand(); static;
    class procedure DoEnvironmentCommand(); static;
    class procedure DoProjectCommand(); static;
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
  PyTools.ExecCmd,
  Builder.Exception,
  Builder.Services, Builder.Services.Factory,
  Builder.Storage.Default,
  Cli.Commands, Cli.Options, Cli.Exception;

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

class procedure TCommandInterpreter.DoHelpCommand(const ACommand: string);
begin
  TCommandInterpreter.PrintUsage(ACommand);
end;

class procedure TCommandInterpreter.DoCreateCommand;
begin
  var LService := TServiceSimpleFactory.CreateProject();
  var LProject := LService.CreateProject(
    TCreateOptions.ProjectNameCommand, TCreateOptions.AddMainScriptCommand);
  LService.SaveProject(LProject);
end;

class procedure TCommandInterpreter.DoListCommand;
begin
  var LService := TServiceSimpleFactory.CreateProject();
  for var LProjectName in LService.ListProjects() do begin
    WriteLn(LProjectName);
  end;
end;

class procedure TCommandInterpreter.DoRemoveCommand;
begin
  var LService := TServiceSimpleFactory.CreateProject();
  var LCanRemove := TRemoveOptions.SkipConfirmationCommand;

  if not LCanRemove then begin
    var Output: string;
    WriteLn(Format('Do you want to remove the %s project and all its data?', [TRemoveOptions.ProjectNameCommand]));
    ReadLn(Output);
    LCanRemove := (Output = 'yes') or (Output = 'y');
  end;

  if LCanRemove then begin
    try
      if not LService.RemoveProject(TRemoveOptions.ProjectNameCommand) then
        raise ERemoveProjectFailure.Create();
    except
      on E: Builder.Exception.EProjectNotFound do
        raise Cli.Exception.EProjectNotFound.Create(TRemoveOptions.ProjectNameCommand)
      else
        raise;
    end;
  end;
end;

class procedure TCommandInterpreter.DoBuildCommand;
begin
  if not InternalBuildApk(
    InternalLoadEnvironment(),
    InternalLoadProject(TBuildOptions.ProjectNameCommand),
    TBuildOptions.VerboseCommand) then
  begin
    raise EBuildProcessFailed.Create();
  end;
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
      raise ENoDevicesAttached.Create();

    if not LDevice.IsEmpty() and (LDeviceList.IndexOf(LDevice) < 0) then
      raise EDeviceNotAttached(LDevice);

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
    raise EBuildProcessFailed.Create();
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
      raise EDeployProcessFailed.Create();
  end;
end;

class procedure TCommandInterpreter.DoDeviceCommand;
begin
  var LEnvironmentModel := InternalLoadEnvironment();
  var LAdbService := TServiceSimpleFactory.CreateAdb();
  var LDeviceList := TStringList.Create();
  try
    LAdbService.ListDevices(LEnvironmentModel.AdbLocation, LDeviceList);
    if LDeviceList.Count = 0 then
      raise ENoDevicesAttached.Create();

    for var I := 0 to LDeviceList.Count - 1 do begin
      if TDeviceOptions.ListCommand then
        WriteLn(LDeviceList.KeyNames[I] + ' -> ' + LDeviceList.Values[LDeviceList.KeyNames[I]])
      else
        WriteLn(LDeviceList.KeyNames[I]);
    end;
  finally
    LDeviceList.Free();
  end;
end;

class procedure TCommandInterpreter.DoEnvironmentCommand;
begin
  if TEnvironmentOptions.Gui then begin
    var LOutput: string;
    if TExecCmdService.Cmd(GetGUIEntityEditorPath(), ['environment'])
      .Run(LOutput)
        .Wait() <> 0 then
          Writeln(LOutput);
    Exit;
  end;

  var LEnvironmentModel: TEnvironmentModel := nil;
  var LEnvironmentStorage := TDefaultStorage<TEnvironmentModel>.Make();

  if not LEnvironmentStorage.LoadModel(LEnvironmentModel) then
    LEnvironmentModel := TEnvironmentModel.Create();
  try
    if TEntityOptionsHelper.HasChanged(TEnvironmentOptions.SdkBasePathCommand) then
      LEnvironmentModel.SdkBasePath := TEnvironmentOptions.SdkBasePathCommand.AsString();

    if TEntityOptionsHelper.HasChanged(TEnvironmentOptions.ApkSignerLocationCommand) then
      LEnvironmentModel.ApkSignerLocation := TEnvironmentOptions.ApkSignerLocationCommand.AsString();

    if TEntityOptionsHelper.HasChanged(TEnvironmentOptions.AdbLocationCommand) then
      LEnvironmentModel.AdbLocation := TEnvironmentOptions.AdbLocationCommand.AsString();

    if TEntityOptionsHelper.HasChanged(TEnvironmentOptions.AptLocationCommand) then
      LEnvironmentModel.AAptLocation := TEnvironmentOptions.AptLocationCommand.AsString();

    if TEntityOptionsHelper.HasChanged(TEnvironmentOptions.SdkApiLocationCommand) then
      LEnvironmentModel.SdkApiLocation := TEnvironmentOptions.SdkApiLocationCommand.AsString();

    if TEntityOptionsHelper.HasChanged(TEnvironmentOptions.ZipAlignLocationCommand) then
      LEnvironmentModel.ZipAlignLocation := TEnvironmentOptions.ZipAlignLocationCommand.AsString();

    if TEntityOptionsHelper.HasChanged(TEnvironmentOptions.JdkBasePathCommand) then
      LEnvironmentModel.JdkBasePath := TEnvironmentOptions.JdkBasePathCommand.AsString();

    if TEntityOptionsHelper.HasChanged(TEnvironmentOptions.KeyToolLocationCommand) then
      LEnvironmentModel.KeyToolLocation := TEnvironmentOptions.KeyToolLocationCommand.AsString();

    if TEntityOptionsHelper.HasChanged(TEnvironmentOptions.JarSignerLocationCommand) then
      LEnvironmentModel.JarSignerLocation := TEnvironmentOptions.JarSignerLocationCommand.AsString();

    if TEnvironmentOptions.FindCommand then begin
      if LEnvironmentModel.SdkBasePath.Trim().IsEmpty() then
        raise EInvalidBasePath.Create(E_STR_INVALID_SDK_BASE_PATH, E_CODE_INVALID_SDK_BASE_PATH);

      if LEnvironmentModel.JdkBasePath.Trim().IsEmpty() then
        raise EInvalidBasePath.Create(E_STR_INVALID_JDK_BASE_PATH, E_CODE_INVALID_JDK_BASE_PATH);

      var LEditPredicate := function(const AValue: string): boolean
      begin
        Result := AValue.Trim().IsEmpty()
          or TEnvironmentOptions.OverrideCommand;
      end;

      if LEditPredicate(LEnvironmentModel.ApkSignerLocation) then
        LEnvironmentModel.ApkSignerLocation := TPathLocator.LoadToolPath(LEnvironmentModel.SdkBasePath, 'apksigner.jar');
      if LEditPredicate(LEnvironmentModel.AdbLocation) then
        LEnvironmentModel.AdbLocation := TPathLocator.LoadToolPath(LEnvironmentModel.SdkBasePath, 'adb.exe');
      if LEditPredicate(LEnvironmentModel.AAptLocation) then
        LEnvironmentModel.AAptLocation := TPathLocator.LoadToolPath(LEnvironmentModel.SdkBasePath, 'aapt.exe');
      if LEditPredicate(LEnvironmentModel.ZipAlignLocation) then
        LEnvironmentModel.ZipAlignLocation := TPathLocator.LoadToolPath(LEnvironmentModel.SdkBasePath, 'zipalign.exe');
      if LEditPredicate(LEnvironmentModel.SdkApiLocation) then
        LEnvironmentModel.SdkApiLocation := TPathLocator.FindSdkApiLocation(LEnvironmentModel.SdkBasePath);

      if LEditPredicate(LEnvironmentModel.KeyToolLocation) then
        LEnvironmentModel.KeyToolLocation := TPathLocator.LoadToolPath(LEnvironmentModel.JdkBasePath, 'keytool.exe');
      if LEditPredicate(LEnvironmentModel.JarSignerLocation) then
        LEnvironmentModel.JarSignerLocation := TPathLocator.LoadToolPath(LEnvironmentModel.JdkBasePath, 'jarsigner.exe');
    end;

    if TEnvironmentOptions.ShowCommand then begin
      Writeln(Format('--sdk_base_path %s', [LEnvironmentModel.SdkBasePath]));
      Writeln(Format('--apk_signer_location %s', [LEnvironmentModel.ApkSignerLocation]));
      Writeln(Format('--adb_location %s', [LEnvironmentModel.AdbLocation]));
      Writeln(Format('--apt_location %s', [LEnvironmentModel.AAptLocation]));
      Writeln(Format('--sdk_api_location %s', [LEnvironmentModel.SdkApiLocation]));
      Writeln(Format('--zip_align_location %s', [LEnvironmentModel.ZipAlignLocation]));
      Writeln(Format('--jdk_base_path %s', [LEnvironmentModel.JdkBasePath]));
      Writeln(Format('--key_tool_location %s', [LEnvironmentModel.KeyToolLocation]));
      Writeln(Format('--jar_signer_location %s', [LEnvironmentModel.JarSignerLocation]));
    end;

    LEnvironmentStorage.SaveModel(LEnvironmentModel);
  finally
    LEnvironmentModel.Free();
  end;
end;

class procedure TCommandInterpreter.DoProjectCommand;
begin
  if TProjectOptions.Gui then begin
    var LOutput: string;
    if TExecCmdService.Cmd(GetGUIEntityEditorPath(), [
      'project',
      '--name "' + TProjectOptions.SelectCommand + '"'])
      .Run()
        .Wait() <> 0 then
          Writeln(LOutput);
    Exit;
  end;

  var LProjectService := TServiceSimpleFactory.CreateProject();
  if not LProjectService.HasProject(TProjectOptions.SelectCommand) then
    raise Cli.Exception.EProjectNotFound.Create(TProjectOptions.SelectCommand);

  var LProjectModel := LProjectService.LoadProject(TProjectOptions.SelectCommand);
  try
    if TEntityOptionsHelper.HasChanged(TProjectOptions.PackageNameCommand) then
      LProjectModel.PackageName := TProjectOptions.PackageNameCommand.AsString();

    if TEntityOptionsHelper.HasChanged(TProjectOptions.VersionCodeCommand) then
      LProjectModel.VersionCode := TProjectOptions.VersionCodeCommand.AsInteger();

    if TEntityOptionsHelper.HasChanged(TProjectOptions.VersionNameCommand) then
      LProjectModel.VersionName := TProjectOptions.VersionNameCommand.AsString();

    if TEntityOptionsHelper.HasChanged(TProjectOptions.PythonVersionCommand) then
      try
        LProjectModel.PythonVersion := TPythonVersion.FromString(
          TProjectOptions.PythonVersionCommand.AsString());
      except
        on E: Builder.Exception.EInvalidPythonVersion do
          raise Cli.Exception.EInvalidPythonVersion.Create()
        else
          raise;
      end;

    if TEntityOptionsHelper.HasChanged(TProjectOptions.ArchitectureCommand) then
      try
        LProjectModel.Architecture := TArchitecture.FromString(
          TProjectOptions.ArchitectureCommand.AsString());
      except
        on E: Builder.Exception.EInvalidArchitecture do
          raise Cli.Exception.EInvalidArchitecture.Create()
        else
          raise;
      end;

    if TEntityOptionsHelper.HasChanged(TProjectOptions.DrawableSmallCommand) then
      LProjectModel.Icons.DrawableSmall := TProjectOptions.DrawableSmallCommand.AsString();

    if TEntityOptionsHelper.HasChanged(TProjectOptions.DrawableNormalCommand) then
      LProjectModel.Icons.DrawableNormal := TProjectOptions.DrawableNormalCommand.AsString();

    if TEntityOptionsHelper.HasChanged(TProjectOptions.DrawableLargeCommand) then
      LProjectModel.Icons.DrawableLarge := TProjectOptions.DrawableLargeCommand.AsString();

    if TEntityOptionsHelper.HasChanged(TProjectOptions.DrawableXLargeCommand) then
      LProjectModel.Icons.DrawableXlarge := TProjectOptions.DrawableXLargeCommand.AsString();

    if TEntityOptionsHelper.HasChanged(TProjectOptions.DrawableLDpiCommand) then
      LProjectModel.Icons.DrawableLdpi := TProjectOptions.DrawableLDpiCommand.AsString();

    if TEntityOptionsHelper.HasChanged(TProjectOptions.DrawableMDpiCommand) then
      LProjectModel.Icons.DrawableMdpi := TProjectOptions.DrawableMDpiCommand.AsString();

    if TEntityOptionsHelper.HasChanged(TProjectOptions.DrawableXHDpiCommand) then
      LProjectModel.Icons.DrawableHdpi := TProjectOptions.DrawableXHDpiCommand.AsString();

    if TEntityOptionsHelper.HasChanged(TProjectOptions.DrawableXHDpiCommand) then
      LProjectModel.Icons.DrawableXhdpi := TProjectOptions.DrawableXHDpiCommand.AsString();

    if TEntityOptionsHelper.HasChanged(TProjectOptions.DrawableXxHDpiCommand) then
      LProjectModel.Icons.DrawableXxhdpi := TProjectOptions.DrawableXxHDpiCommand.AsString();

    if TEntityOptionsHelper.HasChanged(TProjectOptions.DrawableXxxHDpiCommand) then
      LProjectModel.Icons.DrawableXxxHdpi := TProjectOptions.DrawableXxxHDpiCommand.AsString();

    for var LFile in TProjectOptions.AddFileCommand do begin
      LProjectService.AddScriptFile(LProjectModel, LFile);
    end;

    for var LFile in TProjectOptions.RemoveFileCommand do begin
      LProjectService.RemoveScriptFile(LProjectModel, LFile);
    end;

    if TEntityOptionsHelper.HasChanged(TProjectOptions.MainFileCommand) then
      LProjectService.SetMainScriptFile(LProjectModel, TProjectOptions.MainFileCommand.AsString());

    if TProjectOptions.ShowCommand then begin
      Writeln(Format('--application_name %s', [LProjectModel.ApplicationName]));
      Writeln(Format('--package_name %s', [LProjectModel.PackageName]));
      Writeln(Format('--version_code %d', [LProjectModel.VersionCode]));
      Writeln(Format('--version_name %s', [LProjectModel.VersionName]));
      Writeln(Format('--python_version %s', [LProjectModel.PythonVersion.AsString()]));
      Writeln(Format('--architecture %s', [LProjectModel.Architecture.AsString()]));

      Writeln(Format('--drawable_small %s', [LProjectModel.Icons.DrawableSmall]));
      Writeln(Format('--drawable_normal %s', [LProjectModel.Icons.DrawableNormal]));
      Writeln(Format('--drawable_large %s', [LProjectModel.Icons.DrawableLarge]));
      Writeln(Format('--drawable_xlarge %s', [LProjectModel.Icons.DrawableXlarge]));
      Writeln(Format('--drawable_ldpi %s', [LProjectModel.Icons.DrawableLdpi]));
      Writeln(Format('--drawable_mdpi %s', [LProjectModel.Icons.DrawableMdpi]));
      Writeln(Format('--drawable_hdpi %s', [LProjectModel.Icons.DrawableHdpi]));
      Writeln(Format('--drawable_xhdpi %s', [LProjectModel.Icons.DrawableXhdpi]));
      Writeln(Format('--drawable_xxhdpi %s', [LProjectModel.Icons.DrawableXxhdpi]));
      Writeln(Format('--drawable_xxxhdpi %s', [LProjectModel.Icons.DrawableXxxHdpi]));

      for var LFile in LProjectModel.Files.Files do begin
        Writeln(Format('--file %s', [LFile]));
      end;

      if LProjectModel.Files.Files.Count = 0 then
        Writeln('--file');

      Writeln(Format('--main_file %s', [LProjectModel.Files.MainFile]));
    end;

    LProjectService.SaveProject(LProjectModel);
  finally
    LProjectModel.Free();
  end;
end;

class procedure TCommandInterpreter.Interpret(
  const AParsed: ICommandLineParseResult);
const
  COMMANDS: array of string = [
    String.Empty,
    HELP_CMD,
    CREATE_CMD, LIST_CMD, REMOVE_CMD,
    BUILD_CMD, DEPLOY_CMD,
    DEVICE_CMD,
    ENVIRONMENT_CMD, PROJECT_CMD];
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
    4: DoRemoveCommand();
    5: DoBuildCommand();
    6: DoDeployCommand();
    7: DoDeviceCommand();
    8: DoEnvironmentCommand();
    9: DoProjectCommand();
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
    //Generates the project necessary files and settings
    LAppService.BuildProject(AProject);
    //Create and sign the APK file
    Result := LAppService.BuildApk(AProject, AEnvironment);
  end);
end;

class function TCommandInterpreter.InternalLoadEnvironment: TEnvironmentModel;
begin
  Result := nil;
  var LEnvironmentStorage := TDefaultStorage<TEnvironmentModel>.Make();
  if not LEnvironmentStorage.LoadModel(Result) then
    raise EEnvironmentSettingsAreEmpty.Create();

  var LModelErrors := TStringList.Create();
  try
    if not Result.Validate(LModelErrors) then
      raise EEnvironmentSettingsInvalidArgs.Create(
        sLineBreak
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
    raise EProjectSettingsAreEmpty.Create();

  Result := LProjectService.LoadProject(AProjectName);

  var LModelErrors := TStringList.Create();
  try
    if not Result.Validate(LModelErrors) then
      raise EProjectSettingsInvalidArgs.Create(
        sLineBreak
        + sLineBreak
        + LModelErrors.Text);
  finally
    LModelErrors.Free();
  end;
end;

end.
