unit Cli.Interpreter;

interface

uses
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.Rtti,
  VSoft.CommandLine.Options,
  Builder.Types,
  Builder.Chain,
  Builder.Model,
  Builder.Model.Environment,
  Builder.Model.Project,
  Builder.Services,
  Builder.Storage;

type
  TCommandInterpreter = class
  private class var
    FInstance: TCommandInterpreter;
  private
    class constructor Create();
    class destructor Destroy();
  private
    FVerbose: boolean;
    //Services
    FProjectServices: IProjectServices;
    FAdbServices: IAdbServices;
    FBuildServices: IBuildServices;
    FUnboundPyServices: IUnboundPythonServices;
    //Storage
    FEnvironmentStorage: IStorage<TEnvironmentModel>;
    //Builder Chain
    FMessageEvent: IDisconnectable;
  private
    procedure DoHelpCommand(const ACommand: string);
    procedure DoCreateCommand();
    procedure DoListCommand();
    procedure DoRemoveCommand();
    procedure DoBuildCommand();
    procedure DoDeployCommand();
    procedure DoRunCommand();
    procedure DoStopCommand();
    procedure DoDeviceCommand();
    procedure DoEnvironmentCommand();
    procedure DoProjectCommand();
    procedure DoUnboundPyCommand();
  private
    procedure PrintUsage(const ACommand: string);
  private
    function InternalLoadEnvironment(): TEnvironmentModel;
    function InternalLoadProject(const AProjectName: string): TProjectModel;
    function GetSetDeviceOrAutoDetect(const ADevice: string): string;
  public
    constructor Create();
    destructor Destroy(); override;

    class procedure Interpret(const AParsed: ICommandLineParseResult); static;
  end;

implementation

uses
  PyTools.ExecCmd,
  Builder.Exception,
  Builder.Services.Factory,
  Builder.Storage.Default,
  Cli.Commands,
  Cli.Options,
  Cli.Exception;

{ TCommandInterpreter }

constructor TCommandInterpreter.Create;
begin
  FVerbose := false;
  FProjectServices := TServiceSimpleFactory.CreateProject();
  FAdbServices := TServiceSimpleFactory.CreateAdb();
  FBuildServices := TServiceSimpleFactory.CreateBuild();
  FUnboundPyServices := TServiceSimpleFactory.CreateUnboundPy();
  FEnvironmentStorage := TDefaultStorage<TEnvironmentModel>.Make();
  FMessageEvent := TGlobalBuilderChain.SubscribeToEvent<TMessageEvent>(
    procedure(const AEventNotification: TMessageEvent)
    begin
      if FVerbose or (AEventNotification.Body.Level = TMessageLevel.Descriptive) then
        Writeln(AEventNotification.Body.Message);
    end);
end;

destructor TCommandInterpreter.Destroy;
begin
  FMessageEvent.Disconnect()
end;

class constructor TCommandInterpreter.Create;
begin
  FInstance := TCommandInterpreter.Create();
end;

class destructor TCommandInterpreter.Destroy;
begin
  FInstance.Free();
end;

function TCommandInterpreter.GetSetDeviceOrAutoDetect(
  const ADevice: string): string;
begin
  var LEnvironmentModel := InternalLoadEnvironment();
  try
    var LDeviceList := TStringList.Create();
    try
      FAdbServices.ListDevices(LDeviceList);
      if LDeviceList.Count = 0 then
        raise ENoDevicesAttached.Create();

      if not ADevice.IsEmpty() and (LDeviceList.IndexOf(ADevice) < 0) then
        raise EDeviceNotAttached(ADevice);

      if ADevice.IsEmpty() then
        Result := LDeviceList.KeyNames[0]
      else
        Result := ADevice;
    finally
      LDeviceList.Free();
    end;
  finally
    LEnvironmentModel.Free();
  end;
end;

procedure TCommandInterpreter.DoHelpCommand(const ACommand: string);
begin
  PrintUsage(ACommand);
end;

procedure TCommandInterpreter.DoCreateCommand;
begin
  var LProjectModel := FProjectServices.CreateProject(
    TCreateOptions.ProjectNameCommand, TCreateOptions.AddMainScriptCommand);
  FProjectServices.SaveProject(LProjectModel);
end;

procedure TCommandInterpreter.DoListCommand;
begin
  for var LProjectName in FProjectServices.ListProjects() do begin
    WriteLn(LProjectName);
  end;
end;

procedure TCommandInterpreter.DoRemoveCommand;
begin
  var LCanRemove := TRemoveOptions.SkipConfirmationCommand;

  if not LCanRemove then begin
    var Output: string;
    WriteLn(Format('Do you want to remove the %s project and all its data?', [TRemoveOptions.ProjectNameCommand]));
    ReadLn(Output);
    LCanRemove := (Output = 'yes') or (Output = 'y');
  end;

  if LCanRemove then
    if not FProjectServices.RemoveProject(TRemoveOptions.ProjectNameCommand) then
      raise ERemoveProjectFailure.Create();
end;

procedure TCommandInterpreter.DoBuildCommand;
begin
  FVerbose := TBuildOptions.VerboseCommand;
  try
    InternalLoadProject(TBuildOptions.ProjectNameCommand);
    try
      FBuildServices.Run(procedure(AProxy: IBuilderTasks) begin
        FProjectServices.GetActiveProject().Debugger := TDebugger.FromString(TGlobalOptions.DebuggerCommand);
        AProxy.BuildActiveProject();
      end);

      TGlobalBuilderChain.Flush();
    except
      Exception.RaiseOuterException(EBuildProcessFailed.Create());
    end;
  finally
    FVerbose := false;
  end;
end;

procedure TCommandInterpreter.DoDeployCommand;
begin
  FVerbose := TDeployOptions.VerboseCommand;
  try
    InternalLoadProject(TDeployOptions.ProjectNameCommand);
    try
      FAdbServiceS.ActiveDevice := GetSetDeviceOrAutoDetect(
        TDeployOptions.DeviceCommand);

      FBuildServices.Run(procedure(AProxy: IBuilderTasks) begin
        AProxy.DeployActiveProject(TDeployOptions.UninstallCommand);
      end);

      TGlobalBuilderChain.Flush();
    except
      Exception.RaiseOuterException(EDeployProcessFailed.Create());
    end;
  finally
    FVerbose := false;
  end;
end;

procedure TCommandInterpreter.DoRunCommand;
begin
  FVerbose := TRunOptions.VerboseCommand;
  try
    InternalLoadProject(TRunOptions.ProjectNameCommand);
    try
      FAdbServices.ActiveDevice := GetSetDeviceOrAutoDetect(
        TRunOptions.DeviceCommand);

      FBuildServices.Run(procedure(AProxy: IBuilderTasks) begin
        if TRunOptions.DebugModeCommand then
          FProjectServices.GetActiveProject().BuildConfiguration := TBuildConfiguration.Debug
        else
          FProjectServices.GetActiveProject().BuildConfiguration := TBuildConfiguration.Release;

        AProxy.RunActiveProject();
      end);

      TGlobalBuilderChain.Flush();
    except
      Exception.RaiseOuterException(ERunProcessFailed.Create());
    end;
  finally
    FVerbose := false;
  end;
end;

procedure TCommandInterpreter.DoStopCommand;
begin
  InternalLoadProject(TStopOptions.ProjectNameCommand);
  try
    FAdbServices.ActiveDevice := GetSetDeviceOrAutoDetect(
      TStopOptions.DeviceCommand);

    FBuildServices.Run(procedure(AProxy: IBuilderTasks) begin
      AProxy.StopActiveProject();
    end);

    TGlobalBuilderChain.Flush();
  except
    Exception.RaiseOuterException(EStopProcessFailed.Create());
  end;
end;

procedure TCommandInterpreter.DoDeviceCommand;
begin
  var LEnvironmentModel := InternalLoadEnvironment();
  try
    var LDeviceList := TStringList.Create();
    try
      FAdbServices.ListDevices(LDeviceList);
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
  finally
    LEnvironmentModel.Free();
  end;
end;

procedure TCommandInterpreter.DoEnvironmentCommand;
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

  if not FEnvironmentStorage.LoadModel(LEnvironmentModel) then
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

    FEnvironmentStorage.SaveModel(LEnvironmentModel);
  finally
    LEnvironmentModel.Free();
  end;
end;

procedure TCommandInterpreter.DoProjectCommand;
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

  if not FProjectServices.HasProject(TProjectOptions.SelectCommand) then
    raise Cli.Exception.EProjectNotFound.Create(TProjectOptions.SelectCommand);

  var LProjectModel := FProjectServices.LoadProject(TProjectOptions.SelectCommand);
  if TEntityOptionsHelper.HasChanged(TProjectOptions.ApplicationNameCommand) then
    LProjectModel.ApplicationName := TProjectOptions.ApplicationNameCommand.AsString();

  if TEntityOptionsHelper.HasChanged(TProjectOptions.PackageNameCommand) then
    LProjectModel.PackageName := TProjectOptions.PackageNameCommand.AsString();

  if TEntityOptionsHelper.HasChanged(TProjectOptions.VersionCodeCommand) then
    LProjectModel.VersionCode := TProjectOptions.VersionCodeCommand.AsInteger();

  if TEntityOptionsHelper.HasChanged(TProjectOptions.VersionNameCommand) then
    LProjectModel.VersionName := TProjectOptions.VersionNameCommand.AsString();

  if TEntityOptionsHelper.HasChanged(TProjectOptions.PythonVersionCommand) then
    LProjectModel.PythonVersion := TPythonVersion.FromString(
      TProjectOptions.PythonVersionCommand.AsString());

  if TEntityOptionsHelper.HasChanged(TProjectOptions.ArchitectureCommand) then
    LProjectModel.Architecture := TArchitecture.FromString(
      TProjectOptions.ArchitectureCommand.AsString());

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
    FProjectServices.AddScriptFile(LProjectModel, LFile);
  end;

  for var LFile in TProjectOptions.RemoveFileCommand do begin
    FProjectServices.RemoveScriptFile(LProjectModel, LFile);
  end;

  for var LDependency in TProjectOptions.AddDependencyCommand do begin
    FProjectServices.AddDependency(LProjectModel, LDependency);
  end;

  for var LDependency in TProjectOptions.RemoveDependencyCommand do begin
    FProjectServices.RemoveDependency(LProjectModel, LDependency);
  end;

  if TEntityOptionsHelper.HasChanged(TProjectOptions.MainFileCommand) then
    FProjectServices.SetMainScriptFile(LProjectModel, TProjectOptions.MainFileCommand.AsString());

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

    for var LDependency in LProjectModel.Files.Dependencies do begin
      Writeln(Format('--dependency %s', [LDependency]));
    end;

    if LProjectModel.Files.Dependencies.Count = 0 then
      Writeln('--dependency');
  end;

  FProjectServices.SaveProject(LProjectModel);
end;

procedure TCommandInterpreter.DoUnboundPyCommand;
var
  LPythonVersion: TPythonVersion;
  LArchitecture: TArchitecture;
  LBuildConfiguration: TBuildConfiguration;
begin
  FAdbServices.ActiveDevice := GetSetDeviceOrAutoDetect(
    TUnboundPyOptions.DeviceCommand);

  LPythonVersion := TPythonVersion.FromString(TUnboundPyOptions.PythonVersionCommand.AsString());
  LArchitecture := TArchitecture.FromString(TUnboundPyOptions.ArchitectureCommand.AsString());
  LBuildConfiguration := TBuildConfiguration.Release;
  if TUnboundPyOptions.DebugModeCommand then
    LBuildConfiguration := TBuildConfiguration.Debug;

  if TUnboundPyOptions.CleanCommand then
    FUnboundPyServices.Remove(LPythonVersion, LArchitecture);

  if not FUnboundPyServices.Exists(LPythonVersion, LArchitecture) then
    FUnboundPyServices.Make(LPythonVersion, LArchitecture);

  FUnboundPyServices.Run(LPythonVersion, LArchitecture,
    TDebugger.FromString(TGlobalOptions.DebuggerCommand),
    LBuildConfiguration);
end;

procedure TCommandInterpreter.PrintUsage(const ACommand: string);
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

function TCommandInterpreter.InternalLoadEnvironment: TEnvironmentModel;
begin
  Result := nil;
  if not FEnvironmentStorage.LoadModel(Result) then
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

function TCommandInterpreter.InternalLoadProject(
  const AProjectName: string): TProjectModel;
begin
  if not FProjectServices.HasProject(AProjectName) then
    raise EProjectSettingsAreEmpty.Create();

  Result := FProjectServices.LoadProject(AProjectName);

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

class procedure TCommandInterpreter.Interpret(
  const AParsed: ICommandLineParseResult);
const
  COMMANDS: array of string = [
    String.Empty, HELP_CMD,
    CREATE_CMD, LIST_CMD, REMOVE_CMD,
    BUILD_CMD, DEPLOY_CMD, RUN_CMD, STOP_CMD,
    DEVICE_CMD, UNBOUNDPY_CMD,
    ENVIRONMENT_CMD, PROJECT_CMD];
begin
  if AParsed.HasErrors then begin
    Writeln;
    Writeln(AParsed.ErrorText);
    FInstance.PrintUsage(AParsed.Command);
    Exit;
  end;

  try
    case IndexStr(AParsed.Command, COMMANDS) of
       0,
       1: FInstance.DoHelpCommand(AParsed.Command);
       2: FInstance.DoCreateCommand();
       3: FInstance.DoListCommand();
       4: FInstance.DoRemoveCommand();
       5: FInstance.DoBuildCommand();
       6: FInstance.DoDeployCommand();
       7: FInstance.DoRunCommand();
       8: FInstance.DoStopCommand();
       9: FInstance.DoDeviceCommand();
      10: FInstance.DoUnboundPyCommand();
      11: FInstance.DoEnvironmentCommand();
      12: FInstance.DoProjectCommand();
    end;
  except
    on E: Builder.Exception.EInvalidArchitecture do
        raise Cli.Exception.EInvalidArchitecture.Create();
    on E: Builder.Exception.EInvalidPythonVersion do
      raise Cli.Exception.EInvalidPythonVersion.Create();
    on E: Exception do
      raise;
  end;

  TGlobalBuilderChain.Flush();
end;

end.
