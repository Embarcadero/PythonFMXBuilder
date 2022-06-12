unit Test.Cli.Commands;

interface

uses
  DUnitX.TestFramework,
  Storage, Storage.Default,
  Services, Services.Factory,
  Model.Environment;

type
  [TestFixture]
  TTestCliCommands = class
  private
    FEnvironmentStorage: IStorage<TEnvironmentModel>;
    FProjectService: IProjectServices;
    FAppService: IAppServices;
    function GetCliExe(): string;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestHelp;
    [Test]
    procedure TestCreate;
    [Test]
    procedure TestList;
    [Test]
    procedure TestDevice;
    [Test]
    procedure TestEnvironment;
    [Test]
    procedure TestProject;
    [Test]
    procedure TestBuild;
    [Test]
    procedure TestDeploy;
  end;

implementation

uses
  System.IOUtils, System.SysUtils, System.StrUtils,
  PyTools.ExecCmd,
  Architecture;

const
  CLI_EXE = 'PythonFMXBuilderCLI.exe';
  TEST_APP = 'testcli';
  SDK_BASE_PATH = 'C:\Users\Public\Documents\Embarcadero\Studio\22.0\CatalogRepository\AndroidSDK-2525-22.0.44500.8973';
  JDK_BASE_PATH = 'C:\Program Files\AdoptOpenJDK\jdk-8.0.242.08-hotspot\bin';

function TTestCliCommands.GetCliExe: string;
begin
  Result := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), CLI_EXE)
end;

procedure TTestCliCommands.Setup;
begin
  FEnvironmentStorage := TDefaultStorage<TEnvironmentModel>.Make();
  FProjectService := TServiceSimpleFactory.CreateProject();
  FAppService := TServiceSimpleFactory.CreateApp();
end;

procedure TTestCliCommands.TearDown;
begin
  FAppService := nil;
  FProjectService := nil;
  FEnvironmentStorage := nil;
end;

procedure TTestCliCommands.TestHelp;
begin
  var LOutput: string;
  var LExitCode := TExecCmdService.Cmd(GetCliExe(), ['help']).Run(LOutput).Wait();
  Assert.AreEqual(0, LExitCode);

  LExitCode := TExecCmdService.Cmd(GetCliExe(), ['help build']).Run(LOutput).Wait();
  Assert.AreEqual(0, LExitCode);

  LExitCode := TExecCmdService.Cmd(GetCliExe(), ['help create']).Run(LOutput).Wait();
  Assert.AreEqual(0, LExitCode);

  LExitCode := TExecCmdService.Cmd(GetCliExe(), ['help deploy']).Run(LOutput).Wait();
  Assert.AreEqual(0, LExitCode);

  LExitCode := TExecCmdService.Cmd(GetCliExe(), ['help device']).Run(LOutput).Wait();
  Assert.AreEqual(0, LExitCode);

  LExitCode := TExecCmdService.Cmd(GetCliExe(), ['help environment']).Run(LOutput).Wait();
  Assert.AreEqual(0, LExitCode);

  LExitCode := TExecCmdService.Cmd(GetCliExe(), ['help list']).Run(LOutput).Wait();
  Assert.AreEqual(0, LExitCode);

  LExitCode := TExecCmdService.Cmd(GetCliExe(), ['help project']).Run(LOutput).Wait();
  Assert.AreEqual(0, LExitCode);
end;

procedure TTestCliCommands.TestCreate;
begin
  if not FProjectService.HasProject(TEST_APP) then begin
    var LOutput: string;
    var LExitCode := TExecCmdService.Cmd(GetCliExe(), [
      'create',
      '--name ',
      TEST_APP]).Run(LOutput).Wait();
    Assert.AreEqual(0, LExitCode);

    Assert.AreEqual(FProjectService.HasProject(TEST_APP), true);
  end;
end;

procedure TTestCliCommands.TestList;
begin
  var LOutput: string;
  var LExitCode := TExecCmdService.Cmd(GetCliExe(), ['list']).Run(LOutput).Wait();
  Assert.AreEqual(0, LExitCode);
end;

procedure TTestCliCommands.TestDevice;
begin
  var LOutput: string;
  var LExitCode := TExecCmdService.Cmd(GetCliExe(), ['device']).Run(LOutput).Wait();
  Assert.AreEqual(0, LExitCode);
end;

procedure TTestCliCommands.TestEnvironment;
begin
  var LEnvironmentModel: TEnvironmentModel := nil;
  var LOutput: string;
  var LExitCode := TExecCmdService.Cmd(GetCliExe(), [
    'environment',
    '--sdk_base_path "' + SDK_BASE_PATH + '"']).Run(LOutput).Wait();
  Assert.AreEqual(0, LExitCode);

  Assert.AreEqual(FEnvironmentStorage.LoadModel(LEnvironmentModel), true);
  Assert.AreEqual(LEnvironmentModel.SdkBasePath, SDK_BASE_PATH);

  //LExitCode := TExecCmdService.Cmd(GetCliExe(), [
  //  'environment',
  //  '--sdk_base_path "' + SDK_BASE_PATH + '"',
  //  '--jdk_base_path "' + JDK_BASE_PATH + '"',
  //  '-f']).Run(LOutput).Wait();
  //Assert.AreEqual(0, LExitCode);

  //Assert.AreEqual(FEnvironmentStorage.LoadModel(LEnvironmentModel), true);

  //LExitCode := TExecCmdService.Cmd(GetCliExe(), [
  //'environment',
  //'--sdk_base_path "' + SDK_BASE_PATH + '"',
  //'--jdk_base_path "' + JDK_BASE_PATH + '"',
  //'-f',
  //'-o']).Run(LOutput).Wait();
  //Assert.AreEqual(0, LExitCode);

  //Assert.AreEqual(FEnvironmentStorage.LoadModel(LEnvironmentModel), true);
end;

procedure TTestCliCommands.TestProject;
begin
  if not FProjectService.HasProject(TEST_APP) then begin
    var LOutput: string;
    var LExitCode := TExecCmdService.Cmd(GetCliExe(), [
      'create',
      '--name',
      TEST_APP]).Run(LOutput).Wait();
    Assert.AreEqual(0, LExitCode);
    Assert.AreEqual(FProjectService.HasProject(TEST_APP), true);
  end;

  var LEnvironmentModel: TEnvironmentModel := nil;
  var LOutput: string;
  var LExitCode := TExecCmdService.Cmd(GetCliExe(), [
    'project',
    'testcli',
    '--version_code 2',
    '--architecture arm64']).Run(LOutput).Wait();
  Assert.AreEqual(0, LExitCode);

  var LProjectModel := FProjectService.LoadProject(TEST_APP);
  Assert.AreEqual(LProjectModel.VersionCode, 2);
  Assert.AreEqual(Ord(LProjectModel.Architecture), Ord(TArchitecture.aarch64));
end;

procedure TTestCliCommands.TestBuild;
begin
  var LEnvironmentModel: TEnvironmentModel := nil;
  var LOutput: string;
  var LExitCode := TExecCmdService.Cmd(GetCliExe(), [
    'environment',
    '--sdk_base_path "' + SDK_BASE_PATH + '"',
    '--jdk_base_path "' + JDK_BASE_PATH + '"',
    '-f',
    '-o']).Run(LOutput).Wait();
  Assert.AreEqual(0, LExitCode);
  Assert.AreEqual(FEnvironmentStorage.LoadModel(LEnvironmentModel), true);

  if not FProjectService.HasProject(TEST_APP) then begin
    LExitCode := TExecCmdService.Cmd(GetCliExe(), [
      'create',
      '--name',
      TEST_APP]).Run(LOutput).Wait();
    Assert.AreEqual(0, LExitCode);
    Assert.AreEqual(FProjectService.HasProject(TEST_APP), true);
  end;

  LExitCode := TExecCmdService.Cmd(GetCliExe(), [
    'build',
    '--name',
    TEST_APP]).Run(LOutput).Wait();
  Assert.AreEqual(0, LExitCode);
end;

procedure TTestCliCommands.TestDeploy;
begin
  var LEnvironmentModel: TEnvironmentModel := nil;
  var LOutput: string;
  var LExitCode := TExecCmdService.Cmd(GetCliExe(), [
    'environment',
    '--sdk_base_path "' + SDK_BASE_PATH + '"',
    '--jdk_base_path "' + JDK_BASE_PATH + '"',
    '-f',
    '-o']).Run(LOutput).Wait();
  Assert.AreEqual(0, LExitCode);
  Assert.AreEqual(FEnvironmentStorage.LoadModel(LEnvironmentModel), true);

  if not FProjectService.HasProject(TEST_APP) then begin
    LExitCode := TExecCmdService.Cmd(GetCliExe(), [
      'create',
      '--name',
      TEST_APP]).Run(LOutput).Wait();
    Assert.AreEqual(0, LExitCode);
    Assert.AreEqual(FProjectService.HasProject(TEST_APP), true);
  end;

 LExitCode := TExecCmdService.Cmd(GetCliExe(), [
    'deploy',
    '--name',
    TEST_APP]).Run(LOutput).Wait();
  Assert.AreEqual(0, LExitCode);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestCliCommands);

end.
