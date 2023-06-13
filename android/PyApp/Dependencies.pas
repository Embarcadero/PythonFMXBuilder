unit Dependencies;

interface

uses
  System.Generics.Collections;

type
  /// <summary>
  ///   Dependency installation strategy
  /// </summary>
  IInstallDependency = interface
    ['{9E41532B-7EAE-4956-B228-3BD09EBF6BAD}']
    function CanHandle(AFilePath: string): boolean;
    function CanDelete(AFilePath: string): boolean;
    function IsInstalled(const AModuleName, AFilePath: string): boolean;
    function Install(const AModuleName, AFilePath: string): boolean;
  end;

  TBaseInstallDependency = class(TInterfacedObject)
  protected
    function ExecCmd(const AArgs: TArray<string>): integer;
    function PkgUtilCheckInstalled(const AModuleName, AFilePath: string): boolean;
  end;

  TInstallDependency = class(TInterfacedObject, IInstallDependency)
  private
    FStrategies: TList<IInstallDependency>;
    function CanHandle(AFilePath: string): boolean;
    function CanDelete(AFilePath: string): boolean;
    function IsInstalled(const AModuleName, AFilePath: string): boolean;
    function Install(const AModuleName, AFilePath: string): boolean;
  public
    constructor Create();
    destructor Destroy(); override;
  end;

implementation

uses
  System.SysUtils, System.IOUtils,
  PythonEngine,
  PyTools.ExecCmd, PyTools.ExecCmd.Args,
  Dependencies.PipWheel,
  Dependencies.Setup,
  Dependencies.ZipImports,
  Dependencies.ZipPackage;

{ TBaseInstallDependency }

function TBaseInstallDependency.ExecCmd(
  const AArgs: TArray<string>): integer;
var
  LOutput: string;
begin
  var LExec := TExecCmdService
    .Cmd(GetPythonEngine().ProgramName,
      TExecCmdArgs.BuildArgv(
        GetPythonEngine().ProgramName,
        AArgs),
      TExecCmdArgs.BuildEnvp(
        GetPythonEngine().PythonHome,
        GetPythonEngine().ProgramName,
        TPath.Combine(GetPythonEngine().DllPath, GetPythonEngine().DllName)))
    .Run(LOutput);
  Result := LExec.Wait();
end;

function TBaseInstallDependency.PkgUtilCheckInstalled(const AModuleName,
  AFilePath: string): boolean;
const
  CMD = 'import pkgutil; exit(0 if pkgutil.find_loader("%s") else 1)';
begin
  Result := ExecCmd(['-c', Format(CMD, [AModuleName])]) = EXIT_SUCCESS;
end;

{ TInstallDependency }

constructor TInstallDependency.Create;
begin
  inherited;
  FStrategies := TList<IInstallDependency>.Create();
  FStrategies.Add(TPipWheelInstallStrategy.Create());
  FStrategies.Add(TSetupInstallStrategy.Create());
  FStrategies.Add(TZipImportsInstallStrategy.Create());
  FStrategies.Add(TZipPackageInstallStrategy.Create());
end;

destructor TInstallDependency.Destroy;
begin
  FStrategies.DisposeOf();
  inherited;
end;

function TInstallDependency.CanDelete(AFilePath: string): boolean;
begin
  for var LStrategy in FStrategies do
    if LStrategy.CanHandle(AFilePath) then
      Exit(LStrategy.CanDelete(AFilePath));

  Result := true;
end;

function TInstallDependency.CanHandle(AFilePath: string): boolean;
begin
  for var LStrategy in FStrategies do
    if LStrategy.CanHandle(AFilePath) then
      Exit(true);

  Result := false;
end;

function TInstallDependency.Install(const AModuleName,
  AFilePath: string): boolean;
begin
  for var LStrategy in FStrategies do
    if LStrategy.CanHandle(AFilePath) then
      Exit(LStrategy.Install(AModuleName, AFilePath));
  
  Result := false;
end;

function TInstallDependency.IsInstalled(const AModuleName,
  AFilePath: string): boolean;
begin
  for var LStrategy in FStrategies do
    if LStrategy.CanHandle(AFilePath) then
      Exit(LStrategy.IsInstalled(AModuleName, AFilePath));
  
  Result := false;
end;

end.
