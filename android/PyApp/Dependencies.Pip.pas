unit Dependencies.Pip;

interface

uses
  Dependencies,
  PyEnvironment,
  PyEnvironment.AddOn.EnsurePip;

type
  TPipInstallStrategy = class(TInterfacedObject, IInstallDependency)
  private
    function ExecCmd(const AArgs: TArray<string>): integer;
  public
    function IsInstalled(const AModuleName, AFilePath: string): boolean;
    function Install(const AModuleName, AFilePath: string): boolean;
  end;

implementation

uses
  System.SysUtils, System.IOUtils,
  PythonEngine,
  PyTools.ExecCmd, PyTools.ExecCmd.Args;

{ TPipInstallStrategy }

function TPipInstallStrategy.ExecCmd(const AArgs: TArray<string>): integer;
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

function TPipInstallStrategy.IsInstalled(const AModuleName, AFilePath: string): boolean;
const
  CMD = 'import pkgutil; exit(0 if pkgutil.find_loader("%s") else 1)';
begin
  Result := ExecCmd(['-c', Format(CMD, [AModuleName])]) = EXIT_SUCCESS;
end;

function TPipInstallStrategy.Install(const AModuleName, AFilePath: string): boolean;
begin
  Result := ExecCmd(['-m', 'pip', 'install', AFilePath]) = EXIT_SUCCESS;
end;

end.
