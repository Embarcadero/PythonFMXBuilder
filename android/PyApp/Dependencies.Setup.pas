unit Dependencies.Setup;

interface

uses
  System.JSON, System.Threading,
  Dependencies;

type
  TSetupInstallStrategy = class(TInterfacedObject, IInstallDependency)
  private
    FDependencies: TJSONArray;
    FUnzipTask: ITask;
  public
    constructor Create(const ADependencies: TJSONArray);
    destructor Destroy(); override;

    function IsInstalled(const AModuleName, AFilePath: string): boolean;
    function Install(const AModuleName, AFilePath: string): boolean;
  end;

implementation

uses
  System.SysUtils, System.IOUtils, System.Zip, System.Classes,
  PythonEngine,
  PyTools.ExecCmd,
  PyTools.ExecCmd.Args;

{ TSetupInstallStrategy }

constructor TSetupInstallStrategy.Create(const ADependencies: TJSONArray);
begin
  inherited Create();
  FDependencies := ADependencies.Clone() as TJSONArray;
  FUnzipTask := TTask.Run(
    procedure()
    begin
      for var LDependency in FDependencies do begin
        var LModuleName := (LDependency as TJSONObject).GetValue<string>('module_name');
        var LFileName := (LDependency as TJSONObject).GetValue<string>('file_name');
        var LFilePath := TPath.Combine(TPath.GetDocumentsPath(), LFileName);

        if TFile.Exists(LFilePath) then
          TZipFile.ExtractZipFile(LFilePath, LFilePath.Replace('.zip', '', []));
      end;
    end);
end;

destructor TSetupInstallStrategy.Destroy;
begin
  FDependencies.Free();
  inherited;
end;

function TSetupInstallStrategy.IsInstalled(const AModuleName, AFilePath: string): boolean;
begin
  Result := not TFile.Exists(AFilePath);
end;

function TSetupInstallStrategy.Install(const AModuleName, AFilePath: string): boolean;
var
  LOutput: string;
begin
  FUnzipTask.Wait();

  var LSetupFiles := TDirectory.GetFiles(
    TPath.GetDirectoryName(AFilePath), 'setup.py', TSearchOption.soAllDirectories);

  if not Assigned(LSetupFiles) then
    Exit(false);

  var LCurDir := TDirectory.GetCurrentDirectory();
  try
    TDirectory.SetCurrentDirectory(TPath.GetDirectoryName(LSetupFiles[Low(LSetupFiles)]));

    var LExec := TExecCmdService
      .Cmd(GetPythonEngine().ProgramName,
        TExecCmdArgs.BuildArgv(
          GetPythonEngine().ProgramName,
          ['setup.py', 'install']),
        TExecCmdArgs.BuildEnvp(
          GetPythonEngine().PythonHome,
          GetPythonEngine().ProgramName,
          TPath.Combine(GetPythonEngine().DllPath, GetPythonEngine().DllName)))
      .Run(LOutput);

    Result := LExec.Wait() = EXIT_SUCCESS;
  finally
    TDirectory.SetCurrentDirectory(LCurDir);
  end;
end;

end.
