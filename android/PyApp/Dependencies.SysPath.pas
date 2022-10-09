unit Dependencies.SysPath;

interface

uses
  System.JSON, System.Threading,
  Dependencies;
type
  TSysPathInstallStrategy = class(TInterfacedObject, IInstallDependency)
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

{ TSysPathInstallStrategy }

constructor TSysPathInstallStrategy.Create(const ADependencies: TJSONArray);
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
          TZipFile.ExtractZipFile(LFilePath, TPath.GetDocumentsPath());
      end;
    end);
end;

destructor TSysPathInstallStrategy.Destroy;
begin
  FDependencies.Free();
  inherited;
end;

function TSysPathInstallStrategy.Install(const AModuleName,
  AFilePath: string): boolean;
begin
  FUnzipTask.Wait();
  GetPythonEngine().ExecString(AnsiString(Format(
    'import sys'
    + sLineBreak
    + 'sys.path.append("%s")',
    [AFilePath.Replace('.zip', '', [])])));
  Result := true;
end;

function TSysPathInstallStrategy.IsInstalled(const AModuleName,
  AFilePath: string): boolean;
begin
  Result := false;
end;

end.
