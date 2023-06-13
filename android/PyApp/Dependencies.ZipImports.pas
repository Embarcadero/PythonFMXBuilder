unit Dependencies.ZipImports;

interface

uses
  System.JSON, System.Threading,
  Dependencies;

type
  TZipImportsInstallStrategy = class(TBaseInstallDependency, IInstallDependency)
  public
    function CanHandle(AFilePath: string): boolean;
    function IsInstalled(const AModuleName, AFilePath: string): boolean;
    function Install(const AModuleName, AFilePath: string): boolean;
  end;

implementation

uses
  System.SysUtils, System.IOUtils, System.Zip, System.Classes, System.StrUtils,
  PythonEngine,
  PyTools.ExecCmd,
  PyTools.ExecCmd.Args;

{ TZipImportsInstallStrategy }

function TZipImportsInstallStrategy.CanHandle(AFilePath: string): boolean;
begin
  if not (TPath.GetExtension(AFilePath) = '.zip') and TZipFile.IsValid(AFilePath) then
    Exit(false);

  var LZipFile := TZipFile.Create();
  try
    LZipFile.Open(AFilePath, TZipMode.zmRead);
    try
      Result := not MatchStr('setup.py', LZipFile.FileNames);
    finally
      LZipFile.Close();
    end;
  finally
    LZipFile.Free();
  end;
end;

function TZipImportsInstallStrategy.Install(const AModuleName,
  AFilePath: string): boolean;
begin
  GetPythonEngine().ExecString(AnsiString(Format(
    'import sys'
    + sLineBreak
    + 'sys.path.append("%s")',
    [AFilePath])));
  Result := true;
end;

function TZipImportsInstallStrategy.IsInstalled(const AModuleName,
  AFilePath: string): boolean;
begin
  Result := false;
end;

end.
