unit Dependencies.ZipPackage;

interface

uses
  System.JSON, System.Threading,
  Dependencies;

type
  TZipPackageInstallStrategy = class(TBaseInstallDependency, IInstallDependency)
  public
    function CanHandle(AFilePath: string): boolean;
    function CanDelete(AFilePath: string): boolean;
    function IsInstalled(const AModuleName, AFilePath: string): boolean;
    function Install(const AModuleName, AFilePath: string): boolean;
  end;

implementation

uses
  System.SysUtils, System.IOUtils, System.Zip, System.Classes, System.StrUtils,
  VarPyth;

{ TZipPackageInstallStrategy }

function TZipPackageInstallStrategy.CanDelete(AFilePath: string): boolean;
begin
  Result := true;
end;

function TZipPackageInstallStrategy.CanHandle(AFilePath: string): boolean;
begin
  if not ((TPath.GetExtension(AFilePath) = '.zip') and TZipFile.IsValid(AFilePath)) then
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

function TZipPackageInstallStrategy.Install(const AModuleName,
  AFilePath: string): boolean;
begin
  if TFile.Exists(AFilePath) then
    TZipFile.ExtractZipFile(AFilePath, TPath.GetDocumentsPath());

  SysModule.path.append(AFilePath.Replace('.zip', '', []));
  Result := true;
end;

function TZipPackageInstallStrategy.IsInstalled(const AModuleName,
  AFilePath: string): boolean;
begin
  Result := SysModule.path.count(AFilePath.Replace('.zip', '', [])) > 0;
end;

end.
