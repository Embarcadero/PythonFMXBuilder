unit Dependencies.ZipImports;

interface

uses
  System.JSON, System.Threading,
  Dependencies;

type
  TZipImportsInstallStrategy = class(TBaseInstallDependency, IInstallDependency)
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

{ TZipImportsInstallStrategy }

function TZipImportsInstallStrategy.CanDelete(AFilePath: string): boolean;
begin
  Result := false;
end;

function TZipImportsInstallStrategy.CanHandle(AFilePath: string): boolean;
begin
  Result := (TPath.GetExtension(AFilePath) = '.pyzip')
    and TZipFile.IsValid(AFilePath);
end;

function TZipImportsInstallStrategy.Install(const AModuleName,
  AFilePath: string): boolean;
begin
  SysModule.path.append(AFilePath);
  Result := true;
end;

function TZipImportsInstallStrategy.IsInstalled(const AModuleName,
  AFilePath: string): boolean;
begin
  Result := SysModule.path.count(AFilePath) > 0;
end;

end.
