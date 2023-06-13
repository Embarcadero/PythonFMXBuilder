unit Dependencies.Setup;

interface

uses
  System.JSON, System.Threading,
  Dependencies;

type
  TSetupInstallStrategy = class(TBaseInstallDependency, IInstallDependency)
  public
    function CanHandle(AFilePath: string): boolean;
    function IsInstalled(const AModuleName, AFilePath: string): boolean;
    function Install(const AModuleName, AFilePath: string): boolean;
  end;

implementation

uses
  System.SysUtils, System.IOUtils, System.Zip, System.Classes, System.StrUtils,
  PyTools.ExecCmd;

{ TSetupInstallStrategy }

function TSetupInstallStrategy.CanHandle(AFilePath: string): boolean;
begin
  if not (TPath.GetExtension(AFilePath) = '.zip') and TZipFile.IsValid(AFilePath) then
    Exit(false);

  var LZipFile := TZipFile.Create();
  try
    LZipFile.Open(AFilePath, TZipMode.zmRead);
    try
      Result := MatchStr('setup.py', LZipFile.FileNames);
    finally
      LZipFile.Close();
    end;
  finally
    LZipFile.Free();
  end;
end;

function TSetupInstallStrategy.IsInstalled(const AModuleName, AFilePath: string): boolean;
begin
  Result := PkgUtilCheckInstalled(AModuleName, AFilePath);
end;

function TSetupInstallStrategy.Install(const AModuleName, AFilePath: string): boolean;
begin
  var LFolder := AFilePath.Replace('.zip', '', []);
  if TFile.Exists(AFilePath) then
    TZipFile.ExtractZipFile(AFilePath, LFolder);
  var LSetupFiles := TDirectory.GetFiles(
    LFolder, 'setup.py', TSearchOption.soAllDirectories);

  if not Assigned(LSetupFiles) then
    Exit(false);

  var LCurDir := TDirectory.GetCurrentDirectory();
  try
    TDirectory.SetCurrentDirectory(TPath.GetDirectoryName(LSetupFiles[Low(LSetupFiles)]));
    Result := ExecCmd(['setup.py', 'install']) = EXIT_SUCCESS;
  finally
    TDirectory.SetCurrentDirectory(LCurDir);
  end;
end;

end.
