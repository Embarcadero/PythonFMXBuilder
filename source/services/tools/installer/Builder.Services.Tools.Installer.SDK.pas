unit Builder.Services.Tools.Installer.SDK;

interface

uses
  Builder.Types,
  Builder.Services.Tools.Installer.DownloadAndExtract;

type
  TSDKToolInstaller = class(TDownloadAndExtractToolInstaller)
  protected
    procedure InternalInstall(const AInstallationDirectory: string;
      const AProgress: TToolInstallationProgress; var AAbort: boolean); override;
  public
    class function IsInstalled(const AToolInfo: PToolInfo): boolean; override;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  PyTools.ExecCmd,
  Builder.Paths,
  Builder.Exception;

{ TSDKToolInstaller }

procedure TSDKToolInstaller.InternalInstall(const AInstallationDirectory: string;
  const AProgress: TToolInstallationProgress; var AAbort: boolean);
begin
  inherited;
  var LOutput := String.Empty;
  var LJDKPath := TBuilderPaths.GetToolInstallationFolder(ToolInfo^.Requires[0]^);
  var LTmpYes := TPath.GetTempFileName();
  var LYes := TArray<string>.Create();
  var LCmd := TAndroidToolsPathLocator.FindToolPath(AInstallationDirectory,
    {$IFDEF POSIX}'sdkmanager'{$ELSE}'sdkmanager.bat'{$ENDIF});

  for var I := 1 to 20 do
    LYes := LYes + ['y'];
  TFile.WriteAllLines(LTmpYes, LYes);

  if Assigned(AProgress) then
    AProgress(ToolInfo, 'Installing tool', 3, 1);

  if TExecCmdService
    .Cmd(
      LCmd,
      {$IFDEF POSIX}[LCmd] + {$ENDIF} [
      '--licenses',
      '--sdk_root=' + AInstallationDirectory,
      '< ' + LTmpYes],
      ['JAVA_HOME=' + LJDKPath])
    .Run(LOutput)
    .Wait() = EXIT_FAILURE then
      raise EOperationFailed.Create(LOutput);

  if Assigned(AProgress) then
    AProgress(ToolInfo, 'Installing tool', 3, 2);

  if AAbort then
    raise EOperationCancelled.Create('The operation has been cancelled.');

  if TExecCmdService
    .Cmd(
      LCmd,
      {$IFDEF POSIX}[LCmd] + {$ENDIF} ['--sdk_root=' + AInstallationDirectory]
      + ToolInfo^.Args,
      ['JAVA_HOME=' + LJDKPath])
    .Run(LOutput)
    .Wait() = EXIT_FAILURE then
      raise EOperationFailed.Create(LOutput);

  if Assigned(AProgress) then
    AProgress(ToolInfo, 'Installing tool', 3, 3);

  if AAbort then
    raise EOperationCancelled.Create('The operation has been cancelled.');

  if TExecCmdService
    .Cmd(
      LCmd,
      {$IFDEF POSIX}[LCmd] + {$ENDIF} [
      '--update',
      '--sdk_root=' + AInstallationDirectory],
      ['JAVA_HOME=' + LJDKPath])
    .Run(LOutput)
    .Wait() = EXIT_FAILURE then
      raise EOperationFailed.Create(LOutput);
end;

class function TSDKToolInstaller.IsInstalled(
  const AToolInfo: PToolInfo): boolean;
begin
  var LToolDirectory := TBuilderPaths.GetToolInstallationFolder(AToolInfo^);

  Result := inherited IsInstalled(AToolInfo);
  // Check tools
  Result := Result
    and not TAndroidToolsPathLocator.FindSdkApiLocation(LToolDirectory).IsEmpty()
    and not TAndroidToolsPathLocator.FindToolPath(LToolDirectory,
      'apksigner.jar').IsEmpty()
    and not TAndroidToolsPathLocator.FindToolPath(LToolDirectory,
      {$IFDEF POSIX}'adb'{$ELSE}'adb.exe'{$ENDIF}).IsEmpty()
    and not TAndroidToolsPathLocator.FindToolPath(LToolDirectory,
      {$IFDEF POSIX}'aapt'{$ELSE}'aapt.exe'{$ENDIF}).IsEmpty()
    and not TAndroidToolsPathLocator.FindToolPath(LToolDirectory,
      {$IFDEF POSIX}'zipalign'{$ELSE}'zipalign.exe'{$ENDIF}).IsEmpty();
end;

end.
