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
  var LJDKPath := TBuilderPaths.GetJDKToolInstallationFolder(ToolInfo^.Requires[0]^);
  var LAgreementFile := TPath.GetTempFileName();
  var LCmd := TAndroidToolsPathLocator.FindToolPath(AInstallationDirectory,
    {$IFDEF POSIX}'sdkmanager'{$ELSE}'sdkmanager.bat'{$ENDIF});

  if Assigned(AProgress) then
    AProgress(ToolInfo, 'Installing tool 1/3', 3, 1);

  if TExecCmdService
    .Cmd(
      LCmd,
      {$IFDEF POSIX}[LCmd] + {$ENDIF POSIX} [
      '--update',
      '--sdk_root=' + AInstallationDirectory],
      ['JAVA_HOME=' + LJDKPath])
    .Run(LOutput)
    .Wait() = EXIT_FAILURE then
      raise EOperationFailed.Create(LOutput);

  if AAbort then
    raise EOperationCancelled.Create('The operation has been cancelled.');

  if Assigned(AProgress) then
    AProgress(ToolInfo, 'Installing tool 2/3', 3, 2);

  try
    {$IFDEF POSIX}
    TFile.WriteAllLines(LAgreementFile, [
      'export JAVA_HOME=' + LJDKPath,
      Format('yes | %s --licenses --sdk_root=%s', [LCmd, AInstallationDirectory])
    ]);
    var LExec := String.Empty;
    var LArgV: TArray<string> := nil;
    {$IFDEF MACOS}
    LExec := '/bin/zsh';
    LArgV := ['zsh', LAgreementFile];
    {$ENDIF MACOS}

    {$IFDEF LINUX}
    LExec := '/usr/bin/bash';
    LArgV := ['bash', LAgreementFile];
    {$ENDIF LINUX}
    if TExecCmdService
      .Cmd(LExec, LArgV, [])
      .Run(LOutput)
      .Wait() = EXIT_FAILURE then
        raise Exception.Create(LOutput);
    {$ENDIF POSIX}

    {$IFDEF MSWINDOWS}
    var LYes := TArray<string>.Create();
    for var I := 1 to 10 do
      LYes := LYes + ['y'];
    TFile.WriteAllLines(LAgreementFile, LYes);

    if TExecCmdService
      .Cmd(
        LCmd,
        {$IFDEF POSIX}[LCmd] + {$ENDIF POSIX} [
        '--licenses',
        '--sdk_root=' + AInstallationDirectory
        {$IFDEF MSWINDOWS}, '< ' + LAgreementFile{$ENDIF MSWINDOWS}
        ],
        ['JAVA_HOME=' + LJDKPath])
      .Run(LOutput)
      .Wait() = EXIT_FAILURE then
        raise EOperationFailed.Create(LOutput);
    {$ENDIF MSWINDOWS}
  finally
    TFile.Delete(LAgreementFile);
  end;

  if AAbort then
    raise EOperationCancelled.Create('The operation has been cancelled.');

  if Assigned(AProgress) then
    AProgress(ToolInfo, 'Installing tool 3/3', 3, 3);

  if TExecCmdService
    .Cmd(
      LCmd,
      {$IFDEF POSIX}[LCmd] + {$ENDIF POSIX}
      ToolInfo^.Args
      + ['--sdk_root=' + AInstallationDirectory],
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
