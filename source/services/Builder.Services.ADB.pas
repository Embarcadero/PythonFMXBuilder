unit Builder.Services.ADB;

interface

uses
  System.Classes, System.SysUtils, System.IOUtils,
  Builder.Services,
  Builder.Model.Environment;

type
  TADBService = class(TInterfacedObject, IADBServices)
  private
    class var FActiveDevice: string;
  private
    procedure ExecCmd(const ACmdLine, ABaseDir: string; ACmdResult: TStrings);
    procedure EnumAssets(const AAssetsBasePath: string; const AProc: TProc<string>);
    procedure EnumLibraries(const ALibBasePath: string; const AProc: TProc<string>);
  private
    procedure EnumDevices(const ADeviceList: TStrings; const AProc: TProc<string>);
    function FindDeviceVendorModel(const AAdbPath, ADevice: string): string;
  public
    procedure ListDevices(const AAdbPath: string; const AStrings: TStrings);
    procedure SetActiveDevice(const ADeviceName: string);
    function GetActiveDevice(): string;

    function BuildApk(const AAppBasePath, AProjectName: string;
      const AEnvironmentModel: TEnvironmentModel; const AResult: TStrings): boolean;

    function InstallApk(const AAdbPath, AApkPath, ADevice: string; const AResult: TStrings): boolean;
    function UnInstallApk(const AAdbPath, APkgName, ADevice: string; const AResult: TStrings): boolean;

    procedure RunApp(const AAdbPath, APkgName, ADevice: string; const AResult: TStrings);
    procedure StartDebugSession(const AAdbPath: string; const APort: integer; const AResult: TStrings);
    procedure StopDebugSession(const AAdbPath: string; const APort: integer; const AResult: TStrings);
    procedure DebugApp(const AAdbPath, APkgName, ADevice, AHost: string; const APort: integer; const AResult: TStrings);
    procedure ForceStopApp(const AAdbPath, APkgName, ADevice: string; const AResult: TStrings);
  end;

implementation

uses
  Builder.Chain,
  Builder.Storage.Default,
  {$IFDEF MSWINDOWS}
  Builder.Services.ADB.Win;
  {$ELSE}
  Builder.Services.ADB.Posix;
  {$ENDIF}

{ TADBService }

function TADBService.BuildApk(const AAppBasePath, AProjectName: string;
  const AEnvironmentModel: TEnvironmentModel; const AResult: TStrings): boolean;
const
  CMD_1 = '"$AAPT" package -f -m -J . -M AndroidManifest.xml -S res -I "$ANDROIDJAR"';

  CMD_2 = '"$AAPT" package -f -m -F bin\$PROJNAME.unaligned.apk -M AndroidManifest.xml -S res -I "$ANDROIDJAR"';

  CMD_3 = 'xcopy "$APPBASEPATH\classes\classes.dex" "$APPBASEPATH\bin\" /y';

  CMD_4 = '"$AAPT" add $PROJNAME.unaligned.apk classes.dex';

  CMD_5 = 'xcopy "$APPBASEPATH\assets" "$APPBASEPATH\bin\assets" /y /E /H /C /I';

  CMD_6 = '"$AAPT" add $PROJNAME.unaligned.apk $FILE';

  CMD_7 = 'xcopy "$APPBASEPATH\library\lib" "$APPBASEPATH\bin\lib" /y /E /H /C /I';

  CMD_8 = '"$AAPT" add $PROJNAME.unaligned.apk $FILE';

  CMD_9 = '"$JARSIGNER" -keystore cert\PyApp.keystore -storepass delphirocks bin\$PROJNAME.unaligned.apk PyApp';

  CMD_10 = '"$ZIPALIGN" -f 4 bin\$PROJNAME.unaligned.apk bin\$PROJNAME.apk';

  CMD_11 = '"$APKSIGNER" sign --ks-key-alias PyApp --ks cert\PyApp.keystore --ks-pass pass:delphirocks --key-pass pass:delphirocks bin\$PROJNAME.apk';
begin
  var LAppBinPath := TPath.Combine(AAppBasePath, 'bin');
  if not TDirectory.Exists(LAppBinPath) then
    TDirectory.CreateDirectory(LAppBinPath);

  var LCmd := CMD_1
    .Replace('$AAPT', AEnvironmentModel.AAptLocation)
    .Replace('$PROJNAME', AProjectName)
    .Replace('$ANDROIDJAR', TPath.Combine(AEnvironmentModel.SdkApiLocation, 'android.jar'));

  ExecCmd(LCmd, AAppBasePath, AResult);

  LCmd := CMD_2
    .Replace('$AAPT', AEnvironmentModel.AAptLocation)
    .Replace('$PROJNAME', AProjectName)
    .Replace('$ANDROIDJAR', TPath.Combine(AEnvironmentModel.SdkApiLocation, 'android.jar'));

  ExecCmd(LCmd, AAppBasePath, AResult);

  LCmd := CMD_3
    .Replace('$APPBASEPATH', AAppBasePath);

  ExecCmd(LCmd, String.Empty, AResult);

  LCmd := CMD_4
    .Replace('$AAPT', AEnvironmentModel.AAptLocation)
    .Replace('$PROJNAME', AProjectName);

  ExecCmd(LCmd, LAppBinPath, AResult);

  LCmd := CMD_5
    .Replace('$APPBASEPATH', AAppBasePath);

  ExecCmd(LCmd, String.Empty, AResult);

  EnumAssets(TPath.Combine(LAppBinPath, 'assets'),
    procedure(AFile: string) begin
      LCmd := CMD_6
        .Replace('$AAPT', AEnvironmentModel.AAptLocation)
        .Replace('$PROJNAME', AProjectName)
        .Replace('$FILE', AFile);

      ExecCmd(LCmd, LAppBinPath, AResult);
    end);

  LCmd := CMD_7
    .Replace('$APPBASEPATH', AAppBasePath);

  ExecCmd(LCmd, String.Empty, AResult);

  EnumLibraries(TPath.Combine(LAppBinPath, 'lib'),
    procedure(AFile: string) begin
      LCmd := CMD_8
        .Replace('$AAPT', AEnvironmentModel.AAptLocation)
        .Replace('$PROJNAME', AProjectName)
        .Replace('$FILE', AFile);

      ExecCmd(LCmd, LAppBinPath, AResult);
    end);

  LCmd := CMD_9
    .Replace('$JARSIGNER', AEnvironmentModel.JarSignerLocation)
    .Replace('$PROJNAME', AProjectName);

  ExecCmd(LCmd, AAppBasePath, AResult);

  LCmd := CMD_10
    .Replace('$ZIPALIGN', AEnvironmentModel.ZipAlignLocation)
    .Replace('$PROJNAME', AProjectName);

  ExecCmd(LCmd, AAppBasePath, AResult);

  //This is the jar file... we want the bat on the parent dir
  var LApkSignerBatDir := TDirectory.GetParent(ExtractFileDir(AEnvironmentModel.ApkSignerLocation));
  var LApkSignerBatPath := TPath.Combine(LApkSignerBatDir, ChangeFileExt(ExtractFileName(AEnvironmentModel.ApkSignerLocation), '.bat'));

  LCmd := CMD_11
    .Replace('$APKSIGNER', LApkSignerBatPath)
    .Replace('$PROJNAME', AProjectName);

  ExecCmd(LCmd, AAppBasePath, AResult);

  var LApkPath := TPath.Combine(LAppBinPath, ChangeFileExt(AProjectName, '.apk'));
  Result := TFile.Exists(LApkPath)
    and not AResult.Text.Contains('Failure');
end;

procedure TADBService.EnumAssets(const AAssetsBasePath: string;
  const AProc: TProc<string>);
begin
  if not Assigned(AProc) then
    Exit;

  for var LFile in TDirectory.GetFiles(AAssetsBasePath, '*.*', TSearchOption.soAllDirectories) do begin
    var LRelativeFilePath := LFile
      .Replace(AAssetsBasePath, String.Empty)
      .Replace('\', '/');

    if LRelativeFilePath.StartsWith('/') then
      LRelativeFilePath := LRelativeFilePath.Remove(0, 1);

    AProc('assets/' + LRelativeFilePath);
  end;
end;

procedure TADBService.EnumDevices(const ADeviceList: TStrings;
  const AProc: TProc<string>);
begin
  if not Assigned(AProc) then
    Exit;

  if ADeviceList.Count > 1 then begin
    for var I := 1 to ADeviceList.Count -1 do begin
      if ADeviceList[I].Trim().IsEmpty() then
        Exit;

      var LPos := Pos(#9, ADeviceList[I]);
      if LPos = -1 then
        Exit;

      var LDevice := Copy(ADeviceList[I], 1, LPos - 1);
      AProc(LDevice);
    end;
  end;
end;

procedure TADBService.EnumLibraries(const ALibBasePath: string;
  const AProc: TProc<string>);
begin
  if not Assigned(AProc) then
    Exit;

  for var LFile in TDirectory.GetFiles(ALibBasePath, '*.so', TSearchOption.soAllDirectories) do begin
    var LRelativeFilePath := LFile
      .Replace(ALibBasePath, String.Empty)
      .Replace('\', '/');

    if LRelativeFilePath.StartsWith('/') then
      LRelativeFilePath := LRelativeFilePath.Remove(0, 1);

    AProc('lib/' + LRelativeFilePath);
  end;
end;

procedure TADBService.ExecCmd(const ACmdLine, ABaseDir: string; ACmdResult: TStrings);
begin
  TGlobalBuilderChain.BroadcastEvent(TMessageEvent.Create('ExecCmd: ' + ACmdLine));

  var LCmdResults := TStringList.Create();
  try
    ExecCmdine(ACmdLine, ABaseDir, LCmdResults);
    ACmdResult.AddStrings(LCmdResults);

    TGlobalBuilderChain.BroadcastEvent(TMessageEvent.Create(LCmdResults.Text));
  finally
    LCmdResults.Free();
  end;
end;

function TADBService.FindDeviceVendorModel(const AAdbPath, ADevice: string): string;
begin
  var LStrings := TStringList.Create();
  try
    ExecCmd(AAdbPath + Format(' -s %s shell getprop ro.product.model', [ADevice]), String.Empty, LStrings);
    Result := LStrings.Text
      .Replace(#13#10, String.Empty);
  finally
    LStrings.Free();
  end;
end;

procedure TADBService.ForceStopApp(const AAdbPath, APkgName, ADevice: string;
  const AResult: TStrings);
const
  CMD = '%s -s %s shell am force-stop %s';
begin
  ExecCmd(Format(CMD, [AAdbPath, ADevice, APkgName]), String.Empty, AResult);
end;

function TADBService.GetActiveDevice: string;
begin
  Result := FActiveDevice;
end;

function TADBService.InstallApk(const AAdbPath, AApkPath, ADevice: string; const AResult: TStrings): boolean;
begin
  var LStrings := TStringList.Create();
  try
    ExecCmd(AAdbPath + Format(' -s %s install ', [ADevice]) + AApkPath, String.Empty, LStrings);
    AResult.AddStrings(LStrings);
    Result := (not LStrings.Text.Contains('failure')) and (not LStrings.Text.Contains('failed'));
  finally
    LStrings.Free();
  end;
end;

function TADBService.UnInstallApk(const AAdbPath, APkgName, ADevice: string;
  const AResult: TStrings): boolean;
begin
  var LStrings := TStringList.Create();
  try
    ExecCmd(AAdbPath + Format(' -s %s uninstall ', [ADevice]) + APkgName, String.Empty, LStrings);
    AResult.AddStrings(LStrings);
    Result := (not LStrings.Text.Contains('failure')) and (not LStrings.Text.Contains('failed'));
  finally
    LStrings.Free();
  end;
end;

procedure TADBService.ListDevices(const AAdbPath: string; const AStrings: TStrings);
begin
  var LStrings := TStringList.Create();
  try
    ExecCmd(AAdbPath + ' devices', String.Empty, LStrings);
    EnumDevices(LStrings, procedure(ADevice: string) begin
      AStrings.AddPair(ADevice, FindDeviceVendorModel(AAdbPath, ADevice));
    end);
  finally
    LStrings.Free();
  end;
end;

procedure TADBService.RunApp(const AAdbPath, APkgName, ADevice: string;
  const AResult: TStrings);
const
  CMD = '%s -s %s shell am start -n %s/com.embarcadero.firemonkey.FMXNativeActivity';
begin
  ExecCmd(Format(CMD, [AAdbPath, ADevice, APkgName]), String.Empty, AResult);
end;

procedure TADBService.DebugApp(const AAdbPath, APkgName, ADevice, AHost: string;
  const APort: integer; const AResult: TStrings);
const
  CMD_START = '%s -s %s shell am start -n %s/com.embarcadero.firemonkey.FMXNativeActivity --es args ''"--debugpy -port %d"''';
begin
  ExecCmd(Format(CMD_START, [AAdbPath, ADevice, APkgName, APort]), String.Empty, AResult);
end;

procedure TADBService.SetActiveDevice(const ADeviceName: string);
begin
  FActiveDevice := ADeviceName;
end;

procedure TADBService.StartDebugSession(const AAdbPath: string; const APort: integer; const AResult: TStrings);
const
  CMD_REDIRECT = '%0:s forward tcp:%1:d tcp:%1:d';
begin
  ExecCmd(Format(CMD_REDIRECT, [AAdbPath, APort]), String.Empty, AResult);
end;

procedure TADBService.StopDebugSession(const AAdbPath: string; const APort: integer; const AResult: TStrings);
const
  CMD_REDIRECT = '%0:s forward --remove tcp:%1:d';
begin
  ExecCmd(Format(CMD_REDIRECT, [AAdbPath, APort]), String.Empty, AResult);
end;

end.
