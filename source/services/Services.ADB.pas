unit Services.ADB;

interface

uses
  Services, System.Classes, System.SysUtils, System.IOUtils, Model.Environment;

type
  TADBService = class(TInterfacedObject, IADBServices)
  private
    procedure ExecCmd(const ACmdLine, ABaseDir: string; ACmdResult: TStrings);
    procedure EnumAssets(const AAssetsBasePath: string; const AProc: TProc<string>);
    procedure EnumLibraries(const ALibBasePath: string; const AProc: TProc<string>);
  private
    procedure EnumDevices(const ADeviceList: TStrings; const AProc: TProc<string>);
    function FindDeviceVendorModel(const AAdbPath, ADevice: string): string;
  public
    procedure ListDevices(const AAdbPath: string; const AStrings: TStrings);
    function InstallApk(const AAdbPath, AApkPath, ADevice: string; const AResult: TStrings): boolean;
    procedure RunApp(const AAdbPath, APkgName, ADevice: string; const AResult: TStrings);

    function BuildApk(const AAppBasePath, AAppName: string;
      const AEnvironmentModel: TEnvironmentModel; const AResult: TStrings): boolean;
  end;

implementation

uses
  Storage.Default,
  {$IFDEF MSWINDOWS}
  Services.ADB.Win;
  {$ELSE}
  Services.ADB.Posix;
  {$ENDIF}

{ TADBService }

function TADBService.BuildApk(const AAppBasePath, AAppName: string;
  const AEnvironmentModel: TEnvironmentModel; const AResult: TStrings): boolean;
const
  CMD_1 = '"$AAPT" package -f -m -J . -M AndroidManifest.xml -S res -I "$ANDROIDJAR"';

  CMD_2 = '"$AAPT" package -f -m -F bin\$APPNAME.unaligned.apk -M AndroidManifest.xml -S res -I "$ANDROIDJAR"';

  CMD_3 = 'xcopy "$APPBASEPATH\classes\classes.dex" "$APPBASEPATH\bin\" /y';

  CMD_4 = '"$AAPT" add $APPNAME.unaligned.apk classes.dex';

  CMD_5 = 'xcopy "$APPBASEPATH\assets" "$APPBASEPATH\bin\assets" /y /E /H /C /I';

  CMD_6 = '"$AAPT" add $APPNAME.unaligned.apk $FILE';

  CMD_7 = 'xcopy "$APPBASEPATH\library\lib" "$APPBASEPATH\bin\lib" /y /E /H /C /I';

  CMD_8 = '"$AAPT" add $APPNAME.unaligned.apk $FILE';

  CMD_9 = '"$JARSIGNER" -keystore cert\PyApp.keystore -storepass delphirocks bin\$APPNAME.unaligned.apk PyApp';

  CMD_10 = '"$ZIPALIGN" -f 4 bin\$APPNAME.unaligned.apk bin\$APPNAME.apk';
begin
  var LAppBinPath := TPath.Combine(AAppBasePath, 'bin');
  if not TDirectory.Exists(LAppBinPath) then
    TDirectory.CreateDirectory(LAppBinPath);

  var LCmd := CMD_1
    .Replace('$AAPT', AEnvironmentModel.AAptLocation)
    .Replace('$APPNAME', AAppName)
    .Replace('$ANDROIDJAR', TPath.Combine(AEnvironmentModel.SdkApiLocation, 'android.jar'));

  ExecCmd(LCmd, AAppBasePath, AResult);

  LCmd := CMD_2
    .Replace('$AAPT', AEnvironmentModel.AAptLocation)
    .Replace('$APPNAME', AAppName)
    .Replace('$ANDROIDJAR', TPath.Combine(AEnvironmentModel.SdkApiLocation, 'android.jar'));

  ExecCmd(LCmd, AAppBasePath, AResult);

  LCmd := CMD_3
    .Replace('$APPBASEPATH', AAppBasePath);

  ExecCmd(LCmd, String.Empty, AResult);

  LCmd := CMD_4
    .Replace('$AAPT', AEnvironmentModel.AAptLocation)
    .Replace('$APPNAME', AAppName);

  ExecCmd(LCmd, LAppBinPath, AResult);

  LCmd := CMD_5
    .Replace('$APPBASEPATH', AAppBasePath);

  ExecCmd(LCmd, String.Empty, AResult);

  EnumAssets(TPath.Combine(LAppBinPath, 'assets'),
    procedure(AFile: string) begin
      LCmd := CMD_6
        .Replace('$AAPT', AEnvironmentModel.AAptLocation)
        .Replace('$APPNAME', AAppName)
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
        .Replace('$APPNAME', AAppName)
        .Replace('$FILE', AFile);

      ExecCmd(LCmd, LAppBinPath, AResult);
    end);

  LCmd := CMD_9
    .Replace('$JARSIGNER', AEnvironmentModel.JarSignerLocation)
    .Replace('$APPNAME', AAppName);

  ExecCmd(LCmd, AAppBasePath, AResult);

  LCmd := CMD_10
    .Replace('$ZIPALIGN', AEnvironmentModel.ZipAlignLocation)
    .Replace('$APPNAME', AAppName);

  ExecCmd(LCmd, AAppBasePath, AResult);

  var LApkPath := TPath.Combine(LAppBinPath, ChangeFileExt(AAppName, '.apk'));
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
  var LLog: ILogServices := nil;
  if Supports(GlobalServices, ILogServices, LLog) then
    LLog.Log('ExecCmd: ' + ACmdLine);

  var LCmdResults := TStringList.Create();
  try
    ExecCmdine(ACmdLine, ABaseDir, LCmdResults);
    ACmdResult.AddStrings(LCmdResults);

    if Assigned(LLog) then
      LLog.Log(LCmdResults.Text);
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

function TADBService.InstallApk(const AAdbPath, AApkPath, ADevice: string; const AResult: TStrings): boolean;
begin
  var LStrings := TStringList.Create();
  try
    ExecCmd(AAdbPath + Format(' -s %s install -r ', [ADevice]) + AApkPath, String.Empty, LStrings);
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
begin
  ExecCmd(AAdbPath
    + Format(' -s %s shell am start -n %s/com.embarcadero.firemonkey.FMXNativeActivity', [
        ADevice, APkgName]), String.Empty, AResult);
end;

end.
