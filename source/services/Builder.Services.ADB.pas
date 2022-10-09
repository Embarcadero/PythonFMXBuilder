unit Builder.Services.ADB;

interface

uses
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  Builder.Services,
  Builder.Model.Environment;

type
  TADBService = class(TInterfacedObject, IADBServices)
  private
    class var FActiveDevice: string;
  private
    procedure ExecCmd(const ACmd: string; const AArgs: TArray<string>; ACmdResult: TStrings);
    procedure EnumAssets(const AAssetsBasePath: string; const AProc: TProc<string>);
    procedure EnumLibraries(const ALibBasePath: string; const AProc: TProc<string>);
  private
    procedure EnumDevices(const ADeviceList: TStrings; const AProc: TProc<string>);
    function FindDeviceVendorModel(const AAdbPath, ADevice: string): string;
  public
    procedure ListDevices(const AAdbPath: string; const AStrings: TStrings);
    procedure SetActiveDevice(const ADeviceName: string);
    function GetActiveDevice(): string;
    procedure CheckActiveDevice();

    //App helpers
    function GetAppInstallationPath(const AAdbPath, APkgName, ADevice: string; const AResult: TStrings): string;
    function IsAppInstalled(const AAdbPath, APkgName, ADevice: string; const AResult: TStrings): boolean;
    function IsAppRunning(const AAdbPath, APkgName, ADevice: string; const AResult: TStrings): boolean;

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
  PyTools.ExecCmd,
  Builder.Chain,
  Builder.Exception,
  Builder.Storage.Default;

{ TADBService }

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
  //TStrings is not breaking line on macOS :/
  //Still needs to invastigate this
  var LInputs := ADeviceList.Text.Split([sLineBreak]);

  if Length(LInputs) > 1 then begin
    for var I := 1 to High(LInputs) do begin
      if LInputs[I].Trim().IsEmpty() then
        Continue;

      var LPos := Pos(#9, LInputs[I]);
      if LPos = -1 then
        Continue;

      var LDevice := Copy(LInputs[I], 1, LPos - 1);
      if not LDevice.Trim.IsEmpty() then
        AProc(LDevice);
    end;
  end;
end;

procedure TADBService.EnumLibraries(const ALibBasePath: string;
  const AProc: TProc<string>);
begin
  if not Assigned(AProc) then
    Exit;

  for var LFile in TDirectory.GetFiles(ALibBasePath, '*', TSearchOption.soAllDirectories) do begin
    var LRelativeFilePath := LFile
      .Replace(ALibBasePath, String.Empty)
      .Replace('\', '/');

    if LRelativeFilePath.StartsWith('/') then
      LRelativeFilePath := LRelativeFilePath.Remove(0, 1);

    AProc('lib/' + LRelativeFilePath);
  end;
end;

procedure TADBService.CheckActiveDevice;
begin
  if GetActiveDevice().IsEmpty() then
    raise ENoActiveDevice.Create('No device selected.');
end;

function TADBService.GetActiveDevice: string;
begin
  Result := FActiveDevice;
end;

procedure TADBService.SetActiveDevice(const ADeviceName: string);
begin
  FActiveDevice := ADeviceName;
end;

procedure TADBService.ExecCmd(const ACmd: string; const AArgs: TArray<string>;
  ACmdResult: TStrings);
var
  LResult: string;
begin
  TGlobalBuilderChain.BroadcastEventAsync(
    TMessageEvent.Create(
      'ExecCmd: ' + ACmd + ' ' + String.Join(' ', AArgs),
      TMessageLevel.Explanatory));

  TExecCmdService.Cmd(
    ACmd,
    {$IFDEF POSIX}[ACmd] + {$ENDIF} AArgs,
    [])
    .Run(LResult)
      .Wait();

  LResult := LResult.Trim();
  ACmdResult.Add(LResult);

  TGlobalBuilderChain.BroadcastEventAsync(
    TMessageEvent.Create(LResult, TMessageLevel.Explanatory));
end;

function TADBService.FindDeviceVendorModel(const AAdbPath, ADevice: string): string;
begin
  var LStrings := TStringList.Create();
  try
    ExecCmd(AAdbPath, [
      '-s',
      ADevice,
      'shell',
      'getprop',
      'ro.product.model'],
      LStrings);
    Result := LStrings.Text
      .Replace(#13#10, String.Empty);
  finally
    LStrings.Free();
  end;
end;

procedure TADBService.ForceStopApp(const AAdbPath, APkgName, ADevice: string;
  const AResult: TStrings);
begin
  ExecCmd(AAdbPath, [
    '-s',
    ADevice,
    'shell',
    'am',
    'force-stop',
    APkgName],
    AResult);
end;

function TADBService.GetAppInstallationPath(const AAdbPath, APkgName,
  ADevice: string; const AResult: TStrings): string;
begin
  var LStrings := TStringList.Create();
  try
    AResult.AddStrings(LStrings);
    ExecCmd(AAdbPath, [
      '-s',
      ADevice,
      'shell',
      'pm',
      'path',
      APkgName],
      AResult);
    Result := LStrings.Text.Replace(sLineBreak, '', [rfReplaceAll]);
  finally
    LStrings.Free();
  end;
end;

function TADBService.BuildApk(const AAppBasePath, AProjectName: string;
  const AEnvironmentModel: TEnvironmentModel; const AResult: TStrings): boolean;
begin
  var LAppBinPath := TPath.Combine(AAppBasePath, 'bin');
  if TDirectory.Exists(LAppBinPath) then
    TDirectory.Delete(LAppBinPath, true);
  TDirectory.CreateDirectory(LAppBinPath);

  SetCurrentDir(AAppBasePath);

  ExecCmd(AEnvironmentModel.AAptLocation, [
    'package',
    '-f',
    '-m',
    '-J',
    '.',
    '-M',
    'AndroidManifest.xml',
    '-S',
    'res',
    '-I',
    TPath.Combine(AEnvironmentModel.SdkApiLocation, 'android.jar')],
    AResult);

  ExecCmd(AEnvironmentModel.AAptLocation, [
    'package',
    '-f',
    '-m',
    '-F',
    Format(TPath.Combine('bin', '%s.unaligned.apk'), [AProjectName]),
    '-M',
    'AndroidManifest.xml',
    '-S',
    'res',
    '-I',
    TPath.Combine(AEnvironmentModel.SdkApiLocation, 'android.jar')],
    AResult);

  var LSourceDexFileName := TPath.Combine(AAppBasePath, TPath.Combine('classes', 'classes.dex'));
  var LDestDexFilePath := TPath.Combine(TPath.Combine(AAppBasePath, 'bin'), 'classes.dex');

  TFile.Copy(LSourceDexFileName, LDestDexFilePath);

  SetCurrentDir(LAppBinPath);

  ExecCmd(AEnvironmentModel.AAptLocation, [
    'add',
    AProjectName + '.unaligned.apk',
    'classes.dex'],
    AResult);

  var LSourceAssetsFolder := TPath.Combine(AAppBasePath, 'assets');
  var LDestAssetsFolder := TPath.Combine(AAppBasePath, TPath.Combine('bin', 'assets'));
  TDirectory.Copy(LSourceAssetsFolder, LDestAssetsFolder);

  EnumAssets(TPath.Combine(LAppBinPath, 'assets'),
    procedure(AFile: string) begin
      ExecCmd(AEnvironmentModel.AAptLocation, [
        'add',
        AProjectName + '.unaligned.apk',
        AFile],
        AResult);
    end);

  var LSourceLibDir := TPath.Combine(AAppBasePath, TPath.Combine('library', 'lib'));
  var LDestLibDir := TPath.Combine(AAppBasePath, TPath.Combine('bin', 'lib'));
  TDirectory.Copy(LSourceLibDir, LDestLibDir);

  EnumLibraries(TPath.Combine(LAppBinPath, 'lib'),
    procedure(AFile: string) begin
      ExecCmd(AEnvironmentModel.AAptLocation, [
        'add',
        AProjectName + '.unaligned.apk',
        AFile],
        AResult);
    end);

  SetCurrentDir(AAppBasePath);

  ExecCmd(AEnvironmentModel.JarSignerLocation, [
    '-keystore',
    TPath.Combine('cert', 'PyApp.keystore'),
    '-storepass',
    'delphirocks',
    Format(TPath.Combine('bin', '%s.unaligned.apk'), [AProjectName]),
    'PyApp'],
    AResult);

  ExecCmd(AEnvironmentModel.ZipAlignLocation, [
    '-f',
    '4',
    Format(TPath.Combine('bin', '%s.unaligned.apk'), [AProjectName]),
    Format(TPath.Combine('bin', '%s.apk'), [AProjectName])],
    AResult);

  //This is the jar file... we want the bat on the parent dir
  var LApkSignerDir := TDirectory.GetParent(ExtractFileDir(AEnvironmentModel.ApkSignerLocation));
  {$IFDEF POSIX}
  var LApkSignerPath := TPath.Combine(LApkSignerDir, ChangeFileExt(ExtractFileName(AEnvironmentModel.ApkSignerLocation), ''));
  {$ELSE}
  var LApkSignerPath := TPath.Combine(LApkSignerDir, ChangeFileExt(ExtractFileName(AEnvironmentModel.ApkSignerLocation), '.bat'));
  {$ENDIF}

  ExecCmd(LApkSignerPath, [
    'sign',
    '--ks-key-alias',
    'PyApp',
    '--ks',
    TPath.Combine('cert', 'PyApp.keystore'),
    '--ks-pass',
    'pass:delphirocks',
    '--key-pass',
    'pass:delphirocks',
    Format(TPath.Combine('bin', '%s.apk'), [AProjectName])],
    AResult);

  var LApkPath := TPath.Combine(LAppBinPath, ChangeFileExt(AProjectName, '.apk'));
  Result := TFile.Exists(LApkPath)
    and not AResult.Text.Contains('Failure');
end;

function TADBService.InstallApk(const AAdbPath, AApkPath, ADevice: string; const AResult: TStrings): boolean;
begin
  var LStrings := TStringList.Create();
  try
    ExecCmd(AAdbPath, [
      '-s',
      ADevice,
      'install',
      AApkPath],
      LStrings);
    AResult.AddStrings(LStrings);
    Result := (not LStrings.Text.Contains('failure')) and (not LStrings.Text.Contains('failed'));
  finally
    LStrings.Free();
  end;
end;

function TADBService.IsAppInstalled(const AAdbPath, APkgName, ADevice: string;
  const AResult: TStrings): boolean;
begin
  var LStrings := TStringList.Create();
  try
    ExecCmd(AAdbPath, [
      '-s',
      ADevice,
      'shell',
      'pm',
      'list',
      'packages',
      '|',
      'grep',
      APkgName],
      LStrings);
    AResult.AddStrings(LStrings);
    Result := LStrings.Text.Contains(APkgName);
  finally
    LStrings.Free();
  end;
end;

function TADBService.IsAppRunning(const AAdbPath, APkgName, ADevice: string;
  const AResult: TStrings): boolean;
var
  LPid: integer;
begin
  var LStrings := TStringList.Create();
  try
    ExecCmd(AAdbPath, [
      '-s',
      ADevice,
      'shell',
      'pidof',
      APkgName],
      LStrings);
    AResult.AddStrings(LStrings);
    Result := TryStrToInt(LStrings.Text.Replace(sLineBreak, '', [rfReplaceAll]), LPid);
  finally
    LStrings.Free();
  end;
end;

function TADBService.UnInstallApk(const AAdbPath, APkgName, ADevice: string;
  const AResult: TStrings): boolean;
begin
  var LStrings := TStringList.Create();
  try
    ExecCmd(AAdbPath, [
      '-s',
      ADevice,
      'uninstall',
      APkgName],
      LStrings);
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
    ExecCmd(AAdbPath, ['devices'], LStrings);
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
  ExecCmd(AAdbPath, [
    '-s',
    ADevice,
    'shell',
    'am',
    'start',
    '-n',
    Format('%s/com.embarcadero.firemonkey.FMXNativeActivity', [APkgName])],
    AResult);
end;

procedure TADBService.DebugApp(const AAdbPath, APkgName, ADevice, AHost: string;
  const APort: integer; const AResult: TStrings);
begin
  ExecCmd(AAdbPath, [
    '-s',
    ADevice,
    'shell',
    'am',
    'start',
    '-n',
    Format('%s/com.embarcadero.firemonkey.FMXNativeActivity', [APkgName]),
    '--es',
    'args',
    Format('"--dbg -host %s -port %d"'.QuotedString(), [AHost, APort])],
    AResult);
end;

procedure TADBService.StartDebugSession(const AAdbPath: string; const APort: integer; const AResult: TStrings);
begin
  ExecCmd(AAdbPath, [
    'forward',
    'tcp:' + APort.ToString(),
    'tcp:' + APort.ToString()],
    AResult);
end;

procedure TADBService.StopDebugSession(const AAdbPath: string; const APort: integer; const AResult: TStrings);
begin
  ExecCmd(AAdbPath, [
    'forward',
    '--remove',
    'tcp:' + APort.ToString()],
    AResult);
end;

end.
