unit Builder.Services.ADB;

interface

uses
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  PyTools.ExecCmd,
  Builder.Storage,
  Builder.Services,
  Builder.Model.Environment;

type
  TADBService = class(TInterfacedObject, IADBServices)
  private
    class var FActiveDevice: string;
  private
    FEnvironmentStorage: IStorage<TEnvironmentModel>;
    FEnvironmentModel: TEnvironmentModel;

    procedure CheckEnvironmentModel();

    function ExecCmd(const ACmd: string; const AArgs, AEnv: TArray<string>; out AOutput: string): integer; overload;
    function ExecCmd(const ACmd: string; const AArgs, AEnv: TArray<string>): integer; overload;
    procedure ExecCmd(const ACmd: string; const AArgs: TArray<string>; ACmdResult: TStrings); overload;

    procedure EnumAssets(const AAssetsBasePath: string; const AProc: TProc<string>);
    procedure EnumLibraries(const ALibBasePath: string; const AProc: TProc<string>);

    procedure EnumDevices(const ADeviceList: TStrings; const AProc: TProc<string>);
    function FindDeviceVendorModel(const AAdbPath, ADevice: string): string;

    procedure SetActiveDevice(const ADeviceName: string);
    function GetActiveDevice(): string;
  public
    constructor Create();
    destructor Destroy(); override;

    procedure ListDevices(const AStrings: TStrings);
    procedure CheckActiveDevice();

    //Exec subprocess
    procedure RunSubprocess(const ACmd: string; const AArgs, AEnvVars: TArray<string>);

    //File helpers
    function SendFile(const ALocalFilePath, ARemoteFilePath: string): boolean;
    procedure RemoveFile(const ARemoteFilePath: string);
    function ExtractZip(const ARemoteFilePath, ARemoteDir: string): boolean;
    //Folder helpers
    function CreateDirectory(const ARemoteDir: string): boolean;
    procedure DeleteDirectory(const ARemoteDir: string);
    function DirectoryExists(const ARemoteDir: string): boolean;

    //App helpers
    function GetAppInstallationPath(const APkgName: string): string;
    function IsAppInstalled(const APkgName: string): boolean;
    function IsAppRunning(const APkgName: string): boolean;

    function BuildApk(const AAppBasePath, AProjectName: string): boolean;
    function InstallApk(const AApkPath: string): boolean;
    function UnInstallApk(const APkgName: string): boolean;

    procedure RunApp(const APkgName: string);
    procedure StartDebugSession(const APort: integer);
    procedure StopDebugSession(const APort: integer);
    procedure DebugApp(const APkgName, AHost: string; const APort: integer);
    procedure ForceStopApp(const APkgName: string);
  end;

implementation

uses
  System.SyncObjs,
  Builder.Chain,
  Builder.Exception,
  Builder.Storage.Default;

{ TADBService }

constructor TADBService.Create;
begin
  inherited;
  FEnvironmentStorage := TDefaultStorage<TEnvironmentModel>.Make();
end;

destructor TADBService.Destroy;
begin
  FEnvironmentModel.Free();
  inherited;
end;

function TADBService.GetActiveDevice: string;
begin
  Result := FActiveDevice;
end;

procedure TADBService.SetActiveDevice(const ADeviceName: string);
begin
  FActiveDevice := ADeviceName;
end;

procedure TADBService.CheckActiveDevice;
begin
  if GetActiveDevice().IsEmpty() then
    raise ENoActiveDevice.Create('No device selected.');
end;

procedure TADBService.CheckEnvironmentModel;
begin
  if Assigned(FEnvironmentModel) then
    Exit;

  if not FEnvironmentStorage.LoadModel(FEnvironmentModel) then
    raise EEmptySettings.Create('The Environment Settings are empty.');
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

function TADBService.ExecCmd(const ACmd: string; const AArgs,
  AEnv: TArray<string>; out AOutput: string): integer;
begin
  TGlobalBuilderChain.BroadcastEventAsync(
    TMessageEvent.Create(
      'ExecCmd: ' + ACmd + ' ' + String.Join(' ', AArgs),
      TMessageLevel.Explanatory));

  Result := TExecCmdService.Cmd(
    ACmd,
    {$IFDEF POSIX}[ACmd] + {$ENDIF} AArgs,
    AEnv)
  .Run(AOutput)
    .Wait();

  AOutput := AOutput.Trim();

  TGlobalBuilderChain.BroadcastEventAsync(
    TMessageEvent.Create(AOutput, TMessageLevel.Explanatory));
end;

function TADBService.ExecCmd(const ACmd: string; const AArgs,
  AEnv: TArray<string>): integer;
var
  LOutput: string;
begin
  Result := ExecCmd(ACmd, AArgs, AEnv, LOutput);
end;

procedure TADBService.ExecCmd(const ACmd: string; const AArgs: TArray<string>;
  ACmdResult: TStrings);
var
  LOutput: string;
begin
  ExecCmd(ACmd, AArgs, [], LOutput);
  if Assigned(ACmdResult) then
    ACmdResult.Add(LOutput);
end;

function TADBService.CreateDirectory(const ARemoteDir: string): boolean;
var
  LOutput: string;
begin
  CheckEnvironmentModel();
  if ExecCmd(FEnvironmentModel.AdbLocation, [
    '-s',
    GetActiveDevice(),
    'shell',
    'mkdir',
    '-p',
    ARemoteDir],
    [],
    LOutput) <> EXIT_SUCCESS then
      Result := false
    else
      Result := LOutput.Replace(sLineBreak, String.Empty).IsEmpty();
end;

function TADBService.SendFile(const ALocalFilePath, ARemoteFilePath: string): boolean;
begin
  CheckEnvironmentModel();
  Result := ExecCmd(FEnvironmentModel.AdbLocation, [
    '-s',
    GetActiveDevice(),
    'push',
    ALocalFilePath,
    ARemoteFilePath],
    []) = EXIT_SUCCESS;
end;

function TADBService.ExtractZip(const ARemoteFilePath,
  ARemoteDir: string): boolean;
var
  LOutput: string;
begin
  CheckEnvironmentModel();
  if ExecCmd(FEnvironmentModel.AdbLocation, [
    '-s',
    GetActiveDevice(),
    'shell',
    'unzip',
    '-o',
    ARemoteFilePath,
    '-d',
    ARemoteDir],
    [],
    LOutput) <> EXIT_SUCCESS then
      Result := false
    else
      Result := LOutput.Replace(sLineBreak, String.Empty).IsEmpty();
end;

function TADBService.FindDeviceVendorModel(const AAdbPath, ADevice: string): string;
begin
  ExecCmd(AAdbPath, [
    '-s',
    ADevice,
    'shell',
    'getprop',
    'ro.product.model'],
    [],
    Result);

  Result := Result.Replace(sLineBreak, String.Empty);
end;

procedure TADBService.ForceStopApp(const APkgName: string);
begin
  CheckEnvironmentModel();
  ExecCmd(FEnvironmentModel.AdbLocation, [
    '-s',
    GetActiveDevice(),
    'shell',
    'am',
    'force-stop',
    APkgName],
    []);
end;

function TADBService.GetAppInstallationPath(const APkgName: string): string;
begin
  CheckEnvironmentModel();
  ExecCmd(FEnvironmentModel.AdbLocation, [
    '-s',
    GetActiveDevice(),
    'shell',
    'pm',
    'path',
    APkgName],
    [],
    Result);

  Result := Result.Replace(sLineBreak, String.Empty, [rfReplaceAll]);
end;

function TADBService.BuildApk(const AAppBasePath, AProjectName: string): boolean;
begin
  CheckEnvironmentModel();
  var LAppBinPath := TPath.Combine(AAppBasePath, 'bin');
  if TDirectory.Exists(LAppBinPath) then
    TDirectory.Delete(LAppBinPath, true);
  TDirectory.CreateDirectory(LAppBinPath);

  SetCurrentDir(AAppBasePath);

  if ExecCmd(FEnvironmentModel.AAptLocation, [
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
    TPath.Combine(FEnvironmentModel.SdkApiLocation, 'android.jar')],
    []) <> EXIT_SUCCESS then
      Exit(false);

  if ExecCmd(FEnvironmentModel.AAptLocation, [
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
    TPath.Combine(FEnvironmentModel.SdkApiLocation, 'android.jar')],
    []) <> EXIT_SUCCESS then
      Exit(false);

  var LSourceDexFileName := TPath.Combine(AAppBasePath, TPath.Combine('classes', 'classes.dex'));
  var LDestDexFilePath := TPath.Combine(TPath.Combine(AAppBasePath, 'bin'), 'classes.dex');

  TFile.Copy(LSourceDexFileName, LDestDexFilePath);

  SetCurrentDir(LAppBinPath);

  if ExecCmd(FEnvironmentModel.AAptLocation, [
    'add',
    AProjectName + '.unaligned.apk',
    'classes.dex'],
    []) <> EXIT_SUCCESS then
      Exit(false);

  var LSourceAssetsFolder := TPath.Combine(AAppBasePath, 'assets');
  var LDestAssetsFolder := TPath.Combine(AAppBasePath, TPath.Combine('bin', 'assets'));
  TDirectory.Copy(LSourceAssetsFolder, LDestAssetsFolder);

  EnumAssets(TPath.Combine(LAppBinPath, 'assets'),
    procedure(AFile: string) begin
      ExecCmd(FEnvironmentModel.AAptLocation, [
        'add',
        AProjectName + '.unaligned.apk',
        AFile],
        []);
    end);

  var LSourceLibDir := TPath.Combine(AAppBasePath, TPath.Combine('library', 'lib'));
  var LDestLibDir := TPath.Combine(AAppBasePath, TPath.Combine('bin', 'lib'));
  TDirectory.Copy(LSourceLibDir, LDestLibDir);

  EnumLibraries(TPath.Combine(LAppBinPath, 'lib'),
    procedure(AFile: string) begin
      ExecCmd(FEnvironmentModel.AAptLocation, [
        'add',
        AProjectName + '.unaligned.apk',
        AFile],
        []);
    end);

  SetCurrentDir(AAppBasePath);

  if ExecCmd(FEnvironmentModel.JarSignerLocation, [
    '-keystore',
    TPath.Combine('cert', 'PyApp.keystore'),
    '-storepass',
    'delphirocks',
    Format(TPath.Combine('bin', '%s.unaligned.apk'), [AProjectName]),
    'PyApp'],
    []) <> EXIT_SUCCESS then
      Exit(false);

  if ExecCmd(FEnvironmentModel.ZipAlignLocation, [
    '-f',
    '4',
    Format(TPath.Combine('bin', '%s.unaligned.apk'), [AProjectName]),
    Format(TPath.Combine('bin', '%s.apk'), [AProjectName])],
    []) <> EXIT_SUCCESS then
      Exit(false);

  //This is the jar file... we want the bat on the parent dir
  var LApkSignerDir := TDirectory.GetParent(ExtractFileDir(FEnvironmentModel.ApkSignerLocation));
  {$IFDEF POSIX}
  var LApkSignerPath := TPath.Combine(LApkSignerDir, ChangeFileExt(ExtractFileName(FEnvironmentModel.ApkSignerLocation), ''));
  {$ELSE}
  var LApkSignerPath := TPath.Combine(LApkSignerDir, ChangeFileExt(ExtractFileName(FEnvironmentModel.ApkSignerLocation), '.bat'));
  {$ENDIF}

  if ExecCmd(LApkSignerPath, [
    'sign',
    '--ks-key-alias',
    'PyApp',
    '--ks',
    TPath.Combine('cert', 'PyApp.keystore'),
    '--v1-signing-enabled true',
    '--v2-signing-enabled true',
    '--ks-pass',
    'pass:delphirocks',
    '--key-pass',
    'pass:delphirocks',
    Format(TPath.Combine('bin', '%s.apk'), [AProjectName])],
    []) <> EXIT_SUCCESS then
      Exit(false);

  var LApkPath := TPath.Combine(LAppBinPath, ChangeFileExt(AProjectName, '.apk'));
  Result := TFile.Exists(LApkPath);
end;

function TADBService.InstallApk(const AApkPath: string): boolean;
var
  LOutput: string;
begin
  CheckEnvironmentModel();
  if ExecCmd(FEnvironmentModel.AdbLocation, [
    '-s',
    GetActiveDevice(),
    'install',
    AApkPath],
    [],
    LOutput) <> EXIT_SUCCESS then
      Result := false
    else
      Result := (not LOutput.Contains('failure')) and (not LOutput.Contains('failed'));
end;

function TADBService.IsAppInstalled(const APkgName: string): boolean;
var
  LOutput: string;
begin
  CheckEnvironmentModel();
  if ExecCmd(FEnvironmentModel.AdbLocation, [
    '-s',
    GetActiveDevice(),
    'shell',
    'pm',
    'list',
    'packages',
    '|',
    'grep',
    APkgName],
    [],
    LOutput) <> EXIT_SUCCESS then
      Result := false
    else
      Result := LOutput.Contains(APkgName);
end;

function TADBService.IsAppRunning(const APkgName: string): boolean;
var
  LOutput: string;
  LPid: integer;
begin
  CheckEnvironmentModel();
  if ExecCmd(FEnvironmentModel.AdbLocation, [
    '-s',
    GetActiveDevice(),
    'shell',
    'pidof',
    APkgName],
    [],
    LOutput) <> EXIT_SUCCESS then
      Result := false
    else
      Result := TryStrToInt(LOutput.Replace(sLineBreak, String.Empty, [rfReplaceAll]), LPid);
end;

function TADBService.UnInstallApk(const APkgName: string): boolean;
var
  LOutput: string;
begin
  CheckEnvironmentModel();
  if ExecCmd(FEnvironmentModel.AdbLocation, [
    '-s',
    GetActiveDevice(),
    'uninstall',
    APkgName],
    [],
    LOutput) <> EXIT_SUCCESS then
      Result := false
    else
      Result := (not LOutput.Contains('failure')) and (not LOutput.Contains('failed'));
end;

procedure TADBService.ListDevices(const AStrings: TStrings);
begin
  FreeAndNil(FEnvironmentModel);
  CheckEnvironmentModel();
  if not TFile.Exists(FEnvironmentModel.AdbLocation) then
    Exit;

  var LStrings := TStringList.Create();
  try
    ExecCmd(FEnvironmentModel.AdbLocation, ['devices'], LStrings);
    EnumDevices(LStrings, procedure(ADevice: string) begin
      AStrings.AddPair(
        ADevice,
        FindDeviceVendorModel(FEnvironmentModel.AdbLocation, ADevice));
    end);
  finally
    LStrings.Free();
  end;
end;

procedure TADBService.RemoveFile(const ARemoteFilePath: string);
begin
  CheckEnvironmentModel();
  ExecCmd(FEnvironmentModel.AdbLocation, [
    '-s',
    GetActiveDevice(),
    'shell',
    'rm',
    ARemoteFilePath],
    []);
end;

procedure TADBService.RunApp(const APkgName: string);
begin
  CheckEnvironmentModel();
  ExecCmd(FEnvironmentModel.AdbLocation, [
    '-s',
    GetActiveDevice(),
    'shell',
    'am',
    'start',
    '-n',
    Format('%s/com.embarcadero.firemonkey.FMXNativeActivity', [APkgName])],
    []);
end;

procedure TADBService.RunSubprocess(const ACmd: string;
  const AArgs, AEnvVars: TArray<string>);
var
  LCmd: string;
  LExport: string;
  LArgs: TArray<string>;
begin
  LCmd := String.Empty;
  for LExport in AEnvVars do begin
    if not LCmd.IsEmpty() then
      LCmd := LCmd + ' && ';
    LCmd := LCmd + 'export ' + LExport;
  end;

  if not LCmd.IsEmpty() then
    LCmd := LCmd + ' &&';

  LCmd := LCmd + ' exec ' + ACmd;

  LArgs := ['-s',
    GetActiveDevice(),
    'shell',
    '"' + (LCmd + ' ' + String.Join(' ', AArgs)) + '"'
  ];

  TExecCmdService.Cmd(
    FEnvironmentModel.AdbLocation,
    {$IFDEF POSIX}[FEnvironmentModel.AdbLocation] + {$ENDIF} LArgs,
    [])
  .Run({No redirections - the child process will use our pipes})
    .Wait();
end;

procedure TADBService.DebugApp(const APkgName, AHost: string;
  const APort: integer);
begin
  CheckEnvironmentModel();
  ExecCmd(FEnvironmentModel.AdbLocation, [
    '-s',
    GetActiveDevice(),
    'shell',
    'am',
    'start',
    '-n',
    Format('%s/com.embarcadero.firemonkey.FMXNativeActivity', [APkgName]),
    '--es',
    'args',
    Format('"--dbg --run -host %s -port %d"'.QuotedString(), [AHost, APort])], []);
end;

procedure TADBService.DeleteDirectory(const ARemoteDir: string);
begin
  CheckEnvironmentModel();
  ExecCmd(FEnvironmentModel.AdbLocation, [
    '-s',
    GetActiveDevice(),
    'shell',
    'rm',
    '-r',
    ARemoteDir], []);
end;

function TADBService.DirectoryExists(const ARemoteDir: string): boolean;
var
  LOutput: string;
begin
  CheckEnvironmentModel();
  if ExecCmd(FEnvironmentModel.AdbLocation, [
    '-s',
    GetActiveDevice(),
    'shell',
    ('ls ' + ARemoteDir + ' > /data/local/tmp/null 2>&1 && echo 1 || echo 0')],
    [],
    LOutput) <> EXIT_SUCCESS then
      Result := false
    else
      Result := LOutput.Contains('1');
end;

procedure TADBService.StartDebugSession(const APort: integer);
begin
  CheckEnvironmentModel();
  ExecCmd(FEnvironmentModel.AdbLocation, [
    'forward',
    'tcp:' + APort.ToString(),
    'tcp:' + APort.ToString()], []);
end;

procedure TADBService.StopDebugSession(const APort: integer);
begin
  CheckEnvironmentModel();
  ExecCmd(FEnvironmentModel.AdbLocation, [
    'forward',
    '--remove',
    'tcp:' + APort.ToString()], []);
end;

end.
