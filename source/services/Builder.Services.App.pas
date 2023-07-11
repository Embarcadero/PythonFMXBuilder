unit Builder.Services.App;

interface

uses
  System.Classes,
  System.IOUtils,
  System.JSON,
  Builder.Types,
  Builder.Services,
  Builder.Model.Project,
  Builder.Model.Environment;

type
  TAppService = class(TInterfacedObject, IAppServices)
  private
    FAdbServices: IAdbServices;
    procedure ClearAssetsInternal(const AProjectName: string);
    procedure ClearDeployInfo(const AProjectName: string);
    procedure AddAssetsInternalFileToDeployInfo(const AProjectName: string; const AFileName: string);
    procedure RemoveAssetsInternalFileToDeployInfo(const AProjectName: string; const AFileName: string);
  public
    constructor Create();

    procedure CopyAppFiles(const AModel: TProjectModel);
    procedure CopyIcons(const AModel: TProjectModel);
    //App script files
    procedure CopyModules(const AModel: TProjectModel);
    procedure CopyDependencies(const AModel: TProjectModel);
    procedure CopyPackages(const AModel: TProjectModel);
    procedure CopyOtherFiles(const AModel: TProjectModel);
    procedure CopyDebugger(const AModel: TProjectModel);
    procedure TryCopyDebugger(const AModel: TProjectModel);
    //App deployables
    function AddFile(const AModel: TProjectModel; const AFileName: string;
      const AStream: TStream): string;
    procedure RemoveFile(const AModel: TProjectModel; const AFilePath: string);
    function GetFiles(const AModel: TProjectModel;
      const AFilter: TDirectory.TFilterPredicate = nil): TArray<string>;
    //App defs. file (used by the Android app)
    procedure CreateAppDefs(const AModel: TProjectModel);
    procedure AddAppDefsModules(const AModel: TProjectModel; const AJSONArray: TJSONArray);
    procedure TryAddAppDefsDebug(const AModel: TProjectModel; const AJSONArray: TJSONArray);
    //App basic info
    procedure UpdateManifest(const AModel: TProjectModel);
    //Project builder
    procedure BuildProject(const AModel: TProjectModel);
    //App builder
    function BuildApk(const AProjectModel: TProjectModel;
      const AEnvironmentModel: TEnvironmentModel): boolean;
    //App installation
    function InstallApk(const AProjectModel: TProjectModel;
      const AEnvironmentModel: TEnvironmentModel; const ADevice: string): boolean;
    function UnInstallApk(const AProjectModel: TProjectModel;
      const AEnvironmentModel: TEnvironmentModel; const ADevice: string): boolean;
    function IsAppInstalled(const AProjectModel: TProjectModel;
      const AEnvironmentModel: TEnvironmentModel; const ADevice: string): boolean;
    function IsAppRunning(const AProjectModel: TProjectModel;
      const AEnvironmentModel: TEnvironmentModel; const ADevice: string): boolean;
  end;

implementation

uses
  System.SysUtils,
  Builder.Paths,
  Builder.Exception,
  Builder.Services.Factory;

{ TAppService }

constructor TAppService.Create;
begin
  inherited;
  FAdbServices := TServiceSimpleFactory.CreateAdb();
end;

function TAppService.InstallApk(const AProjectModel: TProjectModel;
  const AEnvironmentModel: TEnvironmentModel; const ADevice: string): boolean;
begin
  var LApkPath := TBuilderPaths.GetApkPath(AProjectModel.ProjectName);
  if not TFile.Exists(LApkPath) then
    raise EApkFileNotFound.CreateFmt('Apk file %s not found at: %s', [
      AProjectModel.ProjectName, LApkPath]);

  Result := FAdbServices.InstallApk(LApkPath);
end;

function TAppService.IsAppInstalled(const AProjectModel: TProjectModel;
  const AEnvironmentModel: TEnvironmentModel; const ADevice: string): boolean;
begin
  Result := FAdbServices.IsAppInstalled(AProjectModel.PackageName);
end;

function TAppService.IsAppRunning(const AProjectModel: TProjectModel;
  const AEnvironmentModel: TEnvironmentModel; const ADevice: string): boolean;
begin
  Result := FAdbServices.IsAppRunning(AProjectModel.PackageName);
end;

function TAppService.UnInstallApk(const AProjectModel: TProjectModel;
  const AEnvironmentModel: TEnvironmentModel; const ADevice: string): boolean;
begin
  Result := FAdbServices.UnInstallApk(AProjectModel.PackageName);
end;

procedure TAppService.UpdateManifest(const AModel: TProjectModel);
begin
  var LManifestPath := TBuilderPaths.GetManifestPath(AModel.ProjectName);
  var LText := TFile.ReadAllText(LManifestPath, TEncoding.UTF8);
  LText := LText
    .Replace('package="com.embarcadero.PyApp"', Format('package="%s"', [AModel.PackageName]))
    .Replace('android:versionCode="1"', Format('android:versionCode="%s"', [AModel.VersionCode.ToString()]))
    .Replace('android:versionName="1.0.0"', Format('android:versionName="%s"', [AModel.VersionName]))
    //android:exported="true" requires this piece of code added to your main activity
    //.Replace('android:label="PyApp"', 'android:label="PyApp"' + ' ' + 'android:exported="true"')
    .Replace('android:label="PyApp"', Format('android:label="%s"', [AModel.ApplicationName]));

  TFile.WriteAllText(LManifestPath, LText);
end;

{ TPreBuiltCopyService }

procedure TAppService.AddAssetsInternalFileToDeployInfo(
  const AProjectName: string; const AFileName: string);
begin
  var LDeployInfoFolder := TBuilderPaths.GetAppAssetsDeployInfoFolder(AProjectName);
  if not TDirectory.Exists(LDeployInfoFolder) then
    TDirectory.CreateDirectory(LDeployInfoFolder);

  var LDelpoyedDataSetsFile := TPath.Combine(LDeployInfoFolder, 'deployedassets.txt');
  if not TFile.Exists(LDelpoyedDataSetsFile) then
    TFile.Create(LDelpoyedDataSetsFile).Free();

  var LDeployedFilePath := '.\assets\internal\' + AFileName;

  var LContent := TFile.ReadAllText(LDelpoyedDataSetsFile);

  if not LContent.IsEmpty() then
    TFile.AppendAllText(LDelpoyedDataSetsFile, #13#10 {MUST be CRLF even on UNIX});

  if not LContent.Contains(LDeployedFilePath) then
    TFile.AppendAllText(LDelpoyedDataSetsFile, LDeployedFilePath);
end;

procedure TAppService.RemoveAssetsInternalFileToDeployInfo(const AProjectName,
  AFileName: string);
begin
  var LDeployInfoFolder := TBuilderPaths.GetAppAssetsDeployInfoFolder(AProjectName);
  if not TDirectory.Exists(LDeployInfoFolder) then
    Exit;

  var LDelpoyedDataSetsFile := TPath.Combine(LDeployInfoFolder, 'deployedassets.txt');
  if not TFile.Exists(LDelpoyedDataSetsFile) then
    Exit;

  if not TFile.Exists(LDelpoyedDataSetsFile) then
    Exit;

  var LDeployedFile := '.\assets\internal\' + AFileName;
  var LData := TStringList.Create();
  try
    LData.LoadFromFile(LDelpoyedDataSetsFile);
    var LIndex := -1;
    if LData.Find(LDeployedFile, LIndex) then
      LData.Delete(LIndex);
    LData.SaveToFile(LDelpoyedDataSetsFile);
  finally
    LData.Free();
  end;
end;

function TAppService.AddFile(const AModel: TProjectModel;
  const AFileName: string; const AStream: TStream): string;
var
  LBytes: TBytes;
begin
  var LAssetsInternal := TBuilderPaths.GetAppAssetsInternalFolder(AModel.ProjectName);

  SetLength(LBytes, AStream.Size);
  AStream.Position := 0;
  AStream.Read(LBytes, AStream.Size);
  var LFilePath := TPath.Combine(LAssetsInternal, AFileName);
  TFile.WriteAllBytes(LFilePath, LBytes);

  AddAssetsInternalFileToDeployInfo(AModel.ProjectName, AFileName);

  Result := LFilePath
end;

procedure TAppService.RemoveFile(const AModel: TProjectModel;
  const AFilePath: string);
begin
  var LScriptFolder := TBuilderPaths.GetAppAssetsInternalFolder(AModel.ProjectName);
  //Remove from the dataset file
  RemoveAssetsInternalFileToDeployInfo(AModel.ProjectName, TPath.GetFileName(AFilePath));
  //Physically delete file
  TFile.Delete(AFilePath);
end;

function TAppService.GetFiles(
  const AModel: TProjectModel; const AFilter: TDirectory.TFilterPredicate): TArray<string>;
begin
  var LScriptFolder := TBuilderPaths.GetAppAssetsInternalFolder(AModel.ProjectName);
  Result := TDirectory.GetFiles(LScriptFolder, '*', TSearchOption.soTopDirectoryOnly, AFilter);
end;

function TAppService.BuildApk(const AProjectModel: TProjectModel;
  const AEnvironmentModel: TEnvironmentModel): boolean;
begin
  Result := FAdbServices.BuildApk(TBuilderPaths.GetAppPath(AProjectModel.ProjectName),
    AProjectModel.ProjectName);
end;

procedure TAppService.BuildProject(const AModel: TProjectModel);
begin
  //Copy Python and other APP files
  CopyAppFiles(AModel);
  //Copy app icons
  CopyIcons(AModel);
  //Save aditional scripts to the APP files
  CopyModules(AModel);
  //Save aditional dependencies to the APP files
  CopyDependencies(AModel);
  //Save user packages to the APP files
  CopyPackages(AModel);
  //Save user other files to the APP files
  CopyOtherFiles(AModel);
  //Save debugger files if build conf. in debug mode
  TryCopyDebugger(AModel);
  //Update the manifest with the custom APP settings
  UpdateManifest(AModel);
  //Creates the app_defs file - it defines which file is the main script and more
  CreateAppDefs(AModel);
end;

procedure TAppService.ClearAssetsInternal(const AProjectName: string);
begin
  var LAssetsInternalFolder := TBuilderPaths.GetAppAssetsInternalFolder(AProjectName);
  if TDirectory.Exists(LAssetsInternalFolder) then
    TDirectory.Delete(LAssetsInternalFolder, true);
end;

procedure TAppService.ClearDeployInfo(const AProjectName: string);
begin
  var LDeployInfoFolder := TBuilderPaths.GetAppAssetsDeployInfoFolder(AProjectName);
  var LDelpoyedDataSetsFile := TPath.Combine(LDeployInfoFolder, 'deployedassets.txt');
  if TFile.Exists(LDelpoyedDataSetsFile) then
    TFile.Delete(LDelpoyedDataSetsFile);
end;

procedure TAppService.CopyAppFiles(const AModel: TProjectModel);
begin
  {|||||| APP folder ||||||}
  var LAppPath := TBuilderPaths.GetAppPath(AModel.ProjectName);

  if TDirectory.Exists(LAppPath) then
    TDirectory.Delete(LAppPath, true);

  TDirectory.CreateDirectory(LAppPath);

  {|||||| Pre-build APP folder ||||||}

  var LPreBuiltFolder := TBuilderPaths.GetPreBuiltFolder(AModel.Architecture);
  if not TDirectory.Exists(LPreBuiltFolder) then
    raise EPreBuiltFolderNotFound.CreateFmt('Pre-built folder not found at: %s', [LPreBuiltFolder]);

  //Copy the app image to the target app path
  TDirectory.Copy(LPreBuiltFolder, LAppPath);

  ClearAssetsInternal(AModel.ProjectName);
  ClearDeployInfo(AModel.ProjectName);

  var LAppAssetsInternalFolder := TBuilderPaths.GetAppAssetsInternalFolder(AModel.ProjectName);
  //Create the /assets/internal/ folder
  if not TDirectory.Exists(LAppAssetsInternalFolder) then
    TDirectory.CreateDirectory(LAppAssetsInternalFolder);

  {|||||| Python distribution zip file ||||||}

  var LPythonZipFile := TBuilderPaths.GetPythonZipFile(AModel.PythonVersion, AModel.Architecture);
  if not TFile.Exists(LPythonZipFile) then
    raise EPythonZipFileNotFound.CreateFmt('Python zip file not found at: %s', [LPythonZipFile]);

  //Copy python zip to the target app python's path
  var LAppPythonPath := TPath.Combine(LAppAssetsInternalFolder, ExtractFileName(LPythonZipFile));
  //Copy the python zip to the app assets/internal/
  TFile.Copy(LPythonZipFile, LAppPythonPath);
  //Add the python zip file to the deploy info file
  AddAssetsInternalFileToDeployInfo(AModel.ProjectName, ExtractFileName(LPythonZipFile));

  {|||||| Python Interpreter ||||||}

  //Get the python interpreter shared lib and executable paths
  var LPythonInterpreterFiles := TBuilderPaths.GetPythonInterpreterFiles(AModel.PythonVersion, AModel.Architecture);

  //Copy the python interpreter to the app lib
  var LAppPythonInterpreterFolder := TPath.Combine(LAppPath, TBuilderPaths.GetAppPythonInterpreterFolder(AModel.Architecture));

  //Create the /library/lib/ folder
  if not TDirectory.Exists(LAppPythonInterpreterFolder) then
    TDirectory.CreateDirectory(LAppPythonInterpreterFolder);

  for var LPythonInterpreterFile in LPythonInterpreterFiles do begin
    var LAppPythonInterpreterPath := TPath.Combine(LAppPythonInterpreterFolder, ExtractFileName(LPythonInterpreterFile));
    //Copy the python interpreter or executable to the app library/lib/{arch}
    TFile.Copy(LPythonInterpreterFile, LAppPythonInterpreterPath);
  end;
end;

procedure TAppService.CopyIcons(const AModel: TProjectModel);
begin
  var LAppResPath := TPath.Combine(TBuilderPaths.GetAppPath(AModel.ProjectName), 'res');

  if TFile.Exists(AModel.Icons.DrawableSmall) then
    TFile.Copy(AModel.Icons.DrawableSmall,
      TPath.Combine(LAppResPath, 'drawable-small' + TPath.DirectorySeparatorChar + 'splash_image.png'), true);

  if TFile.Exists(AModel.Icons.DrawableNormal) then
    TFile.Copy(AModel.Icons.DrawableNormal,
      TPath.Combine(LAppResPath, 'drawable-normal' + TPath.DirectorySeparatorChar +'splash_image.png'), true);

  if TFile.Exists(AModel.Icons.DrawableLarge) then
    TFile.Copy(AModel.Icons.DrawableLarge,
      TPath.Combine(LAppResPath, 'drawable-large' + TPath.DirectorySeparatorChar +'splash_image.png'), true);

  if TFile.Exists(AModel.Icons.DrawableXlarge) then
    TFile.Copy(AModel.Icons.DrawableXlarge,
      TPath.Combine(LAppResPath, 'drawable-xlarge' + TPath.DirectorySeparatorChar +'splash_image.png'), true);

  if TFile.Exists(AModel.Icons.DrawableLdpi) then
    TFile.Copy(AModel.Icons.DrawableLdpi,
      TPath.Combine(LAppResPath, 'drawable-ldpi' + TPath.DirectorySeparatorChar +'ic_launcher.png'), true);

  if TFile.Exists(AModel.Icons.DrawableMdpi) then
    TFile.Copy(AModel.Icons.DrawableMdpi,
      TPath.Combine(LAppResPath, 'drawable-mdpi' + TPath.DirectorySeparatorChar +'ic_launcher.png'), true);

  if TFile.Exists(AModel.Icons.DrawableHdpi) then
    TFile.Copy(AModel.Icons.DrawableHdpi,
      TPath.Combine(LAppResPath, 'drawable-hdpi' + TPath.DirectorySeparatorChar +'ic_launcher.png'), true);

  if TFile.Exists(AModel.Icons.DrawableXhdpi) then
    TFile.Copy(AModel.Icons.DrawableXhdpi,
      TPath.Combine(LAppResPath, 'drawable-xhdpi' + TPath.DirectorySeparatorChar +'ic_launcher.png'), true);

  if TFile.Exists(AModel.Icons.DrawableXxhdpi) then
    TFile.Copy(AModel.Icons.DrawableXxhdpi,
      TPath.Combine(LAppResPath, 'drawable-xxhdpi' + TPath.DirectorySeparatorChar +'ic_launcher.png'), true);

  if TFile.Exists(AModel.Icons.DrawableXxxHdpi) then
    TFile.Copy(AModel.Icons.DrawableXxxHdpi,
      TPath.Combine(LAppResPath, 'drawable-xxxhdpi' + TPath.DirectorySeparatorChar +'ic_launcher.png'), true);
end;

procedure TAppService.CopyModules(const AModel: TProjectModel);
begin
  for var LScript in AModel.Files.Files do begin
    var LStream := TFileStream.Create(LScript, fmOpenRead);
    try
      AddFile(AModel, TPath.GetFileName(LScript), LStream);
    finally
      LStream.Free();
    end;
  end;
end;

procedure TAppService.CopyOtherFiles(const AModel: TProjectModel);
begin
  for var LScript in AModel.Files.Others do begin
    var LStream := TFileStream.Create(LScript, fmOpenRead);
    try
      AddFile(AModel, TPath.GetFileName(LScript), LStream);
    finally
      LStream.Free();
    end;
  end;
end;

procedure TAppService.CopyPackages(const AModel: TProjectModel);
begin
  for var LScript in AModel.Files.Packages do begin
    var LStream := TFileStream.Create(LScript, fmOpenRead);
    try
      AddFile(AModel, TPath.GetFileName(LScript), LStream);
    finally
      LStream.Free();
    end;
  end;
end;

procedure TAppService.CopyDebugger(const AModel: TProjectModel);
begin
  var LDebuggerPackage := String.Empty;
  var LDebuggerScript := String.Empty;
  case AModel.Debugger of
    TDebugger.DebugPy: begin
      LDebuggerPackage := TBuilderPaths.GetDebugpyPackagePath();
      LDebuggerScript := TBuilderPaths.GetDebugpyScriptPath();
    end;
    TDebugger.Rpyc: begin
      LDebuggerPackage := TBuilderPaths.GetRpycPackagePath();
      LDebuggerScript := TBuilderPaths.GetRpycScriptPath();
    end;
  end;

  if TFile.Exists(LDebuggerPackage) then begin
    var LStream := TFileStream.Create(LDebuggerPackage, fmOpenRead);
    try
      AddFile(AModel, TPath.GetFileName(LDebuggerPackage), LStream);
    finally
      LStream.Free();
    end;
  end;

  if TFile.Exists(LDebuggerScript) then begin
    //The Android app will look for a file called debug.py
    var LDebugPath := TPath.Combine(TPath.GetTempPath(), 'debug.py');
    if TFile.Exists(LDebugPath) then
      TFile.Delete(LDebugPath);
    TFile.Copy(LDebuggerScript, LDebugPath);
    var LStream := TFileStream.Create(LDebugPath, fmOpenRead);
    try
      AddFile(AModel, TPath.GetFileName(LDebugPath), LStream);
    finally
      LStream.Free();
    end;
  end;
end;

procedure TAppService.TryCopyDebugger(const AModel: TProjectModel);
begin
  if (AModel.BuildConfiguration = TBuildConfiguration.debug) then
    CopyDebugger(AModel);
end;

procedure TAppService.CopyDependencies(const AModel: TProjectModel);
begin
  for var LScript in AModel.Files.Dependencies do begin
    var LStream := TFileStream.Create(LScript, fmOpenRead);
    try
      AddFile(AModel, TPath.GetFileName(LScript), LStream);
    finally
      LStream.Free();
    end;
  end;
end;

procedure TAppService.CreateAppDefs(const AModel: TProjectModel);
const
  APP_DEFS_FILE_NAME = 'app_defs.json';
begin
  var LScriptFolder := TBuilderPaths.GetAppAssetsInternalFolder(AModel.ProjectName);
  var LAppDefsFiles := TPath.Combine(LScriptFolder, APP_DEFS_FILE_NAME);

  if not TFile.Exists(LAppDefsFiles) then
    TFile.Create(LAppDefsFiles).Free();

  var LJSON := TJSONObject.Create();
  try
    LJSON.AddPair('python_version', AModel.PythonVersion.AsString());
    LJSON.AddPair('python_distribution', TPath.GetFileName(
      TBuilderPaths.GetPythonZipFile(AModel.PythonVersion, AModel.Architecture)));
    LJSON.AddPair('main_file', AModel.Files.MainFile);

    var LDependencies := TJSONArray.Create();
    try
      AddAppDefsModules(AModel, LDependencies);
      TryAddAppDefsDebug(AModel, LDependencies);
    finally
      LJSON.AddPair('dependencies', LDependencies);
    end;

    TFile.WriteAllText(LAppDefsFiles, LJSON.ToJSON(), TEncoding.UTF8);
  finally
    LJSON.Free();
  end;

  AddAssetsInternalFileToDeployInfo(AModel.ProjectName, APP_DEFS_FILE_NAME);
end;

procedure TAppService.AddAppDefsModules(const AModel: TProjectModel;
  const AJSONArray: TJSONArray);
begin
  for var LDependency in AModel.Files.Dependencies do begin
    var LJSONDependency := TJSONObject.Create();
    try
      var LFileName := TPath.GetFileName(LDependency);
      LJSONDependency.AddPair('module_name', TPath.GetFileNameWithoutExtension(LFileName));
      LJSONDependency.AddPair('file_name', LFileName);
    finally
      AJSONArray.Add(LJSONDependency);
    end;
  end;
end;

procedure TAppService.TryAddAppDefsDebug(const AModel: TProjectModel;
  const AJSONArray: TJSONArray);
begin
  if AModel.BuildConfiguration = TBuildConfiguration.release then
    Exit;

  var LDebugger := String.Empty;
  case AModel.Debugger of
    TDebugger.DebugPy: LDebugger := 'debugpy.zip';
    TDebugger.Rpyc: LDebugger := 'rpyc.zip';
  end;

  if not LDebugger.IsEmpty() then begin
    var LJSONDependency := TJSONObject.Create();
    try
      LJSONDependency.AddPair('module_name', TPath.GetFileNameWithoutExtension(LDebugger));
      LJSONDependency.AddPair('file_name', LDebugger);
    finally
      AJSONArray.Add(LJSONDependency);
    end;
  end;
end;

end.
