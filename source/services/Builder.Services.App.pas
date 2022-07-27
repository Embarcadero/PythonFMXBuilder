unit Builder.Services.App;

interface

uses
  Builder.Services,
  Builder.Architecture, Builder.PythonVersion,
  Builder.Model.Project, Builder.Model.Environment,
  System.Classes, System.IOUtils;

type
  TAppService = class(TInterfacedObject, IAppServices)
  private
    function GetPreBuiltFolder(const AArchitecture: TArchitecture): string;
    function GetPythonZipFile(const APythonVersion: TPythonVersion;
      const AArchitecture: TArchitecture): string;
    function GetPythonInterpreterFiles(const APythonVersion: TPythonVersion;
      const AArchitecture: TArchitecture): TArray<string>;
    function GetDependencyFiles(): TArray<string>;
    function GetAppPythonInterpreterFolder(const AArchitecture: TArchitecture): string;
    function GetApkPath(const AProjectName: string): string;
    function GetManifestPath(const AProjectName: string): string;
    function GetPythonFolder(): string;
    function GetAppPath(const AProjectName: string): string;
    function GetAppAssetsInternal(const AProjectName: string; const AValidate: boolean = true): string;
    function GetAppAssestsInternalFolder(const AProjectName: string): string;
    function GetAppDeployInfoFolder(const AProjectName: string): string;
    procedure ClearAssetsInternal(const AProjectName: string);
    procedure ClearDeployInfo(const AProjectName: string);
    procedure AddAssetsInternalFileToDeployInfo(const AProjectName: string; const AFileName: string);
    procedure RemoveAssetsInternalFileToDeployInfo(const AProjectName: string; const AFileName: string);
  public
    procedure CopyAppFiles(const AModel: TProjectModel);
    procedure CopyIcons(const AModel: TProjectModel);
    //App defs. file (used by the Android app)
    procedure CreateAppDefs(const AModel: TProjectModel);
    //App script files
    procedure CopyScriptFiles(const AModel: TProjectModel);
    function AddScriptFile(const AModel: TProjectModel; const AFileName: string;
      const AStream: TStream): string;
    procedure RemoveScriptFile(const AModel: TProjectModel; const AFilePath: string);
    function GetScriptFiles(const AModel: TProjectModel;
      const AFilter: TDirectory.TFilterPredicate = nil): TArray<string>;
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
  end;

implementation

uses
  System.SysUtils, System.JSON,
  Builder.Services.Factory;

const
  APPS_FOLDER = 'apps';
  APP_IMAGE_NAME = 'PyApp';
  APP_IMAGE_APK_NAME = 'PyApp.apk';

{ TPreBuiltCopyService }

function TAppService.GetApkPath(const AProjectName: string): string;
begin
  Result := TPath.Combine(GetAppPath(AProjectName), 'bin');
  Result := TPath.Combine(Result, ChangeFileExt(AProjectName, '.apk'));
end;

function TAppService.GetAppAssetsInternal(const AProjectName: string;
  const AValidate: boolean): string;
begin
  Result := GetAppPath(AProjectName);
  Result := TPath.Combine(Result, 'assets');
  Result := TPath.Combine(Result, 'internal');

  if AValidate and not TDirectory.Exists(Result) then
    raise Exception.CreateFmt('Script folder not found at: %s', [Result]);
end;

function TAppService.GetAppDeployInfoFolder(const AProjectName: string): string;
begin
  Result := GetAppPath(AProjectName);
  Result := TPath.Combine(Result, 'assets');
  Result := TPath.Combine(Result, 'deployinfo');
end;

function TAppService.GetAppPath(const AProjectName: string): string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), APPS_FOLDER);
  Result := TPath.Combine(Result, AProjectName);
end;

function TAppService.GetAppAssestsInternalFolder(const AProjectName: string): string;
begin
  Result := GetAppPath(AProjectName);
  Result := TPath.Combine(Result, 'assets');
  Result := TPath.Combine(Result, 'internal');
end;

function TAppService.GetAppPythonInterpreterFolder(const AArchitecture: TArchitecture): string;
begin
  Result := TPath.Combine('library', 'lib');
  case AArchitecture of
    arm: Result := TPath.Combine(Result, 'armeabi-v7a');
    aarch64: Result := TPath.Combine(Result, 'arm64-v8a');
  end;
end;

function TAppService.GetDependencyFiles: TArray<string>;
begin
  var LPath := GetPythonFolder();
  LPath := TPath.Combine(LPath, 'dependencies');
  Result := TDirectory.GetFiles(LPath, '*.zip');
end;

function TAppService.GetManifestPath(const AProjectName: string): string;
begin
  Result := TPath.Combine(GetAppPath(AProjectName), 'AndroidManifest.xml');
end;

function TAppService.GetPreBuiltFolder(
  const AArchitecture: TArchitecture): string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), 'android');
  Result := TPath.Combine(Result, 'pre-built');
  case AArchitecture of
    arm: Result := TPath.Combine(Result, 'arm');
    aarch64: Result := TPath.Combine(Result, 'aarch64');
  end;
  Result := TPath.Combine(Result, APP_IMAGE_NAME);
end;

function TAppService.GetPythonFolder: string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), 'python');
end;

function TAppService.GetPythonInterpreterFiles(
  const APythonVersion: TPythonVersion;
  const AArchitecture: TArchitecture): TArray<string>;
begin
  var LPath := GetPythonFolder();
  case AArchitecture of
    arm: LPath := TPath.Combine(LPath, 'arm');
    aarch64: LPath := TPath.Combine(LPath, 'aarch64');
  end;

  case APythonVersion of
    cp38: LPath := TPath.Combine(LPath, 'python3.8');
    cp39: LPath := TPath.Combine(LPath, 'python3.9');
    cp310: LPath := TPath.Combine(LPath, 'python3.10');
  end;

  var LFiles := TDirectory.GetFiles(LPath, '*.so', TSearchOption.soTopDirectoryOnly);
  if (Length(LFiles) = 0) then
    raise Exception.CreateFmt('Python interpreter not found at %s', [LPath]);

  Result := Result + [TPath.Combine(LPath, LFiles[Low(LFiles)])];

  LFiles := TDirectory.GetFiles(LPath, 'python*', TSearchOption.soTopDirectoryOnly,
    function(const Path: string; const SearchRec: TSearchRec): boolean
    begin
      var LPythonExecutableName := String.Empty;
      case APythonVersion of
        cp38: LPythonExecutableName := 'python3.8';
        cp39: LPythonExecutableName := 'python3.9';
        cp310: LPythonExecutableName := 'python3.10';
      end;
      Result := SearchRec.Name = LPythonExecutableName;
    end);

  if (Length(LFiles) = 0) then
    raise Exception.CreateFmt('Python executable not found at %s', [LPath]);

  Result := Result + [TPath.Combine(LPath, LFiles[Low(LFiles)])];
end;

function TAppService.GetPythonZipFile(
  const APythonVersion: TPythonVersion;
  const AArchitecture: TArchitecture): string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), 'python');
  case AArchitecture of
    arm: Result := TPath.Combine(Result, 'arm');
    aarch64: Result := TPath.Combine(Result, 'aarch64');
  end;

  case APythonVersion of
    cp38: Result := TPath.Combine(Result, 'python3.8');
    cp39: Result := TPath.Combine(Result, 'python3.9');
    cp310: Result := TPath.Combine(Result, 'python3.10');
  end;

  var LFiles := TDirectory.GetFiles(Result, '*.zip', TSearchOption.soTopDirectoryOnly);
  if (Length(LFiles) = 0) then
    raise Exception.CreateFmt('Python distribution not found at %s', [Result]);

  Result := TPath.Combine(Result, LFiles[Low(LFiles)]);
end;

function TAppService.InstallApk(const AProjectModel: TProjectModel;
  const AEnvironmentModel: TEnvironmentModel; const ADevice: string): boolean;
begin
  var LApkPath := GetApkPath(AProjectModel.ProjectName);
  if not TFile.Exists(LApkPath) then
    raise Exception.CreateFmt('Apk file %s not found at: %s', [
      AProjectModel.ProjectName, LApkPath]);

  var LService := TServiceSimpleFactory.CreateAdb();
  var LStrings := TStringList.Create();
  try
    Result := LService.InstallApk(AEnvironmentModel.AdbLocation, LApkPath,
      ADevice, LStrings);
  finally
    LStrings.Free();
  end;
end;

function TAppService.IsAppInstalled(const AProjectModel: TProjectModel;
  const AEnvironmentModel: TEnvironmentModel; const ADevice: string): boolean;
begin
  var LService := TServiceSimpleFactory.CreateAdb();
  var LStrings := TStringList.Create();
  try
    Result := LService.IsAppInstalled(AEnvironmentModel.AdbLocation,
      AProjectModel.PackageName, ADevice, LStrings);
  finally
    LStrings.Free();
  end;
end;

function TAppService.UnInstallApk(const AProjectModel: TProjectModel;
  const AEnvironmentModel: TEnvironmentModel; const ADevice: string): boolean;
begin
  var LService := TServiceSimpleFactory.CreateAdb();
  var LStrings := TStringList.Create();
  try
    Result := LService.UnInstallApk(AEnvironmentModel.AdbLocation,
      AProjectModel.PackageName, ADevice, LStrings);
  finally
    LStrings.Free();
  end;
end;

procedure TAppService.UpdateManifest(const AModel: TProjectModel);
begin
  var LManifestPath := GetManifestPath(AModel.ProjectName);
  var LText := TFile.ReadAllText(LManifestPath, TEncoding.UTF8);
  LText := LText
    .Replace('package="com.embarcadero.PyApp"', Format('package="%s"', [AModel.PackageName]))
    .Replace('android:versionCode="1"', Format('android:versionCode="%s"', [AModel.VersionCode.ToString()]))
    .Replace('android:versionName="1.0.0"', Format('android:versionName="%s"', [AModel.VersionName]))
    .Replace('android:label="PyApp"', Format('android:label="%s"', [AModel.ApplicationName]));

  TFile.WriteAllText(LManifestPath, LText);
end;

{ TPreBuiltCopyService }

procedure TAppService.AddAssetsInternalFileToDeployInfo(
  const AProjectName: string; const AFileName: string);
begin
  var LDeployInfoFolder := GetAppDeployInfoFolder(AProjectName);
  if not TDirectory.Exists(LDeployInfoFolder) then
    TDirectory.CreateDirectory(LDeployInfoFolder);

  var LDelpoyedDataSetsFile := TPath.Combine(LDeployInfoFolder, 'deployedassets.txt');
  if not TFile.Exists(LDelpoyedDataSetsFile) then
    TFile.Create(LDelpoyedDataSetsFile).Free();

  var LDeployedFilePath := '.\assets\internal\' + AFileName;

  var LContent := TFile.ReadAllText(LDelpoyedDataSetsFile);

  if not LContent.IsEmpty() then
    TFile.AppendAllText(LDelpoyedDataSetsFile, sLineBreak);

  if not LContent.Contains(LDeployedFilePath) then
    TFile.AppendAllText(LDelpoyedDataSetsFile, LDeployedFilePath);
end;


procedure TAppService.RemoveAssetsInternalFileToDeployInfo(const AProjectName,
  AFileName: string);
begin
  var LDeployInfoFolder := GetAppDeployInfoFolder(AProjectName);
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

function TAppService.AddScriptFile(const AModel: TProjectModel;
  const AFileName: string; const AStream: TStream): string;
var
  LBytes: TBytes;
begin
  var LScriptFolder := GetAppAssetsInternal(AModel.ProjectName);

  SetLength(LBytes, AStream.Size);
  AStream.Position := 0;
  AStream.Read(LBytes, AStream.Size);
  var LFilePath := TPath.Combine(LScriptFolder, AFileName);
  TFile.WriteAllBytes(LFilePath, LBytes);

  AddAssetsInternalFileToDeployInfo(AModel.ProjectName, AFileName);

  Result := LFilePath
end;

procedure TAppService.RemoveScriptFile(const AModel: TProjectModel;
  const AFilePath: string);
begin
  var LScriptFolder := GetAppAssetsInternal(AModel.ProjectName);
  //Remove from the dataset file
  RemoveAssetsInternalFileToDeployInfo(AModel.ProjectName, TPath.GetFileName(AFilePath));
  //Physically delete file
  TFile.Delete(AFilePath);
end;

function TAppService.GetScriptFiles(
  const AModel: TProjectModel; const AFilter: TDirectory.TFilterPredicate): TArray<string>;
begin
  var LScriptFolder := GetAppAssetsInternal(AModel.ProjectName);
  Result := TDirectory.GetFiles(LScriptFolder, '*', TSearchOption.soTopDirectoryOnly, AFilter);
end;

function TAppService.BuildApk(const AProjectModel: TProjectModel;
  const AEnvironmentModel: TEnvironmentModel): boolean;
begin
  var LService := TServiceSimpleFactory.CreateAdb();
  var LStrings := TStringList.Create();
  try
    Result := LService.BuildApk(GetAppPath(AProjectModel.ProjectName),
      AProjectModel.ProjectName, AEnvironmentModel, LStrings);
  finally
    LStrings.Free();
  end;
end;

procedure TAppService.BuildProject(const AModel: TProjectModel);
begin
  //Copy Python and other APP files
  CopyAppFiles(AModel);
  //Copy icons
  CopyIcons(AModel);
  //Save aditional scripts to the APP files
  CopyScriptFiles(AModel);
  //Update the manifest with the custom APP settings
  UpdateManifest(AModel);
  //Creates the app_defs file - it defines which file is the main script and more
  CreateAppDefs(AModel);
end;

procedure TAppService.ClearAssetsInternal(const AProjectName: string);
begin
  var LAssetsInternalFolder := GetAppAssestsInternalFolder(AProjectName);
  if TDirectory.Exists(LAssetsInternalFolder) then
    TDirectory.Delete(LAssetsInternalFolder, true);
end;

procedure TAppService.ClearDeployInfo(const AProjectName: string);
begin
  var LDeployInfoFolder := GetAppDeployInfoFolder(AProjectName);
  var LDelpoyedDataSetsFile := TPath.Combine(LDeployInfoFolder, 'deployedassets.txt');
  if TFile.Exists(LDelpoyedDataSetsFile) then
    TFile.Delete(LDelpoyedDataSetsFile);
end;

procedure TAppService.CopyAppFiles(const AModel: TProjectModel);
begin
  {|||||| APP folder ||||||}
  var LAppPath := GetAppPath(AModel.ProjectName);

  if TDirectory.Exists(LAppPath) then
    TDirectory.Delete(LAppPath, true);

  TDirectory.CreateDirectory(LAppPath);

  {|||||| Pre-build APP folder ||||||}

  var LPreBuiltFolder := GetPreBuiltFolder(AModel.Architecture);
  if not TDirectory.Exists(LPreBuiltFolder) then
    raise Exception.CreateFmt('Pre-built folder not found at: %s', [LPreBuiltFolder]);

  //Copy the app image to the target app path
  TDirectory.Copy(LPreBuiltFolder, LAppPath);

  ClearAssetsInternal(AModel.ProjectName);
  ClearDeployInfo(AModel.ProjectName);

  var LAppAssetsInternalFolder := GetAppAssestsInternalFolder(AModel.ProjectName);
  //Create the /assets/internal/ folder
  if not TDirectory.Exists(LAppAssetsInternalFolder) then
    TDirectory.CreateDirectory(LAppAssetsInternalFolder);

  {|||||| Python distribution zip file ||||||}

  var LPythonZipFile := GetPythonZipFile(AModel.PythonVersion, AModel.Architecture);
  if not TFile.Exists(LPythonZipFile) then
    raise Exception.CreateFmt('Python zip file not found at: %s', [LPythonZipFile]);

  //Copy python zip to the target app python's path
  var LAppPythonPath := TPath.Combine(LAppAssetsInternalFolder, ExtractFileName(LPythonZipFile));
  //Copy the python zip to the app assets/internal/
  TFile.Copy(LPythonZipFile, LAppPythonPath);
  //Add the python zip file to the deploy info file
  AddAssetsInternalFileToDeployInfo(AModel.ProjectName, ExtractFileName(LPythonZipFile));

  {|||||| Python dependencies ||||||}

  for var LDependency in GetDependencyFiles() do begin
    LAppPythonPath := TPath.Combine(LAppAssetsInternalFolder, ExtractFileName(LDependency));
    //Copy the python zip to the app assets/internal/
    TFile.Copy(LDependency, LAppPythonPath);
    //Add the python zip file to the deploy info file
    AddAssetsInternalFileToDeployInfo(AModel.ProjectName, ExtractFileName(LDependency));
  end;

  {|||||| Python Interpreter ||||||}

  //Get the python interpreter shared lib and executable paths
  var LPythonInterpreterFiles := GetPythonInterpreterFiles(AModel.PythonVersion, AModel.Architecture);

  //Copy the python interpreter to the app lib
  var LAppPythonInterpreterFolder := TPath.Combine(LAppPath, GetAppPythonInterpreterFolder(AModel.Architecture));

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
  var LAppResPath := TPath.Combine(GetAppPath(AModel.ProjectName), 'res');

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

procedure TAppService.CopyScriptFiles(const AModel: TProjectModel);
begin
  for var LScript in AModel.Files.Files do begin
    var LStream := TFileStream.Create(LScript, fmOpenRead);
    try
      AddScriptFile(AModel, TPath.GetFileName(LScript), LStream);
    finally
      LStream.Free();
    end;
  end;
end;

procedure TAppService.CreateAppDefs(const AModel: TProjectModel);
const
  APP_DEFS_FILE_NAME = 'app_defs.json';
begin
  var LScriptFolder := GetAppAssetsInternal(AModel.ProjectName);
  var LAppDefsFiles := TPath.Combine(LScriptFolder, APP_DEFS_FILE_NAME);

  if not TFile.Exists(LAppDefsFiles) then
    TFile.Create(LAppDefsFiles).Free();

  var LJSON := TJSONObject.Create();
  try
    LJSON.AddPair('main_file', AModel.Files.MainFile);

    var LDependencies := TJSONArray.Create();
    try
      for var LDependency in GetDependencyFiles() do begin
        var LJSONDependency := TJSONObject.Create();
        try
          var LFileName := TPath.GetFileName(LDependency);
          //We are only accepting packages following PIP formated names
          var LIdx := Pos('-', LFileName) - 1;
          if LIdx <= 0 then
            raise Exception.Create('Invalid dependency package name');

          LJSONDependency.AddPair('module_name', LFileName.Substring(0, LIdx));
          LJSONDependency.AddPair('file_name', LFileName);
        finally
          LDependencies.Add(LJSONDependency);
        end;
      end;
    finally
      LJSON.AddPair('dependencies', LDependencies);
    end;

    TFile.WriteAllText(LAppDefsFiles, LJSON.ToJSON(), TEncoding.UTF8);
  finally
    LJSON.Free();
  end;

  AddAssetsInternalFileToDeployInfo(AModel.ProjectName, APP_DEFS_FILE_NAME);
end;

end.
