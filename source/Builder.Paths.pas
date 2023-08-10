unit Builder.Paths;

interface

uses
  System.SysUtils,
  Builder.Consts,
  Builder.Types;

type
  TBuilderPaths = record
  public
    //Workspace
    class function WorkspaceFolder(): string; static;
    class function RecommendProjectName(const AWorkspace: string): string; static;
    class function RecommendModuleName(const AProjectName: string;
      APredicate: TPredicate<string> = nil): string; static;
    class function UntitledProject(const AWorkspace: string): string; static;
    //Python embeddable
    class function GetPythonFolder(): string; static;
    class function GetDistributionFolder(): string; static;
    class function GetPythonZipFile(const APythonVersion: TPythonVersion;
      const AArchitecture: TArchitecture): string; static;
    class function GetPythonInterpreterFiles(const APythonVersion: TPythonVersion;
      const AArchitecture: TArchitecture): TArray<string>; static;
    class function GetPythonDependenciesFolder(): string; static;
    class function GetPythonScriptsFolder(): string; static;
    class function GetPreBuiltFolder(const AArchitecture: TArchitecture): string; static;
    //Build app files
    class function GetAppPath(const AProjectPath: string;
      const ABuildConfiguration: TBuildConfiguration;
      const AArchitecture: TArchitecture): string; static;
    class function GetApkPath(const AProjectPath, AProjectName: string;
      const ABuildConfiguration: TBuildConfiguration;
      const AArchitecture: TArchitecture): string; static;
    class function GetManifestPath(const AProjectPath: string;
      const ABuildConfiguration: TBuildConfiguration;
      const AArchitecture: TArchitecture): string; static;
    class function GetAppAssetsInternalFolder(const AProjectPath: string;
      const ABuildConfiguration: TBuildConfiguration;
      const AArchitecture: TArchitecture): string; static;
    class function GetAppAssetsDeployInfoFolder(const AProjectPath: string;
      const ABuildConfiguration: TBuildConfiguration;
      const AArchitecture: TArchitecture): string; static;
    class function GetAppResPath(const AProjectPath: string;
      const ABuildConfiguration: TBuildConfiguration;
      const AArchitecture: TArchitecture): string; static;
    class function GetAppPythonInterpreterFolder(const AProjectPath: string;
      const AArchitecture: TArchitecture): string; static;
    //Debugpy
    class function GetDebugpyPackagePath(): string; static;
    class function GetDebugpyScriptPath(): string; static;
    //Rpyc
    class function GetRpycPackagePath(): string; static;
    class function GetRpycScriptPath(): string; static;
    //RemServer
    class function GetRemServerScriptPath(): string; static;
  end;

  TBuilderUnboundPaths = record
  public
    class function GetPythonBasePath(): string; static;
    class function GetPythonHomePath(const APythonVersion: TPythonVersion;
      const AArchitecture: TArchitecture): string; static;
    //Debugpy
    class function GetDebugpyPackagePath(): string; static;
    class function GetDebugpyScriptPath(): string; static;
    //Rpyc
    class function GetRpycPackagePath(): string; static;
    class function GetRpycScriptPath(): string; static;
    //RemServer
    class function GetRemServerScriptPath(): string; static;
  end;

const
  APP_IMAGE_NAME = 'PyApp';

implementation

uses
  System.IOUtils,
  Builder.Exception;

{ TBuilderPaths }

class function TBuilderPaths.GetApkPath(const AProjectPath, AProjectName: string;
  const ABuildConfiguration: TBuildConfiguration;
  const AArchitecture: TArchitecture): string;
begin
  Result :=
    TPath.Combine(
      TPath.Combine(
        GetAppPath(AProjectPath, ABuildConfiguration, AArchitecture),
        'bin'),
      AProjectName + '.apk');
end;

class function TBuilderPaths.GetAppAssetsInternalFolder(const AProjectPath: string;
  const ABuildConfiguration: TBuildConfiguration;
  const AArchitecture: TArchitecture): string;
begin
  Result :=
    TPath.Combine(
      TPath.Combine(
        TBuilderPaths.GetAppPath(AProjectPath, ABuildConfiguration, AArchitecture),
        'assets'),
      'internal');
end;

class function TBuilderPaths.GetAppAssetsDeployInfoFolder(const AProjectPath: string;
  const ABuildConfiguration: TBuildConfiguration;
  const AArchitecture: TArchitecture): string;
begin
  Result :=
    TPath.Combine(
      TPath.Combine(
        TBuilderPaths.GetAppPath(AProjectPath, ABuildConfiguration, AArchitecture),
        'assets'),
      'deployinfo');
end;

class function TBuilderPaths.GetAppPath(const AProjectPath: string;
  const ABuildConfiguration: TBuildConfiguration;
  const AArchitecture: TArchitecture): string;
begin
  Result :=
    TPath.Combine(
      TPath.Combine(
        TPath.GetDirectoryName(AProjectPath),
        ABuildConfiguration.AsString()),
    AArchitecture.AsString());
end;

class function TBuilderPaths.GetAppPythonInterpreterFolder(
  const AProjectPath: string; const AArchitecture: TArchitecture): string;
begin
  Result := TPath.Combine(AProjectPath, TPath.Combine('library', 'lib'));
  case AArchitecture of
    TArchitecture.arm: Result := TPath.Combine(Result, 'armeabi-v7a');
    TArchitecture.aarch64: Result := TPath.Combine(Result, 'arm64-v8a');
  end;
end;

class function TBuilderPaths.GetAppResPath(const AProjectPath: string;
  const ABuildConfiguration: TBuildConfiguration;
  const AArchitecture: TArchitecture): string;
begin
  Result := TPath.Combine(
    GetAppPath(AProjectPath, ABuildConfiguration, AArchitecture),
    'res');
end;

class function TBuilderPaths.GetManifestPath(const AProjectPath: string;
  const ABuildConfiguration: TBuildConfiguration;
  const AArchitecture: TArchitecture): string;
begin
  Result := TPath.Combine(
    TBuilderPaths.GetAppPath(AProjectPath, ABuildConfiguration, AArchitecture),
    'AndroidManifest.xml');
end;

class function TBuilderPaths.GetPreBuiltFolder(
  const AArchitecture: TArchitecture): string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), 'android');
  Result := TPath.Combine(Result, 'pre-built');
  case AArchitecture of
    TArchitecture.arm: Result := TPath.Combine(Result, 'arm');
    TArchitecture.aarch64: Result := TPath.Combine(Result, 'aarch64');
  end;
  Result := TPath.Combine(Result, APP_IMAGE_NAME);
end;

class function TBuilderPaths.GetPythonFolder: string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), 'python');
end;

class function TBuilderPaths.GetPythonInterpreterFiles(
  const APythonVersion: TPythonVersion;
  const AArchitecture: TArchitecture): TArray<string>;
begin
  var LPath := TBuilderPaths.GetDistributionFolder();

  // Discover Python distrib version number
  var LVersionNumber := String.Empty;
  case APythonVersion of
    TPythonVersion.cp38 : LVersionNumber := '3.8';
    TPythonVersion.cp39 : LVersionNumber := '3.9';
    TPythonVersion.cp310: LVersionNumber := '3.10';
    TPythonVersion.cp311: LVersionNumber := '3.11';
  end;

  // Look for the Python version directory
  LPath := TPath.Combine(LPath, LVersionNumber);

  // Look for the arch
  case AArchitecture of
    TArchitecture.arm: LPath := TPath.Combine(LPath, 'arm');
    TArchitecture.aarch64: LPath := TPath.Combine(LPath, 'arm64');
  end;

  // Look for the Python interpreter
  var LFileName := TPath.Combine(LPath, 'libpython' + LVersionNumber + '.so');
  if not TFile.Exists(LFileName) then
    raise EPythonExecutableNotFound.CreateFmt('Python interpreter not found at %s', [LPath]);
  Result := Result + [LFileName];

  // Look for the Python executable
  LFileName := TPath.Combine(LPath, 'libpythonlauncher' + LVersionNumber + '.so');
  if not TFile.Exists(LFileName) then
    raise EPythonInterpreterNotFound.CreateFmt('Python executable not found at %s', [LPath]);
  Result := Result + [LFileName];
end;

class function TBuilderPaths.GetPythonDependenciesFolder: string;
begin
  Result := TPath.Combine(GetPythonFolder(), 'dependencies');
end;

class function TBuilderPaths.GetPythonScriptsFolder: string;
begin
  Result := TPath.Combine(GetPythonFolder(), 'scripts');
end;

class function TBuilderPaths.GetPythonZipFile(
  const APythonVersion: TPythonVersion;
  const AArchitecture: TArchitecture): string;
begin
  Result := TBuilderPaths.GetDistributionFolder();

  var LPythonVer := String.Empty;
  case APythonVersion of
    TPythonVersion.cp38 : LPythonVer := '3.8';
    TPythonVersion.cp39 : LPythonVer := '3.9';
    TPythonVersion.cp310: LPythonVer := '3.10';
    TPythonVersion.cp311: LPythonVer := '3.11';
  end;

  var LArch := String.Empty;
  case AArchitecture of
    TArchitecture.arm: LArch := 'arm';
    TArchitecture.aarch64: LArch := 'arm64';
  end;

  var LSearchPattern := Format('python3-android-%s*-%s.zip', [LPythonVer, LArch]);
  var LFiles := TDirectory.GetFiles(Result, LSearchPattern, TSearchOption.soTopDirectoryOnly);
  if (Length(LFiles) = 0) then
    raise EPythonDistributionNotFound.CreateFmt('Python distribution not found at %s', [Result]);

  Result := TPath.Combine(Result, LFiles[Low(LFiles)]);
end;

class function TBuilderPaths.GetDebugpyPackagePath: string;
begin
  Result := TPath.Combine(TBuilderPaths.GetPythonDependenciesFolder(), 'debugpy.zip');
end;

class function TBuilderPaths.GetDebugpyScriptPath: string;
begin
  Result := TPath.Combine(TBuilderPaths.GetPythonScriptsFolder(), 'debugpy.py');
end;

class function TBuilderPaths.GetDistributionFolder: string;
begin
  Result := TPath.Combine(TBuilderPaths.GetPythonFolder(), 'distributions');
end;

class function TBuilderPaths.GetRpycPackagePath: string;
begin
  Result := TPath.Combine(TBuilderPaths.GetPythonDependenciesFolder(), 'rpyc.zip');
end;

class function TBuilderPaths.GetRpycScriptPath: string;
begin
  Result := TPath.Combine(TBuilderPaths.GetPythonScriptsFolder(), 'rpyc.py');
end;

class function TBuilderPaths.UntitledProject(
  const AWorkspace: string): string;
begin
  var I := 1;
  repeat
    Result := TPath.Combine(
      TBuilderPaths.WorkspaceFolder(),
      'Project' + I.ToString() + PYTHON_PROJECT_FILE_EXTENSION);
    Inc(I);
  until not TFile.Exists(Result);
end;

class function TBuilderPaths.RecommendModuleName(
  const AProjectName: string; APredicate: TPredicate<string>): string;
begin
  if not Assigned(APredicate) then
    APredicate := function(Arg: string): boolean begin
      Result := true;
    end;

  var I := 1;
  repeat
    Result := TPath.Combine(
      TPath.GetDirectoryName(AProjectName),
      'module' + I.ToString() + PYTHON_MODULE_FILE_EXTENSION);
    Inc(I);
  until not TFile.Exists(Result) and APredicate(Result);
end;

class function TBuilderPaths.RecommendProjectName(
  const AWorkspace: string): string;
begin
  var LProjectName := String.Empty;
  var I := 1;
  repeat
    LProjectName := 'Project' + I.ToString();
    Result := TPath.Combine(TBuilderPaths.WorkspaceFolder(), LProjectName);
    Inc(I);
  until not TDirectory.Exists(Result);
  Result := TPath.Combine(Result, LProjectName + PYTHON_PROJECT_FILE_EXTENSION);
end;

class function TBuilderPaths.WorkspaceFolder: string;
begin
  Result := TPath.Combine(
    TPath.Combine(TPath.GetDocumentsPath(), 'PythonFMXBuilder'),
    'Projects');
end;

class function TBuilderPaths.GetRemServerScriptPath: string;
begin
  Result := TPath.Combine(TBuilderPaths.GetPythonScriptsFolder(), 'remserver.py');
end;

{ TBuilderUnboundPaths }

class function TBuilderUnboundPaths.GetPythonBasePath: string;
begin
  Result := '/data/local/tmp';
end;

class function TBuilderUnboundPaths.GetPythonHomePath(
  const APythonVersion: TPythonVersion;
  const AArchitecture: TArchitecture): string;
begin
  Result := TBuilderUnboundPaths.GetPythonBasePath();
  case AArchitecture of
    TArchitecture.arm    : Result := Result + '/arm';
    TArchitecture.aarch64: Result := Result + '/aarch64';
  end;

  case APythonVersion of
    TPythonVersion.cp38 : Result := Result + '/python3.8';
    TPythonVersion.cp39 : Result := Result + '/python3.9';
    TPythonVersion.cp310: Result := Result + '/python3.10';
    TPythonVersion.cp311: Result := Result + '/python3.11';
  end;
end;

class function TBuilderUnboundPaths.GetDebugpyPackagePath: string;
begin
  Result := TBuilderUnboundPaths.GetPythonBasePath() + '/debugpy.zip';
end;

class function TBuilderUnboundPaths.GetDebugpyScriptPath: string;
begin
  Result := TBuilderUnboundPaths.GetPythonBasePath() + '/debugpy.py';
end;

class function TBuilderUnboundPaths.GetRpycPackagePath: string;
begin
  Result := TBuilderUnboundPaths.GetPythonBasePath() + '/rpyc.zip';
end;

class function TBuilderUnboundPaths.GetRpycScriptPath: string;
begin
  Result := TBuilderUnboundPaths.GetPythonBasePath() + '/rpyc.py';
end;

class function TBuilderUnboundPaths.GetRemServerScriptPath: string;
begin
  Result := TBuilderUnboundPaths.GetPythonBasePath() + '/remserver.py';
end;

end.
