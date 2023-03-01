unit Builder.Paths;

interface

uses
  Builder.Types;

type
  TBuilderPaths = record
  public
    //Python embeddable
    class function GetPythonFolder(): string; static;
    class function GetPythonZipFile(const APythonVersion: TPythonVersion;
      const AArchitecture: TArchitecture): string; static;
    class function GetPythonInterpreterFiles(const APythonVersion: TPythonVersion;
      const AArchitecture: TArchitecture): TArray<string>; static;
    
    class function GetPythonDependenciesFolder(): string; static;
    class function GetPythonScriptsFolder(): string; static;
    class function GetPreBuiltFolder(const AArchitecture: TArchitecture): string; static;
    class function GetAppPath(const AProjectName: string): string; static;
    class function GetApkPath(const AProjectName: string): string; static;
    class function GetManifestPath(const AProjectName: string): string; static;
    class function GetAppAssetsInternalFolder(const AProjectName: string): string; static;
    class function GetAppAssetsDeployInfoFolder(const AProjectName: string): string; static;
    class function GetAppPythonInterpreterFolder(const AArchitecture: TArchitecture): string; static;
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
  APPS_FOLDER = 'apps';
  APP_IMAGE_NAME = 'PyApp';

implementation

uses
  System.SysUtils,
  System.IOUtils;

{ TBuilderPaths }

class function TBuilderPaths.GetApkPath(const AProjectName: string): string;
begin
  Result := TPath.Combine(GetAppPath(AProjectName), 'bin');
  Result := TPath.Combine(Result, ChangeFileExt(AProjectName, '.apk'));
end;

class function TBuilderPaths.GetAppAssetsInternalFolder(
  const AProjectName: string): string;
begin
  Result := TBuilderPaths.GetAppPath(AProjectName);
  Result := TPath.Combine(Result, 'assets');
  Result := TPath.Combine(Result, 'internal');
end;

class function TBuilderPaths.GetAppAssetsDeployInfoFolder(
  const AProjectName: string): string;
begin
  Result := TBuilderPaths.GetAppPath(AProjectName);
  Result := TPath.Combine(Result, 'assets');
  Result := TPath.Combine(Result, 'deployinfo');
end;

class function TBuilderPaths.GetAppPath(const AProjectName: string): string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), APPS_FOLDER);
  Result := TPath.Combine(Result, AProjectName);
end;

class function TBuilderPaths.GetAppPythonInterpreterFolder(
  const AArchitecture: TArchitecture): string;
begin
  Result := TPath.Combine('library', 'lib');
  case AArchitecture of
    TArchitecture.arm: Result := TPath.Combine(Result, 'armeabi-v7a');
    TArchitecture.aarch64: Result := TPath.Combine(Result, 'arm64-v8a');
  end;
end;

class function TBuilderPaths.GetManifestPath(
  const AProjectName: string): string;
begin
  Result := TPath.Combine(TBuilderPaths.GetAppPath(AProjectName), 'AndroidManifest.xml');
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
  var LPath := TBuilderPaths.GetPythonFolder();
  case AArchitecture of
    TArchitecture.arm: LPath := TPath.Combine(LPath, 'arm');
    TArchitecture.aarch64: LPath := TPath.Combine(LPath, 'aarch64');
  end;

  case APythonVersion of
    TPythonVersion.cp38: LPath := TPath.Combine(LPath, 'python3.8');
    TPythonVersion.cp39: LPath := TPath.Combine(LPath, 'python3.9');
    TPythonVersion.cp310: LPath := TPath.Combine(LPath, 'python3.10');
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
        TPythonVersion.cp38: LPythonExecutableName := 'python3.8';
        TPythonVersion.cp39: LPythonExecutableName := 'python3.9';
        TPythonVersion.cp310: LPythonExecutableName := 'python3.10';
      end;
      Result := SearchRec.Name = LPythonExecutableName;
    end);

  if (Length(LFiles) = 0) then
    raise Exception.CreateFmt('Python executable not found at %s', [LPath]);

  Result := Result + [TPath.Combine(LPath, LFiles[Low(LFiles)])];
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
  Result := TBuilderPaths.GetPythonFolder();
  case AArchitecture of
    TArchitecture.arm: Result := TPath.Combine(Result, 'arm');
    TArchitecture.aarch64: Result := TPath.Combine(Result, 'aarch64');
  end;

  case APythonVersion of
    TPythonVersion.cp38: Result := TPath.Combine(Result, 'python3.8');
    TPythonVersion.cp39: Result := TPath.Combine(Result, 'python3.9');
    TPythonVersion.cp310: Result := TPath.Combine(Result, 'python3.10');
  end;

  var LFiles := TDirectory.GetFiles(Result, '*.zip', TSearchOption.soTopDirectoryOnly);
  if (Length(LFiles) = 0) then
    raise Exception.CreateFmt('Python distribution not found at %s', [Result]);

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

class function TBuilderPaths.GetRpycPackagePath: string;
begin
  Result := TPath.Combine(TBuilderPaths.GetPythonDependenciesFolder(), 'rpyc.zip');
end;

class function TBuilderPaths.GetRpycScriptPath: string;
begin
  Result := TPath.Combine(TBuilderPaths.GetPythonScriptsFolder(), 'rpyc.py');
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
