unit Builder.Paths;

interface

uses
  Builder.Types;

type
  TBuilderPaths = record
  public
    class function GetPythonFolder(): string; static;
    class function GetPythonDependenciesFolder(): string; static;
    class function GetPythonScriptsFolder(): string; static;
    class function GetPreBuiltFolder(const AArchitecture: TArchitecture): string; static;
    class function GetAppPath(const AProjectName: string): string; static;
    class function GetApkPath(const AProjectName: string): string; static;
    class function GetManifestPath(const AProjectName: string): string; static;
    class function GetAppAssetsInternalFolder(const AProjectName: string): string; static;
    class function GetAppAssetsDeployInfoFolder(const AProjectName: string): string; static;
    class function GetAppPythonInterpreterFolder(const AArchitecture: TArchitecture): string; static;
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
    arm: Result := TPath.Combine(Result, 'armeabi-v7a');
    aarch64: Result := TPath.Combine(Result, 'arm64-v8a');
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
    arm: Result := TPath.Combine(Result, 'arm');
    aarch64: Result := TPath.Combine(Result, 'aarch64');
  end;
  Result := TPath.Combine(Result, APP_IMAGE_NAME);
end;

class function TBuilderPaths.GetPythonFolder: string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), 'python');
end;

class function TBuilderPaths.GetPythonDependenciesFolder: string;
begin
  Result := TPath.Combine(GetPythonFolder(), 'dependencies');
end;

class function TBuilderPaths.GetPythonScriptsFolder: string;
begin
  Result := TPath.Combine(GetPythonFolder(), 'scripts');
end;

end.
