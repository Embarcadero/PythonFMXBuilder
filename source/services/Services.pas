unit Services;

interface

uses
  System.Classes, Architecture, PythonVersion, Model.Project,
  Model.Environment;

type
  IServices = interface
    ['{0959ED55-8FA5-4D88-88BB-EB2738261A23}']
  end;

  ILogServices = interface
    ['{50EDF1E4-BABC-42BD-A93D-957C53ED9663}']
    procedure Log(const AString: string);
  end;

  IADBServices = interface
    ['{BAF1EE13-B459-4EBC-9E81-7C782F285F22}']
    procedure ListDevices(const AAdbPath: string; const AStrings: TStrings);
    function BuildApk(const AAppBasePath, AAppName: string;
      const AEnvironmentModel: TEnvironmentModel; const AResult: TStrings): boolean;
    function InstallApk(const AAdbPath, AApkPath, ADevice: string;
      const AResult: TStrings): boolean;
    procedure RunApp(const AAdbPath, APkgName, ADevice: string; const AResult: TStrings);
  end;

  IAppServices = interface
    ['{0F669CC6-DB3A-437E-8724-8831719A3E9B}']
    procedure CopyAppFiles(const AModel: TProjectModel);
    procedure UpdateManifest(const AModel: TProjectModel);
    function BuildApk(const AProjectModel: TProjectModel;
      const AEnvironmentModel: TEnvironmentModel): boolean;
    function InstallApk(const AProjectModel: TProjectModel;
      const AEnvironmentModel: TEnvironmentModel; const ADevice: string): boolean;
    procedure AddScriptFile(const AModel: TProjectModel; const AFileName: string;
      const AStream: TStream);
  end;

var
  GlobalServices: IServices;

implementation

end.
