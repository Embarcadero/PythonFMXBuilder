unit Builder.Services;

interface

uses
  System.Classes, System.IOUtils,
  Builder.Architecture, Builder.PythonVersion,
  Builder.Model.Project, Builder.Model.Environment;

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
    function UnInstallApk(const AAdbPath, APkgName, ADevice: string;
      const AResult: TStrings): boolean;
    procedure RunApp(const AAdbPath, APkgName, ADevice: string; const AResult: TStrings);
  end;

  IProjectServices = interface
    ['{4C39B307-4536-4832-972D-0DEB0319509A}']
    function CreateProject(const AApplicationName: string;
      const AAddMainScript: boolean = true): TProjectModel;
    procedure SaveProject(const AProject: TProjectModel);
    function LoadProject(const AApplicationName: string): TProjectModel;
    function ListProjects(): TArray<string>;
    function HasProject(const AApplicationName: string): boolean;
    function RemoveProject(const AAplicationName: string): boolean;

    function AddMainScriptFile(const AModel: TProjectModel): string;
    procedure SetMainScriptFile(const AModel: TProjectModel;
      const AFilePath: string);
    function IsMainScriptFile(const AModel: TProjectModel;
      const AFilePath: string): boolean;

    function AddScriptFile(const AModel: TProjectModel;
      const AFilePath: string): boolean;
    procedure RemoveScriptFile(const AModel: TProjectModel;
      const AFilePath: string);
    function GetScriptFiles(const AModel: TProjectModel): TArray<string>;
  end;

  IAppServices = interface
    ['{0F669CC6-DB3A-437E-8724-8831719A3E9B}']
    //Creates a snapshot from the image project
    procedure CopyAppFiles(const AModel: TProjectModel);
    //Defines the app package name, version and etc.
    procedure UpdateManifest(const AModel: TProjectModel);
    //assets/internal dataset
    procedure CopyScriptFiles(const AModel: TProjectModel);
    function AddScriptFile(const AModel: TProjectModel; const AFileName: string;
      const AStream: TStream): string;
    procedure RemoveScriptFile(const AModel: TProjectModel; const AFilePath: string);
    function GetScriptFiles(const AModel: TProjectModel;
      const AFilter: TDirectory.TFilterPredicate = nil): TArray<string>;
    //Send the user icons to the deployable folder
    procedure CopyIcons(const AModel: TProjectModel);
    //Generate the new APK
    function BuildApk(const AProjectModel: TProjectModel;
      const AEnvironmentModel: TEnvironmentModel): boolean;
    //Launch APK to user's device
    function InstallApk(const AProjectModel: TProjectModel;
      const AEnvironmentModel: TEnvironmentModel; const ADevice: string): boolean;
    //Remove APK from user's device
    function UnInstallApk(const AProjectModel: TProjectModel;
      const AEnvironmentModel: TEnvironmentModel; const ADevice: string): boolean;
  end;

var
  GlobalServices: IServices;

implementation

end.
