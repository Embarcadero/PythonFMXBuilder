unit Builder.Services;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils,
  Builder.Architecture, Builder.PythonVersion,
  Builder.Model.Project, Builder.Model.Environment;

type
  ILogServices = interface;
  IDesignServices = interface;

  IServices = interface
    ['{0959ED55-8FA5-4D88-88BB-EB2738261A23}']
    function GetLogServices(): ILogServices;
    function GetDesignServices(): IDesignServices;

    property LogServices: ILogServices read GetLogServices;
    property DesignServices: IDesignServices read GetDesignServices;
  end;

  ILogServices = interface
    ['{50EDF1E4-BABC-42BD-A93D-957C53ED9663}']
    procedure Clear();
    procedure Log(const AString: string);
  end;

  IDesignServices = interface
    ['{D42F5686-63D2-4429-9B25-E53983897862}']
    procedure BeginAsync();
    procedure EndAsync();
    procedure ShowException(const AException: Exception);
    procedure OpenProject(const AProjectModel: TProjectModel);
    procedure CloseProject(const AProjectModel: TProjectModel);
  end;

  IAdbServices = interface
    ['{BAF1EE13-B459-4EBC-9E81-7C782F285F22}']
    procedure ListDevices(const AAdbPath: string; const AStrings: TStrings);
    procedure SetActiveDevice(const ADeviceName: string);
    function GetActiveDevice(): string;

    function BuildApk(const AAppBasePath, AProjectName: string;
      const AEnvironmentModel: TEnvironmentModel; const AResult: TStrings): boolean;
    function InstallApk(const AAdbPath, AApkPath, ADevice: string;
      const AResult: TStrings): boolean;
    function UnInstallApk(const AAdbPath, APkgName, ADevice: string;
      const AResult: TStrings): boolean;
    procedure RunApp(const AAdbPath, APkgName, ADevice: string; const AResult: TStrings);

    property ActiveDevice: string read GetActiveDevice write SetActiveDevice;
  end;

  IProjectServices = interface
    ['{4C39B307-4536-4832-972D-0DEB0319509A}']
    function CreateProject(const AProjectName: string;
      const AAddMainScript: boolean = true): TProjectModel;
    procedure SaveProject(const AProject: TProjectModel);
    function LoadProject(const AProjectName: string): TProjectModel;
    function ListProjects(): TArray<string>;
    function HasProject(const AProjectName: string): boolean;
    function RemoveProject(const AProjectName: string): boolean;
    function GetActiveProject(): TProjectModel;

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
    //App defs. file (used by the Android app)
    procedure CreateAppDefs(const AModel: TProjectModel);
    //assets/internal dataset
    procedure CopyScriptFiles(const AModel: TProjectModel);
    function AddScriptFile(const AModel: TProjectModel; const AFileName: string;
      const AStream: TStream): string;
    procedure RemoveScriptFile(const AModel: TProjectModel; const AFilePath: string);
    function GetScriptFiles(const AModel: TProjectModel;
      const AFilter: TDirectory.TFilterPredicate = nil): TArray<string>;
    //Send the user icons to the deployable folder
    procedure CopyIcons(const AModel: TProjectModel);
    procedure BuildProject(const AModel: TProjectModel);
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
