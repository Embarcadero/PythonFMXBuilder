unit Builder.Services;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils,
  Builder.Architecture, Builder.PythonVersion,
  Builder.Model.Project, Builder.Model.Environment;

type
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
    function IsAppInstalled(const AAdbPath, APkgName, ADevice: string; const AResult: TStrings): boolean;
    function IsAppRunning(const AAdbPath, APkgName, ADevice: string; const AResult: TStrings): boolean;
    function GetAppInstallationPath(const AAdbPath, APkgName, ADevice: string; const AResult: TStrings): string;
    procedure RunApp(const AAdbPath, APkgName, ADevice: string; const AResult: TStrings);
    procedure StartDebugSession(const AAdbPath: string; const APort: integer; const AResult: TStrings);
    procedure StopDebugSession(const AAdbPath: string; const APort: integer; const AResult: TStrings);
    procedure DebugApp(const AAdbPath, APkgName, ADevice, AHost: string;
      const APort: integer; const AResult: TStrings);
    procedure ForceStopApp(const AAdbPath, APkgName, ADevice: string; const AResult: TStrings);

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
    //Check if app is installed
    function IsAppInstalled(const AProjectModel: TProjectModel;
      const AEnvironmentModel: TEnvironmentModel; const ADevice: string): boolean;
    //Check if app is running
    function IsAppRunning(const AProjectModel: TProjectModel;
      const AEnvironmentModel: TEnvironmentModel; const ADevice: string): boolean;
  end;

  {$SCOPEDENUMS ON}
  TDebuggerConnectionStatus = (
    OutOfWork,
    Connecting, //We are ordering a conenction to the debugger
    Started, //We are conected to the debugger - Debugger has confirmed
    Disconnecting, //We ordered to disconnect from the debugger
    Stopped //We are disconnected from the debugger - Debugger has confirmed
  );
  {$SCOPEDENUMS OFF}
  IDebugServices = interface
    ['{568CC96C-4A33-4CA1-8972-1F2C7280B0EE}']
    function GetConnectionStatus(): TDebuggerConnectionStatus;
    function GetIsDebugging(): boolean;

    procedure Start(const AHost: string; const APort: integer; const ATimeOut: Int64 = 120);
    procedure Pause();
    procedure Stop();
    procedure StepIn();
    procedure StepOver();
    procedure StepOut();
    procedure Continue();

    function CanStart(): boolean;
    function CanPause(): boolean;
    function CanStop(): boolean;
    function CanStepIn(): boolean;
    function CanStepOut(): boolean;
    function CanStepOver(): boolean;
    function CanContinue(): boolean;

    property ConnectionStatus: TDebuggerConnectionStatus read GetConnectionStatus;
    property IsDebugging: boolean read GetIsDebugging;
  end;

implementation

end.
