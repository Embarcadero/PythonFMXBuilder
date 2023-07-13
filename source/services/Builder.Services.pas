unit Builder.Services;

interface

uses
  System.Classes,
  System.IOUtils,
  System.SysUtils,
  System.Types,
  Builder.Types,
  Builder.Model.Project,
  Builder.Model.Project.Files,
  Builder.Model.Environment;

type
  IRunner<T> = interface
    ['{8C84D954-BD52-4B33-A6C0-C006B16A9248}']
    procedure Run(const AProxy: TProc<T>);
    function RunAsync(const AProxy: TProc<T>;
      const AAsyncCallback: TAsyncCallback = nil): IAsyncResult;
  end;

  IAdbServices = interface
    ['{BAF1EE13-B459-4EBC-9E81-7C782F285F22}']
    procedure ListDevices(const AStrings: TStrings);
    procedure SetActiveDevice(const ADeviceName: string);
    function GetActiveDevice(): string;
    procedure CheckActiveDevice();

    //Exec subprocess
    procedure RunSubprocess(const ACmd: string; const AArgs, AEnvVars: TArray<string>);
    //File helpers
    function SendFile(const ALocalFilePath, ARemoteFilePath: string): boolean;
    procedure RemoveFile(const ARemoteFilePath: string);
    function ExtractZip(const ARemoteFilePath, ARemoteDir: string): boolean;
    //Directory helpers
    function CreateDirectory(const ARemotePath: string): boolean;
    procedure DeleteDirectory(const ARemoteDir: string);
    function DirectoryExists(const ARemoteDir: string): boolean;

    function BuildApk(const AAppBasePath, AProjectName: string): boolean;
    function InstallApk(const AApkPath: string): boolean;
    function UnInstallApk(const APkgName: string): boolean;
    function IsAppInstalled(const APkgName: string): boolean;
    function IsAppRunning(const APkgName: string): boolean;
    function GetAppInstallationPath(const APkgName: string): string;
    procedure RunApp(const APkgName: string);
    procedure StartDebugSession(const APort: integer);
    procedure StopDebugSession(const APort: integer);
    procedure DebugApp(const APkgName, AHost: string;
      const APort: integer);
    procedure ForceStopApp(const APkgName: string);

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
    procedure CheckActiveProject();

    //Main module
    function CreateMainModule(const AModel: TProjectModel): string;
    procedure SetMainModule(const AModel: TProjectModel;
      const AFilePath: string);
    function IsMainModule(const AModel: TProjectModel;
      const AFilePath: string): boolean;

    //Modules
    function AddModule(const AModel: TProjectModel;
      const AFilePath: string): TProjectFilesModule;
    procedure RemoveModule(const AModel: TProjectModel; const AFilePath: string);
    function GetModules(const AModel: TProjectModel): TProjectFilesModules;

    //Dependencies
    function AddDependency(const AModel: TProjectModel;
      const AFilePath: string): TProjectFilesDependency;
    procedure RemoveDependency(const AModel: TProjectModel;
      const AFilePath: string);
    function GetDependencies(const AModel: TProjectModel): TProjectFilesDependencies;
    procedure ClearDependencies(const AModel: TProjectModel);

    //Packages
    function AddPackage(const AModel: TProjectModel;
      const AFilePath: string): TProjectFilesPackage;
    procedure RemovePackage(const AModel: TProjectModel;
      const AFilePath: string);
    function GetPackages(const AModel: TProjectModel): TProjectFilesPackages;
    procedure ClearPackages(const AModel: TProjectModel);

    //Other files
    function AddOtherFile(const AModel: TProjectModel;
      const AFilePath: string): TProjectFilesOther;
    procedure RemoveOtherFile(const AModel: TProjectModel;
      const AFilePath: string);
    function GetOtherFiles(const AModel: TProjectModel): TProjectFilesOthers;
    procedure ClearOtherFiles(const AModel: TProjectModel);
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
    function AddFile(const AModel: TProjectModel; const AFileName: string;
      const AStream: TStream): string;
    procedure RemoveFile(const AModel: TProjectModel; const AFilePath: string);
    function GetFiles(const AModel: TProjectModel;
      const AFilter: TDirectory.TFilterPredicate = nil): TArray<string>;
    procedure CopyModules(const AModel: TProjectModel);
    procedure CopyDependencies(const AModel: TProjectModel);
    //Send the user icons to the deployable folder
    procedure CopyIcons(const AModel: TProjectModel);
    procedure BuildProject(const AModel: TProjectModel);
    //Generate the new APK
    function BuildApk(const AProjectModel: TProjectModel;
      const AEnvironmentModel: TEnvironmentModel): boolean;
    //Launch APK on user's device
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

  IBuilderTasks = interface
    ['{84F93F2A-AD66-46C7-B93D-FA9F70076212}']
    procedure BuildActiveProject();
    procedure DeployActiveProject(const AUninstall: boolean = true);
    procedure RunActiveProject();
    procedure DebugActiveProject(const ADebugger: IDebugServices);
    procedure StopActiveProject();
  end;

  IBuildServices = interface(IRunner<IBuilderTasks>)
    ['{8BA3AEDE-8E35-42AE-9014-DCBFD0AA197C}']
    function GetIsBuilding(): boolean;
    property IsBuilding: boolean read GetIsBuilding;
  end;

  IUnboundPythonServices = interface
    ['{114EE2AC-1DC0-4B24-9BBA-3E0D172A8422}']
    procedure Make(const APythonVersion: TPythonVersion;
      const AArchitecture: TArchitecture);
    procedure Remove(const APythonVersion: TPythonVersion;
      const AArchitecture: TArchitecture);
    function Exists(const APythonVersion: TPythonVersion;
      const AArchitecture: TArchitecture): boolean;
    procedure Run(const APythonVersion: TPythonVersion;
      const AArchitecture: TArchitecture; const ADebugger: TDebugger;
      const ABuildConfiguration: TBuildConfiguration);
  end;

implementation

end.
