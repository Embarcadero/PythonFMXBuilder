unit Builder.Services;

interface

uses
  System.Classes,
  System.IOUtils,
  System.SysUtils,
  System.Types,
  Builder.Types,
  Builder.Model.Project,
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
    procedure ListDevices(const AAdbPath: string; const AStrings: TStrings);
    procedure SetActiveDevice(const ADeviceName: string);
    function GetActiveDevice(): string;
    procedure CheckActiveDevice();

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
    procedure CheckActiveProject();

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

    function AddDependency(const AModel: TProjectModel;
      const AFilePath: string): boolean;
    procedure RemoveDependency(const AModel: TProjectModel;
      const AFilePath: string);
    function GetDependencies(const AModel: TProjectModel): TArray<string>;
    procedure ClearDependencies(const AModel: TProjectModel);
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
    procedure CopyScripts(const AModel: TProjectModel);
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

  {$SCOPEDENUMS ON}
  TDebuggerConnectionStatus = (
    OutOfWork, //We are tension-free
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

  {$SCOPEDENUMS ON}
  TRunMode = (
    RunNormalMode, //It launches the application, but it doesn't initialize the debugger
    RunDebugMode //It launches the application and initialize the debugger - it sets the Python interpreter to interactive mode
  );
  {$SCOPEDENUMS OFF}

  IBuilderTasks = interface
    ['{84F93F2A-AD66-46C7-B93D-FA9F70076212}']
    procedure BuildActiveProject();
    procedure DeployActiveProject(const AUninstall: boolean = true);
    procedure RunActiveProject(const ARunMode: TRunMode = TRunMode.RunNormalMode);
    procedure DebugActiveProject(const ADebugger: IDebugServices);
    procedure StopActiveProject();
  end;

  IBuildServices = interface(IRunner<IBuilderTasks>)
    ['{8BA3AEDE-8E35-42AE-9014-DCBFD0AA197C}']
    function GetIsBuilding(): boolean;
    property IsBuilding: boolean read GetIsBuilding;
  end;

implementation

end.
