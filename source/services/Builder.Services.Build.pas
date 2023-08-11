unit Builder.Services.Build;

interface

uses
  System.Types,
  System.SysUtils,
  System.Classes,
  Builder.Types,
  Builder.Messagery,
  Builder.Model.Project,
  Builder.Model.Environment,
  Builder.Services,
  Builder.Model;

type
  TBuildService = class(TInterfacedObject, IBuildServices, IRunner<IBuilderTasks>, IBuilderTasks)
  private
    //Status
    FBuilding: boolean;
    FAsync: boolean;
    //Models
    FProjectModel: TProjectModel;
    FEnvironmentModel: TEnvironmentModel;
    //Services
    FProjectServices: IProjectServices;
    FEnvironmentServices: IEnvironmentServices;
    FAppServices: IAppServices;
    FAdbServices: IAdbServices;
    //Events
    FDebugSessionEndedEvent: IDisconnectable;
    //Getters and setters
    function GetIsBuilding(): boolean;
    //Validators
    procedure UpdateModels();
    //Runner
    procedure DoRunAsync(const AAsyncOperation: TAsyncOperation; const AProc: TProc);
    //Internal build
    procedure BeginBuild(const AAsync: boolean = false);
    procedure EndBuild();
    procedure DoBuildProject();
    procedure DoSmartBuildProject();
    procedure DoDeployProject(const AUninstall: boolean);
    procedure DoSmartDeployProject();
    procedure DoRunProject();
    procedure DoStopProject();
    procedure DoDebugProject(const ADebugger: IDebugServices);
    //IBuilderTasks
    procedure BuildActiveProject();
    procedure DeployActiveProject(const AUninstall: boolean = true);
    procedure RunActiveProject();
    procedure DebugActiveProject(const ADebugger: IDebugServices);
    procedure StopActiveProject();
    //IBuilderTasks smart tasks
    procedure SmartBuildActiveProject();
    procedure SmartDeployActiveProject();
  protected type
    TBuilderRunnerAsyncResult = class(TBaseAsyncResult)
    private
      FAsyncTask: TProc;
      FAsyncCallback: TAsyncCallback;
    protected
      procedure Complete; override;
      procedure Schedule; override;
      procedure AsyncDispatch; override;
      constructor Create(const AContext: TObject; const AAsyncTask: TProc;
        AAsyncCallback: TAsyncCallback = nil);
    end;
  public
    constructor Create();
    destructor Destroy(); override;

    procedure Run(const ATasksProxy: TProc<IBuilderTasks>);
    function RunAsync(const ATasksProxy: TProc<IBuilderTasks>;
      const AAsyncCallback: TAsyncCallback = nil): IAsyncResult;
  end;

implementation

uses
  System.Threading,
  System.IOUtils,
  Builder.Paths,
  Builder.Exception;

{ TBuildService }

constructor TBuildService.Create;
begin
  inherited;
  FEnvironmentServices := TBuilderService.CreateService<IEnvironmentServices>;
  FProjectServices := TBuilderService.CreateService<IProjectServices>;
  FAppServices := TBuilderService.CreateService<IAppServices>;
  FAdbServices := TBuilderService.CreateService<IADBServices>;
  FDebugSessionEndedEvent := TMessagery.SubscribeToEvent<TDebugSessionStoppedEvent>(
    procedure(const AEventNotification: TDebugSessionStoppedEvent)
    begin
      FAdbServices.StopDebugSession(FEnvironmentModel.RemoteDebuggerPort);
      FAdbServices.ForceStopApp(FProjectModel.PackageName);
    end);
end;

destructor TBuildService.Destroy;
begin
  inherited;
  FDebugSessionEndedEvent.Disconnect();
end;

function TBuildService.GetIsBuilding: boolean;
begin
  Result := FBuilding;
end;

procedure TBuildService.UpdateModels;
begin
  FEnvironmentServices.CheckActiveEnvironment();
  FEnvironmentModel := FEnvironmentServices.GetActiveEnvironment();

  FProjectServices.CheckActiveProject();
  FProjectModel := FProjectServices.GetActiveProject();

  var LModelErrors := TStringList.Create();
  try
    if not FEnvironmentModel.Validate(LModelErrors) then
      raise EModelValidationError.Create('The Environment Settings has invalid arguments:'
        + sLineBreak
        + sLineBreak
        + LModelErrors.Text);

    if not FProjectModel.Validate(LModelErrors) then
      raise EModelValidationError.Create('The Project Settings has invalid arguments:'
        + sLineBreak
        + sLineBreak
        + LModelErrors.Text);
  finally
    LModelErrors.Free();
  end;
end;

procedure TBuildService.BeginBuild(const AAsync: boolean);
begin
  FBuilding := true;
  FAsync := AAsync;
  try
    FProjectServices.CheckActiveProject();
    UpdateModels();
  except
    on E: exception do begin
      FBuilding := false;
      TMessagery.BroadcastEventAsync(
        TAsyncExceptionEvent.Create());
      Abort;
    end;
  end;
end;

procedure TBuildService.EndBuild;
begin
  FBuilding := false;
  FAsync := false;
end;

procedure TBuildService.DoRunAsync(const AAsyncOperation: TAsyncOperation;
  const AProc: TProc);
begin
  TMessagery.BroadcastEventAsync(
    TAsyncOperationStartedEvent.Create(AAsyncOperation));
  try
    AProc();
    TMessagery.BroadcastEventAsync(
      TAsyncOperationEndedEvent.Create(AAsyncOperation));
  except
    on E: exception do begin
      TMessagery.BroadcastEventAsync(
        TAsyncOperationEndedEvent.Create(AAsyncOperation));
      TMessagery.BroadcastEventAsync(
        TAsyncExceptionEvent.Create());
      Abort;
    end;
  end;
end;

procedure TBuildService.DoBuildProject;
begin
  TMessagery.BroadcastEventAsync(
    TMessageEvent.Create('Build process has started.'));
  //Generates the project necessary files and settings
  FAppServices.BuildProject(FProjectServices.GetActiveProject());
  //Creates and signs the APK file
  if not FAppServices.BuildApk(FProjectModel, FEnvironmentModel) then
    raise EBuildFailed.Create('Build process failed. Check log for details.');
  TMessagery.BroadcastEventAsync(
    TMessageEvent.Create('Build process finished.'));
end;

procedure TBuildService.DoDeployProject(const AUninstall: boolean);
begin
  TMessagery.BroadcastEventAsync(
    TMessageEvent.Create('Deployment process has started.'));
  if AUninstall and FAppServices.IsAppInstalled(FProjectModel, FEnvironmentModel, FAdbServices.ActiveDevice) then
    FAppServices.UnInstallApk(FProjectModel, FEnvironmentModel, FAdbServices.ActiveDevice);
  if not FAppServices.InstallApk(FProjectModel, FEnvironmentModel, FAdbServices.ActiveDevice) then
    raise EInstallFailed.Create('Install process failed. Check log for details.');
  TMessagery.BroadcastEventAsync(
    TMessageEvent.Create('Deployment process finished.'));
end;

procedure TBuildService.DoRunProject();
begin
  TMessagery.BroadcastEventAsync(
    TMessageEvent.Create('Launch process has started.'));

  FAdbServices.ForceStopApp(FProjectModel.PackageName);

  //Launch app on device
  FAdbServices.RunApp(FProjectModel.PackageName);

  TMessagery.BroadcastEventAsync(
    TMessageEvent.Create('Launch process finished.'));
end;

procedure TBuildService.DoSmartBuildProject;
begin
  TMessagery.BroadcastEventAsync(
    TMessageEvent.Create('Smart build process has started.'));

  if not FAppServices.IsAppInstalled(FProjectModel, FEnvironmentModel, FAdbServices.ActiveDevice) then begin
    TMessagery.BroadcastEventAsync(
      TMessageEvent.Create('Application not installed. Build process fallback.'));
    DoBuildProject();
    Exit;
  end;

  //Generates the project necessary files and settings
  FAppServices.BuildProject(FProjectServices.GetActiveProject());

  TMessagery.BroadcastEventAsync(
    TMessageEvent.Create('Smart build process finished.'));
end;

procedure TBuildService.DoSmartDeployProject;
begin
  TMessagery.BroadcastEventAsync(
    TMessageEvent.Create('Smart deployment process has started.'));

  if not FAppServices.IsAppInstalled(FProjectModel, FEnvironmentModel, FAdbServices.ActiveDevice) then begin
    TMessagery.BroadcastEventAsync(
      TMessageEvent.Create('Application not installed. Deployment process fallback.'));
    DoDeployProject(false);
    Exit;
  end;

  //Send app files
  for var LAsset in FAppServices.GetFiles(FProjectModel) do
    FAdbServices.SendFile(FProjectModel.PackageName, LAsset, TPath.GetFileName(LAsset));

  TMessagery.BroadcastEventAsync(
    TMessageEvent.Create('Smart deployment process finished.'));
end;

procedure TBuildService.DoStopProject;
begin
  FAdbServices.ForceStopApp(FProjectModel.PackageName);
  FAdbServices.StopDebugSession(FEnvironmentModel.RemoteDebuggerPort);
end;

procedure TBuildService.DoDebugProject(const ADebugger: IDebugServices);
begin
  //Launch app on device and debug the Python script
  FAdbServices.ForceStopApp(FProjectModel.PackageName);

  FAdbServices.DebugApp(
    FProjectModel.PackageName,
    FEnvironmentModel.RemoteDebuggerHost,
    FEnvironmentModel.RemoteDebuggerPort);
  FAdbServices.StartDebugSession(FEnvironmentModel.RemoteDebuggerPort);
  try
    ADebugger.Start(
      FEnvironmentModel.RemoteDebuggerHost, FEnvironmentModel.RemoteDebuggerPort);
    TMessagery.BroadcastEventAsync(
      TMessageEvent.Create('Debug process has started.'));
  except
    on E: Exception do begin
      FAdbServices.StopDebugSession(FEnvironmentModel.RemoteDebuggerPort);
      raise;
    end;
  end;
end;

procedure TBuildService.BuildActiveProject();
begin
  if FAsync then begin
    DoRunAsync(TAsyncOperation.BuildProject, DoBuildProject)
  end else
    DoBuildProject();
end;

procedure TBuildService.DeployActiveProject(const AUninstall: boolean = true);
begin
  FAdbServices.CheckActiveDevice();
  if FAsync then begin
    DoRunAsync(TAsyncOperation.DeployProject, procedure() begin
      DoDeployProject(AUninstall)
    end);
  end else
    DoDeployProject(AUninstall);
end;

procedure TBuildService.RunActiveProject();
begin
  FAdbServices.CheckActiveDevice();
  if FAsync then begin
    DoRunAsync(TAsyncOperation.DebugProject, procedure() begin
      DoRunProject();
    end);
  end else
    DoRunProject();
end;

procedure TBuildService.DebugActiveProject(const ADebugger: IDebugServices);
begin
  FAdbServices.CheckActiveDevice();
  if FAsync then begin
    DoRunAsync(TAsyncOperation.RunProject, procedure() begin
      DoDebugProject(ADebugger);
    end);
  end else
    DoDebugProject(ADebugger);
end;

procedure TBuildService.SmartBuildActiveProject;
begin
  FAdbServices.CheckActiveDevice();
  if FAsync then begin
    DoRunAsync(TAsyncOperation.BuildProject, procedure() begin
      DoSmartBuildProject()
    end);
  end else
    DoSmartBuildProject();
end;

procedure TBuildService.SmartDeployActiveProject;
begin
  FAdbServices.CheckActiveDevice();
  if FAsync then begin
    DoRunAsync(TAsyncOperation.DeployProject, procedure() begin
      DoSmartDeployProject()
    end);
  end else
    DoSmartDeployProject();
end;

procedure TBuildService.StopActiveProject;
begin
  FAdbServices.CheckActiveDevice();
  if FAsync then begin
    DoRunAsync(TAsyncOperation.StopProject, procedure() begin
      DoStopProject();
    end);
  end else
    DoStopProject();
end;

procedure TBuildService.Run(const ATasksProxy: TProc<IBuilderTasks>);
begin
  Assert(Assigned(ATasksProxy), 'Invalid argument [ATasksProxy].');
  BeginBuild();
  try
    ATasksProxy(Self);
  finally
    EndBuild();
  end;
end;

function TBuildService.RunAsync(
  const ATasksProxy: TProc<IBuilderTasks>;
  const AAsyncCallback: TAsyncCallback): IAsyncResult;
begin
  Assert(Assigned(ATasksProxy), 'Invalid argument [ATasksProxy].');
  Result := TBuilderRunnerAsyncResult.Create(Self, procedure() begin
    BeginBuild(true);
    try
      ATasksProxy(Self);
    finally
      EndBuild();
    end;
  end, AAsyncCallback).Invoke();
end;

{ TBuildService.TBuilderRunnerAsyncResult }

procedure TBuildService.TBuilderRunnerAsyncResult.AsyncDispatch;
begin
  FAsyncTask();
end;

procedure TBuildService.TBuilderRunnerAsyncResult.Complete;
begin
  inherited;
  if Assigned(FAsyncCallback) then
    FAsyncCallback(Self as IAsyncResult);
end;

constructor TBuildService.TBuilderRunnerAsyncResult.Create(
  const AContext: TObject; const AAsyncTask: TProc;
  AAsyncCallback: TAsyncCallback);
begin
  inherited Create(AContext);
  FAsyncTask := AAsyncTask;
  FAsyncCallback := AAsyncCallback;
end;

procedure TBuildService.TBuilderRunnerAsyncResult.Schedule;
begin
  TTask.Run(DoAsyncDispatch);
end;

end.
