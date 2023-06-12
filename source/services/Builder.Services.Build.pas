unit Builder.Services.Build;

interface

uses
  System.Types,
  System.SysUtils,
  System.Classes,
  Builder.Chain,
  Builder.Types,
  Builder.Model.Project,
  Builder.Model.Environment,
  Builder.Services,
  Builder.Storage,
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
    FAppServices: IAppServices;
    FAdbServices: IAdbServices;
    //Storages
    FEnvironmentStorage: IStorage<TEnvironmentModel>;
    FProjectStorage: IStorage<TProjectModel>;
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
    procedure DoDeployProject(const AUninstall: boolean);
    procedure DoRunProject();
    procedure DoStopProject();
    procedure DoDebugProject(const ADebugger: IDebugServices);
    //IBuilderTasks
    procedure BuildActiveProject();
    procedure DeployActiveProject(const AUninstall: boolean = true);
    procedure RunActiveProject();
    procedure DebugActiveProject(const ADebugger: IDebugServices);
    procedure StopActiveProject();
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
  Builder.Exception,
  Builder.Storage.Factory,
  Builder.Services.Factory,
  Builder.Storage.Default;

{ TBuildService }

constructor TBuildService.Create;
begin
  inherited;
  FProjectServices := TServiceSimpleFactory.CreateProject();
  FAppServices := TServiceSimpleFactory.CreateApp();
  FAdbServices := TServiceSimpleFactory.CreateAdb();
  FEnvironmentStorage := TDefaultStorage<TEnvironmentModel>.Make();
  FProjectStorage := TDefaultStorage<TProjectModel>.Make();
  FDebugSessionEndedEvent := TGlobalBuilderChain.SubscribeToEvent<TDebugSessionStoppedEvent>(
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
  if not FEnvironmentStorage.LoadModel(FEnvironmentModel) then
    raise EEmptySettings.Create('The Environment Settings are empty.');

  FProjectModel := FProjectServices.GetActiveProject();
  if not Assigned(FProjectModel)
    or not FProjectStorage.LoadModel(FProjectModel, String.Empty, FProjectModel.Id) then
      raise EEmptySettings.Create('The Project Settings are empty.');

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
      TGlobalBuilderChain.BroadcastEventAsync(
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
  TGlobalBuilderChain.BroadcastEventAsync(
    TAsyncOperationStartedEvent.Create(AAsyncOperation));
  try
    AProc();
    TGlobalBuilderChain.BroadcastEventAsync(
      TAsyncOperationEndedEvent.Create(AAsyncOperation));
  except
    on E: exception do begin
      TGlobalBuilderChain.BroadcastEventAsync(
        TAsyncOperationEndedEvent.Create(AAsyncOperation));
      TGlobalBuilderChain.BroadcastEventAsync(
        TAsyncExceptionEvent.Create());
    end;
  end;
end;

procedure TBuildService.DoBuildProject;
begin
  TGlobalBuilderChain.BroadcastEventAsync(
    TMessageEvent.Create('Build process has started.'));
  //Generates the project necessary files and settings
  FAppServices.BuildProject(FProjectServices.GetActiveProject());
  //Creates and signs the APK file
  if not FAppServices.BuildApk(FProjectModel, FEnvironmentModel) then
    raise EBuildFailed.Create('Build process failed. Check log for details.');
  TGlobalBuilderChain.BroadcastEventAsync(
    TMessageEvent.Create('Build process finished.'));
end;

procedure TBuildService.DoDeployProject(const AUninstall: boolean);
begin
  TGlobalBuilderChain.BroadcastEventAsync(
    TMessageEvent.Create('Deployment process has started.'));
  if AUninstall and FAppServices.IsAppInstalled(FProjectModel, FEnvironmentModel, FAdbServices.ActiveDevice) then
    FAppServices.UnInstallApk(FProjectModel, FEnvironmentModel, FAdbServices.ActiveDevice);
  if not FAppServices.InstallApk(FProjectModel, FEnvironmentModel, FAdbServices.ActiveDevice) then
    raise EInstallFailed.Create('Install process failed. Check log for details.');
  TGlobalBuilderChain.BroadcastEventAsync(
    TMessageEvent.Create('Deployment process finished.'));
end;

procedure TBuildService.DoRunProject();
begin
  TGlobalBuilderChain.BroadcastEventAsync(
    TMessageEvent.Create('Launch process has started.'));

  FAdbServices.ForceStopApp(FProjectModel.PackageName);

  //Launch app on device
  case FProjectModel.BuildConfiguration of
    TBuildConfiguration.Release: begin
      FAdbServices.RunApp(FProjectModel.PackageName);
    end;
    TBuildConfiguration.Debug: begin
      FAdbServices.StartDebugSession(FEnvironmentModel.RemoteDebuggerPort);
      try
        FAdbServices.DebugApp(
          FProjectModel.PackageName,
          FEnvironmentModel.RemoteDebuggerHost,
          FEnvironmentModel.RemoteDebuggerPort);
      except
        on E: Exception do begin
          FAdbServices.StopDebugSession(FEnvironmentModel.RemoteDebuggerPort);
          raise;
        end;
      end;
    end;
  end;

  TGlobalBuilderChain.BroadcastEventAsync(
    TMessageEvent.Create('Launch process finished.'));
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
    TGlobalBuilderChain.BroadcastEventAsync(
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
