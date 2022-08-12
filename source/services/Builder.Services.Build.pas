unit Builder.Services.Build;

interface

uses
  Builder.Chain,
  Builder.Model.Project,
  Builder.Model.Environment,
  Builder.Services,
  Builder.Storage,
  Builder.Model;

type
  TBuildService = class(TInterfacedObject, IBuildServices)
  private
    //Status
    FBuilding: boolean;
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
    //Getters
    function GetIsBuilding(): boolean;
    //Validators
    procedure UpdateModels();
    //Internal build
    procedure DoBuildProject();
    procedure DoDeployProject(const ABuildProject: boolean = true);
    procedure DoRunProject();
    procedure DoDebugProject(const ADebugger: IDebugServices);
  public
    constructor Create();
    destructor Destroy(); override;

    procedure BuildActiveProject();
    procedure BuildActiveProjectAsync();
    procedure DeployActiveProject();
    procedure DeployActiveProjectAsync();
    procedure RunActiveProject();
    procedure RunActiveProjectAsync();
    procedure DebugActiveProject(const ADebugger: IDebugServices);
    procedure DebugActiveProjectAsync(const ADebugger: IDebugServices);
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.Threading,
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
      var LResult := TStringList.Create();
      try
        FAdbServices.StopDebugSession(
          FEnvironmentModel.AdbLocation, FEnvironmentModel.RemoteDebuggerPort, LResult);

        FAdbServices.ForceStopApp(
          FEnvironmentModel.AdbLocation,
          FProjectModel.PackageName,
          FAdbServices.ActiveDevice,
          LResult);
      finally
        LResult.Free();
      end;
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
    raise Exception.Create('The Environment Settings are empty.');

  FProjectModel := FProjectServices.GetActiveProject();
  if not Assigned(FProjectModel)
    or not FProjectStorage.LoadModel(FProjectModel, String.Empty, FProjectModel.Id) then
      raise Exception.Create('The Project Settings are empty.');

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

procedure TBuildService.DoBuildProject;
begin
  //Generates the project necessary files and settings
  FAppServices.BuildProject(FProjectServices.GetActiveProject());
  //Creates and signs the APK file
  if not FAppServices.BuildApk(FProjectModel, FEnvironmentModel) then
    raise Exception.Create('Build process failed. Check log for details.');
  TGlobalBuilderChain.BroadcastEventAsync(
    TMessageEvent.Create('Build process done.'));
end;

procedure TBuildService.DoDeployProject(const ABuildProject: boolean);
begin
  if ABuildProject then
    DoBuildProject();
  //Installs the APK on the device
  if FAppServices.IsAppInstalled(FProjectModel, FEnvironmentModel, FAdbServices.ActiveDevice) then
    FAppServices.UnInstallApk(FProjectModel, FEnvironmentModel, FAdbServices.ActiveDevice);
  if not FAppServices.InstallApk(FProjectModel, FEnvironmentModel, FAdbServices.ActiveDevice) then
    raise Exception.Create('Install process failed. Check log for details.');
end;

procedure TBuildService.DoRunProject;
begin
  DoDeployProject();
  var LResult := TStringList.Create();
  try
    //Launch app on device
    FAdbServices.RunApp(
      FEnvironmentModel.AdbLocation,
      FProjectModel.PackageName,
      FAdbServices.ActiveDevice,
      LResult);
  finally
    LResult.Free();
  end;
end;

procedure TBuildService.DoDebugProject(const ADebugger: IDebugServices);
begin
  //DoDeployProject(true);
  //Launch app on device and debug the Python script
  var LResult := TStringList.Create();
  try
    FAdbServices.ForceStopApp(
      FEnvironmentModel.AdbLocation,
      FProjectModel.PackageName,
      FAdbServices.ActiveDevice,
      LResult);

    FAdbServices.DebugApp(
      FEnvironmentModel.AdbLocation,
      FProjectModel.PackageName,
      FAdbServices.ActiveDevice,
      FEnvironmentModel.RemoteDebuggerHost,
      FEnvironmentModel.RemoteDebuggerPort,
      LResult);
    FAdbServices.StartDebugSession(
      FEnvironmentModel.AdbLocation, FEnvironmentModel.RemoteDebuggerPort, LResult);
    try
      ADebugger.Start(
        FEnvironmentModel.RemoteDebuggerHost, FEnvironmentModel.RemoteDebuggerPort);
    except
      on E: Exception do begin
        FAdbServices.StopDebugSession(
          FEnvironmentModel.AdbLocation, FEnvironmentModel.RemoteDebuggerPort, LResult);
        raise;
      end;
    end;
  finally
    LResult.Free();
  end;
end;

procedure TBuildService.BuildActiveProject();
begin
  FProjectServices.CheckActiveProject();
  UpdateModels();
  FBuilding := true;
  try
    DoBuildProject();
  finally
    FBuilding := false;
  end;
end;

procedure TBuildService.BuildActiveProjectAsync();
begin
  FProjectServices.CheckActiveProject();
  UpdateModels();

  TGlobalBuilderChain.BroadcastEventAsync(
    TMessageEvent.Create(true));
  TGlobalBuilderChain.BroadcastEventAsync(
    TAsyncOperationStartedEvent.Create(TAsyncOperation.BuildProject));

  FBuilding := true;
  TTask.Run(procedure begin
    try
      try
        DoBuildProject();
      finally
        FBuilding := false;
      end;
      TGlobalBuilderChain.BroadcastEventAsync(
        TAsyncOperationEndedEvent.Create(TAsyncOperation.BuildProject));
    except
      on E: exception do begin
        TGlobalBuilderChain.BroadcastEventAsync(
          TAsyncOperationEndedEvent.Create(TAsyncOperation.BuildProject));
        TGlobalBuilderChain.BroadcastEventAsync(
          TAsyncExceptionEvent.Create());
      end;
    end;
  end);
end;

procedure TBuildService.DebugActiveProject(const ADebugger: IDebugServices);
begin
  FProjectServices.CheckActiveProject();
  FAdbServices.CheckActiveDevice();
  UpdateModels();

  FBuilding := true;
  try
    DoDebugProject(ADebugger);
  finally
    FBuilding := false;
  end;
end;

procedure TBuildService.DebugActiveProjectAsync(const ADebugger: IDebugServices);
begin
  FProjectServices.CheckActiveProject();
  FAdbServices.CheckActiveDevice();
  UpdateModels();

  TGlobalBuilderChain.BroadcastEventAsync(TMessageEvent.Create(true));
  TGlobalBuilderChain.BroadcastEventAsync(
    TAsyncOperationStartedEvent.Create(TAsyncOperation.DebugProject));

  FBuilding := true;
  TTask.Run(procedure begin
    try
      try
        DoDebugProject(ADebugger);
      finally
        FBuilding := false;
      end;
      TGlobalBuilderChain.BroadcastEventAsync(
        TAsyncOperationEndedEvent.Create(TAsyncOperation.DebugProject));
    except
      on E: Exception do begin
        TGlobalBuilderChain.BroadcastEventAsync(
          TAsyncOperationEndedEvent.Create(TAsyncOperation.DebugProject));
        TGlobalBuilderChain.BroadcastEventAsync(
          TAsyncExceptionEvent.Create());
      end;
    end;
  end);
end;

procedure TBuildService.DeployActiveProject();
begin
  FProjectServices.CheckActiveProject();
  FAdbServices.CheckActiveDevice();
  UpdateModels();
  FBuilding := true;
  try
    DoDeployProject();
  finally
    FBuilding := false;
  end;
end;

procedure TBuildService.DeployActiveProjectAsync();
begin
  FProjectServices.CheckActiveProject();
  FAdbServices.CheckActiveDevice();
  UpdateModels();

  TGlobalBuilderChain.BroadcastEventAsync(
    TMessageEvent.Create(true));
  TGlobalBuilderChain.BroadcastEventAsync(
    TAsyncOperationStartedEvent.Create(TAsyncOperation.DeployProject));

  FBuilding := true;
  TTask.Run(procedure begin
    try
      try
        DoDeployProject();
      finally
        FBuilding := false;
      end;
      TGlobalBuilderChain.BroadcastEventAsync(
        TAsyncOperationEndedEvent.Create(TAsyncOperation.DeployProject));
    except
      on E: Exception do begin
        TGlobalBuilderChain.BroadcastEventAsync(
          TAsyncOperationEndedEvent.Create(TAsyncOperation.DeployProject));
        TGlobalBuilderChain.BroadcastEventAsync(
          TAsyncExceptionEvent.Create());
      end;
    end;
  end);
end;

procedure TBuildService.RunActiveProject();
begin
  FProjectServices.CheckActiveProject();
  FAdbServices.CheckActiveDevice();
  UpdateModels();
  DoRunProject();
end;

procedure TBuildService.RunActiveProjectAsync();
begin
  FProjectServices.CheckActiveProject();
  FAdbServices.CheckActiveDevice();
  UpdateModels();

  TGlobalBuilderChain.BroadcastEventAsync(
    TMessageEvent.Create(true));
  TGlobalBuilderChain.BroadcastEventAsync(
    TAsyncOperationStartedEvent.Create(TAsyncOperation.RunProject));

  FBuilding := true;
  TTask.Run(procedure begin
    try
      try
        DoRunProject();
      finally
        FBuilding := false;
      end;
      TGlobalBuilderChain.BroadcastEventAsync(
        TAsyncOperationEndedEvent.Create(TAsyncOperation.RunProject));
    except
      on E: Exception do begin
        TGlobalBuilderChain.BroadcastEventAsync(
          TAsyncOperationEndedEvent.Create(TAsyncOperation.RunProject));
        TGlobalBuilderChain.BroadcastEventAsync(
          TAsyncExceptionEvent.Create());
      end;
    end;
  end);
end;

end.
