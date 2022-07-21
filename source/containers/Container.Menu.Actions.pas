unit Container.Menu.Actions;

interface

uses
  System.SysUtils, System.Classes, System.Actions, FMX.ActnList, FMX.Forms,
  Form.Slider, Form.Project.Create, Form.SelectProject,
  Builder.Chain,
  Builder.Model.Project,
  Builder.Model.Environment,
  Builder.Services,
  Builder.Storage,
  Builder.Model;

type
  TMenuActionsContainer = class(TDataModule)
    actlMenu: TActionList;
    actUpdateEnvironment: TAction;
    actUpdateCurrentProject: TAction;
    actBuildCurrentProject: TAction;
    actDeployCurrentProject: TAction;
    actNewProject: TAction;
    actOpenProject: TAction;
    actRemoveCurrentProject: TAction;
    actRunCurrentProject: TAction;
    actBuildCurrentProjectAsync: TAction;
    actDeployCurrentProjectAsync: TAction;
    actRunCurrentProjectAsync: TAction;
    actDebug: TAction;
    actStepInto: TAction;
    actStepOver: TAction;
    actStepOut: TAction;
    actPause: TAction;
    actStop: TAction;
    procedure actUpdateEnvironmentExecute(Sender: TObject);
    procedure actUpdateCurrentProjectExecute(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure actBuildCurrentProjectExecute(Sender: TObject);
    procedure actDeployCurrentProjectExecute(Sender: TObject);
    procedure actRunCurrentProjectExecute(Sender: TObject);
    procedure actNewProjectExecute(Sender: TObject);
    procedure actOpenProjectExecute(Sender: TObject);
    procedure actRemoveCurrentProjectExecute(Sender: TObject);
    procedure actBuildCurrentProjectAsyncExecute(Sender: TObject);
    procedure actDeployCurrentProjectAsyncExecute(Sender: TObject);
    procedure actRunCurrentProjectAsyncExecute(Sender: TObject);
    procedure actlMenuUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actDebugExecute(Sender: TObject);
    procedure actPauseExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actStepOutExecute(Sender: TObject);
    procedure actStepOverExecute(Sender: TObject);
    procedure actStepIntoExecute(Sender: TObject);
  private
    //Async control
    FLoadingProject: integer;
    FAsyncOperationStartedEvent: IDisconnectable;
    FAsyncOperationEndedEvent: IDisconnectable;
    FDebugSessionEndedEvent: IDisconnectable;
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
    //Debugger
    FDebugger: IDebugServices;
    //Predicates
    function IsLoadingProject(): boolean; inline;
    function HasActiveProject(): boolean; inline;
    function IsDebugging(): boolean; inline;
    //Validators
    procedure CheckCurrentProject();
    procedure CheckActiveDevice();
    procedure UpdateModels();
    procedure DoBuildProject();
    procedure DoDeployProject();
    procedure DoRunProject();
    procedure DoDebugProject();
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  end;

var
  MenuActionsContainer: TMenuActionsContainer;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses
  System.Threading, System.UITypes, System.SyncObjs, System.Net.Socket,
  System.DateUtils,
  FMX.DialogService,
  Container.Images,
  Form.Factory,
  Builder.Storage.Factory,
  Builder.Services.Factory,
  Builder.Storage.Default;

const
  DEFAULT_HOST = '127.0.0.1';
  DEFAULT_PORT = 5678;

constructor TMenuActionsContainer.Create(AOwner: TComponent);
begin
  inherited;
  FDebugger := TServiceSimpleFactory.CreateDebug();
  FAsyncOperationStartedEvent := TGlobalBuilderChain.SubscribeToEvent<TAsyncOperationStartedEvent>(
    procedure(const AEventNotification: TAsyncOperationStartedEvent)
    begin
      case AEventNotification.Body.Operation of
        TAsyncOperation.OpenProject: TInterlocked.Add(FLoadingProject, 1);
      end;
    end);

  FAsyncOperationEndedEvent := TGlobalBuilderChain.SubscribeToEvent<TAsyncOperationEndedEvent>(
    procedure(const AEventNotification: TAsyncOperationEndedEvent)
    begin
      case AEventNotification.Body.Operation of
        TAsyncOperation.OpenProject: TInterlocked.Add(FLoadingProject, -1);
      end;
    end);

  FDebugSessionEndedEvent := TGlobalBuilderChain.SubscribeToEvent<TDebugSessionStoppedEvent>(
    procedure(const AEventNotification: TDebugSessionStoppedEvent)
    begin
      var LResult := TStringList.Create();
      try
        FAdbServices.StopDebugSession(FEnvironmentModel.AdbLocation, DEFAULT_PORT, LResult);
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

destructor TMenuActionsContainer.Destroy;
begin
  FDebugSessionEndedEvent.Disconnect();
  FAsyncOperationEndedEvent.Disconnect();
  FAsyncOperationStartedEvent.Disconnect();
  inherited;
end;

procedure TMenuActionsContainer.actBuildCurrentProjectAsyncExecute(
  Sender: TObject);
begin
  CheckCurrentProject();
  UpdateModels();
  
  TGlobalBuilderChain.BroadcastEvent(TMessageEvent.Create(true));
  TGlobalBuilderChain.BroadcastEvent(TAsyncOperationStartedEvent.Create(TAsyncOperation.BuildProject));

  TTask.Run(procedure begin
    try
      DoBuildProject();
      TGlobalBuilderChain.BroadcastEvent(TAsyncOperationEndedEvent.Create(TAsyncOperation.BuildProject));
    except
      on E: exception do begin
        TGlobalBuilderChain.BroadcastEvent(TAsyncOperationEndedEvent.Create(TAsyncOperation.BuildProject));
        TGlobalBuilderChain.BroadcastEvent(TAsyncExceptionEvent.Create());
      end;
    end;
  end);
end;

procedure TMenuActionsContainer.actBuildCurrentProjectExecute(Sender: TObject);
begin
  CheckCurrentProject();
  UpdateModels();
  DoBuildProject();
end;

procedure TMenuActionsContainer.actDebugExecute(Sender: TObject);
begin
  CheckCurrentProject();
  CheckActiveDevice();
  UpdateModels();

  TGlobalBuilderChain.BroadcastEvent(TMessageEvent.Create(true));
  TGlobalBuilderChain.BroadcastEvent(TAsyncOperationStartedEvent.Create(TAsyncOperation.DebugProject));

  TTask.Run(procedure begin
    try
      DoDebugProject();
      TGlobalBuilderChain.BroadcastEvent(TAsyncOperationEndedEvent.Create(TAsyncOperation.DebugProject));
    except
      on E: Exception do begin

        TGlobalBuilderChain.BroadcastEvent(TAsyncOperationEndedEvent.Create(TAsyncOperation.DebugProject));
        TGlobalBuilderChain.BroadcastEvent(TAsyncExceptionEvent.Create());
      end;
    end;
  end);
end;

procedure TMenuActionsContainer.actDeployCurrentProjectAsyncExecute(
  Sender: TObject);
begin
  CheckCurrentProject();
  CheckActiveDevice();
  UpdateModels();

  TGlobalBuilderChain.BroadcastEvent(TMessageEvent.Create(true));
  TGlobalBuilderChain.BroadcastEvent(TAsyncOperationStartedEvent.Create(TAsyncOperation.DeployProject));

  TTask.Run(procedure begin
    try
      DoDeployProject();
      TGlobalBuilderChain.BroadcastEvent(TAsyncOperationEndedEvent.Create(TAsyncOperation.DeployProject));
    except
      on E: Exception do begin
        TGlobalBuilderChain.BroadcastEvent(TAsyncOperationEndedEvent.Create(TAsyncOperation.DeployProject));
        TGlobalBuilderChain.BroadcastEvent(TAsyncExceptionEvent.Create());
      end;
    end;
  end);
end;

procedure TMenuActionsContainer.actDeployCurrentProjectExecute(Sender: TObject);
begin
  CheckCurrentProject();
  CheckActiveDevice();
  UpdateModels();
  DoDeployProject();
end;

procedure TMenuActionsContainer.actlMenuUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  //Entities
  actUpdateEnvironment.Enabled := not IsLoadingProject() and not IsDebugging();
  actUpdateCurrentProject.Enabled := HasActiveProject() and not IsLoadingProject() and not IsDebugging();
  //Project
  actNewProject.Enabled := not IsLoadingProject() and not IsDebugging();
  actOpenProject.Enabled := not IsLoadingProject() and not IsDebugging();
  actRemoveCurrentProject.Enabled := not IsLoadingProject() and not IsDebugging();
  //Build
  actBuildCurrentProject.Enabled := HasActiveProject() and not IsLoadingProject() and not IsDebugging();
  actDeployCurrentProject.Enabled := HasActiveProject() and not IsLoadingProject() and not IsDebugging();
  actRunCurrentProject.Enabled := HasActiveProject() and not IsLoadingProject() and not IsDebugging();
  //Build Async
  actBuildCurrentProjectAsync.Enabled := HasActiveProject() and not IsLoadingProject() and not IsDebugging();
  actDeployCurrentProjectAsync.Enabled := HasActiveProject() and not IsLoadingProject() and not IsDebugging();
  actRunCurrentProjectAsync.Enabled := HasActiveProject() and not IsLoadingProject() and not IsDebugging();
  //Debug
  actDebug.Enabled := HasActiveProject and not IsLoadingProject() and not IsDebugging();
  actStepInto.Enabled := IsDebugging();
  actStepOver.Enabled := IsDebugging();
  actStepOut.Enabled := IsDebugging();
  actPause.Enabled := (FDebugger.Status in [TDebuggerStatus.Started]);
  actStop.Enabled := (FDebugger.Status in [TDebuggerStatus.Started]);
end;

procedure TMenuActionsContainer.actNewProjectExecute(Sender: TObject);
begin
  var LProjectName: string;
  var LCreateMainFile: boolean;
  if TProjectCreateForm.CreateProject(LProjectName, LCreateMainFile) then begin
    FProjectModel := FProjectServices.CreateProject(LProjectName, LCreateMainFile);
    FProjectServices.SaveProject(FProjectModel);
    TGlobalBuilderChain.BroadcastEvent(TOpenProjectEvent.Create(FProjectModel), true, false);
  end;
end;

procedure TMenuActionsContainer.actOpenProjectExecute(Sender: TObject);
begin
  var LProjects := FProjectServices.ListProjects();
  if Length(LProjects) > 0 then begin
    var LSelected := TSelectProjectForm.Select(LProjects);
    if not LSelected.IsEmpty() then begin
      FProjectModel := FProjectServices.LoadProject(LSelected);
      TGlobalBuilderChain.BroadcastEvent(TOpenProjectEvent.Create(FProjectModel), true, false);
    end;
  end else
    raise Exception.Create('Your workspace is empty. Try to create a new project.');
end;

procedure TMenuActionsContainer.actPauseExecute(Sender: TObject);
begin
  FDebugger.Pause();
end;

procedure TMenuActionsContainer.actRemoveCurrentProjectExecute(Sender: TObject);
begin
  CheckCurrentProject();

  var LRemove := true;
  if not IsConsole then
    TDialogService.MessageDialog('Do you really want to remove this project?',
      TMsgDlgType.mtConfirmation,
      [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbNo, -1,
      procedure(const AResult: TModalResult) begin
        LRemove := AResult = mrYes;
      end);

  if not LRemove then
    Exit;

  FProjectServices.RemoveProject(FProjectModel.ProjectName);
  TGlobalBuilderChain.BroadcastEvent(TCloseProjectEvent.Create(FProjectModel));
end;

procedure TMenuActionsContainer.actRunCurrentProjectAsyncExecute(
  Sender: TObject);
begin
  CheckCurrentProject();
  CheckActiveDevice();
  UpdateModels();

  TGlobalBuilderChain.BroadcastEvent(TMessageEvent.Create(true));
  TGlobalBuilderChain.BroadcastEvent(TAsyncOperationStartedEvent.Create(TAsyncOperation.RunProject));

  TTask.Run(procedure begin
    try
      DoRunProject();
      TGlobalBuilderChain.BroadcastEvent(TAsyncOperationEndedEvent.Create(TAsyncOperation.RunProject));
    except
      on E: Exception do begin
        TGlobalBuilderChain.BroadcastEvent(TAsyncOperationEndedEvent.Create(TAsyncOperation.RunProject));
        TGlobalBuilderChain.BroadcastEvent(TAsyncExceptionEvent.Create());
      end;
    end;
  end);
end;

procedure TMenuActionsContainer.actRunCurrentProjectExecute(Sender: TObject);
begin
  CheckCurrentProject();
  CheckActiveDevice();
  UpdateModels();
  DoRunProject();
end;

procedure TMenuActionsContainer.actStepIntoExecute(Sender: TObject);
begin
  FDebugger.StepIn();
end;

procedure TMenuActionsContainer.actStepOutExecute(Sender: TObject);
begin
  FDebugger.StepOut();
end;

procedure TMenuActionsContainer.actStepOverExecute(Sender: TObject);
begin
  FDebugger.Next();
end;

procedure TMenuActionsContainer.actStopExecute(Sender: TObject);
begin
  FDebugger.Stop();
end;

procedure TMenuActionsContainer.actUpdateCurrentProjectExecute(Sender: TObject);
begin
  CheckCurrentProject();
  var LForm := TFormSimpleFactory.CreateProject();
  try
    LForm.Id := FProjectServices.GetActiveProject().Id;
    TFormSlider.ShowModal(Application.MainForm, LForm);
  finally
    LForm.Free();
  end;
end;

procedure TMenuActionsContainer.actUpdateEnvironmentExecute(Sender: TObject);
begin
  var LForm := TFormSimpleFactory.CreateEnvironment();
  try
    TFormSlider.ShowModal(Application.MainForm, LForm);
  finally
    LForm.Free();
  end;
end;

procedure TMenuActionsContainer.CheckActiveDevice;
begin
  if FAdbServices.ActiveDevice.IsEmpty() then
    raise Exception.Create('No device selected.');
end;

procedure TMenuActionsContainer.CheckCurrentProject;
begin
  if not Assigned(FProjectServices.GetActiveProject()) then
    raise Exception.Create('Open/Create a project before continue.');
end;

procedure TMenuActionsContainer.DataModuleCreate(Sender: TObject);
begin
  FProjectServices := TServiceSimpleFactory.CreateProject();
  FAppServices := TServiceSimpleFactory.CreateApp();
  FAdbServices := TServiceSimpleFactory.CreateAdb();
  FEnvironmentStorage := TDefaultStorage<TEnvironmentModel>.Make();
  FProjectStorage := TDefaultStorage<TProjectModel>.Make();
end;

procedure TMenuActionsContainer.DoBuildProject;
begin
  //Generates the project necessary files and settings
  FAppServices.BuildProject(FProjectServices.GetActiveProject());
  //Creates and signs the APK file
  if not FAppServices.BuildApk(FProjectModel, FEnvironmentModel) then
    raise Exception.Create('Build process failed. Check log for details.');
  TGlobalBuilderChain.BroadcastEvent(TMessageEvent.Create('Build process done.'));
end;

procedure TMenuActionsContainer.DoDebugProject;
begin
  //DoDeployProject();
  //Launch app on device and debug the Python script
  var LResult := TStringList.Create();
  try
    FAdbServices.DebugApp(
      FEnvironmentModel.AdbLocation,
      FProjectModel.PackageName,
      FAdbServices.ActiveDevice,
      DEFAULT_HOST,
      DEFAULT_PORT,
      LResult);
    FAdbServices.StartDebugSession(FEnvironmentModel.AdbLocation, DEFAULT_PORT, LResult);
    try
      FDebugger.Start(DEFAULT_HOST, DEFAULT_PORT);
    except
      on E: Exception do begin
        FAdbServices.StopDebugSession(FEnvironmentModel.AdbLocation, DEFAULT_PORT, LResult);
        raise;
      end;
    end;
  finally
    LResult.Free();
  end;
end;

procedure TMenuActionsContainer.DoDeployProject;
begin
  DoBuildProject();
  //Installs the APK on the device
  FAppServices.UnInstallApk(FProjectModel, FEnvironmentModel, FAdbServices.ActiveDevice);
  if not FAppServices.InstallApk(FProjectModel, FEnvironmentModel, FAdbServices.ActiveDevice) then
    raise Exception.Create('Install process failed. Check log for details.');
end;

procedure TMenuActionsContainer.DoRunProject;
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

function TMenuActionsContainer.HasActiveProject: boolean;
begin
  Result := Assigned(FProjectServices.GetActiveProject());
end;

function TMenuActionsContainer.IsDebugging: boolean;
begin
  Result := (FDebugger.Status in [TDebuggerStatus.Connecting, TDebuggerStatus.Started, TDebuggerStatus.Disconnecting]);
end;

function TMenuActionsContainer.IsLoadingProject: boolean;
begin
  Result := FLoadingProject <> 0;
end;

procedure TMenuActionsContainer.UpdateModels;
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

end.
