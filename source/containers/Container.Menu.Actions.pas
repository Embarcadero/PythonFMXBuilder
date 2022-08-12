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
    actDebugCurrentProjectAsync: TAction;
    actStepIn: TAction;
    actStepOver: TAction;
    actStepOut: TAction;
    actPause: TAction;
    actStop: TAction;
    actContinue: TAction;
    procedure actUpdateEnvironmentExecute(Sender: TObject);
    procedure actUpdateCurrentProjectExecute(Sender: TObject);
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
    procedure actDebugCurrentProjectAsyncExecute(Sender: TObject);
    procedure actPauseExecute(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actStepOutExecute(Sender: TObject);
    procedure actStepOverExecute(Sender: TObject);
    procedure actStepInExecute(Sender: TObject);
    procedure actContinueExecute(Sender: TObject);
  private
    //Async control
    FLoadingProject: integer;
    FAsyncOperationStartedEvent: IDisconnectable;
    FAsyncOperationEndedEvent: IDisconnectable;
    //Models
    FProjectModel: TProjectModel;
    //Services
    FProjectServices: IProjectServices;
    FAppServices: IAppServices;
    FAdbServices: IAdbServices;
    //Storages
    FEnvironmentStorage: IStorage<TEnvironmentModel>;
    FProjectStorage: IStorage<TProjectModel>;
    //Builder
    FBuilder: IBuildServices;
    //Debugger
    FDebugger: IDebugServices;
    function IsLoadingProject(): boolean; inline;
    function HasActiveProject(): boolean; inline;
    function GetActionEnabledByTag(AAction: TBasicAction): boolean;
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

constructor TMenuActionsContainer.Create(AOwner: TComponent);
begin
  inherited;
  FProjectServices := TServiceSimpleFactory.CreateProject();
  FAppServices := TServiceSimpleFactory.CreateApp();
  FAdbServices := TServiceSimpleFactory.CreateAdb();
  FEnvironmentStorage := TDefaultStorage<TEnvironmentModel>.Make();
  FProjectStorage := TDefaultStorage<TProjectModel>.Make();
  FBuilder := TServiceSimpleFactory.CreateBuild();
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
end;

destructor TMenuActionsContainer.Destroy;
begin
  FAsyncOperationEndedEvent.Disconnect();
  FAsyncOperationStartedEvent.Disconnect();
  inherited;
end;

procedure TMenuActionsContainer.actBuildCurrentProjectAsyncExecute(
  Sender: TObject);
begin
  FBuilder.BuildActiveProjectAsync();
end;

procedure TMenuActionsContainer.actBuildCurrentProjectExecute(Sender: TObject);
begin
  FBuilder.BuildActiveProject();
end;

procedure TMenuActionsContainer.actDebugCurrentProjectAsyncExecute(Sender: TObject);
begin
  FBuilder.DebugActiveProjectAsync(FDebugger);
end;

procedure TMenuActionsContainer.actDeployCurrentProjectAsyncExecute(
  Sender: TObject);
begin
  FBuilder.DeployActiveProjectAsync();
end;

procedure TMenuActionsContainer.actDeployCurrentProjectExecute(Sender: TObject);
begin
  FBuilder.DeployActiveProject();
end;

procedure TMenuActionsContainer.actlMenuUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  if Action is TAction then
    TAction(Action).Enabled := GetActionEnabledByTag(Action);
end;

procedure TMenuActionsContainer.actNewProjectExecute(Sender: TObject);
begin
  var LProjectName: string;
  var LCreateMainFile: boolean;
  if TProjectCreateForm.CreateProject(LProjectName, LCreateMainFile) then begin
    FProjectModel := FProjectServices.CreateProject(LProjectName, LCreateMainFile);
    FProjectServices.SaveProject(FProjectModel);
    TGlobalBuilderChain.BroadcastEvent(TOpenProjectEvent.Create(FProjectModel));
  end;
end;

procedure TMenuActionsContainer.actOpenProjectExecute(Sender: TObject);
begin
  var LProjects := FProjectServices.ListProjects();
  if Length(LProjects) > 0 then begin
    var LSelected := TSelectProjectForm.Select(LProjects);
    if not LSelected.IsEmpty() then begin
      FProjectModel := FProjectServices.LoadProject(LSelected);
      TGlobalBuilderChain.BroadcastEvent(TOpenProjectEvent.Create(FProjectModel));
    end;
  end else
    raise Exception.Create('Your workspace is empty. Try to create a new project.');
end;

procedure TMenuActionsContainer.actRemoveCurrentProjectExecute(Sender: TObject);
begin
  FProjectServices.CheckActiveProject();

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
  TGlobalBuilderChain.BroadcastEventAsync(TCloseProjectEvent.Create(FProjectModel));
end;

procedure TMenuActionsContainer.actRunCurrentProjectAsyncExecute(
  Sender: TObject);
begin
  FBuilder.RunActiveProjectAsync();
end;

procedure TMenuActionsContainer.actRunCurrentProjectExecute(Sender: TObject);
begin
  FBuilder.RunActiveProject();
end;

procedure TMenuActionsContainer.actContinueExecute(Sender: TObject);
begin
  FDebugger.Continue();
end;

procedure TMenuActionsContainer.actPauseExecute(Sender: TObject);
begin
  FDebugger.Pause();
end;

procedure TMenuActionsContainer.actStepInExecute(Sender: TObject);
begin
  FDebugger.StepIn();
end;

procedure TMenuActionsContainer.actStepOutExecute(Sender: TObject);
begin
  FDebugger.StepOut();
end;

procedure TMenuActionsContainer.actStepOverExecute(Sender: TObject);
begin
  FDebugger.StepOver();
end;

procedure TMenuActionsContainer.actStopExecute(Sender: TObject);
begin
  FDebugger.Stop();
end;

procedure TMenuActionsContainer.actUpdateCurrentProjectExecute(Sender: TObject);
begin
  FProjectServices.CheckActiveProject();
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

function TMenuActionsContainer.GetActionEnabledByTag(
  AAction: TBasicAction): boolean;
begin
  var LPredicate1 := function(): boolean
  begin
    Result := not IsLoadingProject()
      and not FBuilder.IsBuilding
        and not FDebugger.IsDebugging
  end;

  var LPredicate2 := function(): boolean
  begin
    Result := HasActiveProject()
      and not IsLoadingProject()
        and not FBuilder.IsBuilding
          and not FDebugger.IsDebugging;
  end;

  var LPredicate3 := function(): boolean
  begin
    Result := FDebugger.CanStepIn();
  end;

  var LPredicate4 := function(): boolean
  begin
    Result := FDebugger.CanStepOver();
  end;

  var LPredicate5 := function(): boolean
  begin
    Result := FDebugger.CanStepOut();
  end;

  var LPredicate6 := function(): boolean
  begin
    Result := FDebugger.CanPause();
  end;

  var LPredicate7 := function(): boolean
  begin
    Result := FDebugger.CanStop();
  end;

  var LPredicate8 := function(): boolean
  begin
    Result := FDebugger.CanContinue();
  end;

  var LPredicate9 := function(): boolean
  begin
    Result := HasActiveProject()
      and not IsLoadingProject()
        and not FBuilder.IsBuilding
          and not FDebugger.IsDebugging;
  end;

  var LPredicate10 := function(): boolean
  begin
    Result := HasActiveProject()
      and not IsLoadingProject()
        and not FBuilder.IsBuilding
          and FDebugger.CanContinue();
  end;

  case AAction.Tag of
    0: Result := true;
    1: Result := LPredicate1();
    2: Result := LPredicate2();
    3: Result := LPredicate3();
    4: Result := LPredicate4();
    5: Result := LPredicate5();
    6: Result := LPredicate6();
    7: Result := LPredicate7();
    8: Result := LPredicate8();
    9: Result := LPredicate9();
   10: Result := LPredicate10();
    else Result := false;
  end;
end;

function TMenuActionsContainer.HasActiveProject: boolean;
begin
  Result := Assigned(FProjectServices.GetActiveProject());
end;

function TMenuActionsContainer.IsLoadingProject: boolean;
begin
  Result := (FLoadingProject <> 0);
end;

end.
