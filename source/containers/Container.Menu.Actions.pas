unit Container.Menu.Actions;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Actions,
  FMX.StdActns,
  FMX.ActnList,
  FMX.Forms,
  FMX.Types,
  FMX.Dialogs,
  FMX.Menus,
  Form.Slider,
  Form.SelectProject,
  Builder.Chain,
  Builder.Types,
  Builder.Model.Project,
  Builder.Model.Environment,
  Builder.Services,
  Builder.Model;

type
  TMenuActionsContainer = class(TDataModule)
    actlMenu: TActionList;
    actUpdateEnvironment: TAction;
    actUpdateCurrentProject: TAction;
    actBuildCurrentProject: TAction;
    actDeployCurrentProject: TAction;
    actOpenProject: TAction;
    actCloseCurrentProject: TAction;
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
    actSaveState: TAction;
    actSaveAllState: TAction;
    actFileExit: TFileExit;
    actFileHideApp: TFileHideApp;
    actFileHideAppOthers: TFileHideAppOthers;
    sdProject: TSaveDialog;
    sdModule: TSaveDialog;
    odProject: TOpenDialog;
    odModule: TOpenDialog;
    odPackage: TOpenDialog;
    odOther: TOpenDialog;
    odFMXModule: TOpenDialog;
    actNewBlankProject: TAction;
    actNewProject: TAction;
    procedure actUpdateEnvironmentExecute(Sender: TObject);
    procedure actUpdateCurrentProjectExecute(Sender: TObject);
    procedure actBuildCurrentProjectExecute(Sender: TObject);
    procedure actDeployCurrentProjectExecute(Sender: TObject);
    procedure actRunCurrentProjectExecute(Sender: TObject);
    procedure actOpenProjectExecute(Sender: TObject);
    procedure actCloseCurrentProjectExecute(Sender: TObject);
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
    procedure actSaveStateExecute(Sender: TObject);
    procedure actSaveAllStateExecute(Sender: TObject);
    procedure actNewBlankProjectExecute(Sender: TObject);
    procedure actNewProjectExecute(Sender: TObject);
  private
    //Async control
    FLoadingProject: integer;
    //Events
    FAsyncOperationStartedEvent: IDisconnectable;
    FAsyncOperationEndedEvent: IDisconnectable;
    FEditorSaveRequest: IDisconnectable;
    //Models
    FProjectModel: TProjectModel;
    //Services
    FEnvironmentServices: IEnvironmentServices;
    FProjectServices: IProjectServices;
    FAppServices: IAppServices;
    FEditorServices: IEditorServices;
    //Builder
    FBuilderServices: IBuildServices;
    //Debugger
    FDebuggerServices: IDebugServices;
    function IsLoadingProject(): boolean; inline;
    function HasActiveProject(): boolean; inline;
    function GetActionEnabledByTag(AAction: TBasicAction): boolean;
    procedure SaveProjectAndModules();
    function SaveEditor(const ATextEditor: ITextEditor): string; overload;
    function SaveEditor(): string; overload;
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
  System.Threading,
  System.UITypes,
  System.SyncObjs,
  System.Net.Socket,
  System.DateUtils,
  System.IOUtils,
  FMX.DialogService,
  Container.Images,
  Form.Factory,
  Builder.Paths;

constructor TMenuActionsContainer.Create(AOwner: TComponent);
begin
  inherited;
  FProjectServices := TBuilderService.CreateService<IProjectServices>;
  FAppServices := TBuilderService.CreateService<IAppServices>;
  FEnvironmentServices := TBuilderService.CreateService<IEnvironmentServices>;
  FBuilderServices := TBuilderService.CreateService<IBuildServices>;
  FDebuggerServices := TBuilderService.CreateService<IDebugServices>;
  FEditorServices := TBuilderService.CreateService<IEditorServices>;

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

  FEditorSaveRequest := TGlobalBuilderChain.SubscribeToReverseRequest<TEditorSaveRequest>(
    function(const AReverseRequest: TEditorSaveRequest; var AHandled: boolean): TChainResponse
    begin
      var LSavedTo := String.Empty;
      if Assigned(AReverseRequest.Arguments.TextEditor) then
        LSavedTo := SaveEditor(AReverseRequest.Arguments.TextEditor);

      AHandled := true;
      if TFile.Exists(LSavedTo) then
        Result := TEditorSaveResponse.Create(LSavedTo)
      else
        Result := TErrorResponse.Create('File not saved.');
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
  TGlobalBuilderChain.BroadcastEventAsync(TMessageEvent.Create(true));
  FBuilderServices.RunAsync(procedure(AProxy: IBuilderTasks) begin
    FProjectServices.GetActiveProject().Debugger := TDebugger.None;
    AProxy.BuildActiveProject();
  end);
end;

procedure TMenuActionsContainer.actBuildCurrentProjectExecute(Sender: TObject);
begin
  TGlobalBuilderChain.BroadcastEventAsync(TMessageEvent.Create(true));
  FBuilderServices.Run(procedure(AProxy: IBuilderTasks) begin
    FProjectServices.GetActiveProject().Debugger := TDebugger.None;
    AProxy.BuildActiveProject();
  end);
end;

procedure TMenuActionsContainer.actDebugCurrentProjectAsyncExecute(Sender: TObject);
begin
  TGlobalBuilderChain.BroadcastEventAsync(TMessageEvent.Create(true));
  FBuilderServices.RunAsync(procedure(AProxy: IBuilderTasks) begin
    FProjectServices.GetActiveProject().Debugger := TDebugger.DebugPy;
    AProxy.BuildActiveProject();
    AProxy.DeployActiveProject();
    AProxy.DebugActiveProject(FDebuggerServices);
  end);
end;

procedure TMenuActionsContainer.actDeployCurrentProjectAsyncExecute(
  Sender: TObject);
begin
  TGlobalBuilderChain.BroadcastEventAsync(TMessageEvent.Create(true));
  FBuilderServices.RunAsync(procedure(AProxy: IBuilderTasks) begin
    FProjectServices.GetActiveProject().Debugger := TDebugger.None;
    AProxy.BuildActiveProject();
    AProxy.DeployActiveProject();
  end);
end;

procedure TMenuActionsContainer.actDeployCurrentProjectExecute(Sender: TObject);
begin
  TGlobalBuilderChain.BroadcastEventAsync(TMessageEvent.Create(true));
  FBuilderServices.Run(procedure(AProxy: IBuilderTasks) begin
    FProjectServices.GetActiveProject().Debugger := TDebugger.None;
    AProxy.BuildActiveProject();
    AProxy.DeployActiveProject();
  end);
end;

procedure TMenuActionsContainer.actlMenuUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  if Action is TAction then
    TAction(Action).Enabled := GetActionEnabledByTag(Action);
end;

procedure TMenuActionsContainer.actNewBlankProjectExecute(Sender: TObject);
begin
  var LUntitledProject := TBuilderPaths.UntitledProject(
    TBuilderPaths.WorkspaceFolder());
  FProjectModel := FProjectServices.CreateProject(
    LUntitledProject, String.Empty);
  FProjectServices.SaveProject(FProjectModel);
  FProjectServices.OpenProject(FProjectModel);
end;

procedure TMenuActionsContainer.actNewProjectExecute(Sender: TObject);
begin
  var LUntitledProject := TBuilderPaths.UntitledProject(
    TBuilderPaths.WorkspaceFolder());
  var LUntitledModuleName := TBuilderPaths.RecommendModuleName(
    LUntitledProject);
  FProjectModel := FProjectServices.CreateProject(
    LUntitledProject, LUntitledModuleName);
  FProjectServices.SaveProject(FProjectModel);
  FProjectServices.OpenProject(FProjectModel);
end;

procedure TMenuActionsContainer.actOpenProjectExecute(Sender: TObject);
begin
  if odProject.Execute() then
    FProjectModel := FProjectServices.OpenProject(odProject.FileName);
end;

procedure TMenuActionsContainer.actCloseCurrentProjectExecute(Sender: TObject);
begin
  FProjectServices.CheckActiveProject();
  FProjectServices.CloseProject();
end;

procedure TMenuActionsContainer.actRunCurrentProjectAsyncExecute(
  Sender: TObject);
begin
  TGlobalBuilderChain.BroadcastEventAsync(TMessageEvent.Create(true));
  FBuilderServices.RunAsync(procedure(AProxy: IBuilderTasks) begin
    FProjectServices.GetActiveProject().Debugger := TDebugger.DebugPy;
    AProxy.BuildActiveProject();
    AProxy.DeployActiveProject();
    AProxy.RunActiveProject();
  end);
end;

procedure TMenuActionsContainer.actRunCurrentProjectExecute(Sender: TObject);
begin
  TGlobalBuilderChain.BroadcastEventAsync(TMessageEvent.Create(true));
  FBuilderServices.Run(procedure(AProxy: IBuilderTasks) begin
    FProjectServices.GetActiveProject().Debugger := TDebugger.None;
    AProxy.BuildActiveProject();
    AProxy.DeployActiveProject();
    AProxy.RunActiveProject();
  end);
end;

procedure TMenuActionsContainer.actContinueExecute(Sender: TObject);
begin
  FDebuggerServices.Continue();
end;

procedure TMenuActionsContainer.actPauseExecute(Sender: TObject);
begin
  FDebuggerServices.Pause();
end;

procedure TMenuActionsContainer.actSaveAllStateExecute(Sender: TObject);
begin
  SaveProjectAndModules();
  SaveEditor();

  TGlobalBuilderChain.BroadcastEventAsync(
    TSaveStateEvent.Create(TSaveState.SaveAll));
end;

procedure TMenuActionsContainer.actSaveStateExecute(Sender: TObject);
begin
  SaveEditor();

  TGlobalBuilderChain.BroadcastEventAsync(
    TSaveStateEvent.Create(TSaveState.Save));
end;

procedure TMenuActionsContainer.actStepInExecute(Sender: TObject);
begin
  FDebuggerServices.StepIn();
end;

procedure TMenuActionsContainer.actStepOutExecute(Sender: TObject);
begin
  FDebuggerServices.StepOut();
end;

procedure TMenuActionsContainer.actStepOverExecute(Sender: TObject);
begin
  FDebuggerServices.StepOver();
end;

procedure TMenuActionsContainer.actStopExecute(Sender: TObject);
begin
  FDebuggerServices.Stop();
end;

procedure TMenuActionsContainer.actUpdateCurrentProjectExecute(Sender: TObject);
begin
  FProjectServices.CheckActiveProject();
  var LForm := TFormSimpleFactory.CreateProject();
  try
    LForm.LoadModel(FProjectServices.GetActiveProject());
    TFormSlider.ShowModal(Application.MainForm, LForm);
  finally
    LForm.Free();
  end;
end;

procedure TMenuActionsContainer.actUpdateEnvironmentExecute(Sender: TObject);
begin
  var LForm := TFormSimpleFactory.CreateEnvironment();
  try
    LForm.LoadModel(FEnvironmentServices.GetActiveEnvironment());
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
      and not FBuilderServices.IsBuilding
        and not FDebuggerServices.IsDebugging
  end;

  var LPredicate2 := function(): boolean
  begin
    Result := HasActiveProject()
      and not IsLoadingProject()
        and not FBuilderServices.IsBuilding
          and not FDebuggerServices.IsDebugging;
  end;

  var LPredicate3 := function(): boolean
  begin
    Result := FDebuggerServices.CanStepIn();
  end;

  var LPredicate4 := function(): boolean
  begin
    Result := FDebuggerServices.CanStepOver();
  end;

  var LPredicate5 := function(): boolean
  begin
    Result := FDebuggerServices.CanStepOut();
  end;

  var LPredicate6 := function(): boolean
  begin
    Result := FDebuggerServices.CanPause();
  end;

  var LPredicate7 := function(): boolean
  begin
    Result := FDebuggerServices.CanStop();
  end;

  var LPredicate8 := function(): boolean
  begin
    Result := FDebuggerServices.CanContinue();
  end;

  var LPredicate9 := function(): boolean
  begin
    Result := HasActiveProject()
      and not IsLoadingProject()
        and not FBuilderServices.IsBuilding
          and not FDebuggerServices.IsDebugging;
  end;

  var LPredicate10 := function(): boolean
  begin
    Result := HasActiveProject()
      and not IsLoadingProject()
        and not FBuilderServices.IsBuilding
          and FDebuggerServices.CanContinue();
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

procedure TMenuActionsContainer.SaveProjectAndModules;
begin
  var LProject := FProjectServices.GetActiveProject();
  if not Assigned(LProject) then
    Exit;

  //Save untracked project only
  FProjectServices.SaveProject(LProject,
    function(AProjectName: string): string begin
      sdProject.InitialDir := TBuilderPaths.WorkspaceFolder();
      sdProject.FileName := AProjectName;
      if not sdProject.Execute then
        Result := String.Empty
      else
        Result := sdProject.FileName;
    end,
    true);

  //Save untracked modules only
  FProjectServices.SaveModules(LProject,
    function(AFileName: string): string begin
      sdModule.InitialDir := TPath.GetDirectoryName(
        LProject.Defs.Storage);
      sdModule.FileName := AFileName;
      if not sdModule.Execute then
        Result := String.Empty
      else
        Result := sdModule.FileName;
    end,
    true);

  FProjectServices.SaveProject(LProject);
end;

function TMenuActionsContainer.SaveEditor(const ATextEditor: ITextEditor): string;
begin
  Assert(Assigned(ATextEditor), 'Argument "ATextEditor" not assigned.');

  var LProject := FProjectServices.GetActiveProject();
  if not Assigned(LProject) then
    Exit(String.Empty);

  ATextEditor.Save();

  //Search for the module that represents the editor
  var LModule := FProjectServices.GetModule(LProject, ATextEditor.FileName);
  if not Assigned(LModule) then
    Exit(String.Empty);

  var LResult := ATextEditor.FileName;
  FProjectServices.SaveModule(LProject, LModule,
    function(AFileName: string): string begin
      sdModule.InitialDir := TPath.GetDirectoryName(
        LProject.Defs.Storage);
      sdModule.FileName := AFileName;
      if not sdModule.Execute then
        Result := String.Empty
      else
        Result := sdModule.FileName;

      LResult := Result;
    end,
    true);

  FProjectServices.SaveProject(LProject);

  Result := LResult;
end;

function TMenuActionsContainer.SaveEditor: string;
begin
  var LEditor := FEditorServices.ActiveTextEditor;
  if not Assigned(LEditor) then
    Exit;

  Result := SaveEditor(LEditor);
end;

end.
