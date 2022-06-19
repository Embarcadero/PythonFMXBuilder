unit Container.Menu.Actions;

interface

uses
  System.SysUtils, System.Classes, System.Actions, FMX.ActnList,
  Form.Slider, FMX.Forms,
  Builder.Model.Project,
  Builder.Model.Environment,
  Builder.Services, Builder.Storage, Form.Project.Create, Form.SelectProject,
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
  private
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
    procedure CheckCurrentProject();
    procedure CheckActiveDevice();
    procedure UpdateModels();
    procedure DoBuildProject();
    procedure DoDeployProject();
    procedure DoRunProject();
  public
    { Public declarations }
  end;

var
  MenuActionsContainer: TMenuActionsContainer;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses
  System.Threading, System.UITypes,
  FMX.DialogService,
  Container.Images,
  Builder.Storage.Factory,
  Builder.Services.Factory,
  Form.Factory, Builder.Storage.Default;

procedure TMenuActionsContainer.actBuildCurrentProjectAsyncExecute(
  Sender: TObject);
begin
  CheckCurrentProject();
  UpdateModels();
  
  GlobalServices.LogServices.Clear();
  GlobalServices.DesignServices.BeginAsync();
  TTask.Run(procedure begin
    try
      DoBuildProject();
      GlobalServices.DesignServices.EndAsync();     
    except
      on E: exception do begin
        GlobalServices.DesignServices.EndAsync();
        GlobalServices.DesignServices.ShowException(E);
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

procedure TMenuActionsContainer.actDeployCurrentProjectAsyncExecute(
  Sender: TObject);
begin
  CheckCurrentProject();
  CheckActiveDevice();
  UpdateModels();
  
  GlobalServices.LogServices.Clear();
  GlobalServices.DesignServices.BeginAsync();
  TTask.Run(procedure begin
    try
      DoDeployProject();
      GlobalServices.DesignServices.EndAsync();
    except
      on E: Exception do begin
        GlobalServices.DesignServices.EndAsync();
        Application.ShowException(E);
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
  actUpdateCurrentProject.Enabled := Assigned(FProjectServices.GetActiveProject());
  actRemoveCurrentProject.Enabled := Assigned(FProjectServices.GetActiveProject());
  actBuildCurrentProject.Enabled := Assigned(FProjectServices.GetActiveProject());
  actDeployCurrentProject.Enabled := Assigned(FProjectServices.GetActiveProject());
  actRunCurrentProject.Enabled := Assigned(FProjectServices.GetActiveProject());
  actBuildCurrentProjectAsync.Enabled := Assigned(FProjectServices.GetActiveProject());
  actDeployCurrentProjectAsync.Enabled := Assigned(FProjectServices.GetActiveProject());
  actRunCurrentProjectAsync.Enabled := Assigned(FProjectServices.GetActiveProject());
end;

procedure TMenuActionsContainer.actNewProjectExecute(Sender: TObject);
begin
  var LProjectName: string;
  var LCreateMainFile: boolean;
  if TProjectCreateForm.CreateProject(LProjectName, LCreateMainFile) then begin
    FProjectModel := FProjectServices.CreateProject(LProjectName, LCreateMainFile);
    FProjectServices.SaveProject(FProjectModel);
    GlobalServices.DesignServices.OpenProject(FProjectModel);
  end;
end;

procedure TMenuActionsContainer.actOpenProjectExecute(Sender: TObject);
begin
  var LProjects := FProjectServices.ListProjects();
  if Length(LProjects) > 0 then begin
    var LSelected := TSelectProjectForm.Select(LProjects);
    if not LSelected.IsEmpty() then begin
      FProjectModel := FProjectServices.LoadProject(LSelected);
      GlobalServices.DesignServices.OpenProject(FProjectModel);
    end;
  end else
    raise Exception.Create('Your workspace is empty. Try to create a new project.');
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
  GlobalServices.DesignServices.CloseProject(FProjectModel);
end;

procedure TMenuActionsContainer.actRunCurrentProjectAsyncExecute(
  Sender: TObject);
begin
  CheckCurrentProject();
  CheckActiveDevice();
  UpdateModels();

  GlobalServices.LogServices.Clear();
  GlobalServices.DesignServices.BeginAsync();
  TTask.Run(procedure begin
    try
      DoRunProject();
      GlobalServices.DesignServices.EndAsync();
    except
      on E: Exception do begin
        GlobalServices.DesignServices.EndAsync();
        Application.ShowException(E);
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
  GlobalServices.LogServices.Log('Build process done.');
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
