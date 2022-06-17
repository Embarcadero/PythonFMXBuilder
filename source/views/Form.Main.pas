unit Form.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Layouts, FMX.ListBox,
  FMX.StdCtrls, FMX.TabControl, System.Actions, FMX.ActnList, FMX.Ani,
  FMX.Objects, Form.Base, Builder.Services, Builder.Storage.Factory, Builder.Storage.Default,
  Builder.Model.Project, Builder.Model.Environment, Builder.Model, Frame.Loading, FMX.Menus,
  Frame.ProjectFiles, Frame.ProjectButtons, FMX.Styles.Objects,
  Frame.ScriptEditor, FMX.TreeView, Frame.Device;

type
  TMainForm = class(TBaseForm, IServices, ILogServices)
    loEditor: TLayout;
    pnlLeftMenu: TPanel;
    lbLeftMenu: TListBox;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    lbiEnvironment: TListBoxItem;
    ListBoxGroupHeader2: TListBoxGroupHeader;
    lbiProject: TListBoxItem;
    ListBoxGroupHeader3: TListBoxGroupHeader;
    lbiDeploy: TListBoxItem;
    loFooter: TLayout;
    spLog: TSplitter;
    rrSpliterGrip: TRoundRect;
    mmLog: TMemo;
    lbiBuild: TListBoxItem;
    frmLoading: TLoadingFrame;
    loMain: TLayout;
    frmProjectFiles: TProjectFilesFrame;
    spProjectFIles: TSplitter;
    RoundRect1: TRoundRect;
    loProjectOptions: TLayout;
    frmScriptEditor: TScriptEditorFrame;
    loEditorHeader: TLayout;
    frmProjectButtons: TProjectButtonsFrame;
    frmDevice: TDeviceFrame;
    procedure lbiEnvironmentClick(Sender: TObject);
    procedure lbiProjectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbiDeployClick(Sender: TObject);
    procedure lbiBuildClick(Sender: TObject);
    procedure AboutClick(Sender: TObject);
    procedure frmProjectButtonsbtnCreateClick(Sender: TObject);
    procedure frmProjectButtonsbtnOpenClick(Sender: TObject);
    procedure tviDblClick(Sender: TObject);
    procedure loProjectOptionsResized(Sender: TObject);
  private
    FProjectServices: IProjectServices;

    FEnvironmentModel: TEnvironmentModel;
    procedure CheckLoadedProject();
    function LoadModels(const AValidate: boolean = true): boolean;
    procedure LoadProject();
    function BuildApk(): boolean;

    procedure DoBuild();
    procedure DoDeploy();
  public
    procedure Log(const AString: string);
  end;

var
  MainForm: TMainForm;

implementation

uses
  System.IOUtils, System.Threading, FMX.DialogService,
  Container.Images, Form.Factory, Form.Slider,
  Builder.Services.Factory, Builder.Services.ADB;

const
  CMD_DETAILS_REF_LINK =
    'Check out for more command details:' + #13#10
  + 'http://delphi.org/2013/11/installing-and-running-android-apps-from-command-line/';

{$R *.fmx}

procedure TMainForm.AboutClick(Sender: TObject);
begin
  // We should add some info of the app as pop up display on clicking this About button
  // We can add license details also
end;

function TMainForm.BuildApk: boolean;
begin
  LoadModels(true);
  var LAppService := TServiceSimpleFactory.CreateApp();
  //Generates the project necessary files and settings
  LAppService.BuildProject(FProjectServices.GetActivetProject());
  //Creates and signs the APK file
  Result := LAppService.BuildApk(FProjectServices.GetActivetProject(), FEnvironmentModel);
end;

procedure TMainForm.CheckLoadedProject;
begin
  if not Assigned(FProjectServices.GetActivetProject()) then
    raise Exception.Create('Open/Create a project before continue.');
end;

procedure TMainForm.DoBuild;
begin
  mmLog.Lines.Clear();
  frmLoading.StartAni();
  TTask.Run(procedure begin
    try
      try
        var LResult := BuildApk();
        TThread.Synchronize(nil, procedure begin
          frmLoading.StopAni();
          if LResult then
            TDialogService.MessageDialog('Build process done.',
              TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, -1, nil)
          else
            raise Exception.Create('Build process failed. Check log for details.');
        end);
      finally
        TThread.Synchronize(nil, procedure begin
          frmLoading.StopAni();
        end);
      end;
    except
      on E: exception do begin
        Application.ShowException(E);
      end;
    end;
  end);
end;

procedure TMainForm.DoDeploy;
begin
  frmDevice.CheckSelectedDevice();
  mmLog.Lines.Clear();
  frmLoading.StartAni();
  TTask.Run(procedure begin
    try
      var LErrors := String.Empty;
      try
        var LAppService := TServiceSimpleFactory.CreateApp();
        //Create and sign the APK file
        if BuildApk() then begin
          var LProjectModel := FProjectServices.GetActivetProject();
          //Installs the APK on the device
          LAppService.UnInstallApk(LProjectModel, FEnvironmentModel,
            frmDevice.GetSelectedDeviceName());

          if LAppService.InstallApk(LProjectModel, FEnvironmentModel,
            frmDevice.GetSelectedDeviceName()) then
          begin
            var LAdbService := TServiceSimpleFactory.CreateAdb();
            var LResult := TStringList.Create();
            try
              LAdbService.RunApp(FEnvironmentModel.AdbLocation,
                LProjectModel.PackageName,
                frmDevice.GetSelectedDeviceName(), LResult);
            finally
              LResult.Free();
            end;
          end else
            LErrors := 'Install process failed. Check log for details.';
        end else
          LErrors := 'Build process failed. Check log for details.';
      finally
        TThread.Synchronize(nil, procedure begin
          frmLoading.StopAni();
          if not LErrors.IsEmpty() then
            raise Exception.Create(LErrors);
        end);
      end;
    except
      on E: Exception do begin
        Application.ShowException(E);
      end;
    end;
  end);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  GlobalServices := Self;
  FProjectServices := TServiceSimpleFactory.CreateProject();
  frmProjectFiles.OnScriptFileDblClick := tviDblClick;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  GlobalServices := nil;
  FEnvironmentModel.Free();
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  frmDevice.LoadDevices();
  LoadModels(false);
end;

procedure TMainForm.frmProjectButtonsbtnCreateClick(Sender: TObject);
begin
  inherited;
  frmProjectButtons.btnCreateClick(Sender);
  LoadProject();
end;

procedure TMainForm.frmProjectButtonsbtnOpenClick(Sender: TObject);
begin
  inherited;
  frmProjectButtons.btnOpenClick(Sender);
  LoadProject();
end;

procedure TMainForm.lbiEnvironmentClick(Sender: TObject);
begin
  var LForm := TFormSimpleFactory.CreateEnvironment();
  try
    TFormSlider.ShowModal(Self, LForm);
  finally
    LForm.Free();
  end;
end;

procedure TMainForm.lbiBuildClick(Sender: TObject);
begin
  inherited;
  CheckLoadedProject();
  DoBuild();
end;

procedure TMainForm.lbiDeployClick(Sender: TObject);
begin
  inherited;
  CheckLoadedProject();
  DoDeploy();
end;

procedure TMainForm.lbiProjectClick(Sender: TObject);
begin
  CheckLoadedProject();
  var LForm := TFormSimpleFactory.CreateProject();
  try
    LForm.Id := FProjectServices.GetActivetProject().Id;
    TFormSlider.ShowModal(Self, LForm);
    LoadModels(false);
    frmProjectFiles.LoadProject(FProjectServices.GetActivetProject());
  finally
    LForm.Free();
  end;
end;

function TMainForm.LoadModels(const AValidate: boolean): boolean;
begin
  var LEnvironmentStorage := TDefaultStorage<TEnvironmentModel>.Make();
  var LProjectStorage := TDefaultStorage<TProjectModel>.Make();

  if not LEnvironmentStorage.LoadModel(FEnvironmentModel) then
    if AValidate then
      raise Exception.Create('The Environment Settings are empty.')
    else
      Exit(false);

  var LProjectModel := FProjectServices.GetActivetProject();
  if not Assigned(LProjectModel)
    or not LProjectStorage.LoadModel(LProjectModel, String.Empty, LProjectModel.Id) then
      if AValidate then
        raise Exception.Create('The Project Settings are empty.')
      else
        Exit(false);

  if not AValidate then
    Exit(true);

  var LModelErrors := TStringList.Create();
  try
    if not FEnvironmentModel.Validate(LModelErrors) then
      raise EModelValidationError.Create('The Environment Settings has invalid arguments:'
        + sLineBreak
        + sLineBreak
        + LModelErrors.Text);

    if not LProjectModel.Validate(LModelErrors) then
      raise EModelValidationError.Create('The Project Settings has invalid arguments:'
        + sLineBreak
        + sLineBreak
        + LModelErrors.Text);
  finally
    LModelErrors.Free();
  end;

  Result := true;
end;

procedure TMainForm.LoadProject;
begin
  var LProjectModel := FProjectServices.GetActivetProject();
  if not Assigned(LProjectModel) then
    Exit;

  frmProjectFiles.LoadProject(LProjectModel);
  frmScriptEditor.CloseAll();
  var LMainScript := frmProjectFiles.GetDefaultScriptFilePath();
  if TFile.Exists(LMainScript) then
    frmScriptEditor.OpenEditor(LMainScript);
end;

procedure TMainForm.Log(const AString: string);
begin
  TThread.Synchronize(nil, procedure begin
    if mmLog.Lines.Count = 0 then begin
      mmLog.Lines.Add(CMD_DETAILS_REF_LINK);
      mmLog.Lines.Add(String.Empty);
    end;

    mmLog.Lines.Add(AString);
    mmLog.GoToTextEnd();
    mmLog.GoToLineBegin();
  end);
end;

procedure TMainForm.loProjectOptionsResized(Sender: TObject);
begin
  inherited;
  if loProjectOptions.Width < 276 then
    loProjectOptions.Width := 276;
end;

procedure TMainForm.tviDblClick(Sender: TObject);
begin
  inherited;
  frmScriptEditor.OpenEditor(
    frmProjectFiles.GetItemFilePath(TTreeViewItem(Sender)));
end;

end.
