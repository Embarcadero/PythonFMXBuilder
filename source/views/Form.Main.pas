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
  Frame.ScriptEditor, FMX.TreeView;

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
    loDevice: TLayout;
    aiDevice: TAniIndicator;
    cbDevice: TComboBox;
    btnRefreshDevice: TSpeedButton;
    loEditorHeader: TLayout;
    frmProjectFiles: TProjectFilesFrame;
    spProjectFIles: TSplitter;
    RoundRect1: TRoundRect;
    loProjectOptions: TLayout;
    frmProjectButtons: TProjectButtonsFrame;
    frmScriptEditor: TScriptEditorFrame;
    procedure lbiEnvironmentClick(Sender: TObject);
    procedure lbiProjectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRefreshDeviceClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbiDeployClick(Sender: TObject);
    procedure lbiBuildClick(Sender: TObject);
    procedure AboutClick(Sender: TObject);
    procedure frmProjectButtonsbtnCreateClick(Sender: TObject);
    procedure frmProjectButtonsbtnOpenClick(Sender: TObject);
    procedure tviDblClick(Sender: TObject);
  private
    FDevices: TStrings;
    FEnvironmentModel: TEnvironmentModel;
    FProjectModel: TProjectModel;
    procedure LoadDevices();
    procedure CheckSelectedDevice();
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
  Container.Images, Form.Factory, Form.Slider, Builder.Services.Factory, Builder.Services.ADB;

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

procedure TMainForm.btnRefreshDeviceClick(Sender: TObject);
begin
  LoadDevices();
end;

function TMainForm.BuildApk: boolean;
begin
  LoadModels(true);

  var LAppService := TServiceSimpleFactory.CreateApp();
  //Generates the project necessary files and settings
  LAppService.BuildProject(FProjectModel);
  //Creates and signs the APK file
  Result := LAppService.BuildApk(FProjectModel, FEnvironmentModel);
end;

procedure TMainForm.CheckLoadedProject;
begin
  if not Assigned(FProjectModel) then
    raise Exception.Create('Open/Create a project before continue.');
end;

procedure TMainForm.CheckSelectedDevice;
begin
  if cbDevice.ItemIndex < 0 then
    raise Exception.Create('Select a device.');
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
  CheckSelectedDevice();
  mmLog.Lines.Clear();
  frmLoading.StartAni();
  TTask.Run(procedure begin
    try
      var LErrors := String.Empty;
      try
        var LAppService := TServiceSimpleFactory.CreateApp();
        //Create and sign the APK file
        if BuildApk() then begin
          //Install the APK on the device
          LAppService.UnInstallApk(FProjectModel, FEnvironmentModel, FDevices.Names[cbDevice.ItemIndex]);
          if LAppService.InstallApk(FProjectModel, FEnvironmentModel, FDevices.Names[cbDevice.ItemIndex]) then begin
            var LAdbService := TServiceSimpleFactory.CreateAdb();
            var LResult := TStringList.Create();
            try
              LAdbService.RunApp(FEnvironmentModel.AdbLocation, FProjectModel.PackageName,
                FDevices.Names[cbDevice.ItemIndex], LResult);
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
  FDevices := TStringList.Create();
  GlobalServices := Self;
  frmProjectButtons.ProjectRef := @FProjectModel;
  frmProjectFiles.OnScriptFileDblClick := tviDblClick;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  GlobalServices := nil;
  FDevices.Free();
  FEnvironmentModel.Free();
  FProjectModel.Free();
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  LoadDevices();
  LoadModels(false);
end;

procedure TMainForm.frmProjectButtonsbtnCreateClick(Sender: TObject);
begin
  inherited;
  var LBuff := FProjectModel;
  try
    frmProjectButtons.btnCreateClick(Sender);
  finally
    if (LBuff <> FProjectModel) then
      FreeAndNil(LBuff);
  end;

  if Assigned(FProjectModel) then
    LoadProject();
end;

procedure TMainForm.frmProjectButtonsbtnOpenClick(Sender: TObject);
begin
  inherited;
  var LBuff := FProjectModel;
  try
    frmProjectButtons.btnOpenClick(Sender);
  finally
    if (LBuff <> FProjectModel) then
      FreeAndNil(LBuff);
  end;

  if Assigned(FProjectModel) then
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
    LForm.Id := FProjectModel.Id;
    TFormSlider.ShowModal(Self, LForm);
    LoadModels(false);
    frmProjectFiles.LoadProject(FProjectModel);
  finally
    LForm.Free();
  end;
end;

procedure TMainForm.LoadDevices;
begin
  FDevices.Clear();
  cbDevice.Clear();
  aiDevice.Enabled := true;
  aiDevice.Visible := true;
  btnRefreshDevice.Enabled := false;
  TTask.Run(procedure begin
    var LStorage := TStorageSimpleFactory.CreateEnvironment();
    try
      var LService := TServiceSimpleFactory.CreateAdb();
      var LAdbPath := LStorage.GetAdbPath();

      if not LAdbPath.IsEmpty() then
        LService.ListDevices(LAdbPath, FDevices);
    finally
      TThread.Synchronize(nil, procedure begin
        for var I := 0 to FDevices.Count - 1 do
          cbDevice.Items.Add(FDevices.ValueFromIndex[I]);

        if (cbDevice.Count > 0)  then
          cbDevice.ItemIndex := 0;

        aiDevice.Enabled := false;
        aiDevice.Visible := false;
        btnRefreshDevice.Enabled := true;
      end);
    end;
  end);
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

  var LId := String.Empty;
  if Assigned(FProjectModel) then
    LId := FProjectModel.Id;

  if not LProjectStorage.LoadModel(FProjectModel, String.Empty, LId) then
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

    if not FProjectModel.Validate(LModelErrors) then
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
  frmProjectFiles.LoadProject(FProjectModel);
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

procedure TMainForm.tviDblClick(Sender: TObject);
begin
  inherited;
  frmScriptEditor.OpenEditor(
    frmProjectFiles.GetItemFilePath(TTreeViewItem(Sender)));
end;

end.
