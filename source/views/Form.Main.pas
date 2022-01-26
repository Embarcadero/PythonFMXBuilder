unit Form.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Layouts, FMX.ListBox,
  FMX.StdCtrls, FMX.TabControl, System.Actions, FMX.ActnList, FMX.Ani,
  FMX.Objects, Form.Base, Services, Storage.Factory, Storage.Default,
  Model.Project, Model.Environment, Model, Frame.Loading;

type
  TMainForm = class(TBaseForm, IServices, ILogServices)
    loEditor: TLayout;
    mmEditor: TMemo;
    pnlLeftMenu: TPanel;
    lbLeftMenu: TListBox;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    lbiEnvironment: TListBoxItem;
    ListBoxGroupHeader2: TListBoxGroupHeader;
    lbiProject: TListBoxItem;
    tbScripts: TTabControl;
    tiMainScript: TTabItem;
    ListBoxGroupHeader3: TListBoxGroupHeader;
    lbiDeploy: TListBoxItem;
    loDevice: TLayout;
    aiDevice: TAniIndicator;
    cbDevice: TComboBox;
    btnRefreshDevice: TSpeedButton;
    tbUpperMenu: TToolBar;
    loFooter: TLayout;
    spLog: TSplitter;
    rrSpliterGrip: TRoundRect;
    mmLog: TMemo;
    lbiBuild: TListBoxItem;
    frmLoading: TLoadingFrame;
    loMain: TLayout;
    procedure lbiEnvironmentClick(Sender: TObject);
    procedure lbiProjectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRefreshDeviceClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbiDeployClick(Sender: TObject);
    procedure lbiBuildClick(Sender: TObject);
  private
    FDevices: TStrings;
    FEnvironmentModel: TEnvironmentModel;
    FProjectModel: TProjectModel;
    procedure LoadDevices();
    procedure CheckSelectedDevice();
    function LoadModels(const AValidate: boolean = true): boolean;
    function BuildApk(): boolean;
  public
    procedure Log(const AString: string);
  end;

var
  MainForm: TMainForm;

implementation

uses
  System.Threading, FMX.DialogService,
  Container.Images, Form.Factory, Form.Slider, Services.Factory, Services.ADB;

{$R *.fmx}

procedure TMainForm.btnRefreshDeviceClick(Sender: TObject);
begin
  LoadDevices();
end;

function TMainForm.BuildApk: boolean;
begin
  LoadModels(true);

  var LAppService := TServiceSimpleFactory.CreateApp();
  //Copy Python and other APP files
  LAppService.CopyAppFiles(FProjectModel);
  //Save the main.py script to the APP files
  var LStream := TMemoryStream.Create();
  try
    mmEditor.Lines.SaveToStream(LStream);
    LAppService.AddScriptFile(FProjectModel, 'main.py', LStream);
  finally
    LStream.Free();
  end;
  //Update the manifest with the custom APP settings
  LAppService.UpdateManifest(FProjectModel);
  //Create and sign the APK file
  Result := LAppService.BuildApk(FProjectModel, FEnvironmentModel);
end;

procedure TMainForm.CheckSelectedDevice;
begin
  if cbDevice.ItemIndex < 0 then
    raise Exception.Create('Select a device.');
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FDevices := TStringList.Create();
  GlobalServices := Self;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  GlobalServices := nil;
  FDevices.Free();
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  LoadDevices();
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
  mmLog.Lines.Clear();

  frmLoading.Start();
  TTask.Run(procedure begin
    try
      var LResult := BuildApk();
      TThread.Synchronize(nil, procedure begin
        frmLoading.Stop();
        if LResult then
          TDialogService.MessageDialog('Build process done.',
            TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, -1, nil)
        else
          raise Exception.Create('Build process failed. Check log for details.');
      end);
    finally
      TThread.Synchronize(nil, procedure begin
        frmLoading.Stop();
      end);
    end;
  end);
end;

procedure TMainForm.lbiDeployClick(Sender: TObject);
begin
  inherited;
  CheckSelectedDevice();
  mmLog.Lines.Clear();

  var LAppService := TServiceSimpleFactory.CreateApp();

  frmLoading.Start();
  Application.ProcessMessages();
  TTask.Run(procedure begin
    try
      //Create and sign the APK file
      if BuildApk() then begin
        //Install the APK on the device
        if LAppService.InstallApk(FProjectModel, FEnvironmentModel, FDevices.Names[cbDevice.ItemIndex]) then begin
          var LAdbService := TServiceSimpleFactory.CreateAdb();
          var LResult := TStringList.Create();
          try
            LAdbService.RunApp(FEnvironmentModel.AdbLocation, FProjectModel.PackageName,
              FDevices.Names[cbDevice.ItemIndex], LResult);
          finally
            LResult.Free();
          end;
        end else begin
          TThread.Synchronize(nil, procedure begin
            frmLoading.Stop();
            raise Exception.Create('Install process failed. Check log for details.');
          end);
        end;
      end else begin
        TThread.Synchronize(nil, procedure begin
          frmLoading.Stop();
          raise Exception.Create('Build process failed. Check log for details.');
        end);
      end;
    finally
      TThread.Synchronize(nil, procedure begin
        frmLoading.Stop();
      end);
    end;
  end);
end;

procedure TMainForm.lbiProjectClick(Sender: TObject);
begin
  var LForm := TFormSimpleFactory.CreateProject();
  try
    TFormSlider.ShowModal(Self, LForm);
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
  var LProjectStorage := TDefaultStorage<TProjectModel>.Make();
  var LEnvironmentStorage := TDefaultStorage<TEnvironmentModel>.Make();

  if not LEnvironmentStorage.LoadModel(FEnvironmentModel) then
    if AValidate then
      raise Exception.Create('The Environment Settings are empty.')
    else
      Exit(false);

  if not LProjectStorage.LoadModel(FProjectModel) then
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

procedure TMainForm.Log(const AString: string);
begin
  TThread.Synchronize(nil, procedure begin
    mmLog.Lines.Add(AString);
    mmLog.GoToTextEnd();
    mmLog.GoToLineBegin();
  end);
end;

end.
