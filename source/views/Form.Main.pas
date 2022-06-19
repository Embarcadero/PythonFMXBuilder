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
  Frame.ScriptEditor, FMX.TreeView, Frame.Device, Frame.LeftMenu,
  Frame.EntityButtons, Frame.BuildButtons;

type
  TMainForm = class(TBaseForm, IServices, ILogServices, IDesignServices)
    loEditor: TLayout;
    loFooter: TLayout;
    spLog: TSplitter;
    rrSpliterGrip: TRoundRect;
    mmLog: TMemo;
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
    tbMenu: TToolBar;
    frmEntityButtons: TEntityButtonsFrame;
    frmBuildButtons: TBuildButtonsFrame;
    mmMenu: TMainMenu;
    miFile: TMenuItem;
    miProject: TMenuItem;
    miTools: TMenuItem;
    MenuItem1: TMenuItem;
    miUpdateEnvironment: TMenuItem;
    miBuild: TMenuItem;
    miDeploy: TMenuItem;
    miRun: TMenuItem;
    miUpdateProject: TMenuItem;
    miAddFile: TMenuItem;
    miRemoveFile: TMenuItem;
    miOpenProject: TMenuItem;
    miNewProject: TMenuItem;
    miRemoveProject: TMenuItem;
    miSetToMain: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure AboutClick(Sender: TObject);
    procedure tviDblClick(Sender: TObject);
    procedure loProjectOptionsResized(Sender: TObject);
  private
    FProjectServices: IProjectServices;
    //IServices implementation
    function GetLogServices(): ILogServices;
    function GetDesignServices(): IDesignServices;
    //ILogServices implementation
    procedure Clear();
    procedure Log(const AString: string);
    //IDesignServices implementation
    procedure BeginAsync();
    procedure EndAsync();
    procedure ShowException(const AException: Exception);
    procedure OpenProject(const AProjectModel: TProjectModel);
    procedure CloseProject(const AProjectModel: TProjectModel);
  end;

var
  MainForm: TMainForm;

implementation

uses
  System.IOUtils, System.Threading, FMX.DialogService,
  Container.Images, Container.Menu.Actions,
  Form.Factory, Form.Slider,
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

function TMainForm.GetDesignServices: IDesignServices;
begin
  Result := self;
end;

function TMainForm.GetLogServices: ILogServices;
begin
  Result := self;
end;

procedure TMainForm.Clear;
begin
  TThread.Synchronize(nil, procedure begin
    mmLog.Lines.Clear();
  end);
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

procedure TMainForm.BeginAsync;
begin
  TThread.Synchronize(nil, procedure begin
    frmLoading.StartAni();
  end);
end;

procedure TMainForm.EndAsync;
begin
  TThread.Synchronize(nil, procedure begin
    frmLoading.StopAni();
  end);
end;

procedure TMainForm.ShowException(const AException: Exception);
begin
  Application.ShowException(AException);
end;

procedure TMainForm.OpenProject(const AProjectModel: TProjectModel);
begin
  if not Assigned(AProjectModel) then
    Exit;

  frmProjectFiles.LoadProject(AProjectModel);
  frmScriptEditor.CloseAll();
  var LMainScript := frmProjectFiles.GetDefaultScriptFilePath();
  if TFile.Exists(LMainScript) then
    frmScriptEditor.OpenEditor(LMainScript);
end;

procedure TMainForm.CloseProject(const AProjectModel: TProjectModel);
begin

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
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  frmDevice.LoadDevices();
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
