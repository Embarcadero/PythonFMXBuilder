unit Form.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Layouts, FMX.ListBox,
  FMX.StdCtrls, FMX.TabControl, System.Actions, FMX.ActnList, FMX.Ani,
  FMX.Objects, Form.Base, Builder.Chain, Builder.Services,
  Builder.Storage.Factory, Builder.Storage.Default,
  Builder.Model.Project, Builder.Model.Environment, Builder.Model, Frame.Loading, FMX.Menus,
  Frame.ProjectFiles, Frame.ProjectButtons, FMX.Styles.Objects,
  Frame.Editor.Control, FMX.TreeView, Frame.Device, Frame.LeftMenu,
  Frame.EntityButtons, Frame.BuildButtons, Frame.DebugButtons, Frame.LeftPanel,
  Frame.BottomPanel;

type
  TMainForm = class(TBaseForm)
    loEditor: TLayout;
    loFooter: TLayout;
    spLog: TSplitter;
    rrSpliterGrip: TRoundRect;
    frmLoading: TLoadingFrame;
    loMain: TLayout;
    frmProjectFiles: TProjectFilesFrame;
    spProjectFIles: TSplitter;
    RoundRect1: TRoundRect;
    loProjectOptions: TLayout;
    frmEditorControl: TEditorControlFrame;
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
    frmDebugButtons: TDebugButtonsFrame;
    spLeftPanel: TSplitter;
    RoundRect2: TRoundRect;
    frmLeftPanel: TLeftPanelFrame;
    frmBottomFrame: TBottomPanelFrame;
    loLeftPanel: TLayout;
    procedure AboutClick(Sender: TObject);
    procedure loProjectOptionsResized(Sender: TObject);
    procedure loLeftPanelResize(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    const
      ANI_OPS = [
        TAsyncOperation.OpenProject,
        TAsyncOperation.BuildProject,
        TAsyncOperation.DeployProject,
        TAsyncOperation.RunProject,
        TAsyncOperation.DebugProject];
  private
    FProjectServices: IProjectServices;
    FAsyncExcpetionEvent: IDisconnectable;
    FAsyncOperationStartedEvent: IDisconnectable;
    FAsyncOperationEndedEvent: IDisconnectable;
    FDebugSessionStarted: IDisconnectable;
    FDebugSessionStopped: IDisconnectable;
    FDebuggerConnectionFrozen: IDisconnectable;
    FBackgroundOperationCount: integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  end;

var
  MainForm: TMainForm;

implementation

uses
  System.IOUtils, System.Threading, System.SyncObjs, FMX.DialogService,
  Container.Images, Container.Menu.Actions,
  Form.Factory, Form.Slider,
  Builder.Services.Factory,
  Builder.Services.ADB;

{$R *.fmx}

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  FBackgroundOperationCount := 0;
  loLeftPanel.Visible := false;
  spLeftPanel.Visible := false;
  FProjectServices := TServiceSimpleFactory.CreateProject();

  FAsyncExcpetionEvent := TGlobalBuilderChain.SubscribeToEvent<TAsyncExceptionEvent>(
    procedure(const AEventNotification: TAsyncExceptionEvent)
    begin
      Application.ShowException(AEventNotification.Body.Exception);
    end);

  FAsyncOperationStartedEvent := TGlobalBuilderChain.SubscribeToEvent<TAsyncOperationStartedEvent>(
    procedure(const AEventNotification: TAsyncOperationStartedEvent)
    begin
      TInterlocked.Add(FBackgroundOperationCount, 1);
      if (AEventNotification.Body.Operation in ANI_OPS) then
        TThread.Queue(TThread.Current, procedure begin
          //frmLoading.StartAni();
        end);
    end);

  FAsyncOperationEndedEvent := TGlobalBuilderChain.SubscribeToEvent<TAsyncOperationEndedEvent>(
    procedure(const AEventNotification: TAsyncOperationEndedEvent)
    begin
      TInterlocked.Add(FBackgroundOperationCount, -1);
      if (AEventNotification.Body.Operation in ANI_OPS) then
        TThread.Queue(TThread.Current, procedure begin
          //frmLoading.StopAni();
        end);
    end);

  FDebugSessionStarted := TGlobalBuilderChain.SubscribeToEvent<TDebugSessionStartedEvent>(
    procedure(const AEventNotification: TDebugSessionStartedEvent)
    begin
      TThread.Queue(TThread.Current,
        procedure()
        begin
          loLeftPanel.Visible := true;
          spLeftPanel.Visible := true;
        end);
    end);

  FDebugSessionStopped := TGlobalBuilderChain.SubscribeToEvent<TDebugSessionStoppedEvent>(
    procedure(const AEventNotification: TDebugSessionStoppedEvent)
    begin
      TThread.Queue(TThread.Current,
        procedure()
        begin
          loLeftPanel.Visible := false;
          spLeftPanel.Visible := false;
        end);
    end);

  FDebuggerConnectionFrozen := TGlobalBuilderChain.SubscribeToReverseRequest<TDebuggerConnectionFrozenActionRequest>(
    function(const AReverseRequest: TDebuggerConnectionFrozenActionRequest; var AHandled: boolean): TChainResponse
    begin
      var LResult := TDebuggerConnectionFrozenAction.ForceDisconnection;
      TThread.Synchronize(nil,
        procedure()
        begin
          TDialogService.MessageDialog(
            'The debugger server is not responsive. Application might be frozen or still launching.'
            + sLineBreak
            + sLineBreak
            + 'Do you want to try again?',
            TMsgDlgType.mtConfirmation,
            [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo],
            TMsgDlgBtn.mbYes,
            -1,
            procedure(const AResult: TModalResult)
            begin
              if AResult = mrYes then
                LResult := TDebuggerConnectionFrozenAction.TryAgain;
            end);
        end);
      Result := TDebuggerConnectionFrozenActionResponse.Create(LResult);
      AHandled := true;
    end);
end;

destructor TMainForm.Destroy;
begin
  FDebuggerConnectionFrozen.Disconnect();
  FDebugSessionStopped.Disconnect();
  FDebugSessionStarted.Disconnect();
  FAsyncOperationEndedEvent.Disconnect();
  FAsyncOperationStartedEvent.Disconnect();
  FAsyncExcpetionEvent.Disconnect();
  inherited;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  inherited;
  CanClose := (FBackgroundOperationCount = 0);
  if not CanClose then
    ShowMessage('Wait for background operations to complete and try again.');
end;

procedure TMainForm.AboutClick(Sender: TObject);
begin
  // We should add some info of the app as pop up display on clicking this About button
  // We can add license details also
end;

procedure TMainForm.loLeftPanelResize(Sender: TObject);
begin
  inherited;
  if TControl(Sender).Width < 276 then
    TControl(Sender).Width := 276;
end;

procedure TMainForm.loProjectOptionsResized(Sender: TObject);
begin
  inherited;
  if TControl(Sender).Width < 276 then
    TControl(Sender).Width := 276;
end;

end.
