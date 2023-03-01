program PythonFMXBuilderGUI;

uses
  System.StartUpCopy,
  FMX.Forms,
  Form.Main in 'source\views\Form.Main.pas' {MainForm},
  Form.Project in 'source\views\Form.Project.pas' {ProjectForm},
  Form.Environment in 'source\views\Form.Environment.pas' {EnvironmentForm},
  Container.Images in 'source\containers\Container.Images.pas' {ImageContainer: TDataModule},
  Form.Data in 'source\views\Form.Data.pas' {DataForm},
  Form.Factory in 'source\views\Form.Factory.pas',
  Form.Slider in 'source\views\Form.Slider.pas',
  Form.Base in 'source\views\Form.Base.pas' {BaseForm},
  Frame.ProjectFiles in 'source\views\frame\Frame.ProjectFiles.pas' {ProjectFilesFrame: TFrame},
  Frame.ProjectButtons in 'source\views\frame\Frame.ProjectButtons.pas' {ProjectButtonsFrame: TFrame},
  Frame.Loading in 'source\views\frame\Frame.Loading.pas' {LoadingFrame: TFrame},
  Form.SelectProject in 'source\views\Form.SelectProject.pas' {SelectProjectForm},
  Form.Project.Create in 'source\views\Form.Project.Create.pas' {ProjectCreateForm},
  Frame.Device in 'source\views\frame\Frame.Device.pas' {DeviceFrame: TFrame},
  Container.Menu.Actions in 'source\containers\Container.Menu.Actions.pas' {MenuActionsContainer: TDataModule},
  Frame.LeftMenu in 'source\views\frame\Frame.LeftMenu.pas' {LeftMenuFrame: TFrame},
  Frame.EntityButtons in 'source\views\frame\Frame.EntityButtons.pas' {EntityButtonsFrame: TFrame},
  Frame.BuildButtons in 'source\views\frame\Frame.BuildButtons.pas' {BuildButtonsFrame: TFrame},
  Frame.Debug.StackTrace in 'source\views\frame\debug\Frame.Debug.StackTrace.pas' {StackTraceDebugFrame: TFrame},
  Frame.Debug.LocalVariables in 'source\views\frame\debug\Frame.Debug.LocalVariables.pas' {LocalVariablesDebugFrame: TFrame},
  Frame.Debug.Threads in 'source\views\frame\debug\Frame.Debug.Threads.pas' {ThreadsDebugFrame: TFrame},
  Frame.Debug.Breakpoints in 'source\views\frame\debug\Frame.Debug.Breakpoints.pas' {BreakpointsDebugFrame: TFrame},
  Frame.Debug.Events in 'source\views\frame\debug\Frame.Debug.Events.pas' {EventsDebugFrame: TFrame},
  Frame.Debug in 'source\views\frame\debug\Frame.Debug.pas' {DebugFrame: TFrame},
  Frame.Debug.LeftPanel in 'source\views\frame\debug\Frame.Debug.LeftPanel.pas' {LeftPanelDebugFrame: TFrame},
  Frame.Debug.BottomPanel in 'source\views\frame\debug\Frame.Debug.BottomPanel.pas' {BottomPanelDebugFrame: TFrame},
  Frame.DebugButtons in 'source\views\frame\Frame.DebugButtons.pas' {DebugButtonsFrame: TFrame},
  Frame.LeftPanel in 'source\views\frame\Frame.LeftPanel.pas' {LeftPanelFrame: TFrame},
  Frame.BottomPanel in 'source\views\frame\Frame.BottomPanel.pas' {BottomPanelFrame: TFrame},
  Frame.Log in 'source\views\frame\Frame.Log.pas' {LogFrame: TFrame},
  Frame.Editor.Memo in 'source\views\frame\editor\Frame.Editor.Memo.pas' {MemoEditorFrame: TFrame},
  Frame.Editor.TabItem in 'source\views\frame\editor\Frame.Editor.TabItem.pas',
  Frame.Editor.Control in 'source\views\frame\editor\Frame.Editor.Control.pas' {EditorControlFrame: TFrame},
  Container.DataSet.Debugger in 'source\containers\Container.DataSet.Debugger.pas' {DebuggerDataSetContainer: TDataModule},
  Frame.SaveButtons in 'source\views\frame\Frame.SaveButtons.pas' {SaveButtonsFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TImageContainer, ImageContainer);
  Application.CreateForm(TDebuggerDataSetContainer, DebuggerDataSetContainer);
  Application.CreateForm(TMenuActionsContainer, MenuActionsContainer);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
