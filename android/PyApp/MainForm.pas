(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'MainForm'     Copyright (c) 2021                        *)
(*                                                                        *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(**************************************************************************)
(*  Functionality:  MainForm of PyApp                                     *)
(*                                                                        *)
(**************************************************************************)
(* This source code is distributed with no WARRANTY, for no reason or use.*)
(* Everyone is allowed to use and change this code free for his own tasks *)
(* and projects, as long as this header and its copyright text is intact. *)
(* For changed versions of this code, which are public distributed the    *)
(* following additional conditions have to be fullfilled:                 *)
(* 1) The header has to contain a comment on the change and the author of *)
(*    it.                                                                 *)
(* 2) A copy of the changed source has to be sent to the above E-Mail     *)
(*    address or my then valid address, if this is possible to the        *)
(*    author.                                                             *)
(* The second condition has the target to maintain an up to date central  *)
(* version of the component. If this condition is not acceptable for      *)
(* confidential or legal reasons, everyone is free to derive a component  *)
(* or to generate a diff file to my or other original sources.            *)
(**************************************************************************)

unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.JSON, FMX.Forms, FMX.Memo.Types, System.Notification, PyEnvironment.AddOn,
  PyEnvironment.AddOn.EnsurePip, FMX.Layouts, PyEnvironment,
  PyEnvironment.Embeddable, PythonEngine, FMX.PythonGUIInputOutput, WrapDelphi,
  System.Actions, FMX.ActnList, FMX.Objects, FMX.StdCtrls, FMX.ScrollBox,
  FMX.Memo, FMX.Controls, FMX.Types, FMX.Controls.Presentation, Dependencies,
  FMX.Platform, WrapDelphiFmx;

type
  TPyMainForm = class(TForm)
    tbTop: TToolBar;
    PythonEngine1: TPythonEngine;
    ActionList1: TActionList;
    actRun: TAction;
    lblTop: TLabel;
    StyleBook1: TStyleBook;
    PyDelphiWrapper1: TPyDelphiWrapper;
    PythonModule1: TPythonModule;
    tbBottom: TToolBar;
    btnRun: TButton;
    mmMainScript: TMemo;
    mmOutput: TMemo;
    spInputOutput: TSplitter;
    RoundRect1: TRoundRect;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    PyEmbeddedEnvironment1: TPyEmbeddedEnvironment;
    loEditor: TLayout;
    NotificationCenter1: TNotificationCenter;
    PyEnvironmentAddOnEnsurePip1: TPyEnvironmentAddOnEnsurePip;
    procedure FormCreate(Sender: TObject);
    procedure actRunExecute(Sender: TObject);
  private
    FMainScriptPath: string;
    FFailure: boolean;
    FDebugging: boolean;
    FDebugHost: string;
    FDebugPort: integer;
    FLog: boolean;
    FInstaller: IInstallDependency;

    function AppEventHandler(AAppEvent: TApplicationEvent; AContext: TObject): boolean;
    procedure DisableComponents();
    procedure EnableComponents();
    procedure FailureState(const AError: string);
    procedure ReadArgs();
    procedure Initialize();
    procedure NotifyDebugSessionStarted();

    function TryToSetupPython(): boolean;
    function TryToActivatePython(): boolean;
    procedure IncludeAppModulesToPath();
    function TryToGetAppDefs(out AAppDefs: TJSONObject): boolean;
    function TryToGetMainFilePath(out AFilePath: string): boolean;
    function TryToInstallDependencies(): boolean;
    function TryToLoadMainScript(): boolean;
    procedure RunMainScript();
    procedure RunScript(const AScriptPath: string);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  PyMainForm: TPyMainForm;

implementation

uses
  System.IOUtils,
  AndroidApi.JniBridge, AndroidApi.Jni.App, AndroidApi.Jni.GraphicsContentViewText,
  FMX.Helpers.Android, Androidapi.JNI.JavaTypes, Androidapi.Helpers,
  FMX.Surfaces, FMX.Platform.Android, FMX.DialogService,
  VarPyth, Dependencies.Setup, Dependencies.Pip;

{$R *.fmx}

{ TPyMainForm }

procedure TPyMainForm.FailureState(const AError: string);
begin
  FFailure := true;
  TDialogService.ShowMessage(
    Format('%s.' + sLineBreak + sLineBreak + 'Application must quit.', [AError]),
    procedure(const AResult: TModalResult)
    begin
      Halt(1);
    end);
end;

procedure TPyMainForm.FormCreate(Sender: TObject);
begin
  FFailure := false;
  var LAppEventService := IFMXApplicationEventService(nil);
  if TPlatformServices.Current.SupportsPlatformService(
    IFMXApplicationEventService, IInterface(LAppEventService)) then
      LAppEventService.SetApplicationEventHandler(AppEventHandler)
  else begin
    Log.d('Platform service "IFMXApplicationEventService" not supported.');
    Halt(1);
  end;
  Initialize()
end;

procedure TPyMainForm.Initialize;
begin
  DisableComponents();
  ReadArgs();
  if TryToSetupPython() and TryToActivatePython() then begin
    IncludeAppModulesToPath();
    if TryToInstallDependencies() and TryToLoadMainScript() then begin
      EnableComponents();
    end;
  end;
end;

procedure TPyMainForm.NotifyDebugSessionStarted;
begin
  if NotificationCenter1.Supported then begin
    var LNotification := NotificationCenter1.CreateNotification();
    LNotification.Name := 'PyDebug';
    LNotification.Title := 'Python Debugger for Android';
    LNotification.AlertBody := Format('Debug session started at %s:%d', [FDebugHost, FDebugPort]);
    LNotification.FireDate := Now();
    NotificationCenter1.PresentNotification(LNotification);
  end;
end;

function TPyMainForm.TryToSetupPython: boolean;
begin
  Result := false;
  try
    PyEmbeddedEnvironment1.Setup();
    Result := true;
  except
    on E: Exception do
      FailureState('Failed setting up Python.');
  end;
end;

function TPyMainForm.TryToActivatePython: boolean;
begin
  Result := false;
  try
    PyEmbeddedEnvironment1.Activate();
    Result := true;
  except
    on E: Exception do
      FailureState('Failed activating Python.');
  end;
end;

function TPyMainForm.TryToGetAppDefs(out AAppDefs: TJSONObject): boolean;
begin
  Result := false;
  var LAppDefsFilePath := TPath.Combine(TPath.GetDocumentsPath(), 'app_defs.json');
  if TFile.Exists(LAppDefsFilePath) then begin
    AAppDefs := TJSONValue.ParseJSONValue(
      TFile.ReadAllText(LAppDefsFilePath, TEncoding.UTF8)) as TJSONobject;
    Result := Assigned(AAppDefs);
  end else
    FailureState('Configuration file not found.');
end;

function TPyMainForm.TryToGetMainFilePath(out AFilePath: string): boolean;
var
  LAppDefs: TJSONobject;
begin
  Result := TryToGetAppDefs(LAppDefs);
  if Result then begin
    try
      AFilePath := TPath.Combine(
        TPath.GetDocumentsPath(), LAppDefs.GetValue<string>('main_file', ''));
      Result := TFile.Exists(AFilePath);
      if not Result then
        FailureState('Main script not found.');
    finally
      LAppDefs.Free();
    end;
  end;
end;

function TPyMainForm.TryToLoadMainScript: boolean;
begin
  Result := TryToGetMainFilePath(FMainScriptPath);
  if Result then
    mmMainScript.Lines.LoadFromFile(FMainScriptPath);
end;

function TPyMainForm.TryToInstallDependencies: boolean;
var
  LAppDefs: TJSONObject;
begin
  Result := TryToGetAppDefs(LAppDefs);
  if Result then begin
    try
      var LDependencies := LAppDefs.GetValue('dependencies') as TJSONArray;
      if Assigned(LDependencies) then
        for var LDependency in LDependencies do begin
          var LModuleName := (LDependency as TJSONObject).GetValue<string>('module_name');
          var LFileName := (LDependency as TJSONObject).GetValue<string>('file_name');
          var LFilePath := TPath.Combine(TPath.GetDocumentsPath(), LFileName);
          if not FInstaller.IsInstalled(LModuleName, LFilePath) then
            if not FInstaller.Install(LModuleName, LFilePath) then begin
              FailureState(Format('Failed to install %s.', [LFileName]));
              Exit(false);
            end else if TFile.Exists(LFilePath)  then
              TFile.Delete(LFilePath);
        end;
    finally
      LAppDefs.Free();
    end;
  end;
end;

procedure TPyMainForm.IncludeAppModulesToPath;
begin
  PythonEngine1.ExecString(AnsiString(Format(
    'import sys'
    + #13#10 +
    'sys.path.append("%s")'
    + #13#10,
    [TPath.GetDocumentsPath()])));
end;

procedure TPyMainForm.ReadArgs;

  function GetValue(const AArgs, AKey: string): string;
  begin
    Result := String.Empty;
    var LKey := AKey + ' ';
    var LIdx := AArgs.IndexOf(LKey);
    if (LIdx > -1) then begin
      LIdx := LIdx + LKey.Length;

      var LEnd := Pos(' ', AArgs, LIdx + 1) - 1;
      if (LEnd <= 0) then
        LEnd := AArgs.Length;

      Result := AArgs.Substring(LIdx, LEnd - (AArgs.IndexOf(LKey) + LKey.Length));
    end;
  end;

begin
  var LArgs := String.Empty;
  var LBundle := MainActivity.getIntent().getExtras();
  if Assigned(LBundle) then begin
    var LIterator := LBundle.keySet.iterator;
    while LIterator.hasNext() do begin
      var LKey := JString(LIterator.next());
      var LValue := JString(LBundle.get(LKey));
      LArgs := LArgs + ' ' + JStringToString(LKey);
      if Assigned(LValue) then
        LArgs := LArgs + ' ' + JStringToString(LValue);
    end;
  end;

  LArgs := LArgs.Trim();

  FDebugging := LArgs.Contains('--debugpy');
  if FDebugging then begin
    FDebugHost := GetValue(LArgs, '-host');
    if FDebugHost.IsEmpty() then
      FDebugHost := '127.0.0.1';
    FDebugPort := StrToIntDef(GetValue(LArgs, '-port'), 5678);
    FLog := LArgs.Contains('--log');
  end;
end;

procedure TPyMainForm.RunMainScript;
begin
  if FDebugging then begin
    NotifyDebugSessionStarted();
    try
      var LSubProcessEnv := NewPythonDict();
      LSubProcessEnv.SetItem('LD_LIBRARY_PATH', GetPythonEngine().DllPath);
      LSubProcessEnv.SetItem('PYTHONHOME', GetPythonEngine().PythonHome);
      LSubProcessEnv.SetItem('PATH', ExtractFileDir(GetPythonEngine().ProgramName));
      LSubProcessEnv.SetItem('TMPDIR', TPath.GetTempPath());

      var LDebugger := Import('debugpy');
      if FLog then
        LDebugger.log_to(TPath.GetDocumentsPath());
      LDebugger.configure(subProcessEnv := LSubProcessEnv);
      LDebugger.listen(VarPythonCreate([FDebugHost, FDebugPort], stTuple));
      LDebugger.wait_for_client();
      NotificationCenter1.CancelNotification('PyDebug');
    except
      on E: Exception do
        FailureState('Failed initializing debugger.');
    end;
  end;

  RunScript(FMainScriptPath);
end;

procedure TPyMainForm.RunScript(const AScriptPath: string);
begin
  var LRunPy := Import('runpy');
  LRunPy.run_path(AScriptPath, run_name := PythonEngine1.ExecModule);
end;

procedure TPyMainForm.actRunExecute(Sender: TObject);
begin
  mmMainScript.Lines.SaveToFile(FMainScriptPath);
  RunMainScript();
end;

function TPyMainForm.AppEventHandler(AAppEvent: TApplicationEvent;
  AContext: TObject): boolean;
begin
  case AAppEvent of
    TApplicationEvent.FinishedLaunching: begin
      if not FFailure then
        RunMainScript();
    end;
  end;
  Result := true;
end;

constructor TPyMainForm.Create(AOwner: TComponent);
begin
  inherited;
  FInstaller := TPipInstallStrategy.Create();
end;

procedure TPyMainForm.DisableComponents;
begin
  loEditor.Visible := false;
  btnRun.Enabled := false;
end;

procedure TPyMainForm.EnableComponents;
begin
  btnRun.Enabled := true;
  if FDebugging then
    loEditor.Visible := true;
end;

end.
