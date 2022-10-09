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
  System.JSON, FMX.Forms, FMX.Memo.Types, System.Notification,
  FMX.Layouts, PyEnvironment,
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
    procedure actRunExecute(Sender: TObject);
  private
    const
      DEBUGGER_SCRIPT_NAME = 'debug.py';
      DEFAULT_DEBUGGER_HOST = '127.0.0.1';
      DEFAULT_DEBUGGER_PORT = 5678;
  private
    FAppDefs: TJSONObject;
    FFailure: boolean;
    FFailureMessage: string;
    FDebugging: boolean;
    FDebugHost: string;
    FDebugPort: integer;
    FRunMainScript: boolean;
    FLog: boolean;
    FInstaller: IInstallDependency;
    procedure Log(const AMsg: string; const AArgs: array of const);
    function AppEventHandler(AAppEvent: TApplicationEvent; AContext: TObject): boolean;
    procedure DisableComponents();
    procedure EnableComponents();
    procedure ReadArgs();
    procedure Initialize();
    procedure NotifyDebugSessionStarted();
    procedure ConfigureEngine();
    procedure SetupPython();
    procedure ActivatePython();
    procedure IncludeAppModulesToPath();
    function GetAppDefs(): TJSONObject;
    function GetDependencies(): TJSONArray;
    function GetMainFilePath(): string;
    procedure InstallDependencies();
    procedure LoadMainScript();
    procedure RunDebugScript();
    procedure RunMainScript();
    procedure RunScript(const AScriptPath: string);
    procedure SafeExec(const AStatement: TProc);
    procedure RaiseGeneralFailure(const AError: string);
    procedure CheckState();
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  end;

var
  PyMainForm: TPyMainForm;

implementation

uses
  System.IOUtils, TypInfo,
  AndroidApi.JniBridge, AndroidApi.Jni.App, AndroidApi.Jni.GraphicsContentViewText,
  FMX.Helpers.Android, Androidapi.JNI.JavaTypes, Androidapi.Helpers, Androidapi.Log,
  FMX.Surfaces, FMX.Platform.Android, FMX.DialogService,
  VarPyth, Dependencies.SysPath;

{$R *.fmx}

type
  EGeneralFailure = class(Exception);

{ TPyMainForm }

constructor TPyMainForm.Create(AOwner: TComponent);
begin
  inherited;       
  Log('TPyMainForm.Create', []);
  FFailure := false;
  FFailureMessage := String.Empty;

  var LAppEventService := IFMXApplicationEventService(nil);
  if TPlatformServices.Current.SupportsPlatformService(
    IFMXApplicationEventService, IInterface(LAppEventService)) then
      LAppEventService.SetApplicationEventHandler(AppEventHandler)
  else begin
    Log('Platform service "IFMXApplicationEventService" not supported.', []);
    Halt(1);
  end;

  SafeExec(Initialize);
end;

destructor TPyMainForm.Destroy;
begin
  FAppDefs.Free();
  inherited;
end;

procedure TPyMainForm.SafeExec(const AStatement: TProc);
begin
  CheckState();
  try
    if Assigned(AStatement) then
      AStatement();
  except
    on E: Exception do begin
      FFailure := true;
      FFailureMessage := E.ToString();
    end;
  end;
end;

procedure TPyMainForm.RaiseGeneralFailure(const AError: string);
begin
  Log('TPyMainForm.FailureState - Error = %s', [AError]);
  var LException := EGeneralFailure.Create(AError);
  if Assigned(ExceptObject()) and (ExceptObject() is Exception) then
    Exception.RaiseOuterException(LException)
  else
    raise LException;
end;

procedure TPyMainForm.Log(const AMsg: string; const AArgs: array of const);
var
  LMarshaller: TMarshaller;
begin
  LOGI(LMarshaller.AsUtf8(Format(AMsg, AArgs)).ToPointer);
end;

procedure TPyMainForm.CheckState;
begin
  Log('TPyMainForm.CheckState', []);
  if FFailure then
    TDialogService.ShowMessage(
      Format(
        '%s.'
        + sLineBreak
        + sLineBreak
        + 'Application must quit.', [FFailureMessage]),
      procedure(const AResult: TModalResult)
      begin
        TAndroidHelper.Activity.finish();
      end);
end;

function TPyMainForm.AppEventHandler(AAppEvent: TApplicationEvent;
  AContext: TObject): boolean;
begin
  Log('TPyMainForm.AppEventHandler - Event = %s', [
    GetEnumName(TypeInfo(TApplicationEvent), Ord(AAppEvent))]);
  case AAppEvent of
    TApplicationEvent.FinishedLaunching: begin
      SafeExec(RunMainScript);
      CheckState();
    end;
  end;
  Result := true;
end;

procedure TPyMainForm.Initialize;
begin
  Log('TPyMainForm.Initialize', []);
  FAppDefs := GetAppDefs();
  FInstaller :=  TSysPathInstallStrategy.Create(GetDependencies());
  DisableComponents();
  ReadArgs();
  ConfigureEngine();
  SetupPython();
  ActivatePython();
  IncludeAppModulesToPath();
  InstallDependencies();
  LoadMainScript();
  EnableComponents();
end;

procedure TPyMainForm.NotifyDebugSessionStarted;
begin
  Log('TPyMainForm.NotifyDebugSessionStarted', []);
  if NotificationCenter1.Supported then begin
    var LNotification := NotificationCenter1.CreateNotification();
    LNotification.Name := 'PyDebug';
    LNotification.Title := 'Python Debugger for Android';
    LNotification.AlertBody := Format('Debug session started at %s:%d', [FDebugHost, FDebugPort]);
    LNotification.FireDate := Now();
    Log('NotificationCenter1.PresentNotification()', []);
    NotificationCenter1.PresentNotification(LNotification);
  end;
end;

procedure TPyMainForm.ConfigureEngine;
begin
  Log('TPyMainForm.ConfigureEngine', []);
  if FDebugging then begin
    PythonEngine1.IO := nil;
    PythonEngine1.PyFlags := PythonEngine1.PyFlags
      + [pfDebug, pfInteractive, pfVerbose];
    PythonEngine1.RedirectIO := false;
    PythonEngine1.InitThreads := true;
  end else begin
    PythonEngine1.IO := PythonGUIInputOutput1;
    PythonEngine1.RedirectIO := true;
  end;
end;

procedure TPyMainForm.SetupPython;
begin
  Log('TPyMainForm.SetupPython', []);
  try
    PyEmbeddedEnvironment1.Setup();
  except
    RaiseGeneralFailure('Failed setting up Python.');
  end;
end;

procedure TPyMainForm.ActivatePython;
begin
  Log('TPyMainForm.ActivatePython', []);
  try
    PyEmbeddedEnvironment1.Activate();
  except
    on E: Exception do
      RaiseGeneralFailure('Failed activating Python.');
  end;
end;

function TPyMainForm.GetAppDefs(): TJSONObject;
begin
  Log('TPyMainForm.GetAppDefs', []);
  Result := nil;
  var LAppDefsFilePath := TPath.Combine(TPath.GetDocumentsPath(), 'app_defs.json');
  if TFile.Exists(LAppDefsFilePath) then begin
    Result := TJSONValue.ParseJSONValue(
      TFile.ReadAllText(LAppDefsFilePath, TEncoding.UTF8)) as TJSONobject;
  end else
    RaiseGeneralFailure('Configuration file not found.');
end;

function TPyMainForm.GetDependencies: TJSONArray;
begin
  Log('TPyMainForm.GetDependencies', []);
  if not FAppDefs.TryGetValue<TJSONArray>('dependencies', Result) then
    RaiseGeneralFailure('Dependencies not set.');
end;

function TPyMainForm.GetMainFilePath(): string;
begin
  Log('TPyMainForm.GetMainFilePath', []);
  Result := TPath.Combine(
    TPath.GetDocumentsPath(), FAppDefs.GetValue<string>('main_file', ''));
  if not TFile.Exists(Result) then begin
    RaiseGeneralFailure('Main script not found.');
  end;
end;

procedure TPyMainForm.LoadMainScript;
begin
  Log('TPyMainForm.LoadMainScript', []);
  mmMainScript.Lines.LoadFromFile(GetMainFilePath());
end;

procedure TPyMainForm.InstallDependencies;
begin
  Log('TPyMainForm.InstallDependencies', []);
  if not Assigned(FInstaller) then
    Exit;

  for var LDependency in GetDependencies() do begin
    var LModuleName := (LDependency as TJSONObject).GetValue<string>('module_name');
    var LFileName := (LDependency as TJSONObject).GetValue<string>('file_name');
    var LFilePath := TPath.Combine(TPath.GetDocumentsPath(), LFileName);
    if not FInstaller.IsInstalled(LModuleName, LFilePath) then
      if not FInstaller.Install(LModuleName, LFilePath) then
        RaiseGeneralFailure(Format('Failed to install %s.', [LFileName]))
      else if TFile.Exists(LFilePath)  then
        TFile.Delete(LFilePath);
  end;
end;

procedure TPyMainForm.IncludeAppModulesToPath;
begin
  Log('TPyMainForm.IncludeAppModulesToPath', []);
  PythonEngine1.ExecString(AnsiString(Format(
    'import sys'
    + sLineBreak
    + 'sys.path.append("%s")',
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
  Log('TPyMainForm.ReadArgs', []);
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

  FDebugging :=  LArgs.Contains('--dbg');
  if FDebugging then begin
    FDebugHost := GetValue(LArgs, '-host');
    if FDebugHost.IsEmpty() then
      FDebugHost := DEFAULT_DEBUGGER_HOST;
    FDebugPort := StrToIntDef(GetValue(LArgs, '-port'), DEFAULT_DEBUGGER_PORT);
    FRunMainScript := LArgs.Contains('--run');
    FLog := LArgs.Contains('--log');
  end else
    FRunMainScript := true;
end;

procedure TPyMainForm.RunDebugScript;
begin
  Log('TPyMainForm.RunDebugScript', []);
  var LFilePath := TPath.Combine(TPath.GetDocumentsPath(), DEBUGGER_SCRIPT_NAME);
  if TFile.Exists(LFilePath) then begin
    TFile.WriteAllText(
      LFilePath,
      TFile.ReadAllText(LFilePath, TEncoding.UTF8)
        .Replace('%lib_path%', GetPythonEngine().DllPath, [rfReplaceAll])
        .Replace('%python_home%', GetPythonEngine().PythonHome, [rfReplaceAll])
        .Replace('%path%', TPath.GetDirectoryName(GetPythonEngine().ProgramName), [rfReplaceAll])
        .Replace('%tmp_dir%', TPath.GetTempPath(), [rfReplaceAll])
        .Replace('%host%', FDebugHost, [rfReplaceAll])
        .Replace('%port%', FDebugPort.ToString(), [rfReplaceAll]),
      TEncoding.UTF8);
    try
      RunScript(LFilePath);
    except
      RaiseGeneralFailure('Failed initializing debugger.');
    end;
  end else
    RaiseGeneralFailure('Debug script not found.');
end;

procedure TPyMainForm.RunMainScript;
begin
  Log('TPyMainForm.RunMainScript', []);
  if FDebugging then begin
    NotifyDebugSessionStarted();
    try
      RunDebugScript();
    finally
      NotificationCenter1.CancelNotification('PyDebug');
    end;
  end;

  if FRunMainScript then
    try
      RunScript(GetMainFilePath());
    except
      if not FDebugging then
        RaiseGeneralFailure('Failed running main script.');
    end;
end;

procedure TPyMainForm.RunScript(const AScriptPath: string);
begin
  Log('TPyMainForm.RunScript', []);
  var LRunPy := Import('runpy');
  LRunPy.run_path(AScriptPath, run_name := PythonEngine1.ExecModule);
end;

procedure TPyMainForm.actRunExecute(Sender: TObject);
begin
  Log('TPyMainForm.actRunExecute(', []);
  mmMainScript.Lines.SaveToFile(GetMainFilePath());
  RunMainScript();
end;

procedure TPyMainForm.EnableComponents;
begin
  Log('TPyMainForm.EnableComponents', []);
  btnRun.Enabled := true;
  if FDebugging then
    loEditor.Visible := true;
end;

procedure TPyMainForm.DisableComponents;
begin
  Log('TPyMainForm.DisableComponents', []);
  loEditor.Visible := false;
  btnRun.Enabled := false;
end;

end.
