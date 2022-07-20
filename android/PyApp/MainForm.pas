(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'MainForm'     Copyright (c) 2021                        *)
(*                                                                        *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(**************************************************************************)
(*  Functionality:  MainForm of PyFun                                     *)
(*                  PyFun on Android                                      *)
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
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Ani, FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  PythonEngine, FMX.PythonGUIInputOutput, System.Actions, FMX.ActnList,
  FMX.Objects, FMX.Layouts, FMX.Platform,
  System.Threading, System.JSON, FMX.ListBox, WrapDelphi, WrapDelphiFMX,
  PyEnvironment, PyEnvironment.Embeddable, PyEnvironment.AddOn,
  PyEnvironment.AddOn.EnsurePip, System.Zip, PyEnvironment.Distribution;

type
  TPyMainForm = class(TForm)
    tbTop: TToolBar;
    PythonEngine1: TPythonEngine;
    ActionList1: TActionList;
    actRun: TAction;
    Label1: TLabel;
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
    PyEnvironmentAddOnEnsurePip1: TPyEnvironmentAddOnEnsurePip;
    sbPythonStatus: TStatusBar;
    lbPythonStatus: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure actRunExecute(Sender: TObject);
    procedure PyEmbeddedEnvironment1BeforeSetup(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedEnvironment1AfterSetup(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedEnvironment1BeforeActivate(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedEnvironment1AfterActivate(Sender: TObject;
      const APythonVersion: string; const AActivated: Boolean);
    procedure PyEmbeddedEnvironment1Ready(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedEnvironment1ZipProgress(Sender: TObject;
      ADistribution: TPyCustomEmbeddableDistribution; FileName: string;
      Header: TZipHeader; Position: Int64);
    procedure PyEnvironmentAddOnEnsurePip1Execute(const ASender: TObject;
      const ATrigger: TPyEnvironmentaddOnTrigger;
      const ADistribution: TPyDistribution);
  private
    FMainScriptPath: string;
    FDebugging: boolean;
    FDebugPort: integer;

    procedure DisableComponents();
    procedure EnableComponents();

    procedure ReadArgs();
    procedure Initialize();

    function ExecCmd(const AArgs: TArray<string>): integer;

    function TryToSetupPython(): boolean;
    function TryToActivatePython(): boolean;
    procedure IncludeAppModulesToPath();

    function TryToGetAppDefs(out AAppDefs: TJSONObject): boolean;
    function TryToGetMainFilePath(out AFilePath: string): boolean;
    function TryToInstallDependencies(): boolean;
    function IsDependencyInstalled(const AModuleName: string): boolean;
    function InstallDependency(const AModuleName: string; const AFilePath: string): boolean;
    function TryToLoadMainScript(): boolean;
    procedure RunMainScript();

    procedure UpdatePythonStatus(const AMessage: string);
  public
    { Public declarations }
  end;

var
  PyMainForm: TPyMainForm;

implementation

uses
  System.IOUtils,
  Androidapi.JNI.JavaTypes,
  Androidapi.Helpers,
  FMX.Platform.Android,
  PyTools.ExecCmd, PyTools.ExecCmd.Args, VarPyth;

type
  ESetupFailed = class(Exception);

{$R *.fmx}

{ TPyMainForm }

procedure TPyMainForm.FormCreate(Sender: TObject);
begin
  DisableComponents();
  ReadArgs();
  Initialize();
end;

procedure TPyMainForm.Initialize;
begin
  var LSetupPython := TTask.Run(
    procedure()
    begin
      if TryToSetupPython() then
        TThread.Queue(nil,
          procedure
          begin
            if TryToActivatePython() and TryToInstallDependencies() then begin
              IncludeAppModulesToPath();
              if TryToLoadMainScript() then begin
                EnableComponents();
                RunMainScript();
                sbPythonStatus.Visible := false;
              end;
            end;
          end);
    end);
end;

function TPyMainForm.TryToSetupPython: boolean;
begin
  Result := false;
  try
    PyEmbeddedEnvironment1.Setup();
    Result := true;
  except
    on E: Exception do
      UpdatePythonStatus('Failed setting up Python.');
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
      UpdatePythonStatus('Failed loading Python.');
  end;
end;

function TPyMainForm.TryToGetAppDefs(out AAppDefs: TJSONObject): boolean;
begin
  Result := false;
  var LAppDefsFilePath := TPath.Combine(TPath.GetDocumentsPath(), 'app_defs.json');
  if TFile.Exists(LAppDefsFilePath) then begin
    AAppDefs := TJSONValue.ParseJSONValue(TFile.ReadAllText(LAppDefsFilePath, TEncoding.UTF8)) as TJSONobject;
    Result := Assigned(AAppDefs);
  end else
    UpdatePythonStatus('Configuration file not found.');
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
        UpdatePythonStatus('Main script not found.');
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
          if not IsDependencyInstalled(LModuleName) then
            if not InstallDependency(LModuleName, LFilePath) then begin
              UpdatePythonStatus(Format('Failed to install %s.', [LFileName]));
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

function TPyMainForm.ExecCmd(const AArgs: TArray<string>): integer;
var
  LOutput: string;
begin
  var LExec := TExecCmdService
    .Cmd(PythonEngine1.ProgramName,
      TExecCmdArgs.BuildArgv(
        PythonEngine1.ProgramName,
        AArgs),
      TExecCmdArgs.BuildEnvp(
        PythonEngine1.PythonHome,
        PythonEngine1.ProgramName,
        TPath.Combine(PythonEngine1.DllPath, PythonEngine1.DllName)))
    .Run(LOutput);

  Result := LExec.Wait();
end;

function TPyMainForm.IsDependencyInstalled(const AModuleName: string): boolean;
const
  CMD = 'import pkgutil; exit(0 if pkgutil.find_loader("%s") else 1)';
begin
  Result := ExecCmd(['-c', Format(CMD, [AModuleName])]) = EXIT_SUCCESS;
end;

function TPyMainForm.InstallDependency(const AModuleName: string; const AFilePath: string): boolean;
begin
  Result := ExecCmd(['-m', 'pip', 'install', AFilePath]) = EXIT_SUCCESS;
end;

procedure TPyMainForm.PyEmbeddedEnvironment1AfterActivate(Sender: TObject;
  const APythonVersion: string; const AActivated: Boolean);
begin
  UpdatePythonStatus('Python is active.');
end;

procedure TPyMainForm.PyEmbeddedEnvironment1AfterSetup(Sender: TObject;
  const APythonVersion: string);
begin
  UpdatePythonStatus('Setup done.');
end;

procedure TPyMainForm.PyEmbeddedEnvironment1BeforeActivate(Sender: TObject;
  const APythonVersion: string);
begin
  UpdatePythonStatus(Format('Activating Python %s.', [APythonVersion]));
end;

procedure TPyMainForm.PyEmbeddedEnvironment1BeforeSetup(Sender: TObject;
  const APythonVersion: string);
begin
  UpdatePythonStatus(Format('Setting up Python %s.', [APythonVersion]));
end;

procedure TPyMainForm.PyEmbeddedEnvironment1Ready(Sender: TObject;
  const APythonVersion: string);
begin
  UpdatePythonStatus('All done.');
end;

procedure TPyMainForm.PyEmbeddedEnvironment1ZipProgress(Sender: TObject;
  ADistribution: TPyCustomEmbeddableDistribution; FileName: string;
  Header: TZipHeader; Position: Int64);
begin
  UpdatePythonStatus(Format('Extracting %s', [FileName]));
end;

procedure TPyMainForm.PyEnvironmentAddOnEnsurePip1Execute(
  const ASender: TObject; const ATrigger: TPyEnvironmentaddOnTrigger;
  const ADistribution: TPyDistribution);
begin
  UpdatePythonStatus('Setting up PIP.');
end;

procedure TPyMainForm.UpdatePythonStatus(const AMessage: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      lbPythonStatus.Text := AMessage;
    end);
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
    FDebugPort := StrToIntDef(GetValue(LArgs, '-port'), 5678);
  end;
end;

procedure TPyMainForm.RunMainScript;
begin
  if FDebugging then begin
    var LSubProcessEnv := NewPythonDict();
    LSubProcessEnv.SetItem('LD_LIBRARY_PATH', GetPythonEngine().DllPath);
    LSubProcessEnv.SetItem('PYTHONHOME', GetPythonEngine().PythonHome);
    LSubProcessEnv.SetItem('PATH', ExtractFileDir(GetPythonEngine().ProgramName));
    LSubProcessEnv.SetItem('TMPDIR', TPath.GetTempPath());

    var LDebugger := Import('debugpy');
    //LDebugger.log_to(TPath.GetDocumentsPath());
    LDebugger.configure(subProcessEnv := LSubProcessEnv);
    LDebugger.listen(FDebugPort);
    LDebugger.wait_for_client();
  end;

  var LRunPy := Import('runpy');
  LRunPy.run_path(FMainScriptPath, run_name := PythonEngine1.ExecModule);
end;

procedure TPyMainForm.actRunExecute(Sender: TObject);
begin
  mmMainScript.Lines.SaveToFile(FMainScriptPath);
  RunMainScript();
end;

procedure TPyMainForm.DisableComponents;
begin
  btnRun.Enabled := false;
end;

procedure TPyMainForm.EnableComponents;
begin
  btnRun.Enabled := true;
end;

end.
