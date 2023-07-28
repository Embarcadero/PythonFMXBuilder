unit Builder.Types;

interface

uses
  System.SysUtils, System.Rtti;

type
  {$SCOPEDENUMS ON}
  TPythonVersion = (
    cp38,
    cp39,
    cp310,
    cp311);

  TArchitecture = (
    arm,
    aarch64);

  TBuildConfiguration = (
    Release, //It launches the application, but it doesn't initialize the debugger and don't even send the debugging dependencies
    Debug //It launches the application and initialize the debugger - it sets the Python interpreter to interactive mode
  );

  TDebugger = (
    None,
    DebugPy,
    Rpyc);

  TDebuggerConnectionStatus = (
    OutOfWork, //We are tension-free
    Connecting, //We are ordering a conenction to the debugger
    Started, //We are conected to the debugger - Debugger has confirmed
    Disconnecting, //We ordered to disconnect from the debugger
    Stopped //We are disconnected from the debugger - Debugger has confirmed
  );
  {$SCOPEDENUMS OFF}

  TSaveRequest = TFunc<string, string>;

  TArchitectureHelper = record helper for TArchitecture
    function AsString(): string;
    function ToTargetPlatform(): string;
    class function FromString(const AValue: string): TArchitecture; static;
  end;

  TPythonVersionHelper = record helper for TPythonVersion
    function AsString(): string;
    function ToTargetPython(): string;
    class function FromString(const AValue: string): TPythonVersion; static;
  end;

  TBuildConfigurationHelper = record helper for TBuildConfiguration
  public
    function AsString(): string;
    function ToBuildConfiguration(): string;
    class function FromString(const AValue: string): TBuildConfiguration; static;
  end;

  TDebuggerHelper = record helper for TDebugger
  public
    function AsString(): string;
    class function FromString(const AValue: string): TDebugger; static;
  end;

  ITextEditor = interface;

  IEditorControl = interface
    ['{9DD9962C-6F91-4C4D-AB83-8D862966E88E}']
    function OpenEditor(const AFileName: string;
      const AEditing: boolean = false): ITextEditor;
    procedure CloseEditor(const AFileName: string);
    procedure CloseAllEditors();
    function FindEditor(const AFileName: string): ITextEditor;
  end;

  ITextEditor = interface
    ['{52EDE988-F0AF-44B2-8445-1BD8960DD3C2}']
    function GetFileName(): string;
    function GetModified(): boolean;
    function GetBreakpoints(): TArray<integer>;
    procedure SetBreakpoints(ABreakpoints: TArray<integer>);
    function GetActiveLine(): integer;
    procedure SetActiveLine(AActiveLine: integer);
    function GetShowActiveLine(): boolean;
    procedure SetShowActiveLine(AShowActiveLine: boolean);

    procedure Open(const AFileName: string; const AEditing: boolean = false);
    procedure Close();
    procedure Save();
    procedure SaveTo(const AFileName: string);

    property FileName: string read GetFileName;
    property Modified: boolean read GetModified;
    property Breakpoints: TArray<integer> read GetBreakpoints write SetBreakpoints;
    property ActiveLine: integer read GetActiveLine write SetActiveLine;
    property ShowActiveLine: boolean read GetShowActiveLine write SetShowActiveLine;
  end;

implementation

uses
  Builder.Exception;

{ TPythonVersionHelper }
function TPythonVersionHelper.AsString: string;
begin
  case Self of
    TPythonVersion.cp38 : Result := 'cp38';
    TPythonVersion.cp39 : Result := 'cp39';
    TPythonVersion.cp310: Result := 'cp310';
    TPythonVersion.cp311: Result := 'cp311';
    else
      raise EInvalidPythonVersion.Create('Invalid Python version.');
  end;
end;
class function TPythonVersionHelper.FromString(
  const AValue: string): TPythonVersion;
begin
  if AValue = 'cp38' then
    Result := TPythonVersion.cp38
  else if AValue = 'cp39' then
    Result := TPythonVersion.cp39
  else if AValue = 'cp310' then
    Result := TPythonVersion.cp310
  else if AValue = 'cp311' then
    Result := TPythonVersion.cp311
  else
    raise EInvalidPythonVersion.Create('Invalid Python version.');
end;
function TPythonVersionHelper.ToTargetPython: string;
begin
  case Self of
    TPythonVersion.cp38: Result := 'Python 3.8';
    TPythonVersion.cp39: Result := 'Python 3.9';
    TPythonVersion.cp310: Result := 'Python 3.10';
    TPythonVersion.cp311: Result := 'Python 3.11';
  end;
end;

{ TArchitectureHelper }
function TArchitectureHelper.AsString: string;
begin
  case Self of
    TArchitecture.arm     : Result := 'arm32';
    TArchitecture.aarch64 : Result := 'arm64';
    else
      raise EInvalidArchitecture.Create('Invalid architecture.');
  end;
end;
class function TArchitectureHelper.FromString(
  const AValue: string): TArchitecture;
begin
  if AValue = 'arm32' then
    Result := TArchitecture.arm
  else if AValue = 'arm64' then
    Result := TArchitecture.aarch64
  else
    raise EInvalidArchitecture.Create('Invalid architecture.');
end;

function TArchitectureHelper.ToTargetPlatform: string;
begin
  case Self of
    TArchitecture.arm: Result := 'Android 32-bit';
    TArchitecture.aarch64: Result := 'Android 64-bit';
    else
      raise EInvalidArchitecture.Create('Invalid architecture.');
  end;
end;

{ TDebuggerHelper }

function TDebuggerHelper.AsString: string;
begin
  case Self of
    TDebugger.None   : Result := String.Empty;
    TDebugger.DebugPy: Result := 'debugpy';
    TDebugger.Rpyc   : Result := 'rpyc';
  end;
end;

class function TDebuggerHelper.FromString(const AValue: string): TDebugger;
begin
  if AValue = 'debugpy' then
    Result := TDebugger.DebugPy
  else if AValue = 'rpyc' then
    Result := TDebugger.Rpyc
  else
    Result := TDebugger.None;
end;

{ TBuildConfigurationHelper }

function TBuildConfigurationHelper.AsString: string;
begin
  case Self of
    TBuildConfiguration.Release: Result := 'release';
    TBuildConfiguration.Debug: Result := 'debug';
    else
      raise EInvalidBuildConfiguration.Create('Invalid build configuration.');
  end;
end;

class function TBuildConfigurationHelper.FromString(
  const AValue: string): TBuildConfiguration;
begin
if AValue = 'release' then
    Result := TBuildConfiguration.Release
  else if AValue = 'debug' then
    Result := TBuildConfiguration.Debug
  else
    raise EInvalidRunMode.Create('Invalid build configuration.');
end;

function TBuildConfigurationHelper.ToBuildConfiguration: string;
begin
  case Self of
    TBuildConfiguration.Release: Result := 'Release';
    TBuildConfiguration.Debug: Result := 'Debug';
    else
      raise EInvalidRunMode.Create('Invalid build configuration.');
  end;
end;

end.
