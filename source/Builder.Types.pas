unit Builder.Types;

interface

type
  {$SCOPEDENUMS ON}
  TPythonVersion = (
    cp38,
    cp39,
    cp310);

  TArchitecture = (
    arm,
    aarch64);

  TDebugger = (
    None,
    DebugPy,
    Rpyc);

  TRunMode = (
    RunNormalMode, //It launches the application, but it doesn't initialize the debugger
    RunDebugMode //It launches the application and initialize the debugger - it sets the Python interpreter to interactive mode
  );

  TDebuggerConnectionStatus = (
    OutOfWork, //We are tension-free
    Connecting, //We are ordering a conenction to the debugger
    Started, //We are conected to the debugger - Debugger has confirmed
    Disconnecting, //We ordered to disconnect from the debugger
    Stopped //We are disconnected from the debugger - Debugger has confirmed
  );
  {$SCOPEDENUMS OFF}

  TArchitectureHelper = record helper for TArchitecture
    function AsString(): string;
    class function FromString(const AValue: string): TArchitecture; static;
  end;

  TPythonVersionHelper = record helper for TPythonVersion
    function AsString(): string;
    class function FromString(const AValue: string): TPythonVersion; static;
  end;

  TDebuggerHelper = record helper for TDebugger
  public
    function AsString(): string;
    class function FromString(const AValue: string): TDebugger; static;
  end;

  TRunModeHelper = record helper for TRunMode
  public
    function AsString(): string;
    class function FromString(const AValue: string): TRunMode; static;
  end;

implementation

uses
  System.SysUtils,
  Builder.Exception;

{ TPythonVersionHelper }
function TPythonVersionHelper.AsString: string;
begin
  case Self of
    TPythonVersion.cp38 : Result := 'cp38';
    TPythonVersion.cp39 : Result := 'cp39';
    TPythonVersion.cp310: Result := 'cp310';
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
  else
    raise EInvalidPythonVersion.Create('Invalid Python version.');
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

{ TRunModeHelper }

function TRunModeHelper.AsString: string;
begin
  case Self of
    TRunMode.RunNormalMode: Result := 'normal';
    TRunMode.RunDebugMode : Result := 'debug';
  end;
end;

class function TRunModeHelper.FromString(const AValue: string): TRunMode;
begin
  if AValue = 'normal' then
    Result := TRunMode.RunNormalMode
  else if AValue = 'debug' then
    Result := TRunMode.RunDebugMode
  else
    raise EInvalidRunMode.Create('Invalid run mode.');
end;

end.
