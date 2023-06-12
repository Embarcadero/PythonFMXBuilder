unit Builder.Types;

interface

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
