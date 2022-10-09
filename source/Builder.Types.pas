unit Builder.Types;

interface

type
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

implementation

uses
  System.SysUtils,
  Builder.Exception;

{ TPythonVersionHelper }
function TPythonVersionHelper.AsString: string;
begin
  case Self of
    cp38 : Result := 'cp38';
    cp39 : Result := 'cp39';
    cp310: Result := 'cp310';
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
    arm     : Result := 'arm32';
    aarch64 : Result := 'arm64';
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

end.
