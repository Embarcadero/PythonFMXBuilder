unit Builder.PythonVersion;

interface

type
  TPythonVersion = (cp38, cp39, cp310);

  TPythonVersionHelper = record helper for TPythonVersion
    function AsString(): string;
    class function FromString(const AValue: string): TPythonVersion; static;
  end;

implementation

uses
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

end.
