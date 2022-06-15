unit Builder.Architecture;

interface

type
  TArchitecture = (arm, aarch64);

  TArchitectureHelper = record helper for TArchitecture
    function AsString(): string;
    class function FromString(const AValue: string): TArchitecture; static;
  end;

implementation

uses
  Builder.Exception;

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

end.
