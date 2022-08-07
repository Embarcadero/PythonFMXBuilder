unit Builder.Environment;

interface

type
  {$SCOPEDENUMS ON}
  TEnvironment = (&private, &public);
  {$SCOPEDENUMS OFF}

  TEnvironmentHelper = record helper for TEnvironment
    function AsString(): string;
    class function FromString(const AValue: string): TEnvironment; static;
  end;

implementation

uses
  Builder.Exception;

{ TEnvironmentHelper }

function TEnvironmentHelper.AsString: string;
begin
  case Self of
    TEnvironment.private: Result := 'private';
    TEnvironment.public: Result := 'public';
    else
      raise EInvalidEnvironment.Create('Invalid environment.');
  end;
end;

class function TEnvironmentHelper.FromString(
  const AValue: string): TEnvironment;
begin
  if AValue = 'private' then
    Result := TEnvironment.private
  else if AValue = 'public' then
    Result := TEnvironment.public
  else
    raise EInvalidEnvironment.Create('Invalid environment.');
end;

end.
