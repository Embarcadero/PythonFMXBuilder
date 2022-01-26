unit Services.Factory;

interface

uses
  Services;

type
  TServiceSimpleFactory = class
  public
    class function CreateAdb(): IADBServices;
    class function CreateApp(): IAppServices;
  end;

implementation

uses
  Services.ADB, Services.App;

{ TServiceSimpleFactory }

class function TServiceSimpleFactory.CreateAdb: IADBServices;
begin
  Result := TADBService.Create();
end;

class function TServiceSimpleFactory.CreateApp: IAppServices;
begin
  Result := TAppService.Create();
end;

end.
