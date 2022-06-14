unit Builder.Services.Factory;

interface

uses
  Builder.Services;

type
  TServiceSimpleFactory = class
  public
    class function CreateAdb(): IADBServices;
    class function CreateProject(): IProjectServices;
    class function CreateApp(): IAppServices;
  end;

implementation

uses
  Builder.Services.ADB, Builder.Services.Project, Builder.Services.App;

{ TServiceSimpleFactory }

class function TServiceSimpleFactory.CreateAdb: IADBServices;
begin
  Result := TADBService.Create();
end;

class function TServiceSimpleFactory.CreateProject: IProjectServices;
begin
  Result := TProjectService.Create();
end;

class function TServiceSimpleFactory.CreateApp: IAppServices;
begin
  Result := TAppService.Create();
end;

end.
