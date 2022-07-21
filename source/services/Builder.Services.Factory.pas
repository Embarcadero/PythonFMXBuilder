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
    class function CreateDebug(): IDebugServices;
  end;

implementation

uses
  Builder.Services.ADB,
  Builder.Services.Project,
  Builder.Services.App,
  Builder.Services.Debug;

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

class function TServiceSimpleFactory.CreateDebug: IDebugServices;
begin
  Result := TDebugService.Create();
end;

end.
