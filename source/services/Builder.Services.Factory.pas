unit Builder.Services.Factory;

interface

uses
  Builder.Services;

type
  TServiceSimpleFactory = class
  public
    class function CreateEnvironment(): IEnvironmentServices;
    class function CreateAdb(): IADBServices;
    class function CreateProject(): IProjectServices;
    class function CreateApp(): IAppServices;
    class function CreateDebug(): IDebugServices;
    class function CreateBuild(): IBuildServices;
    class function CreateUnboundPy(): IUnboundPythonServices;
  end;

implementation

uses
  Builder.Services.Environment,
  Builder.Services.ADB,
  Builder.Services.Project,
  Builder.Services.App,
  Builder.Services.Debug,
  Builder.Services.Build,
  Builder.Services.UnboundPython;

{ TServiceSimpleFactory }

class function TServiceSimpleFactory.CreateAdb: IADBServices;
begin
  Result := TADBService.Create();
end;

class function TServiceSimpleFactory.CreateProject: IProjectServices;
begin
  Result := TProjectService.Create();
end;

class function TServiceSimpleFactory.CreateUnboundPy: IUnboundPythonServices;
begin
  Result := TUnboundPythonServices.Create();
end;

class function TServiceSimpleFactory.CreateApp: IAppServices;
begin
  Result := TAppService.Create();
end;

class function TServiceSimpleFactory.CreateBuild: IBuildServices;
begin
  Result := TBuildService.Create();
end;

class function TServiceSimpleFactory.CreateDebug: IDebugServices;
begin
  Result := TDebugService.Create();
end;

class function TServiceSimpleFactory.CreateEnvironment: IEnvironmentServices;
begin
  Result := TEnvironmentService.Create();
end;

end.
