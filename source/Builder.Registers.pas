unit Builder.Registers;

interface

uses
  Builder.Services;

procedure RegisterServices();
procedure UnregisterServices();

implementation

uses
  Builder.Services.ADB,
  Builder.Services.App,
  Builder.Services.Build,
  Builder.Services.Debug,
  Builder.Services.Project,
  Builder.Services.UnboundPython,
  Builder.Services.Environment,
  Builder.Services.Editor;

procedure RegisterServices();
begin
  TBuilderService.Instance.RegisterService<IADBServices>(TADBService);
  TBuilderService.Instance.RegisterService<IAppServices>(TAppService);
  TBuilderService.Instance.RegisterService<IBuildServices>(TBuildService);
  TBuilderService.Instance.RegisterService<IDebugServices>(TDebugService);
  TBuilderService.Instance.RegisterService<IProjectServices>(TProjectService);
  TBuilderService.Instance.RegisterService<IUnboundPythonServices>(TUnboundPythonService);
  TBuilderService.Instance.RegisterService<IEnvironmentServices>(TEnvironmentService);
  TBuilderService.Instance.RegisterService<IEditorServices>(TEditorService);
end;

procedure UnregisterServices();
begin
  TBuilderService.Instance.UnregisterService<IADBServices>();
  TBuilderService.Instance.UnregisterService<IAppServices>();
  TBuilderService.Instance.UnregisterService<IBuildServices>();
  TBuilderService.Instance.UnregisterService<IDebugServices>();
  TBuilderService.Instance.UnregisterService<IProjectServices>();
  TBuilderService.Instance.UnregisterService<IUnboundPythonServices>();
  TBuilderService.Instance.UnregisterService<IEnvironmentServices>();
  TBuilderService.Instance.UnregisterService<IEditorServices>();
end;

end.
