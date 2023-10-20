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
  Builder.Services.Editor,
  Builder.Services.Tools.Install,
  // Models
  Builder.Model.Environment,
  Builder.Model.Environment.Android;

procedure RegisterServices();
begin
  TBuilderService.Instance.RegisterService<IADBServices>(TADBService);
  TBuilderService.Instance.RegisterService<IAppServices>(TAppService);
  TBuilderService.Instance.RegisterService<IBuildServices>(TBuildService);
  TBuilderService.Instance.RegisterService<IDebugServices>(TDebugService);
  TBuilderService.Instance.RegisterService<IProjectServices>(TProjectService);
  TBuilderService.Instance.RegisterService<IUnboundPythonServices>(TUnboundPythonService);
  TBuilderService.Instance.RegisterService<IEnvironmentServices<TAndroidEnvironmentModel>>(TEnvironmentService<TAndroidEnvironmentModel>);
  TBuilderService.Instance.RegisterService<IEditorServices>(TEditorService);
  TBuilderService.Instance.RegisterService<IToolInstallServices>(TToolInstallService);
end;

procedure UnregisterServices();
begin
  TBuilderService.Instance.UnregisterService<IADBServices>();
  TBuilderService.Instance.UnregisterService<IAppServices>();
  TBuilderService.Instance.UnregisterService<IBuildServices>();
  TBuilderService.Instance.UnregisterService<IDebugServices>();
  TBuilderService.Instance.UnregisterService<IProjectServices>();
  TBuilderService.Instance.UnregisterService<IUnboundPythonServices>();
  TBuilderService.Instance.UnregisterService<IEnvironmentServices<TAndroidEnvironmentModel>>();
  TBuilderService.Instance.UnregisterService<IEditorServices>();
  TBuilderService.Instance.UnregisterService<IToolInstallServices>();
end;

end.
