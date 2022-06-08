unit Cli.Commands;

interface

const
  HELP_CMD    = 'help';
  CREATE_CMD  = 'create';
  LIST_CMD    = 'list';
  BUILD_CMD   = 'build';
  DEPLOY_CMD  = 'deploy';

implementation

uses
  System.SysUtils,
  VSoft.CommandLine.Options,
  Cli.Options;

procedure ConfigureHelpOptions();
begin
  var LCmd := TOptionsRegistry.RegisterCommand(
    HELP_CMD, 'h',
    'Display a list of command options and their help strings.',
    String.Empty,
    'help [command]');

  LCmd.RegisterUnNamedOption<string>(
    'Help for command.',
    'command',
    procedure(const AValue: string) begin
      THelpOptions.HelpCommand := AValue;
    end);

  LCmd.Examples.Add('help create');
end;

procedure ConfigureCreateOptions();
begin
  var LCmd := TOptionsRegistry.RegisterCommand(
    CREATE_CMD,
    String.Empty,
    'Create a new project from the scratch.',
    String.Empty,
    'create [options]');

  var LOption := LCmd.RegisterOption<string>(
    'name',
    String.Empty,
    'Project name',
    procedure(const AValue: string) begin
      TCreateOptions.ProjectNameCommand := AValue;
    end);
  LOption.Required := true;

  LCmd.Examples.Add('create --name my_project');
  LCmd.Examples.Add('create --name my_project -s');
end;

procedure ConfigureListOptions();
begin
  var LCmd := TOptionsRegistry.RegisterCommand(
    LIST_CMD,
    String.Empty,
    'List all projects available in the data folder.',
    String.Empty,
    'list [options]');
end;

procedure ConfigureSelectOptions();
begin
  var LCmd := TOptionsRegistry.RegisterCommand(
    'select',
    String.Empty,
    'Select an existent project and set it as the current project.',
    String.Empty,
    'select [options]');

  var LOption := LCmd.RegisterUnNamedOption<string>(
    'Select a project by its name.',
    'Project name',
    procedure(const AValue: string) begin
      TSelectOptions.ProjectNameCommand := AValue;
    end);
  LOption.Required := true;

  LCmd.Examples.Add('select my_project');
end;

procedure ConfigureBuildOptions();
begin
  var LCmd := TOptionsRegistry.RegisterCommand(
    BUILD_CMD,
    String.Empty,
    'Build the current project.',
    String.Empty,
    'build [options]');

  var LOption := LCmd.RegisterOption<string>(
    'name',
    String.Empty,
    'Project name',
    procedure(const AValue: string) begin
      TBuildOptions.ProjectNameCommand := AValue;
    end);
  LOption.Required := true;

  LOption := LCmd.RegisterOption<boolean>(
    'verbose',
    'v',
    'Print logs',
    procedure(const AValue: boolean) begin
      TBuildOptions.VerboseCommand := true;
    end
  );
  LOption.Required := false;
  LOption.HasValue := false;

  LCmd.Examples.Add('build --name my_project');
  LCmd.Examples.Add('build --name my_project -v');
end;

procedure ConfigureDeployOptions();
begin
  var LCmd := TOptionsRegistry.RegisterCommand(
    DEPLOY_CMD,
    String.Empty,
    'Deploy the current project to a device.',
    String.Empty,
    'deploy [options]');

  var LOption := LCmd.RegisterOption<string>(
    'name',
    String.Empty,
    'Project name',
    procedure(const AValue: string) begin
      TDeployOptions.ProjectNameCommand := AValue;
    end);
  LOption.Required := true;

  LOption := LCmd.RegisterOption<string>(
    'device', 'd', 'Select the target device.',
    procedure(const Value: string) begin
      TDeployOptions.DeviceCommand := Value;
    end);
  LOption.Required := false;
  LOption.HasValue := false;

  LOption := LCmd.RegisterOption<boolean>(
    'uninstall',
    'u',
    'Uninstall application before deployment.',
    procedure(const AValue: boolean) begin
      TDeployOptions.UninstallCommand := true;
    end);
  LOption.Required := false;
  LOption.HasValue := false;

  LOption := LCmd.RegisterOption<boolean>(
    'verbose',
    'v',
    'Print logs',
    procedure(const AValue: boolean) begin
      TDeployOptions.VerboseCommand := true;
    end
  );
  LOption.Required := false;
  LOption.HasValue := false;

  LCmd.Examples.Add('deploy --name my_project');
  LCmd.Examples.Add('deploy --name my_project -d my_device');
  LCmd.Examples.Add('deploy --name my_project -d my_device -u');
  LCmd.Examples.Add('deploy --name my_project -d my_device -u -v');
end;

procedure ConfigureOptions();
begin
  TOptionsRegistry.DescriptionTab := 35;
  TOptionsRegistry.NameValueSeparator := ' ';

  ConfigureHelpOptions();
  ConfigureCreateOptions();
  ConfigureListOptions();
  ConfigureSelectOptions();
  ConfigureBuildOptions();
  ConfigureDeployOptions();
end;

initialization
  ConfigureOptions();

end.
