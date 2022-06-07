unit Cli.Commands;

interface

implementation

uses
  System.SysUtils,
  VSoft.CommandLine.Options,
  Cli.Options;

procedure ConfigureHelpOptions();
begin
  var LCmd := TOptionsRegistry.RegisterCommand(
    'help', 'h',
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
    'create',
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

  LOption := LCmd.RegisterUnNamedOption<string>(
    'Include the default python script file.',
    'defaultscript',
    procedure(const AValue: string) begin
      if (AValue = 'defaultscript') then
        TCreateOptions.IncludeDefaultScriptCommand := true;
    end);

  LCmd.Examples.Add('create --name my_project');
  LCmd.Examples.Add('create --name my_project defaultscript');
end;

procedure ConfigureListOptions();
begin
  var LCmd := TOptionsRegistry.RegisterCommand(
    'list',
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
    'build',
    String.Empty,
    'Build the current project.',
    String.Empty,
    'build [options]');
end;

procedure ConfigureDeployOptions();
begin
  var LCmd := TOptionsRegistry.RegisterCommand(
    'deploy',
    String.Empty,
    'Deploy the current project to a device.',
    String.Empty,
    'deploy [options]');

  var LOption := LCmd.RegisterOption<string>(
    'device', 'd', 'Select the target device.',
    procedure(const Value: string) begin
      TDeployOptions.DeviceCommand := Value;
    end);
  LOption.Required := false;

  LOption := LCmd.RegisterUnNamedOption<string>(
    'Uninstall application before deployment.',
    'uninstall',
    procedure(const AValue: string) begin
      TDeployOptions.UninstallCommand := (AValue = 'uninstall');
    end);
  LOption.Required := false;

  LCmd.Examples.Add('deploy -d my_device');
  LCmd.Examples.Add('deploy -d my_device uninstall');
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
