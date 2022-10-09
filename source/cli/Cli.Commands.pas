unit Cli.Commands;

interface

const
  HELP_CMD         = 'help';
  CREATE_CMD       = 'create';
  LIST_CMD         = 'list';
  REMOVE_CMD       = 'remove';
  BUILD_CMD        = 'build';
  DEPLOY_CMD       = 'deploy';
  RUN_CMD          = 'run';
  STOP_CMD         = 'stop';
  DEVICE_CMD       = 'device';
  ENVIRONMENT_CMD  = 'environment';
  PROJECT_CMD      = 'project';

implementation

uses
  System.SysUtils,
  System.IOUtils,
  VSoft.CommandLine.Options,
  Cli.Options,
  Builder.Types,
  Builder.Model,
  Builder.Model.Environment,
  Builder.Storage.Default;

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

procedure ConfigureGlobalOptions();
begin
  TOptionsRegistry.RegisterOption<string>(
    'debugger',
    String.Empty,
    'Select a debuger.',
    procedure(const AValue: string) begin
      TGlobalOptions.DebuggerCommand := AValue;
    end);
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
    'Project name.',
    procedure(const AValue: string) begin
      TCreateOptions.ProjectNameCommand := AValue;
    end);
  LOption.Required := true;

  LOption := LCmd.RegisterOption<boolean>(
    'main',
    String.Empty,
    'Add the main script file.',
    procedure(const AValue: boolean) begin
      TCreateOptions.AddMainScriptCommand := true;
    end
  );
  LOption.Required := false;
  LOption.HasValue := false;

  LCmd.Examples.Add('create --name my_project');
  LCmd.Examples.Add('create --name my_project -main');
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

procedure ConfigureRemoveOptions();
begin
  var LCmd := TOptionsRegistry.RegisterCommand(
    REMOVE_CMD,
    String.Empty,
    'Remove a project.',
    String.Empty,
    'remove [options]');

  var LOption := LCmd.RegisterOption<string>(
    'name',
    String.Empty,
    'Project name.',
    procedure(const AValue: string) begin
      TRemoveOptions.ProjectNameCommand := AValue;
    end);
  LOption.Required := true;

  LOption := LCmd.RegisterOption<boolean>(
    'yes',
    'y',
    'Skip confirmation.',
    procedure(const AValue: boolean) begin
      TRemoveOptions.SkipConfirmationCommand := true;
    end
  );
  LOption.Required := false;
  LOption.HasValue := false;

  LCmd.Examples.Add('remove --name my_project');
  LCmd.Examples.Add('remove --name my_project -y');
end;

procedure ConfigureBuildOptions();
begin
  var LCmd := TOptionsRegistry.RegisterCommand(
    BUILD_CMD,
    String.Empty,
    'Build the selected project.',
    String.Empty,
    'build [options]');

  var LOption := LCmd.RegisterOption<string>(
    'name',
    String.Empty,
    'Project name.',
    procedure(const AValue: string) begin
      TBuildOptions.ProjectNameCommand := AValue;
    end);
  LOption.Required := true;

  LOption := LCmd.RegisterOption<boolean>(
    'verbose',
    'v',
    'Print logs.',
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
    'Deploy the selected project to a device.',
    String.Empty,
    'deploy [options]');

  var LOption := LCmd.RegisterOption<string>(
    'name',
    String.Empty,
    'Project name.',
    procedure(const AValue: string) begin
      TDeployOptions.ProjectNameCommand := AValue;
    end);
  LOption.Required := true;
  LOption := LCmd.RegisterOption<string>(
    'device',
    'd',
    'Select the target device. Empty to auto detect.',
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
    'Print logs.',
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

procedure ConfigureRunOptions();
begin
  var LCmd := TOptionsRegistry.RegisterCommand(
    RUN_CMD,
    String.Empty,
    'Run the selected project in a device.',
    String.Empty,
    'run [options]');

  var LOption := LCmd.RegisterOption<string>(
    'name',
    String.Empty,
    'Project name.',
    procedure(const AValue: string) begin
      TRunOptions.ProjectNameCommand := AValue;
    end);
  LOption.Required := true;

  LOption := LCmd.RegisterOption<string>(
    'device',
    'd',
    'Select the target device. Empty to auto detect.',
    procedure(const Value: string) begin
      TRunOptions.DeviceCommand := Value;
    end);
  LOption.Required := false;
  LOption.HasValue := false;

  LOption := LCmd.RegisterOption<boolean>(
    'debug',
    'dbg',
    'Run in debug mode.',
    procedure(const AValue: boolean) begin
      TRunOptions.DebugModeCommand := true;
    end);
  LOption.Required := false;
  LOption.HasValue := false;

  LOption := LCmd.RegisterOption<boolean>(
    'verbose',
    'v',
    'Print logs.',
    procedure(const AValue: boolean) begin
      TRunOptions.VerboseCommand := true;
    end
  );
  LOption.Required := false;
  LOption.HasValue := false;

  LCmd.Examples.Add('deploy --name my_project');
  LCmd.Examples.Add('deploy --name my_project -d my_device');
  LCmd.Examples.Add('deploy --name my_project -d my_device -dbg');
  LCmd.Examples.Add('deploy --name my_project -d my_device -dbg -v');
end;

procedure ConfigureStopOptions();
begin
  var LCmd := TOptionsRegistry.RegisterCommand(
    STOP_CMD,
    String.Empty,
    'Stop the selected project running on a device.',
    String.Empty,
    'stop [options]');

  var LOption := LCmd.RegisterOption<string>(
    'name',
    String.Empty,
    'Project name.',
    procedure(const AValue: string) begin
      TStopOptions.ProjectNameCommand := AValue;
    end);
  LOption.Required := true;

  LOption := LCmd.RegisterOption<string>(
    'device',
    'd',
    'Select the target device. Empty to auto detect.',
    procedure(const Value: string) begin
      TStopOptions.DeviceCommand := Value;
    end);
  LOption.Required := false;
  LOption.HasValue := false;

  LCmd.Examples.Add('stop --name my_project');
  LCmd.Examples.Add('stop --name my_project -d my_device');
end;

//miscellaneous
procedure ConfigureDeviceOptions();
begin
  var LCmd := TOptionsRegistry.RegisterCommand(
    DEVICE_CMD,
    String.Empty,
    'Show all connected devices.',
    String.Empty,
    'device [options]');

  var LOption := LCmd.RegisterOption<boolean>(
    'list',
    'l',
    'List all connected devices.',
    procedure(const AValue: boolean) begin
      TDeviceOptions.ListCommand := AValue;
    end);
  LOption.Required := false;
  LOption.HasValue := false;

  LCmd.Examples.Add('device');
  LCmd.Examples.Add('device --list');
end;

//entity commands
procedure ConfigureEnvironment();
begin
  var LCmd := TOptionsRegistry.RegisterCommand(
    ENVIRONMENT_CMD,
    String.Empty,
    'Configure the Environment entity.',
    String.Empty,
    'environment [options]');

  var LOption := LCmd.RegisterOption<boolean>(
    'find',
    'f',
    'Automatically finds SDK and JDK locations based on its base paths.',
    procedure(const AValue: boolean) begin
      TEnvironmentOptions.FindCommand := AValue;
    end);
  LOption.Required := false;
  LOption.HasValue := false;

  LOption := LCmd.RegisterOption<boolean>(
    'override',
    'o',
    'Overrides paths when using the find option.',
    procedure(const AValue: boolean) begin
      TEnvironmentOptions.OverrideCommand := AValue;
    end);
  LOption.Required := false;
  LOption.HasValue := false;

  LOption := LCmd.RegisterOption<boolean>(
    'show',
    's',
    'Show all fields.',
    procedure(const AValue: boolean) begin
      TEnvironmentOptions.ShowCommand := AValue;
    end);
  LOption.Required := false;
  LOption.HasValue := false;

  //Fields
  LOption := LCmd.RegisterOption<string>(
    'sdk_base_path',
    String.Empty,
    'SDK base path. Use the -f option to automatically look up for SDK paths.',
    procedure(const AValue: string) begin
      TEnvironmentOptions.SdkBasePathCommand := AValue;
    end);
  LOption.Required := false;

  LOption := LCmd.RegisterOption<string>(
    'apk_signer_location',
    String.Empty,
    'APK signer location.',
    procedure(const AValue: string) begin
      TEnvironmentOptions.ApkSignerLocationCommand := AValue;
    end);
  LOption.Required := false;

  LOption := LCmd.RegisterOption<string>(
    'adb_location',
    String.Empty,
    'ADB location.',
    procedure(const AValue: string) begin
      TEnvironmentOptions.AdbLocationCommand := AValue;
    end);
  LOption.Required := false;

  LOption := LCmd.RegisterOption<string>(
    'apt_location',
    String.Empty,
    'APT location.',
    procedure(const AValue: string) begin
      TEnvironmentOptions.AptLocationCommand := AValue;
    end);
  LOption.Required := false;

  LOption := LCmd.RegisterOption<string>(
    'sdk_api_location',
    String.Empty,
    'SDK API location.',
    procedure(const AValue: string) begin
      TEnvironmentOptions.SdkApiLocationCommand := AValue;
    end);
  LOption.Required := false;

  LOption := LCmd.RegisterOption<string>(
    'zip_align_location',
    String.Empty,
    'ZIP align location.',
    procedure(const AValue: string) begin
      TEnvironmentOptions.ZipAlignLocationCommand := AValue;
    end);
  LOption.Required := false;

  LOption := LCmd.RegisterOption<string>(
    'jdk_base_path',
    String.Empty,
    'JDK base path. Use the -f option to automatically look up for JDK paths.',
    procedure(const AValue: string) begin
      TEnvironmentOptions.JdkBasePathCommand := AValue;
    end);
  LOption.Required := false;

  LOption := LCmd.RegisterOption<string>(
    'key_tool_location',
    String.Empty,
    'Key-tool location.',
    procedure(const AValue: string) begin
      TEnvironmentOptions.KeyToolLocationCommand := AValue;
    end);
  LOption.Required := false;

  LOption := LCmd.RegisterOption<string>(
    'jar_signer_location',
    String.Empty,
    'JAR signer location.',
    procedure(const AValue: string) begin
      TEnvironmentOptions.JarSignerLocationCommand := AValue;
    end);
  LOption.Required := false;

  if TFile.Exists(GetGUIEntityEditorPath()) then begin
    LOption := LCmd.RegisterOption<boolean>(
      'gui',
      String.Empty,
      'Edit the environment settings using the GUI editor.',
      procedure(const AValue: boolean) begin
        TEnvironmentOptions.Gui := AValue;
      end);
    LOption.Required := false;
    LOption.HasValue := false;

    LCmd.Examples.Add('environment -gui');
  end;

  LCmd.Examples.Add('environment --sdk_base_path "my_sdk_path" --jdk_base_path "my_jdk_path" -f');
  LCmd.Examples.Add('environment --sdk_base_path "my_sdk_path" --jdk_base_path "my_jdk_path" -f -o');
  LCmd.Examples.Add('environment --sdk_base_path "my_sdk_path" --jdk_base_path "my_jdk_path" --jar_signer_location "my_path" --zip_align_location "my_path"');
end;

procedure ConfigureProject();
begin
  var LCmd := TOptionsRegistry.RegisterCommand(
    PROJECT_CMD,
    String.Empty,
    'Configure the Project entity.',
    String.Empty,
    'project [options]');

  var LOption := LCmd.RegisterUnNamedOption<string>(
    'Select a project to configure.',
    'select',
    procedure(const AValue: string) begin
      TProjectOptions.SelectCommand := AValue;
    end);
  LOption.Required := true;

  LOption := LCmd.RegisterOption<boolean>(
    'show',
    's',
    'Show all fields.',
    procedure(const AValue: boolean) begin
      TProjectOptions.ShowCommand := AValue;
    end);
  LOption.Required := false;
  LOption.HasValue := false;

  //Fields
  LOption := LCmd.RegisterOption<string>(
    'application_name',
    String.Empty,
    'Application name.',
    procedure(const AValue: string) begin
      TProjectOptions.ApplicationNameCommand := AValue;
    end);
  LOption.Required := false;

  LOption := LCmd.RegisterOption<string>(
    'package_name',
    String.Empty,
    'Package name (com.embarcadero.my_app).',
    procedure(const AValue: string) begin
      TProjectOptions.PackageNameCommand := AValue;
    end);
  LOption.Required := false;

  LOption := LCmd.RegisterOption<integer>(
    'version_code',
    String.Empty,
    'Version code (1).',
    procedure(const AValue: integer) begin
      TProjectOptions.VersionCodeCommand := AValue;
    end);
  LOption.Required := false;

  LOption := LCmd.RegisterOption<string>(
    'version_name',
    String.Empty,
    'Version name (1.0.0).',
    procedure(const AValue: string) begin
      TProjectOptions.VersionNameCommand := AValue;
    end);
  LOption.Required := false;

  LOption := LCmd.RegisterOption<string>(
    'python_version',
    String.Empty,
    Format('Python version (%s, %s and %s).', [
      TPythonVersion.cp38.AsString(),
      TPythonVersion.cp39.AsString(),
      TPythonVersion.cp310.AsString()]),
    procedure(const AValue: string) begin
      TProjectOptions.PythonVersionCommand := AValue;
    end);
  LOption.Required := false;

  LOption := LCmd.RegisterOption<string>(
    'architecture',
    String.Empty,
    Format('Architecture (%s or %s).', [
      TArchitecture.arm.AsString(),
      TArchitecture.aarch64.AsString]),
    procedure(const AValue: string) begin
      TProjectOptions.ArchitectureCommand := AValue;
    end);
  LOption.Required := false;

  //Icons
  LOption := LCmd.RegisterOption<string>(
    'drawable_small',
    String.Empty,
    'Drawable small.',
    procedure(const AValue: string) begin
      TProjectOptions.DrawableSmallCommand := AValue;
    end);
  LOption.Required := false;

  LOption := LCmd.RegisterOption<string>(
    'drawable_normal',
    String.Empty,
    'Drawable normal.',
    procedure(const AValue: string) begin
      TProjectOptions.DrawableNormalCommand := AValue;
    end);
  LOption.Required := false;

  LOption := LCmd.RegisterOption<string>(
    'drawable_large',
    String.Empty,
    'Drawable large.',
    procedure(const AValue: string) begin
      TProjectOptions.DrawableLargeCommand := AValue;
    end);
  LOption.Required := false;

  LOption := LCmd.RegisterOption<string>(
    'drawable_xlarge',
    String.Empty,
    'Drawable xlarge.',
    procedure(const AValue: string) begin
      TProjectOptions.DrawableXLargeCommand := AValue;
    end);
  LOption.Required := false;

  LOption := LCmd.RegisterOption<string>(
    'drawable_ldpi',
    String.Empty,
    'Drawable ldpi.',
    procedure(const AValue: string) begin
      TProjectOptions.DrawableLDpiCommand := AValue;
    end);
  LOption.Required := false;

  LOption := LCmd.RegisterOption<string>(
    'drawable_mdpi',
    String.Empty,
    'Drawable mdpi.',
    procedure(const AValue: string) begin
      TProjectOptions.DrawableMDpiCommand := AValue;
    end);
  LOption.Required := false;

  LOption := LCmd.RegisterOption<string>(
    'drawable_hdpi',
    String.Empty,
    'Drawable hdpi.',
    procedure(const AValue: string) begin
      TProjectOptions.DrawableHDpiCommand := AValue;
    end);
  LOption.Required := false;

  LOption := LCmd.RegisterOption<string>(
    'drawable_xhdpi',
    String.Empty,
    'Drawable xhdpi.',
    procedure(const AValue: string) begin
      TProjectOptions.DrawableXHDpiCommand := AValue;
    end);
  LOption.Required := false;

  LOption := LCmd.RegisterOption<string>(
    'drawable_xxhdpi',
    String.Empty,
    'Drawable xxhdpi.',
    procedure(const AValue: string) begin
      TProjectOptions.DrawableXxHDpiCommand := AValue;
    end);
  LOption.Required := false;

  LOption := LCmd.RegisterOption<string>(
    'drawable_xxxhdpi',
    String.Empty,
    'Drawable xxxhdpi.',
    procedure(const AValue: string) begin
      TProjectOptions.DrawableXxxHDpiCommand := AValue;
    end);
  LOption.Required := false;

  //Files
  LOption := LCmd.RegisterOption<string>(
    'main_file',
    String.Empty,
    'Set the project main file.',
    procedure(const AValue: string) begin
      TProjectOptions.MainFileCommand := AValue;
    end);
  LOption.Required := false;

  LOption := LCmd.RegisterOption<string>(
    'add_file',
    String.Empty,
    'Add a file.',
    procedure(const AValue: string) begin
      TProjectOptions.AddFileCommand := TProjectOptions.AddFileCommand + [AValue];
    end);
  LOption.Required := false;
  LOption.AllowMultiple := true;

  LOption := LCmd.RegisterOption<string>(
    'remove_file',
    String.Empty,
    'Remove a file.',
    procedure(const AValue: string) begin
      TProjectOptions.RemoveFileCommand := TProjectOptions.RemoveFileCommand + [AValue];
    end);
  LOption.Required := false;
  LOption.AllowMultiple := true;

  LOption := LCmd.RegisterOption<string>(
    'add_dependency',
    String.Empty,
    'Add a dependency.',
    procedure(const AValue: string) begin
      TProjectOptions.AddDependencyCommand := TProjectOptions.AddDependencyCommand + [AValue];
    end);
  LOption.Required := false;
  LOption.AllowMultiple := true;

  LOption := LCmd.RegisterOption<string>(
    'remove_dependency',
    String.Empty,
    'Remove a dependency.',
    procedure(const AValue: string) begin
      TProjectOptions.RemoveDependencyCommand := TProjectOptions.RemoveDependencyCommand + [AValue];
    end);
  LOption.Required := false;
  LOption.AllowMultiple := true;

  if TFile.Exists(GetGUIEntityEditorPath()) then begin
    LOption := LCmd.RegisterOption<boolean>(
      'gui',
      String.Empty,
      'Edit a project settings using the GUI editor.',
      procedure(const AValue: boolean) begin
        TProjectOptions.Gui := AValue;
      end);
    LOption.Required := false;
    LOption.HasValue := false;

    LCmd.Examples.Add('project my_project -gui');
  end;

  LCmd.Examples.Add('project my_project --package_name com.embarcadero.my_app --version_name 1.0.0 --version_code 1');
  LCmd.Examples.Add('project my_project --package_name com.embarcadero.my_app --version_name 1.0.0 --python_version 3.9 --architecture arm64');
  LCmd.Examples.Add('project my_project --package_name com.embarcadero.my_app --version_name 1.0.0 --drawable_small "my_icon_path"');
  LCmd.Examples.Add('project my_project --main_file "main_file_path"');
  LCmd.Examples.Add('project my_project --add_file "file_path_1" --add_file "file_path_2"');
  LCmd.Examples.Add('project my_project --remove_file "file_path_1"');
  LCmd.Examples.Add('project my_project --add_file "file_path_1" --add_file "file_path_3" --remove_file "file_path_2"');
  LCmd.Examples.Add('project my_project --add_dependency "file_path_1" --add_dependency "file_path_2" --remove_dependency "file_path_3"');
end;

procedure ConfigureOptions();
begin
  TOptionsRegistry.DescriptionTab := 35;
  TOptionsRegistry.NameValueSeparator := ' ';
  ConfigureHelpOptions();
  ConfigureGlobalOptions();
  ConfigureDeviceOptions();
  ConfigureCreateOptions();
  ConfigureListOptions();
  ConfigureRemoveOptions();
  ConfigureBuildOptions();
  ConfigureDeployOptions();
  ConfigureRunOptions();
  ConfigureStopOptions();
  ConfigureEnvironment();
  ConfigureProject();
end;

initialization
  ConfigureOptions();

end.
