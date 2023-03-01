unit Cli.Exception;

interface

uses
  System.SysUtils;

type
  ECliException = class(Exception)
  private
    FCode: integer;
  public
    constructor Create(const AMessage: string; const ACode: integer);
    constructor CreateFmt(const AMessage: string; const ACode: integer; const AArgs: array of const);

    property Code: integer read FCode write FCode;
  end;

  ERemoveProjectFailure = class(ECliException)
  public
    constructor Create();
  end;

  EBuildProcessFailed = class(ECliException)
  public
    constructor Create();
  end;

  EDeployProcessFailed = class(ECliException)
  public
    constructor Create();
  end;

  ERunProcessFailed = class(ECliException)
  public
    constructor Create();
  end;

  EStopProcessFailed = class(ECliException)
  public
    constructor Create();
  end;

  ENoDevicesAttached = class(ECliException)
  public
    constructor Create();
  end;

  EDeviceNotAttached = class(ECliException)
  public
    constructor Create(const ADevice: string);
  end;

  EInvalidBasePath = class(ECliException);

  EProjectNotFound = class(ECliException)
  public
    constructor Create(const AProjectName: string);
  end;

  EInvalidPythonVersion = class(ECliException)
  public
    constructor Create();
  end;

  EInvalidArchitecture = class(ECliException)
  public
    constructor Create();
  end;

  EEnvironmentSettingsAreEmpty = class(ECliException)
  public
    constructor Create();
  end;

  EEnvironmentSettingsInvalidArgs = class(ECliException)
  public
    constructor Create(const AArgs: string);
  end;

  EProjectSettingsAreEmpty = class(ECliException)
  public
    constructor Create();
  end;

  EProjectSettingsInvalidArgs = class(ECliException)
  public
    constructor Create(const AArgs: string);
  end;

resourcestring
  E_STR_BUILD_PROCESS_FAILED                 = 'Build process has failed.';
  E_STR_DEPLOY_PROCESS_FAILED                = 'Deploy process has failed.';
  E_STR_RUN_PROCESS_FAILED                   = 'Run process has failed.';
  E_STR_STOP_PROCESS_FAILED                  = 'Stop process has failed.';
  E_STR_NO_DEVICES_ATTACHED                  = 'No devices attached.';
  E_STR_DEVICE_NOT_ATTACHED                  = 'Device %s not attached.';
  E_STR_INVALID_SDK_BASE_PATH                = 'Invalid SDK base path.';
  E_STR_INVALID_JDK_BASE_PATH                = 'Invalid JDK base path.';
  E_STR_PROJECT_NOT_FOUND                    = 'Project %s not found.';
  E_STR_INVALID_PYTHON_VERSION               = 'Invalid Python version.';
  E_STR_INVALID_ARCHITECTURE                 = 'Invalid architecture.';
  E_STR_ENVIRONMENT_SETTINGS_EMPTY           = 'The Environment Settings are empty.';
  E_STR_ENVIRONMENT_SETTINGS_INVALID_ARGS    = 'The Environment Settings has invalid arguments: %s';
  E_STR_PROJECT_SETTINGS_EMPTY               = 'The Project Settings are empty.';
  E_STR_PROJECT_SETTINGS_INVALID_ARGS        = 'The Project Settings has invalid arguments: %s';
  E_STR_REMOVE_PROJECT_FAILURE               = 'Remove project failed.';

const
  //Build actions
  E_CODE_BUILD_PROCESS_FAILED                = 101;
  E_CODE_DEPLOY_PROCESS_FAILED               = 102;
  E_CODE_RUN_PROCESS_FAILED                  = 103;
  E_CODE_STOP_PROCESS_FAILED                 = 104;
  //Environment settings and actions
  E_CODE_ENVIRONMENT_SETTINGS_EMPTY          = 201;
  E_CODE_ENVIRONMENT_SETTINGS_INVALID_ARGS   = 202;
  E_CODE_INVALID_SDK_BASE_PATH               = 203;
  E_CODE_INVALID_JDK_BASE_PATH               = 204;
  //Project settings and actions
  E_CODE_PROJECT_SETTINGS_EMPTY              = 301;
  E_CODE_PROJECT_SETTINGS_INVALID_ARGS       = 302;
  E_CODE_INVALID_PYTHON_VERSION              = 303;
  E_CODE_INVALID_ARCHITECTURE                = 304;
  E_CODE_PROJECT_NOT_FOUND                   = 305;
  E_CODE_PROJECT_REMOVE_FAILURE              = 306;
  //Miscellaneous
  E_CODE_NO_DEVICES_ATTACHED                 = 401;
  E_CODE_DEVICE_NOT_ATTACHED                 = 402;

implementation

{ ECliException }

constructor ECliException.Create(const AMessage: string; const ACode: integer);
begin
  inherited Create(AMessage);
  FCode := ACode;
end;

constructor ECliException.CreateFmt(const AMessage: string;
  const ACode: integer; const AArgs: array of const);
begin
  inherited CreateFmt(AMessage, AArgs);
  FCode := ACode;
end;

{ EBuildProcessFailed }

constructor EBuildProcessFailed.Create;
begin
  inherited Create(E_STR_BUILD_PROCESS_FAILED, E_CODE_BUILD_PROCESS_FAILED);
end;

{ EDeployProcessFailed }

constructor EDeployProcessFailed.Create;
begin
  inherited Create(E_STR_DEPLOY_PROCESS_FAILED, E_CODE_DEPLOY_PROCESS_FAILED);
end;

{ ERunProcessFailed }

constructor ERunProcessFailed.Create;
begin
  inherited Create(E_STR_RUN_PROCESS_FAILED, E_CODE_STOP_PROCESS_FAILED);
end;

{ EStopProcessFailed }

constructor EStopProcessFailed.Create;
begin
  inherited Create(E_STR_STOP_PROCESS_FAILED, E_CODE_STOP_PROCESS_FAILED);
end;

{ ENoDevicesAttached }

constructor ENoDevicesAttached.Create;
begin
  inherited Create(E_STR_NO_DEVICES_ATTACHED, E_CODE_NO_DEVICES_ATTACHED);
end;

{ EDeviceNotAttached }

constructor EDeviceNotAttached.Create(const ADevice: string);
begin
  inherited CreateFmt(E_STR_DEVICE_NOT_ATTACHED, E_CODE_DEVICE_NOT_ATTACHED, [ADevice]);
end;

{ EProjectNotFound }

constructor EProjectNotFound.Create(const AProjectName: string);
begin
  inherited CreateFmt(E_STR_PROJECT_NOT_FOUND, E_CODE_PROJECT_NOT_FOUND, [AProjectName]);
end;

{ EInvalidPythonVersion }

constructor EInvalidPythonVersion.Create;
begin
  inherited Create(E_STR_INVALID_PYTHON_VERSION, E_CODE_INVALID_PYTHON_VERSION);
end;

{ EInvalidArchitecture }

constructor EInvalidArchitecture.Create;
begin
  inherited Create(E_STR_INVALID_ARCHITECTURE, E_CODE_INVALID_ARCHITECTURE);
end;

{ EEnvironmentSettingsAreEmpty }

constructor EEnvironmentSettingsAreEmpty.Create;
begin
  inherited Create(E_STR_ENVIRONMENT_SETTINGS_EMPTY, E_CODE_ENVIRONMENT_SETTINGS_EMPTY);
end;

{ EEnvironmentSettingsInvalidArgs }

constructor EEnvironmentSettingsInvalidArgs.Create(const AArgs: string);
begin
  inherited CreateFmt(E_STR_ENVIRONMENT_SETTINGS_INVALID_ARGS, E_CODE_ENVIRONMENT_SETTINGS_INVALID_ARGS, [AArgs]);
end;

{ EProjectSettingsAreEmpty }

constructor EProjectSettingsAreEmpty.Create;
begin
  inherited Create(E_STR_PROJECT_SETTINGS_EMPTY, E_CODE_PROJECT_SETTINGS_EMPTY);
end;

{ EProjectSettingsInvalidArgs }

constructor EProjectSettingsInvalidArgs.Create(const AArgs: string);
begin
  inherited CreateFmt(E_STR_PROJECT_SETTINGS_INVALID_ARGS, E_CODE_PROJECT_SETTINGS_INVALID_ARGS, [AArgs]);
end;

{ EProjectRemoveFailure }

constructor ERemoveProjectFailure.Create;
begin
  inherited Create(E_STR_REMOVE_PROJECT_FAILURE, E_CODE_PROJECT_REMOVE_FAILURE);
end;

end.
