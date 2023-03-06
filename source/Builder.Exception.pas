unit Builder.Exception;

interface

uses
  System.SysUtils;

type
  EProjectNotFound = class(Exception);

  EInvalidArchitecture = class(Exception);

  EInvalidPythonVersion = class(Exception);

  EInvalidEnvironment = class(Exception);

  EInvalidRunMode = class(Exception);

  EEmptySettings = class(Exception);

  EModelValidationError = class(Exception);

  EBuildFailed = class(Exception);

  EInstallFailed = class(Exception);

  ENoActiveDevice = class(Exception);

implementation

end.
