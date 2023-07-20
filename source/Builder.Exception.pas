unit Builder.Exception;

interface

uses
  System.SysUtils;

type
  EBuilderError = class(Exception);

  EProjectNotFound = class(EBuilderError);

  EInvalidArchitecture = class(EBuilderError);

  EInvalidPythonVersion = class(EBuilderError);

  EInvalidEnvironment = class(EBuilderError);

  EInvalidRunMode = class(EBuilderError);

  EInvalidBuildConfiguration = class(EBuilderError);

  EEmptySettings = class(EBuilderError);

  EModelValidationError = class(EBuilderError);

  EModuleAlreadyExists = class(EBuilderError);

  EBuildFailed = class(EBuilderError);

  EInstallFailed = class(EBuilderError);

  ENoActiveDevice = class(EBuilderError);

  EApkFileNotFound = class(EBuilderError);

  EPreBuiltFolderNotFound = class(EBuilderError);

  EPythonZipFileNotFound = class(EBuilderError);

  EMustOpenOrCreateProject = class(EBuilderError);

  EUnableToSaveEntity = class(EBuilderError);

  ERequestTypeAttributeNotFound = class(EBuilderError);

  EEventTypeAttributeNotFound = class(EBuilderError);

  EDebuggerError = class(EBuilderError);

  EDebuggerStillConnected = class(EDebuggerError);

  EDebuggerIsBusy = class(EDebuggerError);

  EDebuggerNotStarted = class(EDebuggerError);

  EDebuggerNotConnected = class(EDebuggerError);

  EFailedToInitializeDebugger = class(EDebuggerError);

  EPythonInterpreterNotFound = class(EBuilderError);

  EPythonExecutableNotFound = class(EBuilderError);

  EPythonDistributionNotFound = class(EBuilderError);

implementation

end.
