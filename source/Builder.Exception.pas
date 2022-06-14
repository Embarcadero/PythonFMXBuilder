unit Builder.Exception;

interface

uses
  System.SysUtils;

type
  EProjectNotFound = class(Exception);

  EInvalidArchitecture = class(Exception);

  EInvalidPythonVersion = class(Exception);

implementation

end.
