unit Model.Project;

interface

uses
  System.Classes, REST.Json.Types, Model, Architecture, PythonVersion;

type
  [Model('project')]
  TProjectModel = class(TModel)
  private
    [JSONName('application_name')]
    FApplicationName: string;
    [JSONName('package_name')]
    FPackageName: string;
    [JSONName('version_code')]
    FVersionCode: integer;
    [JSONName('version_name')]
    FVersionName: string;
    [JSONName('python_version')]
    FPythonVersion: TPythonVersion;
    [JSONName('architecture')]
    FArchitecture: TArchitecture;
  public
    function Validate(const AErrors: TStrings): boolean; override;
  public
    property ApplicationName: string read FApplicationName write FApplicationName;
    property PackageName: string read FPackageName write FPackageName;
    property VersionCode: integer read FVersionCode write FVersionCode;
    property VersionName: string read FVersionName write FVersionName;
    property PythonVersion: TPythonVersion read FPythonVersion write FPythonVersion;
    property Architecture: TArchitecture read FArchitecture write FArchitecture;
  end;

implementation

uses
  System.SysUtils;

{ TProjectModel }

function TProjectModel.Validate(const AErrors: TStrings): boolean;
begin
  AErrors.Clear();

  {|||||| CHECK FOR PATHS |||||||}
  if FApplicationName.Trim().IsEmpty() then
    AErrors.Add('* The Application Name can not be empty.');

  if FPackageName.Trim().IsEmpty() then
    AErrors.Add('* The Package Name can not be empty.');

  if FVersionCode <= 0 then
    AErrors.Add('* The Version Code must be greater than 0.');

  if FVersionName.Trim().IsEmpty() then
    AErrors.Add('* The Version Name can not be empty.');

  Result := (AErrors.Count = 0);
end;

end.
