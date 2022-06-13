unit Builder.Model.Project;

interface

uses
  System.Classes, REST.Json.Types,
  Builder.Architecture, Builder.PythonVersion,
  Builder.Model, Builder.Model.Project.Icon, Builder.Model.Project.Files;

type
  PProjectModel = ^TProjectModel;
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
    [JSONName('icons')]
    FIcons: TProjectIconModel;
    [JSONName('files')]
    FFiles: TProjectFilesModel;
  private
    function GetId(): string;
  public
    constructor Create(); overload; override;
    constructor Create(const AApplicationName: string); reintroduce; overload;
    destructor Destroy(); override;

    function Validate(const AErrors: TStrings): boolean; override;
  public
    property Id: string read GetId;
    property ApplicationName: string read FApplicationName write FApplicationName;
    property PackageName: string read FPackageName write FPackageName;
    property VersionCode: integer read FVersionCode write FVersionCode;
    property VersionName: string read FVersionName write FVersionName;
    property PythonVersion: TPythonVersion read FPythonVersion write FPythonVersion;
    property Architecture: TArchitecture read FArchitecture write FArchitecture;
    property Icons: TProjectIconModel read FIcons write FIcons;
    property Files: TProjectFilesModel read FFiles write FFiles;
  end;

implementation

uses
  System.SysUtils;

{ TProjectModel }

constructor TProjectModel.Create;
begin
  inherited;
  FIcons := TProjectIconModel.Create();
  FFiles := TProjectFilesModel.Create();
end;

constructor TProjectModel.Create(const AApplicationName: string);
begin
  Create();
  FApplicationName := AApplicationName;
  FPackageName := 'com.embarcadero.' + AApplicationName;
  FVersionCode := 1;
  FVersionName := '1.0.0';
  FPythonVersion := TPythonVersion.cp39;
  FArchitecture := TArchitecture.aarch64;
end;

destructor TProjectModel.Destroy;
begin
  FFiles.Free();
  FIcons.Free();
  inherited;
end;

function TProjectModel.GetId: string;
begin
  Result := FApplicationName;
end;

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

  Result := Result
    and FIcons.Validate(AErrors)
      and FFiles.Validate(AErrors);
end;

end.
