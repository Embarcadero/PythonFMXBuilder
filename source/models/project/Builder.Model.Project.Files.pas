unit Builder.Model.Project.Files;

interface

uses
  System.Classes, System.Generics.Collections, REST.Json.Types, System.SysUtils,
  Builder.Model;

type
  TProjectFilesModule = class;
  TProjectFilesModules = class;

  TProjectFilesDependency = class;
  TProjectFilesDependencies = class;

  TProjectFilesPackage = class;
  TProjectFilesPackages = class;

  TProjectFilesOther = class;
  TProjectFilesOthers = class;

  [JSONOwned, JSONOwnedReflect]
  TProjectFilesModel = class(TModel)
  private
    [JSONName('main')]
    FMain: string;
    [JSONName('modules')]
    FModules: TProjectFilesModules;
    [JSONName('packages')]
    FPackages: TProjectFilesPackages;
    [JSONName('others')]
    FOthers: TProjectFilesOthers;
    [JSONName('dependencies')]
    FDependencies: TProjectFilesDependencies;
    function GetModules: TProjectFilesModules;
    function GetDependencies: TProjectFilesDependencies;
    function GetPackages: TProjectFilesPackages;
    function GetOthers: TProjectFilesOthers;
  public
    constructor Create(); override;
    destructor Destroy(); override;

    function Validate(const AErrors: TStrings): boolean; override;
  public
    //User main module
    property Main: string read FMain write FMain;
    //User modules
    property Modules: TProjectFilesModules read GetModules write FModules;
    //Packages as Zip Imports and/or Wheels
    property Packages: TProjectFilesPackages read GetPackages write FPackages;
    //Other user files
    property Others: TProjectFilesOthers read GetOthers write FOthers;
    //Internal dependencies
    property Dependencies: TProjectFilesDependencies read GetDependencies write FDependencies;
  end;

  TProjectFileClass = class of TProjectFile;

  TProjectFile = class
  private
    [JSONName('name')]
    FName: string;
    [JSONName('path')]
    FPath: string;
  public
    constructor Create(const AName, APath: string); overload;
    constructor Create(const APath: string); overload;

    property Name: string read FName write FName;
    property Path: string read FPath write FPath;
  end;

  TProjectFiles<T: TProjectFile> = class(TObjectList<T>);

  TProjectFilesModule = class(TProjectFile);

  TProjectFilesModules = class(TProjectFiles<TProjectFilesModule>);

  TProjectFilesDependency = class(TProjectFile);

  TProjectFilesDependencies = class(TProjectFiles<TProjectFilesDependency>);

  TProjectFilesPackage = class(TProjectFile);

  TProjectFilesPackages = class(TProjectFiles<TProjectFilesPackage>);

  TProjectFilesOther = class(TProjectFile);

  TProjectFilesOthers = class(TProjectFiles<TProjectFilesOther>);

implementation

uses
  System.IOUtils;

{ TProjectFilesModel }

constructor TProjectFilesModel.Create;
begin
  inherited;
end;

destructor TProjectFilesModel.Destroy;
begin
  FOthers.Free();
  FPackages.Free();
  FDependencies.Free();
  FModules.Free();
  inherited;
end;

function TProjectFilesModel.GetDependencies: TProjectFilesDependencies;
begin
  if not Assigned(FDependencies) then
    FDependencies := TProjectFilesDependencies.Create();
  Result := FDependencies;
end;

function TProjectFilesModel.GetModules: TProjectFilesModules;
begin
  if not Assigned(FModules) then
    FModules := TProjectFilesModules.Create();
  Result := FModules;
end;

function TProjectFilesModel.GetOthers: TProjectFilesOthers;
begin
  if not Assigned(FOthers) then
    FOthers := TProjectFilesOthers.Create();
  Result := FOthers;
end;

function TProjectFilesModel.GetPackages: TProjectFilesPackages;
begin
  if not Assigned(FPackages) then
    FPackages := TProjectFilesPackages.Create();
  Result := FPackages;
end;

function TProjectFilesModel.Validate(const AErrors: TStrings): boolean;
begin
  Result := true;
  var LMainFileExists := false;
  for var LModule in GetModules do begin
    if not TFile.Exists(LModule.Path) then begin
      Result := false;
      AErrors.Add(Format('* Module %s not found.', [LModule.Path]))
    end;
    //Let's make sure the main file is part of the project files list
    if not LMainFileExists and not Main.IsEmpty() then
      LMainFileExists := (TPath.GetFileName(LModule.Path) = Main);
  end;

  if Main.IsEmpty() then begin
    Result := false;
    AErrors.Add('* Main script file is empty.');
  end else if not LMainFileExists then begin
    Result := false;
    AErrors.Add('* Main script file not found.');
  end;

  for var LDependency in GetDependencies do begin
    if not TFile.Exists(LDependency.Path) then begin
      Result := false;
      AErrors.Add(Format('* Dependency %s not found.', [LDependency.Path]))
    end;
  end;

  for var LPackage in GetPackages do begin
    if not TFile.Exists(LPackage.Path) then begin
      Result := false;
      AErrors.Add(Format('* Package %s not found.', [LPackage.Path]))
    end;
  end;

  for var LOthers in GetOthers do begin
    if not TFile.Exists(LOthers.Path) then begin
      Result := false;
      AErrors.Add(Format('* File %s not found.', [LOthers.Path]))
    end;
  end;
end;

{ TProjectFile }

constructor TProjectFile.Create(const AName, APath: string);
begin
  inherited Create();
  FName := AName;
  FPath := APath;
end;

constructor TProjectFile.Create(const APath: string);
begin
  Create(TPath.GetFileName(APath), APath);
end;

end.
