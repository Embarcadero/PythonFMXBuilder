unit Builder.Model.Project.Files;

interface

uses
  System.Classes, System.Generics.Collections, REST.Json.Types, System.SysUtils,
  Builder.Model;

type
  [JSONOwned, JSONOwnedReflect]
  TProjectFilesModel = class(TModel)
  private
    [JSONName('files')]
    FFiles: TList<string>;
    [JSONName('main_file')]
    FMainFile: string;
    [JSONName('dependencies')]
    FDependencies: TList<string>;
    [JSONName('packages')]
    FPackages: TList<string>;
    [JSONName('others')]
    FOthers: TList<string>;
    function GetDependencies: TList<string>;
    function GetFiles: TList<string>;
    function GetOthers: TList<string>;
    function GetPackages: TList<string>;
  public
    constructor Create(); override;
    destructor Destroy(); override;

    function Validate(const AErrors: TStrings): boolean; override;
  public
    //User modules
    property Files: TList<string> read GetFiles write FFiles;
    //User main module
    property MainFile: string read FMainFile write FMainFile;
    //Internal dependency
    property Dependencies: TList<string> read GetDependencies write FDependencies;
    //Packages as Zip Imports and/or Wheels
    property Packages: TList<string> read GetPackages write FPackages;
    //Other user files
    property Others: TList<string> read GetOthers write FOthers;
  end;

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
  FFiles.Free();
  inherited;
end;

function TProjectFilesModel.GetDependencies: TList<string>;
begin
  if not Assigned(FDependencies) then
    FDependencies := TList<string>.Create();
  Result := FDependencies;
end;

function TProjectFilesModel.GetFiles: TList<string>;
begin
  if not Assigned(FFiles) then
    FFiles := TList<string>.Create();
  Result := FFiles;
end;

function TProjectFilesModel.GetOthers: TList<string>;
begin
  if not Assigned(FOthers) then
    FOthers := TList<string>.Create();
  Result := FOthers;
end;

function TProjectFilesModel.GetPackages: TList<string>;
begin
  if not Assigned(FPackages) then
    FPackages := TList<string>.Create();
  Result := FPackages;
end;

function TProjectFilesModel.Validate(const AErrors: TStrings): boolean;
begin
  Result := true;
  var LMainFileExists := false;
  for var LFile in FFiles do begin
    if not TFile.Exists(LFile) then begin
      Result := false;
      AErrors.Add(Format('* File %s not found.', [LFile]))
    end;
    //Let's make sure the main file is part of the project files list
    if not LMainFileExists and not MainFile.Trim().IsEmpty() then
      LMainFileExists := (TPath.GetFileName(LFile) = MainFile);
  end;

  if MainFile.Trim().IsEmpty() then begin
    Result := false;
    AErrors.Add('* Main script file is empty.');
  end else if not LMainFileExists then begin
    Result := false;
    AErrors.Add('* Main script file not found.');
  end;

  for var LFile in GetDependencies do begin
    if not TFile.Exists(LFile) then begin
      Result := false;
      AErrors.Add(Format('* Dependency %s not found.', [LFile]))
    end;
  end;

  for var LPackage in GetPackages do begin
    if not TFile.Exists(LPackage) then begin
      Result := false;
      AErrors.Add(Format('* Package %s not found.', [LPackage]))
    end;
  end;

  for var LOthers in GetOthers do begin
    if not TFile.Exists(LOthers) then begin
      Result := false;
      AErrors.Add(Format('* File %s not found.', [LOthers]))
    end;
  end;
end;

end.
