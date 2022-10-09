unit Builder.Model.Project.Files;

interface

uses
  System.Classes, System.Generics.Collections, REST.Json.Types, System.SysUtils,
  Builder.Model;

type
  TProjectFilesModel = class(TModel)
  private
    [JSONName('files')]
    FFiles: TList<string>;
    [JSONName('main_file')]
    FMainFile: string;
    [JSONName('dependencies')]
    FDependencies: TList<string>;
  public
    constructor Create(); override;
    destructor Destroy(); override;

    function Validate(const AErrors: TStrings): boolean; override;
  public
    property Files: TList<string> read FFiles write FFiles;
    property MainFile: string read FMainFile write FMainFile;
    property Dependencies: TList<string> read FDependencies write FDependencies;
  end;

implementation

uses
  System.IOUtils;

{ TProjectFilesModel }

constructor TProjectFilesModel.Create;
begin
  inherited;
  FFiles := TList<string>.Create();
  FDependencies := TList<string>.Create();
end;

destructor TProjectFilesModel.Destroy;
begin
  FDependencies.Free();
  FFiles.Free();
  inherited;
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

  for var LFile in FDependencies do begin
    if not TFile.Exists(LFile) then begin
      Result := false;
      AErrors.Add(Format('* Dependency %s not found.', [LFile]))
    end;
  end;
end;

end.
