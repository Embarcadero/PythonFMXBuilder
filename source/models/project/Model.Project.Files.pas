unit Model.Project.Files;

interface

uses
  System.Classes, System.Generics.Collections, REST.Json.Types, Model,
  System.SysUtils;

type
  TProjectFilesModel = class(TModel)
  private
    [JSONName('files')]
    FFiles: TList<string>;
  public
    constructor Create(); override;
    destructor Destroy(); override;

    function Validate(const AErrors: TStrings): boolean; override;
  public
    property Files: TList<string> read FFiles write FFiles;
  end;

implementation

uses
  System.IOUtils;

{ TProjectFilesModel }

constructor TProjectFilesModel.Create;
begin
  inherited;
  FFiles := TList<string>.Create();
end;

destructor TProjectFilesModel.Destroy;
begin
  FFiles.Free();
  inherited;
end;

function TProjectFilesModel.Validate(const AErrors: TStrings): boolean;
begin
  Result := true;
  var HasMainScript := false;
  for var LFile in FFiles do begin
    if TPath.GetFileName(LFile).ToLower() = 'main.py' then
      HasMainScript := true;
    if not TFile.Exists(LFile) then begin
      Result := false;
      AErrors.Add(Format('* Script file %s not found.', [LFile]))
    end;
  end;
  if not HasMainScript then begin
    Result := false;
    AErrors.Add('* Main script file not found.');
  end;
end;

end.
