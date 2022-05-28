unit Model.Project.Files;

interface

uses
  System.Classes, System.Generics.Collections, REST.Json.Types, Model,
  System.SysUtils;

type
  TProjectFilesModel = class(TModel)
  private
    [JSONName('script_files')]
    FScriptFiles: TList<string>;
  public
    constructor Create(); override;
    destructor Destroy(); override;

    function Validate(const AErrors: TStrings): boolean; override;
  public
    property ScriptFiles: TList<string> read FScriptFiles write FScriptFiles;
  end;

implementation

uses
  System.IOUtils;

{ TProjectFilesModel }

constructor TProjectFilesModel.Create;
begin
  inherited;
  FScriptFiles := TList<string>.Create();
end;

destructor TProjectFilesModel.Destroy;
begin
  FScriptFiles.Free();
  inherited;
end;

function TProjectFilesModel.Validate(const AErrors: TStrings): boolean;
begin
  Result := true;
  for var LFile in FScriptFiles do begin
    if not TFile.Exists(LFile) then begin
      Result := false;
      AErrors.Add(Format('* Script file %s not found.', [LFile]))
    end;
  end;
end;

end.
