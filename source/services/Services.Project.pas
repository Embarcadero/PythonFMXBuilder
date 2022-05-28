unit Services.Project;

interface

uses
  Services, System.IOUtils, System.Classes, Model.Project, System.SysUtils,
  System.Generics.Collections;

type
  TProjectService = class(TInterfacedObject, IProjectServices)
  private
    function TestProject(const AModel: TProjectModel): boolean;
  public
    function CreateProject(const AApplicationName: string): TProjectModel;
    procedure SaveProject(const AProject: TProjectModel);
    function LoadProject(const AApplicationName: string): TProjectModel;
    function ListProjects(): TArray<string>;
    function HasProject(const AApplicationName: string): boolean;

    function AddScriptFile(const AModel: TProjectModel;
      const AFilePath: string): boolean;
    procedure RemoveScriptFile(const AModel: TProjectModel;
      const AFilePath: string);
    function GetScriptFiles(const AModel: TProjectModel): TArray<string>;
  end;

implementation

uses
  Storage.Default;

{ TProjectService }

function TProjectService.HasProject(const AApplicationName: string): boolean;
begin
  var LStorage := TDefaultStorage<TProjectModel>.Make();
  var LModel: TProjectModel := nil;
  try
    Result := LStorage.LoadModel(LModel, String.Empty, AApplicationName);
  finally
    LModel.Free();
  end;
end;

function TProjectService.TestProject(const AModel: TProjectModel): boolean;
begin
  Result := HasProject(AModel.ApplicationName);
end;

function TProjectService.CreateProject(const AApplicationName: string): TProjectModel;
begin
  Result := TProjectModel.Create(AApplicationName);
end;

function TProjectService.ListProjects: TArray<string>;
begin
  var LStorage := TDefaultStorage<TProjectModel>.Make();
  var LModels := LStorage.ListModels();
  if Length(LModels) > 0 then begin
    try
      var LList := TList<string>.Create();
      try
        for var LModel in LModels do begin
          if TestProject(LModel) then
            LList.Add(LModel.ApplicationName);
        end;
        Result := LList.ToArray();
      finally
        LList.Free();
      end;
    finally
      for var LModel in LModels do begin
        LModel.Free();
      end;
    end;
  end else
    Result := [];
end;

function TProjectService.LoadProject(const AApplicationName: string): TProjectModel;
begin
  var LStorage := TDefaultStorage<TProjectModel>.Make();
  if not LStorage.LoadModel(Result, String.Empty, AApplicationName) then
    raise Exception.CreateFmt('Project %s not found.', [AApplicationName]);
end;

function TProjectService.AddScriptFile(const AModel: TProjectModel;
  const AFilePath: string): boolean;
begin
  //We are not accepting duplicated file names
  for var LFile in AModel.Files.ScriptFiles do begin
    if TPath.GetFileName(LFile) = TPath.GetFileName(AFilePath) then
      Exit(false);
  end;

  //Should we copy this file to a local dir?
  AModel.Files.ScriptFiles.Add(AFilePath);
  Result := true;
end;

procedure TProjectService.RemoveScriptFile(const AModel: TProjectModel;
  const AFilePath: string);
begin
  AModel.Files.ScriptFiles.Remove(AFilePath);
end;

procedure TProjectService.SaveProject(const AProject: TProjectModel);
begin
  var LStorage := TDefaultStorage<TProjectModel>.Make();
  LStorage.SaveModel(AProject);
end;

function TProjectService.GetScriptFiles(const AModel: TProjectModel): Tarray<string>;
begin
  Result := AModel.Files.ScriptFiles.ToArray();
end;

end.
