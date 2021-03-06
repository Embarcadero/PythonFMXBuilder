unit Builder.Services.Project;

interface

uses
  Builder.Services, System.IOUtils, System.Classes, Builder.Model.Project, System.SysUtils,
  System.Generics.Collections;

type
  TProjectService = class(TInterfacedObject, IProjectServices)
  private
    function GetBasePath(): string;
    function GetProjectFilesPath(const AProjectName: string): string;
    function TestProject(const AModel: TProjectModel): boolean;
  public
    function CreateProject(const AApplicationName: string;
      const AAddMainScript: boolean = true): TProjectModel;
    procedure SaveProject(const AProject: TProjectModel);
    function LoadProject(const AApplicationName: string): TProjectModel;
    function ListProjects(): TArray<string>;
    function HasProject(const AApplicationName: string): boolean;
    function RemoveProject(const AAplicationName: string): boolean;

    function AddMainScriptFile(const AModel: TProjectModel): string;
    procedure SetMainScriptFile(const AModel: TProjectModel;
      const AFilePath: string);
    function IsMainScriptFile(const AModel: TProjectModel;
      const AFilePath: string): boolean;

    function AddScriptFile(const AModel: TProjectModel;
      const AFilePath: string): boolean;
    procedure RemoveScriptFile(const AModel: TProjectModel;
      const AFilePath: string);
    function GetScriptFiles(const AModel: TProjectModel): TArray<string>;
  end;

implementation

uses
  Builder.Exception,
  Builder.Storage.Default;

{ TProjectService }

function TProjectService.GetBasePath: string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), 'files');
end;

function TProjectService.GetProjectFilesPath(const AProjectName: string): string;
begin
  Result := TPath.Combine(GetBasePath(), AProjectName);
end;

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

function TProjectService.IsMainScriptFile(const AModel: TProjectModel;
  const AFilePath: string): boolean;
begin
  Result := TPath.GetFileName(AFilePath) = AModel.Files.MainFile;
end;

function TProjectService.TestProject(const AModel: TProjectModel): boolean;
begin
  Result := HasProject(AModel.ApplicationName);
end;

function TProjectService.CreateProject(const AApplicationName: string;
  const AAddMainScript: boolean): TProjectModel;
begin
  Result := TProjectModel.Create(AApplicationName);

  if AAddMainScript then begin
    AddMainScriptFile(Result);
  end;
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
  Result := nil;
  var LStorage := TDefaultStorage<TProjectModel>.Make();
  if not LStorage.LoadModel(Result, String.Empty, AApplicationName) then
    raise EProjectNotFound.CreateFmt('Project %s not found.', [AApplicationName]);
end;

function TProjectService.AddMainScriptFile(const AModel: TProjectModel): string;
const
  SCRIPT_TEXT =
      'from delphifmx import *'
    + #13#10
    + #13#10
    + 'MainForm = Form(Application)'
    + #13#10
    + 'MainForm.SetProps(Caption = "Hello World")'
    + #13#10
    + 'msg = Label(MainForm)'
    + #13#10
    + 'msg.SetProps(Parent = MainForm,'
    + #13#10
    + '    Text = "Hello Python from Delphi FMX",'
    + #13#10
    + '    Position = Position(PointF(50, 50)),'
    + #13#10
    + '    Width = 300)'
    + #13#10
    + 'MainForm.Show()';
begin
  var LProjectFilesFolder := GetProjectFilesPath(AModel.ApplicationName);
  if not TDirectory.Exists(LProjectFilesFolder) then
    TDirectory.CreateDirectory(LProjectFilesFolder);

  var LMainScriptPath := TPath.Combine(
    GetProjectFilesPath(AModel.ApplicationName),
    'main.py');

  if not TFile.Exists(LMainScriptPath) then begin
    with TFile.Create(LMainScriptPath) do begin
      try
        WriteData(
          TEncoding.UTF8.GetBytes(SCRIPT_TEXT),
          TEncoding.UTF8.GetByteCount(SCRIPT_TEXT));
      finally
        Free();
      end;
    end;
  end;

  //Save the script file in the model files
  AddScriptFile(AModel, LMainScriptPath);
  //Once we add the main file, we automatically set it as the main file
  SetMainScriptFile(AModel, LMainScriptPath);
end;

function TProjectService.AddScriptFile(const AModel: TProjectModel;
  const AFilePath: string): boolean;
begin
  //We are not accepting duplicated file names
  for var LFile in AModel.Files.Files do begin
    if TPath.GetFileName(LFile) = TPath.GetFileName(AFilePath) then
      Exit(false);
  end;

  //Should we copy this file to a local dir?
  AModel.Files.Files.Add(AFilePath);
  Result := true;
end;

function TProjectService.RemoveProject(const AAplicationName: string): boolean;
begin
  var LProjectModel := LoadProject(AAplicationName);
  var LStorage := TDefaultStorage<TProjectModel>.Make();
  Result := LStorage.DeleteModel(LProjectModel);

  if not Result then
    Exit;

  var LProjectFilesFolder := GetProjectFilesPath(LProjectModel.ApplicationName);
  TDirectory.Delete(LProjectFilesFolder, true);
end;

procedure TProjectService.RemoveScriptFile(const AModel: TProjectModel;
  const AFilePath: string);
begin
  AModel.Files.Files.Remove(AFilePath);
  //If we remove the main file, then we update it to empty.
  if (AModel.Files.MainFile = TPath.GetFileName(AFilePath)) then
    AModel.Files.MainFile := String.Empty;
end;

procedure TProjectService.SaveProject(const AProject: TProjectModel);
begin
  var LStorage := TDefaultStorage<TProjectModel>.Make();
  LStorage.SaveModel(AProject);
end;

procedure TProjectService.SetMainScriptFile(const AModel: TProjectModel;
  const AFilePath: string);
begin
  AModel.Files.MainFile := TPath.GetFileName(AFilePath);
end;

function TProjectService.GetScriptFiles(const AModel: TProjectModel): Tarray<string>;
begin
  Result := AModel.Files.Files.ToArray();
end;

end.
