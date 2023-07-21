unit Builder.Services.Project;

interface

uses
  Builder.Services, System.IOUtils, System.Classes, System.SysUtils,
  System.Generics.Collections,
  Builder.Model.Project, Builder.Model.Project.Files;

type
  TProjectService = class(TInterfacedObject, IProjectServices)
  private
    class var FActiveProject: TProjectModel;
  private
    class destructor Destroy();
  private
    function GetBasePath(): string;
    function GetProjectFilesPath(const AProjectName: string): string;
  public
    function CreateProject(const AProjectName,
      AMainModuleName: string): TProjectModel;
    procedure SaveProject(const AProjectPath: string;
      const AProject: TProjectModel);
    function RemoveProject(const AProjectName: string): boolean;
    //Open/close project in editor
    procedure OpenProject(const AProject: TProjectModel); overload;
    function OpenProject(const AProjectPath: string): TProjectModel; overload;
    procedure CloseProject();
    procedure RenameProject(const AModel: TProjectModel; const AProjectName: string);
    function HasActiveProject(): boolean;
    function GetActiveProject(): TProjectModel;
    procedure CheckActiveProject();

    //Main module
    function CreateMainModule(const AModel: TProjectModel;
      const AFilePath: string): string;
    procedure SetMainModule(const AModel: TProjectModel;
      const AFilePath: string);
    function IsMainModule(const AModel: TProjectModel;
      const AFilePath: string): boolean;

    //Modules
    function AddModule(const AModel: TProjectModel;
      const AFilePath: string): TProjectFilesModule;
    function GetModule(const AModel: TProjectModel;
      const AFilePath: string): TProjectFilesModule;
    procedure RemoveModule(const AModel: TProjectModel; const AFilePath: string);
    function GetModules(const AModel: TProjectModel): TProjectFilesModules;
    procedure CheckModuleExists(const AModel: TProjectModel;
      const AFilePath: string);
    procedure RenameModule(const AProject: TProjectModel;
      const AProjectModule: TProjectFilesModule; const AFilePath: string);

    //Dependencies
    function AddDependency(const AModel: TProjectModel;
      const AFilePath: string): TProjectFilesDependency;
    procedure RemoveDependency(const AModel: TProjectModel;
      const AFilePath: string);
    function GetDependencies(const AModel: TProjectModel): TProjectFilesDependencies;
    procedure ClearDependencies(const AModel: TProjectModel);

    //Packages
    function AddPackage(const AModel: TProjectModel;
      const AFilePath: string): TProjectFilesPackage;
    procedure RemovePackage(const AModel: TProjectModel;
      const AFilePath: string);
    function GetPackages(const AModel: TProjectModel): TProjectFilesPackages;
    procedure ClearPackages(const AModel: TProjectModel);

    //Other files
    function AddOtherFile(const AModel: TProjectModel;
      const AFilePath: string): TProjectFilesOther;
    procedure RemoveOtherFile(const AModel: TProjectModel;
      const AFilePath: string);
    function GetOtherFiles(const AModel: TProjectModel): TProjectFilesOthers;
    procedure ClearOtherFiles(const AModel: TProjectModel);
  end;

implementation

uses
  Builder.Exception,
  Builder.Chain,
  Builder.Storage.Default;

{ TProjectService }

class destructor TProjectService.Destroy;
begin
  FActiveProject.Free();
end;

function TProjectService.GetActiveProject: TProjectModel;
begin
  Result := FActiveProject;
end;

function TProjectService.GetBasePath: string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), 'files');
end;

function TProjectService.GetModule(const AModel: TProjectModel;
  const AFilePath: string): TProjectFilesModule;
begin
  for var LModule in AModel.Files.Modules do begin
    if (LModule.Path = AFilePath) then
      Exit(LModule);
  end;

  Result := nil;
end;

function TProjectService.GetModules(const AModel: TProjectModel): TProjectFilesModules;
begin
  Result := AModel.Files.Modules;
end;

function TProjectService.GetDependencies(
  const AModel: TProjectModel): TProjectFilesDependencies;
begin
  Result := AModel.Files.Dependencies;
end;

function TProjectService.GetOtherFiles(
  const AModel: TProjectModel): TProjectFilesOthers;
begin
  Result := AModel.Files.Others;
end;

function TProjectService.GetPackages(
  const AModel: TProjectModel): TProjectFilesPackages;
begin
  Result := AModel.Files.Packages;
end;

function TProjectService.GetProjectFilesPath(const AProjectName: string): string;
begin
  Result := TPath.Combine(GetBasePath(), AProjectName);
end;

function TProjectService.HasActiveProject: boolean;
begin
  Result := Assigned(FActiveProject);
end;

function TProjectService.IsMainModule(const AModel: TProjectModel;
  const AFilePath: string): boolean;
begin
  Result := TPath.GetFileName(AFilePath) = AModel.Files.Main;
end;

procedure TProjectService.CheckActiveProject;
begin
  if not Assigned(GetActiveProject()) then
    raise EMustOpenOrCreateProject.Create('Open/Create a project before continue.');
end;

procedure TProjectService.CheckModuleExists(const AModel: TProjectModel;
  const AFilePath: string);
begin
  var LModule := GetModule(AModel, AFilePath);
  if Assigned(LModule) then
    raise EModuleAlreadyExists.CreateFmt(
      'There is another module with the same name "%s".', [
      TPath.GetFileName(AFilePath)]);
end;

procedure TProjectService.ClearDependencies(const AModel: TProjectModel);
begin
  AModel.Files.Dependencies.Clear();
end;

procedure TProjectService.ClearOtherFiles(const AModel: TProjectModel);
begin
  AModel.Files.Others.Clear();
end;

procedure TProjectService.ClearPackages(const AModel: TProjectModel);
begin
  AModel.Files.Packages.Clear();
end;

function TProjectService.CreateProject(const AProjectName,
  AMainModuleName: string): TProjectModel;
begin
  Result := TProjectModel.Create(AProjectName);
  //Must be saved by user
  Result.Defs.Untitled := true;

  if not TDirectory.Exists(TPath.GetDirectoryName(AProjectName)) then
    TDirectory.CreateDirectory(TPath.GetDirectoryName(AProjectName));

  if not AMainModuleName.IsEmpty() then
    CreateMainModule(Result, AMainModuleName);
end;

procedure TProjectService.OpenProject(const AProject: TProjectModel);
begin
  CloseProject();
  FActiveProject := AProject;
  TGlobalBuilderChain.BroadcastEvent(TOpenProjectEvent.Create(AProject));
end;

function TProjectService.OpenProject(const AProjectPath: string): TProjectModel;
begin
  Result := nil;
  var LStorage := TDefaultStorage<TProjectModel>.Make();
  if not LStorage.LoadModel(Result, AProjectPath) then
    raise EProjectNotFound.CreateFmt('Project %s not found.', [AProjectPath]);
  OpenProject(Result);
end;

procedure TProjectService.CloseProject;
begin
  if Assigned(FActiveProject) then
    TGlobalBuilderChain.BroadcastEvent(TCloseProjectEvent.Create(FActiveProject));
  FreeAndNil(FActiveProject);
end;

function TProjectService.CreateMainModule(const AModel: TProjectModel;
  const AFilePath: string): string;
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
  if not TFile.Exists(AFilePath) then
    with TFile.Create(AFilePath) do
      try
        WriteData(
          TEncoding.UTF8.GetBytes(SCRIPT_TEXT),
          TEncoding.UTF8.GetByteCount(SCRIPT_TEXT));
      finally
        Free();
      end;

  //Save the script file in the model files
  var LModule := AddModule(AModel, AFilePath);
  //Must be saved by user
  LModule.Defs.Untitled := true;
  //Once we add the main file, we automatically set it as the main file
  SetMainModule(AModel, AFilePath);
end;

function TProjectService.AddDependency(const AModel: TProjectModel;
  const AFilePath: string): TProjectFilesDependency;
begin
  //We are not accepting duplicated file names
  for var LDependency in AModel.Files.Dependencies do begin
    if TPath.GetFileName(LDependency.Path) = TPath.GetFileName(AFilePath) then
      Exit(nil);
  end;

  //We are only accepting zip files
  if TPath.GetExtension(AFilePath) <> '.zip' then
    Exit(nil);

  //Should we copy this file to a local dir?
  Result := TProjectFilesDependency.Create(AFilePath);
  AModel.Files.Dependencies.Add(Result);
end;

function TProjectService.AddOtherFile(const AModel: TProjectModel;
  const AFilePath: string): TProjectFilesOther;
begin
  //We are not accepting duplicated file names
  for var LOther in AModel.Files.Others do begin
    if TPath.GetFileName(LOther.Path) = TPath.GetFileName(AFilePath) then
      Exit(nil);
  end;

  //Should we copy this file to a local dir?
  Result := TProjectFilesOther.Create(AFilePath);
  AModel.Files.Others.Add(Result);
end;

function TProjectService.AddPackage(const AModel: TProjectModel;
  const AFilePath: string): TProjectFilesPackage;
begin
  //We are not accepting duplicated file names
  for var LPackage in AModel.Files.Packages do begin
    if LPackage.Path = TPath.GetFileName(AFilePath) then
      Exit(nil);
  end;

  //We are only accepting zip and/or wheel files
  if (TPath.GetExtension(AFilePath) <> '.zip')
    and (TPath.GetExtension(AFilePath) <> '.whl') then
      Exit(nil);

  //Should we copy this file to a local dir?
  Result := TProjectFilesPackage.Create(AFilePath);
  AModel.Files.Packages.Add(Result);
end;

function TProjectService.AddModule(const AModel: TProjectModel;
  const AFilePath: string): TProjectFilesModule;
begin
  //We are not accepting duplicated file names
  for var LModule in AModel.Files.Modules do begin
    if TPath.GetFileName(LModule.Path) = TPath.GetFileName(AFilePath) then
      Exit(nil);
  end;

  //Should we copy this file to a local dir?
  Result := TProjectFilesModule.Create(AFilePath);
  AModel.Files.Modules.Add(Result);
end;

procedure TProjectService.RemoveModule(const AModel: TProjectModel;
  const AFilePath: string);
begin
  for var LModule in AModel.Files.Modules do
    if (LModule.Path = AFilePath) then begin
      AModel.Files.Modules.Remove(LModule);
      Break;
    end;

  //If we remove the main file, then we update it to empty.
  if (AModel.Files.Main = TPath.GetFileName(AFilePath)) then
    AModel.Files.Main := String.Empty;
end;

procedure TProjectService.RemoveDependency(const AModel: TProjectModel;
  const AFilePath: string);
begin
  for var LDependency in AModel.Files.Dependencies do
    if (LDependency.Path = AFilePath) then begin
      AModel.Files.Dependencies.Remove(LDependency);
      Break;
    end;
end;

procedure TProjectService.RemoveOtherFile(const AModel: TProjectModel;
  const AFilePath: string);
begin
  for var LOther in AModel.Files.Others do
    if (LOther.Path = AFilePath) then begin
      AModel.Files.Others.Remove(LOther);
      Break;
    end;
end;

procedure TProjectService.RemovePackage(const AModel: TProjectModel;
  const AFilePath: string);
begin
  for var LPackage in AModel.Files.Packages do
    if (LPackage.Path = AFilePath) then begin
      AModel.Files.Packages.Remove(LPackage);
      Break;
    end;
end;

function TProjectService.RemoveProject(const AProjectName: string): boolean;
begin
  var LProjectModel := OpenProject(AProjectName);
  var LStorage := TDefaultStorage<TProjectModel>.Make();
  Result := LStorage.DeleteModel(LProjectModel);

  if not Result then
    Exit;

  var LProjectFilesFolder := GetProjectFilesPath(LProjectModel.ProjectName);
  if TDirectory.Exists(LProjectFilesFolder) then
    TDirectory.Delete(LProjectFilesFolder, true);

  if Assigned(FActiveProject) and (FActiveProject.ProjectName = AProjectName) then
    CloseProject();
end;

procedure TProjectService.RenameModule(const AProject: TProjectModel;
  const AProjectModule: TProjectFilesModule; const AFilePath: string);
begin
  //Rename Main
  if AProject.Files.Main = AProjectModule.Name then
    AProject.Files.Main := TPath.GetFileName(AFilePath);

  AProjectModule.Name := TPath.GetFileName(AFilePath);
  AProjectModule.Path := AFilePath;
end;

procedure TProjectService.RenameProject(const AModel: TProjectModel;
  const AProjectName: string);
begin
  var LNewProject := TProjectModel.Create(TPath.GetFileName(AProjectName));
  try
    AModel.Merge(LNewProject);
  finally
    LNewProject.Free();
  end;
end;

procedure TProjectService.SaveProject(const AProjectPath: string;
  const AProject: TProjectModel);
begin
  var LStorage := TDefaultStorage<TProjectModel>.Make();
  LStorage.SaveModel(AProject, AProjectPath);
end;

procedure TProjectService.SetMainModule(const AModel: TProjectModel;
  const AFilePath: string);
begin
  AModel.Files.Main := TPath.GetFileName(AFilePath);
end;

end.
