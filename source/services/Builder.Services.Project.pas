unit Builder.Services.Project;

interface

uses
  System.IOUtils, System.Classes, System.SysUtils,
  System.Generics.Collections,
  Builder.Types, Builder.Services,
  Builder.Model.Project, Builder.Model.Project.Files;

type
  TProjectService = class(TInterfacedObject, IProjectServices)
  private
    class var FActiveProject: TProjectModel;
  private
    class destructor Destroy();
  public
    function CreateProject(const AProjectPath: string;
      AMainModulePath: string = ''): TProjectModel;
    procedure SaveProject(const AProject: TProjectModel); overload;
    //Save untracked project
    procedure SaveProject(const AProject: TProjectModel;
      const ASaveRequest: TSaveRequest;
      const ACheckUntracked: boolean = true); overload;
    //Open/close project in editor
    procedure OpenProject(const AProject: TProjectModel); overload;
    function OpenProject(const AProjectPath: string): TProjectModel; overload;
    procedure CloseProject();
    procedure RenameProject(const AProject: TProjectModel;
      const AProjectPath: string);
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
    //Save untracked module
    procedure SaveModule(const AProject: TProjectModel; const AModel: TProjectFilesModule;
      const ASaveRequest: TSaveRequest; const ACheckUntracked: boolean = true);
    procedure SaveModules(const AProject: TProjectModel;
      const ASaveRequest: TSaveRequest; const ACheckUntracked: boolean = true);
    function GetModule(const AModel: TProjectModel;
      const AFilePath: string): TProjectFilesModule;
    procedure RemoveModule(const AModel: TProjectModel; const AFilePath: string);
    function GetModules(const AModel: TProjectModel): TProjectFilesModules;
    procedure RenameModule(const AProject: TProjectModel;
      const AModule: TProjectFilesModule; const AFilePath: string);
    procedure CheckModuleExists(const AModel: TProjectModel;
      const AFilePath: string);

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
  Builder.Consts,
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

function TProjectService.GetModule(const AModel: TProjectModel;
  const AFilePath: string): TProjectFilesModule;
begin
  Assert(Assigned(AModel), 'Argument "AModel" not assigned.');

  for var LModule in AModel.Files.Modules do begin
    if (LModule.Path = AFilePath) then
      Exit(LModule);
  end;

  Result := nil;
end;

function TProjectService.GetModules(const AModel: TProjectModel): TProjectFilesModules;
begin
  Assert(Assigned(AModel), 'Argument "AModel" not assigned.');

  Result := AModel.Files.Modules;
end;

function TProjectService.GetDependencies(
  const AModel: TProjectModel): TProjectFilesDependencies;
begin
  Assert(Assigned(AModel), 'Argument "AModel" not assigned.');

  Result := AModel.Files.Dependencies;
end;

function TProjectService.GetOtherFiles(
  const AModel: TProjectModel): TProjectFilesOthers;
begin
  Assert(Assigned(AModel), 'Argument "AModel" not assigned.');

  Result := AModel.Files.Others;
end;

function TProjectService.GetPackages(
  const AModel: TProjectModel): TProjectFilesPackages;
begin
  Assert(Assigned(AModel), 'Argument "AModel" not assigned.');

  Result := AModel.Files.Packages;
end;

function TProjectService.HasActiveProject: boolean;
begin
  Result := Assigned(FActiveProject);
end;

function TProjectService.IsMainModule(const AModel: TProjectModel;
  const AFilePath: string): boolean;
begin
  Assert(Assigned(AModel), 'Argument "AModel" not assigned.');

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
  Assert(Assigned(AModel), 'Argument "AModel" not assigned.');

  var LModule := GetModule(AModel, AFilePath);
  if Assigned(LModule) then
    raise EModuleAlreadyExists.CreateFmt(
      'There is another module with the same name "%s".', [
      TPath.GetFileName(AFilePath)]);
end;

procedure TProjectService.ClearDependencies(const AModel: TProjectModel);
begin
  Assert(Assigned(AModel), 'Argument "AModel" not assigned.');

  AModel.Files.Dependencies.Clear();
end;

procedure TProjectService.ClearOtherFiles(const AModel: TProjectModel);
begin
  Assert(Assigned(AModel), 'Argument "AModel" not assigned.');

  AModel.Files.Others.Clear();
end;

procedure TProjectService.ClearPackages(const AModel: TProjectModel);
begin
  Assert(Assigned(AModel), 'Argument "AModel" not assigned.');

  AModel.Files.Packages.Clear();
end;

function TProjectService.CreateProject(const AProjectPath: string;
  AMainModulePath: string): TProjectModel;
begin
  Result := TProjectModel.Create(AProjectPath);

  if not TDirectory.Exists(TPath.GetDirectoryName(AProjectPath)) then
    TDirectory.CreateDirectory(TPath.GetDirectoryName(AProjectPath));

  if not AMainModulePath.IsEmpty() then
    CreateMainModule(Result, AMainModulePath);

  //Must be saved by user
  Result.Defs.Storage := AProjectPath;
  Result.Defs.Untracked := true;
end;

procedure TProjectService.OpenProject(const AProject: TProjectModel);
begin
  Assert(Assigned(AProject), 'Argument "AProject" not assigned.');

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
  Assert(Assigned(AModel), 'Argument "AModel" not assigned.');

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
  LModule.Defs.Untracked := true;
  //Once we add the main file, we automatically set it as the main file
  SetMainModule(AModel, AFilePath);
end;

function TProjectService.AddDependency(const AModel: TProjectModel;
  const AFilePath: string): TProjectFilesDependency;
begin
  Assert(Assigned(AModel), 'Argument "AModel" not assigned.');

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
  Assert(Assigned(AModel), 'Argument "AModel" not assigned.');

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
  Assert(Assigned(AModel), 'Argument "AModel" not assigned.');

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
  Assert(Assigned(AModel), 'Argument "AModel" not assigned.');

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
  Assert(Assigned(AModel), 'Argument "AModel" not assigned.');

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
  Assert(Assigned(AModel), 'Argument "AModel" not assigned.');

  for var LDependency in AModel.Files.Dependencies do
    if (LDependency.Path = AFilePath) then begin
      AModel.Files.Dependencies.Remove(LDependency);
      Break;
    end;
end;

procedure TProjectService.RemoveOtherFile(const AModel: TProjectModel;
  const AFilePath: string);
begin
  Assert(Assigned(AModel), 'Argument "AModel" not assigned.');

  for var LOther in AModel.Files.Others do
    if (LOther.Path = AFilePath) then begin
      AModel.Files.Others.Remove(LOther);
      Break;
    end;
end;

procedure TProjectService.RemovePackage(const AModel: TProjectModel;
  const AFilePath: string);
begin
  Assert(Assigned(AModel), 'Argument "AModel" not assigned.');

  for var LPackage in AModel.Files.Packages do
    if (LPackage.Path = AFilePath) then begin
      AModel.Files.Packages.Remove(LPackage);
      Break;
    end;
end;

procedure TProjectService.RenameProject(const AProject: TProjectModel;
  const AProjectPath: string);
begin
  Assert(Assigned(AProject), 'Argument "AProject" not assigned.');

  if AProjectPath.IsEmpty() then
    Exit;

  var LOldFilePath := AProject.Defs.Storage;
  var LNewFilePath := TPath.ChangeExtension(AProjectPath, PYTHON_PROJECT_FILE_EXTENSION);
  var LNewProject := TProjectModel.Create(LNewFilePath);
  try
    var LErrors := TStringList.Create();
    try
      if not LNewProject.ValidateName(LErrors) then
        raise EModelValidationError.Create(LErrors.Text);
    finally
      LErrors.Free();
    end;

    //Skip moving to the same place
    if (AProject.Defs.Storage <> LNewFilePath) then begin
      //Rename the project file
      if TFile.Exists(LOldFilePath) then begin
        if TFile.Exists(LNewFilePath) then
          TFile.Delete(LNewFilePath);
        TFile.Move(LOldFilePath, LNewFilePath);
      end;
    end;

    AProject.Merge(LNewProject);
    AProject.Defs.Storage := LNewFilePath;
  finally
    LNewProject.Free();
  end;

  TGlobalBuilderChain.BroadcastEventAsync(
    TRenameFileEvent.Create(LOldFilePath, LNewFilePath));
end;

procedure TProjectService.SaveProject(const AProject: TProjectModel);
begin
  Assert(Assigned(AProject), 'Argument "AProject" not assigned.');

  var LStorage := TDefaultStorage<TProjectModel>.Make();
  LStorage.SaveModel(AProject);
end;

procedure TProjectService.SaveModule(const AProject: TProjectModel;
  const AModel: TProjectFilesModule;
  const ASaveRequest: TSaveRequest;
  const ACheckUntracked: boolean);
begin
  Assert(Assigned(AProject), 'Argument "AProject" not assigned.');
  Assert(Assigned(AModel), 'Argument "AModel" not assigned.');
  Assert(Assigned(ASaveRequest), 'Argument "ASaveRequest" not assigned.');

  if ACheckUntracked and not AModel.Defs.Untracked then
    Exit;

  var LFileName := ASaveRequest(AModel.Name);
  if LFileName.IsEmpty() then
    Exit;

  RenameModule(AProject, AModel, LFileName);

  AModel.Defs.Untracked := false;
end;

procedure TProjectService.SaveModules(const AProject: TProjectModel;
  const ASaveRequest: TSaveRequest; const ACheckUntracked: boolean);
begin
  Assert(Assigned(AProject), 'Argument "AProject" not assigned.');
  Assert(Assigned(ASaveRequest), 'Argument "ASaveRequest" not assigned.');

  for var LModule in AProject.Files.Modules do
    SaveModule(AProject, LModule, ASaveRequest, ACheckUntracked);
end;

procedure TProjectService.SaveProject(const AProject: TProjectModel;
  const ASaveRequest: TSaveRequest; const ACheckUntracked: boolean);
begin
  Assert(Assigned(AProject), 'Argument "AProject" not assigned.');
  Assert(Assigned(ASaveRequest), 'Argument "ASaveRequest" not assigned.');

  if ACheckUntracked and not AProject.Defs.Untracked then
    Exit;

  var LFileName := ASaveRequest(AProject.ProjectName);
  if LFileName.IsEmpty() then
    Exit;

  RenameProject(AProject, LFileName);
  SaveProject(AProject);

  AProject.Defs.Untracked := false;
end;

procedure TProjectService.SetMainModule(const AModel: TProjectModel;
  const AFilePath: string);
begin
  Assert(Assigned(AModel), 'Argument "AModel" not assigned.');
  AModel.Files.Main := TPath.GetFileName(AFilePath);
end;

procedure TProjectService.RenameModule(const AProject: TProjectModel;
  const AModule: TProjectFilesModule; const AFilePath: string);
begin
  Assert(Assigned(AProject), 'Argument "AProject" not assigned.');
  Assert(Assigned(AProject), 'Argument "AModule" not assigned.');

  if AFilePath.IsEmpty() then
    Exit;

  CheckModuleExists(AProject, AFilePath);

  var LOldFilePath := AModule.Path;
  var LNewModule := TProjectFilesModule.Create(AFilePath);
  try
    var LErrors := TStringList.Create();
    try
      if not LNewModule.ValidateName(LErrors) then
        raise EModelValidationError.Create(LErrors.Text);
    finally
      LErrors.Free();
    end;

    //Skip moving to the same place
    if (LOldFilePath <> AFilePath) then begin
      //Rename the project file
      if TFile.Exists(LOldFilePath) then begin
        if TFile.Exists(AFilePath) then
          TFile.Delete(AFilePath);
        TFile.Move(LOldFilePath, AFilePath);
      end;
    end;

    //Rename Main
    if AProject.Files.Main = AModule.Name then
      SetMainModule(AProject, AFilePath);

    AModule.Name := TPath.GetFileName(AFilePath);
    AModule.Path := AFilePath;
  finally
    LNewModule.Free();
  end;

  TGlobalBuilderChain.BroadcastEventAsync(
    TRenameFileEvent.Create(LOldFilePath, AFilePath));
end;

end.
