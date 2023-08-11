unit Frame.ProjectFiles;

interface

uses
  System.Types, System.UITypes, System.Classes, System.Variants, System.Rtti,
  System.Actions, System.Generics.Collections, System.ImageList, FMX.Types,
  FMX.Graphics, FMX.Menus, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.TreeView, FMX.ImgList, FMX.ActnList,
  Container.Images,
  Builder.Services, Builder.Types, Builder.Messagery,
  Builder.Model.Project, Builder.Model.Project.Files,
  Builder.TreeView;

type
  TNodeType = (
    ntProject,
    ntRootModule, ntModule,
    ntRootBuildConfiguration, ntBuildConfiguration,
    ntRootTargetPlatform, ntTargetPlatform,
    ntRootTargetPython, ntTargetPython,
    ntRootPackage, ntPackage,
    ntRootOtherFile, ntOtherFile);

  TProjectFilesFrame = class(TFrame)
    tvProjectFiles: TTreeView;
    pmtvProjectFiles: TPopupMenu;
    miAddModule: TMenuItem;
    miRemoveModule: TMenuItem;
    altvProjectFiles: TActionList;
    actAddModule: TAction;
    actRemoveModule: TAction;
    miSetToMain: TMenuItem;
    actSetToMain: TAction;
    miSepPackages: TMenuItem;
    miAddBundle: TMenuItem;
    miRemoveBundle: TMenuItem;
    actAddPackage: TAction;
    actRemovePackage: TAction;
    miSepOtherFiles: TMenuItem;
    miAddOtherFile: TMenuItem;
    miRemoveOtherFile: TMenuItem;
    actAddOtherFile: TAction;
    actRemoveOtherFile: TAction;
    miSepOptions: TMenuItem;
    miRevealFile: TMenuItem;
    actRevealFile: TAction;
    actNewModule: TAction;
    miNewModule: TMenuItem;
    procedure actAddModuleExecute(Sender: TObject);
    procedure actRemoveModuleExecute(Sender: TObject);
    procedure altvProjectFilesUpdate(Action: TBasicAction;
      var Handled: Boolean);
    procedure actSetToMainExecute(Sender: TObject);
    procedure actAddPackageExecute(Sender: TObject);
    procedure actRemovePackageExecute(Sender: TObject);
    procedure actAddOtherFileExecute(Sender: TObject);
    procedure actRemoveOtherFileExecute(Sender: TObject);
    procedure actRevealFileExecute(Sender: TObject);
    procedure actNewModuleExecute(Sender: TObject);
  private type
    TModuleNodeData = TPair<string, TProjectFilesModule>;
  private
    FProjectModel: TProjectModel;
    FProjectServices: IProjectServices;
    FEditorServices: IEditorServices;
    FRoot: TTreeViewItem;
    FOpenProjectEvent: IDisconnectable;
    FCloseProjectEvent: IDisconnectable;
    FRenameFileEvent: IDisconnectable;
  private
    function GetProjectServices: IProjectServices;
    //Project operations
    function DoAddModule(const AFileName: string; const ANew: boolean): TProjectFilesModule;
    //TreeView operations
    procedure LoadIcon(const AItem: TTreeViewItem);
    procedure LoadStyles(const AItem: TTreeViewItem);
    procedure LoadEvents(const AItem: TTreeViewItem);
    function BuildNode(const AParent: TFmxObject; const ANodeType: TNodeType;
      const ANodeData: TValue): TTreeViewItem;
    function NodeIsType(const AItem: TTreeViewItem; const ANodeType: TNodeType): boolean;
    //Node data builders
    function BuildModuleNodeData(const AModule: TProjectFilesModule): TValue;
    //TreeView nodes
    function AddProjectNode(): TTreeViewItem;
    procedure AddBuildConfigurationNodes(const ARoot: TTreeViewItem);
    procedure AddTargetPlatformNodes(const ARoot: TTreeViewItem);
    procedure AddTargetPythonNodes(const ARoot: TTreeViewItem);
    procedure AddModuleNodes(const ARoot: TTreeViewItem);
    procedure AddPackageNodes(const ARoot: TTreeViewItem);
    procedure AddOtherFileNodes(const ARoot: TTreeViewItem);
    //Node events
    procedure OnTreeViewItemDblClickModule(Sender: TObject);
    procedure OnTreeViewItemDblClickBuildConfigurationItem(Sender: TObject);
    procedure OnTreeViewItemDblClickPlatformItem(Sender: TObject);
    procedure OnTreeViewItemDblClickPythonItem(Sender: TObject);
    procedure OnTreeViewItemRename(Sender: TObject; const AOldName: string; var ANewName: string);
    //Visual node updates
    procedure UpdateSelectedPlatform(const ASelected: TTreeViewItem);
    procedure UpdateSelectedPython(const ASelected: TTreeViewItem);
    procedure UpdateSelectedBuildConfiguration(const ASelected: TTreeViewItem);
    //Node bindings
    function GetNodeType(const AItem: TTreeViewItem): TNodeType;
    function GetNodeByNodeType(const ANodeType: TNodeType): TTreeViewItem;
    function GetNodeByProject(const AProject: TProjectModel): TTreeViewItem;
    function GetNodeByModule(const AModule: TProjectFilesModule): TTreeViewItem;
    function GetNodeData<T>(const AItem: TTreeViewItem): T;
    //Data updates
    procedure UpdateProjectNode();
    procedure UpdateModulesNode();
    procedure DoRenameProject(const AProject: TProjectModel;
      const AProjectFileName: string);
    procedure DoRenameModule(const AModule: TProjectFilesModule;
      const AModuleFileName: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    procedure LoadProject(const AProjectModel: TProjectModel);
    procedure UnLoadProject(const AProjectModel: TProjectModel);

    function GetDefaultProjectFilePath(const AProject: TProjectModel): string; overload;
    function GetDefaultProjectFilePath(): string; overload;

    function GetItemFilePath(const AItem: TTreeViewItem): string;
  end;

  PNodeInfo = ^TNodeInfo;
  TNodeInfo = record
  public
    constructor Create(const ANodeType: TNodeType); overload;
    constructor Create(const ANodeType: TNodeType; const ANodeData: TValue); overload;

    class operator Implicit(ANodeInfo: TNodeInfo): TValue;
  public
    NodeType: TNodeType;
    NodeData: TValue;
  end;

implementation

uses
  System.StrUtils, System.IOUtils, System.SysUtils,
  FMX.DialogService,
  {$IFDEF MSWINDOWS}
  ShellApi, WinAPI.Windows,
  {$ENDIF MSWINDOWS}
  Container.Menu.Actions,
  Builder.Paths;

{$R *.fmx}

{ TNodeInfo }

constructor TNodeInfo.Create(const ANodeType: TNodeType;
  const ANodeData: TValue);
begin
  NodeType := ANodeType;
  NodeData := ANodeData;
end;

constructor TNodeInfo.Create(const ANodeType: TNodeType);
begin
  Create(ANodeType, TValue.Empty);
end;

class operator TNodeInfo.Implicit(ANodeInfo: TNodeInfo): TValue;
begin
  TValue.Make<TNodeInfo>(ANodeInfo, Result);
end;

{ TProjectFilesFrame }

constructor TProjectFilesFrame.Create(AOwner: TComponent);
begin
  inherited;
  FEditorServices := TBuilderService.CreateService<IEditorServices>;
  FOpenProjectEvent := TMessagery.SubscribeToEvent<TOpenProjectEvent>(
    procedure(const AEventNotification: TOpenProjectEvent)
    begin
      TMessagery.BroadcastEventAsync(
        TAsyncOperationStartedEvent.Create(TAsyncOperation.OpenProject));

      var LProject := AEventNotification.Body.Project;
      TThread.Queue(TThread.Current,
        procedure()
        begin
          try
            LoadProject(LProject);
            var LDefaultProjectFilePath := GetDefaultProjectFilePath();
            if not LDefaultProjectFilePath.IsEmpty() then
              FEditorServices.OpenEditor(LDefaultProjectFilePath);
          finally
            TMessagery.BroadcastEventAsync(
              TAsyncOperationEndedEvent.Create(TAsyncOperation.OpenProject));
          end;
        end);
    end);

  FCloseProjectEvent := TMessagery.SubscribeToEvent<TCloseProjectEvent>(
    procedure(const AEventNotification: TCloseProjectEvent)
    begin
      var LProject := AEventNotification.Body.Project;
      TThread.Queue(TThread.Current,
        procedure()
        begin
          UnLoadProject(LProject);
        end);
    end);

  FRenameFileEvent := TMessagery.SubscribeToEvent<TRenameFileEvent>(
    procedure(const AEventNotification: TRenameFileEvent)
    begin
      var LOldFilePath := AEventNotification.Body.OldFilePath;
      var LNewFilePath := AEventNotification.Body.NewFilePath;
      TThread.Queue(TThread.Current,
        procedure()
        begin
          if not Assigned(FRoot) then
            Exit;

          UpdateProjectNode();
          UpdateModulesNode();
        end);
    end);
end;

destructor TProjectFilesFrame.Destroy;
begin
  FRenameFileEvent.Disconnect();
  FOpenProjectEvent.Disconnect();
  FCloseProjectEvent.Disconnect();
  inherited;
end;

function TProjectFilesFrame.DoAddModule(const AFileName: string;
  const ANew: boolean): TProjectFilesModule;
begin
  Result := GetProjectServices().AddModule(FProjectModel, AFileName);
  if Assigned(Result) then begin
    //Creates the tree item
    var LItem := BuildNode(
      GetNodeByNodeType(TNodeType.ntRootModule),
      ntModule,
      BuildModuleNodeData(Result));

    LItem.Text := Result.Name;

    //Add as the deafult script file if none
    if FProjectModel.Files.Main.IsEmpty() then
      GetProjectServices().SetMainModule(FProjectModel, Result.Path);

    tvProjectFiles.Selected := LItem;

    FEditorServices.OpenEditor(AFileName, ANew);
  end;
end;

procedure TProjectFilesFrame.DoRenameModule(
  const AModule: TProjectFilesModule;
  const AModuleFileName: string);
begin
  GetProjectServices().RenameModule(FProjectModel, AModule, AModuleFileName);
  GetProjectServices().SaveProject(FProjectModel);
end;

procedure TProjectFilesFrame.DoRenameProject(
  const AProject: TProjectModel; const AProjectFileName: string);
begin
  GetProjectServices().RenameProject(FProjectModel, AProjectFileName);
  GetProjectServices().SaveProject(FProjectModel);
end;

function TProjectFilesFrame.GetItemFilePath(const AItem: TTreeViewItem): string;
begin
  case GetNodeType(AItem) of
    ntModule:
      Result := GetNodeData<TModuleNodeData>(AItem).Key
    else
      Result := GetNodeData<string>(AItem);
  end;
end;

function TProjectFilesFrame.GetDefaultProjectFilePath(
  const AProject: TProjectModel): string;
begin
  if not Assigned(AProject) then
    Exit(String.Empty);

  for var LModule in AProject.Files.Modules do begin
    if AProject.Files.Main.IsEmpty() then
      Exit(LModule.Path);

    if (LModule.Name = AProject.Files.Main) then
      Exit(LModule.Path);
  end;

  Result := String.Empty;
end;

function TProjectFilesFrame.GetDefaultProjectFilePath: string;
begin
  Result := GetDefaultProjectFilePath(FProjectModel);
end;

function TProjectFilesFrame.GetNodeByModule(
  const AModule: TProjectFilesModule): TTreeViewItem;
begin
  var LRoot := GetNodeByNodeType(TNodeType.ntRootModule);
  if not Assigned(LRoot) then
    Exit(nil);

  for var I := 0 to LRoot.Count - 1 do
    if GetNodeData<TModuleNodeData>(LRoot.Items[I]).Value = AModule then
      Exit(LRoot.Items[I]);

  Result := nil;
end;

function TProjectFilesFrame.GetNodeByNodeType(
  const ANodeType: TNodeType): TTreeViewItem;

  function RecursivelyGetNode(const ARoot: TTreeViewItem;
    const ANodeType: TNodeType): TTreeViewItem;
  begin
    if NodeIsType(ARoot, ANodeType) then
      Exit(ARoot);
    
    for var I := 0 to ARoot.Count - 1 do begin
      Result := RecursivelyGetNode(ARoot.Items[I], ANodeType);
      if Assigned(Result) then
        Exit;
    end;

    Result := nil;
  end;
  
begin
  if not Assigned(FRoot) then
    Exit(nil);

  Result := RecursivelyGetNode(FRoot, ANodeType);
end;

function TProjectFilesFrame.GetNodeByProject(
  const AProject: TProjectModel): TTreeViewItem;
begin
  Result := FRoot;
end;

function TProjectFilesFrame.GetNodeData<T>(const AItem: TTreeViewItem): T;
begin
  Result := AItem.Data.AsType<TNodeInfo>.NodeData.AsType<T>;
end;

function TProjectFilesFrame.GetNodeType(const AItem: TTreeViewItem): TNodeType;
begin
  Result := AItem.Data.AsType<TNodeInfo>().NodeType;
end;

function TProjectFilesFrame.GetProjectServices: IProjectServices;
begin
  if not Assigned(FProjectServices) then
    FProjectServices := TBuilderService.CreateService<IProjectServices>;
  Result := FProjectServices;
end;

procedure TProjectFilesFrame.LoadEvents(const AItem: TTreeViewItem);
begin
  case GetNodeType(AItem) of
    ntProject:
      AItem.OnRename := OnTreeViewItemRename;
    ntModule: begin
      AItem.OnDblClick := OnTreeViewItemDblClickModule;
      AItem.OnRename := OnTreeViewItemRename;
    end;
    ntBuildConfiguration:
      AItem.OnDblClick := OnTreeViewItemDblClickBuildConfigurationItem;
    ntTargetPlatform:
      AItem.OnDblClick := OnTreeViewItemDblClickPlatformItem;
    ntTargetPython:
      AItem.OnDblClick := OnTreeViewItemDblClickPythonItem;
  end;
end;

procedure TProjectFilesFrame.LoadIcon(const AItem: TTreeViewItem);
begin
  case AItem.Data.AsType<TNodeInfo>().NodeType of
    ntProject:
      AItem.ImageIndex := PROJECT_ICON_INDEX;
    ntRootModule:
      AItem.ImageIndex := SOURCE_ICON_INDEX;
    ntModule:
      AItem.ImageIndex := MODULE_ICON_INDEX;
    ntRootPackage:
      AItem.ImageIndex := PACKAGE_ICON_INDEX;
    ntPackage:
      AItem.ImageIndex := PACKAGE_ICON_INDEX;
    ntRootBuildConfiguration:
      AItem.ImageIndex := BUILD_CONFIGURATION_ICON_INDEX;
    ntBuildConfiguration:
      AItem.ImageIndex := BUILD_CONFIGURATION_ITEM_ICON_INDEX;
    ntRootTargetPlatform:
      AItem.ImageIndex := TARGET_PLATFORMS_ICON_INDEX;
    ntTargetPlatform:
      AItem.ImageIndex := TARGET_PLATFORMS_ANDROID_ICON_INDEX;
    ntRootTargetPython:
      AItem.ImageIndex := TARGET_PYTHON_ICON_INDEX;
    ntTargetPython:
      AItem.ImageIndex := TARGET_PYTHON_VER_ICON_INDEX;
    ntRootOtherFile:
      AItem.ImageIndex := ROOT_OTHER_FILE_ICON_INDEX;
    ntOtherFile:
      AItem.ImageIndex := OTHER_FILE_ICON_INDEX;
  end;
end;

procedure TProjectFilesFrame.LoadStyles(const AItem: TTreeViewItem);

  procedure RestoreChildrenTextSettings(const AItem: TTreeViewItem);
  begin
    for var I := 0 to AItem.ParentItem.Count - 1 do
      AItem.ParentItem.Items[I].ResultingTextSettings.Assign(
        AItem.ParentItem.Items[I].DefaultTextSettings);
  end;

begin
  AItem.BeginUpdate();
  try
    case AItem.Data.AsType<TNodeInfo>().NodeType of
      ntProject: begin
        AItem.Expand();
        AItem.ResultingTextSettings.Font.Size := 12;
        AItem.ResultingTextSettings.Font.Style := [TFontStyle.fsBold];
      end;
      ntModule : begin
        //AItem.ResultingTextSettings.Font.Size := 12;
      end;
      ntBuildConfiguration: begin
        if not (GetNodeData<TBuildConfiguration>(AItem) = FProjectModel.BuildConfiguration) then
          Exit;
        RestoreChildrenTextSettings(AItem);
        AItem.ResultingTextSettings.Font.Style := [TFontStyle.fsBold];
      end;
      ntTargetPlatform: begin
        if not (GetNodeData<TArchitecture>(AItem) = FProjectModel.Architecture) then
          Exit;
        RestoreChildrenTextSettings(AItem);
        AItem.ResultingTextSettings.Font.Style := [TFontStyle.fsBold];
      end;
      ntTargetPython: begin
        if not (GetNodeData<TPythonVersion>(AItem) = FProjectModel.PythonVersion) then
          Exit;
        RestoreChildrenTextSettings(AItem);
        AItem.ResultingTextSettings.Font.Style := [TFontStyle.fsBold];
      end;
    end;
  finally
    AItem.EndUpdate;
  end;
end;

function TProjectFilesFrame.NodeIsType(const AItem: TTreeViewItem;
  const ANodeType: TNodeType): boolean;
begin
  Result := AItem.Data.AsType<TNodeInfo>().NodeType = ANodeType;
end;

procedure TProjectFilesFrame.OnTreeViewItemDblClickModule(Sender: TObject);
begin
  var LFilePath := GetItemFilePath(TTreeViewItem(Sender));
  if not LFilePath.IsEmpty() then
    FEditorServices.OpenEditor(LFilePath);
end;

procedure TProjectFilesFrame.OnTreeViewItemRename(Sender: TObject; const AOldName: string;
  var ANewName: string);
begin
  if ANewName.Trim().IsEmpty() then begin
    ANewName := AOldName;
    Exit;
  end;

  var LNode := Sender as TTreeViewItem;
  if (GetNodeType(LNode) = TNodeType.ntProject) then begin
    var LNewFileName := TPath.Combine(
      TPath.GetDirectoryName(FProjectModel.Defs.Storage),
      ANewName);
    
    DoRenameProject(FProjectModel, LNewFileName);
  end else if (GetNodeType(LNode) = TNodeType.ntModule) then begin
    var LModule := GetNodeData<TModuleNodeData>(LNode).Value;
    var LNewFileName := TPath.Combine(
      TPath.GetDirectoryName(LModule.Path),
      ANewName);
    DoRenameModule(LModule, LNewFileName);
  end else
    ANewName := AOldName;
end;

procedure TProjectFilesFrame.OnTreeViewItemDblClickBuildConfigurationItem(
  Sender: TObject);
begin
  var LNode := TTreeViewItem(Sender);
  FProjectModel.BuildConfiguration := LNode.Data.AsType<TNodeInfo>
    .NodeData.AsType<TBuildConfiguration>;
  UpdateSelectedBuildConfiguration(LNode);
  LoadStyles(LNode);
end;

procedure TProjectFilesFrame.OnTreeViewItemDblClickPlatformItem(
  Sender: TObject);
begin
  var LNode := TTreeViewItem(Sender);
  FProjectModel.Architecture := LNode.Data.AsType<TNodeInfo>
    .NodeData.AsType<TArchitecture>;
  UpdateSelectedPlatform(LNode);
  LoadStyles(LNode);
end;

procedure TProjectFilesFrame.OnTreeViewItemDblClickPythonItem(Sender: TObject);
begin
  var LNode := TTreeViewItem(Sender);
  FProjectModel.PythonVersion := LNode.Data.AsType<TNodeInfo>
    .NodeData.AsType<TPythonVersion>;
  UpdateSelectedPython(LNode);
  LoadStyles(LNode);
end;

procedure TProjectFilesFrame.UnLoadProject(const AProjectModel: TProjectModel);
begin
  FEditorServices.CloseAll();
  tvProjectFiles.Clear();
  FRoot := nil;
  FProjectModel := nil;
end;

procedure TProjectFilesFrame.UpdateModulesNode;
begin
  for var LModule in FProjectModel.Files.Modules do begin
    var LNode := GetNodeByModule(LModule);
    if Assigned(LNode) then begin
      LNode.Text := LModule.Name;
      LNode.Data := TNodeInfo.Create(GetNodeType(LNode), BuildModuleNodeData(LModule));
    end;
  end;
end;

procedure TProjectFilesFrame.UpdateProjectNode;
begin
  var LNode := GetNodeByProject(FProjectModel);
  if Assigned(LNode) then begin
    LNode.Text := FProjectModel.ProjectName;
    LNode.Data := TNodeInfo.Create(GetNodeType(LNode), FProjectModel.ProjectName);
  end;
end;

procedure TProjectFilesFrame.UpdateSelectedBuildConfiguration(
  const ASelected: TTreeViewItem);
begin
  var LRoot := ASelected.ParentItem;
  LRoot.Text := Format('Build Configuration (%s)', [
    ASelected.Data.AsType<TNodeInfo>
      .NodeData.AsType<TBuildConfiguration>.ToBuildConfiguration()]);
end;

procedure TProjectFilesFrame.UpdateSelectedPlatform(
  const ASelected: TTreeViewItem);
begin
  var LRoot := ASelected.ParentItem;
  LRoot.Text := Format('Target Platforms (%s)', [
    ASelected.Data.AsType<TNodeInfo>
      .NodeData.AsType<TArchitecture>.ToTargetPlatform()]);
end;

procedure TProjectFilesFrame.UpdateSelectedPython(
  const ASelected: TTreeViewItem);
begin
  var LRoot := ASelected.ParentItem;
  LRoot.Text := Format('Target Python (%s)', [
    ASelected.Data.AsType<TNodeInfo>
      .NodeData.AsType<TPythonVersion>.ToTargetPython()]);
end;

function TProjectFilesFrame.BuildModuleNodeData(
  const AModule: TProjectFilesModule): TValue;
begin
  TValue.Make<TModuleNodeData>(
    TModuleNodeData.Create(AModule.Path, AModule), Result);
end;

function TProjectFilesFrame.BuildNode(const AParent: TFmxObject;
  const ANodeType: TNodeType; const ANodeData: TValue): TTreeViewItem;
begin
  tvProjectFiles.BeginUpdate();
  try
    Result := TTreeViewItem.Create(AParent);
    Result.BeginUpdate();
    try
      Result.Parent := AParent;
      Result.Data := TNodeInfo.Create(ANodeType, ANodeData);
      Result.Renamable := ANodeType in [TNodeType.ntProject, TNodeType.ntModule];
      if not Result.Renamable then
        Result.StyleLookup := 'treeviewitemstyle';
      Result.LazyInput := true;
      LoadStyles(Result);
      LoadIcon(Result);
      LoadEvents(Result);
    finally
      Result.EndUpdate()
    end;            
  finally
    tvProjectFiles.EndUpdate();
  end;
end;

function TProjectFilesFrame.AddProjectNode: TTreeViewItem;
begin
  Result := BuildNode(tvProjectFiles, ntProject, FProjectModel.ProjectName);
  Result.Text := FProjectModel.ProjectName;
end;

procedure TProjectFilesFrame.AddBuildConfigurationNodes(
  const ARoot: TTreeViewItem);
begin
  if not Assigned(ARoot) then
    Exit;

  var LBuildConfigurationNode := BuildNode(
    ARoot, ntRootBuildConfiguration, String.Empty);
  var LCurrentBuildConfiguration := FProjectModel.BuildConfiguration;

  for var LBuildConfiguration := Low(TBuildConfiguration) to High(TBuildConfiguration) do begin
    var LNode := BuildNode(LBuildConfigurationNode, ntBuildConfiguration,
      TValue.From<TBuildConfiguration>(LBuildConfiguration));
    LNode.Text := LBuildConfiguration.ToBuildConfiguration();
    if LBuildConfiguration = LCurrentBuildConfiguration then
      UpdateSelectedBuildConfiguration(LNode);
  end;
end;

procedure TProjectFilesFrame.AddTargetPlatformNodes(
  const ARoot: TTreeViewItem);
begin
  if not Assigned(ARoot) then
    Exit;

  var LTargetPlatformsNode := BuildNode(ARoot, ntRootTargetPlatform, String.Empty);
  var LCurrentArch := FProjectModel.Architecture;

  for var LArch := Low(TArchitecture) to High(TArchitecture) do
  begin
    var LNode := BuildNode(LTargetPlatformsNode, ntTargetPlatform,
      TValue.From<TArchitecture>(LArch));
    LNode.Text := LArch.ToTargetPlatform();
    if LArch = LCurrentArch then
      UpdateSelectedPlatform(LNode);
  end;
end;

procedure TProjectFilesFrame.AddTargetPythonNodes(const ARoot: TTreeViewItem);
begin
  if not Assigned(ARoot) then
    Exit;

  var LTargetPythonNode := BuildNode(ARoot, ntRootTargetPython, String.Empty);
  var LCurrentPythonVer := FProjectModel.PythonVersion;

  for var LPythonVer := Low(TPythonVersion) to High(TPythonVersion) do
  begin
    var LNode := BuildNode(LTargetPythonNode, ntTargetPython,
      TValue.From<TPythonVersion>(LPythonVer));
    LNode.Text := LPythonVer.ToTargetPython();
    if LPythonVer = LCurrentPythonVer then
      UpdateSelectedPython(LNode);
  end;
end;

procedure TProjectFilesFrame.AddModuleNodes(const ARoot: TTreeViewItem);
begin
  if not Assigned(ARoot) then
    Exit;

  var LSourceNode := BuildNode(ARoot, ntRootModule, String.Empty);
  LSourceNode.Text := 'Modules';
  LSourceNode.IsExpanded := true;

  var LModules := GetProjectServices().GetModules(FProjectModel);
  for var LModule in LModules do begin
    var LItem := BuildNode(
      LSourceNode, ntModule, BuildModuleNodeData(LModule));
    LItem.Text := LModule.Name;
  end;
end;

procedure TProjectFilesFrame.AddPackageNodes(const ARoot: TTreeViewItem);
begin
  if not Assigned(ARoot) then
    Exit;

  var LPackageNode := BuildNode(ARoot, ntRootPackage, String.Empty);
  LPackageNode.Text := 'Packages (Zip imports and/or Wheels)';

  var LDeps := GetProjectServices().GetPackages(FProjectModel);
  for var LDep in LDeps do begin
    var LItem := BuildNode(LPackageNode, ntPackage, LDep.Path);
    LItem.Text := LDep.Name;
  end;
end;

procedure TProjectFilesFrame.AddOtherFileNodes(const ARoot: TTreeViewItem);
begin
  if not Assigned(ARoot) then
    Exit;

  var LBundleNode := BuildNode(ARoot, ntRootOtherFile, String.Empty);
  LBundleNode.Text := 'Other files';

  var LOthers := GetProjectServices().GetOtherFiles(FProjectModel);
  for var LOther in LOthers do begin
    var LItem := BuildNode(LBundleNode, ntOtherFile, LOther.Path);
    LItem.Text := LOther.Name;
  end;
end;

procedure TProjectFilesFrame.altvProjectFilesUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  //Modules
  actNewModule.Enabled := Assigned(FRoot)
    and NodeIsType(FRoot, TNodeType.ntProject);

  actAddModule.Enabled := Assigned(FRoot)
    and NodeIsType(FRoot, TNodeType.ntProject);

  actRemoveModule.Enabled := Assigned(tvProjectFiles.Selected)
    and NodeIsType(tvProjectFiles.Selected, TNodeType.ntModule);

  //Main module
  actSetToMain.Enabled := Assigned(tvProjectFiles.Selected)
    and NodeIsType(tvProjectFiles.Selected, TNodeType.ntModule)
    and Assigned(FProjectModel)
    and not GetProjectServices().IsMainModule(FProjectModel,
      GetItemFilePath(tvProjectFiles.Selected));

  //Packages
  actAddPackage.Enabled := Assigned(FRoot)
    and NodeIsType(FRoot, TNodeType.ntProject);

  actRemovePackage.Enabled := Assigned(tvProjectFiles.Selected)
    and NodeIsType(tvProjectFiles.Selected, TNodeType.ntPackage);

  //Other files
  actAddOtherFile.Enabled := Assigned(FRoot)
    and NodeIsType(FRoot, TNodeType.ntProject);

  actRemoveOtherFile.Enabled := Assigned(tvProjectFiles.Selected)
    and NodeIsType(tvProjectFiles.Selected, TNodeType.ntOtherFile);

  //Options
  {$IFDEF MSWINDOWS}
  actRevealFile.Visible := Assigned(tvProjectFiles.Selected) and (
    NodeIsType(tvProjectFiles.Selected, TNodeType.ntModule)
    or
    NodeIsType(tvProjectFiles.Selected, TNodeType.ntPackage)
    or
    NodeIsType(tvProjectFiles.Selected, TNodeType.ntOtherFile)
  );
  {$ELSE}
  actRevealFile.Visible := false;
  {$ENDIF MSWINDOWS}
end;

procedure TProjectFilesFrame.actAddOtherFileExecute(Sender: TObject);
begin
  if MenuActionsContainer.odOther.Execute() then begin
    //Save file into the project
    var LStream := TFileStream.Create(MenuActionsContainer.odOther.FileName, fmOpenRead);
    try
      var LOther := GetProjectServices().AddOtherFile(FProjectModel, MenuActionsContainer.odOther.FileName);
      if Assigned(LOther) then
      begin
        //Creates the tree item
        var LItem := BuildNode(
          GetNodeByNodeType(TNodeType.ntRootOtherFile),
          TNodeType.ntOtherFile,
          LOther.Path);

        LItem.Text := LOther.Name;

        tvProjectFiles.Selected := LItem;
      end;
    finally
      LStream.Free();
    end;
  end;
end;

procedure TProjectFilesFrame.actAddPackageExecute(Sender: TObject);
begin
  if MenuActionsContainer.odPackage.Execute() then begin
    //Save file into the project
    var LStream := TFileStream.Create(MenuActionsContainer.odPackage.FileName, fmOpenRead);
    try
      var LPackage := GetProjectServices().AddPackage(FProjectModel, MenuActionsContainer.odPackage.FileName);
      if Assigned(LPackage) then
      begin
        //Creates the tree item
        var LItem := BuildNode(
          GetNodeByNodeType(TNodeType.ntRootPackage),
          TNodeType.ntPackage,
          LPackage.Path);

        LItem.Text := LPackage.Name;

        tvProjectFiles.Selected := LItem;
      end;
    finally
      LStream.Free();
    end;
  end;
end;

procedure TProjectFilesFrame.actNewModuleExecute(Sender: TObject);
begin
  var LUntitledModule := TBuilderPaths.RecommendModuleName(
    FProjectModel.Defs.Storage,
    function(AFileName: string): boolean begin
      Result := GetProjectServices().GetModule(FProjectModel, AFileName) = nil;
    end);
  //Create an empty file
  TFile.WriteAllText(LUntitledModule, String.Empty, TEncoding.UTF8);
  var LModule := DoAddModule(LUntitledModule, true);
  LModule.Defs.Untracked := true;
end;

procedure TProjectFilesFrame.actAddModuleExecute(Sender: TObject);
begin
  if MenuActionsContainer.odFMXModule.Execute() then begin
    //Save file into the project
    var LStream := TFileStream.Create(MenuActionsContainer.odFMXModule.FileName, fmOpenRead);
    try
      DoAddModule(MenuActionsContainer.odFMXModule.FileName, false);
    finally
      LStream.Free();
    end;
  end;
end;

procedure TProjectFilesFrame.actRemovePackageExecute(Sender: TObject);
begin
  var LNode := tvProjectFiles.Selected;
  if not Assigned(LNode) then
    Exit;

  var LInfo := LNode.Data.AsType<TNodeInfo>();
  if not NodeIsType(LNode, TNodeType.ntPackage) then
    Exit;

  var LRemove := true;
  if not IsConsole then
    TDialogService.MessageDialog('Do you really want to remove this package?',
      TMsgDlgType.mtConfirmation,
      [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbNo, -1,
      procedure(const AResult: TModalResult) begin
        LRemove := AResult = mrYes;
      end);

  if not LRemove then
    Exit;

  LNode.Free();     

  GetProjectServices().RemovePackage(FProjectModel, LInfo.NodeData.AsString);
end;

procedure TProjectFilesFrame.actRevealFileExecute(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  var LFileName := GetItemFilePath(TTreeViewItem(tvProjectFiles.Selected));
  ShellExecute(
    0,
    nil,
    'explorer.exe',
    PChar('/select,' + LFileName),
    nil,
    SW_NORMAL
    );
  {$ENDIF MSWINDOWS}
end;

procedure TProjectFilesFrame.actRemoveModuleExecute(Sender: TObject);
begin
  var LNode := tvProjectFiles.Selected;
  if not Assigned(LNode) then
    Exit;

  var LInfo := LNode.Data.AsType<TNodeInfo>();
  if not NodeIsType(LNode, TNodeType.ntModule) then
    Exit;

  var LRemove := true;
  TDialogService.MessageDialog('Do you really want to remove this module?',
    TMsgDlgType.mtConfirmation,
    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbNo, -1,
    procedure(const AResult: TModalResult) begin
      LRemove := AResult = mrYes;
    end);

  if not LRemove then
    Exit;

  var LFilePath := GetItemFilePath(LNode);
  GetProjectServices().RemoveModule(FProjectModel, LFilePath);
  FEditorServices.CloseEditor(LFilePath);

  LNode.Free();
end;

procedure TProjectFilesFrame.actRemoveOtherFileExecute(Sender: TObject);
begin
  var LNode := tvProjectFiles.Selected;
  if not Assigned(LNode) then
    Exit;

  var LInfo := LNode.Data.AsType<TNodeInfo>();
  if not NodeIsType(LNode, TNodeType.ntOtherFile) then
    Exit;

  var LRemove := true;
  if not IsConsole then
    TDialogService.MessageDialog('Do you really want to remove this file?',
      TMsgDlgType.mtConfirmation,
      [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbNo, -1,
      procedure(const AResult: TModalResult) begin
        LRemove := AResult = mrYes;
      end);

  if not LRemove then
    Exit;

  LNode.Free();

  GetProjectServices().RemoveOtherFile(FProjectModel, LInfo.NodeData.AsString);
end;

procedure TProjectFilesFrame.actSetToMainExecute(Sender: TObject);
begin
  GetProjectServices().SetMainModule(FProjectModel,
    GetItemFilePath(tvProjectFiles.Selected));
end;

procedure TProjectFilesFrame.LoadProject(const AProjectModel: TProjectModel);
begin
  FProjectModel := AProjectModel;
  tvProjectFiles.Clear();
  FRoot := AddProjectNode();
  AddBuildConfigurationNodes(FRoot);
  AddTargetPlatformNodes(FRoot);
  AddTargetPythonNodes(FRoot);
  AddPackageNodes(FRoot);
  AddOtherFileNodes(FRoot);
  AddModuleNodes(FRoot);
end;

end.
