unit Frame.ProjectFiles;

interface

uses
  System.Types, System.UITypes, System.Classes, System.Variants, System.Rtti,
  System.Actions, FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.Layouts, FMX.TreeView, FMX.Menus, System.ImageList,
  FMX.ImgList, FMX.ActnList,
  Builder.Services, Builder.Types, Builder.Chain, Builder.Model.Project,
  Container.Images;

type
  TNodeType = (
    ntProject, ntModule,
    ntBuildConfiguration, ntTargetPlatform, ntTargetPython,
    ntBundle,
    ntSource,
    ntOther);

  TProjectFilesFrame = class(TFrame)
    tvProjectFiles: TTreeView;
    pmtvProjectFiles: TPopupMenu;
    miAddFile: TMenuItem;
    miRemoveFile: TMenuItem;
    altvProjectFiles: TActionList;
    actAddFile: TAction;
    actRemoveFile: TAction;
    odtvProjectFiles: TOpenDialog;
    miSetToMain: TMenuItem;
    actSetToMain: TAction;
    miSeparator: TMenuItem;
    miAddBundle: TMenuItem;
    miRemoveBundle: TMenuItem;
    actAddBundle: TAction;
    actRemoveBundle: TAction;
    procedure actAddFileExecute(Sender: TObject);
    procedure actRemoveFileExecute(Sender: TObject);
    procedure altvProjectFilesUpdate(Action: TBasicAction;
      var Handled: Boolean);
    procedure actSetToMainExecute(Sender: TObject);
    procedure actAddBundleExecute(Sender: TObject);
    procedure actRemoveBundleExecute(Sender: TObject);
  private
    [weak]
    FProjectModel: TProjectModel;
    FFileExt: TArray<string>;
    FProjectServices: IProjectServices;
    FRoot: TTreeViewItem;
    FOpenProjectEvent: IDisconnectable;
    FCloseProjectEvent: IDisconnectable;
  private
    function GetProjectServices: IProjectServices;
    procedure SaveChanges();
    // TreeView operations
    procedure LoadIcon(const AItem: TTreeViewItem);
    procedure LoadStyles(const AItem: TTreeViewItem);
    procedure LoadEvents(const AItem: TTreeViewItem);
    function BuildNode(const AParent: TFmxObject; const ANodeType: TNodeType;
      const ANodeData: TValue): TTreeViewItem;
    function NodeIsType(const AItem: TTreeViewItem; const ANodeType: TNodeType): boolean;
    function ParentNodeIsType(const AItem: TTreeViewItem; const ANodeType: TNodeType): boolean;
    //TreeView nodes
    function AddProjectNode(): TTreeViewItem;
    procedure AddBuildConfigurationNodes(const ARoot: TTreeViewItem);
    procedure AddTargetPlatformNodes(const ARoot: TTreeViewItem);
    procedure AddTargetPythonNodes(const ARoot: TTreeViewItem);
    procedure AddSourceNodes(const ARoot: TTreeViewItem);
    procedure AddBundleNodes(const ARoot: TTreeViewItem);
    //Node events
    procedure OnTreeViewItemDblClickModule(Sender: TObject);
    procedure OnTreeViewItemDblClickBuildConfigurationItem(Sender: TObject);
    procedure OnTreeViewItemDblClickPlatformItem(Sender: TObject);
    procedure OnTreeViewItemDblClickPythonItem(Sender: TObject);
    //Visual node updates
    procedure UpdateSelectedPlatform(const ASelected: TTreeViewItem);
    procedure UpdateSelectedPython(const ASelected: TTreeViewItem);
    procedure UpdateSelectedBuildConfiguration(const ASelected: TTreeViewItem);
    //
    function FileIsScriptFile(const AFileName: string): boolean;
    function GetNodeTypeByFileName(const AFileName: string): TNodeType;
    function GetNodeByNodeType(const ANodeType: TNodeType): TTreeViewItem;
    function GetNodeData<T>(const AItem: TTreeViewItem): T;
    //Chained events
    procedure BroadcastOpenFile(const AFilePath: string);
    procedure BroadcastCloseFile(const AFilePath: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    procedure LoadProject(const AProjectModel: TProjectModel);
    procedure UnLoadProject(const AProjectModel: TProjectModel);

    function GetDefaultScriptFilePath(const AProject: TProjectModel): string; overload;
    function GetDefaultScriptFilePath(): string; overload;

    function GetItemFilePath(const AItem: TTreeViewItem): string;

    property FileExt: TArray<string> read FFileExt write FFileExt;
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
  Builder.Services.Factory;

type
  TProjectFilesTreeViewItem = class(FMX.TreeView.TTreeViewItem)
  private
    FData: TValue;
  protected
    function GetData(): TValue; override;
    procedure SetData(const Value: TValue); override;
  end;

{$R *.fmx}

{ TProjectFilesTreeViewItem }

function TProjectFilesTreeViewItem.GetData: TValue;
begin
  Result := FData;
end;

procedure TProjectFilesTreeViewItem.SetData(const Value: TValue);
begin
  FData := Value;
end;

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
  FFileExt := ['.py'];

  FOpenProjectEvent := TGlobalBuilderChain.SubscribeToEvent<TOpenProjectEvent>(
    procedure(const AEventNotification: TOpenProjectEvent)
    begin
      TGlobalBuilderChain.BroadcastEventAsync(
        TAsyncOperationStartedEvent.Create(TAsyncOperation.OpenProject));

      var LProject := AEventNotification.Body.Project;
      TThread.Queue(TThread.Current,
        procedure()
        begin
          try
            LoadProject(LProject);
            var LDefaultScriptFile := GetDefaultScriptFilePath();
            if TFile.Exists(LDefaultScriptFile) then
              BroadcastOpenFile(LDefaultScriptFile);
          finally
            TGlobalBuilderChain.BroadcastEventAsync(
              TAsyncOperationEndedEvent.Create(TAsyncOperation.OpenProject));
          end;
        end);
    end);

  FCloseProjectEvent := TGlobalBuilderChain.SubscribeToEvent<TCloseProjectEvent>(
    procedure(const AEventNotification: TCloseProjectEvent)
    begin
      var LProject := AEventNotification.Body.Project;
      TThread.Queue(TThread.Current,
        procedure()
        begin
          UnLoadProject(LProject);
        end);
    end);
end;

destructor TProjectFilesFrame.Destroy;
begin
  FOpenProjectEvent.Disconnect();
  FCloseProjectEvent.Disconnect();
  inherited;
end;

function TProjectFilesFrame.FileIsScriptFile(const AFileName: string): boolean;
begin
  Result := MatchStr(TPath.GetExtension(AFileName), ['.py', '.pydfm', '.pyfmx']);
end;

function TProjectFilesFrame.GetItemFilePath(const AItem: TTreeViewItem): string;
begin
  Result := String(AItem.Data.AsType<TNodeInfo>().NodeData.AsString);
end;

function TProjectFilesFrame.GetDefaultScriptFilePath(const AProject: TProjectModel): string;
begin
  if not Assigned(AProject) then
    Exit(String.Empty);

  //We try to find the main script
  if TFile.Exists(AProject.Files.MainFile) then
    Exit(AProject.Files.MainFile);

  //If we don't have a main script, we try to get the first existent script
  for var LScriptFile in AProject.Files.Files do begin
    if TFile.Exists(LScriptFile) then
      Exit(LScriptFile);
  end;

  Result := String.Empty;
end;

function TProjectFilesFrame.GetDefaultScriptFilePath: string;
begin
  Result := GetDefaultScriptFilePath(FProjectModel);
end;

function TProjectFilesFrame.GetNodeByNodeType(
  const ANodeType: TNodeType): TTreeViewItem;

function RecursivelyGetNode(const ARoot: TTreeViewItem; 
  const ANodeType: TNodeType): TTreeViewItem;
begin
  if NodeIsType(ARoot, ANodeType) then
    Exit(ARoot);
    
  for var I := 0 to ARoot.Count - 1 do
  begin  
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

function TProjectFilesFrame.GetNodeData<T>(const AItem: TTreeViewItem): T;
begin
  Result := AItem.Data.AsType<TNodeInfo>.NodeData.AsType<T>;
end;

function TProjectFilesFrame.GetNodeTypeByFileName(
  const AFileName: string): TNodeType;
begin
  Result := TNodeType.ntOther;
  if FileIsScriptFile(AFileName) then
    Result := ntModule;
end;

function TProjectFilesFrame.GetProjectServices: IProjectServices;
begin
  if not Assigned(FProjectServices) then
    FProjectServices := TServiceSimpleFactory.CreateProject();
  Result := FProjectServices;
end;

procedure TProjectFilesFrame.LoadEvents(const AItem: TTreeViewItem);
begin
  case AItem.Data.AsType<TNodeInfo>().NodeType of
    ntModule: AItem.OnDblClick := OnTreeViewItemDblClickModule;
    ntOther:
    begin
      if ParentNodeIsType(AItem, ntBuildConfiguration) then
        AItem.OnDblClick := OnTreeViewItemDblClickBuildConfigurationItem
      else if ParentNodeIsType(AItem, ntTargetPlatform) then
        AItem.OnDblClick := OnTreeViewItemDblClickPlatformItem
      else if ParentNodeIsType(AItem, ntTargetPython) then
        AItem.OnDblClick := OnTreeViewItemDblClickPythonItem;
    end
  end;
end;

procedure TProjectFilesFrame.LoadIcon(const AItem: TTreeViewItem);
begin
  case AItem.Data.AsType<TNodeInfo>().NodeType of
    ntProject: AItem.ImageIndex := PROJECT_ICON_INDEX;
    ntModule: AItem.ImageIndex := MODULE_ICON_INDEX;
    ntSource: AItem.ImageIndex := SOURCE_ICON_INDEX;
    ntBundle: AItem.ImageIndex := BUNDLE_ICON_INDEX;
    ntBuildConfiguration: AItem.ImageIndex := BUILD_CONFIGURATION_ICON_INDEX;
    ntTargetPlatform: AItem.ImageIndex := TARGET_PLATFORMS_ICON_INDEX;
    ntTargetPython: AItem.ImageIndex := TARGET_PYTHON_ICON_INDEX;
    ntOther:
    begin
       if ParentNodeIsType(AItem, ntBuildConfiguration) then
        AItem.ImageIndex := BUILD_CONFIGURATION_ITEM_ICON_INDEX
      else if ParentNodeIsType(AItem, ntTargetPlatform) then
        AItem.ImageIndex := TARGET_PLATFORMS_ANDROID_ICON_INDEX
      else if ParentNodeIsType(AItem, ntTargetPython) then
        AItem.ImageIndex := TARGET_PYTHON_VER_ICON_INDEX
      else if ParentNodeIsType(AItem, ntBundle) then
        AItem.ImageIndex := BUNDLE_ICON_INDEX
      else
        AItem.ImageIndex := ANY_FILE_ICON_INDEX;
    end
    else
      AItem.ImageIndex := ANY_FILE_ICON_INDEX;
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
      ntOther:
      begin
        //Build configuration nodes
        if ParentNodeIsType(AItem, ntBuildConfiguration) then
        begin
          if not (GetNodeData<TBuildConfiguration>(AItem) = FProjectModel.BuildConfiguration) then
            Exit;
          RestoreChildrenTextSettings(AItem);
          AItem.ResultingTextSettings.Font.Style := [TFontStyle.fsBold];
        //Android architecture nodes
        end else if ParentNodeIsType(AItem, ntTargetPlatform) then
        begin
          if not (GetNodeData<TArchitecture>(AItem) = FProjectModel.Architecture) then
            Exit;
          RestoreChildrenTextSettings(AItem);
          AItem.ResultingTextSettings.Font.Style := [TFontStyle.fsBold];
        //Python itens node
        end else if ParentNodeIsType(AItem, ntTargetPython) then
        begin
          if not (GetNodeData<TPythonVersion>(AItem) = FProjectModel.PythonVersion) then
            Exit;
          RestoreChildrenTextSettings(AItem);
          AItem.ResultingTextSettings.Font.Style := [TFontStyle.fsBold];
        end;
      end
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

function TProjectFilesFrame.ParentNodeIsType(const AItem: TTreeViewItem;
  const ANodeType: TNodeType): boolean;
begin
  Result := Assigned(AItem.ParentItem()) and NodeIsType(AItem.ParentItem(), ANodeType);
end;

procedure TProjectFilesFrame.SaveChanges;
begin
  GetProjectServices().SaveProject(FProjectModel);
end;

procedure TProjectFilesFrame.OnTreeViewItemDblClickModule(Sender: TObject);
begin
  BroadcastOpenFile(GetItemFilePath(TTreeViewItem(Sender)));
end;

procedure TProjectFilesFrame.OnTreeViewItemDblClickBuildConfigurationItem(
  Sender: TObject);
begin
  var LNode := TTreeViewItem(Sender);
  FProjectModel.BuildConfiguration := LNode.Data.AsType<TNodeInfo>.NodeData.AsType<TBuildConfiguration>;
  UpdateSelectedBuildConfiguration(LNode);
  SaveChanges;
  LoadStyles(LNode);
end;

procedure TProjectFilesFrame.OnTreeViewItemDblClickPlatformItem(
  Sender: TObject);
begin
  var LNode := TTreeViewItem(Sender);
  FProjectModel.Architecture := LNode.Data.AsType<TNodeInfo>.NodeData.AsType<TArchitecture>;  
  UpdateSelectedPlatform(LNode);
  SaveChanges;
  LoadStyles(LNode);
end;

procedure TProjectFilesFrame.OnTreeViewItemDblClickPythonItem(Sender: TObject);
begin
  var LNode := TTreeViewItem(Sender);
  FProjectModel.PythonVersion := LNode.Data.AsType<TNodeInfo>.NodeData.AsType<TPythonVersion>;
  UpdateSelectedPython(LNode);
  SaveChanges;
  LoadStyles(LNode);
end;

procedure TProjectFilesFrame.UnLoadProject(const AProjectModel: TProjectModel);
begin
  tvProjectFiles.Clear();
  FRoot := nil;
  FProjectModel := nil;
end;

procedure TProjectFilesFrame.UpdateSelectedBuildConfiguration(
  const ASelected: TTreeViewItem);
begin
  var LRoot := ASelected.ParentItem;
  LRoot.Text := Format('Build Configuration (%s)', [
    ASelected.Data.AsType<TNodeInfo>.NodeData.AsType<TBuildConfiguration>.ToBuildConfiguration()]);
end;

procedure TProjectFilesFrame.UpdateSelectedPlatform(
  const ASelected: TTreeViewItem);
begin
  var LRoot := ASelected.ParentItem;
  LRoot.Text := Format('Target Platforms (%s)', [
    ASelected.Data.AsType<TNodeInfo>.NodeData.AsType<TArchitecture>.ToTargetPlatform()]);
end;

procedure TProjectFilesFrame.UpdateSelectedPython(
  const ASelected: TTreeViewItem);
begin
  var LRoot := ASelected.ParentItem;
  LRoot.Text := Format('Target Python (%s)', [
    ASelected.Data.AsType<TNodeInfo>.NodeData.AsType<TPythonVersion>.ToTargetPython()]);
end;

procedure TProjectFilesFrame.BroadcastOpenFile(const AFilePath: string);
begin
  TGlobalBuilderChain.BroadcastEventAsync(
    TOpenFileEvent.Create(AFilePath));
end;

procedure TProjectFilesFrame.BroadcastCloseFile(const AFilePath: string);
begin
  TGlobalBuilderChain.BroadcastEventAsync(
    TCloseFileEvent.Create(AFilePath));
end;

function TProjectFilesFrame.BuildNode(const AParent: TFmxObject;
  const ANodeType: TNodeType; const ANodeData: TValue): TTreeViewItem;
begin
  tvProjectFiles.BeginUpdate();
  try
    Result := TProjectFilesTreeViewItem.Create(AParent);
    Result.BeginUpdate();
    try
      Result.Parent := AParent;
      Result.Data := TNodeInfo.Create(ANodeType, ANodeData);
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
  Result := BuildNode(tvProjectFiles, ntProject, String.Empty);
  Result.Text := ExtractFileName(
    ExcludeTrailingPathDelimiter(FProjectModel.ProjectName));
end;

procedure TProjectFilesFrame.AddBuildConfigurationNodes(
  const ARoot: TTreeViewItem);
begin
  if not Assigned(ARoot) then
    Exit;

  var LBuildConfigurationNode := BuildNode(ARoot, ntBuildConfiguration, String.Empty);
  var LCurrentBuildConfiguration := FProjectModel.BuildConfiguration;

  for var LBuildConfiguration := Low(TBuildConfiguration) to High(TBuildConfiguration) do
  begin
    var LNode := BuildNode(LBuildConfigurationNode, ntOther,
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

  var LTargetPlatformsNode := BuildNode(ARoot, ntTargetPlatform, String.Empty);
  var LCurrentArch := FProjectModel.Architecture;

  for var LArch := Low(TArchitecture) to High(TArchitecture) do
  begin
    var LNode := BuildNode(LTargetPlatformsNode, ntOther,
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

  var LTargetPythonNode := BuildNode(ARoot, ntTargetPython, String.Empty);
  var LCurrentPythonVer := FProjectModel.PythonVersion;

  for var LPythonVer := Low(TPythonVersion) to High(TPythonVersion) do
  begin
    var LNode := BuildNode(LTargetPythonNode, ntOther,
      TValue.From<TPythonVersion>(LPythonVer));
    LNode.Text := LPythonVer.ToTargetPython();
    if LPythonVer = LCurrentPythonVer then
      UpdateSelectedPython(LNode);
  end;
end;

procedure TProjectFilesFrame.AddSourceNodes(const ARoot: TTreeViewItem);
begin
  if not Assigned(ARoot) then
    Exit;

  var LSourceNode := BuildNode(ARoot, ntSource, String.Empty);
  LSourceNode.Text := 'Source';
  LSourceNode.IsExpanded := true;

  var LFiles := GetProjectServices().GetScriptFiles(FProjectModel);
  for var LFile in LFiles do begin
    var LItem := BuildNode(LSourceNode, GetNodeTypeByFileName(LFile), LFile);
    LItem.Text := TPath.GetFileName(LFile);
  end;
end;

procedure TProjectFilesFrame.AddBundleNodes(const ARoot: TTreeViewItem);
begin
  if not Assigned(ARoot) then
    Exit;

  var LBundleNode := BuildNode(ARoot, ntBundle, String.Empty);
  LBundleNode.Text := 'Bundles (Zip imports and/or Wheels)';

  var LDeps := GetProjectServices().GetDependencies(FProjectModel);
  for var LDep in LDeps do
  begin
    var LItem := BuildNode(LBundleNode, ntOther, LDep);
    LItem.Text := TPath.GetFileName(LDep);  
  end;
end;

procedure TProjectFilesFrame.altvProjectFilesUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  actAddFile.Enabled := Assigned(FRoot)
    and NodeIsType(FRoot, TNodeType.ntProject);

  actRemoveFile.Enabled := Assigned(tvProjectFiles.Selected)
    and NodeIsType(tvProjectFiles.Selected, TNodeType.ntModule);

  actSetToMain.Enabled := Assigned(tvProjectFiles.Selected)
    and NodeIsType(tvProjectFiles.Selected, TNodeType.ntModule)
    and Assigned(FProjectModel)
    and not GetProjectServices().IsMainScriptFile(FProjectModel,
      GetItemFilePath(tvProjectFiles.Selected));

  actAddBundle.Enabled := Assigned(FRoot)
    and NodeIsType(FRoot, TNodeType.ntProject);

  actRemoveBundle.Enabled := Assigned(tvProjectFiles.Selected)
    and ParentNodeIsType(tvProjectFiles.Selected, TNodeType.ntBundle);
end;

procedure TProjectFilesFrame.actAddBundleExecute(Sender: TObject);
begin
  odtvProjectFiles.Filter := 'Zip imports|*.zip|PIP wheel|*.whl'; 
  odtvProjectFiles.FilterIndex := 1;
  if odtvProjectFiles.Execute() then begin
    //Save file into the project
    var LStream := TFileStream.Create(odtvProjectFiles.FileName, fmOpenRead);
    try
      if GetProjectServices().AddDependency(FProjectModel, odtvProjectFiles.FileName) then
      begin
        //Creates the tree item
        var LItem := BuildNode(
          GetNodeByNodeType(TNodeType.ntBundle),
          TNodeType.ntBundle,
          odtvProjectFiles.FileName);

        LItem.Text := TPath.GetFileName(odtvProjectFiles.FileName);

        SaveChanges();
      end;
    finally
      LStream.Free();
    end;
  end;
end;

procedure TProjectFilesFrame.actAddFileExecute(Sender: TObject);
begin                  
  odtvProjectFiles.Filter := 'Python module|*.py|Delphi FMX file|*.pyfmx'; 
  odtvProjectFiles.FilterIndex := 1;
  if odtvProjectFiles.Execute() then begin
    //Save file into the project
    var LStream := TFileStream.Create(odtvProjectFiles.FileName, fmOpenRead);
    try
      if GetProjectServices().AddScriptFile(FProjectModel, odtvProjectFiles.FileName) then
      begin
        //Creates the tree item
        var LItem := BuildNode(FRoot,
          GetNodeTypeByFileName(odtvProjectFiles.FileName),
          odtvProjectFiles.FileName);
        LItem.Text := TPath.GetFileName(odtvProjectFiles.FileName);

        //Add as the deafult script file if none
        if NodeIsType(LItem, TNodeType.ntModule) then
          if FProjectModel.Files.MainFile.IsEmpty() then
            GetProjectServices().SetMainScriptFile(FProjectModel, odtvProjectFiles.FileName);

        SaveChanges();
        tvProjectFiles.Selected := LItem;

        if NodeIsType(LItem, TNodeType.ntModule) then
          BroadcastOpenFile(odtvProjectFiles.FileName);
      end;
    finally
      LStream.Free();
    end;
  end;
end;

procedure TProjectFilesFrame.actRemoveBundleExecute(Sender: TObject);
begin
  var LNode := tvProjectFiles.Selected;
  if not Assigned(LNode) then
    Exit;

  var LInfo := LNode.Data.AsType<TNodeInfo>();
  if not ParentNodeIsType(LNode, TNodeType.ntBundle) then
    Exit;

  var LRemove := true;
  if not IsConsole then
    TDialogService.MessageDialog('Do you really want to remove this bundle?',
      TMsgDlgType.mtConfirmation,
      [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbNo, -1,
      procedure(const AResult: TModalResult) begin
        LRemove := AResult = mrYes;
      end);

  if not LRemove then
    Exit;

  LNode.Free();     

  GetProjectServices().RemoveDependency(FProjectModel, LInfo.NodeData.AsString);
  SaveChanges();
end;

procedure TProjectFilesFrame.actRemoveFileExecute(Sender: TObject);
begin
  var LNode := tvProjectFiles.Selected;
  if not Assigned(LNode) then
    Exit;

  var LInfo := LNode.Data.AsType<TNodeInfo>();
  if not NodeIsType(LNode, TNodeType.ntModule) then
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

  GetProjectServices().RemoveScriptFile(FProjectModel, LInfo.NodeData.AsString);
  SaveChanges();
  BroadcastCloseFile(LInfo.NodeData.AsString);
end;

procedure TProjectFilesFrame.actSetToMainExecute(Sender: TObject);
begin
  GetProjectServices().SetMainScriptFile(FProjectModel,
    GetItemFilePath(tvProjectFiles.Selected));
  SaveChanges();
end;

procedure TProjectFilesFrame.LoadProject(const AProjectModel: TProjectModel);
begin
  FProjectModel := AProjectModel;
  tvProjectFiles.Clear();
  FRoot := AddProjectNode();
  AddBuildConfigurationNodes(FRoot);
  AddTargetPlatformNodes(FRoot);
  AddTargetPythonNodes(FRoot);
  AddBundleNodes(FRoot);
  AddSourceNodes(FRoot);
end;

end.
