unit Frame.ProjectFiles;

interface

uses
  System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.TreeView, System.Rtti, FMX.Menus, System.ImageList,
  FMX.ImgList, Builder.Services, System.Actions, FMX.ActnList,
  Builder.Chain, Builder.Model.Project, Container.Images;

type
  TNodeType = (ntProject, ntModule, ntOther);

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
    procedure actAddFileExecute(Sender: TObject);
    procedure actRemoveFileExecute(Sender: TObject);
    procedure altvProjectFilesUpdate(Action: TBasicAction;
      var Handled: Boolean);
    procedure actSetToMainExecute(Sender: TObject);
  private
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
      const AFilePath: string = ''): TTreeViewItem;
    function NodeIsType(const AItem: TTreeViewItem; const ANodeType: TNodeType): boolean;
    function AddProjectNode(): TTreeViewItem;
    procedure AddDirectoryModuleNodes(const ARoot: TTreeViewItem);
    function FileIsScriptFile(const AFileName: string): boolean;
    function GetNodeTypeByFileName(const AFileName: string): TNodeType;
    procedure TreeViewIteDblClick(Sender: TObject);
    procedure BroadcastOpenFile(const AFilePath: string);
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

implementation

uses
  System.StrUtils, System.IOUtils, System.SysUtils,
  FMX.DialogService, Builder.Services.Factory;

type
  TProjectFilesTreeViewItem = class(FMX.TreeView.TTreeViewItem)
  private
    FData: TValue;
  protected
    function GetData(): TValue; override;
    procedure SetData(const Value: TValue); override;
  end;

  PNodeInfo = ^TNodeInfo;
  TNodeInfo = record
  public
    constructor Create(const ANodeType: TNodeType); overload;
    constructor Create(const ANodeType: TNodeType; const AFilePath: string); overload;

    class operator Implicit(ANodeInfo: TNodeInfo): TValue;
  public
    case NodeType: TNodeType of
      ntModule: (
        FilePath: string[255];
      );
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
  const AFilePath: string);
begin
  NodeType := ANodeType;
  case NodeType of
    ntModule: FilePath := ShortString(AFilePath);
  end;
end;

constructor TNodeInfo.Create(const ANodeType: TNodeType);
begin
  Create(ANodeType, String.Empty);
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
  Result := String(AItem.Data.AsType<TNodeInfo>().FilePath);
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
  if AItem.Data.AsType<TNodeInfo>().NodeType = TNodeType.ntModule then
    AItem.OnDblClick := TreeViewIteDblClick;
end;

procedure TProjectFilesFrame.LoadIcon(const AItem: TTreeViewItem);
begin
  case AItem.Data.AsType<TNodeInfo>().NodeType of
    ntProject: AItem.ImageIndex := PROJECT_ICON_INDEX;
    ntModule : AItem.ImageIndex := MODULE_ICON_INDEX;
    else
      AItem.ImageIndex := ANY_FILE_ICON_INDEX;
  end;
end;

procedure TProjectFilesFrame.LoadStyles(const AItem: TTreeViewItem);
begin
  case AItem.Data.AsType<TNodeInfo>().NodeType of
    ntProject: begin
      AItem.Expand();
      AItem.TextSettings.Font.Size := 16;
      AItem.TextSettings.Font.Style := [TFontStyle.fsBold];
    end;
    ntModule : begin
      AItem.TextSettings.Font.Size := 14;
    end;
  end;
end;

function TProjectFilesFrame.NodeIsType(const AItem: TTreeViewItem;
  const ANodeType: TNodeType): boolean;
begin
  Result := AItem.Data.AsType<TNodeInfo>().NodeType = ANodeType;
end;

procedure TProjectFilesFrame.SaveChanges;
begin
  GetProjectServices().SaveProject(FProjectModel);
end;

procedure TProjectFilesFrame.TreeViewIteDblClick(Sender: TObject);
begin
  BroadcastOpenFile(GetItemFilePath(TTreeViewItem(Sender)));
end;

procedure TProjectFilesFrame.UnLoadProject(const AProjectModel: TProjectModel);
begin
  tvProjectFiles.Clear();
  FRoot := nil;
  FProjectModel := nil;
end;

procedure TProjectFilesFrame.BroadcastOpenFile(const AFilePath: string);
begin
  TGlobalBuilderChain.BroadcastEventAsync(
    TOpenFileEvent.Create(AFilePath));
end;

function TProjectFilesFrame.BuildNode(const AParent: TFmxObject;
  const ANodeType: TNodeType; const AFilePath: string): TTreeViewItem;
begin
  Result := TProjectFilesTreeViewItem.Create(AParent);
  Result.Parent := AParent;
  Result.Data := TNodeInfo.Create(ANodeType, AFilePath);
  LoadStyles(Result);
  LoadIcon(Result);
  LoadEvents(Result);
end;

function TProjectFilesFrame.AddProjectNode: TTreeViewItem;
begin
  Result := BuildNode(tvProjectFiles, ntProject);
  Result.Text := ExtractFileName(
    ExcludeTrailingPathDelimiter(FProjectModel.ProjectName));
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
    and not FProjectServices.IsMainScriptFile(FProjectModel,
      GetItemFilePath(tvProjectFiles.Selected));
end;

procedure TProjectFilesFrame.actAddFileExecute(Sender: TObject);
begin
  if odtvProjectFiles.Execute() then begin
    //Save file into the project
    var LStream := TFileStream.Create(odtvProjectFiles.FileName, fmOpenRead);
    try
      if GetProjectServices().AddScriptFile(FProjectModel, odtvProjectFiles.FileName) then
      begin
        //Add as the deafult script file if none
        if FProjectModel.Files.MainFile.IsEmpty() then
          GetProjectServices().SetMainScriptFile(FProjectModel, odtvProjectFiles.FileName);
        //Creates the tree item
        var LItem := BuildNode(FRoot,
          GetNodeTypeByFileName(odtvProjectFiles.FileName),
          odtvProjectFiles.FileName);
        LItem.Text := TPath.GetFileName(odtvProjectFiles.FileName);
        SaveChanges();
        tvProjectFiles.Selected := LItem;
      end;
    finally
      LStream.Free();
    end;
  end;
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

  GetProjectServices().RemoveScriptFile(FProjectModel, String(LInfo.FilePath));
  SaveChanges();
end;

procedure TProjectFilesFrame.actSetToMainExecute(Sender: TObject);
begin
  GetProjectServices().SetMainScriptFile(FProjectModel,
    GetItemFilePath(tvProjectFiles.Selected));
  SaveChanges();
end;

procedure TProjectFilesFrame.AddDirectoryModuleNodes(const ARoot: TTreeViewItem);
begin
  if not Assigned(ARoot) then
    Exit;

  var LFiles := GetProjectServices().GetScriptFiles(FProjectModel);

  for var LFile in LFiles do begin
    var LItem := BuildNode(ARoot, GetNodeTypeByFileName(LFile), LFile);
    LItem.Text := TPath.GetFileName(LFile);
  end;
end;

procedure TProjectFilesFrame.LoadProject(const AProjectModel: TProjectModel);
begin
  FProjectModel := AProjectModel;
  tvProjectFiles.Clear();
  FRoot := AddProjectNode();
  AddDirectoryModuleNodes(FRoot);
end;

end.
