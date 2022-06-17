unit Frame.ProjectFiles;

interface

uses
  System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.TreeView, System.Rtti, FMX.Menus, System.ImageList,
  FMX.ImgList, Builder.Services, System.Actions, FMX.ActnList, Builder.Model.Project,
  Container.Images;

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
  private
    FOnScriptFileDblClick: TNotifyEvent;
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
  public
    constructor Create(AOwner: TComponent); override;

    procedure LoadProject(const AProjectModel: TProjectModel);
    function GetDefaultScriptFilePath(): string;
    function GetItemFilePath(const AItem: TTreeViewItem): string;

    property FileExt: TArray<string> read FFileExt write FFileExt;
    property OnScriptFileDblClick: TNotifyEvent read FOnScriptFileDblClick write FOnScriptFileDblClick;
  end;

implementation

uses
  System.StrUtils, System.IOUtils, Builder.Services.Factory, System.SysUtils;

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
end;

function TProjectFilesFrame.FileIsScriptFile(const AFileName: string): boolean;
begin
  Result := MatchStr(TPath.GetExtension(AFileName), ['.py', '.pydfm', '.pyfmx']);
end;

function TProjectFilesFrame.GetItemFilePath(const AItem: TTreeViewItem): string;
begin
  Result := String(AItem.Data.AsType<TNodeInfo>().FilePath);
end;

function TProjectFilesFrame.GetDefaultScriptFilePath: string;
begin
  if not Assigned(FProjectModel) then
    Exit(String.Empty);

  //We try to find the main script
  for var LScriptFile in FProjectModel.Files.Files do begin
    if (TPath.GetFileName(LScriptFile) = 'main.py') and TFile.Exists(LScriptFile) then
      Exit(LScriptFile);
  end;

  //If we don't have a main script, we try to get the first existent script
  for var LScriptFile in FProjectModel.Files.Files do begin
    if TFile.Exists(LScriptFile) then
      Exit(LScriptFile);
  end;

  Result := String.Empty;
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
    AItem.OnDblClick := OnScriptFileDblClick;
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
  actAddFile.Visible := Assigned(tvProjectFiles.Selected)
    and NodeIsType(tvProjectFiles.Selected, TNodeType.ntProject);

  actRemoveFile.Visible := Assigned(tvProjectFiles.Selected)
    and NodeIsType(tvProjectFiles.Selected, TNodeType.ntModule);

  actSetToMain.Visible := Assigned(tvProjectFiles.Selected)
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
      if GetProjectServices().AddScriptFile(
        FProjectModel, odtvProjectFiles.FileName) then
      begin
        //Creates the tree item
        var LItem := BuildNode(FRoot,
          GetNodeTypeByFileName(odtvProjectFiles.FileName),
          odtvProjectFiles.FileName);
        LItem.Text := TPath.GetFileName(odtvProjectFiles.FileName);
        SaveChanges();
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

  LNode.Free();

  GetProjectServices().RemoveScriptFile(FProjectModel, String(LInfo.FilePath));
  SaveChanges();
end;

procedure TProjectFilesFrame.actSetToMainExecute(Sender: TObject);
begin
  FProjectServices.SetMainScriptFile(FProjectModel,
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
