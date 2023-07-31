unit Frame.Editor.Control;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.TabControl, FMX.Objects, System.Rtti, Data.DB,
  Frame.Editor.TabItem,
  Builder.Services, Builder.Types, Builder.TabControl;

type
  TEditorControlFrame = class(TFrame, IEditorControl)
    tbScripts: TTabControl;
    dsActiveSource: TDataSource;
    procedure dsActiveSourceDataChange(Sender: TObject; Field: TField);
    procedure tbScriptsChange(Sender: TObject);
  private
    FEditorServices: IEditorServices;
    function DoCreateTab(const ACanClose: boolean = true): TTabItem;
    function GetEditorTab(const AFileName: string): TTabItem;
    function GetActiveTextEditor: ITextEditor;
    //IEditorControl implementation
    function OpenEditor(const AFileName: string;
      const AEditing: boolean = false): ITextEditor;
    procedure CloseEditor(const AFileName: string);
    procedure CloseAllEditors();
    function FindEditor(const AFileName: string): ITextEditor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  end;

implementation

uses
  System.IOUtils,
  Container.DataSet.Debugger;

{$R *.fmx}

{ TScriptEditorFrame }

constructor TEditorControlFrame.Create(AOwner: TComponent);
begin
  inherited;
  FEditorServices := TBuilderService.CreateService<IEditorServices>;
  FEditorServices.EditorControl := Self;
end;

destructor TEditorControlFrame.Destroy;
begin
  FEditorServices.EditorControl := nil;
  FEditorServices := nil;
  inherited;
end;

function TEditorControlFrame.DoCreateTab(const ACanClose: boolean): TTabItem;
begin
  Result := tbScripts.Add(TEditorTabItem) as TEditorTabItem;
  Result.AutoSize := false;
  TEditorTabItem(Result).CanClose := ACanClose;
end;

procedure TEditorControlFrame.dsActiveSourceDataChange(Sender: TObject;
  Field: TField);
begin
  var LActiveTextEditor := GetActiveTextEditor();
  if Assigned(LActiveTextEditor) then
    LActiveTextEditor.ShowActiveLine := false;

  if DebuggerDataSetContainer.fdmtActiveSource.IsEmpty() then
    Exit();

  var LFilePath := DebuggerDataSetContainer.fdmtActiveSourceactive_source_local_file_path.AsString;
  { TODO : Let's create an open file dialog here and ask users for file path }
  if not TFile.Exists(LFilePath) then
    Exit;

  var LTextEditor := OpenEditor(LFilePath, false);

  if DebuggerDataSetContainer.fdmtActiveSourceactive_source_line.AsInteger > 0 then
    LTextEditor.ActiveLine := DebuggerDataSetContainer.fdmtActiveSourceactive_source_line.AsInteger - 1;

  LTextEditor.ShowActiveLine := DebuggerDataSetContainer.fdmtActiveSourceactive_source_line_indicator.AsBoolean;
end;

function TEditorControlFrame.FindEditor(const AFileName: string): ITextEditor;
begin
  var LTab := GetEditorTab(AFileName);
  if Assigned(LTab) then
    Result := LTab as ITextEditor
  else
    Result := nil;
end;

function TEditorControlFrame.GetActiveTextEditor: ITextEditor;
begin
  Result := TEditorTabItem(tbScripts.ActiveTab) as ITextEditor;
end;

function TEditorControlFrame.GetEditorTab(const AFileName: string): TTabItem;
begin
  for var I := 0 to tbScripts.TabCount - 1 do
    if (tbScripts.Tabs[I] as ITextEditor).FileName = AFileName then
      Exit(tbScripts.Tabs[I] as TEditorTabItem);
  Result := nil;
end;

function TEditorControlFrame.OpenEditor(const AFileName: string;
  const AEditing: boolean): ITextEditor;
begin
  if not TFile.Exists(AFileName) then
    raise Exception.CreateFmt('File %s not found.', [AFileName]);

  var LItem := GetEditorTab(AFileName);
  if Assigned(LItem) then begin
    tbScripts.ActiveTab := LItem;
    Result := LItem as ITextEditor;
    Exit;
  end;

  LItem := DoCreateTab();
  (LItem as ITextEditor).Open(AFileName, AEditing);
  LItem.Height := 26;
  tbScripts.ActiveTab := LItem;
  Result := (LItem as ITextEditor);
end;

procedure TEditorControlFrame.tbScriptsChange(Sender: TObject);
begin
  if Assigned(tbScripts.ActiveTab) then
    FEditorServices.ActiveTextEditor := GetActiveTextEditor()
  else
    FEditorServices.ActiveTextEditor := nil;
end;

procedure TEditorControlFrame.CloseEditor(const AFileName: string);
begin
  var LItem := GetEditorTab(AFileName);
  if Assigned(LItem) then
    (LItem as ITextEditor).Close();
end;

procedure TEditorControlFrame.CloseAllEditors;
begin
  for var I := tbScripts.TabCount -1 downto 0 do
    (tbScripts.Tabs[I] as ITextEditor).Close();
end;

end.
