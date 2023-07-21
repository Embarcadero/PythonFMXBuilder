unit Frame.Editor.Control;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.TabControl, FMX.Objects, System.Rtti, Builder.Chain,
  Frame.Editor.TabItem, Data.DB;

type
  TEditorControlFrame = class(TFrame)
    tbScripts: TTabControl;
    dsActiveSource: TDataSource;
    procedure dsActiveSourceDataChange(Sender: TObject; Field: TField);
  private
    FOpenProjectEvent: IDisconnectable;
    FCloseProjectEvent: IDisconnectable;
    FOpenFileEvent: IDisconnectable;
    FCloseFileEvent: IDisconnectable;
    function DoCreateTab(const ACanClose: boolean = true): TTabItem;
    function GetEditorTab(const AFileName: string): TTabItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    function OpenEditor(const AFileName: string): ITextEditor;
    procedure CloseEditor(const AFileName: string);
    procedure CloseAll();
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
  FOpenProjectEvent := TGlobalBuilderChain.SubscribeToEvent<TOpenProjectEvent>(
    procedure(const AEventNotification: TOpenProjectEvent)
    begin
      //
    end);

  FCloseProjectEvent := TGlobalBuilderChain.SubscribeToEvent<TCloseProjectEvent>(
    procedure(const AEventNotification: TCloseProjectEvent)
    begin
      TThread.Queue(TThread.Current,
        procedure()
        begin
          CloseAll();
        end);
    end);

  FOpenFileEvent := TGlobalBuilderChain.SubscribeToEvent<TOpenFileEvent>(
    procedure(const AEventNotification: TOpenFileEvent)
    begin
      var LFilePath := AEventNotification.Body.FilePath;
      var LActiveLine := AEventNotification.Body.ActiveLine;
      var LShowActiveLine := AEventNotification.Body.ShowActiveLineIndicator;
      var LBreakpoints := AEventNotification.Body.Breakpoints;

      TThread.Queue(TThread.Current,
        procedure()
        begin
          var LTextEditor := OpenEditor(LFilePath);

          if (LActiveLine > 0) then begin
            LTextEditor.ActiveLine := LActiveLine - 1;
            LTextEditor.ShowActiveLine := LShowActiveLine;
          end;

          if Assigned(LBreakpoints) then
            LTextEditor.Breakpoints := LBreakpoints;
        end);
    end);

  FCloseFileEvent := TGlobalBuilderChain.SubscribeToEvent<TCloseFileEvent>(
    procedure(const AEventNotification: TCloseFileEvent)
    begin
      var LFilePath := AEventNotification.Body.FilePath;

      TThread.Queue(TThread.Current,
        procedure()
        begin
          CloseEditor(LFilePath);
        end);
    end);
end;

destructor TEditorControlFrame.Destroy;
begin
  FOpenFileEvent.Disconnect();
  FOpenProjectEvent.Disconnect();
  FCloseProjectEvent.Disconnect();
  inherited;
end;

function TEditorControlFrame.DoCreateTab(const ACanClose: boolean): TTabItem;
begin
  Result := tbScripts.Add(TEditorTabItem);
  Result.AutoSize := false;
  TEditorTabItem(Result).CanClose := ACanClose;
end;

procedure TEditorControlFrame.dsActiveSourceDataChange(Sender: TObject;
  Field: TField);
begin
  var LActiveTab := TEditorTabItem(tbScripts.ActiveTab);
  if Assigned(LActiveTab) then
    LActiveTab.TextEditor.ShowActiveLine := false;

  if DebuggerDataSetContainer.fdmtActiveSource.IsEmpty() then
    Exit();

  var LFilePath := DebuggerDataSetContainer.fdmtActiveSourceactive_source_local_file_path.AsString;
  { TODO : Let's create an open file dialog here and ask users for file path }
  if not TFile.Exists(LFilePath) then
    Exit;

  var LTextEditor := OpenEditor(LFilePath);

  if DebuggerDataSetContainer.fdmtActiveSourceactive_source_line.AsInteger > 0 then
    LTextEditor.ActiveLine := DebuggerDataSetContainer.fdmtActiveSourceactive_source_line.AsInteger - 1;

  LTextEditor.ShowActiveLine := DebuggerDataSetContainer.fdmtActiveSourceactive_source_line_indicator.AsBoolean;
end;

function TEditorControlFrame.GetEditorTab(const AFileName: string): TTabItem;
begin
  for var I := 0 to tbScripts.TabCount - 1 do
    if (TEditorTabItem(tbScripts.Tabs[I]).FileName = AFileName) then
      Exit(tbScripts.Tabs[I]);
  Result := nil;
end;

function TEditorControlFrame.OpenEditor(const AFileName: string): ITextEditor;
begin
  if not TFile.Exists(AFileName) then
    raise Exception.CreateFmt('File %s not found.', [AFileName]);

  var LItem := GetEditorTab(AFileName);
  if Assigned(LItem) then begin
    tbScripts.ActiveTab := LItem;
    Result := TEditorTabItem(LItem).TextEditor;
    Exit;
  end;

  LItem := DoCreateTab();
  TEditorTabItem(LItem).LoadFile(AFileName);
  LItem.Height := 26;
  LItem.StyleLookup := 'tabitemclosebutton';
  tbScripts.ActiveTab := LItem;
  Result := TEditorTabItem(LItem).TextEditor;
end;

procedure TEditorControlFrame.CloseEditor(const AFileName: string);
begin
  var LItem := GetEditorTab(AFileName);
  if Assigned(LItem) then
    tbScripts.Delete(LItem.Index);
end;

procedure TEditorControlFrame.CloseAll;
begin
  for var I := tbScripts.TabCount -1 downto 0 do
    tbScripts.Tabs[I].Destroy(); 
end;

end.
