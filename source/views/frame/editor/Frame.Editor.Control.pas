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
    function DoCreateTab(const AText: string;
      const ACanClose: boolean = true): TTabItem;
    function GetEditorTab(const AFilePath: string): TTabItem;
    procedure LoadEditorFile(const AItem: TTabItem; const AFilePath: string = '');
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    function OpenEditor(const AFilePath: string): ITextEditor;
    procedure CloseEditor(const AFilePath: string);
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

          if LActiveLine > 0 then begin
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

procedure TEditorControlFrame.LoadEditorFile(const AItem: TTabItem;
  const AFilePath: string);
begin
  var LFilePath := AFilePath;
  if LFilePath.IsEmpty() then
    LFilePath := AItem.Data.AsString();  

  TCustomEditorTabItem(AItem).TextEditor.LoadFromFile(LFilePath);
end;

function TEditorControlFrame.DoCreateTab(const AText: string;
  const ACanClose: boolean): TTabItem;
begin
  Result := tbScripts.Add(TCustomEditorTabItem.DefaultTabItemClass);
  Result.AutoSize := false;
  Result.StyleLookup := 'tabitemclosebutton';
  Result.Text := AText;  
  Result.Height := 26;
  TCustomEditorTabItem(Result).CanClose := ACanClose;
end;

procedure TEditorControlFrame.dsActiveSourceDataChange(Sender: TObject;
  Field: TField);
begin
  var LActiveTab := TCustomEditorTabItem(tbScripts.ActiveTab);
  if Assigned(LActiveTab) then
    LActiveTab.TextEditor.ShowActiveLine := false;

  var LDatS := (Sender as TDataSource).DataSet;

  if LDatS.IsEmpty() then
    Exit();

  var LFilePath := LDatS.FieldByName('active_source_local_file_path').AsString;
  if not TFile.Exists(LFilePath) then
    { TODO : Let's create an open file dialog here and ask users for file path }
    Exit;

  var LTextEditor := OpenEditor(LFilePath);

  if LDatS.FieldByName('active_source_line').AsInteger > 0 then begin
    LTextEditor.ActiveLine := LDatS.FieldByName('active_source_line').AsInteger - 1;
  end;
  LTextEditor.ShowActiveLine := LDatS.FieldByName('active_source_line_indicator').AsBoolean;
end;

function TEditorControlFrame.GetEditorTab(const AFilePath: string): TTabItem;
begin
  for var I := 0 to tbScripts.TabCount - 1 do
    if (tbScripts.Tabs[I].Data.AsString() = AFilePath) then
      Exit(tbScripts.Tabs[I]);
  Result := nil;
end;

function TEditorControlFrame.OpenEditor(const AFilePath: string): ITextEditor;
begin
  if not TFile.Exists(AFilePath) then
    raise Exception.CreateFmt('File %s not found.', [AFilePath]);

  var LItem := GetEditorTab(AFilePath);
  if Assigned(LItem) then begin
    tbScripts.ActiveTab := LItem;
    Result := TCustomEditorTabItem(LItem).TextEditor;
    Exit;
  end;
  
  LItem := DoCreateTab(TPath.GetFileName(AFilePath));
  LItem.Data := AFilePath;
  LoadEditorFile(LItem);

  tbScripts.ActiveTab := LItem;

  Result := TCustomEditorTabItem(LItem).TextEditor;
end;

procedure TEditorControlFrame.CloseEditor(const AFilePath: string);
begin
  var LItem := GetEditorTab(AFilePath);
  if Assigned(LItem) then
    tbScripts.Delete(LItem.Index);
end;

procedure TEditorControlFrame.CloseAll;
begin
  for var I := tbScripts.TabCount -1 downto 0 do
    tbScripts.Tabs[I].Destroy(); 
end;

end.
