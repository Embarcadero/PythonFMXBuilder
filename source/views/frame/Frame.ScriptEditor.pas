unit Frame.ScriptEditor;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.TabControl, FMX.Objects, System.Rtti;

type
  TScriptEditorFrame = class(TFrame)
    tbScripts: TTabControl;
  private
    function DoCreateTab(const AText: string;
      const ACanCloes: boolean = true): TTabItem;
    procedure DoCloseTab(Sender: TObject);  
    function GetEditorTab(const AFilePath: string): TTabItem;
    procedure DoCloseEditor(const AItem: TTabItem);
    procedure LoadEditorFile(const AItem: TTabItem; const AFilePath: string = '');
  public
    procedure OpenEditor(const AFilePath: string);
    procedure CloseEditor(const AFilePath: string);
    procedure CloseAll();
  end;

implementation

uses
  System.IOUtils;

type
  TScriptEditorTabItem = class(FMX.TabControl.TTabItem)
  private
    FFilePath: string;
    FEditor: TMemo;
    function GetCloseControl(): TControl;
    function GetOnCloseClick: TNotifyEvent;
    procedure SetOnCloseClick(const Value: TNotifyEvent);
  protected
    procedure SetText(const Value: string); override;
    function GetData: TValue; override;
    procedure SetData(const Value: TValue); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    
    property Editor: TMemo read FEditor;
    property OnCloseClick: TNotifyEvent read GetOnCloseClick write SetOnCloseClick;
  end;

{$R *.fmx}

{ TScriptEditorTabItem }

constructor TScriptEditorTabItem.Create(AOwner: TComponent);
begin
  inherited;
  FEditor := TMemo.Create(Self);
  FEditor.Parent := Self;
  FEditor.Align := TAlignLayout.Client;  
  FEditor.StyledSettings := [TStyledSetting.Size,
    TStyledSetting.Style, TStyledSetting.FontColor];
  FEditor.TextSettings.Font.Family := 'Cascadia Code';
  FEditor.TextSettings.Font.Size := 14;
end;

destructor TScriptEditorTabItem.Destroy;
begin
  FEditor.Lines.SaveToFile(Self.Data.AsString());
  inherited;
end;

function TScriptEditorTabItem.GetCloseControl: TControl;
begin
  Result := (ResourceLink.FindStyleResource('close') as TControl);
end;

function TScriptEditorTabItem.GetData: TValue;
begin
  Result := FFilePath;
end;

procedure TScriptEditorTabItem.SetData(const Value: TValue);
begin
  FFilePath := Value.AsString();
end;

function TScriptEditorTabItem.GetOnCloseClick: TNotifyEvent;
begin
  var LControl := GetCloseControl();
  if Assigned(LControl) then  
    Result := LControl.OnClick
  else
    Result := nil;
end;

procedure TScriptEditorTabItem.SetOnCloseClick(const Value: TNotifyEvent);
begin
  var LControl := GetCloseControl();
  if Assigned(LControl) then  
    LControl.OnClick := Value;
end;

procedure TScriptEditorTabItem.SetText(const Value: string);
const
  CLOSE_BTN_WIDTH = 30;
var
  LSize: TSizeF;
begin
  inherited;
  CalcTextObjectSize(0, LSize);
  Self.Width := LSize.cx + CLOSE_BTN_WIDTH;
end;

{ TScriptEditorFrame }

procedure TScriptEditorFrame.LoadEditorFile(const AItem: TTabItem;
  const AFilePath: string);
begin
  var LFilePath := AFilePath;
  if LFilePath.IsEmpty() then
    LFilePath := AItem.Data.AsString();  

  TScriptEditorTabItem(AItem).Editor.Lines.LoadFromFile(LFilePath);
end;

procedure TScriptEditorFrame.DoCloseEditor(const AItem: TTabItem);
begin
  if not Assigned(AItem) then
    Exit;    

  tbScripts.GetTabList().Remove(AItem);
  AItem.Destroy();
  tbScripts.ActiveTab := tbScripts.Tabs[tbScripts.TabCount - 1];  
end;

procedure TScriptEditorFrame.DoCloseTab(Sender: TObject);
begin
  if not (Sender is TFMXObject) then
    Exit;
  var LParent := TFMXObject(Sender).Parent;
  while Assigned(LParent) do begin
    if LParent is TTabItem then begin
      DoCloseEditor(LParent as TTabItem);
      Break;
    end;
    LParent := LParent.Parent;
  end;     
end;

function TScriptEditorFrame.DoCreateTab(const AText: string;
  const ACanCloes: boolean): TTabItem;
begin
  Result := tbScripts.Add(TScriptEditorTabItem);
  Result.AutoSize := false;
  Result.StyleLookup := 'tabitemclosebutton';
  Result.Text := AText;  
  Result.Height := 26;
  if ACanCloes then
    TScriptEditorTabItem(Result).OnCloseClick := DoCloseTab;    
end;

function TScriptEditorFrame.GetEditorTab(const AFilePath: string): TTabItem;
begin
  for var I := 0 to tbScripts.TabCount - 1 do
    if (tbScripts.Tabs[I].Data.AsString() = AFilePath) then
      Exit(tbScripts.Tabs[I]);
  Result := nil;
end;

procedure TScriptEditorFrame.OpenEditor(const AFilePath: string);
begin
  if not TFile.Exists(AFilePath) then
    raise Exception.Create('File not found.');

  var LItem := GetEditorTab(AFilePath);
  if Assigned(LItem) then begin
    tbScripts.ActiveTab := LItem;
    Exit;
  end;
  
  LItem := DoCreateTab(TPath.GetFileName(AFilePath));
  LItem.Data := AFilePath;
  LoadEditorFile(LItem);

  tbScripts.ActiveTab := LItem;
end;

procedure TScriptEditorFrame.CloseAll;
begin
  for var I := tbScripts.TabCount -1 downto 0 do
    tbScripts.Tabs[I].Destroy(); 
end;

procedure TScriptEditorFrame.CloseEditor(const AFilePath: string);
begin  
  DoCloseEditor(GetEditorTab(AFilePath));                 
end;

end.
