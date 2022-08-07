unit Frame.Editor.TabItem;

interface

uses
  System.Rtti,
  FMX.TabControl, FMX.Controls, System.Classes, System.Types;

type
  ITextEditor = interface
    ['{52EDE988-F0AF-44B2-8445-1BD8960DD3C2}']
    function GetBreakpoints(): TArray<integer>;
    procedure SetBreakpoints(ABreakpoints: TArray<integer>);
    function GetActiveLine(): integer;
    procedure SetActiveLine(AActiveLine: integer);
    function GetShowActiveLine(): boolean;
    procedure SetShowActiveLine(AShowActiveLine: boolean);

    procedure LoadFromFile(const AFileName: string);
    property Breakpoints: TArray<integer> read GetBreakpoints write SetBreakpoints;
    property ActiveLine: integer read GetActiveLine write SetActiveLine;
    property ShowActiveLine: boolean read GetShowActiveLine write SetShowActiveLine;
  end;

  TCustomEditorTabItem = class(FMX.TabControl.TTabItem)
  private
    class var FDefaultTabItemClass: TTabItemClass;
  private
    FFilePath: string;
    FCanClose: boolean;
    function GetCloseControl(): TControl;
    procedure OnCloseTab(Sender: TObject);
    procedure SetCanClose(const Value: boolean);
  protected
    procedure ApplyStyle; override;
    procedure SetText(const Value: string); override;
    function GetData: TValue; override;
    procedure SetData(const Value: TValue); override;

    function GetTextEditor(): ITextEditor; virtual; abstract;
    procedure DoClose(); virtual;
  public
    constructor Create(AOwner: TComponent); override;

    property TextEditor: ITextEditor read GetTextEditor;
    property CanClose: boolean read FCanClose write SetCanClose;
    class property DefaultTabItemClass: TTabItemClass read FDefaultTabItemClass write FDefaultTabItemClass;
  end;

implementation

{ TCustomEditorTabItem }

constructor TCustomEditorTabItem.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TCustomEditorTabItem.ApplyStyle;
begin
  inherited;
  if Assigned(ResourceLink) then begin
    var LControl := GetCloseControl();
    if Assigned(LControl) then
      LControl.OnClick := OnCloseTab;
  end;
end;

procedure TCustomEditorTabItem.DoClose;
begin
  TabControl.GetTabList().Remove(Self);
  TabControl.ActiveTab := TabControl.Tabs[TabControl.TabCount - 1];  
  Destroy();  
end;

function TCustomEditorTabItem.GetCloseControl: TControl;
begin
  Result := (ResourceLink.FindStyleResource('close') as TControl);
end;

function TCustomEditorTabItem.GetData: TValue;
begin
  Result := FFilePath;
end;

procedure TCustomEditorTabItem.SetCanClose(const Value: boolean);
begin
  FCanClose := Value;
  GetCloseControl().Visible := Value;
end;

procedure TCustomEditorTabItem.SetData(const Value: TValue);
begin
  FFilePath := Value.AsString();
end;

procedure TCustomEditorTabItem.SetText(const Value: string);
const
  CLOSE_BTN_WIDTH = 30;
var
  LSize: TSizeF;
begin
  inherited;
  CalcTextObjectSize(0, LSize);
  Self.Width := LSize.cx + CLOSE_BTN_WIDTH;
end;

procedure TCustomEditorTabItem.OnCloseTab(Sender: TObject);
begin
  DoClose();
end;

end.
