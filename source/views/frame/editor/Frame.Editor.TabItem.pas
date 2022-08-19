unit Frame.Editor.TabItem;

interface

uses
  System.Rtti, FMX.TabControl, FMX.Controls, System.Classes, System.Types,
  Builder.Chain;

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
    procedure Save();

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
    FSaveState: IDisconnectable;
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
    destructor Destroy(); override;

    property TextEditor: ITextEditor read GetTextEditor;
    property CanClose: boolean read FCanClose write SetCanClose;
    class property DefaultTabItemClass: TTabItemClass read FDefaultTabItemClass write FDefaultTabItemClass;
  end;

implementation

{ TCustomEditorTabItem }

constructor TCustomEditorTabItem.Create(AOwner: TComponent);
begin
  inherited;
  FSaveState := TGlobalBuilderChain.SubscribeToEvent<TSaveStateEvent>(
    procedure(const AEventNotification: TSaveStateEvent)
    begin
      if (AEventNotification.Body.SaveState = TSaveState.Save) then begin
        if Self.TabControl.ActiveTab = Self then
          TextEditor.Save();
      end else if (AEventNotification.Body.SaveState = TSaveState.SaveAll) then
        TextEditor.Save();
    end);
end;

destructor TCustomEditorTabItem.Destroy;
begin
  FSaveState.Disconnect();
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
  TGlobalBuilderChain.BroadcastEventAsync(
    TCloseFileEvent.Create(FFilePath));
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
