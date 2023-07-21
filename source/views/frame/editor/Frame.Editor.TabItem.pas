unit Frame.Editor.TabItem;

interface

uses
  System.Rtti, System.Classes, System.Types, FMX.Types, FMX.Controls, FMX.TabControl,
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
    procedure SaveTo(const AFileName: string);

    property Breakpoints: TArray<integer> read GetBreakpoints write SetBreakpoints;
    property ActiveLine: integer read GetActiveLine write SetActiveLine;
    property ShowActiveLine: boolean read GetShowActiveLine write SetShowActiveLine;
  end;

  TControlClass = class of TControl;

  TEditorTabItem = class(TTabItem)
  private
    class var FDefaultEditorClass: TControlClass;
  private
    FFileName: string;
    FCanClose: boolean;
    FEditor: TControl;
    //Events
    FSaveState: IDisconnectable;
    FRenameFile: IDisconnectable;
    function GetCloseControl(): TControl;
    procedure OnCloseTab(Sender: TObject);
    procedure CreateEditor();
  protected
    procedure ApplyStyle; override;
    procedure SetText(const Value: string); override;
    function GetData: TValue; override;
    procedure SetData(const Value: TValue); override;

    function GetTextEditor(): ITextEditor;
    procedure DoClose(); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    procedure LoadFile(const AFileName: string);

    property FileName: string read FFileName write FFileName;
    property TextEditor: ITextEditor read GetTextEditor;
    property CanClose: boolean read FCanClose write FCanClose;

    class property DefaultEditorClass: TControlClass
      read FDefaultEditorClass write FDefaultEditorClass;
  end;

implementation

uses
  System.SysUtils, System.IOUtils;

{ TEditorTabItem }

constructor TEditorTabItem.Create(AOwner: TComponent);
begin
  inherited;
  CreateEditor();
  FSaveState := TGlobalBuilderChain.SubscribeToEvent<TSaveStateEvent>(
    procedure(const AEventNotification: TSaveStateEvent)
    begin
      if (AEventNotification.Body.SaveState = TSaveState.Save) then begin
        if (TabControl.ActiveTab = Self) then
          TextEditor.Save();
      end else if (AEventNotification.Body.SaveState = TSaveState.SaveAll) then
        TextEditor.Save();
    end);
  FRenameFile := TGlobalBuilderChain.SubscribeToEvent<TRenameFileEvent>(
    procedure(const AEventNotification: TRenameFileEvent)
    begin
      if (FFileName = AEventNotification.Body.OldFilePath) then begin
        TextEditor.SaveTo(AEventNotification.Body.NewFilePath);
        TThread.Synchronize(TThread.Current, procedure() begin
          LoadFile(AEventNotification.Body.NewFilePath);
        end);
      end;
    end);
end;

procedure TEditorTabItem.CreateEditor;
begin
  FEditor := DefaultEditorClass.Create(Self);
  FEditor.Parent := Self;
  FEditor.Align := TAlignLayout.Client;
end;

destructor TEditorTabItem.Destroy;
begin
  FRenameFile.Disconnect();
  FSaveState.Disconnect();
  inherited;
end;

procedure TEditorTabItem.ApplyStyle;
begin
  inherited;
  var LControl := GetCloseControl();
  if Assigned(LControl) then begin
    LControl.OnClick := OnCloseTab;
    LControl.Visible := FCanClose;
  end;
end;

function TEditorTabItem.GetData: TValue;
begin
  Result := FFileName;
end;

function TEditorTabItem.GetTextEditor: ITextEditor;
begin
  Result := FEditor as ITextEditor;
end;

procedure TEditorTabItem.LoadFile(const AFileName: string);
begin
  FFileName := AFileName;
  Text := TPath.GetFileName(FFileName);
  TextEditor.LoadFromFile(FFileName);
end;

procedure TEditorTabItem.SetData(const Value: TValue);
begin
  FFileName := Value.AsString();
end;

procedure TEditorTabItem.SetText(const Value: string);
const
  CLOSE_BTN_WIDTH = 30;
var
  LSize: TSizeF;
begin
  inherited;
  CalcTextObjectSize(0, LSize);
  Self.Width := LSize.cx + CLOSE_BTN_WIDTH;
end;

procedure TEditorTabItem.DoClose;
begin
  TGlobalBuilderChain.BroadcastEventAsync(
    TCloseFileEvent.Create(FFileName));
end;

function TEditorTabItem.GetCloseControl: TControl;
begin
  var LItemStyle: TFMXObject := nil;
  if Assigned(ResourceLink) then
    case TabControl.EffectiveTabPosition of
      TTabPosition.Top:
        LItemStyle := ResourceLink.FindStyleResource('top');
      TTabPosition.Bottom:
        LItemStyle := ResourceLink.FindStyleResource('bottom');
    end;

  if Assigned(LItemStyle) then
    Result := (LItemStyle.FindStyleResource('close') as TControl)
  else
    Result := nil;
end;

procedure TEditorTabItem.OnCloseTab(Sender: TObject);
begin
  DoClose();
end;

end.
