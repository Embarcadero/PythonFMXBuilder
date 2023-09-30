unit Builder.TabControl;

interface

uses
  System.Classes, System.Types, System.Rtti,
  FMX.Types, FMX.Controls, FMX.TabControl;

type
  TTabItem = class;

  TTabControl = class(FMX.TabControl.TTabControl)
  protected
    procedure DoAddObject(const AObject: TFmxObject); override;
  public
    function Delete(const Index: Integer): Boolean;
  end;

  TOnTabItemClose = TNotifyEvent;
  TTabItem = class(FMX.TabControl.TTabItem)
  private
    FData: TValue;
    FClose: TControl;
    FModify: TControl;
    FCanClose: boolean;
    FModified: boolean;
    FInteracting: boolean;
    FOnClose: TOnTabItemClose;

    procedure SetCanClose(const Value: boolean);
    procedure SetModified(const Value: boolean);
    procedure UpdateModifyButtonVisibility();
    procedure UpdateCloseButtonVisibility();
    procedure UpdateControls();
    procedure OnMouseEnterTab(Sender: TObject);
    procedure OnMouseLeaveTab(Sender: TObject);
    procedure OnCloseClick(Sender: TObject);
  protected
    procedure SetText(const Value: string); override;
    function GetData: TValue; override;
    procedure SetData(const Value: TValue); override;
    function GetDefaultStyleLookupName: string; override;
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    procedure DoClose(); virtual;
  public
    constructor Create(AOwner: TComponent); override;

    procedure Close();
    
    property CanClose: boolean read FCanClose write SetCanClose;
    property Modified: boolean read FModified write SetModified;
    property OnClose: TOnTabItemClose read FOnClose write FOnClose;
  end;

implementation

{ TTabControl }

function TTabControl.Delete(const Index: Integer): Boolean;
begin
  if not Assigned(ActiveTab) then
    Exit(inherited Delete(Index));
  //There's a bug in the TabEditor preventing the OnChange event
  //on deleting the first tab
  var LOldIndex := ActiveTab.Index;
  Result := inherited Delete(Index);
  if Assigned(ActiveTab) and  (LOldIndex = ActiveTab.Index) then
    DoChange();
end;

procedure TTabControl.DoAddObject(const AObject: TFmxObject);
begin
  //There's a bug in the TabEditor preventing the OnChange event
  //on adding the first tab
  var LOldActiveTab := ActiveTab;
  inherited;
  if (ActiveTab <> LOldActiveTab) then
    DoChange();
end;

{ TTabItem }

procedure TTabItem.Close;
begin
  DoClose();
end;

constructor TTabItem.Create(AOwner: TComponent);
begin
  inherited;
  FModified := false;
  OnMouseEnter := OnMouseEnterTab;
  OnMouseLeave := OnMouseLeaveTab;
end;

function TTabItem.GetData: TValue;
begin
  Result := FData;
end;

procedure TTabItem.SetData(const Value: TValue);
begin
  FData := Value;
end;

procedure TTabItem.SetModified(const Value: boolean);
begin
  FModified := Value;
  UpdateControls();
end;

procedure TTabItem.SetText(const Value: string);
const
  CLOSE_BTN_WIDTH = 30;
var
  LSize: TSizeF;
begin
  inherited;
  CalcTextObjectSize(0, LSize);
  Self.Width := LSize.cx + CLOSE_BTN_WIDTH;
end;

procedure TTabItem.UpdateCloseButtonVisibility;
begin
  if not Assigned(FClose) then
    Exit;

  FClose.Visible := FCanClose and (not FModified or FInteracting);
end;

procedure TTabItem.UpdateControls;
begin
  UpdateCloseButtonVisibility();
  UpdateModifyButtonVisibility();
end;

procedure TTabItem.UpdateModifyButtonVisibility;
begin
  if not Assigned(FModify) then
    Exit;

  FModify.Visible := FModified and not FInteracting;
end;

procedure TTabItem.SetCanClose(const Value: boolean);
begin
  FCanClose := Value;
  UpdateControls();
end;

function TTabItem.GetDefaultStyleLookupName: string;
begin
  Result := 'tabitemeditor';
end;

procedure TTabItem.ApplyStyle;
begin
  inherited;
  var LItemStyle: TFMXObject := nil;
  if Assigned(ResourceLink) then
    case TabControl.EffectiveTabPosition of
      TTabPosition.Top:
        LItemStyle := ResourceLink.FindStyleResource('top');
      TTabPosition.Bottom:
        LItemStyle := ResourceLink.FindStyleResource('bottom');
    end;

  if Assigned(LItemStyle) then begin
    FModify := LItemStyle.FindStyleResource('modify') as TControl;
    if Assigned(FModify) then begin
      //
    end;

    FClose := LItemStyle.FindStyleResource('close') as TControl;
    if Assigned(FClose) then begin
      FClose.OnClick := OnCloseClick;
      FClose.OnMouseEnter := OnMouseEnterTab;
      FClose.OnMouseLeave := OnMouseLeaveTab;
    end;    

    UpdateControls();
  end;
end;

procedure TTabItem.FreeStyle;
begin
  inherited;
  if Assigned(FModify) then begin
    //
    FModify := nil;
  end;

  if Assigned(FClose) then begin
    FClose.OnClick := nil;
    FClose.OnMouseEnter := nil;
    FClose.OnMouseLeave := nil;
    FClose := nil;
  end;
end;

procedure TTabItem.DoClose;
begin
  if Assigned(FOnClose) then
    FOnClose(Self);

  if Assigned(TabControl) then
    TTabControl(TabControl).Delete(Self.Index);
end;

procedure TTabItem.OnCloseClick(Sender: TObject);
begin
  DoClose();
end;

procedure TTabItem.OnMouseEnterTab(Sender: TObject);
begin
  FInteracting := true;
  UpdateControls();
end;

procedure TTabItem.OnMouseLeaveTab(Sender: TObject);
begin
  FInteracting := false;
  UpdateControls();
end;

end.
