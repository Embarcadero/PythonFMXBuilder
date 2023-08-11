unit Builder.SpeedButton;

interface

uses
  FMX.Controls, FMX.StdCtrls, System.Types;

type
  TSpeedButton = class(FMX.StdCtrls.TSpeedButton)
  private
    FDropDown: TControl;
    procedure OnDropdownClick(Sender: TObject);
  protected
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
  end;

implementation

{ TSpeedButton }

procedure TSpeedButton.ApplyStyle;
begin
  inherited;
  if FindStyleResource<TControl>('dropdown', FDropDown) then begin
    FDropDown.OnClick := OnDropdownClick;
  end;
end;

procedure TSpeedButton.FreeStyle;
begin
  inherited;
  if Assigned(FDropDown) then
    FDropDown.OnClick := nil;
  FDropDown := nil;
end;

procedure TSpeedButton.OnDropdownClick(Sender: TObject);
begin
  var LP := LocalToAbsolute(TPointF.Create(
    Self.Width * 0.18, Self.Height));
  LP := Scene.LocalToScreen(LP);
  Self.ShowContextMenu(LP);
end;

end.
