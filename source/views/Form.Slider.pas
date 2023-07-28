unit Form.Slider;

interface

uses
  FMX.Forms, System.SysUtils;

type
  TFormSlider = class
  public
    class procedure ShowModal(const AParent, AForm: TCommonCustomForm);
  end;

implementation

uses
  FMX.Ani;

{ TFormSlider }

class procedure TFormSlider.ShowModal(const AParent, AForm: TCommonCustomForm);
begin
  var LSize := Trunc(AParent.Bounds.Width * 0.4);
  AForm.BorderIcons := [];
  AForm.SetBounds(AParent.Bounds.Right - LSize, AParent.Bounds.Top, LSize, AParent.Bounds.Height);
  AForm.ShowModal();
end;

end.
