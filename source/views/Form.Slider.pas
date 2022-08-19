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

{ TFormSlider }

class procedure TFormSlider.ShowModal(const AParent, AForm: TCommonCustomForm);
const
  PARENT_PERCENT = 0.4;
  SLIDE_INTERVAL = 0.03;
begin
  AForm.BorderIcons := [];
  AForm.SetBounds(0, AParent.Bounds.Top, 0, AParent.Bounds.Height);
  AForm.Show();
  var LSize := Trunc(AParent.Bounds.Width * PARENT_PERCENT);
  var LCurSize := 0;
  repeat
    AForm.SetBounds(AParent.Bounds.Right - LCurSize, AParent.Bounds.Top, LCurSize, AParent.Bounds.Height);
    Inc(LCurSize, Trunc(LSize * SLIDE_INTERVAL));
  until LCurSize >= LSize;
  AForm.SetBounds(AParent.Bounds.Right - LSize, AParent.Bounds.Top, LSize, AParent.Bounds.Height);
  AForm.ShowModal();
end;

end.
