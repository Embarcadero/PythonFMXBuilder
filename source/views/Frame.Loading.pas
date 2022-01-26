unit Frame.Loading;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects;

type
  TLoadingFrame = class(TFrame)
    recLoading: TRectangle;
    aiLoading: TAniIndicator;
  public
    procedure StartAni();
    procedure StopAni();
  end;

implementation

{$R *.fmx}

procedure TLoadingFrame.StartAni;
begin
  aiLoading.Enabled := true;
  Visible := true;
end;

procedure TLoadingFrame.StopAni;
begin
  Visible := false;
  aiLoading.Enabled := false;
end;

end.
