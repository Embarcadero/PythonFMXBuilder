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
    procedure Start();
    procedure Stop();
  end;

implementation

{$R *.fmx}

procedure TLoadingFrame.Start;
begin
  aiLoading.Enabled := true;
  Visible := true;
end;

procedure TLoadingFrame.Stop;
begin
  Visible := false;
  aiLoading.Enabled := false;
end;

end.
