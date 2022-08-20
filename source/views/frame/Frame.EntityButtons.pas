unit Frame.EntityButtons;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TEntityButtonsFrame = class(TFrame)
    sbUpdateEnvironment: TSpeedButton;
    sbUpdateProject: TSpeedButton;
  end;

implementation

{$R *.fmx}

end.
