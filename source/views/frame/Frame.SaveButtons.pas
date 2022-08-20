unit Frame.SaveButtons;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, Container.Images, Container.Menu.Actions;

type
  TSaveButtonsFrame = class(TFrame)
    sbSaveAll: TSpeedButton;
    sbSave: TSpeedButton;
  end;

implementation

{$R *.fmx}

end.
