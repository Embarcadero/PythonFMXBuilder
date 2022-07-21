unit Frame.DebugButtons;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TDebugButtonsFrame = class(TFrame)
    sbDebugCurrentProject: TSpeedButton;
    spStepInto: TSpeedButton;
    sbStepOver: TSpeedButton;
    sbStepOut: TSpeedButton;
    btnStop: TSpeedButton;
    btnPause: TSpeedButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  Container.Menu.Actions,
  Container.Images;

{$R *.fmx}

end.
