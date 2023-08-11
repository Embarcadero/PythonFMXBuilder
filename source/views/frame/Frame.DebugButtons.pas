unit Frame.DebugButtons;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, Builder.Messagery;

type
  TDebugButtonsFrame = class(TFrame)
    sbDebugCurrentProject: TSpeedButton;
    spStepInto: TSpeedButton;
    sbStepOver: TSpeedButton;
    sbStepOut: TSpeedButton;
    btnStop: TSpeedButton;
    btnPause: TSpeedButton;
    sbContinue: TSpeedButton;
  private
    FUpdateSettings: IDisconnectable;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  end;

implementation

uses
  Container.Menu.Actions,
  Container.Images;

{$R *.fmx}

{ TDebugButtonsFrame }

constructor TDebugButtonsFrame.Create(AOwner: TComponent);
begin
  inherited;
  FUpdateSettings := TMessagery.SubscribeToEvent<TUpdateSettingsEvent>(
    procedure(const AEventNotification: TUpdateSettingsEvent)
    begin
      var LSmartDeploy := AEventNotification.Body.SmartDeploy;
      TThread.Queue(TThread.Current,
        procedure()
        begin
          if LSmartDeploy then begin
            sbDebugCurrentProject.Action := MenuActionsContainer.actSmartDebugCurrentProjectAsync;
          end else begin
            sbDebugCurrentProject.Action := MenuActionsContainer.actDebugCurrentProjectAsync;
          end;
        end);
    end);
end;

destructor TDebugButtonsFrame.Destroy;
begin
  FUpdateSettings.Disconnect();
  inherited;
end;

end.
