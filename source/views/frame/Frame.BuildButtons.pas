unit Frame.BuildButtons;
interface
uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, Builder.Messagery, Builder.SpeedButton, FMX.Menus;

type
  TBuildButtonsFrame = class(TFrame)
    sbBuildCurrentProject: TSpeedButton;
    spDeployCurrentProject: TSpeedButton;
    sbRunCurrentProject: TSpeedButton;
    pmDeployDropDown: TPopupMenu;
    miSmartDeploy: TMenuItem;
    procedure miSmartDeployClick(Sender: TObject);
  private
    FUpdateSettings: IDisconnectable;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  end;

implementation

uses
  Container.Menu.Actions;

{$R *.fmx}

{ TBuildButtonsFrame }

constructor TBuildButtonsFrame.Create(AOwner: TComponent);
begin
  inherited;
  FUpdateSettings := TMessagery.SubscribeToEvent<TUpdateSettingsEvent>(
    procedure(const AEventNotification: TUpdateSettingsEvent)
    begin
      var LSmartDeploy := AEventNotification.Body.SmartDeploy;
      TThread.Queue(TThread.Current,
        procedure()
        begin
          miSmartDeploy.IsChecked := LSmartDeploy;
          if LSmartDeploy then begin
            spDeployCurrentProject.Action := MenuActionsContainer.actSmartDeployCurrentProjectAsync;
            sbRunCurrentProject.Action := MenuActionsContainer.actSmartRunCurrentProjectAsync;
          end else begin
            spDeployCurrentProject.Action := MenuActionsContainer.actDeployCurrentProjectAsync;
            sbRunCurrentProject.Action := MenuActionsContainer.actRunCurrentProjectAsync;
          end;
        end);
    end);
end;

destructor TBuildButtonsFrame.Destroy;
begin
  FUpdateSettings.Disconnect();
  inherited;
end;

procedure TBuildButtonsFrame.miSmartDeployClick(Sender: TObject);
begin
  TMessagery.BroadcastEventAsync(
    TUpdateSettingsEvent.Create(TMenuItem(Sender).IsChecked));
end;

end.
