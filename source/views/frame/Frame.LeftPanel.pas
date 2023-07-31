unit Frame.LeftPanel;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TabControl, Frame.Debug.LeftPanel, Builder.Messagery;

type
  TLeftPanelFrame = class(TFrame)
    tcLeftPanelControls: TTabControl;
    tiLeftPanelDebugSession: TTabItem;
    tiLeftPanelDesignSession: TTabItem;
    frmLeftPanelDebug: TLeftPanelDebugFrame;
  private
    FDebugSessionStarted: IDisconnectable;
    FDebugSessionStopped: IDisconnectable;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  end;

implementation

{$R *.fmx}

{ TLeftPanelFrame }

constructor TLeftPanelFrame.Create(AOwner: TComponent);
begin
  inherited;
  tcLeftPanelControls.TabPosition := TTabPosition.None;
  FDebugSessionStarted := TMessagery.SubscribeToEvent<TDebugSessionStartedEvent>(
    procedure(const AEventNotification: TDebugSessionStartedEvent)
    begin
      TThread.Queue(TThread.Current,
        procedure()
        begin
          tcLeftPanelControls.ActiveTab := tiLeftPanelDebugSession;
        end);
    end);

  FDebugSessionStopped := TMessagery.SubscribeToEvent<TDebugSessionStoppedEvent>(
    procedure(const AEventNotification: TDebugSessionStoppedEvent)
    begin
      TThread.Queue(TThread.Current,
        procedure()
        begin
          tcLeftPanelControls.ActiveTab := tiLeftPanelDesignSession;
        end);
    end);
end;

destructor TLeftPanelFrame.Destroy;
begin
  FDebugSessionStopped.Disconnect();
  FDebugSessionStarted.Disconnect();
  inherited;
end;

end.
