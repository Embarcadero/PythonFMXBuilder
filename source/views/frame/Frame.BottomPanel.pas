unit Frame.BottomPanel;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TabControl, Frame.Debug.BottomPanel, Frame.Log, Builder.Messagery;

type
  TBottomPanelFrame = class(TFrame)
    tcBottomPanelControls: TTabControl;
    tiBottomPanelDesignSession: TTabItem;
    tiBottomPanelDebugSession: TTabItem;
    frmLog: TLogFrame;
    frmBottomPanelDebug: TBottomPanelDebugFrame;
  private
    FDebugSessionStarted: IDisconnectable;
    FDebugSessionStopped: IDisconnectable;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  end;

implementation

{$R *.fmx}

{ TBottomPanelFrame }

constructor TBottomPanelFrame.Create(AOwner: TComponent);
begin
  inherited;
  tcBottomPanelControls.TabPosition := TTabPosition.None;
  tcBottomPanelControls.ActiveTab := tiBottomPanelDesignSession;
  FDebugSessionStarted := TMessagery.SubscribeToEvent<TDebugSessionStartedEvent>(
    procedure(const AEventNotification: TDebugSessionStartedEvent)
    begin
      TThread.Queue(TThread.Current,
        procedure()
        begin
          tcBottomPanelControls.ActiveTab := tiBottomPanelDebugSession;
        end);
    end);

  FDebugSessionStopped := TMessagery.SubscribeToEvent<TDebugSessionStoppedEvent>(
    procedure(const AEventNotification: TDebugSessionStoppedEvent)
    begin
      TThread.Queue(TThread.Current,
        procedure()
        begin
          tcBottomPanelControls.ActiveTab := tiBottomPanelDesignSession;
        end);
    end);
end;

destructor TBottomPanelFrame.Destroy;
begin
  FDebugSessionStopped.Disconnect();
  FDebugSessionStarted.Disconnect();
  inherited;
end;

end.
