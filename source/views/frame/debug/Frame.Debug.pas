unit Frame.Debug;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  BaseProtocol.Client, FMX.Controls.Presentation, Builder.Messagery;

type
  TDebugFrame = class(TFrame)
    tbFrame: TToolBar;
    lbDescription: TLabel;
  private
    FDebugSessionStarted: IDisconnectable;
    FDebugSessionStopped: IDisconnectable;
  protected
    procedure DebugSessionStarted(const ABaseProtocolClient: TBaseProtocolClient); virtual;
    procedure DebugSessionEnded(); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  end;

implementation

{$R *.fmx}

{ TDebugFrame }

constructor TDebugFrame.Create(AOwner: TComponent);
begin
  inherited;
  FDebugSessionStarted := TMessagery.SubscribeToEvent<TDebugSessionStartedEvent>(
    procedure(const AEventNotification: TDebugSessionStartedEvent)
    begin
      DebugSessionStarted(AEventNotification.Body.Debugger);
    end);

  FDebugSessionStopped := TMessagery.SubscribeToEvent<TDebugSessionStoppedEvent>(
    procedure(const AEventNotification: TDebugSessionStoppedEvent)
    begin
      DebugSessionEnded();
    end);
end;

destructor TDebugFrame.Destroy;
begin
  FDebugSessionStopped.Disconnect();
  FDebugSessionStarted.Disconnect();
  inherited;
end;

procedure TDebugFrame.DebugSessionEnded;
begin
  //
end;

procedure TDebugFrame.DebugSessionStarted(
  const ABaseProtocolClient: TBaseProtocolClient);
begin
  //
end;

end.
