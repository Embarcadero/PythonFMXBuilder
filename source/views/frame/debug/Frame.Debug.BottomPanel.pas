unit Frame.Debug.BottomPanel;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TabControl, Frame.Debug.Threads, Frame.Debug.Breakpoints,
  Frame.Debug.Events, Frame.Debug;

type
  TBottomPanelDebugFrame = class(TFrame)
    tiBottomPanelDebugEvents: TTabItem;
    tiBottomPanelDebugThreads: TTabItem;
    tiBottomPanelDebugBreakpoints: TTabItem;
    frmDebugEvents: TEventsDebugFrame;
    frmDebugBreakpoint: TBreakpointsDebugFrame;
    frmDebugThreads: TThreadsDebugFrame;
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  end;

implementation

{$R *.fmx}

{ TBottomPanelDebugFrame }

constructor TBottomPanelDebugFrame.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TBottomPanelDebugFrame.Destroy;
begin

  inherited;
end;

end.
