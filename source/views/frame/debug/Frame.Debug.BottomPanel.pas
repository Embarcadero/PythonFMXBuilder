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
  end;

implementation

{$R *.fmx}

end.
