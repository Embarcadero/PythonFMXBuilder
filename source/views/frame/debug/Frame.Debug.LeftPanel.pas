unit Frame.Debug.LeftPanel;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  Frame.Debug.LocalVariables, Frame.Debug.StackTrace, FMX.Objects, Frame.Debug,
  FMX.TabControl;

type
  TLeftPanelDebugFrame = class(TFrame)
    frmStackTrace: TStackTraceDebugFrame;
    frmLocalVariables: TLocalVariablesDebugFrame;
    spHorizontal: TSplitter;
    rrSpliterGrip: TRoundRect;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.
