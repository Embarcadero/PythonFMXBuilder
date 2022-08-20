unit Frame.Debug.Threads;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Data.Bind.Components, Data.Bind.DBScope, Data.Bind.EngExt, Fmx.Bind.DBEngExt,
  Fmx.Bind.Grid, System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.Grid,
  Frame.Debug, System.Rtti, FMX.Grid.Style, FMX.ScrollBox, FMX.Grid,
  FMX.StdCtrls, FMX.Types, FMX.Controls, FMX.Controls.Presentation,
  Container.DataSet.Debugger;

type
  TThreadsDebugFrame = class(TDebugFrame)
    grdThreads: TGrid;
    bsdbThreads: TBindSourceDB;
    blThreads: TBindingsList;
    lgdsbsdbThreads: TLinkGridToDataSource;
  end;

implementation

{$R *.fmx}

end.
