unit Frame.Debug.StackTrace;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  Frame.Debug, FMX.Controls.Presentation, System.Rtti,  FMX.Grid.Style,
  FMX.ScrollBox, FMX.Grid, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Data.Bind.EngExt, Fmx.Bind.DBEngExt, Fmx.Bind.Grid,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.Components,
  Data.Bind.Grid, Data.Bind.DBScope, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, Container.DataSet.Debugger;

type
  TStackTraceDebugFrame = class(TDebugFrame)
    grdStackTrace: TGrid;
    bsdbStackTrace: TBindSourceDB;
    blStackTrace: TBindingsList;
    lgdsbsdbStackTrace: TLinkGridToDataSource;
  end;

implementation

{$R *.fmx}

end.
