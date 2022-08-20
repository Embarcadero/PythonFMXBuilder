unit Frame.Debug.LocalVariables;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  Frame.Debug, FMX.Controls.Presentation, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, System.Rtti, FMX.Grid.Style, FMX.ScrollBox, FMX.Grid, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Fmx.Bind.Grid, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope,
  Container.DataSet.Debugger;

type
  TLocalVariablesDebugFrame = class(TDebugFrame)
    grdLocalVariables: TGrid;
    bsdbLocalVariables: TBindSourceDB;
    blLocalVariables: TBindingsList;
    lgdsbsdbLocalVariables: TLinkGridToDataSource;
  end;

implementation

{$R *.fmx}

end.
