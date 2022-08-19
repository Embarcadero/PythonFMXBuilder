unit Frame.Debug.Events;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Variants, Frame.Debug,
  System.Rtti, FMX.Grid.Style, Data.Bind.EngExt, Fmx.Bind.DBEngExt,
  Fmx.Bind.Grid, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope, FMX.ScrollBox,
  FMX.Grid, FMX.StdCtrls, System.Classes, FMX.Types, FMX.Controls,
  FMX.Controls.Presentation, Container.DataSet.Debugger, BaseProtocol.Client;

type
  TEventsDebugFrame = class(TDebugFrame)
    grdEvents: TGrid;
    bsdbEvents: TBindSourceDB;
    bsEvents: TBindingsList;
    lgdsbsdbEvents: TLinkGridToDataSource;
  private
  protected
    procedure DebugSessionStarted(const ABaseProtocolClient: TBaseProtocolClient); override;
    procedure DebugSessionEnded(); override;
  public
  end;

implementation

uses
  System.StrUtils,
  BaseProtocol.Types,
  BaseProtocol.Events;

{$R *.fmx}

{ TEventsDebugFrame }

procedure TEventsDebugFrame.DebugSessionStarted(
  const ABaseProtocolClient: TBaseProtocolClient);
begin

end;

procedure TEventsDebugFrame.DebugSessionEnded;
begin
  inherited;

end;

end.
