unit Frame.Debug.Breakpoints;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  Frame.Debug, Builder.Chain, BaseProtocol.Client, FMX.Controls.Presentation,
  System.Rtti, FMX.Grid.Style, FMX.ScrollBox, FMX.Grid, Container.DataSet.Debugger,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, Fmx.Bind.Grid, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TBreakpointsDebugFrame = class(TDebugFrame)
    grdEvents: TGrid;
    bsdbBreakpoints: TBindSourceDB;
    blBreakpoints: TBindingsList;
    lgdsbsdbBreakpoints: TLinkGridToDataSource;
  private
    FSetupDebuggerDone: IDisconnectable;
    FBreakpointEvent: IUnsubscribable;
  protected
    procedure DebugSessionStarted(const ABaseProtocolClient: TBaseProtocolClient); override;
    procedure DebugSessionEnded(); override;
  public
  end;

implementation

uses
  Baseprotocol,
  BaseProtocol.Events,
  BaseProtocol.Requests;

{$R *.fmx}

{ TBreakpointsDebugFrame }

procedure TBreakpointsDebugFrame.DebugSessionStarted(
  const ABaseProtocolClient: TBaseProtocolClient);
begin
  FSetupDebuggerDone := TGlobalBuilderChain.SubscribeToEvent<TSetupDebuggerDoneEvent>(
    procedure(const AEventNotification: TSetupDebuggerDoneEvent)
    begin
      //
    end);

  FBreakpointEvent := ABaseProtocolClient.SubscribeToEvent<TBreakpointEvent>(
    procedure(const AEventNotification: TBreakpointEvent)
    begin
      //Show all breakpoints
    end);
end;

procedure TBreakpointsDebugFrame.DebugSessionEnded;
begin
  inherited;
  FBreakpointEvent.Unsubscribe();
  FSetupDebuggerDone.Disconnect();
end;

end.
