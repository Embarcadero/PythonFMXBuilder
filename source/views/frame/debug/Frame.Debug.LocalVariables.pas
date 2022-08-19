unit Frame.Debug.LocalVariables;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  Frame.Debug, BaseProtocol.Client, FMX.Controls.Presentation,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  System.Rtti, FMX.Grid.Style, FMX.ScrollBox, FMX.Grid, Data.DB,
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
  private
    FStoppedEvent: IUnsubscribable;
    FBreakpointEvent: IUnsubscribable;
  protected
    procedure DebugSessionStarted(const ABaseProtocolClient: TBaseProtocolClient); override;
    procedure DebugSessionEnded(); override;
  public
  end;

implementation

uses
  BaseProtocol.Events;

{$R *.fmx}

{ TLocalVariablesDebugFrame }

procedure TLocalVariablesDebugFrame.DebugSessionStarted(
  const ABaseProtocolClient: TBaseProtocolClient);
begin
  FStoppedEvent := ABaseProtocolClient.SubscribeToEvent<TStoppedEvent>(
    procedure(const AEventNotification: TStoppedEvent)
    begin
      //Show local variables
    end);

  FBreakpointEvent := ABaseProtocolClient.SubscribeToEvent<TBreakpointEvent>(
    procedure(const AEventNotification: TBreakpointEvent)
    begin
      //Show local variables
    end);
end;

procedure TLocalVariablesDebugFrame.DebugSessionEnded;
begin
  inherited;
  FStoppedEvent.Unsubscribe();
  FBreakpointEvent.Unsubscribe();
end;

end.
