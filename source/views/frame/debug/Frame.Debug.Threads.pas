unit Frame.Debug.Threads;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Builder.Chain, BaseProtocol.Client, Data.Bind.Components, Data.Bind.DBScope,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, Fmx.Bind.Grid, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.Bind.Grid, Frame.Debug, System.Rtti, FMX.Grid.Style,
  FMX.ScrollBox, FMX.Grid, FMX.StdCtrls, FMX.Types, FMX.Controls,
  FMX.Controls.Presentation, Container.DataSet.Debugger;

type
  TThreadsDebugFrame = class(TDebugFrame)
    grdThreads: TGrid;
    bsdbThreads: TBindSourceDB;
    blThreads: TBindingsList;
    lgdsbsdbThreads: TLinkGridToDataSource;
  protected
    procedure DebugSessionStarted(const ABaseProtocolClient: TBaseProtocolClient); override;
    procedure DebugSessionEnded(); override;
  end;

implementation

uses
  BaseProtocol,
  BaseProtocol.Events,
  BaseProtocol.Requests;

{$R *.fmx}

{ TThreadsDebugFrame }

procedure TThreadsDebugFrame.DebugSessionStarted(
  const ABaseProtocolClient: TBaseProtocolClient);
begin
  
end;

procedure TThreadsDebugFrame.DebugSessionEnded;
begin
  inherited;

end;

end.
