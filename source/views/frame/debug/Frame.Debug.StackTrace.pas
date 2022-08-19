unit Frame.Debug.StackTrace;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  Frame.Debug, BaseProtocol.Client, FMX.Controls.Presentation, System.Rtti,
  FMX.Grid.Style, FMX.ScrollBox, FMX.Grid, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, Data.Bind.EngExt, Fmx.Bind.DBEngExt,
  Fmx.Bind.Grid, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, Container.DataSet.Debugger;

type
  TStackTraceDebugFrame = class(TDebugFrame)
    grdStackTrace: TGrid;
    bsdbStackTrace: TBindSourceDB;
    blStackTrace: TBindingsList;
    lgdsbsdbStackTrace: TLinkGridToDataSource;
  private
    FStoppedEvent: IUnsubscribable;
    FModuleEvent: IUnsubscribable;
    FLoadedSourceEvent: IUnsubscribable;
  protected
    procedure DebugSessionStarted(const ABaseProtocolClient: TBaseProtocolClient); override;
    procedure DebugSessionEnded(); override;
  public
  end;

implementation

uses
  BaseProtocol,
  BaseProtocol.Requests,
  BaseProtocol.Events;

{$R *.fmx}

{ TStackTraceDebugFrame }

procedure TStackTraceDebugFrame.DebugSessionStarted(
  const ABaseProtocolClient: TBaseProtocolClient);
begin
  FStoppedEvent := ABaseProtocolClient.SubscribeToEvent<TStoppedEvent>(
    procedure(const AEventNotification: TStoppedEvent)
    begin
      //
    end);

  FModuleEvent := ABaseProtocolClient.SubscribeToEvent<TModuleEvent>(
    procedure(const AEventNotification: TModuleEvent)
    begin
      //Show current frame
    end);

  FLoadedSourceEvent := ABaseProtocolClient.SubscribeToEvent<TDynamicLoadedSourceEvent>(
    procedure(const AEventNotification: TDynamicLoadedSourceEvent)
    begin
      //Show current frame
    end);
end;

procedure TStackTraceDebugFrame.DebugSessionEnded;
begin
  inherited;
  FStoppedEvent.Unsubscribe();
  FModuleEvent.Unsubscribe();
  FLoadedSourceEvent.Unsubscribe();
end;

end.
