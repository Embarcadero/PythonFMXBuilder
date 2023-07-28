unit Frame.Editor.TMSMemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  System.Generics.Collections, FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms,
  FMX.Dialogs, FMX.StdCtrls, FMX.TMSBaseControl, FMX.TMSMemo, FMX.Controls.Presentation,
  Frame.Editor.TabItem, Builder.Chain, BaseProtocol, BaseProtocol.Types,
  BaseProtocol.Events, BaseProtocol.Requests, BaseProtocol.Client,
  Builder.Types, Builder.Model.Environment, Builder.Services;

type
  TTMSMemoEditorFrame = class(TFrame, ITextEditor)
    mmEditor: TTMSFMXMemo;
    procedure mmEditorGutterClick(Sender: TObject; LineNo: Integer);
    procedure mmEditorChangeTracking(Sender: TObject);
  private
    FFileName: string;
    FModified: boolean;
    FEnvironmentServices: IEnvironmentServices;
    FProjectServices: IProjectServices;
    FDebugSessionStarted: IDisconnectable;
    FDebugSessionStopped: IDisconnectable;
    FSetupDebugger: IDisconnectable;
    FStoppedEvent: IUnsubscribable;
    function GetFileName(): string;
    function GetModified(): boolean;
    procedure SetModified(const AModified: boolean);
    function GetBreakpoints(): TArray<integer>;
    procedure SetBreakpoints(ABreakpoints: TArray<integer>);
    function GetActiveLine(): integer;
    procedure SetActiveLine(AActiveLine: integer);
    function GetShowActiveLine(): boolean;
    procedure SetShowActiveLine(AShowActiveLine: boolean);
    function GetRemoteRootPath(const AFileName: string): string;
    procedure Open(const AFileName: string; const AEditing: boolean = false);
    procedure Close();
    procedure Save();
    procedure SaveTo(const AFileName: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  end;

implementation

uses
  System.IOUtils, System.Math,
  Container.DataSet.Debugger,
  Builder.Services.Factory;

{$R *.fmx}

{ TTMSMemoEditorFrame }

constructor TTMSMemoEditorFrame.Create(AOwner: TComponent);
begin
  inherited;
  FProjectServices := TServiceSimpleFactory.CreateProject();
  FEnvironmentServices := TServiceSimpleFactory.CreateEnvironment();

  mmEditor.ActiveLineSettings.ActiveLineAtCursor := false;

  FDebugSessionStarted := TGlobalBuilderChain.SubscribeToEvent<TDebugSessionStartedEvent>(
    procedure(const AEventNotification: TDebugSessionStartedEvent)
    begin
      var LDebugger := AEventNotification.Body.Debugger;
      FStoppedEvent := LDebugger.SubscribeToEvent<TStoppedEvent>(
      procedure(const AEvent: TStoppedEvent)
      begin
        //
      end);
    end);

  FDebugSessionStopped := TGlobalBuilderChain.SubscribeToEvent<TDebugSessionStoppedEvent>(
    procedure(const AEventNotification: TDebugSessionStoppedEvent)
    begin
      mmEditor.ActiveLineSettings.ShowActiveLine := false;
      mmEditor.ActiveLineSettings.ShowActiveLineIndicator := false;
      FStoppedEvent.Unsubscribe();
    end);

  FSetupDebugger := TGlobalBuilderChain.SubscribeToEvent<TSetupDebuggerEvent>(
    procedure(const AEventNotification: TSetupDebuggerEvent)
    begin

    end);
end;

destructor TTMSMemoEditorFrame.Destroy;
begin
  inherited;
  DebuggerDataSetContainer.RemoveSource(TPath.GetFileName(FFileName));
  FSetupDebugger.Disconnect();
  FDebugSessionStopped.Disconnect();
  FDebugSessionStarted.Disconnect();
end;

procedure TTMSMemoEditorFrame.Close;
begin
  //
end;

function TTMSMemoEditorFrame.GetBreakpoints: TArray<integer>;
begin
  Result := [];
  for var I := 0 to mmEditor.Lines.Count - 1 do begin
    if mmEditor.BreakPoint[I] then
      Result := Result + [I + 1];
  end;
end;

function TTMSMemoEditorFrame.GetFileName: string;
begin
  Result := FFileName;
end;

function TTMSMemoEditorFrame.GetModified: boolean;
begin
  Result := FModified;
end;

function TTMSMemoEditorFrame.GetRemoteRootPath(const AFileName: string): string;
begin
  if FEnvironmentServices.HasActiveEnvironment() then begin
    var LProjectModel := FProjectServices.GetActiveProject();
    var LEnvironmentModel := FEnvironmentServices.GetActiveEnvironment();
    Result := LEnvironmentModel.RemoteDebuggerRoot
      .Replace('$(package_name)', LProjectModel.PackageName, [rfIgnoreCase])
        .Trim();

    if not Result.EndsWith('/') then
      Result := Result + '/';

    Result := Result + TPath.GetFileName(AFileName);
  end else
    Result := String.Empty;
end;

procedure TTMSMemoEditorFrame.SetBreakpoints(ABreakpoints: TArray<integer>);
begin
  for var ABrekpoints in ABreakpoints do begin
    mmEditor.BreakPoint[ABrekpoints] := true;
  end;
end;

procedure TTMSMemoEditorFrame.SetModified(const AModified: boolean);
begin
  if (FModified <> AModified) then begin
    FModified := AModified;
    TGlobalBuilderChain.BroadcastEventAsync(
      TEditorChangedEvent.Create(Self, AModified));
  end;
end;

function TTMSMemoEditorFrame.GetActiveLine: integer;
begin
  Result := mmEditor.ActiveLine;
end;

procedure TTMSMemoEditorFrame.SetActiveLine(AActiveLine: integer);
begin
  mmEditor.ActiveLine := AActiveLine;
end;

function TTMSMemoEditorFrame.GetShowActiveLine: boolean;
begin
  Result := mmEditor.ActiveLineSettings.ShowActiveLineIndicator;
end;

procedure TTMSMemoEditorFrame.SetShowActiveLine(AShowActiveLine: boolean);
begin
  if AShowActiveLine then
    if (mmEditor.VisibleLineCount + mmEditor.TopLine) < mmEditor.ActiveLine then
      mmEditor.TopLine := Max(mmEditor.ActiveLine - 5, 0)
    else if mmEditor.ActiveLine < (mmEditor.VisibleLineCount + mmEditor.TopLine) then
      mmEditor.TopLine := Max(mmEditor.ActiveLine - 5, 0);

  mmEditor.ActiveLineSettings.ShowActiveLine := AShowActiveLine;
  mmEditor.ActiveLineSettings.ShowActiveLineIndicator := AShowActiveLine;
end;

procedure TTMSMemoEditorFrame.Open(const AFileName: string; const AEditing: boolean);
begin
  FFileName := AFileName;
  if TFile.Exists(AFileName) then
    mmEditor.Lines.LoadFromFile(AFileName);

  { TODO : REMOVE ALL OLD SETTINGS }
  DebuggerDataSetContainer.AddSource(
    TPath.GetFileName(AFileName),
    AFileName,
    GetRemoteRootPath(AFileName));
  SetModified(AEditing);
end;

procedure TTMSMemoEditorFrame.Save;
begin
  if TFile.Exists(FFileName) then
    SaveTo(FFileName);
end;

procedure TTMSMemoEditorFrame.SaveTo(const AFileName: string);
begin
  if TFile.Exists(AFileName) then begin
    mmEditor.Lines.SaveToFile(AFileName);
    SetModified(false);
  end;
end;

procedure TTMSMemoEditorFrame.mmEditorChangeTracking(Sender: TObject);
begin
  if mmEditor.Modified then
    SetModified(mmEditor.Modified);
end;

procedure TTMSMemoEditorFrame.mmEditorGutterClick(Sender: TObject;
  LineNo: Integer);
begin
  //There's a bug in version 1.6.0.1 that doesn't get the correct LineNo.
  //Not sure which versions might be affected.
  //-> You should consider only the LineNo if this fix results on
  //   breakpoint line number error for you.
  var LCurLine := LineNo + mmEditor.TopLine;

  //Setting a breakpoint in an empty line
  if LCurLine >= mmEditor.Lines.Count then
    Exit;

  mmEditor.BreakPoint[LCurLine] := not mmEditor.BreakPoint[LCurLine];

  if mmEditor.BreakPoint[LCurLine] then begin
    DebuggerDataSetContainer.AddBreakpoint(
      TPath.GetFileName(FFileName),
      LCurLine + 1
    );
  end else begin
    DebuggerDataSetContainer.RemoveBreakpoint(
      TPath.GetFileName(FFileName),
      LCurLine + 1
    );
  end;
end;

initialization
  TEditorTabItem.DefaultEditorClass := TTMSMemoEditorFrame;

finalization
  TEditorTabItem.DefaultEditorClass := nil;

end.
