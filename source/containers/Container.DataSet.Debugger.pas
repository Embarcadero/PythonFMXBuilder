unit Container.DataSet.Debugger;

interface

uses
  System.SysUtils, System.Classes, Builder.Messagery,
  BaseProtocol, BaseProtocol.Types, BaseProtocol.Events,
  BaseProtocol.Requests, BaseProtocol.Client,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TDebuggerDataSetContainer = class(TDataModule)
    fdmtThread: TFDMemTable;
    fdmtThreadthread_id: TIntegerField;
    fdmtThreadthread_name: TStringField;
    fdmtEvent: TFDMemTable;
    fdmtEventevent_desc: TStringField;
    fdmtSource: TFDMemTable;
    fdmtSourcesource_name: TStringField;
    fdmtSourcesource_local_path: TStringField;
    fdmtSourcesource_remote_path: TStringField;
    fdmtBreakpoint: TFDMemTable;
    fdmtBreakpointbreakpoint_id: TIntegerField;
    fdmtBreakpointbreakpoint_line: TIntegerField;
    fdmtBreakpointbreakpoint_source_name: TStringField;
    fdmtStackTrace: TFDMemTable;
    fdmtStackTracestacktrace_name: TStringField;
    fdmtStackTracestacktrace_line: TIntegerField;
    fdmtStackTracestacktrace_source_local_path: TStringField;
    fdmtStackTracestacktrace_line_content: TStringField;
    fdmtStackTracestacktrace_description: TStringField;
    fdmtStackTracestacktrace_id: TIntegerField;
    fdmtVariable: TFDMemTable;
    fdmtScope: TFDMemTable;
    fdmtScopescope_stacktrace_id: TIntegerField;
    fdmtScopescope_name: TStringField;
    fdmtScopescope_variables_reference: TIntegerField;
    fdmtVariablevariable_reference: TIntegerField;
    fdmtVariablevariable_name: TStringField;
    fdmtVariablevariable_type: TStringField;
    fdmtVariablevariable_value: TStringField;
    fdmtVariablevariable_evaluate_name: TStringField;
    dsStackTrace: TDataSource;
    dsScope: TDataSource;
    fdmtVariablevariable_stacktrace_id: TIntegerField;
    fdmtActiveSource: TFDMemTable;
    fdmtStackTracestacktrace_thread_id: TIntegerField;
    fdmtActiveSourceactive_source_thread_id: TIntegerField;
    fdmtActiveSourceactive_source_local_file_path: TStringField;
    fdmtActiveSourceactive_source_line: TIntegerField;
    fdmtActiveSourceactive_source_line_indicator: TBooleanField;
    fdmtBreakpointbreakpoint_confirmed: TBooleanField;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure fdmtBreakpointAfterPost(DataSet: TDataSet);
    procedure fdmtBreakpointAfterDelete(DataSet: TDataSet);
  private
    //Builder events
    FDebugSessionStarted: IDisconnectable;
    FDebugSessionStopped: IDisconnectable;
    FSetupDebugger: IDisconnectable;
    FSetupDebuggerDone: IDisconnectable;
    FDebugAction: IDisconnectable;
    //Debugger events
    FContinuedEvent: IUnsubscribable;
    FExitedEvent: IUnsubscribable;
    FModuleEvent: IUnsubscribable;
    FThreadEvent: IUnsubscribable;
    FOutputEvent: IUnsubscribable;
    FStoppedEvent: IUnsubscribable;
    FBreakpointEvent: IUnsubscribable;
    FLoadedSourceEvent: IUnsubscribable;
    FProcessEvent: IUnsubscribable;
    FMemoryEvent: IUnsubscribable;
    //Debugger
    FDebugger: TBaseProtocolClient;
    procedure DebugSessionStarted(const ABaseProtocolClient: TBaseProtocolClient);
    procedure DebugSessionEnded();
    procedure SetupDebugger(const ABaseProtocolClient: TBaseProtocolClient);
    procedure SetupDebuggerDone(const ABaseProtocolClient: TBaseProtocolClient);

    procedure SetupBreakpoints(const ABaseProtocolClient: TBaseProtocolClient);
    procedure SetupBreakpointsForCurrentSource(const ABaseProtocolClient: TBaseProtocolClient);
    procedure BreakPointsConfirmation(const ASetBreakpointsResponse: TSetBreakpointsResponse);

    //Stack Frames
    procedure RequestStackTraces(const ABaseProtocolClient: TBaseProtocolClient;
      const AThreadId: integer; const AOpenFileOfLastFrame: boolean);
    function GetCodeAtLine(const AFileName: string; const ALine: integer): string;
    procedure RequestScopes(const ABaseProtocolClient: TBaseProtocolClient;
      const AFrameId: integer);
    procedure RequestVariables(const ABaseProtocolClient: TBaseProtocolClient;
      const AFrameId, AVariablesReference: integer);
  public
    //Event
    procedure Event(const AMessage: string);
    //Source
    procedure AddSource(const ASourceName, ALocalPath, ARemotePath: string);
    procedure RemoveSource(const ASourceName: string);
    function GetLocalSourcePathByName(const ASourceName: string): string;
    //Breakpoint
    procedure AddBreakpoint(const ASourceName: string; const ALine: integer);
    procedure RemoveBreakpoint(const ASourceName: string; const ALine: integer);
    procedure ClearBreakpoints(const ASourceName: string);
    //Clear active source line and indicator values
    procedure ClearSourceLineAndIndicator();
  end;

var
  DebuggerDataSetContainer: TDebuggerDataSetContainer;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses
  System.StrUtils, System.IOUtils;

procedure TDebuggerDataSetContainer.DataModuleCreate(Sender: TObject);
begin
  fdmtEvent.CreateDataSet();
  fdmtThread.CreateDataSet();
  fdmtSource.CreateDataSet();
  fdmtBreakpoint.CreateDataSet();
  fdmtStackTrace.CreateDataSet();
  fdmtScope.CreateDataSet();
  fdmtVariable.CreateDataSet();
  fdmtActiveSource.CreateDataSet();

  FDebugSessionStarted := TMessagery.SubscribeToEvent<TDebugSessionStartedEvent>(
    procedure(const AEventNotification: TDebugSessionStartedEvent)
    begin
      FDebugger := AEventNotification.Body.Debugger;
      DebugSessionStarted(AEventNotification.Body.Debugger);
    end);

  FDebugSessionStopped := TMessagery.SubscribeToEvent<TDebugSessionStoppedEvent>(
    procedure(const AEventNotification: TDebugSessionStoppedEvent)
    begin
      FDebugger := nil;
      DebugSessionEnded();
    end);

  FSetupDebugger := TMessagery.SubscribeToEvent<TSetupDebuggerEvent>(
    procedure(const AEventNotification: TSetupDebuggerEvent)
    begin
      SetupDebugger(AEventNotification.Body.Debugger);
    end);

  FSetupDebuggerDone := TMessagery.SubscribeToEvent<TSetupDebuggerDoneEvent>(
    procedure(const AEventNotification: TSetupDebuggerDoneEvent)
    begin
      SetupDebuggerDone(AEventNotification.Body.Debugger);
    end);

  FDebugAction := TMessagery.SubscribeToEvent<TDebugActionEvent>(
    procedure(const AEventNotification: TDebugActionEvent)
    const
      DANGLE_INDICATOR_STATES = [
        TDebugAction.Stop,
        TDebugAction.StepIn,
        TDebugAction.StepOver,
        TDebugAction.StepOut,
        TDebugAction.Continue];
    begin
      if not (AEventNotification.Body.Action in DANGLE_INDICATOR_STATES) then
        Exit;

      if fdmtActiveSource.IsEmpty then
        Exit;

      ClearSourceLineAndIndicator();
    end);
end;

procedure TDebuggerDataSetContainer.DataModuleDestroy(Sender: TObject);
begin
  FSetupDebuggerDone.Disconnect();
  FSetupDebugger.Disconnect();
  FDebugSessionStopped.Disconnect();
  FDebugSessionStarted.Disconnect();
end;

procedure TDebuggerDataSetContainer.DebugSessionStarted(
  const ABaseProtocolClient: TBaseProtocolClient);
begin
  FStoppedEvent := ABaseProtocolClient.SubscribeToEvent<TStoppedEvent>(
    procedure(const AEventNotification: TStoppedEvent)
    begin
      var LMessage := 'has stopped due to a %s instruction.';
      if AEventNotification.Body.AllThreadStopped then
        LMessage := 'All threads '
      + LMessage
      else
        LMessage := 'Thread '
          + AEventNotification.Body.ThreadId.ToString()
          + ' '
          + LMessage;

      case AEventNotification.Body.Reason of
        TStoppedEventReason.Step:
          LMessage := Format(LMessage, ['step']);
        TStoppedEventReason.Breakpoint:
          LMessage := Format(LMessage, ['breakpoint']);
        TStoppedEventReason.Exception:
          LMessage := Format(LMessage, ['exception']);
        TStoppedEventReason.Pause:
          LMessage := Format(LMessage, ['pause']);
        TStoppedEventReason.Entry:
          LMessage := Format(LMessage, ['entry']);
        TStoppedEventReason.Goto:
          LMessage := Format(LMessage, ['goto']);
        TStoppedEventReason.FunctionBreakpoint:
          LMessage := Format(LMessage, ['function breakpoint']);
        TStoppedEventReason.DataBreakpoint:
          LMessage := Format(LMessage, ['data breakpoint']);
        TStoppedEventReason.InstructionBreakpoint:
          LMessage := Format(LMessage, ['instruction breakpoint']);
      end;

      if not AEventNotification.Body.Description.IsEmpty() then
        LMessage := LMessage
          + ' Details: '
          + AEventNotification.Body.Description;

      if not AEventNotification.Body.Text.IsEmpty() then
        LMessage := LMessage
          + ' - '
          + AEventNotification.Body.Text;

      Event(LMessage);

      case AEventNotification.Body.Reason of
        TStoppedEventReason.Step,
        TStoppedEventReason.Breakpoint,
        TStoppedEventReason.Exception,
        TStoppedEventReason.Pause,
        TStoppedEventReason.FunctionBreakpoint,
        TStoppedEventReason.DataBreakpoint,
        TStoppedEventReason.InstructionBreakpoint: begin
          RequestStackTraces(
            ABaseProtocolClient,
            AEventNotification.Body.ThreadId,
            true);
        end;
      end;
    end);

  FContinuedEvent := ABaseProtocolClient.SubscribeToEvent<TContinuedEvent>(
    procedure(const AEventNotification: TContinuedEvent)
    begin
      if AEventNotification.Body.AllThreadsContinued then
        Event('All threads continued.')
      else
        Event(Format(
          'Thread %d continued.', [
          AEventNotification.Body.ThreadId]));

      ClearSourceLineAndIndicator();
    end);

  FBreakpointEvent := ABaseProtocolClient.SubscribeToEvent<TBreakpointEvent>(
    procedure(const AEventNotification: TBreakpointEvent)
    begin
      case AEventNotification.Body.Reason of
        TBreakpointEventReason.Changed: Event(Format(
          'Breakpoint changed at %s line %d.', [
          AEventNotification.Body.Breakpoint.Source.Name,
          AEventNotification.Body.Breakpoint.Line]));
        TBreakpointEventReason.New    : Event(Format(
          'Breakpoint added at %s line %d', [
          AEventNotification.Body.Breakpoint.Source.Name,
          AEventNotification.Body.Breakpoint.Line]));
        TBreakpointEventReason.Removed: Event(Format(
          'Breakpoint removed at %s line %d', [
          AEventNotification.Body.Breakpoint.Source.Name,
          AEventNotification.Body.Breakpoint.Line]));
      end;
    end);

  FModuleEvent := ABaseProtocolClient.SubscribeToEvent<TModuleEvent>(
    procedure(const AEventNotification: TModuleEvent)
    begin
      case AEventNotification.Body.Reason of
        TModuleEventReason.New    : Event(Format(
          '%sodule %s was added.', [
          IfThen(AEventNotification.Body.Module.IsUserCode, 'User m', 'M'),
          AEventNotification.Body.Module.Name]));
        TModuleEventReason.Changed: Event(Format(
          '%sodule %s has changed.', [
          IfThen(AEventNotification.Body.Module.IsUserCode, 'User m', 'M'),
          AEventNotification.Body.Module.Name]));
        TModuleEventReason.Removed: Event(Format(
          '%sodule %s was removed.', [
          IfThen(AEventNotification.Body.Module.IsUserCode, 'User m', 'M'),
          AEventNotification.Body.Module.Name]));
      end;
    end);

  FThreadEvent := ABaseProtocolClient.SubscribeToEvent<TThreadEvent>(
    procedure(const AEventNotification: TThreadEvent)
    begin
      case AEventNotification.Body.Reason of
        TThreadEventReason.Started: Event(Format(
          'Thread %d has started.', [
          AEventNotification.Body.ThreadId]));
        TThreadEventReason.Exited : Event(Format(
          'Thread %d exited.', [
          AEventNotification.Body.ThreadId]));
      end;
    end);

  FMemoryEvent := ABaseProtocolClient.SubscribeToEvent<TMemoryEvent>(
    procedure(const AEventNotification: TMemoryEvent)
    begin
      Event(Format(
        'Memory %s has been updated.', [
        AEventNotification.Body.MemoryReference]));
    end);

  FOutputEvent := ABaseProtocolClient.SubscribeToEvent<TDynamicOutputEvent>(
    procedure(const AEventNotification: TDynamicOutputEvent)
    begin
      //Log event
    end);

  FLoadedSourceEvent := ABaseProtocolClient.SubscribeToEvent<TDynamicLoadedSourceEvent>(
    procedure(const AEventNotification: TDynamicLoadedSourceEvent)
    begin
      case AEventNotification.Body.Reason of
        TLoadedSourceEventReason.New     : Event(Format(
          'Source %s was added.', [AEventNotification.Body.Source.Name]));
        TLoadedSourceEventReason.Changed : Event(Format(
          'Source %s has changed.', [AEventNotification.Body.Source.Name]));
        TLoadedSourceEventReason.Removed : Event(Format(
          'Source %s was removed.', [AEventNotification.Body.Source.Name]));
      end;
    end);

  FProcessEvent := ABaseProtocolClient.SubscribeToEvent<TProcessEvent>(
    procedure(const AEventNotification: TProcessEvent)
    begin
      case AEventNotification.Body.StartedMethod of
        TProcessStartedMethod.None: Event(Format(
          'Process %s (%d) is running.', [
          AEventNotification.Body.Name, AEventNotification.Body.SystemProcessId]));
        TProcessStartedMethod.Launch: Event(Format(
          'Process %s (%d) launch.', [
          AEventNotification.Body.Name, AEventNotification.Body.SystemProcessId]));
        TProcessStartedMethod.Attach: Event(Format(
          'Process %s (%d) attached.', [
          AEventNotification.Body.Name, AEventNotification.Body.SystemProcessId]));
        TProcessStartedMethod.AttachForSuspendedLaunch: Event(Format(
          'Process %s (%d) attached for suspended launch.', [
          AEventNotification.Body.Name, AEventNotification.Body.SystemProcessId]));
      end;
    end);

  FExitedEvent := ABaseProtocolClient.SubscribeToEvent<TExitedEvent>(
    procedure(const AEventNotification: TExitedEvent)
    begin
      Event(Format(
        'Process exited with result code %d.', [
        AEventNotification.Body.ExitCode]))
    end);
end;

procedure TDebuggerDataSetContainer.DebugSessionEnded;
begin
  System.Classes.TThread.Queue(System.Classes.TThread.Current,
    procedure
    begin
      fdmtEvent.EmptyDataSet();
      fdmtThread.EmptyDataSet();
      fdmtStackTrace.EmptyDataSet();
      fdmtScope.EmptyDataSet();
      fdmtVariable.EmptyDataSet();
    end);
  FContinuedEvent.Unsubscribe();
  FExitedEvent.Unsubscribe();
  FModuleEvent.Unsubscribe();
  FThreadEvent.Unsubscribe();
  FOutputEvent.Unsubscribe();
  FStoppedEvent.Unsubscribe();
  FBreakpointEvent.Unsubscribe();
  FLoadedSourceEvent.Unsubscribe();
  FProcessEvent.Unsubscribe();
  FMemoryEvent.Unsubscribe();
end;

procedure TDebuggerDataSetContainer.SetupBreakpoints(
  const ABaseProtocolClient: TBaseProtocolClient);
begin
  fdmtSource.DisableControls();
  try
    fdmtSource.First();
    while not fdmtSource.Eof do begin
      SetupBreakpointsForCurrentSource(ABaseProtocolClient);
      fdmtSource.Next();
    end;
  finally
    fdmtSource.EnableControls();
  end;
end;

procedure TDebuggerDataSetContainer.SetupBreakpointsForCurrentSource(
  const ABaseProtocolClient: TBaseProtocolClient);
begin
  fdmtBreakpoint.DisableControls();
  try
    fdmtBreakpoint.Filter := 'breakpoint_source_name='
                           + fdmtSourcesource_name.AsString.QuotedString();
    fdmtBreakpoint.Filtered := true;
    try
      if fdmtBreakpoint.IsEmpty() then
        Exit;

      var LSetBreakpoints := TDynamicSetBreakpointsRequest.Create();
      try
        var LSource := LSetBreakpoints.Arguments.Source;
        LSource.Name := fdmtSourcesource_name.AsString;
        LSource.Path := fdmtSourcesource_remote_path.AsString;
        LSource.Origin := fdmtSourcesource_local_path.AsString;

        var LBreakpoints := LSetBreakpoints.Arguments.Breakpoints;
        fdmtBreakpoint.First();
        while not fdmtBreakpoint.Eof do begin
          var LBreakpoint := TSourceBreakpoint.Create();
          LBreakpoint.Line := fdmtBreakpointbreakpoint_line.AsInteger;
          LBreakpoints.Add(LBreakpoint);
          fdmtBreakpoint.Next();
        end;
      except
        on E: Exception do begin
          LSetBreakpoints.Free();
          raise;
        end;
      end;

      try
        ABaseProtocolClient.SendRequest<TSetBreakpointsResponse>(
          LSetBreakpoints,
          procedure(const AArg: TSetBreakpointsResponse)
          begin
            System.Classes.TThread.Synchronize(
              System.Classes.TThread.Current,
              procedure()
              begin
                BreakPointsConfirmation(AArg);
              end);
          end,
          procedure(const AArg: TResponseMessage)
          begin

          end);
      finally
        LSetBreakpoints.Free();
      end;
    finally
      fdmtBreakpoint.Filtered := false;
    end;
  finally
    fdmtBreakpoint.EnableControls();
  end;
end;

procedure TDebuggerDataSetContainer.BreakPointsConfirmation(
  const ASetBreakpointsResponse: TSetBreakpointsResponse);
begin
  fdmtSource.DisableControls();
  fdmtBreakpoint.DisableControls();
  try
    var LSourceBookmark := fdmtSource.Bookmark;
    try
      for var LItem in ASetBreakpointsResponse.Body.Breakpoints do begin
        var LBreakpointBookmark := fdmtBreakpoint.Bookmark;
        try
          if not fdmtSource.Locate('source_name', LItem.Source.Name) then
            Continue;

          if not fdmtBreakpoint.Locate('breakpoint_line', LItem.Line) then
            Continue;

          if not LItem.Verified then
            Continue;

          fdmtBreakpoint.Edit();
          fdmtBreakpointbreakpoint_id.AsInteger := LItem.Id;
          fdmtBreakpointbreakpoint_confirmed.AsBoolean := true;
          fdmtBreakpoint.Post();
        finally
          if fdmtBreakpoint.BookmarkValid(LBreakpointBookmark) then
            fdmtBreakpoint.Bookmark := LBreakpointBookmark;
        end;
      end;
    finally
      if fdmtSource.BookmarkValid(LSourceBookmark) then
        fdmtSource.Bookmark := LSourceBookmark;
    end;
  finally
    fdmtBreakpoint.EnableControls();
    fdmtSource.EnableControls();
  end;
end;

procedure TDebuggerDataSetContainer.SetupDebugger(
  const ABaseProtocolClient: TBaseProtocolClient);
begin
  System.Classes.TThread.Synchronize(
    System.Classes.TThread.Current,
    procedure()
    begin
      SetupBreakpoints(ABaseProtocolClient);
    end);
end;

procedure TDebuggerDataSetContainer.SetupDebuggerDone(
  const ABaseProtocolClient: TBaseProtocolClient);
begin
  var LThreads := TThreadsRequest.Create();
  try
    ABaseProtocolClient.SendRequest<TThreadsResponse>(
      LThreads,
      procedure(const AArg: TThreadsResponse)
      begin
        System.Classes.TThread.Synchronize(System.Classes.TThread.Current,
          procedure
          begin
            for var LThread in AArg.Body.Threads do
              fdmtThread.AppendRecord([LThread.Id, LThread.Name]);
          end);
      end,
      procedure(const AArg: TResponseMessage)
      begin

      end);
  finally
    LThreads.Free();
  end;
end;

procedure TDebuggerDataSetContainer.Event(const AMessage: string);
begin
  System.Classes.TThread.Queue(System.Classes.TThread.Current,
    procedure
    begin
      fdmtEvent.AppendRecord([AMessage]);
    end);
end;

procedure TDebuggerDataSetContainer.fdmtBreakpointAfterDelete(
  DataSet: TDataSet);
begin
  if Assigned(FDebugger) and fdmtBreakpointbreakpoint_confirmed.AsBoolean then
    SetupBreakpointsForCurrentSource(FDebugger);
end;

procedure TDebuggerDataSetContainer.fdmtBreakpointAfterPost(DataSet: TDataSet);
begin
  if Assigned(FDebugger) and not fdmtBreakpointbreakpoint_confirmed.AsBoolean then
    SetupBreakpointsForCurrentSource(FDebugger);
end;

function TDebuggerDataSetContainer.GetCodeAtLine(const AFileName: string;
  const ALine: integer): string;
begin
  Result := String.Empty;
  fdmtSource.DisableControls();
  try
    var LSourceBookmark := fdmtSource.Bookmark;
    try
      if fdmtSource.Locate('source_name', AFileName, [loCaseInsensitive]) then begin
        var LLines := TFile.ReadAllLines(fdmtSourcesource_local_path.AsString);
        if (Length(LLines) >= Pred(ALine)) then
          Result := LLines[Pred(ALine)].Trim();
      end;
    finally
      if fdmtSource.BookmarkValid(LSourceBookmark) then
        fdmtSource.Bookmark := LSourceBookmark;
    end;
  finally
    fdmtSource.EnableControls();
  end;
end;

function TDebuggerDataSetContainer.GetLocalSourcePathByName(
  const ASourceName: string): string;
begin
  if ASourceName.Trim.IsEmpty() then
    Exit(String.Empty);

  var LResult := String.Empty;
  System.Classes.TThread.Synchronize(System.Classes.TThread.Current,
    procedure
    begin
      fdmtSource.DisableControls();
      try
        var LBookmark := fdmtSource.Bookmark;
        try
          if fdmtSource.Locate('source_name', ASourceName, [loCaseInsensitive]) then
            LResult := fdmtSourcesource_local_path.AsString;
        finally
          if fdmtSource.BookmarkValid(LBookmark) then
            fdmtSource.Bookmark := LBookmark;
        end;
      finally
        fdmtSource.EnableControls();
      end;
    end);
  Result := LResult;
end;

procedure TDebuggerDataSetContainer.RequestStackTraces(
  const ABaseProtocolClient: TBaseProtocolClient; const AThreadId: integer;
  const AOpenFileOfLastFrame: boolean);
begin
  var LStackTraceRequest := TStackTraceRequest.Create();
  try
    var LArgs := LStackTraceRequest.Arguments;
    LArgs.ThreadId := AThreadId;
    LArgs.StartFrame := 0;
    LArgs.Levels := 0;

    var LFormat := LArgs.Format;
    LFormat.Parameters := true;
    LFormat.ParameterTypes := true;
    LFormat.ParameterNames := true;
    LFormat.ParameterValues := true;
    LFormat.Lines := true;
    LFormat.Module := true;
    LFormat.IncludeAll := true;

    ABaseProtocolClient.SendRequest<TStackTraceResponse>(
      LStackTraceRequest,
      procedure(const AArg: TStackTraceResponse)
      begin
        System.Classes.TThread.Synchronize(System.Classes.TThread.Current,
          procedure()
          begin
            fdmtStackTrace.DisableControls();
            try
              fdmtStackTrace.EmptyDataSet();
              fdmtScope.EmptyDataSet();
              fdmtVariable.EmptyDataSet();
              fdmtActiveSource.EmptyDataSet();

              if AOpenFileOfLastFrame and Assigned(AArg.Body.StackFrames) then begin
                var LStackFrame := AArg.Body.StackFrames[0];
                var LSourcePath := GetLocalSourcePathByName(
                  TPath.GetFileName(LStackFrame.Source.Path));
                fdmtActiveSource.AppendRecord([
                  AThreadId,
                  LSourcePath,
                  LStackFrame.Line,
                  true]);
              end;

              for var LStackFrame in AArg.Body.StackFrames do begin
                fdmtStackTrace.AppendRecord([
                  LStackFrame.Id,
                  AThreadId,
                  LStackFrame.Name,
                  LStackFrame.Line,
                  LStackFrame.Name,
                  LStackFrame.Source.Origin,
                    '(' + LStackFrame.Line.ToString() + ') '
                    + TPath.GetFileName(LStackFrame.Source.Path)
                    + '.'
                    + GetCodeAtLine(
                        TPath.GetFileName(LStackFrame.Source.Path),
                        LStackFrame.Line)]);
              end;

              fdmtStackTrace.First();
            finally
              fdmtStackTrace.EnableControls();
            end;

            for var LStackFrame in AArg.Body.StackFrames do
              RequestScopes(ABaseProtocolClient, LStackFrame.Id);
          end);
      end,
      procedure(const AArg: TResponseMessage)
      begin

      end);
  finally
    LStackTraceRequest.Free();
  end;
end;

procedure TDebuggerDataSetContainer.RequestVariables(
  const ABaseProtocolClient: TBaseProtocolClient;
  const AFrameId, AVariablesReference: integer);
begin
  var LVariablesRequest := TVariablesRequest.Create();
  try
    LVariablesRequest.Arguments.VariablesReference := AVariablesReference;

    ABaseProtocolClient.SendRequest<TVariablesResponse>(
      LVariablesRequest,
      procedure(const AArg: TVariablesResponse)
      begin
        System.Classes.TThread.Synchronize(System.Classes.TThread.Current,
          procedure()
          begin
            fdmtVariable.DisableControls();
            try
              for var LVariable in AArg.Body.Variables do begin
                fdmtVariable.AppendRecord([
                  AFrameId,
                  AVariablesReference,
                  LVariable.Name,
                  LVariable.EvaluateName,
                  LVariable.&Type,
                  LVariable.Value]);
              end;
            finally
              fdmtVariable.EnableControls();
            end;
            fdmtStackTrace.Last();
            fdmtStackTrace.First();
          end);
      end,
      procedure(const AArg: TResponseMessage)
      begin

      end);
  finally
    LVariablesRequest.Free();
  end;
end;

procedure TDebuggerDataSetContainer.RequestScopes(
  const ABaseProtocolClient: TBaseProtocolClient;
  const AFrameId: integer);
begin
  var LScopesRequest := TScopesRequest.Create();
  try
    LScopesRequest.Arguments.FrameId := AFrameId;

    ABaseProtocolClient.SendRequest<TScopesResponse>(
      LScopesRequest,
      procedure(const AArg: TScopesResponse)
      begin
        System.Classes.TThread.Synchronize(System.Classes.TThread.Current,
          procedure()
          begin
            fdmtScope.DisableControls();
            try
              for var LScope in AArg.Body.Scopes do begin
                fdmtScope.AppendRecord([
                  AFrameId,
                  LScope.Name,
                  LScope.VariablesReference]);
              end;
            finally
              fdmtScope.EnableControls();
            end;
          end);

        for var LScope in AArg.Body.Scopes do
          RequestVariables(ABaseProtocolClient, AFrameId, LScope.VariablesReference);
      end,
      procedure(const AArg: TResponseMessage)
      begin

      end);
  finally
    LScopesRequest.Free();
  end;
end;

procedure TDebuggerDataSetContainer.AddSource(const ASourceName, ALocalPath,
  ARemotePath: string);
begin
  System.Classes.TThread.Queue(System.Classes.TThread.Current,
    procedure
    begin
      if not fdmtSource.Locate('source_name', ASourceName, [loCaseInsensitive]) then
        fdmtSource.AppendRecord([ASourceName, ALocalPath, ARemotePath]);
    end);
end;

procedure TDebuggerDataSetContainer.RemoveSource(const ASourceName: string);
begin
  System.Classes.TThread.Queue(System.Classes.TThread.Current,
    procedure
    begin
      if fdmtSource.Locate('source_name', ASourceName, [loCaseInsensitive]) then begin
        ClearBreakpoints(ASourceName);
        fdmtSource.Delete();
      end;
    end);
end;

procedure TDebuggerDataSetContainer.AddBreakpoint(const ASourceName: string;
  const ALine: integer);
begin
  System.Classes.TThread.Synchronize(System.Classes.TThread.Current,
    procedure
    begin
      if fdmtSource.Locate('source_name', ASourceName, [loCaseInsensitive]) then begin
        var LExists := fdmtBreakpoint.Locate('breakpoint_line', ALine);
        if not LExists then
          fdmtBreakpoint.AppendRecord([-1, ASourceName, ALine]);
      end;
    end);
end;

procedure TDebuggerDataSetContainer.RemoveBreakpoint(const ASourceName: string;
  const ALine: integer);
begin
  System.Classes.TThread.Synchronize(System.Classes.TThread.Current,
    procedure
    begin
      if fdmtSource.Locate('source_name', ASourceName, [loCaseInsensitive]) then begin
        if fdmtBreakpoint.Locate('breakpoint_line', ALine) then
          fdmtBreakpoint.Delete();
      end;
    end);
end;

procedure TDebuggerDataSetContainer.ClearBreakpoints(const ASourceName: string);
begin
  System.Classes.TThread.Synchronize(System.Classes.TThread.Current,
    procedure
    begin
      fdmtBreakpoint.Filter := 'breakpoint_source_name=' + ASourceName.QuotedString();
      fdmtBreakpoint.Filtered := true;
      try
        fdmtBreakpoint.First();
        while not fdmtBreakpoint.Eof do
          fdmtBreakpoint.Delete();
      finally
        fdmtBreakpoint.Filtered := false;
      end;
    end);
end;

procedure TDebuggerDataSetContainer.ClearSourceLineAndIndicator;
begin
  System.Classes.TThread.Synchronize(System.Classes.TThread.Current,
    procedure()
    begin
      fdmtActiveSource.Edit();
      fdmtActiveSourceactive_source_line.AsInteger := 0;
      fdmtActiveSourceactive_source_line_indicator.AsBoolean := false;
      fdmtActiveSource.Post();
    end);
end;

end.
