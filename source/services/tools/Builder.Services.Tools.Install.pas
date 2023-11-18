unit Builder.Services.Tools.Install;

interface

uses
  System.Types,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  Builder.Types,
  Builder.Services;

type
  TToolInstallService = class(TInterfacedObject, IInstallItServices)
  public type
    TWaitList = TArray<TPair<PToolInfo, IAsyncResult>>;
    TWaitListCallback = reference to procedure(const AWaitList: TWaitList;
      const ACurrent: integer);
  private
    FInstalling: TDictionary<PToolInfo, IAsyncResult>;

    procedure CheckInstalling(const ATool: PToolInfo);

    // Dependencies resolution strategy
    function BuildWaitList(const ATool: PToolInfo): TWaitList;
    procedure WaitForDependencies(const ATool: PToolInfo;
      const AWaitList: TWaitList; const AWaitListCallback: TWaitListCallback);
    procedure CheckDependencies(const ATool: PToolInfo);
    procedure ResolveDependencies(const ATool: PToolInfo;
      const AWaitList: TWaitList; const AProgress: TToolInstallationProgress;
      var AAbort: boolean);
    // Internal install
    function Install(const ATool: PToolInfo;
      const AProgress: TToolInstallationProgress = nil;
      const ACallback: TAsyncCallback = nil): IAsyncResult;
  public
    constructor Create();
    destructor Destroy(); override;

    function GetTools(): TArray<PToolInfo>;
    function GetMissingTools(): TArray<PToolInfo>;

    function IsInstalled(const ATool: PToolInfo): boolean;

    function BeginInstall(const ATool: PToolInfo;
      const AProgress: TToolInstallationProgress = nil;
      const ACompletition: TAsyncCallback = nil): IAsyncResult;
    procedure EndInstall(const AAsyncResult: IAsyncResult);
  end;

implementation

uses
  System.IOutils,
  System.Threading,
  System.SyncObjs,
  Builder.Paths,
  Builder.Exception,
  Builder.Services.Tools.Installer.Factory;

type
  TToolsInfo = array[0..1] of TToolInfo;

  TToolInstallAsyncResult = class(TBaseAsyncResult)
  private
    FTask: TProc;
    FCancellation: TProc;
    FCallback: TAsyncCallback;
  protected
    procedure AsyncDispatch; override;
    procedure Complete; override;
    procedure Schedule; override;

    function DoCancel: Boolean; override;
  public
    constructor Create(const AContext: TObject;
      const ATask, ACancellationCallback: TProc;
      const ACallback: TAsyncCallback);
  end;

const
  INDEX_JDK_21_35 = 0;
  INDEX_SDK_32 = 1;

  {$IFDEF WIN32}
  {$I required_tools_win_x86.inc}
  {$ENDIF WIN32}
  {$IFDEF WIN64}
  {$I required_tools_win_x64.inc}
  {$ENDIF WIN64}

  {$IFDEF MACOS}
  {$IFDEF CPUARM64}
  {$I required_tools_mac_arm64.inc}
  {$ELSE}
  {$I required_tools_mac_x86_x64.inc}
  {$ENDIF CPUARM64}
  {$ENDIF MACOS}

  {$IFDEF LINUX}
  {$I required_tools_linux_x86_x64.inc}
  {$ENDIF LINUX}

{ TToolInstallService }

constructor TToolInstallService.Create;
begin
  inherited Create();
  FInstalling := TDictionary<PToolInfo, IAsyncResult>.Create();
end;

destructor TToolInstallService.Destroy;
begin
  FInstalling.Free();
  inherited;
end;

procedure TToolInstallService.CheckInstalling(const ATool: PToolInfo);
begin
  if FInstalling.ContainsKey(ATool) then
    if FInstalling.Items[ATool].IsCompleted or FInstalling.Items[ATool].IsCancelled then
      FInstalling.Remove(ATool)
    else
      raise EInstallInProgress.CreateFmt('"%s" is currently being installed.', [ATool^.Description]);
end;

function TToolInstallService.BuildWaitList(
  const ATool: PToolInfo): TWaitList;
begin
  Result := [];
  for var LRequired in ATool.Requires do
    if FInstalling.ContainsKey(LRequired) then
      Result := Result + [
        TPair<PToolInfo, IAsyncResult>.Create(
          LRequired, FInstalling.Items[LRequired])];
end;

procedure TToolInstallService.WaitForDependencies(
  const ATool: PToolInfo; const AWaitList: TWaitList;
  const AWaitListCallback: TWaitListCallback);
begin
  for var I := Low(AWaitList) to High(AWaitList) do begin
    // Notify we are waiting for this tool
    AWaitListCallback(AWaitList, I);
    // Wait for completition
    EndInstall(AWaitList[I].Value);
  end;
end;

procedure TToolInstallService.CheckDependencies(
  const ATool: PToolInfo);
begin
  for var LMissingTool in GetMissingTools() do
    for var LDependency in ATool.Requires do
      if LDependency = LMissingTool then
        raise EMissingTool.CreateFmt('"%s" requires "%s" to be installed.', [ATool.Name, LDependency.Name]);
end;

function TToolInstallService.IsInstalled(const ATool: PToolInfo): boolean;
begin
  var LInstaller := TToolsInstallerFactory.Resolve(ATool);

  if not Assigned(LInstaller) then
    Exit(false);

  Result := LInstaller.IsInstalled(ATool);
end;

procedure TToolInstallService.ResolveDependencies(const ATool: PToolInfo;
  const AWaitList: TWaitList; const AProgress: TToolInstallationProgress;
  var AAbort: boolean);
begin
  var LAbort: PBoolean := @AAbort;
  // Wait for all dependencies to complete installation
  WaitForDependencies(ATool, AWaitList,
    procedure(const AWaitList: TWaitList; const ACurrent: integer) begin
      if Assigned(AProgress) then
        AProgress(ATool,
          Format('Waiting for [%s] to complete', [AWaitList[ACurrent].Key.Description]),
          Length(AWaitList), Succ(ACurrent));

      if LAbort^ then
        raise EOperationCancelled.Create('The operation has been cancelled.');
    end);

  // Check if all required tools are installed
  CheckDependencies(ATool);
end;

function TToolInstallService.GetTools: TArray<PToolInfo>;
begin
  Result := nil;
  for var I := Low(REQUIRED_TOOLS) to High(REQUIRED_TOOLS) do
    Result := Result + [@REQUIRED_TOOLS[I]];
end;

function TToolInstallService.GetMissingTools: TArray<PToolInfo>;
begin
  Result := nil;
  for var I := Low(REQUIRED_TOOLS) to High(REQUIRED_TOOLS) do
    if not IsInstalled(@REQUIRED_TOOLS[I]) then
      Result := Result + [@REQUIRED_TOOLS[I]];
end;

function TToolInstallService.Install(const ATool: PToolInfo;
  const AProgress: TToolInstallationProgress;
  const ACallback: TAsyncCallback): IAsyncResult;
begin
  // Check for installation under progress
  CheckInstalling(ATool);

  // Create the Android Tools directory
  var LToolsBaseFolder := TBuilderPaths.GetToolsInstallationBaseFolder();
  if not TDirectory.Exists(LToolsBaseFolder) then
    TDirectory.CreateDirectory(LToolsBaseFolder);

  // Build the wait list for the installing tool
  var LWaitList := BuildWaitList(ATool);

  // Check for undefined installation class
  if not Assigned(TToolsInstallerFactory.Resolve(ATool)) then
    raise EUnknownTool.CreateFmt('Unknown tool "%s" can''t be installed.', [
      ATool.Description]);

  // Instantiate the installer
  var LAbort := false;
  Result := TToolInstallAsyncResult.Create(Self,
    procedure() begin
      var LInstaller := TToolsInstallerFactory.CreateInstance(ATool);
      try
        LInstaller.Install(
          procedure(const AToolInfo: PToolInfo) begin
            ResolveDependencies(AToolInfo, LWaitList, AProgress, LAbort)
        end, LAbort, AProgress);
      finally
        LInstaller.Free();
      end;
    end,
    // Cancellation callback
    procedure() begin
      LAbort := true;
    end,
    // Completition callback
    ACallback).Invoke();

  FInstalling.Add(ATool, Result);
end;

function TToolInstallService.BeginInstall(const ATool: PToolInfo;
  const AProgress: TToolInstallationProgress;
  const ACompletition: TAsyncCallback): IAsyncResult;
begin
  Result := Install(ATool, AProgress, ACompletition);
end;

procedure TToolInstallService.EndInstall(const AAsyncResult: IAsyncResult);
begin
  (AAsyncResult as TBaseAsyncResult).WaitForCompletion;
end;

{ TToolInstallAsyncResult }

constructor TToolInstallAsyncResult.Create(const AContext: TObject;
  const ATask, ACancellationCallback: TProc; const ACallback: TAsyncCallback);
begin
  inherited Create(AContext);
  FTask := ATask;
  FCancellation := ACancellationCallback;
  FCallback := ACallback;
end;

procedure TToolInstallAsyncResult.AsyncDispatch;
begin
  if Assigned(FTask) then
    FTask();
end;

procedure TToolInstallAsyncResult.Complete;
begin
  if Assigned(FCallback) then
    FCallback(Self);
  inherited;
end;

function TToolInstallAsyncResult.DoCancel: Boolean;
begin
  inherited DoCancel();
  if Assigned(FCancellation) then
    FCancellation();
  Result := true;
end;

procedure TToolInstallAsyncResult.Schedule;
begin
  TTask.Run(DoAsyncDispatch);
end;

end.
