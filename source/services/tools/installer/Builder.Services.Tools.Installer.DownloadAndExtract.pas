unit Builder.Services.Tools.Installer.DownloadAndExtract;

interface

uses
  System.Zip,
  System.Classes,
  System.Net.HttpClient,
  Builder.Types,
  Builder.Services.Tools.Installer;

type
  TExtractProgressCallback = reference to procedure(const AFileName: string; const ATotal, APosition: Int64);

  TDownloadAndExtractToolInstaller = class(TToolInstaller)
  private
    function GetOriginalFileName(const AUrl: string): string;
    function BuildFileName(): string;
  private
    procedure DownloadFile(const AUrl, ADestinationPath: string;
      const AReceiveDataCallback: TReceiveDataCallback);
    procedure ExtractFile(const AZipFilePath, ADestinationFolder: string;
      const AProgressCallback: TExtractProgressCallback);
  protected
    //Defines whether we should validade dependencies, before of aftert download and extract
    function CheckDependenciesBeforeDownloadAndExtract(): boolean; virtual;    
    // Moves from temp folder to persistent
    procedure PersistInstallation(const ATransientFolder, APersistentFolder: string); virtual;
    // Install operation after donwloading and extraction
    procedure InternalInstall(const AInstallationDirectory: string;
      const AProgress: TToolInstallationProgress; var AAbort: boolean); virtual;
    procedure DoInstall(
      const ADependenciesResolution: TDependenciesResolutionStrategy;
      const AProgress: TToolInstallationProgress;
      var AAbort: boolean); override;
  end;

implementation

uses
  System.IOUtils,
  System.SysUtils,
  System.StrUtils,
  System.RegularExpressions,
  Builder.Paths,
  Builder.Exception,
  PyTools.ExecCmd;

type
  TZipFile = class(System.Zip.TZipFile)
  private
    FProgressCallback: TExtractProgressCallback;
    procedure DoOnProgress(Sender: TObject; FileName: string; Header: TZipHeader; Position: Int64);
  public
    class procedure Extract(const AZipFileName: string; const APath: string; AZipProgress: TExtractProgressCallback = nil);
  end;

  TTarFile = class
  public
    class procedure Extract(const AZipFileName: string; const APath: string; AUnTarProgress: TExtractProgressCallback = nil);
  end;

  TExtractFile = class
  public
    class procedure Extract(const AZipFileName: string; const APath: string; AExtractProgress: TExtractProgressCallback = nil);
  end;

{ TDownloadAndExtractToolInstaller }

function TDownloadAndExtractToolInstaller.BuildFileName(): string;
begin
  // Try to get the original file name sent by the provider
  Result := GetOriginalFileName(ToolInfo^.Url);
  // Try to get the original file name from url
  if Result.IsEmpty then
    Result := ToolInfo^.Url.Substring(ToolInfo^.Url.LastDelimiter('/') + 1);
  // Do we have a valid file name?
  if TPath.GetExtension(Result).IsEmpty() then
    Result := String.Empty;
  // Create a new name
  if Result.IsEmpty then
    Result := Format('%s-%s.%s', [ToolInfo^.Name, ToolInfo^.Version, 'zip']);
end;

function TDownloadAndExtractToolInstaller.CheckDependenciesBeforeDownloadAndExtract: boolean;
begin
  Result := false;
end;

procedure TDownloadAndExtractToolInstaller.DoInstall(
  const ADependenciesResolution: TDependenciesResolutionStrategy;
  const AProgress: TToolInstallationProgress;
  var AAbort: boolean);
begin
  var LAbort: PBoolean := @AAbort;
  var LToolDirectory := TBuilderPaths.GetToolInstallationFolder(ToolInfo^);
  var LToolDirectoryTmp := LToolDirectory + '.installer';
  var LCheckDependenciesFirst := CheckDependenciesBeforeDownloadAndExtract();
  var LToolBundle := TPath.Combine(LToolDirectoryTmp, BuildFileName());

  // Check dependencies before ops
  if LCheckDependenciesFirst then
    ADependenciesResolution(ToolInfo);

  if not TDirectory.Exists(LToolDirectoryTmp) then
    TDirectory.CreateDirectory(LToolDirectoryTmp);

  try
    try
      // Download content
      DownloadFile(ToolInfo^.Url, LToolBundle,
        procedure(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var AAbort: Boolean)
        begin
          if Assigned(AProgress) then
            AProgress(ToolInfo, 'Downloading content', AContentLength, AReadCount);

          AAbort := AAbort or LAbort^;
        end);

      if AAbort then
        raise EOperationCancelled.Create('The operation has been cancelled.');

      // Extract files
      ExtractFile(LToolBundle, LToolDirectoryTmp,
        procedure(const AFileName: string; const ATotal, APosition: Int64) begin
          if Assigned(AProgress) then
            AProgress(ToolInfo, 'Extracting files', ATotal, APosition);

          if LAbort^ then
            raise EOperationCancelled.Create('The operation has been cancelled.');
        end);
    finally
      if TFile.Exists(LToolBundle) then
        TFile.Delete(LToolBundle);
    end;

    // Check dependencies after ops
    if not LCheckDependenciesFirst then
      ADependenciesResolution(ToolInfo);

    // Execute installation procedures after downloading and extracting
    InternalInstall(LToolDirectoryTmp, AProgress, AAbort);

    if Assigned(AProgress) then
      AProgress(ToolInfo, 'Getting ready', 2, 1);
      
    // Move from transient to persistent folder
    PersistInstallation(LToolDirectoryTmp, LToolDirectory);
  finally
    // Delete transient folder
    if TDirectory.Exists(LToolDirectoryTmp) then
      TDirectory.Delete(LToolDirectoryTmp, true);
  end;
end;

procedure TDownloadAndExtractToolInstaller.DownloadFile(const AUrl,
  ADestinationPath: string; const AReceiveDataCallback: TReceiveDataCallback);
begin
  var LNetClient := THTTPClient.Create();
  try
    LNetClient.ReceiveDataCallBack := AReceiveDataCallback;

    var LStream := TFileStream.Create(ADestinationPath, fmCreate or fmOpenReadWrite);
    try
      var LResponse := LNetClient.Get(AUrl, LStream);

      if LResponse.StatusCode <> 200 then
        if not LResponse.StatusText.IsEmpty() then
          raise EDownloadToolFailed.Create('Download failed with response text: '
            + sLINEBREAK
            + sLINEBREAK
            + LResponse.StatusText)
        else
          raise EDownloadToolFailed.Create('Download failed. Try again.');
    finally
      LStream.Free();
    end;
  finally
    LNetClient.Free();
  end;
end;

procedure TDownloadAndExtractToolInstaller.ExtractFile(const AZipFilePath,
  ADestinationFolder: string; const AProgressCallback: TExtractProgressCallback);
begin
  TExtractFile.Extract(AZipFilePath, ADestinationFolder, AProgressCallback);
end;

function TDownloadAndExtractToolInstaller.GetOriginalFileName(
  const AUrl: string): string;
begin
  var LNetClient := THTTPClient.Create();
  try
    var LResp := LNetClient.Head(AUrl);
    Result := LResp.HeaderValue['Content-Disposition'];
    var LValues := Result.Split(['filename=']);
    if Length(LValues) = 2 then
      Result := LValues[1]
    else
      Result := String.Empty;
  finally
    LNetClient.Free();
  end;
end;

procedure TDownloadAndExtractToolInstaller.InternalInstall(const AInstallationDirectory: string;
  const AProgress: TToolInstallationProgress; var AAbort: boolean);
begin
  //
end;

procedure TDownloadAndExtractToolInstaller.PersistInstallation(
  const ATransientFolder, APersistentFolder: string);
begin
  TDirectory.Move(
    {$IFDEF MSWINDOWS}'\\?\' + {$ENDIF MSWINDOWS} ATransientFolder,
    {$IFDEF MSWINDOWS}'\\?\' + {$ENDIF MSWINDOWS} APersistentFolder);
end;

{ TZipFile }

procedure TZipFile.DoOnProgress(Sender: TObject; FileName: string;
  Header: TZipHeader; Position: Int64);
begin
  if Assigned(FProgressCallback) then
    FProgressCallback(FileName, Header.UncompressedSize, Position);
end;

class procedure TZipFile.Extract(const AZipFileName, APath: string;
  AZipProgress: TExtractProgressCallback);
begin
  var LZip := TZipFile.Create();
  try
    LZip.FProgressCallback := AZipProgress;
    inherited ExtractZipFile(AZipFileName, APath, LZip.DoOnProgress);
  finally
    LZip.Free();
  end;
end;

{ TTarFile }

class procedure TTarFile.Extract(const AZipFileName, APath: string;
  AUnTarProgress: TExtractProgressCallback);
begin
  var LOutput := String.Empty;
  if TExecCmdService
    .Cmd('/usr/bin/tar', ['tar', '-tzf', AZipFileName])
    .Run(LOutput)
    .Wait <> EXIT_SUCCESS
  then
    raise Exception.Create(LOutput);

  var LTotal := TRegEx.Matches(LOutput, sLINEBREAK).Count;

  var LCmd := TExecCmdService
    .Cmd('/usr/bin/tar', ['tar', '-xzvf', AZipFileName, '-C', APath])
    .Run([TRedirect.stdout, TRedirect.stderr]);

  var LStrings := TStringList.Create();
  try
    while LCmd.IsAlive do begin
      LStrings.Text := LStrings.Text + LCmd.StdOut.ReadNext() + LCmd.StdErr.ReadAll();

      if Assigned(AUnTarProgress) and (LStrings.Count > 0) then
        AUnTarProgress(LStrings.Strings[LStrings.Count - 1], LTotal, LStrings.Count);
    end;

    LStrings.Text := LStrings.Text + LCmd.StdOut.ReadAll() + LCmd.StdErr.ReadAll();

    if Assigned(AUnTarProgress) and (LStrings.Count > 0) then
      AUnTarProgress(LStrings.Strings[LStrings.Count - 1], LTotal, LStrings.Count);

    if (LCmd.Wait <> EXIT_SUCCESS) then
      raise Exception.Create(LCmd.StdErr.ReadAll());
  finally
    LStrings.Free();
  end;
end;

{ TExtractFile }

class procedure TExtractFile.Extract(const AZipFileName, APath: string;
  AExtractProgress: TExtractProgressCallback);
begin
  if TPath.GetExtension(AZipFileName).EndsWith('zip') then begin
    {$IFDEF MSWINDOWS}
    // Avoid path issues
    if not TPath.IsUNCPath(APath) then
      TZipFile.Extract(AZipFileName, '\\?\' + APath, AExtractProgress)
    else
      TZipFile.Extract(AZipFileName, APath, AExtractProgress);
    {$ELSE}
    TZipFile.Extract(AZipFileName, APath, AExtractProgress);
    {$ENDIF MSWINDOWS}
  end else if TPath.GetFileName(AZipFileName).Contains('tar') then begin
    TTarFile.Extract(AZipFileName, APath, AExtractProgress);
  end;
end;

end.
