unit Builder.Services.Tools.Installer.DownloadAndExtract;

interface

uses
  System.Zip,
  System.Classes,
  System.Net.HttpClient,
  Builder.Types,
  Builder.Services.Tools.Installer;

type
  TZipProgressCallback = reference to procedure(AFileName: string; Header: TZipHeader; Position: Int64);
  
  TDownloadAndExtractToolInstaller = class(TToolInstaller)
  private
    procedure DownloadFile(const AUrl, ADestinationPath: string;
      const AReceiveDataCallback: TReceiveDataCallback);
    procedure ExtractFile(const AZipFilePath, ADestinationFolder: string;
      const AProgressCallback: TZipProgressCallback);
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
  System.SysUtils,
  System.IOUtils,
  Builder.Paths,
  Builder.Exception;

type
  TZipFile = class(System.Zip.TZipFile)
  private
    FProgressCallback: TZipProgressCallback;
    procedure DoOnProgress(Sender: TObject; FileName: string; Header: TZipHeader; Position: Int64);
  public
    class procedure ExtractZipFile(const ZipFileName: string; const Path: string; ZipProgress: TZipProgressCallback = nil);
  end;

{ TDownloadAndExtractToolInstaller }

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
  var LToolBundle := TPath.Combine(LToolDirectoryTmp, Format('%s-%s.zip', [
    ToolInfo^.Name, ToolInfo^.Version]));

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
        procedure(AFileName: string; Header: TZipHeader; Position: Int64) begin
          if Assigned(AProgress) then
            AProgress(ToolInfo, 'Extracting files', Header.GetUncompressedSize64(), Position);

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

    var LStream := TFileStream.Create(ADestinationPath, fmCreate or fmOpenReadWrite, fmShareExclusive);
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
  ADestinationFolder: string; const AProgressCallback: TZipProgressCallback);
begin
  {$IFDEF MSWINDOWS}
  // Avoid path issues
  if not TPath.IsUNCPath(ADestinationFolder) then
    TZipFile.ExtractZipFile(AZipFilePath, '\\?\' + ADestinationFolder, AProgressCallback)
  else
    TZipFile.ExtractZipFile(AZipFilePath, ADestinationFolder, AProgressCallback);
  {$ELSE}
  TZipFile.ExtractZipFile(AZipFilePath, ADestinationFolder, AProgressCallback);
  {$ENDIF MSWINDOWS}
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
    FProgressCallback(FileName, Header, Position);
end;

class procedure TZipFile.ExtractZipFile(const ZipFileName, Path: string;
  ZipProgress: TZipProgressCallback);
begin
  var LZip := TZipFile.Create();
  try
    LZip.FProgressCallback := ZipProgress;
    inherited ExtractZipFile(ZipFileName, Path, LZip.DoOnProgress);
  finally
    LZip.Free();
  end;
end;

end.
