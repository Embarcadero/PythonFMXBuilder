unit Builder.Services.Tools.Installer.JDK;

interface

uses
  Builder.Types,
  Builder.Services.Tools.Installer.DownloadAndExtract;

type
  TJDKToolInstaller = class(TDownloadAndExtractToolInstaller)
  protected
    procedure PersistInstallation(const ATransientFolder, APersistentFolder: string); override;
  public
    class function IsInstalled(const AToolInfo: PToolInfo): boolean; override;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  Builder.Paths;

{ TJDKToolInstaller }

class function TJDKToolInstaller.IsInstalled(
  const AToolInfo: PToolInfo): boolean;
begin
  var LToolDirectory := TBuilderPaths.GetToolInstallationFolder(AToolInfo^);

  Result := inherited IsInstalled(AToolInfo);
  // Check tools
  Result := Result
    and not TAndroidToolsPathLocator.FindToolPath(LToolDirectory,
      {$IFDEF POSIX}'keytool'{$ELSE}'keytool.exe'{$ENDIF}).IsEmpty()
    and not TAndroidToolsPathLocator.FindToolPath(LToolDirectory,
      {$IFDEF POSIX}'jarsigner'{$ELSE}'jarsigner.exe'{$ENDIF}).IsEmpty();
end;

procedure TJDKToolInstaller.PersistInstallation(const ATransientFolder,
  APersistentFolder: string);
begin
  if not Assigned(TDirectory.GetDirectories(ATransientFolder)) then
    raise Exception.Create('Invalid installation tree.');

  TDirectory.Move(
    {$IFDEF MSWINDOWS}'\\?\' + {$ENDIF MSWINDOWS} TDirectory.GetDirectories(ATransientFolder)[0],
    {$IFDEF MSWINDOWS}'\\?\' + {$ENDIF MSWINDOWS} APersistentFolder);
end;

end.
