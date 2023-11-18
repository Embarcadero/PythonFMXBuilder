unit Builder.Services.Tools.Installer;

interface

uses
  System.Classes,
  Builder.Types;

type
  TToolInstallerClass = class of TToolInstaller;

  TDependenciesResolutionStrategy = reference to procedure(const AToolInfo: PToolInfo);

  TToolInstaller = class
  private
    FToolInfo: PToolInfo;
  protected
    procedure DoInstall(
      const ADependenciesResolution: TDependenciesResolutionStrategy;
      const AProgress: TToolInstallationProgress;
      var AAbort: boolean); virtual; abstract;
  public
    constructor Create(const AToolInfo: PToolInfo);

    class function IsInstalled(const AToolInfo: PToolInfo): boolean; virtual;
    procedure Install(
      const ADependenciesResolution: TDependenciesResolutionStrategy;
      var AAbort: boolean;
      const AProgress: TToolInstallationProgress = nil);

    property ToolInfo: PToolInfo read FToolInfo;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  Builder.Paths;

{ TToolInstaller }

constructor TToolInstaller.Create(const AToolInfo: PToolInfo);
begin
  inherited Create();
  FToolInfo := AToolInfo;
end;

class function TToolInstaller.IsInstalled(const AToolInfo: PToolInfo): boolean;
begin
  Result := TDirectory.Exists(TBuilderPaths.GetToolInstallationFolder(AToolInfo^));
end;

procedure TToolInstaller.Install(
  const ADependenciesResolution: TDependenciesResolutionStrategy;
  var AAbort: boolean;
  const AProgress: TToolInstallationProgress);
begin
  if Assigned(AProgress) then
    AProgress(ToolInfo, 'Starting', 1, 0);
    
  try
    DoInstall(ADependenciesResolution, AProgress, AAbort);
  except
    on E: Exception do begin
      if Assigned(AProgress) then
        if AAbort then
          AProgress(ToolInfo, 'Cancelled', 1, 1)
        else
          AProgress(ToolInfo, 'Failed', 1, 1);
      raise;
    end;
  end;

  if Assigned(AProgress) then
    AProgress(ToolInfo, 'Done', 1, 1);
end;

end.
