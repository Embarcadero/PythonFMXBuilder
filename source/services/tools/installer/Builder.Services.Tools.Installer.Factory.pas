unit Builder.Services.Tools.Installer.Factory;

interface

uses
  Builder.Types,
  Builder.Services.Tools.Installer,
  Builder.Services.Tools.Installer.JDK,
  Builder.Services.Tools.Installer.SDK;

type
  TToolsInstallerFactory = class
  public
    class function Resolve(const AToolInfo: PToolInfo): TToolInstallerClass;
    class function CreateInstance(const AToolInfo: PToolInfo): TToolInstaller;
  end;

implementation

{ TToolsInstallerFactory }

class function TToolsInstallerFactory.Resolve(const AToolInfo: PToolInfo): TToolInstallerClass;
begin
  if (AToolInfo.Installer = 'TJDKToolInstaller') then
    Result := TJDKToolInstaller
  else if (AToolInfo.Installer = 'TSDKToolInstaller') then
    Result := TSDKToolInstaller
  else
    Result := nil;
end;

class function TToolsInstallerFactory.CreateInstance(
  const AToolInfo: PToolInfo): TToolInstaller;
begin
  var LClass := Resolve(AToolInfo);
  if not Assigned(LClass) then
    Exit(nil);

  Result := LClass.Create(AToolInfo);
end;

end.
