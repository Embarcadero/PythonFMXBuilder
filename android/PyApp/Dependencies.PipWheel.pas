unit Dependencies.PipWheel;

interface

uses
  Dependencies,
  PyEnvironment,
  PyEnvironment.AddOn.EnsurePip;

type
  TPipWheelInstallStrategy = class(TBaseInstallDependency, IInstallDependency)
  public
    function CanHandle(AFilePath: string): boolean;
    function IsInstalled(const AModuleName, AFilePath: string): boolean;
    function Install(const AModuleName, AFilePath: string): boolean;
  end;

implementation

uses
  System.IOUtils,
  PyTools.ExecCmd;

{ TPipWheelInstallStrategy }

function TPipWheelInstallStrategy.CanHandle(AFilePath: string): boolean;
begin
  Result := TPath.GetExtension(AFilePath) = '.whl';
end;

function TPipWheelInstallStrategy.IsInstalled(const AModuleName, AFilePath: string): boolean;
begin
  Result := PkgUtilCheckInstalled(AModuleName, AFilePath);
end;

function TPipWheelInstallStrategy.Install(const AModuleName, AFilePath: string): boolean;
begin
  Result := ExecCmd(['-m', 'pip', 'install', AFilePath]) = EXIT_SUCCESS;
end;

end.
