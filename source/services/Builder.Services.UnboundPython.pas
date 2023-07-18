unit Builder.Services.UnboundPython;

interface

uses
  System.SysUtils,
  Builder.Types,
  Builder.Services,
  Builder.Model.Environment;

type
  TUnboundPythonServices = class(TInterfacedObject, IUnboundPythonServices)
  private
    FAdbServices: IAdbServices;
    FEnvironmentServices: IEnvironmentServices;
    FEnvironmentModel: TEnvironmentModel;
  public
    constructor Create();
    destructor Destroy(); override;

    procedure Make(const APythonVersion: TPythonVersion;
      const AArchitecture: TArchitecture);
    procedure Remove(const APythonVersion: TPythonVersion;
      const AArchitecture: TArchitecture);
    function Exists(const APythonVersion: TPythonVersion;
      const AArchitecture: TArchitecture): boolean;
    procedure Run(const APythonVersion: TPythonVersion;
      const AArchitecture: TArchitecture; const ADebugger: TDebugger;
      const ABuildConfiguration: TBuildConfiguration);
  end;

implementation

uses
  System.IOUtils,
  Builder.Paths,
  Builder.Exception,
  Builder.Services.Factory;

{ TUnboundPythonServices }

constructor TUnboundPythonServices.Create;
begin
  inherited;
  FAdbServices := TServiceSimpleFactory.CreateAdb();
  FEnvironmentServices := TServiceSimpleFactory.CreateEnvironment();
  FEnvironmentModel := FEnvironmentServices.GetActiveEnvironment();
end;

destructor TUnboundPythonServices.Destroy;
begin
  FEnvironmentModel.Free();
  inherited;
end;

procedure TUnboundPythonServices.Make(const APythonVersion: TPythonVersion;
  const AArchitecture: TArchitecture);
var
  LLocalZipFile: string;
  LRemoteZipFile: string;
  LRemotePythonHome: string;
begin
  LLocalZipFile := TBuilderPaths.GetPythonZipFile(APythonVersion, AArchitecture);
  LRemoteZipFile := TBuilderUnboundPaths.GetPythonBasePath()
    + '/'
    + TPath.GetFileName(LLocalZipFile);
  LRemotePythonHome := TBuilderUnboundPaths.GetPythonHomePath(APythonVersion, AArchitecture);

  FAdbServices.SendFile(LLocalZipFile, LRemoteZipFile);
  try
    FAdbServices.CreateDirectory(LRemotePythonHome);
    FAdbServices.ExtractZip(LRemoteZipFile, LRemotePythonHome);
  finally
    FAdbServices.RemoveFile(LRemoteZipFile);
  end;
end;

procedure TUnboundPythonServices.Remove(const APythonVersion: TPythonVersion;
  const AArchitecture: TArchitecture);
var
  LRemotePythonHome: string;
begin
  LRemotePythonHome := TBuilderUnboundPaths.GetPythonHomePath(APythonVersion, AArchitecture);
  FAdbServices.DeleteDirectory(LRemotePythonHome);
end;

procedure TUnboundPythonServices.Run(const APythonVersion: TPythonVersion;
  const AArchitecture: TArchitecture; const ADebugger: TDebugger;
  const ABuildConfiguration: TBuildConfiguration);
var
  LArgs: TArray<string>;
  LRemotePythonHome: string;
begin
  LArgs := [];
  if (ABuildConfiguration = TBuildConfiguration.Debug) then
    case ADebugger of
      TDebugger.DebugPy: begin
        FAdbServices.SendFile(
          TBuilderPaths.GetDebugpyPackagePath(),
          TBuilderUnboundPaths.GetDebugpyPackagePath());
        FAdbServices.SendFile(
          TBuilderPaths.GetDebugpyScriptPath(),
          TBuilderUnboundPaths.GetDebugpyScriptPath());

        LArgs := [TBuilderUnboundPaths.GetDebugpyScriptPath(),
          FEnvironmentModel.RemoteDebuggerPort.ToString(),
          TBuilderUnboundPaths.GetDebugpyPackagePath()];
      end;
      TDebugger.Rpyc: begin
//        FAdbServices.SendFile(
//          TBuilderPaths.GetRpycPackagePath(),
//          TBuilderUnboundPaths.GetRpycPackagePath());
        FAdbServices.SendFile(
          TBuilderPaths.GetRpycScriptPath(),
          TBuilderUnboundPaths.GetRpycScriptPath());
        //This one is not adapted to the Android app
        FAdbServices.SendFile(
          TBuilderPaths.GetRemServerScriptPath(),
          TBuilderUnboundPaths.GetRemServerScriptPath());

        LArgs := [TBuilderUnboundPaths.GetRemServerScriptPath(),
          FEnvironmentModel.RemoteDebuggerPort.ToString(),
          TBuilderUnboundPaths.GetRpycPackagePath()];
      end;
    end;

  FAdbServices.StartDebugSession(FEnvironmentModel.RemoteDebuggerPort);
  try
    LRemotePythonHome := TBuilderUnboundPaths.GetPythonHomePath(APythonVersion, AArchitecture);
    FAdbServices.RunSubprocess(
      LRemotePythonHome + '/bin/python3.9',
      LArgs,
      ['LD_LIBRARY_PATH=' + LRemotePythonHome + '/lib/',
       'PYTHONHOME=' + LRemotePythonHome,
       'PATH=' + LRemotePythonHome + '/bin/']);
  finally
    FAdbServices.StopDebugSession(FEnvironmentModel.RemoteDebuggerPort);
  end;
end;

function TUnboundPythonServices.Exists(const APythonVersion: TPythonVersion;
  const AArchitecture: TArchitecture): boolean;
var
  LRemotePythonHome: string;
begin
  LRemotePythonHome := TBuilderUnboundPaths.GetPythonHomePath(APythonVersion, AArchitecture);
  Result := FAdbServices.DirectoryExists(LRemotePythonHome);
end;

end.
