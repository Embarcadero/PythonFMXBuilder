unit Builder.Services.UnboundPython;

interface

uses
  System.SysUtils,
  Builder.Types,
  Builder.Services,
  Builder.Storage,
  Builder.Model.Environment;

type
  TUnboundPythonServices = class(TInterfacedObject, IUnboundPythonServices)
  private
    FAdbServices: IAdbServices;
    FEnvironmentStorage: IStorage<TEnvironmentModel>;
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
      const ARunMode: TRunMode);
  end;

implementation

uses
  System.IOUtils,
  Builder.Paths,
  Builder.Exception,
  Builder.Storage.Default,
  Builder.Services.Factory;

{ TUnboundPythonServices }

constructor TUnboundPythonServices.Create;
begin
  inherited;
  FAdbServices := TServiceSimpleFactory.CreateAdb();
  FEnvironmentStorage := TDefaultStorage<TEnvironmentModel>.Make();
  if not FEnvironmentStorage.LoadModel(FEnvironmentModel) then
    raise EEmptySettings.Create('The Environment Settings are empty.');
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
  const ARunMode: TRunMode);
var
  LArgs: TArray<string>;
  LRemotePythonHome: string;
begin
  LArgs := [];
  if (ARunMode = TRunMode.RunDebugMode) then begin
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
