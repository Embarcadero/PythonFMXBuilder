unit Builder.Services.Environment;

interface

uses
  System.Classes,
  System.IOUtils,
  System.SysUtils,
  System.Types,
  Builder.Types,
  Builder.Services,
  Builder.Model.Environment;

type
  TEnvironmentService = class(TInterfacedObject, IEnvironmentServices)
  private
    class var FActiveEnvironment: TEnvironmentModel;
  private
    class destructor Destroy();
  public
    procedure SaveEnvironment(const AEnvironment: TEnvironmentModel);
    function LoadEnvironment(): TEnvironmentModel;
    procedure UnLoadEnvironment();

    function HasActiveEnvironment(): boolean;
    function GetActiveEnvironment(): TEnvironmentModel;
    procedure CheckActiveEnvironment();
  end;

implementation

uses
  Builder.Exception,
  Builder.Storage.Default;

{ TEnvironmentService }

procedure TEnvironmentService.CheckActiveEnvironment;
begin
  if not Assigned(FActiveEnvironment) then
    raise EInvalidEnvironment.Create('No active environment.');
end;

class destructor TEnvironmentService.Destroy;
begin
  FActiveEnvironment.Free();
end;

function TEnvironmentService.GetActiveEnvironment: TEnvironmentModel;
begin
  if not Assigned(FActiveEnvironment) then
    LoadEnvironment();
  Result := FActiveEnvironment;
end;

function TEnvironmentService.HasActiveEnvironment: boolean;
begin
  Result := Assigned(FActiveEnvironment);
end;

function TEnvironmentService.LoadEnvironment: TEnvironmentModel;
begin
  UnLoadEnvironment();
  var LStorage := TDefaultStorage<TEnvironmentModel>.Make();
  LStorage.LoadModel(FActiveEnvironment);
  Result := FActiveEnvironment;
end;

procedure TEnvironmentService.SaveEnvironment(
  const AEnvironment: TEnvironmentModel);
begin
  var LStorage := TDefaultStorage<TEnvironmentModel>.Make();
  LStorage.SaveModel(AEnvironment);
end;

procedure TEnvironmentService.UnLoadEnvironment;
begin
  FreeAndNil(FActiveEnvironment);
end;

end.
