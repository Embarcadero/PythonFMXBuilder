unit Builder.Services.Environment;

interface

uses
  System.Classes,
  System.IOUtils,
  System.SysUtils,
  System.Types,
  Builder.Types,
  Builder.Services,
  Builder.Model.Environment,
  Builder.Model.Environment.Android;

type
  TEnvironmentService<T: TEnvironmentModel> = class(TInterfacedObject, IEnvironmentServices<T>)
  private
    class var FActiveEnvironment: T; //Android only by now
  private
    class destructor Destroy();
  public
    function CreateEnvironment(): T;

    procedure SaveEnvironment(const AEnvironment: T);
    function LoadEnvironment(): T;
    procedure UnLoadEnvironment();

    function HasActiveEnvironment(): boolean;
    procedure CheckActiveEnvironment();
    function GetActiveEnvironment(): T;
  end;

implementation

uses
  Builder.Exception,
  Builder.Storage.Default;

{ TEnvironmentService<T> }

class destructor TEnvironmentService<T>.Destroy;
begin
  FActiveEnvironment.Free();
end;

function TEnvironmentService<T>.CreateEnvironment: T;
begin
  Result := TEnvironmentModelClass(T).Create() as T;
end;

procedure TEnvironmentService<T>.SaveEnvironment(
  const AEnvironment: T);
begin
  var LStorage := TDefaultStorage<T>.Make();
  LStorage.SaveModel(AEnvironment);
end;

function TEnvironmentService<T>.LoadEnvironment: T;
begin
  UnLoadEnvironment();
  var LStorage := TDefaultStorage<T>.Make();
  LStorage.LoadModel(FActiveEnvironment);
  if not Assigned(FActiveEnvironment) then
    FActiveEnvironment := CreateEnvironment();
  Result := FActiveEnvironment;
end;

procedure TEnvironmentService<T>.UnLoadEnvironment;
begin
  FreeAndNil(FActiveEnvironment);
end;

function TEnvironmentService<T>.HasActiveEnvironment: boolean;
begin
  Result := Assigned(FActiveEnvironment);
end;

procedure TEnvironmentService<T>.CheckActiveEnvironment;
begin
  if not Assigned(FActiveEnvironment) then
    raise EInvalidEnvironment.Create('No active environment.');
end;

function TEnvironmentService<T>.GetActiveEnvironment: T;
begin
  if not Assigned(FActiveEnvironment) then
    LoadEnvironment();
  Result := FActiveEnvironment;
end;

end.
