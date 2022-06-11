unit Storage.Environment;

interface

uses
  Storage, Storage.Default,
  Model.Environment;

type
  TEnvironmentStorage = class(TInterfacedObject, IStorageEnvironment)
  private
    FStorage: IStorage<TEnvironmentModel>;
  public
    constructor Create();
    destructor Destroy(); override;

    function GetAdbPath(): string;
  end;

implementation

uses
  System.SysUtils;

{ TEnvironmentStorage }

constructor TEnvironmentStorage.Create;
begin
  FStorage := TDefaultStorage<TEnvironmentModel>.Make();
end;

destructor TEnvironmentStorage.Destroy;
begin
  FStorage := nil;
  inherited;
end;

function TEnvironmentStorage.GetAdbPath: string;
begin
  var LModel: TEnvironmentModel := nil;
  if FStorage.LoadModel(LModel) then begin
    try
      Result := LModel.AdbLocation
    finally
      LModel.Free();
    end;
  end else
    Result := String.Empty;
end;

end.
