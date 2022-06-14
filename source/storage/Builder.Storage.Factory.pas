unit Builder.Storage.Factory;

interface

uses
  Builder.Storage;

type
  TStorageSimpleFactory = class
  public
    class function CreateEnvironment(): IStorageEnvironment;
  end;

implementation

uses
  Builder.Storage.Environment;

{ TStorageSimpleFactory }

class function TStorageSimpleFactory.CreateEnvironment: IStorageEnvironment;
begin
  Result := TEnvironmentStorage.Create();
end;

end.
