unit Storage.Factory;

interface

uses
  Storage;

type
  TStorageSimpleFactory = class
  public
    class function CreateEnvironment(): IStorageEnvironment;
  end;

implementation

uses
  Storage.Environment;

{ TStorageSimpleFactory }

class function TStorageSimpleFactory.CreateEnvironment: IStorageEnvironment;
begin
  Result := TEnvironmentStorage.Create();
end;

end.
