unit Builder.Storage.Default;

interface

uses
  Builder.Storage;

type
  TDefaultStorage<Model: class> = class
  public
    class function Make(): IStorage<Model>;    
  end;

implementation

uses
  Builder.Storage.Json;

{ TDefaultStorage<Model> }

class function TDefaultStorage<Model>.Make: IStorage<Model>;
begin
  Result := TJsonStorage<Model>.Create();
end;

end.
