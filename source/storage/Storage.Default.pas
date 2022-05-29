unit Storage.Default;

interface

uses
  Storage;

type
  TDefaultStorage<Model: class> = class
  public
    class function Make(): IStorage<Model>;    
  end;

implementation

uses
  Storage.Json;

{ TDefaultStorage<Model> }

class function TDefaultStorage<Model>.Make: IStorage<Model>;
begin
  Result := TJsonStorage<Model>.Create();
end;

end.
