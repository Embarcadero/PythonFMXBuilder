unit Storage.Default;

interface

uses
  Storage;

type
  TDefaultStorage<Model: class> = class(TInterfacedObject, IStorage<Model>)
  private
     FDefaultStorage: IStorage<Model>;
  protected
    constructor Create();
    property Storage: IStorage<Model> read FDefaultStorage implements IStorage<Model>;
  public
    class function Make(): IStorage<Model>;    
  end;

implementation

uses
  Storage.Json;

{ TDefaultStorage<Model> }

constructor TDefaultStorage<Model>.Create;
begin
  FDefaultStorage := TJsonStorage<Model>.Create();
end;

class function TDefaultStorage<Model>.Make: IStorage<Model>;
begin
  Result := TDefaultStorage<Model>.Create();
end;

end.
