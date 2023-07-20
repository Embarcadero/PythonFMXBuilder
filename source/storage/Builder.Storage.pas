unit Builder.Storage;

interface

uses
  System.TypInfo;

type
  IStorage = interface
    ['{BF1A532A-66D2-4D8B-A8A2-B356A6FC7CC9}']
    procedure SaveModel(const AModel: TObject; const AFileName: string = '');
    function LoadModel(const ATypeInfo: PTypeInfo; var AModel: TObject; const AFileName: string = ''): boolean;
    function DeleteModel(const AModel: TObject; const AFileName: string = ''): boolean;
  end;

  IStorage<Model : class> = interface(IStorage)
    ['{37366D5D-ECDA-4282-8762-CF87B7B440F7}']
    procedure SaveModel(const AModel: Model; const AFileName: string = '');
    function LoadModel(var AModel: Model; const AFileName: string = ''): boolean;
    function DeleteModel(const AModel: Model; const AFileName: string = ''): boolean;
  end;

implementation

end.
