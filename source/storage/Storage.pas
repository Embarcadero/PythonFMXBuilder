unit Storage;

interface

uses
  System.TypInfo;

type
  IStorage = interface
    ['{BF1A532A-66D2-4D8B-A8A2-B356A6FC7CC9}']
    procedure SaveModel(const ATypeInfo: PTypeInfo; const AModel: TObject; const AEntity: string = '');
    function LoadModel(const ATypeInfo: PTypeInfo; var AModel: TObject;
      const AEntity: string = ''; const AId: string = ''): boolean;
    function ListModels(const ATypeInfo: PTypeInfo; const AEntity: string): TArray<TObject>;
  end;

  IStorage<Model : class> = interface(IStorage)
    ['{37366D5D-ECDA-4282-8762-CF87B7B440F7}']
    procedure SaveModel(const AModel: Model; const AEntity: string = '');
    function LoadModel(var AModel: Model; const AEntity: string = '';
      const AId: string = ''): boolean;
    function ListModels(const AEntity: string = ''): TArray<Model>;
  end;

  IStorageEnvironment = interface
    ['{523D8BF9-23F6-49DD-BFA5-234B8AA5D7E8}']
    function GetAdbPath(): string;
  end;

implementation

end.
