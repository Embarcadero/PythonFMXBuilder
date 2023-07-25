unit Builder.Model;

interface

uses
  System.Classes, System.SysUtils, REST.Json.Types, REST.JsonReflect;

type
  TModelClass = class of TModel;

  TModelDefinition = class
  private
    FPhantom: boolean;
    FStorage: string;
    FUntracked: boolean;
  public
    constructor Create();
    //Never persisted
    property Phantom: boolean read FPhantom write FPhantom;
    //Storage info
    property Storage: string read FStorage write FStorage;
    //Never saved by user
    property Untracked: boolean read FUntracked write FUntracked;
  end;

  TModel = class
  private
    [JSONMarshalled(false)]
    FDefs: TModelDefinition;
  protected
    function CreateDefs(): TModelDefinition; virtual;
  public
    constructor Create(); virtual;
    destructor Destroy(); override;

    function Validate(const AErrors: TStrings): boolean; virtual; abstract;

    property Defs: TModelDefinition read FDefs;
  end;

  ModelAttribute = class(TCustomAttribute)
  private
    FModelName: string;
  public
    constructor Create(const AModelName: string); overload;

    property ModelName: string read FModelName write FModelName;
  end;

  JSONOwnedReflectAttribute = class(JsonReflectAttribute)
  public
    constructor Create();
  end;

implementation

{ TModel }

constructor TModel.Create;
begin
  inherited;
  FDefs := CreateDefs;
end;

destructor TModel.Destroy;
begin
  FDefs.Free();
  inherited;
end;

function TModel.CreateDefs: TModelDefinition;
begin
  Result := TModelDefinition.Create();
end;

{ ModelAttribute }

constructor ModelAttribute.Create(const AModelName: string);
begin
  inherited Create();
  FModelName := AModelName;
end;

{ JSONOwnedReflectAttribute }

constructor JSONOwnedReflectAttribute.Create;
begin
  inherited Create(
    TConverterType.ctObjects,
    TReverterType.rtObjects,
    nil, TJSONPopulationCustomizer, false);
end;

{ TModelDefinition }

constructor TModelDefinition.Create;
begin
  inherited;
  FPhantom := true;
end;

end.
