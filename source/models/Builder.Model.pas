unit Builder.Model;

interface

uses
  System.Classes, System.SysUtils, REST.Json.Types, REST.JsonReflect;

type
  TModelClass = class of TModel;

  TModel = class
  private
    [JSONMarshalled(false)]
    FStorage: string;
  public
    constructor Create(); virtual;

    function Validate(const AErrors: TStrings): boolean; virtual; abstract;

    property Storage: string read FStorage write FStorage;
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

end.
