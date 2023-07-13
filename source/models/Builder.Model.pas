unit Builder.Model;

interface

uses
  System.Classes, System.SysUtils, REST.JsonReflect;

type
  TModelClass = class of TModel;

  TModel = class
  public
    constructor Create(); virtual;

    function Validate(const AErrors: TStrings): boolean; virtual; abstract;
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
