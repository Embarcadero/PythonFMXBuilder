unit Builder.Model;

interface

uses
  System.Classes, System.SysUtils;

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

implementation

{ TModel }

constructor TModel.Create;
begin
  inherited;
end;

{ ModelAttribute }

constructor ModelAttribute.Create(const AModelName: string);
begin
  FModelName := AModelName;
end;

end.
