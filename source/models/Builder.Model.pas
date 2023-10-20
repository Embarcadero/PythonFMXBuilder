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
    FName: string;
    FId: string;
  public
    constructor Create(const AName: string); overload;
    constructor Create(const AName, AId: string); overload;

    property Name: string read FName write FName;
    property Id: string read FId write FId;
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

constructor ModelAttribute.Create(const AName, AId: string);
begin
  inherited Create();
  FName := AName;
  FId := AId;
end;

constructor ModelAttribute.Create(const AName: string);
begin
  Create(AName, AName);
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
