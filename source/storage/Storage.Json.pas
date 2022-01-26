unit Storage.Json;

interface

uses
  REST.JsonReflect, System.TypInfo, Storage, Model;

type
  TJsonStorage<ModelType : class> = class(TInterfacedObject, IStorage, IStorage<ModelType>)
  private
    FConverters: TJSONConverters;
    function GetBasePath(): string;
    function GetEntityPath(const AEntity: string): string;
    function GetModelName(const ATypeInfo: PTypeInfo): string;
    function GetModelPath(const AEntity, AModel: string): string;
  public
    constructor Create();
    destructor Destroy(); override;

    //IStorageModel interface
    procedure SaveModel(const ATypeInfo: PTypeInfo; const AModel: TObject; const AEntity: string = ''); overload;
    function LoadModel(const ATypeInfo: PTypeInfo; var AModel: TObject; const AEntity: string = ''): boolean; overload;
    //IStorageModel<Model> interface
    procedure SaveModel(const AModel: ModelType; const AEntity: string); overload;
    function LoadModel(var AModel: ModelType; const AEntity: string): boolean; overload;
  end;

implementation

uses
  System.SysUtils, System.IOUtils, System.JSON, System.Rtti;

{ TJsonStorage<Model> }

constructor TJsonStorage<ModelType>.Create;
begin
  FConverters := TJSONConverters.Create();
end;

destructor TJsonStorage<ModelType>.Destroy;
begin
  FConverters.Free();
  inherited;
end;

function TJsonStorage<ModelType>.GetBasePath: string;
begin
  Result := TPath.Combine(ExtractFilePath(ParamStr(0)), 'data');
end;

function TJsonStorage<ModelType>.GetEntityPath(const AEntity: string): string;
begin
  Result := TPath.Combine(GetBasePath(), AEntity);
end;

function TJsonStorage<ModelType>.GetModelName(const ATypeInfo: PTypeInfo): string;
begin
  var LRttiCtx := TRttiContext.Create();
  try
    var LRttiType := LRttiCtx.GetType(ATypeInfo);
    var LAttrib := LRttiType.GetAttribute<ModelAttribute>();
    if Assigned(LAttrib) then
      Result := LAttrib.ModelName
    else
      Result := String.Empty;
  finally
    LRttiCtx.Free();
  end;
end;

function TJsonStorage<ModelType>.GetModelPath(const AEntity, AModel: string): string;
begin
  Result := ChangeFileExt(TPath.Combine(GetEntityPath(AEntity), AModel), '.json');
end;

function TJsonStorage<ModelType>.LoadModel(const ATypeInfo: PTypeInfo;
  var AModel: TObject; const AEntity: string): boolean;
begin
  var LEntity: string;
  if not AEntity.IsEmpty() then
    LEntity := AEntity
  else
    LEntity := GetModelName(ATypeInfo);

  if LEntity.IsEmpty then
    LEntity := ATypeInfo^.TypeData^.ClassType.ClassName;

  var LEntityPath := GetModelPath(LEntity, LEntity);

  if not TFile.Exists(LEntityPath) then
    Exit(false);

  var LJsonObj := TJSONObject.ParseJSONValue(TFile.ReadAllText(LEntityPath, TEncoding.UTF8));
  try
    if not Assigned(LJsonObj) then
      Exit(false);

    AModel := ModelType(FConverters.GetJSONUnMarshaler().CreateObject(
      ATypeInfo^.TypeData^.ClassType, LJsonObj.AsType<TJSONObject>, AModel));

    Result := Assigned(AModel);
  finally
    LJSONObj.Free();
  end;
end;

function TJsonStorage<ModelType>.LoadModel(var AModel: ModelType; const AEntity: string): boolean;
begin
  Result := LoadModel(TypeInfo(ModelType), TObject(AModel), AEntity);
end;

procedure TJsonStorage<ModelType>.SaveModel(const ATypeInfo: PTypeInfo;
  const AModel: TObject; const AEntity: string);
begin
  var LEntity: string;
  if not AEntity.IsEmpty() then
    LEntity := AEntity
  else
    LEntity := GetModelName(ATypeInfo);

  if LEntity.IsEmpty then
    LEntity := ATypeInfo^.TypeData^.ClassType.ClassName;


  var LJsonValue := FConverters.GetJSONMarshaler().Marshal(AModel);
  try
    if Assigned(LJsonValue) then begin
      var LEntityPath := GetModelPath(LEntity, LEntity);

      if not TDirectory.Exists(ExtractFilePath(LEntityPath)) then
        TDirectory.CreateDirectory(ExtractFilePath(LEntityPath));

      TFile.WriteAllText(LEntityPath, LJsonValue.ToJSON(), TEncoding.UTF8);
    end else
      raise Exception.CreateFmt('Unable to save %s', [LEntity]);
  finally
    LJsonValue.Free();
  end;
end;

procedure TJsonStorage<ModelType>.SaveModel(const AModel: ModelType; const AEntity: string);
begin
  SaveModel(TypeInfo(ModelType), AModel, AEntity);
end;

end.
