unit Storage.Json;

interface

uses
  System.TypInfo, System.Generics.Collections,
  REST.JsonReflect,
  Storage, Model;

type
  TJsonStorage<ModelType : class> = class(TInterfacedObject, IStorage, IStorage<ModelType>)
  private
    FConverters: TJSONConverters;
    function GetBasePath(): string;
    function GetEntityPath(const AEntity: string): string;
    function GetModelName(const ATypeInfo: PTypeInfo): string;
    function GetModelPath(const AEntity, AModel: string): string;
    function GetModelId(const AEntity: string; const AModel: TObject): string;
    function MakeEntityName(const AEntity: string; const ATypeInfo: PTypeInfo): string;
  public
    constructor Create();
    destructor Destroy(); override;

    //IStorageModel interface
    procedure SaveModel(const ATypeInfo: PTypeInfo; const AModel: TObject;
      const AEntity: string = ''); overload;
    function LoadModel(const ATypeInfo: PTypeInfo; var AModel: TObject;
      const AEntity: string = ''; const AId: string = ''): boolean; overload;
    function ListModels(const ATypeInfo: PTypeInfo;
      const AEntity: string = ''): TArray<TObject>; overload;
    function DeleteModel(const ATypeInfo: PTypeInfo; AModel: TObject;
      const AEntity: string = ''; const AId: string = ''): boolean; overload;
    //IStorageModel<Model> interface
    procedure SaveModel(const AModel: ModelType; const AEntity: string); overload;
    function LoadModel(var AModel: ModelType; const AEntity: string = '';
      const AId: string = ''): boolean; overload;
    function ListModels(const AEntity: string = ''): TArray<ModelType>; overload;
    function DeleteModel(AModel: ModelType;
      const AEntity: string = ''; const AId: string = ''): boolean; overload;
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

function TJsonStorage<ModelType>.GetModelId(const AEntity: string;
  const AModel: TObject): string;
begin
  var LRttiCtx := TRttiContext.Create();
  try
    var LRttiType := LRttiCtx.GetType(AModel.ClassInfo);
    var LRttiProp := LRttiType.GetProperty('Id');
    if Assigned(LRttiProp) then
      Result := LRttiProp.GetValue(AModel).AsString()
    else
      Result := AEntity;
  finally
    LRttiCtx.Free();
  end;
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

function TJsonStorage<ModelType>.MakeEntityName(const AEntity: string;
  const ATypeInfo: PTypeInfo): string;
begin
  if not AEntity.IsEmpty() then
    Result := AEntity
  else
    Result := GetModelName(ATypeInfo);

  if Result.IsEmpty then
    Result := ATypeInfo^.TypeData^.ClassType.ClassName;
end;

function TJsonStorage<ModelType>.LoadModel(const ATypeInfo: PTypeInfo;
  var AModel: TObject; const AEntity: string; const AId: string): boolean;
begin
  var LEntity := MakeEntityName(AEntity, ATypeInfo);

  var LId := AId;
  if LId.IsEmpty then
    LId := LEntity;

  //Each model has its own file
  var LEntityPath := GetModelPath(LEntity, LId);

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

function TJsonStorage<ModelType>.ListModels(
  const ATypeInfo: PTypeInfo; const AEntity: string): TArray<TObject>;
begin
  var LEntity := MakeEntityName(AEntity, ATypeInfo);
  var LPath := GetEntityPath(LEntity);

  if not TDirectory.Exists(LPath) then
    Exit(TArray<TObject>.Create());

  var LJsonFiles := TDirectory.GetFiles(LPath, '*.json', TSearchOption.soTopDirectoryOnly, nil);
  var LModels := TList<TObject>.Create();
  try
    for var LJsonFile in LJsonFiles do begin
      var LModel: TObject := nil;
      var LFile := TPath.GetFileName(LJsonFile);
      if LoadModel(ATypeInfo, LModel, AEntity, LFile.Replace('.json', String.Empty, [])) then
        LModels.Add(LModel);
    end;
    Result := LModels.ToArray();
  finally
    LModels.Free();
  end;
end;

function TJsonStorage<ModelType>.ListModels(const AEntity: string): TArray<ModelType>;
begin
  Result := TArray<ModelType>(ListModels(TypeInfo(ModelType), AEntity));
end;

function TJsonStorage<ModelType>.LoadModel(var AModel: ModelType;
  const AEntity: string; const AId: string): boolean;
begin
  Result := LoadModel(TypeInfo(ModelType), TObject(AModel), AEntity, AId);
end;

procedure TJsonStorage<ModelType>.SaveModel(const ATypeInfo: PTypeInfo;
  const AModel: TObject; const AEntity: string);
begin
  var LEntity := MakeEntityName(AEntity, ATypeInfo);
  var LJsonValue := FConverters.GetJSONMarshaler().Marshal(AModel);
  try
    if Assigned(LJsonValue) then begin
      var LEntityPath := GetModelPath(LEntity, GetModelId(LEntity, AModel));

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

function TJsonStorage<ModelType>.DeleteModel(const ATypeInfo: PTypeInfo;
  AModel: TObject; const AEntity, AId: string): boolean;
begin
  var LEntity := MakeEntityName(AEntity, ATypeInfo);
  var LEntityPath := GetModelPath(LEntity, GetModelId(LEntity, AModel));

  if not TFile.Exists(LEntityPath) then
    Exit(false);

  TFile.Delete(LEntityPath);

  Result := true;
end;

function TJsonStorage<ModelType>.DeleteModel(AModel: ModelType; const AEntity,
  AId: string): boolean;
begin
  Result := DeleteModel(TypeInfo(ModelType), AModel, AEntity, AId);
end;

end.
