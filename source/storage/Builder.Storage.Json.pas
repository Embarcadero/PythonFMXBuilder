unit Builder.Storage.Json;

interface

uses
  System.TypInfo, System.Generics.Collections,
  REST.JsonReflect,
  Builder.Storage, Builder.Model;

type
  TJsonStorage<ModelType : class> = class(TInterfacedObject, IStorage, IStorage<ModelType>)
  private
    FConverters: TJSONConverters;
    function GetBasePath(): string; //Internal models path
    function GetEntityPath(const AEntity: string): string; //Internal entity path
    function GetModelName(const ATypeInfo: PTypeInfo): string;
    function GetModelId(const ATypeInfo: PTypeInfo): string;
    function GetModelPath(const AEntity, AModel: string): string;
    function MakeModelName(const ATypeInfo: PTypeInfo): string;
    function MakeModelId(const ATypeInfo: PTypeInfo): string;

    function GetModelFileName(const ATypeInfo: PTypeInfo;
      const AModel: TObject; const AFileName: string = ''): string;
    procedure SetModelFileName(const ATypeInfo: PTypeInfo;
      const AModel: TObject; const AFileName: string);
    procedure SetPersistent(const AModel: TObject; const APersistent: boolean);
  public
    constructor Create();
    destructor Destroy(); override;

    //IStorageModel interface
    procedure SaveModel(const AModel: TObject; const AFileName: string = ''); overload;
    function LoadModel(const ATypeInfo: PTypeInfo; var AModel: TObject; const AFileName: string = ''): boolean; overload;
    function DeleteModel(const AModel: TObject; const AFileName: string = ''): boolean; overload;

    //IStorageModel<Model> interface
    procedure SaveModel(const AModel: ModelType; const AFileName: string = ''); overload;
    function LoadModel(var AModel: ModelType; const AFileName: string = ''): boolean; overload;
    function DeleteModel(const AModel: ModelType; const AFileName: string = ''): boolean; overload;
  end;

implementation

uses
  System.SysUtils, System.IOUtils, System.JSON, System.Rtti,
  Builder.Exception, Builder.Paths;

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
  Result := TBuilderPaths.GetStorageBasePath();
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
      Result := LAttrib.Name
    else
      Result := String.Empty;
  finally
    LRttiCtx.Free();
  end;
end;

function TJsonStorage<ModelType>.GetModelId(const ATypeInfo: PTypeInfo): string;
begin
  var LRttiCtx := TRttiContext.Create();
  try
    var LRttiType := LRttiCtx.GetType(ATypeInfo);
    var LAttrib := LRttiType.GetAttribute<ModelAttribute>();
    if Assigned(LAttrib) then
      Result := LAttrib.Id
    else
      Result := String.Empty;
  finally
    LRttiCtx.Free();
  end;
end;

function TJsonStorage<ModelType>.GetModelPath(const AEntity, AModel: string): string;
begin
  Result := TPath.Combine(GetEntityPath(AEntity), AModel + '.json');
end;

function TJsonStorage<ModelType>.MakeModelId(
  const ATypeInfo: PTypeInfo): string;
begin
  Result := GetModelId(ATypeInfo);
  if Result.IsEmpty then
    Result := ATypeInfo^.TypeData^.ClassType.ClassName;
end;

function TJsonStorage<ModelType>.MakeModelName(const ATypeInfo: PTypeInfo): string;
begin
  Result := GetModelName(ATypeInfo);
  if Result.IsEmpty then
    Result := ATypeInfo^.TypeData^.ClassType.ClassName;
end;

function TJsonStorage<ModelType>.GetModelFileName(const ATypeInfo: PTypeInfo;
  const AModel: TObject; const AFileName: string): string;
begin
  //By filename itself
  if not AFileName.IsEmpty() then
    Exit(AFileName);

  //By storage path property
  if Assigned(AModel) then begin
    var LRttiCtx := TRttiContext.Create();
    try
      var LRttiType := LRttiCtx.GetType(ATypeInfo);
      var LRttiProp := LRttiType.GetProperty('Defs');
      if Assigned(LRttiProp) then begin
        var LDefs := LRttiProp.GetValue(AModel).AsObject();
        if Assigned(LDefs) then begin
          LRttiType := LRttiCtx.GetType(LDefs.ClassType);
          LRttiProp := LRttiType.GetProperty('Storage');
          if Assigned(LRttiProp) then
            if not LRttiProp.GetValue(LDefs).AsString().IsEmpty() then
              Exit(LRttiProp.GetValue(LDefs).AsString());
        end;
      end;
    finally
      LRttiCtx.Free();
    end;
  end;

  //By internal path
  Result := GetModelPath(MakeModelName(ATypeInfo), MakeModelId(ATypeInfo));
end;

procedure TJsonStorage<ModelType>.SetModelFileName(const ATypeInfo: PTypeInfo;
  const AModel: TObject; const AFileName: string);
begin
  if not Assigned(AModel) then
    Exit;

  var LRttiCtx := TRttiContext.Create();
  try
    var LRttiType := LRttiCtx.GetType(ATypeInfo);
    var LRttiProp := LRttiType.GetProperty('Defs');
    if Assigned(LRttiProp) then begin
      var LDefs := LRttiProp.GetValue(AModel).AsObject();
      if Assigned(LDefs) then begin
        LRttiType := LRttiCtx.GetType(LDefs.ClassType);
        LRttiProp := LRttiType.GetProperty('Storage');
        if Assigned(LRttiProp) then
          LRttiProp.SetValue(LDefs, AFileName);
      end;
    end;
  finally
    LRttiCtx.Free();
  end;
end;

procedure TJsonStorage<ModelType>.SetPersistent(const AModel: TObject;
  const APersistent: boolean);
begin
  if not Assigned(AModel) then
    Exit;

  var LRttiCtx := TRttiContext.Create();
  try
    var LRttiType := LRttiCtx.GetType(AModel.ClassType);
    var LRttiProp := LRttiType.GetProperty('Defs');
    if Assigned(LRttiProp) then begin
      var LDefs := LRttiProp.GetValue(AModel).AsObject();
      if Assigned(LDefs) then begin
        LRttiType := LRttiCtx.GetType(LDefs.ClassType);
        LRttiProp := LRttiType.GetProperty('Phantom');
        if Assigned(LRttiProp) then
          LRttiProp.SetValue(LDefs, not APersistent);
      end;
    end;
  finally
    LRttiCtx.Free();
  end;
end;

procedure TJsonStorage<ModelType>.SaveModel(const AModel: TObject;
  const AFileName: string);
begin
  if not Assigned(AModel) then
    raise EUnableToSaveEntity.Create('Unable to save file.');

  var LFileName := GetModelFileName(PTypeInfo(AModel.ClassInfo), AModel, AFileName);

  if not TDirectory.Exists(TPath.GetDirectoryName(LFileName)) then
    TDirectory.CreateDirectory(TPath.GetDirectoryName(LFileName));

  var LMarshaler := FConverters.GetJSONMarshaler();
  try
    var LJsonValue := LMarshaler.Marshal(AModel);
    try
      if Assigned(LJsonValue) then begin
        TFile.WriteAllText(LFileName, LJsonValue.ToJSON(), TEncoding.UTF8);
        SetModelFileName(PTypeInfo(AModel.ClassInfo), AModel, LFileName);
        SetPersistent(AModel, true);
      end else
        raise EUnableToSaveEntity.CreateFmt('Unable to save "%s".', [LFileName]);
    finally
      LJsonValue.Free();
    end;
  finally
    LMarshaler.Free();
  end;
end;

function TJsonStorage<ModelType>.LoadModel(const ATypeInfo: PTypeInfo; var AModel: TObject;
  const AFileName: string = ''): boolean;
begin
  var LFileName := GetModelFileName(ATypeInfo, AModel, AFileName);

  if not TFile.Exists(LFileName) then
    Exit(false);

  var LJsonObj := TJSONObject.ParseJSONValue(TFile.ReadAllText(LFileName, TEncoding.UTF8));
  try
    if not Assigned(LJsonObj) then
      Exit(false);

    var LUnmarshaler := FConverters.GetJSONUnMarshaler();
    try
      AModel := ModelType(
        LUnmarshaler.CreateObject(
          ATypeInfo^.TypeData^.ClassType,
          LJsonObj.AsType<TJSONObject>,
          AModel));
      SetModelFileName(ATypeInfo, AModel, LFileName);
      SetPersistent(AModel, true);
      Result := Assigned(AModel);
    finally
      LUnmarshaler.Free();
    end;
  finally
    LJSONObj.Free();
  end;
end;

function TJsonStorage<ModelType>.DeleteModel(const AModel: TObject;
  const AFileName: string): boolean;
begin
  if not Assigned(AModel) then
    Exit(false);

  var LFileName := GetModelFileName(
    PTypeInfo(AModel.ClassInfo), AModel, AFileName);

  if not TFile.Exists(LFileName) then
    Exit(false);

  TFile.Delete(LFileName);

  SetPersistent(AModel, false);

  Result := true;
end;

procedure TJsonStorage<ModelType>.SaveModel(const AModel: ModelType;
  const AFileName: string);
begin
  SaveModel(TObject(AModel), AFileName);
end;

function TJsonStorage<ModelType>.LoadModel(var AModel: ModelType;
  const AFileName: string = ''): boolean;
begin
  Result := LoadModel(TypeInfo(ModelType), TObject(AModel), AFileName);
end;

function TJsonStorage<ModelType>.DeleteModel(const AModel: ModelType;
  const AFileName: string): boolean;
begin
  Result := DeleteModel(AModel, AFileName);
end;

end.
