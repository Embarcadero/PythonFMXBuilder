unit Form.Data;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ImgList,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit, FMX.ListBox, FMX.Layouts,
  FMX.Objects, System.Actions, FMX.ActnList, System.Rtti, FMX.Ani, Form.Base, Builder.Model;

type
  TDataForm = class(TBaseForm)
    lnHeaderSeparator: TLine;
    loBody: TLayout;
    lbProject: TListBox;
    loFooter: TLayout;
    loLeftActions: TLayout;
    btnSave: TButton;
    btnCancel: TButton;
    loHeader: TLayout;
    lblProject: TLabel;
    imgHeader: TGlyph;
    actBase: TActionList;
    actSave: TAction;
    actCancel: TAction;
    procedure actSaveExecute(Sender: TObject);
    procedure actCancelExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FModel: TModel;
    FHasLoaded: boolean;
    FId: string;
  protected
    //Create the model that represents the form data
    function CreateModel(): TModel; virtual;
    function GetEntityType(): TClass; virtual;
    //Form fields updates
    procedure FormUpdate(); virtual; abstract;
    procedure ModelUpdate(); virtual; abstract;
    //Model validation
    procedure ModelValidate(); virtual;

    procedure Load(); virtual;
    procedure Save(); virtual;
    procedure Cancel(); virtual;

    property Model: TModel read FModel;
  public
    //Model id for autoload
    property Id: string read FId write FId;
    //Loading status
    property HasLoaded: boolean read FHasLoaded;
  end;

  EntityAttribute = class(TCustomAttribute)
  private
    FModelType: TClass;
  public
    constructor Create(const AModelType: TClass); overload;

    property ModelType: TClass read FModelType write FModelType;
  end;

var
  DataForm: TDataForm;

implementation

uses
  Container.Images, Builder.Storage, Builder.Storage.Default;

{$R *.fmx}

procedure TDataForm.FormCreate(Sender: TObject);
begin
  FModel := CreateModel();
end;

procedure TDataForm.FormDestroy(Sender: TObject);
begin
  FModel.Free();
end;

procedure TDataForm.FormShow(Sender: TObject);
begin
  Load();
end;

function TDataForm.GetEntityType: TClass;
begin
  Result := nil;

  var LRttiCtx := TRttiContext.Create();
  try
    var LRttiType := LRttiCtx.GetType(Self.ClassType);
    var LAttrib := LRttiType.GetAttribute<EntityAttribute>();
    if Assigned(LAttrib) then begin
      Result := LAttrib.ModelType;
    end;
  finally
    LRttiCtx.Free();
  end;
end;

function TDataForm.CreateModel: TModel;
begin
  var LEntityType := GetEntityType();
  if Assigned(LEntityType) then begin
    Result := LEntityType.NewInstance() as TModel;
    Result.Create();
  end else
    raise Exception.Create('Model not initialized.');
end;

procedure TDataForm.actCancelExecute(Sender: TObject);
begin
  Cancel();
end;

procedure TDataForm.actSaveExecute(Sender: TObject);
begin
  Save();
end;

procedure TDataForm.Cancel;
begin
  Close();
end;

procedure TDataForm.Load;
begin
  var LStorage: IStorage := TDefaultStorage<TObject>.Make();
  if LStorage.LoadModel(
    GetEntityType().ClassInfo, TObject(FModel), String.Empty, FId) then
  begin
    FormUpdate();
    FHasLoaded := true;
  end;
end;

procedure TDataForm.ModelValidate;
begin
  var LErrors := TStringList.Create();
  try
    if not FModel.Validate(LErrors) then
      raise EModelValidationError.Create(LErrors.Text);
  finally
    LErrors.Free();
  end;
end;

procedure TDataForm.Save;
begin
  ModelUpdate();
  ModelValidate();
  var LStorage: IStorage := TDefaultStorage<TObject>.Make();
  LStorage.SaveModel(GetEntityType().ClassInfo, FModel);
  Close();
end;

{ EntityAttribute }

constructor EntityAttribute.Create(const AModelType: TClass);
begin
  FModelType := AModelType;
end;

end.
