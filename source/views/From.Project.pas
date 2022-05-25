unit From.Project;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects,
  FMX.Layouts, FMX.Edit, FMX.ListBox, FMX.ImgList, System.Actions,
  FMX.ActnList, Form.Data, Model.Project, FMX.Ani;

type
  [Entity(TProjectModel)]
  TProjectForm = class(TDataForm)
    ListBoxGroupHeader1: TListBoxGroupHeader;
    ListBoxItem1: TListBoxItem;
    edtPackageName: TEdit;
    ListBoxGroupHeader2: TListBoxGroupHeader;
    ListBoxItem2: TListBoxItem;
    edtVersionCode: TEdit;
    ListBoxItem3: TListBoxItem;
    edtVersionName: TEdit;
    ListBoxGroupHeader3: TListBoxGroupHeader;
    ListBoxItem4: TListBoxItem;
    cbPythonVersion: TComboBox;
    ListBoxGroupHeader4: TListBoxGroupHeader;
    ListBoxItem5: TListBoxItem;
    cbArchitecture: TComboBox;
    ListBoxGroupHeader5: TListBoxGroupHeader;
    ListBoxItem6: TListBoxItem;
    edtAppName: TEdit;
    lblVersionCode: TLabel;
    lblVersionName: TLabel;
    lblPackageName: TLabel;
    lblPythonVersion: TLabel;
    lbghAppIcons: TListBoxGroupHeader;
    lbiDrawableSmall: TListBoxItem;
    edtDrawableSmall: TEdit;
    lblDrawableSmall: TLabel;
    lbiDrawableNormal: TListBoxItem;
    edtDrawableNormal: TEdit;
    lblDrawableNormal: TLabel;
    lbiDrawableLarge: TListBoxItem;
    edtDrawableLarge: TEdit;
    lblDrawableLarge: TLabel;
    lbiDrawableXLarge: TListBoxItem;
    edtDrawableXLarge: TEdit;
    lblDrawableXLarge: TLabel;
    lbiDrawableLDpi: TListBoxItem;
    edtDrawableLDpi: TEdit;
    lblDrawableLDpi: TLabel;
    lbiDrawableMDpi: TListBoxItem;
    edtDrawableMDpi: TEdit;
    lblDrawableMDpi: TLabel;
    lbiDrawableHDpi: TListBoxItem;
    edtDrawableHDpi: TEdit;
    lblDrawableHDpi: TLabel;
    lbiDrawableXHdpi: TListBoxItem;
    edtDrawableXHdpi: TEdit;
    lblDrawableXHdpi: TLabel;
    lbiDrawableXxHdpi: TListBoxItem;
    edtDrawableXxhdpi: TEdit;
    lblDrawableXxHDpi: TLabel;
    lbiDrawableXxxHdpi: TListBoxItem;
    edtDrawableXxxHDpi: TEdit;
    lblDrawableXxxHDpi: TLabel;
    sebDrawableXxxHDpi: TSearchEditButton;
    sebDrawableXxHDpi: TSearchEditButton;
    sebDrawableXHDpi: TSearchEditButton;
    sebDrawableHDpi: TSearchEditButton;
    sebDrawableMDpi: TSearchEditButton;
    sebDrawableLDpi: TSearchEditButton;
    sebDrawableXLarge: TSearchEditButton;
    sebDrawableLarge: TSearchEditButton;
    sebDrawableNormal: TSearchEditButton;
    sebDrawableSmall: TSearchEditButton;
    odIcon: TOpenDialog;
    procedure sebDrawableClick(Sender: TObject);
  protected
    procedure FormUpdate(); override;
    procedure ModelUpdate(); override;
  end;

var
  ProjectForm: TProjectForm;

implementation

uses
  Container.Images, Architecture, PythonVersion;

{$R *.fmx}

{ TProjectForm }

procedure TProjectForm.FormUpdate;
const
  PY_VER: array[cp38..cp310] of integer = (0, 1, 2);
  ARCH: array[arm..aarch64] of integer = (0, 1);
begin
  with Model as TProjectModel do begin
    edtAppName.Text := ApplicationName;
    edtPackageName.Text := PackageName;
    edtVersionCode.Text := VersionCode.ToString();
    edtVersionName.Text := VersionName;
    cbPythonVersion.ItemIndex := PY_VER[PythonVersion];
    cbArchitecture.ItemIndex := ARCH[Architecture];

    with Icons do begin
      DrawableSmall := edtDrawableSmall.Text;
      DrawableNormal := edtDrawableNormal.Text;
      DrawableLarge := edtDrawableLarge.Text;
      DrawableXlarge := edtDrawableXLarge.Text;
      DrawableLdpi := edtDrawableLDpi.Text;
      DrawableMdpi := edtDrawableMDpi.Text;
      DrawableHdpi := edtDrawableHDpi.Text;
      DrawableXhdpi := edtDrawableXHdpi.Text;
      DrawableXxhdpi := edtDrawableXxhdpi.Text;
      DrawableXxxHdpi := edtDrawableXxxHDpi.Text;
    end;
  end;
end;

procedure TProjectForm.ModelUpdate;
const
  PY_VER: array[0..2] of TPythonVersion = (cp38, cp39, cp310);
  ARCH: array[0..1] of TArchitecture = (arm, aarch64);
var
  LInt: integer;
begin
  with Model as TProjectModel do begin
    ApplicationName := edtAppName.Text;
    PackageName := edtPackageName.Text;
    if TryStrToInt(edtVersionCode.Text, LInt) then
      VersionCode := LInt
    else
      VersionCode := 0;
    VersionName := edtVersionName.Text;
    PythonVersion := PY_VER[cbPythonVersion.ItemIndex];
    Architecture := ARCH[cbArchitecture.ItemIndex];

    with Icons do begin
      edtDrawableSmall.Text := DrawableSmall;
      edtDrawableNormal.Text := DrawableNormal;
      edtDrawableLarge.Text := DrawableLarge;
      edtDrawableXLarge.Text := DrawableXlarge;
      edtDrawableLDpi.Text := DrawableLdpi;
      edtDrawableMDpi.Text := DrawableMdpi;
      edtDrawableHDpi.Text := DrawableHdpi;
      edtDrawableXHdpi.Text := DrawableXhdpi;
      edtDrawableXxhdpi.Text := DrawableXxhdpi;
      edtDrawableXxxHDpi.Text := DrawableXxxHdpi;
    end;
  end;
end;

procedure TProjectForm.sebDrawableClick(Sender: TObject);
begin
  inherited;
  if odIcon.Execute then
    ((Sender as TComponent).Owner as TEdit).Text := odIcon.FileName;
end;

end.
