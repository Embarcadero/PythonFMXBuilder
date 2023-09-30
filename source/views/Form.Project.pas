unit Form.Project;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects,
  FMX.Layouts, FMX.Edit, FMX.ListBox, FMX.ImgList, System.Actions,
  FMX.ActnList, Form.Data, Builder.Model.Project, FMX.Ani, FMX.Effects,
  FMX.Filter.Effects;

type
  [Entity(TProjectModel)]
  TProjectForm = class(TDataForm)
    ListBoxGroupHeader1: TListBoxGroupHeader;
    lbiPackageName: TListBoxItem;
    edtPackageName: TEdit;
    ListBoxGroupHeader2: TListBoxGroupHeader;
    lbiVersionCode: TListBoxItem;
    edtVersionCode: TEdit;
    lbiVersionName: TListBoxItem;
    edtVersionName: TEdit;
    ListBoxGroupHeader5: TListBoxGroupHeader;
    lbiApplicationName: TListBoxItem;
    edtAppName: TEdit;
    lblVersionCode: TLabel;
    lblVersionName: TLabel;
    lblPackageName: TLabel;
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
    lbiSetMainFile: TListBoxItem;
    lblInitScript: TListBoxGroupHeader;
    cbMainFile: TComboBox;
    lblMainFile: TLabel;
    procedure sebDrawableClick(Sender: TObject);
  protected
    procedure FormUpdate(); override;
    procedure ModelUpdate(); override;
  end;

var
  ProjectForm: TProjectForm;

implementation

uses
  System.IOUtils,
  Builder.Types,
  Container.Images;

{$R *.fmx}

{ TProjectForm }

procedure TProjectForm.FormUpdate;
begin
  with Model as TProjectModel do begin
    edtAppName.Text := ApplicationName;
    edtPackageName.Text := PackageName;
    edtVersionCode.Text := VersionCode.ToString();
    edtVersionName.Text := VersionName;

    with Files do begin
      cbMainFile.Clear();
      for var LFile in Modules do begin
        cbMainFile.Items.Add(LFile.Name);
      end;

      if Main.IsEmpty() then
        cbMainFile.ItemIndex := 0
      else
        cbMainFile.ItemIndex := cbMainFile.Items.IndexOf(Main);
    end;

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

procedure TProjectForm.ModelUpdate;
const
  PY_VER: array[0..3] of TPythonVersion = (
    TPythonVersion.cp38, TPythonVersion.cp39, TPythonVersion.cp310, TPythonVersion.cp311);
  ARCH: array[0..1] of TArchitecture = (TArchitecture.arm, TArchitecture.aarch64);
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

    with Files do begin
      if Assigned(cbMainFile.Selected) then
        Main := cbMainFile.Selected.Text;
    end;

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

procedure TProjectForm.sebDrawableClick(Sender: TObject);
begin
  inherited;
  if odIcon.Execute then
    (Sender as TSearchEditButton).GetEdit().Text := odIcon.FileName;
end;

end.
