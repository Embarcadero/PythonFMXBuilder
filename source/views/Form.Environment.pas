unit Form.Environment;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.ListBox, FMX.Controls.Presentation, FMX.Edit, FMX.Layouts,
  FMX.ImgList, System.Actions, FMX.ActnList, Form.Data, Model.Environment,
  FMX.Ani;

type
  [Entity(TEnvironmentModel)]
  TEnvironmentForm = class(TDataForm)
    ListBoxGroupHeader1: TListBoxGroupHeader;
    ListBoxItem1: TListBoxItem;
    edtSdkBasePath: TEdit;
    ListBoxItem2: TListBoxItem;
    edtJarSignerLocation: TEdit;
    ListBoxItem3: TListBoxItem;
    edtAdbLocation: TEdit;
    ListBoxItem4: TListBoxItem;
    edtAaptLocation: TEdit;
    ListBoxItem5: TListBoxItem;
    edtSdkAPILocation: TEdit;
    ListBoxItem6: TListBoxItem;
    edtZipAlign: TEdit;
    ListBoxItem7: TListBoxItem;
    edtKeyTool: TEdit;
    lblSdkBasePath: TLabel;
    lblJarSigner: TLabel;
    lblAdb: TLabel;
    lblAapt: TLabel;
    lblSdkApi: TLabel;
    lblZipAlign: TLabel;
    lblKeyTool: TLabel;
  protected
    procedure FormUpdate(); override;
    procedure ModelUpdate(); override;
  end;

var
  EnvironmentForm: TEnvironmentForm;

implementation

uses
  Container.Images;

{$R *.fmx}

{ TEnvironmentForm }

procedure TEnvironmentForm.FormUpdate;
begin
  with Model as TEnvironmentModel do begin
    edtSdkBasePath.Text := SdkBasePath;
    edtJarSignerLocation.Text := JarSignerLocation;
    edtAdbLocation.Text := AdbLocation;
    edtAaptLocation.Text := AAptLocation;
    edtSdkAPILocation.Text := SdkApiLocation;
    edtZipAlign.Text := ZipAlignLocation;
    edtKeyTool.Text := KeyToolLocation;
  end;
end;

procedure TEnvironmentForm.ModelUpdate;
begin
  with Model as TEnvironmentModel do begin
    SdkBasePath := edtSdkBasePath.Text;
    JarSignerLocation := edtJarSignerLocation.Text;
    AdbLocation := edtAdbLocation.Text;
    AAptLocation := edtAaptLocation.Text;
    SdkApiLocation := edtSdkAPILocation.Text;
    ZipAlignLocation := edtZipAlign.Text;
    KeyToolLocation := edtKeyTool.Text;
  end;
end;

end.
