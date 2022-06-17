unit Form.Project.Create;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ImgList,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit, FMX.ListBox, FMX.Layouts,
  FMX.Objects, Builder.Services;

type
  TProjectCreateForm = class(TForm)
    lnHeaderSeparator: TLine;
    loBody: TLayout;
    lbProject: TListBox;
    lbghApplicationOptions: TListBoxGroupHeader;
    lbiCreateMainFile: TListBoxItem;
    loFooter: TLayout;
    loRightActions: TLayout;
    btnSave: TButton;
    btnCancel: TButton;
    loHeader: TLayout;
    lblProject: TLabel;
    imgHeader: TGlyph;
    cbCreateMainFile: TCheckBox;
    lbghProjectName: TListBoxGroupHeader;
    lbiProjectName: TListBoxItem;
    edtProjectName: TEdit;
    procedure FormConstrainedResize(Sender: TObject; var MinWidth, MinHeight,
      MaxWidth, MaxHeight: Single);
    procedure btnSaveClick(Sender: TObject);
    procedure edtAppNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure edtProjectNameKeyUp(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
  private
    FProjectServices: IProjectServices;
  public
    class function CreateProject(var AProjectName: string; var ACreateMainScript: boolean): boolean;
  end;

var
  ProjectCreateForm: TProjectCreateForm;

implementation

uses
  Builder.Services.Factory;

{$R *.fmx}

procedure TProjectCreateForm.btnSaveClick(Sender: TObject);
begin
  if edtProjectName.Text.IsEmpty() then
    raise Exception.Create('Project name can''t be empty.');
  if FProjectServices.HasProject(edtProjectName.Text) then
    raise Exception.Create('A project with the same name already exists.');

  ModalResult := mrOk;
end;

class function TProjectCreateForm.CreateProject(var AProjectName: string;
  var ACreateMainScript: boolean): boolean;
begin
  var LForm := TProjectCreateForm.Create(nil);
  try
    Result := LForm.ShowModal() = mrOk;
    if Result then begin
      AProjectName := LForm.edtProjectName.Text;
      ACreateMainScript := LForm.cbCreateMainFile.IsChecked;
    end;
  finally
    LForm.Free();
  end;
end;

procedure TProjectCreateForm.edtAppNameChange(Sender: TObject);
begin
  TEdit(Sender).Text := TEdit(Sender).Text.Trim();
end;

procedure TProjectCreateForm.edtProjectNameKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if KeyChar = ' ' then
    KeyChar := #0;
end;

procedure TProjectCreateForm.FormConstrainedResize(Sender: TObject;
  var MinWidth, MinHeight, MaxWidth, MaxHeight: Single);
begin
  MinWidth := 600;
  MaxWidth := 600;

  MinHeight := 280;
  MaxHeight := 280;
end;

procedure TProjectCreateForm.FormCreate(Sender: TObject);
begin
  FProjectServices := TServiceSimpleFactory.CreateProject();
end;

end.
