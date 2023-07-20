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
    lbiCreateMainModule: TListBoxItem;
    loFooter: TLayout;
    loRightActions: TLayout;
    btnSave: TButton;
    btnCancel: TButton;
    loHeader: TLayout;
    lblProject: TLabel;
    imgHeader: TGlyph;
    cbCreateMainModule: TCheckBox;
    lbghProjectName: TListBoxGroupHeader;
    lbiProjectName: TListBoxItem;
    edtProjectName: TEdit;
    lbiMainModulePath: TListBoxItem;
    edtMainModulePath: TEdit;
    btnSaveProject: TButton;
    btnSaveMainModule: TButton;
    procedure FormConstrainedResize(Sender: TObject; var MinWidth, MinHeight,
      MaxWidth, MaxHeight: Single);
    procedure btnSaveClick(Sender: TObject);
    procedure edtAppNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSaveProjectClick(Sender: TObject);
    procedure btnSaveMainModuleClick(Sender: TObject);
    procedure cbCreateMainModuleChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FProjectServices: IProjectServices;
    FRecommendedProjectName: string;
    FRecommendedModuleName: string;
  public
    class function CreateProject(var AProjectName: string; var AMainModuleName: string): boolean;
  end;

var
  ProjectCreateForm: TProjectCreateForm;

implementation

uses
  System.IOUtils,
  Container.Menu.Actions,
  Builder.Paths,
  Builder.Services.Factory;

{$R *.fmx}

procedure TProjectCreateForm.btnSaveClick(Sender: TObject);
begin
  if edtProjectName.Text.IsEmpty() then
    raise Exception.Create('Project name can''t be empty.');

  ModalResult := mrOk;
end;

procedure TProjectCreateForm.btnSaveMainModuleClick(Sender: TObject);
begin
  if edtMainModulePath.Text.Trim().IsEmpty() then
    edtMainModulePath.Text := FRecommendedModuleName;

  var LProjectDir := TPath.GetDirectoryName(edtProjectName.Text);
  if not TDirectory.Exists(LProjectDir) then
    TDirectory.CreateDirectory(LProjectDir);

  MenuActionsContainer.sdModule.InitialDir := TPath.GetDirectoryName(edtMainModulePath.Text);
  MenuActionsContainer.sdModule.FileName := TPath.GetFileName(edtMainModulePath.Text);
  if MenuActionsContainer.sdModule.Execute() then
    edtMainModulePath.Text := MenuActionsContainer.sdModule.FileName;
end;

procedure TProjectCreateForm.btnSaveProjectClick(Sender: TObject);
begin
  if edtProjectName.Text.Trim().IsEmpty() then
    edtProjectName.Text := FRecommendedProjectName;

  var LProjectDir := TPath.GetDirectoryName(edtProjectName.Text);
  if not TDirectory.Exists(LProjectDir) then
    TDirectory.CreateDirectory(LProjectDir);

  MenuActionsContainer.sdProject.InitialDir := LProjectDir;
  MenuActionsContainer.sdProject.FileName := TPath.GetFileName(edtProjectName.Text);
  if MenuActionsContainer.sdProject.Execute() then
    edtProjectName.Text := MenuActionsContainer.sdProject.FileName;
end;

procedure TProjectCreateForm.cbCreateMainModuleChange(Sender: TObject);
begin
  lbiMainModulePath.Visible := TCheckBox(Sender).IsChecked;
end;

class function TProjectCreateForm.CreateProject(var AProjectName: string;
  var AMainModuleName: string): boolean;
begin
  var LForm := TProjectCreateForm.Create(nil);
  try
    Result := LForm.ShowModal() = mrOk;
    if Result then begin
      AProjectName := LForm.edtProjectName.Text;
      if LForm.cbCreateMainModule.IsChecked then
        AMainModuleName := LForm.edtMainModulePath.Text
      else
        AMainModuleName := String.Empty;
    end;
  finally
    LForm.Free();
  end;
end;

procedure TProjectCreateForm.edtAppNameChange(Sender: TObject);
begin
  TEdit(Sender).Text := TEdit(Sender).Text.Trim();
  lbiMainModulePath.Enabled := not TEdit(Sender).Text.IsEmpty();
end;

procedure TProjectCreateForm.FormConstrainedResize(Sender: TObject;
  var MinWidth, MinHeight, MaxWidth, MaxHeight: Single);
begin
  MinWidth := 600;
  MaxWidth := 600;

  MinHeight := 296;
  MaxHeight := 296;
end;

procedure TProjectCreateForm.FormCreate(Sender: TObject);
begin
  FProjectServices := TServiceSimpleFactory.CreateProject();
  FRecommendedProjectName := TBuilderPaths.RecommendProjectName(
    TBuilderPaths.WorkspaceFolder());
  FRecommendedModuleName := TPath.Combine(
    TPath.GetDirectoryName(FRecommendedProjectName),
    'main.py');

  lbiMainModulePath.Visible := false;
  edtProjectName.Text := FRecommendedProjectName;
  edtMainModulePath.Text := FRecommendedModuleName;
end;

procedure TProjectCreateForm.FormDestroy(Sender: TObject);
begin
  var LRecommendedDir := TPath.GetDirectoryName(FRecommendedProjectName);
  var LChosenDir := TPath.GetDirectoryName(edtProjectName.Text);
  if (LRecommendedDir <> LChosenDir) or (ModalResult <> mrOk) then
    if TDirectory.Exists(LRecommendedDir) then //might be renamed
      TDirectory.Delete(LRecommendedDir);
end;

end.
