unit Form.SelectProject;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ListBox;

type
  TSelectProjectForm = class(TForm)
    cbValues: TComboBox;
    btnCancel: TButton;
    btnSelect: TButton;
    procedure FormResize(Sender: TObject);
    procedure btnSelectClick(Sender: TObject);
  public
    class function Select(const AValues: TArray<string>): string; static;
  end;

var
  SelectProjectForm: TSelectProjectForm;

implementation

{$R *.fmx}

procedure TSelectProjectForm.btnSelectClick(Sender: TObject);
begin
  if cbValues.ItemIndex <= -1 then
    raise Exception.Create('Select an item.');
  ModalResult := mrOk;
end;

procedure TSelectProjectForm.FormResize(Sender: TObject);
begin
  if Height <> 86 then
    Height := 86;
  if Width <> 256 then
    Width := 256;
end;

class function TSelectProjectForm.Select(const AValues: TArray<string>): string;
begin
  var LForm := TSelectProjectForm.Create(nil);
  try
    for var LValue in AValues do begin
      LForm.cbValues.Items.Add(LValue);
    end;

    LForm.cbValues.ItemIndex := 0;

    if LForm.ShowModal() = mrOk then
      Result := LForm.cbValues.Items[LForm.cbValues.ItemIndex]
    else
      Result := String.Empty;
  finally
    LForm.Free();
  end;
end;

end.
