unit Form.Base;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs;

type
  TBaseForm = class(TForm)
    StyleBook1: TStyleBook;
  end;

var
  BaseForm: TBaseForm;

implementation

{$R *.fmx}

end.
