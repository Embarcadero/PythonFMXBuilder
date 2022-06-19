unit Frame.ProjectButtons;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, Container.Images, Builder.Services,
  Builder.Model.Project, Form.SelectProject, Form.Project.Create;

type
  TProjectButtonsFrame = class(TFrame)
    btnCreate: TSpeedButton;
    btnOpen: TSpeedButton;
    btnRemove: TSpeedButton;
  end;

implementation

uses
  FMX.DialogService, System.UITypes,
  Container.Menu.Actions;

{$R *.fmx}

{ TProjectButtonsFrame }

end.
