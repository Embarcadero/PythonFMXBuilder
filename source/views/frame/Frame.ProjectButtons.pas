unit Frame.ProjectButtons;
interface
uses
  System.SysUtils, System.Types, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, FMX.Menus, Container.Images,
  Builder.Services, Builder.Model.Project, Form.SelectProject;
type
  TProjectButtonsFrame = class(TFrame)
    btnCreate: TSpeedButton;
    btnOpen: TSpeedButton;
    btnClose: TSpeedButton;
    pmCreateProject: TPopupMenu;
    miNewBlankProject: TMenuItem;
    miNewProject: TMenuItem;
    procedure btnCreateClick(Sender: TObject);
  end;
implementation
uses
  FMX.DialogService, System.UITypes,
  Container.Menu.Actions;
{$R *.fmx}
type
  TSpeedButtonCracker = class(TSpeedButton);
procedure TProjectButtonsFrame.btnCreateClick(Sender: TObject);
begin
  var LP := LocalToAbsolute(TPointF.Create(
    TControl(Sender).Width * 0.18, TControl(Sender).Height));
  LP := Scene.LocalToScreen(LP);
  TSpeedButtonCracker(Sender).ShowContextMenu(LP);
end;

end.
