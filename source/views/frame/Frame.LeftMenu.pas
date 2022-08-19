unit Frame.LeftMenu;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListBox, FMX.Layouts, FMX.Controls.Presentation;

type
  TLeftMenuFrame = class(TFrame)
    lbLeftMenu: TListBox;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    lbiEnvironment: TListBoxItem;
    ListBoxGroupHeader2: TListBoxGroupHeader;
    lbiProject: TListBoxItem;
    ListBoxGroupHeader3: TListBoxGroupHeader;
    lbiBuild: TListBoxItem;
    lbiDeploy: TListBoxItem;
    sbUpdateEnvironment: TSpeedButton;
    sbUpdateProject: TSpeedButton;
    sbBuildCurrentProject: TSpeedButton;
    spDeployCurrentProject: TSpeedButton;
    lbiRunCurrentProject: TListBoxItem;
    sbRunCurrentProject: TSpeedButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

uses
  Container.Menu.Actions;

end.
