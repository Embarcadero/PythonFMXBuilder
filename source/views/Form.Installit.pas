unit Form.Installit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListBox, FMX.Layouts, FMX.Controls.Presentation,
  Builder.Services;

type
  TInstallItListBoxItem = class(TListBoxItem)
  private
    FText: TLabel;
    FDetail: TLabel;
    FProgressLayout: TLayout;
    FProgressBar: TProgressBar;
    FProgressText: TLabel;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TInstallItForm = class(TForm)
    loBottom: TLayout;
    loActions: TLayout;
    btnInstallSelectedTools: TCornerButton;
    loAgreement: TLayout;
    lblReadTerms: TLabel;
    cbAgree: TCheckBox;
    lbPackages: TListBox;
    procedure lblReadTermsMouseEnter(Sender: TObject);
    procedure lblReadTermsMouseLeave(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnInstallSelectedToolsClick(Sender: TObject);
    procedure lblReadTermsClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure cbAgreeChange(Sender: TObject);
    procedure lbPackagesChangeCheck(Sender: TObject);
  private
    FToolsInstaller: IToolInstallServices;
    procedure UpdateComponents();
    procedure Install(const AItem: TListBoxItem);
  public
    constructor Create(AOwner: TComponent); override;

    procedure ListTools();
  end;

var
  InstallItForm: TInstallItForm;

implementation

uses
  System.Threading,
  FMX.DialogService,
  {$IFDEF MSWINDOWS}
  Winapi.ShellAPI, Winapi.Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF POSIX}
  Posix.Stdlib,
  {$ENDIF POSIX}
  Builder.Types;

type
  TURL = class
    class procedure Open(const AUrl: string);
  end;

const
  ITEM_REGULAR_HEIGHT = 50;
  ITEM_INSTALLING_HEIGHT = 90;

{$R *.fmx}

{ TInstallItListBoxItem }

constructor TInstallItListBoxItem.Create(AOwner: TComponent);
begin
  inherited;
  FText := TLabel.Create(Self);
  FDetail := TLabel.Create(Self);
  FProgressLayout := TLayout.Create(Self);
  FProgressBar := TProgressBar.Create(Self);
  FProgressText := TLabel.Create(Self);

  ItemData.Accessory := TListBoxItemData.TAccessory.aNone;

  with FText do begin
    Parent := self;
    Align := TAlignLayout.Top;
    StyledSettings := [TStyledSetting.Family, TStyledSetting.Style, TStyledSetting.FontColor];
    Margins.Left := 30;
    Margins.Top := 10;
    Position.X := 10;
    Position.Y := 10;
    Size.Width := 462;
    Size.Height := 24;
    Size.PlatformDefault := False;
    TextSettings.Font.Size := 15;
  end;

  with FDetail do begin
    Parent := self;
    Align := TAlignLayout.Top;
    Margins.Left := 30;
    Position.X := 10;
    Position.Y := 34;
    Size.Width := 462;
    Size.Height := 17;
    Size.PlatformDefault := False;
  end;

  with FProgressLayout do begin
    Parent := self;
    Align := TAlignLayout.Bottom;
    Margins.Left := 30;
    Margins.Right := 50;
    Position.X := 10;
    Position.Y := 61;
    Size.Width := 412;
    Size.Height := 34;
    Size.PlatformDefault := False;
    Visible := false;
  end;

  with FProgressBar do begin
    Parent := FProgressLayout;
    Align := TAlignLayout.Top;
    HitTest := False;
    Orientation := TOrientation.Horizontal;
    Margins.Left := 2;
    Margins.Right := 2;
    Position.X := 2;
    Position.Y := 17;
    Size.Width := 408;
    Size.Height := 14;
    Size.PlatformDefault := False;
  end;

  with FProgressText do begin
    Parent := FProgressLayout;
    Align := TAlignLayout.Top;
    Margins.Left := 2;
    Margins.Right := 2;
    Position.X := 2;
    Size.Width := 408;
    Size.Height := 17;
    Size.PlatformDefault := False;
  end;
end;

{ TInstallItForm }

constructor TInstallItForm.Create(AOwner: TComponent);
begin
  inherited;
  FToolsInstaller := TBuilderService.CreateService<IToolInstallServices>();
end;

procedure TInstallItForm.cbAgreeChange(Sender: TObject);
begin
  UpdateComponents();
end;

procedure TInstallItForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  var LInstallingList: TArray<IAsyncResult> := [];
  for var LItem in FToolsInstaller.GetInstallingTools() do
    if not (LItem.Value.IsCancelled or LItem.Value.IsCompleted) then
      LInstallingList := LInstallingList + [LItem.Value];

  if not Assigned(LInstallingList) then
    Exit;

  var LCanClose := false;
  TDialogService.MessageDialog('Do you want to cancel all running tasks?', 
    TMsgDlgType.mtConfirmation, 
    [TMsgDlgBtn.mbNo, TMsgDlgBtn.mbYes], 
    TMsgDlgBtn.mbNo, 
    0, 
    procedure(const AResult: TModalResult) begin
      if AResult <> mrYes then
        Exit;

      for var LTask in LInstallingList do
        if not (LTask.IsCancelled or LTask.IsCompleted) then
          LTask.Cancel();

      ShowMessage('Cancelling operations. It can take a while.');

      for var LTask in LInstallingList do
        LTask.AsyncWaitEvent.WaitFor();

      // Clear queue
      Application.ProcessMessages();

      LCanClose := true;
    end);

  CanClose := LCanClose;
end;

procedure TInstallItForm.FormShow(Sender: TObject);
begin
  ListTools();
end;

procedure TInstallItForm.btnInstallSelectedToolsClick(Sender: TObject);
begin
  for var I := 0 to lbPackages.Count - 1 do begin
    if not (lbPackages.ListItems[I].Enabled and lbPackages.ListItems[I].IsChecked) then
      Continue;

    Install(lbPackages.ListItems[I]);
  end;
  
  UpdateComponents();
end;

procedure TInstallItForm.Install(const AItem: TListBoxItem);
begin
  TInstallItListBoxItem(AItem).FProgressLayout.Visible := true;
  AItem.Height := ITEM_INSTALLING_HEIGHT;
  AItem.Enabled := false;

  FToolsInstaller.Install(
    Pointer(AItem.Data),
    procedure(const ATool: PToolInfo;
      const ACurrentAction: string; const ATotal, AStep: Int64)
    begin
      TThread.Queue(TThread.Current, procedure() begin
        TInstallItListBoxItem(AItem).FProgressText.Text := ACurrentAction;
        TInstallItListBoxItem(AItem).FProgressBar.Max := ATotal;
        TInstallItListBoxItem(AItem).FProgressBar.Value := AStep;
      end);
    end,
    procedure(const ACallback: IAsyncResult) begin 
      TThread.Queue(TThread.Current, procedure() begin 
        AItem.Enabled := not FToolsInstaller.IsInstalled(Pointer(AItem.Data));
        UpdateComponents();
      end);      
    end);
end;

procedure TInstallItForm.lblReadTermsClick(Sender: TObject);
begin
  for var I := 0 to lbPackages.Count - 1 do begin
    var LTermsOfUse := PToolInfo(Pointer(lbPackages.ListItems[I].Data)).TermsOfUse;
    if LTermsOfUse.IsEmpty() then
        Continue;

    if lbPackages.ListItems[I].IsChecked then
      TURL.Open(PToolInfo(Pointer(lbPackages.ListItems[I].Data)).TermsOfUse);
  end;
end;

procedure TInstallItForm.lblReadTermsMouseEnter(Sender: TObject);
begin
  lblReadTerms.Font.Style := lblReadTerms.Font.Style + [TFontStyle.fsUnderline];
end;

procedure TInstallItForm.lblReadTermsMouseLeave(Sender: TObject);
begin
  lblReadTerms.Font.Style := lblReadTerms.Font.Style - [TFontStyle.fsUnderline];
end;

procedure TInstallItForm.lbPackagesChangeCheck(Sender: TObject);
begin
  UpdateComponents();
end;

procedure TInstallItForm.ListTools;
begin
  lbPackages.Clear();
  for var LTool in FToolsInstaller.GetTools() do begin
    var LItem := TInstallItListBoxItem.Create(lbPackages);
    LItem.Height := ITEM_REGULAR_HEIGHT;
    LItem.FText.Text := LTool^.Description;
    LItem.FDetail.Text := 'Version: ' + LTool^.Version;
    LItem.Data := Pointer(LTool);

    if FToolsInstaller.IsInstalled(LTool) then begin
      LItem.ItemData.Accessory := TListBoxItemData.TAccessory.aCheckmark;
      LItem.IsChecked := true;
      LItem.Enabled := false;
      LItem.Hint := 'Installed';
    end else begin
      LItem.ItemData.Accessory := TListBoxItemData.TAccessory.aNone;
      LItem.IsChecked := false;
      LItem.Enabled := true;
      LItem.Hint := 'Install';
    end;

    lbPackages.AddObject(LItem);
  end;

  UpdateComponents();
end;

procedure TInstallItForm.UpdateComponents;
begin
  var LHasSelection := false;
  for var I := 0 to lbPackages.Count - 1 do
    if (lbPackages.ListItems[I].Enabled and lbPackages.ListItems[I].IsChecked) then
      LHasSelection := true;

  var LIsInstalling := false;
  for var LItem in FToolsInstaller.GetInstallingTools() do
    if not (LItem.Value.IsCancelled or LItem.Value.IsCompleted) then
      LIsInstalling := true;
    
  btnInstallSelectedTools.Enabled := cbAgree.IsChecked
    and LHasSelection
    and not LIsInstalling;  
end;

{ TURL }

class procedure TURL.Open(const AUrl: string);
begin
{$IFDEF MSWINDOWS}
  ShellExecute(0, 'OPEN', PChar(AUrl), '', '', SW_SHOWNORMAL);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  _system(PAnsiChar('open ' + AnsiString(AUrl)));
{$ENDIF POSIX}
end;

end.
