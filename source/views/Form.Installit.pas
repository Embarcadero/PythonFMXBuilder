unit Form.Installit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListBox, FMX.Layouts, FMX.Controls.Presentation,
  Builder.Services, Builder.Message, Builder.Messagery;

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
    procedure FormDestroy(Sender: TObject);
  private
    FInstallItService: IInstallItServices;
    FInstalling: TArray<IAsyncResult>;
    procedure UpdateComponents();
    procedure Install(const AItem: TListBoxItem);
    function IsInstalling(): boolean;
    procedure CancellAll();
    procedure WaitForAll();
    function AskUserToCancelAll(): boolean;
  public
    constructor Create(AOwner: TComponent); override;

    procedure ListTools();
  end;

var
  InstallItForm: TInstallItForm;

implementation

uses
  System.SyncObjs,
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
  ITEM_REGULAR_HEIGHT = 60;
  ITEM_INSTALLING_HEIGHT = 90;

{$R *.fmx}

{ TInstallItListBoxItem }

constructor TInstallItListBoxItem.Create(AOwner: TComponent);
begin
  inherited;
  // TODO: MUST MAKE THIS A STYLE
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
  FInstallItService := TBuilderService.CreateService<IInstallItServices>();
end;

function TInstallItForm.IsInstalling: boolean;
begin
  Result := false;
  for var LTool in FInstalling do
    if not (LTool.IsCancelled or LTool.IsCompleted) then
      Exit(true);
end;

procedure TInstallItForm.CancellAll;
begin
  for var LTask in FInstalling do
    if not (LTask.IsCancelled or LTask.IsCompleted) then
      LTask.Cancel();
end;

procedure TInstallItForm.WaitForAll;
begin
  // TODO: SHOW ANI HERE
  for var LTask in FInstalling do
    try
      FInstallItService.EndInstall(LTask);
    except
      //
    end;

  TThread.RemoveQueuedEvents(Pointer(Self));
end;

function TInstallItForm.AskUserToCancelAll: boolean;
begin
  var LResult := false;
  TDialogService.MessageDialog('Do you want to cancel all running tasks?',
    TMsgDlgType.mtConfirmation,
    [TMsgDlgBtn.mbNo, TMsgDlgBtn.mbYes],
    TMsgDlgBtn.mbNo,
    0,
    procedure(const AResult: TModalResult) begin
      if AResult <> mrYes then
        Exit;

      ShowMessage('Cancelling operations. It can take a while...');

      CancellAll();
      WaitForAll();

      LResult := true;
    end);

  Result := LResult;
end;

procedure TInstallItForm.cbAgreeChange(Sender: TObject);
begin
  UpdateComponents();
end;

procedure TInstallItForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if IsInstalling() then
    CanClose := AskUserToCancelAll();
end;

procedure TInstallItForm.FormDestroy(Sender: TObject);
begin
  // There's a very old bug in MacOS preventing modal forms
  // to call the OnClose and onCloseQuery events
  CancellAll();
  WaitForAll();
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
var
  LItem: TInstallItListBoxItem absolute AItem;
  LTool: IAsyncResult;
begin
  LItem.FProgressLayout.Visible := true;
  LItem.Height := ITEM_INSTALLING_HEIGHT;
  LItem.Enabled := false;

  // DO NOT SYNCHRONIZE HERE
  // It can cause a deadlock with WaitForAll

  LTool := FInstallItService.BeginInstall(
    Pointer(AItem.Data),
    procedure(const ATool: PToolInfo;
      const ACurrentAction: string; const ATotal, AStep: Int64)
    begin
      if (LItem.FProgressText.Text = ACurrentAction)
         and
         (LItem.FProgressBar.Value = AStep)
      then
        Exit;

      TThread.Queue(Pointer(Self), procedure() begin
        LItem.FProgressText.Text := ACurrentAction;
        LItem.FProgressBar.Max := ATotal;
        LItem.FProgressBar.Value := AStep;
      end);
    end,
    procedure(const ACallback: IAsyncResult) begin
      if ACallback.IsCancelled then
        Exit;

      TThread.Queue(Pointer(Self), procedure() begin
        AItem.Enabled := not FInstallItService.IsInstalled(Pointer(LItem.Data));
        UpdateComponents();

        // Show errors
        if not ACallback.IsCancelled then
          FInstallItService.EndInstall(ACallback);
      end);
    end);
  
  FInstalling := FInstalling + [LTool];
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
  for var LTool in FInstallItService.GetTools() do begin
    var LItem := TInstallItListBoxItem.Create(lbPackages);
    LItem.Height := ITEM_REGULAR_HEIGHT;
    LItem.FText.Text := LTool^.Description;
    LItem.FDetail.Text := 'Version: ' + LTool^.Version;
    LItem.Data := Pointer(LTool);

    if FInstallItService.IsInstalled(LTool) then begin
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
  for var LItem in FInstalling do
    if not (LItem.IsCancelled or LItem.IsCompleted) then
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
