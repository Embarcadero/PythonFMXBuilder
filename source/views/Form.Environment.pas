unit Form.Environment;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Threading, System.Generics.Collections, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Objects, FMX.ListBox, FMX.Ani,
  FMX.Controls.Presentation, FMX.Edit, FMX.Layouts, FMX.ImgList, System.Actions,
  FMX.ActnList, Form.Data, Builder.Model.Environment.Android, FMX.Effects,
  FMX.Filter.Effects;

type
  [Entity(TAndroidEnvironmentModel)]
  TEnvironmentForm = class(TDataForm)
    lbghSdkttings: TListBoxGroupHeader;
    lbiSdkBasePath: TListBoxItem;
    edtSdkBasePath: TEdit;
    lbiJarSigner: TListBoxItem;
    edtJarSignerLocation: TEdit;
    lbiAdbLocation: TListBoxItem;
    edtAdbLocation: TEdit;
    lbiAaptLocation: TListBoxItem;
    edtAaptLocation: TEdit;
    lbiSdkApiLocation: TListBoxItem;
    lbiZipAlign: TListBoxItem;
    edtZipAlign: TEdit;
    lbiKeytoolLocation: TListBoxItem;
    edtKeyTool: TEdit;
    lblSdkBasePath: TLabel;
    lblJarSigner: TLabel;
    lblAdb: TLabel;
    lblAapt: TLabel;
    lblSdkApi: TLabel;
    lblZipAlign: TLabel;
    lblKeyTool: TLabel;
    lbiApkSignerLocation: TListBoxItem;
    edtApkSigner: TEdit;
    lblApkSigner: TLabel;
    lbghJdkSettings: TListBoxGroupHeader;
    lbiJdkBasePath: TListBoxItem;
    edtJdkBasePath: TEdit;
    lblJdkBasePath: TLabel;
    edtSdkApiLocation: TEdit;
    lbghDebuggerSettings: TListBoxGroupHeader;
    lbiRemoteDebuggerHost: TListBoxItem;
    edtRemoteDebuggerHost: TEdit;
    lblRemoteDebuggerHost: TLabel;
    lbiRemoteDebuggerPort: TListBoxItem;
    edtRemoteDebuggerPort: TEdit;
    lblRemoteDebuggerPort: TLabel;
    lbiRemoteDebuggerRoot: TListBoxItem;
    edtRemoteDebuggerRoot: TEdit;
    lblRemoteDebuggerRoot: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure edtSdkBasePathChange(Sender: TObject);
    procedure edtJdkBasePathChange(Sender: TObject);
  private
    FTasks: TList<ITask>;
    function CreateAni(const AControl: TPresentedControl): TAniIndicator;
    procedure LoadEditContent(const AEdit: TEdit; ATask: TFunc<string>);
    procedure LoadToolPath(const ABasePath, ATool: string; const AEdit: TEdit);
    procedure LoadSdkToolsPath(const ABasePath: string);
    procedure LoadJdkToolsPath(const ABasePath: string);
    function CanUpdatePaths(): boolean;
  protected
    procedure FormUpdate(); override;
    procedure ModelUpdate(); override;
  public
    class procedure CheckAndUpdateAndroidEnvironment();
  end;

var
  EnvironmentForm: TEnvironmentForm;

implementation

uses
  System.IOUtils,
  FMX.DialogService,
  Container.Images,
  Builder.Paths,
  Builder.Services,
  Form.Factory;

{$R *.fmx}

{ TEnvironmentForm }

procedure TEnvironmentForm.FormCreate(Sender: TObject);
begin
  inherited;
  FTasks := TList<ITask>.Create();
end;

procedure TEnvironmentForm.FormDestroy(Sender: TObject);
begin
  inherited;
  FTasks.Free();
end;

procedure TEnvironmentForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  inherited;
  TTask.WaitForAll(FTasks.ToArray());
end;

procedure TEnvironmentForm.edtSdkBasePathChange(Sender: TObject);
begin
  inherited;
  if not CanUpdatePaths() then
    Exit;

  if not edtSdkBasePath.Text.IsEmpty() then
    LoadSdkToolsPath(edtSdkBasePath.Text);
end;

procedure TEnvironmentForm.edtJdkBasePathChange(Sender: TObject);
begin
  inherited;
  if not CanUpdatePaths() then
    Exit;

  if not edtJdkBasePath.Text.IsEmpty() then
    LoadJdkToolsPath(edtJdkBasePath.Text);
end;

function TEnvironmentForm.CanUpdatePaths: boolean;
begin
  Result := ModelReady;
end;

function TEnvironmentForm.CreateAni(
  const AControl: TPresentedControl): TAniIndicator;
begin
  Result := TAniIndicator.Create(nil);
  Result.Visible := false;
  Result.Enabled := false;
  Result.Align := TAlignLayout.Right;
  with Result.Margins do begin
    Top := 1;
    Left := 1;
    Bottom := 1;
    Right := 1;
  end;
  Result.Parent := AControl;
  Result.Visible := true;
  Result.Enabled := true;
end;

procedure TEnvironmentForm.LoadEditContent(const AEdit: TEdit; ATask: TFunc<string>);
begin
  var LAni := CreateAni(AEdit);
  try
    AEdit.Enabled := false;
    FTasks.Add(
      TTask.Run(
        procedure()
        var
          LText: string;
        begin
          try
            try
              LText := ATask();
            finally
              TThread.Queue(TThread.Current,
                procedure()
                begin
                  AEdit.Text := LText;
                  LAni.Visible := false;
                  LAni.Enabled := false;
                  AEdit.Enabled := true;
                end);
            end;
          finally
            TThread.Queue(TThread.Current,
              procedure()
              begin
                LAni.Free();
              end);
          end;
        end));
  except
    on E: Exception do begin
      LAni.Free();
      raise;
    end;
  end;
end;

procedure TEnvironmentForm.LoadToolPath(const ABasePath, ATool: string;
  const AEdit: TEdit);
begin
  if not AEdit.Text.IsEmpty() then
    Exit;

  LoadEditContent(AEdit, function(): string begin
    Result := TAndroidToolsPathLocator.FindToolPath(ABasePath, ATool);
  end);
end;

procedure TEnvironmentForm.LoadSdkToolsPath(const ABasePath: string);
begin
  if not TDirectory.Exists(ABasePath) then
    Exit;

  LoadToolPath(edtSdkBasePath.Text, 'apksigner.jar', edtApkSigner);
  LoadToolPath(edtSdkBasePath.Text, {$IFDEF POSIX}'adb'{$ELSE}'adb.exe'{$ENDIF}, edtAdbLocation);
  LoadToolPath(edtSdkBasePath.Text, {$IFDEF POSIX}'aapt'{$ELSE}'aapt.exe'{$ENDIF}, edtAaptLocation);
  LoadToolPath(edtSdkBasePath.Text, {$IFDEF POSIX}'zipalign'{$ELSE}'zipalign.exe'{$ENDIF}, edtZipAlign);

  if edtSdkApiLocation.Text.IsEmpty() then
    LoadEditContent(edtSdkAPILocation, function(): string begin
      Result := TAndroidToolsPathLocator.FindSdkApiLocation(ABasePath);
    end);
end;

procedure TEnvironmentForm.LoadJdkToolsPath(const ABasePath: string);
begin
  if not TDirectory.Exists(ABasePath) then
    Exit;

  LoadToolPath(edtJdkBasePath.Text,
    {$IFDEF POSIX}'keytool'{$ELSE}'keytool.exe'{$ENDIF},
    edtKeyTool);
  LoadToolPath(edtJdkBasePath.Text,
    {$IFDEF POSIX}'jarsigner'{$ELSE}'jarsigner.exe'{$ENDIF},
    edtJarSignerLocation);
end;

procedure TEnvironmentForm.FormUpdate;
begin
  with Model as TAndroidEnvironmentModel do begin
    edtSdkBasePath.Text := SdkBasePath;
    edtApkSigner.Text := ApkSignerLocation;
    edtAdbLocation.Text := AdbLocation;
    edtAaptLocation.Text := AAptLocation;
    edtSdkAPILocation.Text := SdkApiLocation;
    edtZipAlign.Text := ZipAlignLocation;
    edtJdkBasePath.Text := JdkBasePath;
    edtKeyTool.Text := KeyToolLocation;
    edtJarSignerLocation.Text := JarSignerLocation;
    edtRemoteDebuggerHost.Text := RemoteDebuggerHost;
    edtRemoteDebuggerPort.Text := RemoteDebuggerPort.ToString();
    edtRemoteDebuggerRoot.Text := RemoteDebuggerRoot;
  end;
end;

procedure TEnvironmentForm.ModelUpdate;
begin
  with Model as TAndroidEnvironmentModel do begin
    SdkBasePath := edtSdkBasePath.Text;
    ApkSignerLocation := edtApkSigner.Text;
    AdbLocation := edtAdbLocation.Text;
    AAptLocation := edtAaptLocation.Text;
    SdkApiLocation := edtSdkAPILocation.Text;
    ZipAlignLocation := edtZipAlign.Text;
    JdkBasePath := edtJdkBasePath.Text;
    KeyToolLocation := edtKeyTool.Text;
    JarSignerLocation := edtJarSignerLocation.Text;
    RemoteDebuggerHost := edtRemoteDebuggerHost.Text;
    if RemoteDebuggerHost.Trim().IsEmpty() then
      RemoteDebuggerHost := '127.0.0.1';
    RemoteDebuggerPort := StrToIntDef(edtRemoteDebuggerPort.Text, 0);
    if (RemoteDebuggerPort = 0) then
      RemoteDebuggerPort := 5678;
    RemoteDebuggerRoot := edtRemoteDebuggerRoot.Text;
    if RemoteDebuggerRoot.Trim().IsEmpty() then
      RemoteDebuggerRoot := '/data/data/$(package_name)/files/';
  end;
end;

class procedure TEnvironmentForm.CheckAndUpdateAndroidEnvironment;
begin
  // Check if the Environment entity has been configured and is valid
  var LEnvironmentService := TBuilderService.CreateService<IEnvironmentServices<TAndroidEnvironmentModel>>;
  var LEnvironmentEntity := LEnvironmentService.GetActiveEnvironment();
  var LErrors := TStringList.Create();
  try
    if LEnvironmentEntity.Validate(LErrors) then
      Exit();
  finally
    LErrors.Free();
  end;

  // Ask user to install the Android tools
  TDialogService.MessageDialog(
    'It seems the Android Environment is not ready. Would you like to update it now?',
    TMsgDlgType.mtConfirmation,
    [TMsgDlgBtn.mbNo, TMsgDlgBtn.mbYes],
    TMsgDlgBtn.mbYes,
    0,
    procedure(const AResult: TModalResult) begin
      if (AResult <> mrYes) then
        Exit;

      // Check if tools are installed
      var LShouldInstall := false;
      var LInstallItService := TBuilderService.CreateService<IInstallItServices>;
      for var LTool in LInstallItService.GetMissingTools() do begin
        if (LTool.Name = 'jdk') or (LTool.Name = 'sdk') then
          LShouldInstall := true;
      end;

      // Show user the available tools
      if LShouldInstall then begin
        var LInstallIt := TFormSimpleFactory.CreateInstallIt();
        try
          LInstallIt.ShowModal();
        finally
          LInstallIt.Free();
        end;
      end;

      // Check if the JDK and SDK has been installed
      var LAllInstalled := true;
      for var LTool in LInstallItService.GetMissingTools() do begin
        if (LTool.Name = 'jdk') or (LTool.Name = 'sdk') then
          LAllInstalled := false;
      end;

      if not LAllInstalled then
        raise Exception.Create('The required tools have not been installed.');

      // Update the Environment entity
      for var LTool in LInstallItService.GetTools() do
        if (LTool.Name = 'jdk') then
          LEnvironmentEntity.UpdateJDKPaths(TBuilderPaths.GetToolInstallationFolder(LTool^))
        else if (LTool.Name = 'sdk') then
          LEnvironmentEntity.UpdateSDKPaths(TBuilderPaths.GetToolInstallationFolder(LTool^));
      LEnvironmentService.SaveEnvironment(LEnvironmentEntity);

      ShowMessage('The Android environment has been updated!');
    end);
end;


end.
