unit Frame.Editor.TabItem;

interface

uses
  System.Rtti, System.Classes, System.Types, FMX.Types, FMX.Controls, FMX.TabControl,
  Builder.Types, Builder.Chain, Builder.TabControl;

type
  TControlClass = class of TControl;

  TEditorTabItem = class(TTabItem)
  private
    class var FDefaultEditorClass: TControlClass;
  private
    FEditor: TControl;
    //Events
    FSaveState: IDisconnectable;
    FRenameFile: IDisconnectable;
    FEditorChanged: IDisconnectable;
    procedure CreateEditor();
  protected
    function GetTextEditor(): ITextEditor;
    procedure InternalSave();
    procedure DoAskToSaveChanges();
    procedure DoClose(); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    procedure LoadFile(const AFileName: string; const AEditing: boolean);

    property TextEditor: ITextEditor read GetTextEditor;

    class property DefaultEditorClass: TControlClass
      read FDefaultEditorClass write FDefaultEditorClass;
  end;

implementation

uses
  System.SysUtils, System.IOUtils, System.UITypes,
  FMX.Dialogs, FMX.DialogService;

{ TEditorTabItem }

constructor TEditorTabItem.Create(AOwner: TComponent);
begin
  inherited;
  CreateEditor();
  FSaveState := TGlobalBuilderChain.SubscribeToEvent<TSaveStateEvent>(
    procedure(const AEventNotification: TSaveStateEvent)
    begin
      if (AEventNotification.Body.SaveState = TSaveState.Save) then begin
        if (TabControl.ActiveTab = Self) then
          InternalSave();
      end else if (AEventNotification.Body.SaveState = TSaveState.SaveAll) then
        InternalSave();
    end);
  FRenameFile := TGlobalBuilderChain.SubscribeToEvent<TRenameFileEvent>(
    procedure(const AEventNotification: TRenameFileEvent)
    begin
      if (TextEditor.FileName = AEventNotification.Body.OldFilePath) then begin
        TextEditor.SaveTo(AEventNotification.Body.NewFilePath);
        TThread.Synchronize(TThread.Current, procedure() begin
          LoadFile(AEventNotification.Body.NewFilePath, false);
        end);
      end;
    end);
  FEditorChanged := TGlobalBuilderChain.SubscribeToEvent<TEditorChangedEvent>(
    procedure(const AEventNotification: TEditorChangedEvent)
    begin
      if (AEventNotification.Body.TextEditor = TextEditor) then begin
        var LModified := AEventNotification.Body.Modified;
        TThread.Queue(TThread.Current, procedure() begin
          Modified := LModified;
        end);
      end;
    end);
end;

destructor TEditorTabItem.Destroy;
begin
  FEditorChanged.Disconnect();
  FRenameFile.Disconnect();
  FSaveState.Disconnect();
  inherited;
end;

procedure TEditorTabItem.CreateEditor;
begin
  FEditor := DefaultEditorClass.Create(Self);
  FEditor.Parent := Self;
  FEditor.Align := TAlignLayout.Client;
end;

function TEditorTabItem.GetTextEditor: ITextEditor;
begin
  Result := FEditor as ITextEditor;
end;

procedure TEditorTabItem.InternalSave;
begin
  TextEditor.Save();
end;

procedure TEditorTabItem.LoadFile(const AFileName: string;
  const AEditing: boolean);
begin
  Text := TPath.GetFileName(AFileName);
  TextEditor.Open(AFileName, AEditing);
end;

procedure TEditorTabItem.DoAskToSaveChanges;
begin
  IsSelected := true;
  TDialogService.MessageDialog(
    Format('Save changes to %s?', [TPath.GetFileName(TextEditor.FileName)]),
    TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbYes, 0,
    procedure(const AResult: TModalResult) begin
      if (AResult = mrYes) then begin
        TextEditor.Save();
        TGlobalBuilderChain.BroadcastEvent(
          TSaveStateEvent.Create(TSaveState.Save));
      end;
    end);
end;

procedure TEditorTabItem.DoClose;
begin
  //No need to replicate changes here when closing
  FEditorChanged.Disconnect();
  FRenameFile.Disconnect();
  FSaveState.Disconnect();
  //Ask user to save editor if modified
  if TextEditor.Modified then
    DoAskToSaveChanges();
  inherited;
end;

end.
