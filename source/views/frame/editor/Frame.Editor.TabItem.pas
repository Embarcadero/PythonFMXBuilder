unit Frame.Editor.TabItem;

interface

uses
  System.Rtti, System.Classes, System.Types, FMX.Types, FMX.Controls, FMX.TabControl,
  Builder.Types, Builder.Chain, Builder.TabControl;

type
  TControlClass = class of TControl;

  TEditorTabItem = class(TTabItem, ITextEditor)
  private
    class var FDefaultEditorClass: TControlClass;
  private
    FDelegator: ITextEditor;
    //Events
    FRenameFile: IDisconnectable;
    FEditorChanged: IDisconnectable;
    procedure CreateEditor();
    //ITextEditor implementation
    function GetFileName(): string;
    function GetModified(): boolean;
    function GetBreakpoints(): TArray<integer>;
    procedure SetBreakpoints(ABreakpoints: TArray<integer>);
    function GetActiveLine(): integer;
    procedure SetActiveLine(AActiveLine: integer);
    function GetShowActiveLine(): boolean;
    procedure SetShowActiveLine(AShowActiveLine: boolean);
    procedure Open(const AFileName: string; const AEditing: boolean = false);
    procedure Close();
    procedure Save();
    procedure SaveTo(const AFileName: string);
  protected
    procedure DoAskToSaveChanges();
    procedure DoClose(); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

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
  FRenameFile := TGlobalBuilderChain.SubscribeToEvent<TRenameFileEvent>(
    procedure(const AEventNotification: TRenameFileEvent)
    begin
      if (GetFileName() = AEventNotification.Body.OldFilePath) then begin
        SaveTo(AEventNotification.Body.NewFilePath);
        TThread.Synchronize(TThread.Current, procedure() begin
          Open(AEventNotification.Body.NewFilePath, false);
        end);
      end;
    end);
  FEditorChanged := TGlobalBuilderChain.SubscribeToEvent<TEditorChangedEvent>(
    procedure(const AEventNotification: TEditorChangedEvent)
    begin
      if (AEventNotification.Body.TextEditor = FDelegator) then begin
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
  FDelegator := nil;
  inherited;
end;

procedure TEditorTabItem.Close;
begin
  inherited Close();
end;

procedure TEditorTabItem.CreateEditor;
begin
  var LEditor := DefaultEditorClass.Create(Self);
  LEditor.Parent := Self;
  LEditor.Align := TAlignLayout.Client;
  FDelegator := LEditor as ITextEditor;
end;

procedure TEditorTabItem.DoAskToSaveChanges;
begin
  IsSelected := true;
  TDialogService.MessageDialog(
    Format('Save changes to %s?', [TPath.GetFileName(GetFileName())]),
    TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbYes, 0,
    procedure(const AResult: TModalResult) begin
      if (AResult = mrYes) then begin
        Save();
        //Ask someone to tell us what to do
        TGlobalBuilderChain.SendRequest<TEditorSaveResponse>(
          TEditorSaveRequest.Create(Self as ITextEditor),
          procedure(const AArg: TEditorSaveResponse)
          begin
            //
          end,
          procedure(const AArg: string)
          begin
            //
          end);
      end;
    end);
end;

procedure TEditorTabItem.DoClose;
begin
  //No need to replicate changes here when closing
  FEditorChanged.Disconnect();
  FRenameFile.Disconnect();
  //Ask user to save editor if modified
  if GetModified() then
    DoAskToSaveChanges();
  inherited;
end;

//ITextEditor delegator

procedure TEditorTabItem.SetActiveLine(AActiveLine: integer);
begin
  FDelegator.SetActiveLine(AActiveLine);
end;

procedure TEditorTabItem.SetBreakpoints(ABreakpoints: TArray<integer>);
begin
  FDelegator.SetBreakpoints(ABreakpoints);
end;

procedure TEditorTabItem.SetShowActiveLine(AShowActiveLine: boolean);
begin
  FDelegator.SetShowActiveLine(AShowActiveLine);
end;

function TEditorTabItem.GetActiveLine: integer;
begin
  Result := FDelegator.GetActiveLine();
end;

function TEditorTabItem.GetBreakpoints: TArray<integer>;
begin
  Result := FDelegator.GetBreakpoints();
end;

function TEditorTabItem.GetFileName: string;
begin
  Result := FDelegator.GetFileName();
end;

function TEditorTabItem.GetModified: boolean;
begin
  Result := FDelegator.GetModified();
end;

function TEditorTabItem.GetShowActiveLine: boolean;
begin
  Result := FDelegator.GetShowActiveLine();
end;

procedure TEditorTabItem.Save;
begin
  FDelegator.Save();
end;

procedure TEditorTabItem.SaveTo(const AFileName: string);
begin
  FDelegator.SaveTo(AFileName);
end;

procedure TEditorTabItem.Open(const AFileName: string; const AEditing: boolean);
begin
  Text := TPath.GetFileName(AFileName);
  FDelegator.Open(AFileName, AEditing);
end;

end.
