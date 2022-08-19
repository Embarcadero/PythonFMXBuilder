unit Frame.Editor.Memo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  Frame.Editor.TabItem;

type
  TMemoEditorFrame = class(TFrame, ITextEditor)
    mmEditor: TMemo;
  private
    FFileName: string;
    procedure LoadFromFile(const AFileName: string);
    procedure Save();
    function GetBreakpoints(): TArray<integer>;
    procedure SetBreakpoints(ABreakpoints: TArray<integer>);
    function GetActiveLine(): integer;
    procedure SetActiveLine(AActiveLine: integer);
    function GetShowActiveLine(): boolean;
    procedure SetShowActiveLine(AShowActiveLine: boolean);
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TMemoScriptEditorTabItem = class(TCustomEditorTabItem)
  private
    FEditor: TMemoEditorFrame;
  protected
    function GetTextEditor(): ITextEditor; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.fmx}

{ TMemoEditorFrame }

constructor TMemoEditorFrame.Create(AOwner: TComponent);
begin
  inherited;
end;

function TMemoEditorFrame.GetActiveLine: integer;
begin
  Result := 0;
end;

function TMemoEditorFrame.GetBreakpoints: TArray<integer>;
begin
  Result := [];
end;

function TMemoEditorFrame.GetShowActiveLine: boolean;
begin
  Result := false;
end;

procedure TMemoEditorFrame.LoadFromFile(const AFileName: string);
begin
  FFileName := AFileName;
  mmEditor.Lines.LoadFromFile(AFileName);
end;

procedure TMemoEditorFrame.Save;
begin
  if not FFileName.IsEmpty() then
    mmEditor.Lines.SaveToFile(FFileName);
end;

procedure TMemoEditorFrame.SetActiveLine(AActiveLine: integer);
begin
  //
end;

procedure TMemoEditorFrame.SetBreakpoints(ABreakpoints: TArray<integer>);
begin
  //
end;

procedure TMemoEditorFrame.SetShowActiveLine(AShowActiveLine: boolean);
begin
  //
end;

{ TMemoScriptEditorTabItem }

constructor TMemoScriptEditorTabItem.Create(AOwner: TComponent);
begin
  inherited;
  FEditor := TMemoEditorFrame.Create(Self);
  FEditor.Parent := Self;
  FEditor.Align := TAlignLayout.Client;
end;

function TMemoScriptEditorTabItem.GetTextEditor: ITextEditor;
begin
  Result := FEditor;
end;

//initialization
//  TScriptEditorTabItem.DefaultTabItemClass := TMemoScriptEditorTabItem;
//
//finalization
//  TScriptEditorTabItem.DefaultTabItemClass := nil;

end.
