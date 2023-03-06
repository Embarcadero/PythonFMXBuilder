unit Frame.Log;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  Builder.Chain;

type
  TLogFrame = class(TFrame)
    mmLog: TMemo;
    tbFrame: TToolBar;
    lbDescription: TLabel;
  private
    FMessageEvent: IDisconnectable    ;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  end;

implementation

const
  CMD_DETAILS_REF_LINK =
    'Check out for more command details:' + #13#10
  + 'http://delphi.org/2013/11/installing-and-running-android-apps-from-command-line/';

{$R *.fmx}

{ TLogFrame }

constructor TLogFrame.Create(AOwner: TComponent);
begin
  inherited;
  FMessageEvent := TGlobalBuilderChain.SubscribeToEvent<TMessageEvent>(
    procedure(const AEventNotification: TMessageEvent)
    begin
      var LClear := AEventNotification.Body.Clear;
      var LMessage := AEventNotification.Body.Message;

      TThread.Queue(TThread.Current,
        procedure()
        begin
          if LClear then
            mmLog.Lines.Clear();

          if mmLog.Lines.Count = 0 then begin
            mmLog.Lines.Add(CMD_DETAILS_REF_LINK);
            mmLog.Lines.Add(String.Empty);
          end;

          if not LMessage.IsEmpty() then
            mmLog.Lines.Append(LMessage);

          mmLog.GoToTextEnd();
          mmLog.GoToLineBegin();
        end)
    end);
end;

destructor TLogFrame.Destroy;
begin
  FMessageEvent.Disconnect();
  inherited;
end;

end.
