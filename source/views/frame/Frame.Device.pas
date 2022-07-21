unit Frame.Device;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ListBox, FMX.Layouts, System.Threading,
  Builder.Services, Builder.Chain;

type
  TDeviceFrame = class(TFrame)
    aiDevice: TAniIndicator;
    cbDevice: TComboBox;
    btnRefreshDevice: TSpeedButton;
    procedure btnRefreshDeviceClick(Sender: TObject);
    procedure cbDeviceChange(Sender: TObject);
  private
    FDevicesMonitor: TThread;
    FRunning: boolean;
    FUpdate: boolean;
    FDevices: TStrings;
    FAdbServices: IAdbServices;
    FAdbPath: string;
    FDebugSessionStarted: IDisconnectable;
    FDebugSessionStopped: IDisconnectable;
    procedure LoadDevices();

    procedure StartDevicesMonitor();
    procedure StopDevicesMonitor();
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    procedure CheckSelectedDevice();
    function GetSelectedDeviceName(): string;
  end;

implementation

uses
  Builder.Storage.Factory,
  Builder.Services.Factory;

{$R *.fmx}

{ TDeviceFrame }

constructor TDeviceFrame.Create(AOwner: TComponent);
begin
  inherited;
  FDevices := TStringList.Create();
  FAdbServices := TServiceSimpleFactory.CreateAdb();
  StartDevicesMonitor();
  LoadDevices();

  FDebugSessionStarted := TGlobalBuilderChain.SubscribeToEvent<TDebugSessionStartedEvent>(
    procedure(const AEventNotification: TDebugSessionStartedEvent)
    begin
      TThread.Queue(TThread.Current,
        procedure()
        begin
          btnRefreshDevice.Enabled := false;
          cbDevice.Enabled := false;
        end);
    end);

  FDebugSessionStopped := TGlobalBuilderChain.SubscribeToEvent<TDebugSessionStoppedEvent>(
    procedure(const AEventNotification: TDebugSessionStoppedEvent)
    begin
      TThread.Queue(TThread.Current,
        procedure()
        begin
          btnRefreshDevice.Enabled := true;
          cbDevice.Enabled := true;
        end);
    end);
end;

destructor TDeviceFrame.Destroy;
begin
  StopDevicesMonitor();
  FDevices.Free();
  inherited;
end;

procedure TDeviceFrame.btnRefreshDeviceClick(Sender: TObject);
begin
  LoadDevices();
end;

procedure TDeviceFrame.cbDeviceChange(Sender: TObject);
begin
  FAdbServices.ActiveDevice := GetSelectedDeviceName();
end;

procedure TDeviceFrame.CheckSelectedDevice;
begin
  if cbDevice.ItemIndex < 0 then
    raise Exception.Create('Select a device.');
end;

function TDeviceFrame.GetSelectedDeviceName: string;
begin
  if (cbDevice.ItemIndex < 0) then
    Result := String.Empty
  else
    Result := FDevices.Names[cbDevice.ItemIndex];
end;

procedure TDeviceFrame.LoadDevices;
begin
  //Users might update environment settings while task is running
  var LStorage := TStorageSimpleFactory.CreateEnvironment();
  FAdbPath := LStorage.GetAdbPath();
  FDevices.Clear();
  cbDevice.Clear();
  aiDevice.Enabled := true;
  aiDevice.Visible := true;
  btnRefreshDevice.Enabled := false;
  FUpdate := true;
end;

procedure TDeviceFrame.StartDevicesMonitor;
begin
  FRunning := true;
  FDevicesMonitor := TThread.CreateAnonymousThread(procedure begin
    try
      while FRunning do begin
        { TODO : Listen to devices updates }
        //Let's listen to user's update request
        if FUpdate and not FAdbPath.IsEmpty() then begin
          try
            FAdbServices.ListDevices(FAdbPath, FDevices);
            TThread.Queue(TThread.Current,
              procedure
              begin
                for var I := 0 to FDevices.Count - 1 do
                  cbDevice.Items.Add(FDevices.ValueFromIndex[I]);

                if (cbDevice.Count > 0)  then
                  cbDevice.ItemIndex := 0;

                aiDevice.Enabled := false;
                aiDevice.Visible := false;
                btnRefreshDevice.Enabled := true;
              end);
          finally
            FUpdate := false;
          end;
        end;
        Sleep(100);
      end;
    finally
      TThread.RemoveQueuedEvents(TThread.Current);
    end;
  end);
  FDevicesMonitor.FreeOnTerminate := false;
  FDevicesMonitor.Start();
end;

procedure TDeviceFrame.StopDevicesMonitor;
begin
  FRunning := false;
  FDevicesMonitor.WaitFor();
  FDevicesMonitor.Free();
end;

end.
