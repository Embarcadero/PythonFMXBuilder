unit Frame.Device;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ListBox, FMX.Layouts, System.Threading,
  Builder.Services;

type
  TDeviceFrame = class(TFrame)
    aiDevice: TAniIndicator;
    cbDevice: TComboBox;
    btnRefreshDevice: TSpeedButton;
    procedure btnRefreshDeviceClick(Sender: TObject);
    procedure cbDeviceChange(Sender: TObject);
  private
    FTask: ITask;
    FDevices: TStrings;
    FAdbServices: IAdbServices;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    procedure LoadDevices();
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
end;

destructor TDeviceFrame.Destroy;
begin
  if Assigned(FTask) then begin
    FTask.Cancel();
    try
      while not FTask.Wait(100)do begin
        Application.ProcessMessages();
      end;
    except
      on E: EOperationCancelled do begin
        //
      end;
      on E: Exception do
        raise;
    end;
  end;
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
  if cbDevice.ItemIndex < 0 then
    Result := String.Empty
  else
    Result := FDevices.Names[cbDevice.ItemIndex];
end;

procedure TDeviceFrame.LoadDevices;
begin
  FDevices.Clear();
  cbDevice.Clear();
  aiDevice.Enabled := true;
  aiDevice.Visible := true;
  btnRefreshDevice.Enabled := false;
  FTask := TTask.Run(procedure begin
    var LStorage := TStorageSimpleFactory.CreateEnvironment();
    try
      var LAdbPath := LStorage.GetAdbPath();

      if not LAdbPath.IsEmpty() then
        FAdbServices.ListDevices(LAdbPath, FDevices);

      FTask.CheckCanceled();
      TThread.Synchronize(nil, procedure begin
        for var I := 0 to FDevices.Count - 1 do
          cbDevice.Items.Add(FDevices.ValueFromIndex[I]);

        if (cbDevice.Count > 0)  then
          cbDevice.ItemIndex := 0;
      end);
    finally
      FTask.CheckCanceled();
      TThread.Queue(nil, procedure begin
        aiDevice.Enabled := false;
        aiDevice.Visible := false;
        btnRefreshDevice.Enabled := true;
      end);
      FTask := nil;
    end;
  end);
end;

end.
