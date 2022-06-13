unit Builder.Services.ADB.Win;

interface

uses
  System.Classes;

procedure ExecCmdine(const CmdLine, ABaseDir: string; CmdResult: TStrings);

implementation

uses
  Winapi.Windows, System.SysUtils;

procedure ExecCmdine(const CmdLine, ABaseDir: string; CmdResult: TStrings);
const
  READ_BUFFER_SIZE = 2400;
var
  Security: TSecurityAttributes;
  readableEndOfPipe, writeableEndOfPipe: THandle;
  start: TStartUpInfo;
  ProcessInfo: TProcessInformation;
  Buffer: PAnsiChar;
  BytesRead: DWORD;
  AppRunning: DWORD;
  dwBytesAvailable: integer;
begin
  Security.nLength := SizeOf(TSecurityAttributes);
  Security.bInheritHandle := True;
  Security.lpSecurityDescriptor := nil;

  if CreatePipe(readableEndOfPipe, writeableEndOfPipe, @Security, 0) then begin
    Buffer := AllocMem(READ_BUFFER_SIZE+1);
    FillChar(Start, Sizeof(Start), #0);
    start.cb := SizeOf(start);

    start.dwFlags := start.dwFlags or STARTF_USESTDHANDLES;
    start.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
    start.hStdOutput := writeableEndOfPipe;
    start.hStdError := writeableEndOfPipe;

    start.dwFlags := start.dwFlags + STARTF_USESHOWWINDOW;
    start.wShowWindow := SW_HIDE;

    ProcessInfo := Default(TProcessInformation);

    var LCmd := CmdLine;
    UniqueString(LCmd);

    var LBaseDir: PChar := nil;
    if not ABaseDir.IsEmpty() then
      LBaseDir := PChar(ABaseDir);

    if CreateProcess(nil, PChar(LCmd), nil, nil, True, NORMAL_PRIORITY_CLASS, nil, LBaseDir, start, ProcessInfo) then begin
      repeat
        Apprunning := WaitForSingleObject(ProcessInfo.hProcess, 100);
      until (Apprunning <> WAIT_TIMEOUT);

      repeat
        BytesRead := 0;
        if PeekNamedPipe(readableEndOfPipe, nil, 0, nil, @dwBytesAvailable, nil) then begin
          if dwBytesAvailable > 0 then begin
            ReadFile(readableEndOfPipe, Buffer[0], READ_BUFFER_SIZE, BytesRead, nil);
            Buffer[BytesRead]:= #0;
            OemToAnsi(Buffer,Buffer);
            CmdResult.Text := CmdResult.text + string(Buffer);
          end;
        end;
      until (BytesRead < READ_BUFFER_SIZE);
    end;
    FreeMem(Buffer);
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
    CloseHandle(readableEndOfPipe);
    CloseHandle(writeableEndOfPipe);
  end;
end;

end.
