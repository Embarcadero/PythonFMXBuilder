unit Services.ADB.Posix;

interface

uses
  System.Classes;

procedure ExecCmdine(const CmdLine, ABaseDir: string; CmdResult: TStrings);

implementation

uses
  Posix.SysMMan, System.SysUtils;

const
  libc = '/usr/lib/libc.dylib';

type
  PIOFile = Pointer;

//Create a new stream connected to a pipe running the given command.
function popen(const Command: PAnsiChar; Modes: PAnsiChar): PIOFile; cdecl;
  external libc name '_popen';

//Close a stream opened by popen and return the status of its child.
function pclose(Stream: PIOFile): Integer; cdecl; external libc name '_pclose';

//Return the EOF indicator for STREAM.
function feof(Stream: PIOFile): Integer; cdecl; external libc name '_feof';

//Read chunks of generic data from STREAM.
function fread(Ptr: Pointer; Size: LongWord; N: LongWord;
  Stream: PIOFile): LongWord; cdecl; external libc name '_fread';

//Wait for a child to die.  When one does, put its status in *STAT_LOC
//and return its process ID.  For errors, return (pid_t) -1.
function wait(__stat_loc: PInteger): Integer; cdecl;
  external libc name '_wait';

procedure ExecCmdine(const CmdLine, ABaseDir: string; CmdResult: TStrings);
var
  Output: PIOFile;
  Buffer: PAnsiChar;
  TempString: string;
  Line: string;
  BytesRead: Integer;
const
  BufferSize: Integer = 1000;
begin
  TempString := '';

  var LCurDir := GetCurrentDir();
  try
    if not ABaseDir.IsEmpty() then
      SetCurrentDir(ABaseDir);

    Output := popen(PAnsiChar(Ansistring(CmdLine)), 'r');
    GetMem(Buffer, BufferSize);
    if Assigned(Output) then
    try
      while feof(Output) = 0 do
      begin
        BytesRead := fread(Buffer, 1, BufferSize, Output);
        SetLength(TempString, Length(TempString) + BytesRead);
        Move(Buffer^, TempString[length(TempString) - (BytesRead - 1)], BytesRead);

        while Pos(#10, TempString) > 0 do
        begin
          Line := Copy(TempString, 1, Pos(#10, TempString) - 1);
          if CmdResult <> nil then
            CmdResult.Add(UTF8ToString(Line));

          TempString := Copy(TempString, Pos(#10, TempString) + 1, Length(TempString));
        end;
      end;
    finally
      pclose(output);
      wait(nil);
      FreeMem(Buffer, BufferSize);
    end;
  finally
    SetCurrentDir(LCurDir);
  end;
end;

end.
