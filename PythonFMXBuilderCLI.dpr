program PythonFMXBuilderCLI;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  VSoft.CommandLine.Options,
  Cli.Options in 'source\cli\Cli.Options.pas',
  Cli.Commands in 'source\cli\Cli.Commands.pas';

procedure PrintUsage(const ACommand: string);
begin
  if (ACommand = 'help') then begin
    if THelpOptions.HelpCommand = '' then
      THelpOptions.HelpCommand := 'help';
  end else
    THelpOptions.HelpCommand := ACommand;

  TOptionsRegistry.PrintUsage(THelpOptions.HelpCommand,
    procedure(const AValue: string) begin
      if THelpOptions.HelpCommand.IsEmpty() and AValue.Trim().IsEmpty() then
        Exit;
      WriteLn(AValue);
    end);
end;

procedure ExecuteCommand(const ACommand: string);
begin
  WriteLn('Will execute command : ' + ACommand + '');
end;

begin
  try
    var LResult := TOptionsRegistry.Parse;
    if LResult.HasErrors then begin
      Writeln;
      Writeln(LResult.ErrorText);
      PrintUsage(LResult.Command);
    end else begin
      if (LResult.Command = '') or (LResult.Command = 'help') then
        PrintUsage(LResult.Command)
      else
        ExecuteCommand(LResult.Command);
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
