program PythonFMXBuilderCLI;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  VSoft.CommandLine.Options,
  Cli.Options in 'source\cli\Cli.Options.pas',
  Cli.Commands in 'source\cli\Cli.Commands.pas',
  Cli.Interpreter in 'source\cli\Cli.Interpreter.pas',
  Cli.Exception in 'source\cli\Cli.Exception.pas';

begin
  try
    TCommandInterpreter.Interpret(TOptionsRegistry.Parse());
  except
    on E: EAbort do begin
      //
    end;
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
