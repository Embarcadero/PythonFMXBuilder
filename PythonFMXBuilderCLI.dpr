program PythonFMXBuilderCLI;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  VSoft.CommandLine.Options,
  Cli.Options in 'source\cli\Cli.Options.pas',
  Cli.Commands in 'source\cli\Cli.Commands.pas',
  Cli.Interpreter in 'source\cli\Cli.Interpreter.pas',
  Services.ADB in 'source\services\Services.ADB.pas',
  Services.App in 'source\services\Services.App.pas',
  Services.Factory in 'source\services\Services.Factory.pas',
  Services in 'source\services\Services.pas',
  Services.Project in 'source\services\Services.Project.pas',
  Model.Environment in 'source\models\Model.Environment.pas',
  Model in 'source\models\Model.pas',
  Model.Project.Files in 'source\models\project\Model.Project.Files.pas',
  Model.Project.Icon in 'source\models\project\Model.Project.Icon.pas',
  Model.Project in 'source\models\project\Model.Project.pas',
  Storage.Default in 'source\storage\Storage.Default.pas',
  Storage.Environment in 'source\storage\Storage.Environment.pas',
  Storage.Factory in 'source\storage\Storage.Factory.pas',
  Storage.Json in 'source\storage\Storage.Json.pas',
  Storage in 'source\storage\Storage.pas',
  Architecture in 'source\Architecture.pas',
  PythonVersion in 'source\PythonVersion.pas';

begin
  try
    TCommandInterpreter.Interpret(TOptionsRegistry.Parse());
    readln;
  except
    on E: EAbort do begin
      //
    end;
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
