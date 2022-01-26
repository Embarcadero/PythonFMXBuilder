(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'PythonLoad'    Copyright (c) 2021                       *)
(*                                                                        *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(**************************************************************************)
(*  Functionality:  Load python distribution                              *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)
(* This source code is distributed with no WARRANTY, for no reason or use.*)
(* Everyone is allowed to use and change this code free for his own tasks *)
(* and projects, as long as this header and its copyright text is intact. *)
(* For changed versions of this code, which are public distributed the    *)
(* following additional conditions have to be fullfilled:                 *)
(* 1) The header has to contain a comment on the change and the author of *)
(*    it.                                                                 *)
(* 2) A copy of the changed source has to be sent to the above E-Mail     *)
(*    address or my then valid address, if this is possible to the        *)
(*    author.                                                             *)
(* The second condition has the target to maintain an up to date central  *)
(* version of the component. If this condition is not acceptable for      *)
(* confidential or legal reasons, everyone is free to derive a component  *)
(* or to generate a diff file to my or other original sources.            *)
(**************************************************************************)
unit PythonLoad;

interface

uses
  System.SysUtils, System.Zip, PythonEngine;

type
  TExtractEvent = reference to procedure(const AFolderExists: boolean; var AReplaceFiles: boolean);
  TPythonLoad = class
  public
    class function FindPythonVer(): integer; static;
    class function GetPyZip(): string; static;
    class function GetPyRoot(): string; static;
    class function GetPyHome(): string; static;
    class function GetPyBin(): string; static;
    class function GetPyLib(): string; static;
    class function GetPyTmp(): string; static;
    class function GetPyInterpreter(): string; static;

    class procedure Extract(const AExtractEvent: TExtractEvent;
      const AZipProgress: TZipProgressEvent); static;
    class procedure Configure(const APythonEngine: TPythonEngine;
      const ACheckPyLib: boolean = true); static;
  end;

implementation

uses
  System.IOUtils,
  Androidapi.Helpers,
  Androidapi.JNI.JavaTypes;

{ TPythonLoad }

class procedure TPythonLoad.Extract(const AExtractEvent: TExtractEvent;
  const AZipProgress: TZipProgressEvent);
begin
  var LPyRoot := TPythonLoad.GetPyRoot();
  var LFolderExists := TDirectory.Exists(LPyRoot);
  var LReplaceFiles := false;

  if Assigned(AExtractEvent) then
    AExtractEvent(LFolderExists, LReplaceFiles);

  if LFolderExists then
    if not LReplaceFiles then
      Exit()
    else
      TDirectory.Delete(LPyRoot, true);

  var LPyZip := TPythonLoad.GetPyZip();
  if not TFile.Exists(LPyZip) then
    raise Exception.Create('Python compressed distribution not found.');

  TZipFile.ExtractZipFile(LPyZip, LPyRoot, AZipProgress);
end;

class function TPythonLoad.FindPythonVer: integer;
begin
  if TFile.Exists(TPath.Combine(TPath.GetLibraryPath(), 'libpython3.8.so')) then
    Result := 6
  else if TFile.Exists(TPath.Combine(TPath.GetLibraryPath(), 'libpython3.9.so')) then
    Result := 7
  else if TFile.Exists(TPath.Combine(TPath.GetLibraryPath(), 'libpython3.10.so')) then
    Result := 8
  else
    raise Exception.Create('Python shared library not found');
end;

class function TPythonLoad.GetPyBin: string;
begin
  Result := TPath.Combine(GetPyHome(), 'bin');
end;

class function TPythonLoad.GetPyHome: string;
begin
  Result := TPath.Combine(GetPyRoot(), 'usr');
end;

class function TPythonLoad.GetPyInterpreter: string;
begin
  Result := TPath.Combine(TPath.GetLibraryPath(), 'python3.9')
end;

class function TPythonLoad.GetPyLib: string;
begin
  Result := TPath.Combine(GetPyHome(), 'lib');
end;

class function TPythonLoad.GetPyRoot: string;
begin
  Result := TPath.Combine(TPath.GetDocumentsPath(), 'build');
end;

class function TPythonLoad.GetPyTmp: string;
begin
  Result := TPath.GetTempPath();
end;

class function TPythonLoad.GetPyZip: string;
begin
  Result := TPath.Combine(TPath.GetDocumentsPath(), 'build.zip');
end;

class procedure TPythonLoad.Configure(const APythonEngine: TPythonEngine;
  const ACheckPyLib: boolean);
begin
  var LPythonVer := FindPythonVer();

  if ACheckPyLib then begin
    var LPyLibFile := TPath.Combine(TPath.GetLibraryPath(),
      PYTHON_KNOWN_VERSIONS[LPythonVer].DllName);

    if not TFile.Exists(LPyLibFile) then
      raise Exception.Create('Python library not found at application''s data folder.'
                            + #13#10
                            + LPyLibFile);
  end;

  APythonEngine.UseLastKnownVersion := false;
  APythonEngine.ProgramName := TPythonLoad.GetPyBin();
  APythonEngine.PythonHome := TPythonLoad.GetPyHome();
  APythonEngine.RegVersion := PYTHON_KNOWN_VERSIONS[LPythonVer].RegVersion;
  APythonEngine.DllName := PYTHON_KNOWN_VERSIONS[LPythonVer].DllName;
  APythonEngine.APIVersion := PYTHON_KNOWN_VERSIONS[LPythonVer].APIVersion;
end;

end.
