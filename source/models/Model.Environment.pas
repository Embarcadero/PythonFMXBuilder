unit Model.Environment;

interface

uses
  System.Classes, REST.Json.Types, Model;

type
  [Model('environment')]
  TEnvironmentModel = class(TModel)
  private
    [JSONName('sdk_base_path')]
    FSdkBasePath: string;
    [JSONName('apk_signer_location')]
    FJarSignerLocation: string;
    [JSONName('adb_location')]
    FAdbLocation: string;
    [JSONName('apt_location')]
    FAAptLocation: string;
    [JSONName('sdk_api_location')]
    FSdkApiLocation: string;
    [JSONName('zip_align_location')]
    FZipAlignLocation: string;
    [JSONName('key_tool_location')]
    FKeyToolLocation: string;
  public
    function Validate(const AErrors: TStrings): boolean; override;
  public
    property SdkBasePath: string read FSdkBasePath write FSdkBasePath;
    property JarSignerLocation: string read FJarSignerLocation write FJarSignerLocation;
    property AdbLocation: string read FAdbLocation write FAdbLocation;
    property AAptLocation: string read FAAptLocation write FAAptLocation;
    property SdkApiLocation: string read FSdkApiLocation write FSdkApiLocation;
    property ZipAlignLocation: string read FZipAlignLocation write FZipAlignLocation;
    property KeyToolLocation: string read FKeyToolLocation write FKeyToolLocation;
  end;

implementation

uses
  System.SysUtils, System.IOUtils;

{ TEnvironmentModel }

function TEnvironmentModel.Validate(const AErrors: TStrings): boolean;
begin
  AErrors.Clear();

  {|||||| CHECK FOR PATHS |||||||}
  if FSdkBasePath.Trim().IsEmpty() then
    AErrors.Add('* The SDK base location can not be empty.');

  if FJarSignerLocation.Trim().IsEmpty() then
    AErrors.Add('* The APK signer location can not be empty.');

  if FAdbLocation.Trim().IsEmpty() then
    AErrors.Add('* The ADB location can not be empty.');

  if FAAptLocation.Trim().IsEmpty() then
    AErrors.Add('* The AAPT location can not be empty.');

  if FSdkApiLocation.Trim().IsEmpty() then
    AErrors.Add('* The SDK API location can not be empty.');

  if FZipAlignLocation.Trim().IsEmpty() then
    AErrors.Add('* The ZipAlign location can not be empty.');

  if FKeyToolLocation.Trim().IsEmpty() then
    AErrors.Add('* The KeyTool location can not be empty.');

  {|||||| CHECK FOR VALID FILES |||||||}
  if not TFile.Exists(FJarSignerLocation) then
    AErrors.Add('* JARSigner tool not found.');

  if not TFile.Exists(FAdbLocation) then
    AErrors.Add('* ADB tool not found.');

  if not TFile.Exists(FAAptLocation) then
    AErrors.Add('* AAPT tool not found.');

  if not TFile.Exists(FZipAlignLocation) then
    AErrors.Add('* ZipAlign tool not found.');

  if not TFile.Exists(FKeyToolLocation) then
    AErrors.Add('* KeyTool tool not found.');

  Result := (AErrors.Count = 0);
end;

end.
