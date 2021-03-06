unit Builder.Model.Environment;

interface

uses
  System.SysUtils, System.Classes, REST.Json.Types, Builder.Model;

type
  [Model('environment')]
  TEnvironmentModel = class(TModel)
  private
    [JSONName('sdk_base_path')]
    FSdkBasePath: string;
    [JSONName('apk_signer_location')]
    FApkSignerLocation: string;
    [JSONName('adb_location')]
    FAdbLocation: string;
    [JSONName('apt_location')]
    FAAptLocation: string;
    [JSONName('sdk_api_location')]
    FSdkApiLocation: string;
    [JSONName('zip_align_location')]
    FZipAlignLocation: string;
    [JSONName('jdk_base_path')]
    FJdkBasePath: string;
    [JSONName('key_tool_location')]
    FKeyToolLocation: string;
    [JSONName('jar_signer_location')]
    FJarSignerLocation: string;
  public
    function Validate(const AErrors: TStrings): boolean; override;
  public
    //SDK
    property SdkBasePath: string read FSdkBasePath write FSdkBasePath;
    property ApkSignerLocation: string read FApkSignerLocation write FApkSignerLocation;
    property AdbLocation: string read FAdbLocation write FAdbLocation;
    property AAptLocation: string read FAAptLocation write FAAptLocation;
    property SdkApiLocation: string read FSdkApiLocation write FSdkApiLocation;
    property ZipAlignLocation: string read FZipAlignLocation write FZipAlignLocation;
    //JDK
    property JdkBasePath: string read FJdkBasePath write FJdkBasePath;
    property KeyToolLocation: string read FKeyToolLocation write FKeyToolLocation;
    property JarSignerLocation: string read FJarSignerLocation write FJarSignerLocation;
  end;

  TPathLocator = class
  public
    class function LoadToolPath(const ABasePath, ATool: string): string; static;
    class function FindSdkApiLocation(const ABasePath: string): string; static;
  end;

implementation

uses
  System.IOUtils;

{ TEnvironmentModel }

function TEnvironmentModel.Validate(const AErrors: TStrings): boolean;
begin
  AErrors.Clear();

  {|||||| CHECK FOR PATHS |||||||}
  if FSdkBasePath.Trim().IsEmpty() then
    AErrors.Add('* The SDK base location can not be empty.');

  if FApkSignerLocation.Trim().IsEmpty() then
    AErrors.Add('* The APK signer location can not be empty.');

  if FAdbLocation.Trim().IsEmpty() then
    AErrors.Add('* The ADB location can not be empty.');

  if FAAptLocation.Trim().IsEmpty() then
    AErrors.Add('* The AAPT location can not be empty.');

  if FSdkApiLocation.Trim().IsEmpty() then
    AErrors.Add('* The SDK API location can not be empty.');

  if FZipAlignLocation.Trim().IsEmpty() then
    AErrors.Add('* The ZipAlign location can not be empty.');

  if FJdkBasePath.Trim().IsEmpty() then
    AErrors.Add('* The JDK base location can not be empty.');

  if FKeyToolLocation.Trim().IsEmpty() then
    AErrors.Add('* The KeyTool location can not be empty.');

  if FJarSignerLocation.Trim().IsEmpty() then
    AErrors.Add('* The JAR signer location can not be empty.');


  {|||||| CHECK FOR VALID FILES |||||||}

  if not TFile.Exists(FApkSignerLocation) then
    AErrors.Add('* APKSigner tool not found.');

  if not TFile.Exists(FAdbLocation) then
    AErrors.Add('* ADB tool not found.');

  if not TFile.Exists(FAAptLocation) then
    AErrors.Add('* AAPT tool not found.');

  if not TFile.Exists(FZipAlignLocation) then
    AErrors.Add('* ZipAlign tool not found.');

  if not TFile.Exists(FKeyToolLocation) then
    AErrors.Add('* KeyTool tool not found.');

  if not TFile.Exists(FJarSignerLocation) then
    AErrors.Add('* JARSigner tool not found.');

  Result := (AErrors.Count = 0);
end;

{ TPathLocator }

class function TPathLocator.FindSdkApiLocation(const ABasePath: string): string;
begin
  var LPlatforms := TPath.Combine(ABasePath, 'platforms');
  var LFolders := TDirectory.GetDirectories(
    LPlatforms, 'android-*', TSearchOption.soTopDirectoryOnly, nil);

  Result := String.Empty;
  if Length(LFolders) > 0 then begin
    var LGreater := 0;
    for var LFolder in LFolders do begin
      var LAndroid :=
        TPath.GetFileName(ExcludeTrailingPathDelimiter(LFolder))
          .Replace('android-', String.Empty, []);

      var LApi := 0;
      if TryStrToInt(LAndroid, LApi) then
        if (LApi > LGreater) then begin
          LGreater := LApi;
          Result := LFolder;
        end;
    end;
  end;
end;

class function TPathLocator.LoadToolPath(const ABasePath,
  ATool: string): string;
begin
  var LFiles := TDirectory.GetFiles(ABasePath, ATool, TSearchOption.soAllDirectories, nil);
  if Length(LFiles) > 0 then
    Result := LFiles[0]
  else
    Result := String.Empty;
end;

end.
