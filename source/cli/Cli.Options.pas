unit Cli.Options;

interface

uses
  System.Generics.Collections, System.Rtti;

type
  THelpOptions = class
  public
    class var
      HelpCommand : string;
  end;

  TCreateOptions = class
  public
    class var
      ProjectNameCommand: string;
  end;

  TSelectOptions = class
  public
    class var
      ProjectNameCommand: string;
  end;

  TBuildOptions = class
  public
    class var
      ProjectNameCommand: string;
      VerboseCommand: boolean;
  end;

  TDeployOptions = class
  public
    class var
      ProjectNameCommand: string;
      VerboseCommand: boolean;
      DeviceCommand: string;
      UninstallCommand: boolean;
  end;

  TDeviceOptions = class
  public
    class var
      ListCommand: boolean;
  end;

  TEnvironmentOptions = class
  public
    class var
      FindCommand: boolean;
      OverrideCommand: boolean;
      SdkBasePathCommand: TValue;
      ApkSignerLocationCommand: TValue;
      AdbLocationCommand: TValue;
      AptLocationCommand: TValue;
      SdkApiLocationCommand: TValue;
      ZipAlignLocationCommand: TValue;
      JdkBasePathCommand: TValue;
      KeyToolLocationCommand: TValue;
      JarSignerLocationCommand: TValue;
  end;

  TProjectOptions = class
  public
    class var
      SelectCommand: string;
      PackageNameCommand: TValue;
      VersionCodeCommand: TValue;
      VersionNameCommand: TValue;
      PythonVersionCommand: TValue;
      ArchitectureCommand: TValue;
      //Icons
      DrawableSmallCommand: TValue;
      DrawableNormalCommand: TValue;
      DrawableLargeCommand: TValue;
      DrawableXLargeCommand: TValue;
      DrawableLDpiCommand: TValue;
      DrawableMDpiCommand: TValue;
      DrawableHDpiCommand: TValue;
      DrawableXHDpiCommand: TValue;
      DrawableXxHDpiCommand: TValue;
      DrawableXxxHDpiCommand: TValue;
      //Files
      AddFile: TList<string>;
      RemoveFile: TList<string>;
  public
    class constructor Create();
    class destructor Destroy();
  end;

  TEntityOptionsHelper = class
  public
    class function HasChanged([ref] AValue: TValue): boolean;
  end;

implementation

{ TProjectOptions }

class constructor TProjectOptions.Create;
begin
  AddFile := TList<string>.Create();
  RemoveFile := TList<string>.Create();
end;

class destructor TProjectOptions.Destroy;
begin
  RemoveFile.Free();
  AddFile.Free();
end;

{ TEntityOptions }

class function TEntityOptionsHelper.HasChanged([ref] AValue: TValue): boolean;
begin
  Result := not AValue.IsEmpty;
end;

end.
