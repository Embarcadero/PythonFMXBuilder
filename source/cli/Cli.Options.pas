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

  TGlobalOptions = class
  public
    class var
      DebuggerCommand: string;
  end;

  TCreateOptions = class
  public
    class var
      ProjectNameCommand: string;
      AddMainScriptCommand: boolean;
  end;

  TSelectOptions = class
  public
    class var
      ProjectNameCommand: string;
  end;

  TRemoveOptions = class
  public
    class var
      ProjectNameCommand: string;
      SkipConfirmationCommand: boolean;
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

  TRunOptions = class
  public
    class var
      ProjectNameCommand: string;
      VerboseCommand: boolean;
      DeviceCommand: string;
      DebugModeCommand: boolean;
  end;

  TStopOptions = class
  public
    class var
      ProjectNameCommand: string;
      DeviceCommand: string;
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
      Gui: boolean;
      ShowCommand: boolean;
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
      ShowCommand: boolean;
      Gui: boolean;
      ApplicationNameCommand: TValue;
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
      MainFileCommand: TValue;
      AddFileCommand: TArray<string>;
      RemoveFileCommand: TArray<string>;
      AddDependencyCommand: TArray<string>;
      RemoveDependencyCommand: TArray<string>;
  end;

  TEntityOptionsHelper = class
  public
    class function HasChanged([ref] AValue: TValue): boolean;
  end;

  function GetGUIEntityEditorPath(): string;

implementation

uses
  System.IOUtils;

const
  GUI_ENTITY_EDITOR_APP = 'pythonfmxbuilderentityeditor.exe';

function GetGUIEntityEditorPath(): string;
begin
  Result := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), GUI_ENTITY_EDITOR_APP);
end;

{ TEntityOptions }

class function TEntityOptionsHelper.HasChanged([ref] AValue: TValue): boolean;
begin
  Result := not AValue.IsEmpty;
end;

end.
