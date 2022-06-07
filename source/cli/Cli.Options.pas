unit Cli.Options;

interface

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
      IncludeDefaultScriptCommand: boolean;
  public
    class constructor Create();
  end;

  TSelectOptions = class
  public
    class var
      ProjectNameCommand: string;
  end;

  TBuildOptions = class
  end;

  TDeployOptions = class
  public
    class var
      DeviceCommand: string;
      UninstallCommand: boolean;
  end;

implementation

{ TCreateOptions }

class constructor TCreateOptions.Create;
begin
  TCreateOptions.IncludeDefaultScriptCommand := true;
end;

end.
