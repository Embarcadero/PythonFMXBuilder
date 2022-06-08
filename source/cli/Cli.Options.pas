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

implementation

end.
