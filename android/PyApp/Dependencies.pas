unit Dependencies;

interface

type
  /// <summary>
  ///   Dependency installation strategy
  /// </summary>
  IInstallDependency = interface
    ['{9E41532B-7EAE-4956-B228-3BD09EBF6BAD}']
    function IsInstalled(const AModuleName, AFilePath: string): boolean;
    function Install(const AModuleName, AFilePath: string): boolean;
  end;

implementation

end.
