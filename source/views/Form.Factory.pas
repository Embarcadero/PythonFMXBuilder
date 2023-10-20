unit Form.Factory;

interface

uses
  System.Classes,
  FMX.Forms,
  Form.Environment, Form.Project, Form.Installit;

type

  TFormSimpleFactory = class
  public
    class function CreateEnvironment(const AOwner: TComponent = nil): TEnvironmentForm;
    class function CreateProject(const AOwner: TComponent = nil): TProjectForm;
    class function CreateInstallIt(const AOwner: TComponent = nil): TInstallItForm;
  end;

implementation

{ TFormSimpleFactory }

class function TFormSimpleFactory.CreateEnvironment(
  const AOwner: TComponent): TEnvironmentForm;
begin
  Result := TEnvironmentForm.Create(AOwner);
end;

class function TFormSimpleFactory.CreateProject(
  const AOwner: TComponent): TProjectForm;
begin
  Result := TProjectForm.Create(AOwner);
end;

class function TFormSimpleFactory.CreateInstallIt(
  const AOwner: TComponent): TInstallItForm;
begin
  Result := TInstallItForm.Create(AOwner);
end;

end.
