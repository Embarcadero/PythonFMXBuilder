unit Form.Factory;

interface

uses
  FMX.Forms,
  Form.Environment, From.Project, System.Classes;

type
  TFormSimpleFactory = class
  public
    class function CreateEnvironment(const AOwner: TComponent = nil): TEnvironmentForm;
    class function CreateProject(const AOwner: TComponent = nil): TProjectForm;
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

end.
