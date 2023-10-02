unit Container.Styles;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls,
  //MaterialOxforBlueSB make uses of TInvertEffect - The effects unit must be used here!
  FMX.Effects, FMX.Filter.Effects;

type
  TStyleContainer = class(TDataModule)
    MaterialOxfordBlueSB: TStyleBook;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  StyleContainer: TStyleContainer;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
