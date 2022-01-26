unit Container.Images;

interface

uses
  System.SysUtils, System.Classes, System.ImageList, FMX.ImgList;

type
  TImageContainer = class(TDataModule)
    images: TImageList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ImageContainer: TImageContainer;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
