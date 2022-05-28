unit Container.Images;

interface

uses
  System.SysUtils, System.Classes, System.ImageList, FMX.ImgList;

type
  TImageContainer = class(TDataModule)
    images: TImageList;
  end;

var
  ImageContainer: TImageContainer;

const
  PROJECT_ICON_INDEX = 11;
  MODULE_ICON_INDEX = 12;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
