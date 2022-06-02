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
  ANY_FILE_ICON_INDEX = 15;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
