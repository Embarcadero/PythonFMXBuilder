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
  PROJECT_ICON_INDEX = 4;
  MODULE_ICON_INDEX = 5;
  ANY_FILE_ICON_INDEX = 15;
  TARGET_PLATFORMS_ICON_INDEX = 23;
  TARGET_PLATFORMS_ANDROID_ICON_INDEX = 24;
  TARGET_PYTHON_ICON_INDEX = 25;
  TARGET_PYTHON_VER_ICON_INDEX = 26;
  SOURCE_ICON_INDEX = 27;
  BUNDLE_ICON_INDEX = 28;
  BUILD_CONFIGURATION_ICON_INDEX = 29;
  BUILD_CONFIGURATION_ITEM_ICON_INDEX = 30;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
