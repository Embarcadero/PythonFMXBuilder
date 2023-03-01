unit Builder.Model.Project.Icon;

interface

uses
  System.Classes, REST.Json.Types, Builder.Model;

type
  TProjectIconModel = class(TModel)
  private
    [JSONName('drawable_small')]
    FDrawableSmall: string;
    [JSONName('drawable_normal')]
    FDrawableNormal: string;
    [JSONName('drawable_large')]
    FDrawableLarge: string;
    [JSONName('drawable_xlarge')]
    FDrawableXlarge: string;
    [JSONName('drawable_ldpi')]
    FDrawableLdpi: string;
    [JSONName('drawable_mdpi')]
    FDrawableMdpi: string;
    [JSONName('drawable_hdpi')]
    FDrawableHdpi: string;
    [JSONName('drawable_xhdpi')]
    FDrawableXhdpi: string;
    [JSONName('drawable_xxhdpi')]
    FDrawableXxhdpi: string;
    [JSONName('drawable_xxxhdpi')]
    FDrawableXxxHdpi: string;
  public
    function Validate(const AErrors: TStrings): boolean; override;
  public
    property DrawableSmall: string read FDrawableSmall write FDrawableSmall;
    property DrawableNormal: string read FDrawableNormal write FDrawableNormal;
    property DrawableLarge: string read FDrawableLarge write FDrawableLarge;
    property DrawableXlarge: string read FDrawableXlarge write FDrawableXlarge;
    property DrawableLdpi: string read FDrawableLdpi write FDrawableLdpi;
    property DrawableMdpi: string read FDrawableMdpi write FDrawableMdpi;
    property DrawableHdpi: string read FDrawableHdpi write FDrawableHdpi;
    property DrawableXhdpi: string read FDrawableXhdpi write FDrawableXhdpi;
    property DrawableXxhdpi: string read FDrawableXxhdpi write FDrawableXxhdpi;
    property DrawableXxxHdpi: string read FDrawableXxxHdpi write FDrawableXxxHdpi;
  end;

implementation

uses
  System.SysUtils, System.Rtti;

{ TProjectIconModel }

function TProjectIconModel.Validate(const AErrors: TStrings): boolean;
begin
  Result := true;
  var LCtx := TRttiContext.Create();
  try
    var LType := LCtx.GetType(Self.ClassType);
    for var LField in LType.GetFields() do begin
      if LField.Name.StartsWith('Drawable') then begin
        var LValue := LField.GetValue(Self);
        if not LValue.AsString.IsEmpty() then begin
          Result := false;
          AErrors.Add(Format('* File %s not found.', [LValue.AsString]));
        end;
      end;
    end;
  finally
    LCtx.Free();
  end;
end;

end.
