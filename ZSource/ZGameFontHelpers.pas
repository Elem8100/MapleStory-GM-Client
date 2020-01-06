unit ZGameFontHelpers;

interface

uses
  AsphyreFonts, ZGameFonts;

  function  GetZVAlign(const VAlign: TVAlign): TZVAlign;
  function  GetZHAlign(const HAlign: THAlign): TZHAlign;

implementation

  function  GetZVAlign(const VAlign: TVAlign): TZVAlign;
  begin
    case VAlign of
      vMiddle: Result := zvMiddle;
      vBottom: Result := zvBottom;
    else
      Result := zvTop;
    end;
  end;

  function  GetZHAlign(const HAlign: THAlign): TZHAlign;
  begin
    case HAlign of
      hCenter: Result := zhCenter;
      hRight:  Result := zhRight;
    else
      Result := zhLeft;
    end;
  end;

end.
