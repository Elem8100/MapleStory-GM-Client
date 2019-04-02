unit AsphyreFontsAlt;

//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Windows, Classes, SysUtils,  d3dx9, DX9Types, Direct3D9;

//---------------------------------------------------------------------------
type
 TFontStyleFx = (feBold, feItalic, feShadow, feEmboss);
 TFontStylesFx = set of TFontStyleFx;

//---------------------------------------------------------------------------
 TAsphyreFontsAlt = class;

//---------------------------------------------------------------------------
 TAsphyreFontAlt = class
 private
  Font      : ID3DXFont;
  FSize     : Cardinal;
  FFontName : string;
  FCharSet  : Cardinal;

  FAntialias  : Boolean;
  FStyle      : TFontStylesFx;

  FShadowColor    : Cardinal;
  FShadowIntensity: Real;
  FShadowDepth    : Integer;

  FEmbossColor    : Cardinal;
  FEmbossStrength : Real;
  FEmbossDepth    : Integer;
 public
  property Size     : Cardinal read FSize write FSize;
  property FontName : string read FFontName write FFontName;
  property CharSet  : Cardinal read FCharSet write FCharSet;

  property Antialias: Boolean read FAntialias write FAntialias;

  //-------------------------------------------------------------------------
  // update the font
  //-------------------------------------------------------------------------
  function Update(): Integer;

  //-------------------------------------------------------------------------
  // Draws the text on the screen at the specified coordinates (no clipping)
  //-------------------------------------------------------------------------
  procedure TextOut(const Text: string; x, y: Integer; Color: Cardinal); overload;

  //-------------------------------------------------------------------------
  // Estimates the text Width without painting it.
  //-------------------------------------------------------------------------
  function TextWidth(const Text: string): Integer;

  //-------------------------------------------------------------------------
  // Estimates the text Height without painting it.
  //-------------------------------------------------------------------------
  function TextHeight(const Text: string): Integer;

  //-------------------------------------------------------------------------
  // Estimates the text Width & Height without painting it.
  //-------------------------------------------------------------------------
  function TextSize(const Text: string): TPoint;

  constructor Create(AOwner: TAsphyreFontsAlt);
  destructor Destroy(); override;
 published
  // Customizable font style.
  property Style: TFontStylesFx read FStyle write FStyle;

  //-------------------------------------------------------------------------
  // How dark the shadow is (0.0 - opaque, 1.0 - dark). Default is 0.5
  //-------------------------------------------------------------------------
  property ShadowIntensity: Real read FShadowIntensity write FShadowIntensity;

  //-------------------------------------------------------------------------
  // Shadow Color. Default is $000000 (black)
  //-------------------------------------------------------------------------
  property ShadowColor    : Cardinal read FShadowColor write FShadowColor;

  //-------------------------------------------------------------------------
  // How distant is the shadow. Default is 2 pixels
  //-------------------------------------------------------------------------
  property ShadowDepth    : Integer read FShadowDepth write FShadowDepth;

  //-------------------------------------------------------------------------
  // How strong the emboss is (0.0 - weak, 1.0 - strong). Default is 0.5
  //-------------------------------------------------------------------------
  property EmbossStrength : Real read FEmbossStrength write FEmbossStrength;

  //-------------------------------------------------------------------------
  // Emboss Color. Default is $000000 (black)
  //-------------------------------------------------------------------------
  property EmbossColor    : Cardinal read FEmbossColor write FEmbossColor;

  //-------------------------------------------------------------------------
  // How distant is the emboss. Default is 1 pixel (max recommended is 2)
  //-------------------------------------------------------------------------
  property EmbossDepth    : Integer read FEmbossDepth write FEmbossDepth;
 end;

//---------------------------------------------------------------------------
 TAsphyreFontsAlt = class(TComponent)
 private
  Fonts: array of TAsphyreFontAlt;

  function GetItem(Index: Integer): TAsphyreFontAlt;
  function GetCount(): Integer;
 public
  property Count: Integer read GetCount;
  property Items[Index: Integer]: TAsphyreFontAlt read GetItem; default;

  constructor Create(AOwner: TComponent); override;
  destructor Destroy(); override;

  //-------------------------------------------------------------------------
  // Add new font to the list *without* initializing or loading it.
  //-------------------------------------------------------------------------
  function Add(): TAsphyreFontAlt;

  //-------------------------------------------------------------------------
  // Finds an existing font in the list and returns its index.
  //-------------------------------------------------------------------------
  function Find(Font: TAsphyreFontAlt): Integer;

  //-------------------------------------------------------------------------
  // Removes the font from the list at the specified index.
  //-------------------------------------------------------------------------
  procedure Remove(Index: Integer);

  //-------------------------------------------------------------------------
  // Removes any loaded fonts from the list.
  //-------------------------------------------------------------------------
  procedure RemoveAll();

  //-------------------------------------------------------------------------
  // Update any loaded fonts.
  //-------------------------------------------------------------------------
  procedure UpdateAll();
 end;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
constructor TAsphyreFontAlt.Create(AOwner: TAsphyreFontsAlt);
begin
 inherited Create();

 FAntialias:= True;
 FSize     := 22;
 FFontName := 'Arial';
 FStyle    := [];
 FCharSet  := DEFAULT_CHARSET;

 FShadowIntensity:= 0.5;
 FShadowColor    := $000000;
 FShadowDepth    := 2;

 FEmbossStrength := 0.5;
 FEMbossColor    := $000000;
 FEmbossDepth    := 1;

 //Font      :=  ID3DXFont;
end;

//---------------------------------------------------------------------------
destructor TAsphyreFontAlt.Destroy();
begin
 Font:= nil;

 inherited;
end;

//---------------------------------------------------------------------------
procedure TAsphyreFontAlt.TextOut(const Text: string; x, y: Integer;
 Color: Cardinal);
var
 Rect  : TRect;
 AuxCol: Cardinal;
begin
 // draw shadow
 if (feShadow in FStyle) then
  begin
   Rect:= Bounds(X + FShadowDepth, Y + FShadowDepth, 0, 0);
   AuxCol:= (Round(FShadowIntensity * 255.0) shl 24) or ((FShadowColor) and $FFFFFF);
   Font.DrawTextW(nil, PWideChar(Text), -1, @Rect, DT_LEFT or DT_NOCLIP, AuxCol);
  end;

 // set draw position
 if (feEmboss in FStyle) then
  Rect:= Bounds(X - (FEmbossDepth * 2), Y - (FEmbossDepth * 2), 0, 0)
   else Rect:= Bounds(X, Y, 0, 0);

 // draw text fast (no clipping)
 Font.DrawTextW(nil, PWideChar(Text), -1, @Rect, DT_LEFT or DT_NOCLIP, Color);


 // draw emboss
 if (feEmboss in FStyle) then
  begin
   AuxCol:= Round(FEmbossStrength * 255.0);
   AuxCol:= (AuxCol shl 24) or ((FEmbossColor) and $FFFFFF);

   Rect:= Bounds(X - FEmbossDepth, Y - FEmbossDepth, 0, 0);
   Font.DrawTextW(nil, PWideChar(Text), -1, @Rect, DT_LEFT or DT_NOCLIP, AuxCol);
  end;
end;

//---------------------------------------------------------------------------
function TAsphyreFontAlt.TextSize(const Text: string): TPoint;
var
 Rect: TRect;
begin
 // calculate size
 Font.DrawTextW(nil, PWideChar(Text), -1, @Rect, DT_CALCRECT, $0);

 // set result
 Result.X:= Rect.Right - Rect.Left;
 Result.Y:= Rect.Bottom - Rect.Top;
end;

//---------------------------------------------------------------------------
function TAsphyreFontAlt.TextWidth(const Text: string): Integer;
begin
 Result:= TextSize(Text).X;
end;

//---------------------------------------------------------------------------
function TAsphyreFontAlt.TextHeight(const Text: string): Integer;
begin
 Result:= TextSize(Text).Y;
end;

//---------------------------------------------------------------------------
function TAsphyreFontAlt.Update(): Integer;
var
 FontWeight: Cardinal;
 Quality   : Cardinal;
begin
 // release previous font
 if (Font <> nil) then Font:= nil;

 // get font weight
 if (feBold in FStyle) then
  FontWeight:= FW_HEAVY
   else FontWeight:= FW_NORMAL;

 // get quality
 if (FAntialias) then
  Quality:= ANTIALIASED_QUALITY
   else Quality:= NONANTIALIASED_QUALITY;

 // create font
 Result:= D3DXCreateFont(Device9, FSize, 0, FontWeight, 1, (feItalic in FStyle),
  FCharSet, OUT_DEFAULT_PRECIS, Quality, DEFAULT_PITCH or FF_DONTCARE,
  PChar(FFontName), Font);
end;

//---------------------------------------------------------------------------
constructor TAsphyreFontsAlt.Create(AOwner: TComponent);
begin
 inherited;

 SetLength(Fonts, 0);
end;

//---------------------------------------------------------------------------
destructor TAsphyreFontsAlt.Destroy();
begin
 RemoveAll();

 inherited;
end;

//---------------------------------------------------------------------------
function TAsphyreFontsAlt.GetCount(): Integer;
begin
 Result:= Length(Fonts);
end;

//---------------------------------------------------------------------------
function TAsphyreFontsAlt.GetItem(Index: Integer): TAsphyreFontAlt;
begin
 Result:= nil;
 if (Index >= 0)and(Index < Length(Fonts)) then Result:= Fonts[Index];
end;

//---------------------------------------------------------------------------
procedure TAsphyreFontsAlt.RemoveAll();
var
 i: Integer;
begin
 for i:= 0 to Length(Fonts) - 1 do
  if (Assigned(Fonts[i])) then
   begin
    Fonts[i].Free();
    Fonts[i]:= nil;
   end;

 SetLength(Fonts, 0);
end;

//---------------------------------------------------------------------------
function TAsphyreFontsAlt.Add(): TAsphyreFontAlt;
var
 Index: Integer;
begin
 Index:= Length(Fonts);
 SetLength(Fonts, Index + 1);

 Fonts[Index]:= TAsphyreFontAlt.Create(Self);

 Result:= Fonts[Index];
end;

//---------------------------------------------------------------------------
procedure TAsphyreFontsAlt.Remove(Index: Integer);
var
 i: Integer;
begin
 if (Index < 0)or(Index >= Length(Fonts)) then Exit;

 // 1. release the font
 if (Assigned(Fonts[Index])) then
  begin
   Fonts[Index].Free();
   Fonts[Index]:= nil;
  end;

 // 2. remove font from the list
 for i:= Index to Length(Fonts) - 2 do
  Fonts[i]:= Fonts[i + 1];

 // 3. update array size
 SetLength(Fonts, Length(Fonts) - 1);
end;

//---------------------------------------------------------------------------
function TAsphyreFontsAlt.Find(Font: TAsphyreFontAlt): Integer;
var
 i: Integer;
begin
 for i:= 0 to Length(Fonts) - 1 do
  if (Fonts[i] = Font) then
   begin
    Result:= i;
    Exit;
   end;

 Result:= -1;
end;

//---------------------------------------------------------------------------
procedure TAsphyreFontsAlt.UpdateAll();
var
 i: Integer;
begin
 for i:= 0 to Length(Fonts) - 1 do
  Fonts[i].Update();
end;

//---------------------------------------------------------------------------
end.


