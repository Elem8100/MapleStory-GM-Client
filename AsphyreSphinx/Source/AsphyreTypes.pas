unit AsphyreTypes;
//---------------------------------------------------------------------------
// AsphyreTypes.pas                                     Modified: 10-Oct-2007
// Asphyre Types and Definitions                                  Version 1.1
//---------------------------------------------------------------------------
// Important Notice:
//
// If you modify/use this code or one of its parts either in original or
// modified form, you must comply with Mozilla Public License v1.1,
// specifically section 3, "Distribution Obligations". Failure to do so will
// result in the license breach, which will be resolved in the court.
// Remember that violating author's rights is considered a serious crime in
// many countries. Thank you!
//
// !! Please *read* Mozilla Public License 1.1 document located at:
//  http://www.mozilla.org/MPL/
//---------------------------------------------------------------------------
// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
// License for the specific language governing rights and limitations
// under the License.
//
// The Original Code is AsphyreTypes.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by M. Sc. Yuriy Kotsarenko are Copyright (C) 2007 - 2008,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------

{$ifdef fpc}{$packenum 1}{$endif}

//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 System.Types, Vectors2px, Vectors2, AsphyreColors, Math;

//---------------------------------------------------------------------------
type
 TRGB32 = record
    B, G, R, A: Byte;
 end;

 TRGB32Array = array[0..MaxInt div SizeOf(TRGB32) - 1] of TRGB32;
 PRGB32=^TRGB32;
 PRGB32Array = ^TRGB32Array;


 PAsphyrePixelFormat = ^TAsphyrePixelFormat;
 TAsphyrePixelFormat = (apf_Unknown, apf_R8G8B8, apf_A8R8G8B8, apf_X8R8G8B8,
  apf_R5G6B5, apf_X1R5G5B5, apf_A1R5G5B5, apf_A4R4G4B4, apf_R3G3B2, apf_A8,
  apf_A8R3G3B2, apf_X4R4G4B4, apf_A2B10G10R10, apf_G16R16, apf_A2R10G10B10,
  apf_A16B16G16R16, apf_L8, apf_A8L8, apf_A4L4, apf_V8U8, apf_L6V5U5,
  apf_X8L8V8U8, apf_Q8W8V8U8, apf_V16U16, apf_A2W10V10U10, apf_UYVY,
  apf_R8G8_B8G8, apf_YUY2, apf_G8R8_G8B8, apf_DXT1, apf_DXT2, apf_DXT3,
  apf_DXT4, apf_DXT5, apf_L16, apf_Q16W16V16U16, apf_R16F, apf_G16R16F,
  apf_A16B16G16R16F, apf_R32F, apf_G32R32F, apf_A32B32G32R32F,
  apf_CxV8U8, apf_A8B8G8R8, apf_X8B8G8R8, apf_A8X8V8U8, apf_L8X8V8U8,
  apf_A6L2, apf_A2R2G2B2, apf_A5R9G9B9);

//---------------------------------------------------------------------------
 PColor2 = ^TColor2;
 TColor2 = array[0..1] of Cardinal;

//---------------------------------------------------------------------------
 PColor4 = ^TColor4;
 TColor4 = array[0..3] of Cardinal;

//---------------------------------------------------------------------------
 PPoint4 = ^TPoint4;
 TPoint4 = array[0..3] of TPoint2;

//---------------------------------------------------------------------------
 PPoint4px = ^TPoint4px;
 TPoint4px = array[0..3] of TPoint2px;
 TPolygon = array of TPoint2;
//---------------------------------------------------------------------------
{$ifndef fpc}
 PtrInt  = Integer;
 PtrUInt = Cardinal;
{$endif}

//---------------------------------------------------------------------------
const
 clWhite1  : Cardinal = $FFFFFFFF;
 clBlack1  : Cardinal = $FF000000;
 clMaroon1 : Cardinal = $FF800000;
 clGreen1  : Cardinal = $FF008000;
 clOlive1  : Cardinal = $FF808000;
 clNavy1   : Cardinal = $FF000080;
 clPurple1 : Cardinal = $FF800080;
 clTeal1   : Cardinal = $FF008080;
 clGray1   : Cardinal = $FF808080;
 clSilver1 : Cardinal = $FFC0C0C0;
 clRed1    : Cardinal = $FFFF0000;
 clLime1   : Cardinal = $FF00FF00;
 clYellow1 : Cardinal = $FFFFFF00;
 clBlue1   : Cardinal = $FF0000FF;
 clFuchsia1: Cardinal = $FFFF00FF;
 clAqua1   : Cardinal = $FF00FFFF;
 clLtGray1 : Cardinal = $FFC0C0C0;
 clDkGray1 : Cardinal = $FF808080;
 clOpaque1 : Cardinal = $00FFFFFF;
 clUnknown : Cardinal = $00000000;

//---------------------------------------------------------------------------
 clWhite2  : TColor2 = ($FFFFFFFF, $FFFFFFFF);
 clBlack2  : TColor2 = ($FF000000, $FF000000);
 clMaroon2 : TColor2 = ($FF800000, $FF800000);
 clGreen2  : TColor2 = ($FF008000, $FF008000);
 clOlive2  : TColor2 = ($FF808000, $FF808000);
 clNavy2   : TColor2 = ($FF000080, $FF000080);
 clPurple2 : TColor2 = ($FF800080, $FF800080);
 clTeal2   : TColor2 = ($FF008080, $FF008080);
 clGray2   : TColor2 = ($FF808080, $FF808080);
 clSilver2 : TColor2 = ($FFC0C0C0, $FFC0C0C0);
 clRed2    : TColor2 = ($FFFF0000, $FFFF0000);
 clLime2   : TColor2 = ($FF00FF00, $FF00FF00);
 clYellow2 : TColor2 = ($FFFFFF00, $FFFFFF00);
 clBlue2   : TColor2 = ($FF0000FF, $FF0000FF);
 clFuchsia2: TColor2 = ($FFFF00FF, $FFFF00FF);
 clAqua2   : TColor2 = ($FF00FFFF, $FF00FFFF);
 clLtGray2 : TColor2 = ($FFC0C0C0, $FFC0C0C0);
 clDkGray2 : TColor2 = ($FF808080, $FF808080);
 clOpaque2 : TColor2 = ($00FFFFFF, $00FFFFFF);
 clUnknown2: TColor2 = ($00000000, $00000000);

//---------------------------------------------------------------------------
 clWhite4  : TColor4 = ($FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF);
 clBlack4  : TColor4 = ($FF000000, $FF000000, $FF000000, $FF000000);
 clMaroon4 : TColor4 = ($FF800000, $FF800000, $FF800000, $FF800000);
 clGreen4  : TColor4 = ($FF008000, $FF008000, $FF008000, $FF008000);
 clOlive4  : TColor4 = ($FF808000, $FF808000, $FF808000, $FF808000);
 clNavy4   : TColor4 = ($FF000080, $FF000080, $FF000080, $FF000080);
 clPurple4 : TColor4 = ($FF800080, $FF800080, $FF800080, $FF800080);
 clTeal4   : TColor4 = ($FF008080, $FF008080, $FF008080, $FF008080);
 clGray4   : TColor4 = ($FF808080, $FF808080, $FF808080, $FF808080);
 clSilver4 : TColor4 = ($FFC0C0C0, $FFC0C0C0, $FFC0C0C0, $FFC0C0C0);
 clRed4    : TColor4 = ($FFFF0000, $FFFF0000, $FFFF0000, $FFFF0000);
 clLime4   : TColor4 = ($FF00FF00, $FF00FF00, $FF00FF00, $FF00FF00);
 clYellow4 : TColor4 = ($FFFFFF00, $FFFFFF00, $FFFFFF00, $FFFFFF00);
 clBlue4   : TColor4 = ($FF0000FF, $FF0000FF, $FF0000FF, $FF0000FF);
 clFuchsia4: TColor4 = ($FFFF00FF, $FFFF00FF, $FFFF00FF, $FFFF00FF);
 clAqua4   : TColor4 = ($FF00FFFF, $FF00FFFF, $FF00FFFF, $FF00FFFF);
 clLtGray4 : TColor4 = ($FFC0C0C0, $FFC0C0C0, $FFC0C0C0, $FFC0C0C0);
 clDkGray4 : TColor4 = ($FF808080, $FF808080, $FF808080, $FF808080);
 clOpaque4 : TColor4 = ($00FFFFFF, $00FFFFFF, $00FFFFFF, $00FFFFFF);
 clUnknown4: TColor4 = ($00000000, $00000000, $00000000, $00000000);

//---------------------------------------------------------------------------
 TexFull4: TPoint4 = ((x: 0.0; y: 0.0), (x: 1.0; y: 0.0), (x: 1.0; y: 1.0),
  (x: 0.0; y: 1.0));
 TexZero4px: TPoint4px = ((x: 0; y: 0), (x: 0; y: 0), (x: 0; y: 0),
  (x: 0; y: 0));

//---------------------------------------------------------------------------
 AsphyrePixelFormatBits: array[TAsphyrePixelFormat] of Integer = (0, 24, 32,
  32, 16, 16, 16, 16, 8, 8, 16, 16, 32, 32, 32, 64, 8, 16, 8, 16, 16, 32, 32,
  32, 32, 16, 16, 16, 16, 4, 8, 8, 8, 8, 16, 64, 16, 32, 64, 32, 64, 128, 16,
  32, 32, 32, 32, 8, 8, 32);

function GetA(const Color: Longword): Byte; inline;
function GetR(const Color: Longword): Byte; inline;
function GetG(const Color: Longword): Byte; inline;
function GetB(const Color: Longword): Byte; inline;
function ARGB(const A, R, G, B: Byte): Longword; inline;

//---------------------------------------------------------------------------
function cRGB1(r, g, b: Cardinal; a: Cardinal = 255): Cardinal; inline;
function cGray1(Gray: Cardinal): Cardinal;
function cAlpha1(Alpha: Cardinal): Cardinal;
function cColorAlpha1(Color, Alpha: Cardinal): Cardinal;
function cColorGrayAlpha1(Color, Gray, Alpha: Cardinal): Cardinal;

//---------------------------------------------------------------------------
function cColorAlpha1f(Color: Cardinal; Alpha: Single): Cardinal;
function cAlpha1f(Alpha: Single): Cardinal;
function cAlpha4f(Alpha: Single): TColor4;
function cColor4f2(TopColor, BottomColor: Cardinal; Alpha: Single): TColor4;

//---------------------------------------------------------------------------
function cColor2(Color0, Color1: Cardinal): TColor2; overload;
function cColor2(Color: Cardinal): TColor2; overload;
function cRGB2(r, g, b: Cardinal; a: Cardinal = 255): TColor2; overload;
function cRGB2(r1, g1, b1, a1, r2, g2, b2, a2: Cardinal): TColor2; overload;
function cGray2(Gray: Cardinal): TColor2; overload;
function cGray2(Gray1, Gray2: Cardinal): TColor2; overload;
function cAlpha2(Alpha: Cardinal): TColor2; overload;
function cAlpha2(Alpha1, Alpha2: Cardinal): TColor2; overload;
function cColorAlpha2(Color, Alpha: Cardinal): TColor2; overload;
function cColorAlpha2(Color1, Color2, Alpha1, Alpha2: Cardinal): TColor2; overload;
function cColorAlpha2of(const Colors: TColor2; Alpha: Single): TColor2;

//---------------------------------------------------------------------------
function cColor4(Color: Cardinal): TColor4; overload;
function cColor4(Color1, Color2, Color3, Color4: Cardinal): TColor4; overload;
function cRGB4(r, g, b: Cardinal; a: Cardinal = 255): TColor4; overload; inline;
function cRGB4(r1, g1, b1, a1, r2, g2, b2, a2: Cardinal): TColor4; overload; inline;
function cGray4(Gray: Cardinal): TColor4; overload;
function cGray4(Gray1, Gray2, Gray3, Gray4: Cardinal): TColor4; overload;
function cAlpha4(Alpha: Cardinal): TColor4; overload;
function cAlpha4(Alpha1, Alpha2, Alpha3, Alpha4: Cardinal): TColor4; overload;
function cGrayAlpha4(Gray, Alpha: Cardinal): TColor4; overload;
function cGrayAlpha4(Gray1, Gray2, Gray3, Gray4, Alpha1, Alpha2, Alpha3,
 Alpha4: Cardinal): TColor4; overload;
function cColorAlpha4(Color, Alpha: Cardinal): TColor4; overload;
function cColorAlpha4(Color1, Color2, Color3, Color4, Alpha1, Alpha2, Alpha3,
 Alpha4: Cardinal): TColor4; overload;
function cColorGrayAlpha4(Color, Gray, Alpha: Cardinal): TColor4; overload;
function cColorGrayAlpha4(Color1, Color2, Color3, Color4,
 Gray1, Gray2, Gray3, Gray4, Alpha1, Alpha2, Alpha3,
 Alpha4: Cardinal): TColor4; overload;

//---------------------------------------------------------------------------
function ColorToFixed4(const Colors: TColor4): TAsphyreColor4;
function FixedToColor4(const Colors: TAsphyreColor4): TColor4;

//---------------------------------------------------------------------------
// Point4 helper routines
//---------------------------------------------------------------------------
// point values -> TPoint4
function Point4(x1, y1, x2, y2, x3, y3, x4, y4: Single): TPoint4; overload;
function Point4(const p1, p2, p3, p4: TPoint2): TPoint4; overload;
// rectangle coordinates -> TPoint4
function pRect4(const Rect: TRect): TPoint4;
// rectangle coordinates -> TPoint4
function pBounds4(_Left, _Top, _Width, _Height: Single): TPoint4;
// rectangle coordinates, scaled -> TPoint4
function pBounds4s(_Left, _Top, _Width, _Height, Theta: Single): TPoint4;
// rectangle coordinates, scaled / centered -> TPoint4
function pBounds4sc(_Left, _Top, _Width, _Height, Theta: Single): TPoint4;
// mirrors the coordinates
function pMirror4(const Point4: TPoint4): TPoint4;
// flips the coordinates
function pFlip4(const Point4: TPoint4): TPoint4;
// shift the given points by the specified amount
function pShift4(const Points: TPoint4; const ShiftBy: TPoint2): TPoint4;
// rotated rectangle (Origin + Size) around (Middle) with Angle and Scale
function pRotate4(const Origin, Size, Middle: TPoint2; Angle: Single;
 Theta: Single = 1.0): TPoint4;
function pRotate4se(const Origin, Size, Middle: TPoint2; Angle: Single;
 Theta: Single = 1.0): TPoint4;
function pRotate4c(const Origin, Size: TPoint2; Angle: Single;
 Theta: Single = 1.0): TPoint4;

//---------------------------------------------------------------------------
function pxBounds4(Left, Top, Width, Height: Integer): TPoint4px;
function pxBounds4f(Left, Top, Width, Height: Single): TPoint4;
function pxRect4(Left, Top, Right, Bottom: Integer): TPoint4px; overload;
function pxRect4(const Rect: TRect): TPoint4px; overload;
function pxRect4(const Pos, Size: TPoint2px): TPoint4px; overload;
function pxRotate90r(const Points: TPoint4px): TPoint4px;

function AngleDiff(SrcAngle, DestAngle: Single): Single;
function Cos256(i: Integer):Double;
function Sin256(i: Integer):Double;
function OverlapRect(const Rect1, Rect2: TRect): Boolean;

function PtInPolygon(Pt : TPoint2 ; Pg : TPolygon):Boolean;
//---------------------------------------------------------------------------
// returns True if the point4  Quadrangle overlap
//---------------------------------------------------------------------------
function OverlapQuadrangle(Q1, Q2: TPoint4): Boolean;

function OverlapPolygon(P1, P2: TPolygon): Boolean;
//---------------------------------------------------------------------------

function RectInRect(const Rect1, Rect2: TRect): Boolean;
function PointInRect(const Point: TPoint; const Rect: TRect): Boolean;

function Angle256(X, Y: Integer): Real;
implementation

function GetA(const Color: Longword): Byte; inline;
begin
  Result := Color shr 24;
end;

function GetR(const Color: Longword): Byte; inline;
begin
  Result := (Color shr 16) and $FF;
end;

function GetG(const Color: Longword): Byte; inline;
begin
  Result := (Color shr 8) and $FF;
end;

function GetB(const Color: Longword): Byte; inline;
begin
  Result := Color and $FF;
end;

function ARGB(const A, R, G, B: Byte): Longword; inline;
begin
  Result := (A shl 24) or (R shl 16) or (G shl 8) or B;
end;

//---------------------------------------------------------------------------
function cColorAlpha1f(Color: Cardinal; Alpha: Single): Cardinal;
begin
 Result:= cColorAlpha1(Color, Round(Alpha * 255.0));
end;

//---------------------------------------------------------------------------
function cAlpha1f(Alpha: Single): Cardinal;
begin
 Result:= cAlpha1(Round(Alpha * 255.0));
end;

//---------------------------------------------------------------------------
function cAlpha4f(Alpha: Single): TColor4;
begin
 Result:= cColor4(cAlpha1f(Alpha));
end;

//---------------------------------------------------------------------------
function cColor4f2(TopColor, BottomColor: Cardinal; Alpha: Single): TColor4;
var
 Color1, Color2: Cardinal;
 Alpha1: Integer;
begin
 Alpha1:= Round(Alpha * 255.0);
 Color1:= cColorAlpha1(TopColor, Alpha1);
 Color2:= cColorAlpha1(BottomColor, Alpha1);

 Result:= cColor4(Color1, Color1, Color2, Color2);
end;

//---------------------------------------------------------------------------
function cRGB1(r, g, b: Cardinal; a: Cardinal = 255): Cardinal;  inline;
begin
 Result:= b or (g shl 8) or (r shl 16) or (a shl 24);
end;

//---------------------------------------------------------------------------
function cGray1(Gray: Cardinal): Cardinal;
begin
 Result:= ((Gray and $FF) or ((Gray and $FF) shl 8) or ((Gray and $FF) shl 16))
  or $FF000000;
end;

//---------------------------------------------------------------------------
function cAlpha1(Alpha: Cardinal): Cardinal;
begin
 Result:= $FFFFFF or ((Alpha and $FF) shl 24);
end;

//---------------------------------------------------------------------------
function cColorAlpha1(Color, Alpha: Cardinal): Cardinal;
begin
 Result:= TAsphyreColor(Color) * TAsphyreColor(cAlpha1(Alpha));
end;

//---------------------------------------------------------------------------
function cColorGrayAlpha1(Color, Gray, Alpha: Cardinal): Cardinal;
begin
 Result:= TAsphyreColor(Color) * cColor(Gray, Gray, Gray, Alpha);
end;

//---------------------------------------------------------------------------
function cColor2(Color0, Color1: Cardinal): TColor2;
begin
 Result[0]:= Color0;
 Result[1]:= Color1;
 end;

//---------------------------------------------------------------------------
function cColor2(Color: Cardinal): TColor2;
begin
 Result[0]:= Color;
 Result[1]:= Color;
end;

//---------------------------------------------------------------------------
function cRGB2(r1, g1, b1, a1, r2, g2, b2, a2: Cardinal): TColor2; overload;
begin
 Result[0]:= cRGB1(r1, g1, b1, a1);
 Result[1]:= cRGB1(r2, g2, b2, a2);
end;

//---------------------------------------------------------------------------
function cRGB2(r, g, b: Cardinal; a: Cardinal = 255): TColor2; overload;
begin
 Result[0]:= cRGB1(r, g, b, a);
 Result[1]:= Result[0];
end;

//---------------------------------------------------------------------------
function cGray2(Gray: Cardinal): TColor2;
begin
 Result:= cColor2(((Gray and $FF) or ((Gray and $FF) shl 8) or
  ((Gray and $FF) shl 16)) or $FF000000);
end;

//---------------------------------------------------------------------------
function cGray2(Gray1, Gray2: Cardinal): TColor2;
begin
 Result[0]:= ((Gray1 and $FF) or ((Gray1 and $FF) shl 8) or
  ((Gray1 and $FF) shl 16)) or $FF000000;
 Result[1]:= ((Gray2 and $FF) or ((Gray2 and $FF) shl 8) or
  ((Gray2 and $FF) shl 16)) or $FF000000;
end;

//---------------------------------------------------------------------------
function cAlpha2(Alpha: Cardinal): TColor2;
begin
 Result:= cColor2($FFFFFF or ((Alpha and $FF) shl 24));
end;

//---------------------------------------------------------------------------
function cAlpha2(Alpha1, Alpha2: Cardinal): TColor2;
begin
 Result[0]:= $FFFFFF or ((Alpha1 and $FF) shl 24);
 Result[1]:= $FFFFFF or ((Alpha2 and $FF) shl 24);
end;

//---------------------------------------------------------------------------
function cColorAlpha2(Color, Alpha: Cardinal): TColor2; overload;
begin
 Result:= cColor2((Color and $FFFFFF) or ((Alpha and $FF) shl 24));
end;

//---------------------------------------------------------------------------
function cColorAlpha2(Color1, Color2, Alpha1, Alpha2: Cardinal): TColor2;
begin
 Result[0]:= cColorAlpha1(Color1, Alpha1);
 Result[1]:= cColorAlpha1(Color2, Alpha2);
end;

//---------------------------------------------------------------------------
function cColorAlpha2of(const Colors: TColor2; Alpha: Single): TColor2;
var
 iAlpha: Integer;
begin
 iAlpha:= Round(Alpha * 255.0);

 Result[0]:= cColorAlpha1(Colors[0], iAlpha);
 Result[1]:= cColorAlpha1(Colors[1], iAlpha);
end;

//---------------------------------------------------------------------------
function cColor4(Color: Cardinal): TColor4;
begin
 Result[0]:= Color;
 Result[1]:= Color;
 Result[2]:= Color;
 Result[3]:= Color;
end;

//---------------------------------------------------------------------------
function cColor4(Color1, Color2, Color3, Color4: Cardinal): TColor4;
begin
 Result[0]:= Color1;
 Result[1]:= Color2;
 Result[2]:= Color3;
 Result[3]:= Color4;
end;

//---------------------------------------------------------------------------
function cRGB4(r, g, b: Cardinal; a: Cardinal = 255): TColor4; inline;
begin
 Result:= cColor4(cRGB1(r, g, b, a));
end;

//---------------------------------------------------------------------------
function cRGB4(r1, g1, b1, a1, r2, g2, b2, a2: Cardinal): TColor4; inline;
begin
 Result[0]:= cRGB1(r1, g1, b1, a1);
 Result[1]:= Result[0];
 Result[2]:= cRGB1(r2, g2, b2, a2);
 Result[3]:= Result[2];
end;

//---------------------------------------------------------------------------
function cGray4(Gray: Cardinal): TColor4;
begin
 Result:= cColor4(((Gray and $FF) or ((Gray and $FF) shl 8) or
  ((Gray and $FF) shl 16)) or $FF000000);
end;

//---------------------------------------------------------------------------
function cGray4(Gray1, Gray2, Gray3, Gray4: Cardinal): TColor4;
begin
 Result[0]:= ((Gray1 and $FF) or ((Gray1 and $FF) shl 8) or ((Gray1 and $FF)
  shl 16)) or $FF000000;
 Result[1]:= ((Gray2 and $FF) or ((Gray2 and $FF) shl 8) or ((Gray2 and $FF)
  shl 16)) or $FF000000;
 Result[2]:= ((Gray3 and $FF) or ((Gray3 and $FF) shl 8) or ((Gray3 and $FF)
  shl 16)) or $FF000000;
 Result[3]:= ((Gray4 and $FF) or ((Gray4 and $FF) shl 8) or ((Gray4 and $FF)
  shl 16)) or $FF000000;
end;

//---------------------------------------------------------------------------
function cAlpha4(Alpha: Cardinal): TColor4;
begin
 Result:= cColor4($FFFFFF or ((Alpha and $FF) shl 24));
end;

//---------------------------------------------------------------------------
function cAlpha4(Alpha1, Alpha2, Alpha3, Alpha4: Cardinal): TColor4;
begin
 Result[0]:= $FFFFFF or ((Alpha1 and $FF) shl 24);
 Result[1]:= $FFFFFF or ((Alpha2 and $FF) shl 24);
 Result[2]:= $FFFFFF or ((Alpha3 and $FF) shl 24);
 Result[3]:= $FFFFFF or ((Alpha4 and $FF) shl 24);
end;

//---------------------------------------------------------------------------
function cGrayAlpha4(Gray, Alpha: Cardinal): TColor4;
begin
 Result:= cColor4(((Gray and $FF) or ((Gray and $FF) shl 8) or
  ((Gray and $FF) shl 16)) or (Alpha shl 24));
end;

//---------------------------------------------------------------------------
function cGrayAlpha4(Gray1, Gray2, Gray3, Gray4, Alpha1, Alpha2, Alpha3,
 Alpha4: Cardinal): TColor4;
begin
 Result[0]:= ((Gray1 and $FF) or ((Gray1 and $FF) shl 8) or ((Gray1 and $FF)
  shl 16)) or (Alpha1 shl 24);
 Result[1]:= ((Gray2 and $FF) or ((Gray2 and $FF) shl 8) or ((Gray2 and $FF)
  shl 16)) or (Alpha2 shl 24);
 Result[2]:= ((Gray3 and $FF) or ((Gray3 and $FF) shl 8) or ((Gray3 and $FF)
  shl 16)) or (Alpha3 shl 24);
 Result[3]:= ((Gray4 and $FF) or ((Gray4 and $FF) shl 8) or ((Gray4 and $FF)
  shl 16)) or (Alpha4 shl 24);
end;

//---------------------------------------------------------------------------
function cColorAlpha4(Color, Alpha: Cardinal): TColor4; overload;
begin
 Result:= cColor4(cColorAlpha1(Color, Alpha));
end;

//---------------------------------------------------------------------------
function cColorAlpha4(Color1, Color2, Color3, Color4, Alpha1, Alpha2, Alpha3,
 Alpha4: Cardinal): TColor4;
begin
 Result[0]:= cColorAlpha1(Color1, Alpha1);
 Result[1]:= cColorAlpha1(Color2, Alpha2);
 Result[2]:= cColorAlpha1(Color3, Alpha3);
 Result[3]:= cColorAlpha1(Color4, Alpha4);
end;

//---------------------------------------------------------------------------
function cColorGrayAlpha4(Color, Gray, Alpha: Cardinal): TColor4; overload;
begin
 Result:= cColor4(cColorGrayAlpha1(Color, Gray, Alpha));
end;

//---------------------------------------------------------------------------
function cColorGrayAlpha4(Color1, Color2, Color3, Color4,
 Gray1, Gray2, Gray3, Gray4, Alpha1, Alpha2, Alpha3,
 Alpha4: Cardinal): TColor4; overload;
begin
 Result[0]:= cColorGrayAlpha1(Color1, Gray1, Alpha1);
 Result[1]:= cColorGrayAlpha1(Color2, Gray2, Alpha2);
 Result[2]:= cColorGrayAlpha1(Color3, Gray3, Alpha3);
 Result[3]:= cColorGrayAlpha1(Color4, Gray4, Alpha4);
end;

//---------------------------------------------------------------------------
function ColorToFixed4(const Colors: TColor4): TAsphyreColor4;
begin
 Result[0]:= Colors[0];
 Result[1]:= Colors[1];
 Result[2]:= Colors[2];
 Result[3]:= Colors[3];
end;

//---------------------------------------------------------------------------
function FixedToColor4(const Colors: TAsphyreColor4): TColor4;
begin
 Result[0]:= Colors[0];
 Result[1]:= Colors[1];
 Result[2]:= Colors[2];
 Result[3]:= Colors[3];
end;

//---------------------------------------------------------------------------
function Point4(x1, y1, x2, y2, x3, y3, x4, y4: Single): TPoint4;
begin
 Result[0].x:= x1;
 Result[0].y:= y1;
 Result[1].x:= x2;
 Result[1].y:= y2;
 Result[2].x:= x3;
 Result[2].y:= y3;
 Result[3].x:= x4;
 Result[3].y:= y4;
end;

//---------------------------------------------------------------------------
function Point4(const p1, p2, p3, p4: TPoint2): TPoint4;
begin
 Result:= Point4(p1.x, p1.y, p2.x, p2.y, p3.x, p3.y, p4.x, p4.y);
end;

//---------------------------------------------------------------------------
function pRect4(const Rect: TRect): TPoint4;
begin
 Result[0].x:= Rect.Left;
 Result[0].y:= Rect.Top;
 Result[1].x:= Rect.Right;
 Result[1].y:= Rect.Top;
 Result[2].x:= Rect.Right;
 Result[2].y:= Rect.Bottom;
 Result[3].x:= Rect.Left;
 Result[3].y:= Rect.Bottom;
end;

//---------------------------------------------------------------------------
function pBounds4(_Left, _Top, _Width, _Height: Single): TPoint4;
begin
 Result[0].X:= _Left;
 Result[0].Y:= _Top;
 Result[1].X:= _Left + _Width;
 Result[1].Y:= _Top;
 Result[2].X:= _Left + _Width;
 Result[2].Y:= _Top + _Height;
 Result[3].X:= _Left;
 Result[3].Y:= _Top + _Height;
end;

//---------------------------------------------------------------------------
function pBounds4s(_Left, _Top, _Width, _Height, Theta: Single): TPoint4;
begin
 Result:= pBounds4(_Left, _Top, Round(_Width * Theta), Round(_Height * Theta));
end;

//---------------------------------------------------------------------------
function pBounds4sc(_Left, _Top, _Width, _Height, Theta: Single): TPoint4;
var
 Left, Top: Single;
 Width, Height: Single;
begin
 if (Theta = 1.0) then
  Result:= pBounds4(_Left, _Top, _Width, _Height)
 else
  begin
   Width := _Width * Theta;
   Height:= _Height * Theta;
   Left  := _Left + ((_Width - Width) * 0.5);
   Top   := _Top + ((_Height - Height) * 0.5);
   Result:= pBounds4(Left, Top, Round(Width), Round(Height));
  end;
end;

//---------------------------------------------------------------------------
function pMirror4(const Point4: TPoint4): TPoint4;
begin
 Result[0].X:= Point4[1].X;
 Result[0].Y:= Point4[0].Y;
 Result[1].X:= Point4[0].X;
 Result[1].Y:= Point4[1].Y;
 Result[2].X:= Point4[3].X;
 Result[2].Y:= Point4[2].Y;
 Result[3].X:= Point4[2].X;
 Result[3].Y:= Point4[3].Y;
end;

//---------------------------------------------------------------------------
function pFlip4(const Point4: TPoint4): TPoint4;
begin
 Result[0].X:= Point4[0].X;
 Result[0].Y:= Point4[2].Y;
 Result[1].X:= Point4[1].X;
 Result[1].Y:= Point4[3].Y;
 Result[2].X:= Point4[2].X;
 Result[2].Y:= Point4[0].Y;
 Result[3].X:= Point4[3].X;
 Result[3].Y:= Point4[1].Y;
end;

//---------------------------------------------------------------------------
function pShift4(const Points: TPoint4; const ShiftBy: TPoint2): TPoint4;
begin
 Result[0].x:= Points[0].x + ShiftBy.x;
 Result[0].y:= Points[0].y + ShiftBy.y;
 Result[1].x:= Points[1].x + ShiftBy.x;
 Result[1].y:= Points[1].y + ShiftBy.y;
 Result[2].x:= Points[2].x + ShiftBy.x;
 Result[2].y:= Points[2].y + ShiftBy.y;
 Result[3].x:= Points[3].x + ShiftBy.x;
 Result[3].y:= Points[3].y + ShiftBy.y;
end;

//---------------------------------------------------------------------------
function pRotate4(const Origin, Size, Middle: TPoint2; Angle: Single;
 Theta: Single): TPoint4;
var
 CosPhi: Single;
 SinPhi: Single;
 Index : Integer;
 Points: TPoint4;
 Point : TPoint2;
begin
 CosPhi:= Cos(Angle);
 SinPhi:= Sin(Angle);

 // create 4 points centered at (0, 0)
 Points:= pBounds4(-Middle.x, -Middle.y, Size.x, Size.y);

 // process the created points
 for Index:= 0 to 3 do
  begin
   // scale the point
   Points[Index].x:= Points[Index].x * Theta;
   Points[Index].y:= Points[Index].y * Theta;

   // rotate the point around Phi
   Point.x:= (Points[Index].x * CosPhi) - (Points[Index].y * SinPhi);
   Point.y:= (Points[Index].y * CosPhi) + (Points[Index].x * SinPhi);

   // translate the point to (Origin)
   Points[Index].x:= Point.x + Origin.x;
   Points[Index].y:= Point.y + Origin.y;
  end;

 Result:= Points;
end;

//---------------------------------------------------------------------------
function pRotate4se(const Origin, Size, Middle: TPoint2; Angle: Single;
 Theta: Single): TPoint4;
var
 CosPhi: Single;
 SinPhi: Single;
 Index : Integer;
 Points: TPoint4;
 Point : TPoint2;
begin
 CosPhi:= Cos(Angle);
 SinPhi:= Sin(Angle);

 // create 4 points centered at (0, 0)
 Points:= pBounds4(-Middle.x, -Middle.y, Size.x, Size.y);

 // process the created points
 for Index:= 0 to 3 do
  begin
   // scale the point
   Points[Index].x:= Points[Index].x * Theta;
   Points[Index].y:= Points[Index].y * Theta;

   // rotate the point around Phi
   Point.x:= (Points[Index].x * CosPhi) - (Points[Index].y * SinPhi);
   Point.y:= (Points[Index].y * CosPhi) + (Points[Index].x * SinPhi);

   // translate the point to (Origin)
   Points[Index].x:= Point.x + Origin.x + Middle.x;
   Points[Index].y:= Point.y + Origin.y + Middle.y;
  end;

 Result:= Points;
end;

//---------------------------------------------------------------------------
function pRotate4c(const Origin, Size: TPoint2; Angle: Single;
 Theta: Single): TPoint4;
begin
 Result:= pRotate4(Origin, Size, Point2(Size.x * 0.5, Size.y * 0.5), Angle,
  Theta);
end;

//-----------------------------------------------------------------------------
function pxBounds4(Left, Top, Width, Height: Integer): TPoint4px;
begin
 Result[0].X:= Left;
 Result[0].Y:= Top;
 Result[1].X:= Left + Width;
 Result[1].Y:= Top;
 Result[2].X:= Left + Width;
 Result[2].Y:= Top + Height;
 Result[3].X:= Left;
 Result[3].Y:= Top + Height;
end;

//-----------------------------------------------------------------------------
function pxBounds4f(Left, Top, Width, Height: Single): TPoint4;
begin
 Result[0].X:= Left;
 Result[0].Y:= Top;
 Result[1].X:= Left + Width;
 Result[1].Y:= Top;
 Result[2].X:= Left + Width;
 Result[2].Y:= Top + Height;
 Result[3].X:= Left;
 Result[3].Y:= Top + Height;
end;

//-----------------------------------------------------------------------------
function pxRect4(Left, Top, Right, Bottom: Integer): TPoint4px;
begin
 Result[0].x:= Left;
 Result[0].y:= Top;
 Result[1].x:= Right;
 Result[1].y:= Top;
 Result[2].x:= Right;
 Result[2].y:= Bottom;
 Result[3].x:= Left;
 Result[3].y:= Bottom;
end;

//---------------------------------------------------------------------------
function pxRect4(const Pos, Size: TPoint2px): TPoint4px; overload;
begin
 Result[0].x:= Pos.x;
 Result[0].y:= Pos.y;
 Result[1].x:= Pos.x + Size.x;
 Result[1].y:= Pos.y;
 Result[2].x:= Pos.x + Size.x;
 Result[2].y:= Pos.y + Size.y;
 Result[3].x:= Pos.x;
 Result[3].y:= Pos.y + Size.y;
end;

//---------------------------------------------------------------------------
function pxRect4(const Rect: TRect): TPoint4px; overload;
begin
 Result:= pxRect4(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
end;

//---------------------------------------------------------------------------
function pxRotate90r(const Points: TPoint4px): TPoint4px;
begin
 Result[0]:= Points[3];
 Result[1]:= Points[0];
 Result[2]:= Points[1];
 Result[3]:= Points[2];
end;

var
   CosTable256: array[0..255] of Double;

procedure InitCosTable;
var
  i: Integer;
begin
   for i:=0 to 255 do
    CosTable256[i] := Cos((i/256)*2*PI);
end;

function Cos256(i: Integer): Double;
begin
  Result := CosTable256[i and 255];
end;

function Sin256(i: Integer): Double;
begin
  Result := CosTable256[(i+192) and 255];
end;


function OverlapRect(const Rect1, Rect2: TRect): Boolean;
begin
 Result:= (Rect1.Left < Rect2.Right)and(Rect1.Right > Rect2.Left)and
  (Rect1.Top < Rect2.Bottom)and(Rect1.Bottom > Rect2.Top);
end;

function OverlapQuadrangle(Q1, Q2: TPoint4): Boolean;
var
 d1, d2, d3, d4: Single;
begin

 d1 := (Q1[2].X - Q1[1].X) * (Q2[0].X - Q1[0].X) + (Q1[2].Y - Q1[1].Y) * (Q2[0].Y - Q1[0].Y);
 d2 := (Q1[3].X - Q1[2].X) * (Q2[0].X - Q1[1].X) + (Q1[3].Y - Q1[2].Y) * (Q2[0].Y - Q1[1].Y);
 d3 := (Q1[0].X - Q1[3].X) * (Q2[0].X - Q1[2].X) + (Q1[0].Y - Q1[3].Y) * (Q2[0].Y - Q1[2].Y);
 d4 := (Q1[1].X - Q1[0].X) * (Q2[0].X - Q1[3].X) + (Q1[1].Y - Q1[0].Y) * (Q2[0].Y - Q1[3].Y);
 if (d1 >= 0) and (d2 >= 0) and (d3 >= 0) and (d4 >= 0) then
 begin
  Result := True;
  Exit;
 end;

 d1 := (Q1[2].X - Q1[1].X) * (Q2[1].X - Q1[0].X) + (Q1[2].Y - Q1[1].Y) * (Q2[1].Y - Q1[0].Y);
 d2 := (Q1[3].X - Q1[2].X) * (Q2[1].X - Q1[1].X) + (Q1[3].Y - Q1[2].Y) * (Q2[1].Y - Q1[1].Y);
 d3 := (Q1[0].X - Q1[3].X) * (Q2[1].X - Q1[2].X) + (Q1[0].Y - Q1[3].Y) * (Q2[1].Y - Q1[2].Y);
 d4 := (Q1[1].X - Q1[0].X) * (Q2[1].X - Q1[3].X) + (Q1[1].Y - Q1[0].Y) * (Q2[1].Y - Q1[3].Y);
 if (d1 >= 0) and (d2 >= 0) and (d3 >= 0) and (d4 >= 0) then
 begin
  Result := True;
  Exit;
 end;

 d1 := (Q1[2].X - Q1[1].X) * (Q2[2].X - Q1[0].X) + (Q1[2].Y - Q1[1].Y) * (Q2[2].Y - Q1[0].Y);
 d2 := (Q1[3].X - Q1[2].X) * (Q2[2].X - Q1[1].X) + (Q1[3].Y - Q1[2].Y) * (Q2[2].Y - Q1[1].Y);
 d3 := (Q1[0].X - Q1[3].X) * (Q2[2].X - Q1[2].X) + (Q1[0].Y - Q1[3].Y) * (Q2[2].Y - Q1[2].Y);
 d4 := (Q1[1].X - Q1[0].X) * (Q2[2].X - Q1[3].X) + (Q1[1].Y - Q1[0].Y) * (Q2[2].Y - Q1[3].Y);
 if (d1 >= 0) and (d2 >= 0) and (d3 >= 0) and (d4 >= 0) then
 begin
  Result := True;
  Exit;
 end;

 d1 := (Q1[2].X - Q1[1].X) * (Q2[3].X - Q1[0].X) + (Q1[2].Y - Q1[1].Y) * (Q2[3].Y - Q1[0].Y);
 d2 := (Q1[3].X - Q1[2].X) * (Q2[3].X - Q1[1].X) + (Q1[3].Y - Q1[2].Y) * (Q2[3].Y - Q1[1].Y);
 d3 := (Q1[0].X - Q1[3].X) * (Q2[3].X - Q1[2].X) + (Q1[0].Y - Q1[3].Y) * (Q2[3].Y - Q1[2].Y);
 d4 := (Q1[1].X - Q1[0].X) * (Q2[3].X - Q1[3].X) + (Q1[1].Y - Q1[0].Y) * (Q2[3].Y - Q1[3].Y);
 if (d1 >= 0) and (d2 >= 0) and (d3 >= 0) and (d4 >= 0) then
 begin
  Result := True;
  Exit;
 end;

 Result := False;
end;

function PointInRect(const Point: TPoint; const Rect: TRect): Boolean;
begin
 Result:= (Point.X >= Rect.Left)and(Point.X <= Rect.Right)and
  (Point.Y >= Rect.Top)and(Point.Y <= Rect.Bottom);
end;

function RectInRect(const Rect1, Rect2: TRect): Boolean;
begin
 Result:= (Rect1.Left >= Rect2.Left)and(Rect1.Right <= Rect2.Right)and
  (Rect1.Top >= Rect2.Top)and(Rect1.Bottom <= Rect2.Bottom);
end;


function PtInPolygon(Pt : TPoint2 ; Pg : TPolygon):Boolean;
var
  N, Counter , I : Integer;
  XInters : Real;
  P1, P2 : TPoint2;
begin
  N := High(Pg);
  Counter := 0;
  P1 := Pg[0];
  for I := 1 to N do
  begin
    P2 := Pg[I mod N];
    if Pt.y > Min(P1.y, P2.y) then
      if Pt.y <= Max(P1.y, P2.y) then
        if Pt.x <= Max(P1.x, P2.x) then
          if P1.y <> P2.y then
          begin
            XInters := (Pt.y - P1.y) * (P2.x - P1.x) / (P2.y - P1.y) + P1.x;
            if (P1.x = P2.x) or (Pt.x <= XInters) then Inc(Counter);
          end;
    P1 := P2;
  end;
  Result := (Counter mod 2 <> 0);
end;

function OverlapPolygon(P1, P2: TPolygon): Boolean;
var
  Poly1, Poly2 : TPolygon;
  I, J : Integer;
  xx , yy : Single;
  StartP, EndP : Integer;
  Found : Boolean;
begin
  Found := False;
  { Find polygon with fewer points }
  if High(P1) < High(P2) then
  begin
    Poly1 := P1;
    Poly2 := P2;
  end
  else
  begin
    Poly1 := P2;
    Poly2 := P1;
  end;

  for I := 0 to High(Poly1) - 1 do
  begin
    { Trace new line }
    StartP := Round(Min(Poly1[I].x, Poly1[I+1].x));
    EndP   := Round(Max(Poly1[I].x, Poly1[I+1].x));


    if StartP = EndP then
    { A vertical line (ramp = inf) }
    begin
      xx := StartP;
      StartP := Round(Min(Poly1[I].y, Poly1[I+1].y));
      EndP   := Round(Max(Poly1[I].y, Poly1[I+1].y));
      { Follow a vertical line }
      for J := StartP to EndP do
      begin
        { line equation }
        if PtInPolygon(Point2(xx,J), Poly2) then
        begin
          Found := True;
          Break;
        end;
      end;
    end
    else
    { Follow a usual line (ramp <> inf) }
    begin
      { A Line which X is its variable i.e. Y = f(X) }
      if Abs(Poly1[I].x -  Poly1[I+1].x) >= Abs(Poly1[I].y -  Poly1[I+1].y) then
      begin
        StartP := Round(Min(Poly1[I].x, Poly1[I+1].x));
        EndP   := Round(Max(Poly1[I].x, Poly1[I+1].x));
        for J := StartP to EndP do
        begin
          xx := J;
          { line equation }
          yy := (Poly1[I+1].y - Poly1[I].y) / (Poly1[I+1].x - Poly1[I].x) * (xx - Poly1[I].x) + Poly1[I].y;
          if PtInPolygon(Point2(xx,yy), Poly2) then
          begin
            Found := True;
            Break;
          end;
        end;
      end
      { A Line which Y is its variable i.e. X = f(Y) }
      else
      begin
        StartP := Round(Min(Poly1[I].y, Poly1[I+1].y));
        EndP   := Round(Max(Poly1[I].y, Poly1[I+1].y));
        for J := StartP to EndP do
        begin
          yy := J;
          { line equation }
          xx := (Poly1[I+1].x - Poly1[I].x) / (Poly1[I+1].y - Poly1[I].y) * (yy - Poly1[I].y) + Poly1[I].x;
          if PtInPolygon(Point2(xx,yy), Poly2) then
          begin
            Found := True;
            Break;
          end;
        end;
      end;
    end;
    if Found then Break;
  end;

  { Maybe one polygon is completely inside another }
  if not Found then
    Found := PtInPolygon(Poly1[0], Poly2) or PtInPolygon(Poly2[0], Poly1);

  Result := Found;
end;

function Angle256(X, Y: Integer): Real;
begin
     Result := (Arctan2(X, Y) * -40.5) + 128;
end;

function AngleDiff(SrcAngle, DestAngle: Single): Single;
var
    Diff: Single;
begin
     Diff := DestAngle - SrcAngle;
     if (SrcAngle > DestAngle) then
     begin
          if (SrcAngle > 128) and (DestAngle < 128) then
              if (Diff < 128.0) then Diff := Diff + 256.0 ;
          if (Diff > 128.0) then  Diff := Diff - 256.0;
     end
     else
     begin
          if (Diff > 128.0) then  Diff := Diff - 256.0;
     end;
     Result:= Diff;
end;
//---------------------------------------------------------------------------
end.
