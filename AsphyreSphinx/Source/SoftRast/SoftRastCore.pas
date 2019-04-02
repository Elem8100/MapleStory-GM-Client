unit SoftRastCore;

//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Types, Classes, SysUtils, AsphyreTypes, SoftRastTypes, Vectors2px;

//---------------------------------------------------------------------------
// Clears the entire buffer with the given number of pixels
//---------------------------------------------------------------------------
procedure SRClear(Addr: Pointer; Count: Integer; Color: Cardinal); stdcall;

//---------------------------------------------------------------------------
// Switches Red and Blue channels in the specified color
//---------------------------------------------------------------------------
function SRDisplaceRB(Color: Longword): Longword; register;

//---------------------------------------------------------------------------
// Blends two colors together with the specified Quantity
//---------------------------------------------------------------------------
function SRBlendColors(Color1, Color2: Cardinal; Quantity: Integer): Cardinal; register;

//---------------------------------------------------------------------------
// Mappes the texture with the given coordinates to destination buffer
//---------------------------------------------------------------------------
procedure SRTextureMapEx(Address, Texture: TSRAddress; ClipRect: TRect;
 PCoords: TPoint4px; Color: TColor4; TexSrc: TSRTexCoords; Op: Integer); stdcall;

procedure SRTriMap(Address, Texture: TSRAddress; ClipRect: TRect;
 Pt1, Pt2, Pt3: TPoint2px; c1, c2, c3: Cardinal; TexSrc: TSRTexCoords; Op: Integer); stdcall;

//---------------------------------------------------------------------------
// Draws an non-antialiased line between two points
// (unrevised, to-do: check wtf SRLine2 is for!)
//---------------------------------------------------------------------------
procedure SRLine(Addr: TSRAddress; ClipRect: TRect; Src, Dest: TPoint2px;
 Color1, Color2: Cardinal; Op: Integer); stdcall;

//---------------------------------------------------------------------------
// Draws an non-antialiased line between two points
// (unrevised)
//---------------------------------------------------------------------------
procedure SRLine2(Addr: TSRAddress; ClipRect: TRect; Src, Dest: TPoint2px;
 Color1, Color2: Cardinal; Op: Integer); stdcall;
 
//---------------------------------------------------------------------------
implementation
uses
 Math, SoftRastPutPixel, SoftRastTexMap, SoftRastTexRect;

//---------------------------------------------------------------------------
type
 TPolyPoint = record
  Left     : Integer;
  Right    : Integer;
  Color1   : Cardinal;
  Color2   : Cardinal;
  TxCoords : TSRTexCoords;
 end;

//---------------------------------------------------------------------------
var
 PolyData: array of TPolyPoint;

//---------------------------------------------------------------------------
procedure SRClear(Addr: Pointer; Count: Integer; Color: Cardinal); stdcall;
{$include include\srClearCode.inc}

//---------------------------------------------------------------------------
// register order EAX, EDX, ECX
// preserve EDI, ESI, ESP, EBP, and EBX
function SRBlendColors(Color1, Color2: Cardinal; Quantity: Integer): Cardinal; register;
{$include include\srBlendColors.inc}

//---------------------------------------------------------------------------
function SRDisplaceRB(Color: Longword): Longword; register;
asm
 mov ecx, eax
 mov edx, eax

 shl eax, 8
 shr eax, 24
 shl ecx, 24
 shr ecx, 8
 and edx, 0FF00FF00h
 or  eax, ecx
 or  eax, edx
end;

//---------------------------------------------------------------------------
procedure SRLine(Addr: TSRAddress; ClipRect: TRect; Src, Dest: TPoint2px;
 Color1, Color2: Cardinal; Op: Integer); stdcall;
var
 xDelta, yDelta, vFixed, vDelta, i, vPos, Alpha, AlphaVel: Integer;
begin
 xDelta:= Abs(Dest.X - Src.X);
 yDelta:= Abs(Dest.Y - Src.Y);

 if (xDelta < 1)and(yDelta < 1) then
  begin
   SRPutPixel(Addr, ClipRect, Src.X, Src.Y, SRBlendColors(Color1, Color2, 128), Op);
   Exit;
  end; 

 if (yDelta > xDelta) then
  begin
   vFixed:= Src.X shl 16;
   vDelta:= ((Dest.X - Src.X) shl 16) div yDelta;
   vPos:= Src.Y;
   Alpha:= 0;
   AlphaVel:= $FFFF div yDelta;
   if (Dest.Y < vPos) then
    begin
     vPos:= Dest.Y;
     vFixed:= Dest.X shl 16;
     vDelta:= -vDelta;
     Alpha:= $FFFF;
     AlphaVel:= -AlphaVel;
    end;

   for i:= 0 to yDelta do
    begin
     SRPutPixel(Addr, ClipRect, vFixed shr 16, vPos + i, SRBlendColors(Color2,
      Color1, Alpha shr 8), Op);

     Inc(vFixed, vDelta);
     Inc(Alpha, AlphaVel);
    end;
  end else
  begin
   vFixed:= Src.Y shl 16;
   vDelta:= ((Dest.Y - Src.Y) shl 16) div xDelta;
   vPos:= Src.X;
   Alpha:= 0;
   AlphaVel:= $FFFF div xDelta;
   if (Dest.X < vPos) then
    begin
     vPos:= Dest.X;
     vFixed:= Dest.Y shl 16;
     vDelta:= -vDelta;
     Alpha:= $FFFF;
     AlphaVel:= -AlphaVel;
    end;

   for i:= 0 to xDelta do
    begin
     SRPutPixel(Addr, ClipRect, vPos + i, vFixed shr 16, SRBlendColors(Color2,
      Color1, Alpha shr 8), Op);

     Inc(vFixed, vDelta);
     Inc(Alpha, AlphaVel);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure SRLine2(Addr: TSRAddress; ClipRect: TRect; Src, Dest: TPoint2px;
 Color1, Color2: Cardinal; Op: Integer); stdcall;
var
 xDelta, yDelta, vFixed, vDelta, i, vPos, Alpha, AlphaVel: Integer;
begin
 xDelta:= Abs(Dest.X - Src.X);
 yDelta:= Abs(Dest.Y - Src.Y);

 if (xDelta < 1)and(yDelta < 1) then
  begin
   SRPutPixel(Addr, ClipRect, Src.X, Src.Y, SRBlendColors(Color1, Color2, 128), Op);
   Exit;
  end;

 if (yDelta > xDelta) then
  begin
   vFixed:= Src.X shl 16;
   vDelta:= ((Dest.X - Src.X) shl 16) div yDelta;
   vPos:= Src.Y;
   Alpha:= 0;
   AlphaVel:= $FFFF div yDelta;
   if (Dest.Y < vPos) then
    begin
     vPos:= Dest.Y;
     vFixed:= Dest.X shl 16;
     vDelta:= -vDelta;
     Alpha:= $FFFF;
     AlphaVel:= -AlphaVel;

     Inc(vPos);
     Inc(vFixed, vDelta);
     Inc(Alpha, AlphaVel);
    end;
   Dec(yDelta);

   for i:= 0 to yDelta do
    begin
     SRPutPixel(Addr, ClipRect, vFixed shr 16, vPos + i, SRBlendColors(Color1,
      Color2, Alpha shr 8), Op);

     Inc(vFixed, vDelta);
     Inc(Alpha, AlphaVel);
    end;
  end else
  begin
   vFixed:= Src.Y shl 16;
   vDelta:= ((Dest.Y - Src.Y) shl 16) div xDelta;
   vPos:= Src.X;
   Alpha:= 0;
   AlphaVel:= $FFFF div xDelta;
   if (Dest.X < vPos) then
    begin
     vPos:= Dest.X;
     vFixed:= Dest.Y shl 16;
     vDelta:= -vDelta;
     Alpha:= $FFFF;
     AlphaVel:= -AlphaVel;

     Inc(vPos);
     Inc(vFixed, vDelta);
     Inc(Alpha, AlphaVel);
    end;
   Dec(xDelta);

   for i:= 0 to xDelta do
    begin
     SRPutPixel(Addr, ClipRect, vPos + i, vFixed shr 16, SRBlendColors(Color1,
      Color2, Alpha shr 8), Op);

     Inc(vFixed, vDelta);
     Inc(Alpha, AlphaVel);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure SRTexRect(Address, Texture: TSRAddress; ClipRect: TRect; PCoords: TPoint4px;
 Color: TColor4; TexSrc: TSRTexCoords; Op: Integer); stdcall;
var
 j, i, Top, Left, Delta, Alpha, Beta0, Beta1, Height, Width: Integer;
 iColor1, iColor2: Cardinal;
 ScreenY, ScreenX: Integer;
 u, v, uDelta, vDelta: Real;
 SrcBits, DestBits: Pointer;
 SrcPos, SrcMove: Cardinal;
 LineFx: TTexStretchLineFx;
begin
{ if (TexSrc.u2 > TexSrc.u1) then TexSrc.u2:= TexSrc.u2 - 1 else TexSrc.u1:= TexSrc.u1 - 1;
 if (TexSrc.v2 > TexSrc.v1) then TexSrc.v2:= TexSrc.v2 - 1 else TexSrc.v1:= TexSrc.v1 - 1;

 if (PCoords[1].X > PCoords[0].X) then
  begin
   PCoords[1].X:= PCoords[1].X - 1;
   PCoords[2].X:= PCoords[2].X - 1;
  end else
  begin
   PCoords[0].X:= PCoords[0].X - 1;
   PCoords[3].X:= PCoords[3].X - 1;
  end;

 if (PCoords[2].Y > PCoords[1].Y) then
  begin
   PCoords[2].Y:= PCoords[2].Y - 1;
   PCoords[3].Y:= PCoords[3].Y - 1;
  end else
  begin
   PCoords[0].Y:= PCoords[0].Y - 1;
   PCoords[1].Y:= PCoords[1].Y - 1;
  end;}

 LineFx:= SRTexStretchLineFx(Op);

 // 1. determine the height of destination rectangle
 Top:= Min(PCoords[0].Y, PCoords[3].Y);
 Height:= Abs(PCoords[0].Y - PCoords[3].Y) + 1;

 // 2. flip texture if the rectangle is flipped
 if (PCoords[0].Y > PCoords[3].Y) then
  begin
   i:= TexSrc.v1;
   TexSrc.v1:= TexSrc.v2;
   TexSrc.v2:= i;
  end;

 // 3. determine the width of rectangle
 Width:= Abs(PCoords[1].X - PCoords[0].X) + 1;
 Left:= Min(PCoords[0].X, PCoords[1].X);

 // 4. don't accept too small textures
 if (Width < 2)or(Height < 2) then Exit;

 // 4. mirror texture if the rectangle is mirrored
 if (PCoords[0].Y > PCoords[3].Y) then
  begin
   i:= TexSrc.u1;
   TexSrc.u1:= TexSrc.u2;
   TexSrc.u2:= i;
  end;

 // 5. determine texture horizontal movement
 u:= TexSrc.u1;
 uDelta:= (TexSrc.u2 - TexSrc.u1) / (Width - 1);
 ScreenX:= Left;
 Beta0:= 0;
 Beta1:= 0;
 if (ScreenX < ClipRect.Left) then
  begin
   Delta:= ClipRect.Left - ScreenX;
   Beta0:= (Delta * 255) div (Width - 1);
   Inc(ScreenX, Delta);
   Dec(Width, Delta);
   u:= u + (uDelta * Delta);
  end;
 if (ScreenX + Width > ClipRect.Right) then
  begin
   Delta:= Width - (ClipRect.Right - ScreenX);
   Beta1:= (Delta * 255) div (Width - 1);
   Width:= ClipRect.Right - ScreenX;
  end;

 if (Width <= 0.0)or(Height <= 0.0) then Exit;

 // 6. determine vertical movement
 v:= TexSrc.v1;
 vDelta:= (TexSrc.v2 - TexSrc.v1) / (Height - 1);

 iColor1:= 0;
 iColor2:= 0;
 for j:= 0 to Height - 1 do
  begin
   ScreenY:= Top + j;
   if (ScreenY >= ClipRect.Top)and(ScreenY < ClipRect.Bottom) then
    begin
     // calculate diffuse colors, if enabled
     if (Op and srDiffuse > 0) then
      begin
       // (a) Find edge colors
       Alpha:= (j * 255) div (Height - 1);
       iColor1:= SRBlendColors(Color[3], Color[0], Alpha);
       iColor2:= SRBlendColors(Color[2], Color[1], Alpha);
       // (b) Clip colors
       iColor1:= SRBlendColors(iColor2, iColor1, Beta0);
       iColor2:= SRBlendColors(iColor1, iColor2, Beta1);
      end;
     // (c) Set source and destination pointers
     SrcBits:= Pointer(Integer(Texture.Bits) + (Trunc(v) * Texture.Pitch));
     DestBits:= Pointer(Integer(Address.Bits) + (ScreenY * Address.Pitch) + (ScreenX * 4));
     // (d) Pixel movement
     SrcPos:= Trunc(u * $10000);
     SrcMove:= Trunc(uDelta * $10000);

     // render the line
     LineFx(DestBits, SrcBits, Width, SrcPos, SrcMove, iColor1, iColor2);
    end;

   v:= v + vDelta;
  end;
end;

//---------------------------------------------------------------------------
function PolyHeight(PCoords: TPoint4px; ClipRect: TRect; var Min, Max: Integer): Integer;
begin
 Min:= PCoords[0].Y;
 Max:= PCoords[0].Y;
 if (PCoords[1].Y < Min) then Min:= PCoords[1].Y;
 if (PCoords[2].Y < Min) then Min:= PCoords[2].Y;
 if (PCoords[3].Y < Min) then Min:= PCoords[3].Y;

 if (PCoords[1].Y > Max) then Max:= PCoords[1].Y;
 if (PCoords[2].Y > Max) then Max:= PCoords[2].Y;
 if (PCoords[3].Y > Max) then Max:= PCoords[3].Y;

 if (Min < ClipRect.Top) then Min:= ClipRect.Top;
 if (Max > ClipRect.Bottom - 1) then Max:= ClipRect.Bottom - 1;

 Result:= (Max - Min) + 1;
end;

//---------------------------------------------------------------------------
procedure InitPoly(Size: Integer);
var
 i: Integer;
begin
 SetLength(PolyData, Size);

 for i:= 0 to Size - 1 do
  begin
   PolyData[i].Left  := High(Integer);
   PolyData[i].Right := Low(Integer);
  end;
end;

//---------------------------------------------------------------------------
procedure Exchange(var Value1, Value2: Integer); overload;
var
 Temp: Integer;
begin
 Temp  := Value1;
 Value1:= Value2;
 Value2:= Temp;
end;

//---------------------------------------------------------------------------
procedure Exchange(var Value1, Value2: Cardinal); overload;
var
 Temp: Cardinal;
begin
 Temp  := Value1;
 Value1:= Value2;
 Value2:= Temp;
end;

//---------------------------------------------------------------------------
procedure FindPolyLine(x1, y1, x2, y2, u1, v1, u2, v2, TopY: Integer; Color1, Color2: Cardinal);
var
 u, v, uDelta, vDelta: Integer;
 xPos, yPos, xFixed, xInc, i: Integer;
 cFixed, cDelta, Height: Integer;
begin
 // 1. horizontal line
 if (y1 = y2) then Exit;

 // 1. assure y1 < y2
 Height:= Abs(y2 - y1){ + 1};
 if (y2 < y1) then
  begin
   Exchange(y1, y2);
   Exchange(x1, x2);
   Exchange(u1, u2);
   Exchange(v1, v2);
   Exchange(Color1, Color2);
  end;

 // texture vector (16.16 fixed-point)
 u:= u1 shl 16;
 v:= v1 shl 16;
 uDelta:= ((u2 - u1) shl 16) div (Height);
 vDelta:= ((v2 - v1) shl 16) div (Height);

 // color (16.16 fixed-point)
 cFixed:= 0;
 cDelta:= $FF00 div Height;

 // screen vector (16.16 fixed-point)
 xFixed:= x1 shl 16;
 xInc:= ((x2 - x1) shl 16) div Height;

 for i:= 0 to Height - 1 do
  begin
   // get array-compatible coordinates
   xPos:= SmallInt(xFixed shr 16);
   yPos:= i + y1 - TopY;

   // update start coordinate
   if (yPos >= 0)and(yPos < Length(PolyData)) then
    begin
     if (xPos < PolyData[yPos].Left) then
      begin
       PolyData[yPos].Left:= xPos;
       PolyData[yPos].TxCoords.u1:= u shr 16;
       PolyData[yPos].TxCoords.v1:= v shr 16;
       PolyData[yPos].Color1:= SRBlendColors(Color2, Color1, cFixed shr 8);
      end;
     // update final coordinate
     if (xPos > PolyData[yPos].Right) then
      begin
       PolyData[yPos].Right:= xPos;
       PolyData[yPos].TxCoords.u2:= u shr 16;
       PolyData[yPos].TxCoords.v2:= v shr 16;
       PolyData[yPos].Color2:= SRBlendColors(Color2, Color1, cFixed shr 8);
      end;
    end;

   Inc(xFixed, xInc);
   Inc(cFixed, cDelta);
   Inc(u, uDelta);
   Inc(v, vDelta);
  end;
end;

//---------------------------------------------------------------------------
procedure SRRenderPoly(Address, Texture: TSRAddress; TopY: Integer; ClipRect: TRect; Op: Integer);
var
 i, Left, Width, u, v, uDelta, vDelta, Diff, Beta: Integer;
 DestBits: Pointer;
 LineFunc: TTexUVLineFx;
 iColor1, iColor2: Cardinal;
begin
 LineFunc:= SRTexUVLineFx(Op);

 for i:= 0 to Length(PolyData) - 1 do
  begin
   Left:= PolyData[i].Left;
   Width:= (PolyData[i].Right - PolyData[i].Left){ + 1};

   if (Width > 0) then
    begin
     // 1. set texture coordinates
     u:= PolyData[i].TxCoords.u1 * $10000;
     v:= PolyData[i].TxCoords.v1 * $10000;
     uDelta:= (Int64(PolyData[i].TxCoords.u2 - PolyData[i].TxCoords.u1) * $10000) div (Width);
     vDelta:= (Int64(PolyData[i].TxCoords.v2 - PolyData[i].TxCoords.v1) * $10000) div (Width);
     iColor1:= PolyData[i].Color1;
     iColor2:= PolyData[i].Color2;

     // 2. perform clipping
     if (Left < ClipRect.Left) then
      begin
       Diff:= ClipRect.Left - Left;
       Beta:= (Diff * 255) div Width;
       Left:= ClipRect.Left;
       Width:= Width - Diff;
       u:= u + (uDelta * Diff);
       v:= v + (vDelta * Diff);
       iColor1:= SRBlendColors(iColor1, iColor2, 255 - Beta);
      end;
     if (Left + Width > ClipRect.Right) then
      begin
       Diff:= (Width - (ClipRect.Right - Left));
       Beta:= (Diff * 255) div Width;
       Width:= ClipRect.Right - Left;
       iColor2:= SRBlendColors(iColor1, iColor2, Beta);
      end; 

     if (Width < 1) then Continue;

     // 3. calculate pointers
     DestBits:= Pointer(Integer(Address.Bits) + ((i + TopY) * Address.Pitch) + (Left * 4));
     LineFunc(DestBits, Texture.Bits, Width, u, v, uDelta, vDelta, Texture.Pitch, iColor1, iColor2);
    end;
  end;
end;

//---------------------------------------------------------------------------
function RemoveLastPoint(p: TPoint4px): TPoint4px;
var
 Max0, Max1, i: Integer;
begin
 // 1. find two points with biggest X coordinate
 Max0:= 0;
 for i:= 1 to 3 do
  if (p[i].X > p[Max0].X) then Max0:= i;

 Max1:= (Max0 + 1) mod 4;
 for i:= 0 to 3 do
  if (p[i].X > p[Max1].X)and(i <> Max0) then Max1:= i;

 // 2. decrease these coordinates by one
 p[Max0].X:= p[Max0].X - 1;
 p[Max1].X:= p[Max1].X - 1;

 // 3. find two points with biggest Y coordinate
 Max0:= 0;
 for i:= 1 to 3 do
  if (p[i].Y > p[Max0].Y) then Max0:= i;

 Max1:= (Max0 + 1) mod 4;
 for i:= 0 to 3 do
  if (p[i].Y > p[Max1].Y)and(i <> Max0) then Max1:= i;

 // 2. decrease these coordinates by one
 p[Max0].Y:= p[Max0].Y - 1;
 p[Max1].Y:= p[Max1].Y - 1;

 Result:= p;
end;

//---------------------------------------------------------------------------
procedure SRTextureMapEx(Address, Texture: TSRAddress; ClipRect: TRect;
 PCoords: TPoint4px; Color: TColor4; TexSrc: TSRTexCoords; Op: Integer); stdcall;
var
 pHeight: Integer;
 Top, Bottom: Integer;
begin
 if (TexSrc.u2 > TexSrc.u1) then TexSrc.u2:= TexSrc.u2 - 1 else TexSrc.u1:= TexSrc.u1 - 1;
 if (TexSrc.v2 > TexSrc.v1) then TexSrc.v2:= TexSrc.v2 - 1 else TexSrc.v1:= TexSrc.v1 - 1;

 if (PCoords[0].Y = PCoords[1].Y)and(PCoords[2].Y = PCoords[3].Y)and
  (PCoords[0].X = PCoords[3].X)and(PCoords[1].X = PCoords[2].X) then
   begin
    PCoords:= RemoveLastPoint(PCoords);
    SRTexRect(Address, Texture, ClipRect, PCoords, Color, TexSrc, Op);
    Exit;
   end;

 // 1. calculate polygon vertical height
 pHeight:= PolyHeight(PCoords, ClipRect, Top, Bottom);
 if (pHeight < 1) then Exit;

 // 2. initialize polygon data
 InitPoly(pHeight);

 // 3. calculate the sides of polygon
 FindPolyLine(PCoords[0].X, PCoords[0].Y, PCoords[1].X, PCoords[1].Y, TexSrc.u1,
  TexSrc.v1, TexSrc.u2, TexSrc.v1, Top, Color[0], Color[1]);
 FindPolyLine(PCoords[1].X, PCoords[1].Y, PCoords[2].X, PCoords[2].Y, TexSrc.u2,
  TexSrc.v1, TexSrc.u2, TexSrc.v2, Top, Color[1], Color[2]);
 FindPolyLine(PCoords[2].X, PCoords[2].Y, PCoords[3].X, PCoords[3].Y, TexSrc.u2,
  TexSrc.v2, TexSrc.u1, TexSrc.v2, Top, Color[2], Color[3]);
 FindPolyLine(PCoords[3].X, PCoords[3].Y, PCoords[0].X, PCoords[0].Y, TexSrc.u1,
  TexSrc.v2, TexSrc.u1, TexSrc.v1, Top, Color[3], Color[0]);

 SRRenderPoly(Address, Texture, Top, ClipRect, Op);
end;

//---------------------------------------------------------------------------
procedure SRTriMap(Address, Texture: TSRAddress; ClipRect: TRect;
 Pt1, Pt2, Pt3: TPoint2px; c1, c2, c3: Cardinal; TexSrc: TSRTexCoords;
 Op: Integer); stdcall;
var
 Coords: TPoint4px;
 pHeight: Integer;
 Top, Bottom: Integer;
begin
 Coords[0]:= Pt1;
 Coords[1]:= Pt2;
 Coords[2]:= Pt3;
 Coords[3]:= Pt3;

 pHeight:= PolyHeight(Coords, ClipRect, Top, Bottom);
 if (pHeight < 1) then Exit;

 InitPoly(pHeight);

 FindPolyLine(Coords[0].X, Coords[0].Y, Coords[1].X, Coords[1].Y, TexSrc.u1,
  TexSrc.v1, TexSrc.u2, TexSrc.v1, Top, c1, c2);
 FindPolyLine(Coords[1].X, Coords[1].Y, Coords[2].X, Coords[2].Y, TexSrc.u2,
  TexSrc.v1, TexSrc.u2, TexSrc.v2, Top, c2, c3);
 FindPolyLine(Coords[2].X, Coords[2].Y, Coords[0].X, Coords[0].Y, TexSrc.u1,
  TexSrc.v2, TexSrc.u1, TexSrc.v1, Top, c3, c1);

 SRRenderPoly(Address, Texture, Top, ClipRect, Op);
end;

//---------------------------------------------------------------------------
end.


