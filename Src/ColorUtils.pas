unit ColorUtils;

interface

uses
  Windows, SysUtils, StrUtils,  Generics.Collections,
  Classes, Math, Vcl.Graphics,
  Dx9Textures,  AsphyreTypes;
type
 TColorEffect=(ceNone,ceHue,ceSaturation,ceLight);

procedure HSVvar(Texture: TDX9LockableTexture; oHue, oSat, oVal: Integer);
procedure HSVvar2(bitmap: TBitmap; oHue, oSat, oVal: Integer);
procedure HSV2RGB(var px: TRGB32; H, S, V: Integer);
procedure RGB2HSV(RGB: TRGB32; var h, s, v: Integer);

implementation

procedure HSVvar(Texture: TDX9LockableTexture; oHue, oSat, oVal: Integer);
var
  x, y: Integer;
  Hue, Sat, Val: Integer;
  pDest: Pointer;
  nPitch: Integer;
  ARGB: PRGB32;
begin
  Texture.Lock(Rect(0, 0, Texture.Width, Texture.Height), pDest, nPitch);
  ARGB := pDest;
  for y := 0 to Texture.Height - 1 do
  begin
    for x := 0 to Texture.Width - 1 do
    begin
      RGB2HSV(ARGB^, Hue, Sat, Val);
      HSV2RGB(ARGB^, Hue + oHue, Sat + oSat, Val + oVal);
      Inc(ARGB);
    end;
  end;
  Texture.Unlock;
end;

procedure HSVvar2(bitmap: TBitmap; oHue, oSat, oVal: Integer);
var
  x, y: Integer;
  Hue, Sat, Val: Integer;
  ARGB: PRGB32;
begin

  for y := 0 to bitmap.Height-1 do
  begin
    ARGB := bitmap.ScanLine[y];
    for x := 0 to bitmap.width-1 do
    begin
      RGB2HSV(ARGB^, Hue, Sat, Val);
      HSV2RGB(ARGB^, Hue + oHue, Sat + oSat, Val + oVal);
      inc(ARGB);
    end;
  end;
end;

procedure HSV2RGB(var px: TRGB32; H, S, V: Integer);
const
  divisor: Integer = 99 * 60;
var
  f: Integer;
  hTemp: Integer;
  p, q, t: Integer;
  VS: Integer;
begin
  // check limits (changed at 2.1.1)
  if H < 0 then
    H := 360 + H
  else if H > 359 then
    H := H - 360;
  if S < 0 then
    S := 0
  else if S > 99 then
    S := 99;
  if V < 0 then
    V := 0
  else if V > 99 then
    V := 99;
  //
  if S = 0 then
  begin
    px.r := V;
    px.g := V;
    px.b := V;
  end
  else
  begin
    if H = 360 then
      hTemp := 0
    else
      hTemp := H;
    f := hTemp mod 60;
    hTemp := hTemp div 60;
    VS := V * S;
    p := V - VS div 99;
    q := V - (VS * f) div divisor;
    t := V - (VS * (60 - f)) div divisor;
    with px do
    begin
      case hTemp of
        0:
          begin
            R := V;
            G := t;
            B := p
          end;
        1:
          begin
            R := q;
            G := V;
            B := p
          end;
        2:
          begin
            R := p;
            G := V;
            B := t
          end;
        3:
          begin
            R := p;
            G := q;
            B := V
          end;
        4:
          begin
            R := t;
            G := p;
            B := V
          end;
        5:
          begin
            R := V;
            G := p;
            B := q
          end;
      end
    end
  end;
  px.r := Round(px.r / 99 * 255);
  px.g := Round(px.g / 99 * 255);
  px.b := Round(px.b / 99 * 255);
end;

procedure RGB2HSV(RGB: TRGB32; var h, s, v: Integer);

  procedure MinMaxInt(const i, j, k: Integer; var min, max: Integer);
  begin
    if i > j then
    begin
      if i > k then
        max := i
      else
        max := k;
      if j < k then
        min := j
      else
        min := k
    end
    else
    begin
      if j > k then
        max := j
      else
        max := k;
      if i < k then
        min := i
      else
        min := k
    end;
  end;

var
  Delta: Integer;
  MinValue: Integer;
  r, g, b: Integer;
begin
  r := Round(RGB.r / 255 * 99);
  g := Round(RGB.g / 255 * 99);
  b := Round(RGB.b / 255 * 99);
  MinMaxInt(r, g, b, MinValue, v);
  Delta := v - MinValue;
  if v = 0 then
    s := 0
  else
    s := (99 * Delta) div v;
  if s = 0 then
    h := 0
  else
  begin
    if r = v then
      h := (60 * (g - b)) div Delta
    else if g = v then
      h := 120 + (60 * (b - r)) div Delta
    else if b = v then
      h := 240 + (60 * (r - g)) div Delta;
    if h < 0 then
      h := h + 360;
  end;
end;

initialization



finalization



end.
