unit PXT.TypesEx;

interface

uses
  System.Types, Math;

type
  TRGB32 = record
    B, G, R, A: Byte;
  end;

  TRGB32Array = array[0..MaxInt div SizeOf(TRGB32) - 1] of TRGB32;

  PRGB32 = ^TRGB32;

  PRGB32Array = ^TRGB32Array;

  PPoint4 = ^TPoint4;

  TPoint4 = array[0..3] of TPointF;

  TPolygon = array of TPoint;

function GetA(const Color: Longword): Byte; inline;

function GetR(const Color: Longword): Byte; inline;

function GetG(const Color: Longword): Byte; inline;

function GetB(const Color: Longword): Byte; inline;

function ARGB(const A, R, G, B: Byte): Longword; inline;

function cRGB1(r, g, b: Cardinal; a: Cardinal = 255): Cardinal; inline;

function AngleDiff(SrcAngle, DestAngle: Single): Single;

function Cos256(i: Integer): Double;

function Sin256(i: Integer): Double;

function OverlapRect(const Rect1, Rect2: TRect): Boolean;

function PtInPolygon(Pt: TPoint; Pg: TPolygon): Boolean;

function OverlapQuadrangle(Q1, Q2: TPoint4): Boolean;

function OverlapPolygon(P1, P2: TPolygon): Boolean;

function RectInRect(const Rect1, Rect2: TRect): Boolean;

function PointInRect(const Point: TPoint; const Rect: TRect): Boolean;

function GetAngle256(const X1, Y1, X2, Y2: Integer): Integer;

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


function cRGB1(r, g, b: Cardinal; a: Cardinal = 255): Cardinal; inline;
begin
  Result := b or (g shl 8) or (r shl 16) or (a shl 24);
end;

function Point4(x1, y1, x2, y2, x3, y3, x4, y4: Single): TPoint4;
begin
  Result[0].x := x1;
  Result[0].y := y1;
  Result[1].x := x2;
  Result[1].y := y2;
  Result[2].x := x3;
  Result[2].y := y3;
  Result[3].x := x4;
  Result[3].y := y4;
end;

var
  CosTable256: array[0..255] of Double;

procedure InitCosTable;
var
  i: Integer;
begin
  for i := 0 to 255 do
    CosTable256[i] := Cos((i / 256) * 2 * PI);
end;

function Cos256(i: Integer): Double;
begin
  Result := CosTable256[i and 255];
end;

function Sin256(i: Integer): Double;
begin
  Result := CosTable256[(i + 192) and 255];
end;

function OverlapRect(const Rect1, Rect2: TRect): Boolean;
begin
  Result := (Rect1.Left < Rect2.Right) and (Rect1.Right > Rect2.Left) and (Rect1.Top < Rect2.Bottom) and (Rect1.Bottom > Rect2.Top);
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
  Result := (Point.X >= Rect.Left) and (Point.X <= Rect.Right) and (Point.Y >= Rect.Top) and (Point.Y <= Rect.Bottom);
end;

function RectInRect(const Rect1, Rect2: TRect): Boolean;
begin
  Result := (Rect1.Left >= Rect2.Left) and (Rect1.Right <= Rect2.Right) and (Rect1.Top >= Rect2.Top) and (Rect1.Bottom <= Rect2.Bottom);
end;

function PtInPolygon(Pt: TPoint; Pg: TPolygon): Boolean;
var
  N, Counter, I: Integer;
  XInters: Real;
  P1, P2: TPoint;
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
            if (P1.x = P2.x) or (Pt.x <= XInters) then
              Inc(Counter);
          end;
    P1 := P2;
  end;
  Result := (Counter mod 2 <> 0);
end;

function OverlapPolygon(P1, P2: TPolygon): Boolean;
var
  Poly1, Poly2: TPolygon;
  I, J: Integer;
  xx, yy: Single;
  StartP, EndP: Integer;
  Found: Boolean;
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
    StartP := Round(Min(Poly1[I].x, Poly1[I + 1].x));
    EndP := Round(Max(Poly1[I].x, Poly1[I + 1].x));

    if StartP = EndP then
    { A vertical line (ramp = inf) }
    begin
      xx := StartP;
      StartP := Round(Min(Poly1[I].y, Poly1[I + 1].y));
      EndP := Round(Max(Poly1[I].y, Poly1[I + 1].y));
      { Follow a vertical line }
      for J := StartP to EndP do
      begin
        { line equation }
        if PtInPolygon(Point(Round(xx), J), Poly2) then
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
      if Abs(Poly1[I].x - Poly1[I + 1].x) >= Abs(Poly1[I].y - Poly1[I + 1].y) then
      begin
        StartP := Round(Min(Poly1[I].x, Poly1[I + 1].x));
        EndP := Round(Max(Poly1[I].x, Poly1[I + 1].x));
        for J := StartP to EndP do
        begin
          xx := J;
          { line equation }
          yy := (Poly1[I + 1].y - Poly1[I].y) / (Poly1[I + 1].x - Poly1[I].x) * (xx - Poly1[I].x) + Poly1[I].y;
          if PtInPolygon(Point(Round(xx), Round(yy)), Poly2) then
          begin
            Found := True;
            Break;
          end;
        end;
      end
      { A Line which Y is its variable i.e. X = f(Y) }
      else
      begin
        StartP := Round(Min(Poly1[I].y, Poly1[I + 1].y));
        EndP := Round(Max(Poly1[I].y, Poly1[I + 1].y));
        for J := StartP to EndP do
        begin
          yy := J;
          { line equation }
          xx := (Poly1[I + 1].x - Poly1[I].x) / (Poly1[I + 1].y - Poly1[I].y) * (yy - Poly1[I].y) + Poly1[I].x;
          if PtInPolygon(Point(Round(xx), Round(yy)), Poly2) then
          begin
            Found := True;
            Break;
          end;
        end;
      end;
    end;
    if Found then
      Break;
  end;

  { Maybe one polygon is completely inside another }
  if not Found then
    Found := PtInPolygon(Poly1[0], Poly2) or PtInPolygon(Poly2[0], Poly1);

  Result := Found;
end;

function Angle256(X, Y: Integer): Real;
begin
  Result := (Arctan2(X, Y) *  - 40.5) + 128;
end;

function GetAngle256(const X1, Y1, X2, Y2: Integer): Integer;
const
  PiConv256 = -128.0 / PI; // ~ 40.743665431
begin
  if (X2 = X1) then
  begin
    Result := 128;
    if (Y1 < Y2) then
      Result := 0;
    Exit;
  end;
  Result := Round(ArcTan2(X2 - X1, Y2 - Y1) * PiConv256) and $FF;
end;

function AngleDiff(SrcAngle, DestAngle: Single): Single;
var
  Diff: Single;
begin
  Diff := DestAngle - SrcAngle;
  if (SrcAngle > DestAngle) then
  begin
    if (SrcAngle > 128) and (DestAngle < 128) then
      if (Diff < 128.0) then
        Diff := Diff + 256.0;
    if (Diff > 128.0) then
      Diff := Diff - 256.0;
  end
  else
  begin
    if (Diff > 128.0) then
      Diff := Diff - 256.0;
  end;
  Result := Diff;
end;

initialization
  InitCosTable;

end.

