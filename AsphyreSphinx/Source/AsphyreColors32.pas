unit AsphyreColors32;

//----------------------------------------------------------------------------
interface

//----------------------------------------------------------------------------
function BlendColors(c1, c2: Longword; Alpha: Integer): Longword;
function LerpColors(c1, c2: Longword; Alpha: Single): Longword;
function AddColors(c1, c2: Longword): Longword;
function SubColors(c1, c2: Longword): Longword;
function AvgColors(c1, c2: Longword): Longword; overload;
function AvgColors(c1, c2, c3, c4: Longword): Longword; overload;

//----------------------------------------------------------------------------
implementation

//----------------------------------------------------------------------------
uses
 AsphyreUtils;

//----------------------------------------------------------------------------
type
 TSegColor = record
  Red  : Integer;
  Green: Integer;
  Blue : Integer;
  Alpha: Integer;
 end;

//----------------------------------------------------------------------------
function ColorToSeg(Color: Longword): TSegColor;
begin
 Result.Blue := Color and $FF;
 Result.Green:= (Color shr 8) and $FF;
 Result.Red  := (Color shr 16) and $FF;
 Result.Alpha:= (Color shr 24) and $FF;
end;

//----------------------------------------------------------------------------
function SegToColor(const Color: TSegColor): Longword;
begin
 Result:=
  Cardinal(Color.Blue) or
  (Cardinal(Color.Green) shl 8) or
  (Cardinal(Color.Red) shl 16) or
  (Cardinal(Color.Alpha) shl 24);
end;

//----------------------------------------------------------------------------
function BlendColors(c1, c2: Longword; Alpha: Integer): Longword;
var
 Seg1, Seg2, Res: TSegColor;
begin
 Seg1:= ColorToSeg(c1);
 Seg2:= ColorToSeg(c2);

 Res.Red  := Seg1.Red + iMul8(Seg2.Red - Seg1.Red, Alpha);
 Res.Green:= Seg1.Green + iMul8(Seg2.Green - Seg1.Green, Alpha);
 Res.Blue := Seg1.Blue + iMul8(Seg2.Blue - Seg1.Blue, Alpha);
 Res.Alpha:= Seg1.Alpha + iMul8(Seg2.Alpha - Seg1.Alpha, Alpha);

 Result:= SegToColor(Res);
end;

//----------------------------------------------------------------------------
function LerpColors(c1, c2: Longword; Alpha: Single): Longword;
var
 Seg1, Seg2, Res: TSegColor;
begin
 Seg1:= ColorToSeg(c1);
 Seg2:= ColorToSeg(c2);

 Res.Red  := Seg1.Red + Round((Seg2.Red - Seg1.Red) * Alpha);
 Res.Green:= Seg1.Green + Round((Seg2.Green - Seg1.Green) * Alpha);
 Res.Blue := Seg1.Blue + Round((Seg2.Blue - Seg1.Blue) * Alpha);
 Res.Alpha:= Seg1.Alpha + Round((Seg2.Alpha - Seg1.Alpha) * Alpha);

 Result:= SegToColor(Res);
end;

//----------------------------------------------------------------------------
function AddColors(c1, c2: Longword): Longword;
var
 Seg1, Seg2, Res: TSegColor;
begin
 Seg1:= ColorToSeg(c1);
 Seg2:= ColorToSeg(c2);

 Res.Red  := Min2(Seg1.Red + Seg2.Red, 255);
 Res.Green:= Min2(Seg1.Green + Seg2.Green, 255);
 Res.Blue := Min2(Seg1.Blue + Seg2.Blue, 255);
 Res.Alpha:= Min2(Seg1.Alpha + Seg2.Alpha, 255);

 Result:= SegToColor(Res);
end;

//----------------------------------------------------------------------------
function SubColors(c1, c2: Longword): Longword;
var
 Seg1, Seg2, Res: TSegColor;
begin
 Seg1:= ColorToSeg(c1);
 Seg2:= ColorToSeg(c2);

 Res.Red  := Min2(Seg1.Red + Seg2.Red, 255);
 Res.Green:= Min2(Seg1.Green + Seg2.Green, 255);
 Res.Blue := Min2(Seg1.Blue + Seg2.Blue, 255);
 Res.Alpha:= Min2(Seg1.Alpha + Seg2.Alpha, 255);

 Result:= SegToColor(Res);
end;

//----------------------------------------------------------------------------
function AvgColors(c1, c2: Longword): Longword;
var
 Seg1, Seg2, Res: TSegColor;
begin
 Seg1:= ColorToSeg(c1);
 Seg2:= ColorToSeg(c2);

 Res.Red  := (Seg1.Red + Seg2.Red) div 2;
 Res.Green:= (Seg1.Green + Seg2.Green) div 2;
 Res.Blue := (Seg1.Blue + Seg2.Blue) div 2;
 Res.Alpha:= (Seg1.Alpha + Seg2.Alpha) div 2;

 Result:= SegToColor(Res);
end;

//----------------------------------------------------------------------------
function AvgColors(c1, c2, c3, c4: Longword): Longword;
var
 Seg1, Seg2, Seg3, Seg4, Res: TSegColor;
begin
 Seg1:= ColorToSeg(c1);
 Seg2:= ColorToSeg(c2);
 Seg3:= ColorToSeg(c3);
 Seg4:= ColorToSeg(c4);

 Res.Red  := (Seg1.Red + Seg2.Red + Seg3.Red + Seg4.Red) div 4;
 Res.Green:= (Seg1.Green + Seg2.Green + Seg3.Green + Seg4.Green) div 4;
 Res.Blue := (Seg1.Blue + Seg2.Blue + Seg3.Blue + Seg4.Blue) div 4;
 Res.Alpha:= (Seg1.Alpha + Seg2.Alpha + Seg3.Alpha + Seg4.Alpha) div 4;

 Result:= SegToColor(Res);
end;

//----------------------------------------------------------------------------
end.
