unit LadderRopes;

interface

uses System.Types, Generics.Collections, Generics.Defaults, WZDirectory, WZIMGFile,
  Global, SysUtils, StrUtils, MapleMap;

type
  TLadderRope = record
  private
    class var LadderRopeList: TList<TLadderRope>;
  public
    X, Y1, Y2, Page, uf, L: Integer;
    class procedure DrawLadderRopeLine;static;
    class function Find(P: TPoint; var OnLadder: Boolean): TLadderRope; static;
    class procedure Create; overload; static;
  end;

implementation

class function TLadderRope.Find(P: TPoint; var OnLadder: Boolean): TLadderRope;
var
  L: TLadderRope;
begin
  OnLadder := False;

  for L in LadderRopeList do
    if (P.X > L.X - 10) and (P.X < L.X + 10) and (P.Y < L.Y2 + 12) and (P.Y > L.Y1 - 12) then
    begin
      Result := L;
      OnLadder := True;
    end;

end;

class procedure TLadderRope.Create;
var
  LadderRope: TLadderRope;
  Iter: TWZIMGEntry;
begin
  if LadderRopeList = nil then
    LadderRopeList := TList<TLadderRope>.Create
  else
    LadderRopeList.Clear;

  for Iter in TMap.ImgFile.Child['ladderRope'].Children do
  begin
    LadderRope.X := Iter.Get('x', '0');
    LadderRope.Y1 := Iter.Get('y1', '0');
    LadderRope.Y2 := Iter.Get('y2', '0');
    LadderRope.L := Iter.Get('l', '0');
    LadderRope.Page := Iter.Get('page', '0');
    LadderRope.uf := Iter.Get('uf', '0');
    LadderRopeList.Add(LadderRope);
  end;
end;

class procedure TLadderRope.DrawLadderRopeLine;
var
  LadderRope: TLadderRope;
  WX, WY: Single;
begin
  WX := SpriteEngine.WorldX;
  WY := SpriteEngine.WorldY;
  for LadderRope in LadderRopeList do
  begin
    GameCanvas.Line(LadderRope.X - WX, LadderRope.Y1 - WY, LadderRope.X  - WX, LadderRope.Y2  - WY, $FF00FF00);
    GameCanvas.Line(LadderRope.X - WX+1, LadderRope.Y1 - WY, LadderRope.X  - WX+1, LadderRope.Y2  - WY, $FF00FF00);
  end;
end;

initialization

finalization

TLadderRope.LadderRopeList.Free;

end.
