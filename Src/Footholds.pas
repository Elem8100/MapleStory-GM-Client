unit Footholds;

interface

uses System.Types, Generics.Collections, Generics.Defaults, WZDirectory, WZIMGFile,
  Global, SysUtils, StrUtils;

type
  TFoothold = class
  private
    FP1, FP2: TPoint;
    FID, FNextID, FPrevID, FZ, FPlatform: Integer;
    FPrev, FNext: TFoothold;
  public
    constructor Create(const P1, P2: TPoint; AID: Integer);
    function IsWall: Boolean;
    property ID: Integer read FID write FID;
    property PlatformID: Integer read FPlatform write FPlatform;
    property Z: Integer read FZ write FZ;
    property NextID: Integer read FNextID write FNextID;
    property PrevID: Integer read FPrevID write FPrevID;
    property X1: Integer read FP1.X;
    property X2: Integer read FP2.X;
    property Y1: Integer read FP1.Y;
    property Y2: Integer read FP2.Y;
    property Prev: TFoothold read FPrev write FPrev;
    property Next: TFoothold read FNext write FNext;
  end;

  TFootholdTree = class
  private
    FFootholds: TObjectList<TFoothold>;
    FP1, FP2: TPoint;
  public
    constructor Create(const P1, P2: TPoint);
    destructor Destroy; override;
    function GetPrev(FH: TFoothold): TFoothold;
    function GetNext(FH: TFoothold): TFoothold;
    function FindBelow(const Pos: TPoint; var FH: TFoothold): TPoint;
    function FindWallR(P: TPoint): TFoothold;
    function FindWallL(P: TPoint): TFoothold;
    function ClosePlatform(FH: TFoothold): Boolean;
    procedure Insert(F: TFoothold);
    property FHs: TObjectList<TFoothold> read FFootholds;
    procedure DrawFootHolds;

  class var
    MinX1: TList<Integer>;
    MaxX2: TList<Integer>;
    This: TFootHoldtree;
    class procedure CreateFHs;
  end;


implementation
     uses MapleMap;
{ TFoothold }

constructor TFoothold.Create(const P1, P2: TPoint; AID: Integer);
begin
  FP1 := P1;
  FP2 := P2;
  FID := AID;
end;

function TFoothold.IsWall: Boolean;
begin
  Result := FP1.X = FP2.X;
end;

{ TFootholdTree }

constructor TFootholdTree.Create(const P1, P2: TPoint);
begin
  FP1 := P1;
  FP2 := P2;
  FFootholds := TObjectList<TFoothold>.Create;
end;

destructor TFootholdTree.Destroy;
var
  F: TFoothold;
begin
  FFootholds.Free;
  MinX1.Free;
  MaxX2.Free;
  inherited;
end;

function TFootholdTree.FindBelow(const Pos: TPoint; var FH: TFoothold): TPoint;
var
  F: TFoothold;
  X, Y: Integer;
  First: Boolean;
  MaxY: Double;
  CMax: Double;
  NewPos: TPoint;
begin
  First := True;
  X := Pos.X;
  Y := Pos.Y;
  for F in FFootholds do
    if ((X >= F.X1) and (X <= F.X2)) or ((X >= F.X2) and (X <= F.X1)) then
    begin
      if (First) then
      begin
        if (F.X1 = F.X2) then
          Continue;
        MaxY := (F.Y1 - F.Y2) / (F.X1 - F.X2) * (X - F.X1) + F.Y1;
        FH := F;
        if (MaxY >= Y) then
          First := False;
      end
      else
      begin
        if (F.X1 = F.X2) then
          Continue;
        CMax := (F.Y1 - F.Y2) / (F.X1 - F.X2) * (X - F.X1) + F.Y1;
        if (CMax < MaxY) and (CMax >= Y) then
        begin
          FH := F;
          MaxY := CMax;
        end;
      end;
    end;

  if (not First) then
  begin
    NewPos.X := X;
    NewPos.Y := Trunc(MaxY);
    Exit(NewPos);
  end
  else
    Exit(Point(99999, 99999));
end;

function TFootholdTree.GetNext(FH: TFoothold): TFoothold;
var
  F: TFoothold;
begin
  for F in FFootholds do
    if F.FID = FH.FNextID then
      Exit(F);
end;

function TFootholdTree.GetPrev(FH: TFoothold): TFoothold;
var
  F: TFoothold;
begin
  for F in FFootholds do
    if F.FID = FH.FPrevID then
      Exit(F);
end;

function TFootholdTree.FindWallR(P: TPoint): TFoothold;
var
  F: TFoothold;
  X, Y: Integer;
  First: Boolean;
  MaxX: Double;
  CMax: Double;
begin
  Result := nil;
  First := True;
  X := P.X;
  Y := P.Y;
  for F in FFootholds do
    if (F.IsWall) and (F.X1 <= P.X) and (F.Y1 <= P.Y) and (F.Y2 >= P.Y) then
    begin
      if (First) then
      begin
        MaxX := F.X1;
        Result := F;
        if (MaxX <= X) then
          First := False;
      end
      else

      begin
        CMax := F.X1;
        if (CMax > MaxX) and (CMax <= X) then
        begin
          MaxX := CMax;
          Result := F;
        end;
      end;

    end;

end;

function TFootholdTree.FindWallL(P: TPoint): TFoothold;
var
  F: TFoothold;
  X, Y: Integer;
  First: Boolean;
  MaxX: Double;
  CMax: Double;
begin
  Result := nil;
  First := True;
  X := P.X;
  Y := P.Y;
  for F in FFootholds do
    if (F.IsWall) and (F.X1 >= P.X) and (F.Y1 >= P.Y) and (F.Y2 <= P.Y) then
    begin
      if (First) then
      begin
        MaxX := F.X1;
        Result := F;
        if (MaxX >= X) then
          First := False;
      end
      else

      begin
        CMax := F.X1;
        if (CMax < MaxX) and (CMax >= X) then
        begin
          MaxX := CMax;
          Result := F;
        end;
      end;

    end;

end;

function TFootholdTree.ClosePlatform(FH: TFoothold): Boolean;
var
  Count: Integer;
  F: TFoothold;
begin
  Count := 0;
  for F in FFootholds do
    if (F.PlatformID = FH.PlatformID) and (F.IsWall) then
      Inc(Count);
  if Count = 2 then
    Result := True;
end;

procedure TFootholdTree.Insert(F: TFoothold);
begin
  FFootholds.Add(F);
end;

procedure TFootholdTree.DrawFootHolds;
var
  FH: TFoothold;
  WX, WY: Single;
  Off: Integer;
begin
  WX := SpriteEngine.WorldX;
  WY := SpriteEngine.WorldY;
  for FH in FFootholds do
  begin
    GameCanvas.Line(FH.X1 - WX, FH.Y1 - WY, FH.X2 - WX, FH.Y2 - WY, $FFFF0000);
    if FH.X1 <> FH.X2 then
      GameCanvas.Line(FH.X1 - WX, FH.Y1 - WY + 1, FH.X2 - WX, FH.Y2 - WY + 1, $FFFF0000)
    else
      GameCanvas.Line(FH.X1 - WX + 1, FH.Y1 - WY, FH.X2 - WX + 1, FH.Y2 - WY, $FFFF0000);
  end;
end;

class procedure TFootholdTree.CreateFHs;
var
  X1, Y1, X2, Y2, I, J: Integer;
  FH: TFoothold;
  Iter, Iter2, Iter3: TWZIMGEntry;
  FHs: TObjectList<TFoothold>;
begin

  if This = nil then
  begin
    This := TFootholdTree.Create(Point(100, 10), Point(-100, -100));
    MinX1 := TList<Integer>.Create;
    MaxX2 := TList<Integer>.Create;
  end
  else
  begin
    This.FFootholds.Clear;
    MinX1.Clear;
    MaxX2.Clear;
  end;

  for Iter in TMap.ImgFile.Child['foothold'].Children do
    for Iter2 in Iter.Children do
      for Iter3 in Iter2.Children do
      begin
        X1 := Iter3.Get('x1', '0');
        Y1 := Iter3.Get('y1', '0');
        X2 := Iter3.Get('x2', '0');
        Y2 := Iter3.Get('y2', '0');
        FH := TFoothold.Create(Point(X1, Y1), Point(X2, Y2), 0);
        FH.NextID := Iter3.Get('next', '0');
        FH.PrevID := Iter3.Get('prev', '0');
        FH.PlatformID := StrToInt(Iter2.Name);
        FH.ID := StrToInt(Iter3.Name);
        FH.Z := StrToInt(Iter.Name);
        This.Insert(FH);
        MinX1.Add(X1);
        MaxX2.Add(X2);
      end;

  MinX1.Sort;
  MaxX2.Sort;
  FHs := This.FHs;
  for I := 0 to FHs.Count - 1 do
    for J := 0 to FHs.Count - 1 do
    begin
      if I = J then
        Continue;
      if FHs[J].ID = FHs[I].PrevID then
        FHs[I].Prev := FHs[J];
      if FHs[J].ID = FHs[I].NextID then
        FHs[I].Next := FHs[J];
    end;

end;



end.
