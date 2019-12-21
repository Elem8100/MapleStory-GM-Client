unit MapleCharacterEx;

interface

uses
  Windows, SysUtils, StrUtils, Generics.Collections, Math, AsphyreSprite, Footholds, LadderRopes,
  ChatBalloon, Classes, Global, Tools, MapleMap, MapleCharacter;

type
  TMoveDirection = (mdLeft, mdRight, mdNone);

  TMoveType = (mtStand, mtMove, mtJump, mtFly);

  TPlayerEx = class(TPlayer)
    MoveDirection: TMoveDirection;
    MoveSpeed: Single;
    JumpEdge: Integer;
    MoveType: TMoveType;
    class procedure Spawn(IDList: string);
    procedure DoMove(const Movecount: Single); override;
    procedure DoDraw; override;
  end;

  TAvatarPartEx = class(TAvatarParts)
  public
    procedure DoMove(const Movecount: Single); override;
    procedure DoDraw; override;
  end;

implementation

uses
  MapleChair, WZIMGFile, WzUtils, mainunit;

var
  Counter: Integer;
  Animate: Boolean = False;

function GetWeaponNum(ID: string): string;
var
  AID: Integer;
begin
  AID := ID.ToInteger;
  Result := ((AID div 10000) - 100).toString;
end;

var
  PlayerEx: TPlayerEx;

class procedure TPlayerEx.Spawn(IDList: string);
var
  BelowFH: TFoothold;
  Below: TPoint;
begin

  PlayerEx := TPlayerEx.Create(SpriteEngine);
  PlayerEx.ImageLib := EquipImages;
  PlayerEx.OtherPlayer := True;
  PlayerEx.X := Player.X;
  PlayerEx.Y := Player.Y;
  Below := TFootholdTree.This.FindBelow(Point(Round(Player.X), Round(Player.Y) - 2), BelowFH);
  PlayerEx.FH := BelowFH;
  PlayerEx.JumpState := jsFalling;

  var Explode: TArray<string>;
  Explode := IDList.Split(['-']);

  var List := TList<string>.Create;
  for var i := 0 to High(Explode) - 1 do
    List.Add(Explode[i]);
  List.Sort;

  for var I := 0 to List.Count - 1 do
    PlayerEx.CreateEquip(List[i], SpriteEngine);
  List.Free;
end;

procedure TPlayerEx.DoMove(const Movecount: Single);
var
  Direction, AnimDelay: Integer;
  X1, Y1, X2, Y2, FallEdge: Integer;
  NewAction: string;
  Below: TPoint;
  BelowFH, WallFH: TFoothold;
  LadderPos: TPoint;
  LadderRope: TLadderRope;
  PX, PY, Delay: Integer;
begin

  X1 := FH.X1;
  Y1 := FH.Y1;
  X2 := FH.X2;
  Y2 := FH.Y2;
  if (JumpState = jsFalling) then
  begin
    Below := TFootholdTree.This.FindBelow(Point(Round(X), Round(Y - VelocityY - 2)), BelowFH);
    if Y >= Below.Y - 3 then
    begin
      Y := Below.Y;
      // MaxFallSpeed :=10;
      JumpState := jsNone;
      FH := BelowFH;
      Z := FH.Z * 100000 + 70000;
    end;
  end;

  case MoveDirection of
    mdLeft:
      begin

        Direction := GetAngle256(X1, Y1, X2, Y2);
        if (not FH.IsWall) then
        begin
          X := X + (Sin256(Direction) * MoveSpeed);
          Y := Y - (Cos256(Direction) * MoveSpeed);
        end;

        FallEdge := -999999;
        JumpEdge := -999999;
        if MoveType = mtMove then
        begin
          // no fh
          if FH.Prev = nil then
            FallEdge := FH.X1;
          // Wall's edge down
          if (FH.Prev <> nil) and (FH.Prev.IsWall) then
            FallEdge := FH.X1;

          if X < FallEdge then
          begin
            X := FallEdge;
            MirrorX := True;
            MoveDirection := mdRight;
          end;
        end;

        if MoveType = mtJump then
        begin
           // .--------.
          if FH.Prev = nil then
            JumpEdge := FH.X1;
          // ¢z--- <--
          if (FH.Prev <> nil) and (FH.Prev.IsWall) and (FH.Prev.Y1 > Y) then
            FallEdge := FH.X1;

          if X < FallEdge then
          begin
            if (Player.Y <= Y) then
              DoJump := True;
            if (Player.Y > Y) and (JumpState = jsNone) then
              JumpState := jsFalling;
          end;

          if X < JumpEdge then
            DoJump := True;
          // -->  ---¢{  <--
          WallFH := TFootholdTree.This.FindWallR(Point(Round(X + 4), Round(Y - 4)));
          if (WallFH <> nil) and (FH.Z = WallFH.Z) then
          begin
            if (X < WallFH.X1 + 30) and (Player.Y <= Y) then
              DoJump := True;
            if X <= WallFH.X1 then
            begin
              X := WallFH.X1 + MoveSpeed;
              if JumpState = jsNone then
              begin
                MirrorX := True;
                MoveDirection := mdRight;
              end;
            end;
          end;
        end;

        // walk left
        if (X <= FH.X1) and (FH.PrevID <> 0) and (not FH.IsWall) and (not FH.Prev.IsWall) then
        begin
          if (JumpState = jsNone) then
          begin
            FH := FH.Prev;
            X := FH.X2;
            Y := FH.Y2;
            Z := FH.Z * 100000 + 70000;
          end;
        end;

      end;

    mdRight:
      begin

        Direction := GetAngle256(X2, Y2, X1, Y1);
        if (not FH.IsWall) then
        begin
          X := X + (Sin256(Direction) * MoveSpeed);
          Y := Y - (Cos256(Direction) * MoveSpeed);
        end;

        FallEdge := 999999;
        JumpEdge := 999999;
        if MoveType = mtMove then
        begin
          if (FH.Next = nil) then
            FallEdge := FH.X2 + 5;
          // Wall down
          if (FH.Next <> nil) and (FH.Next.IsWall) { and (FH.Next.Y2 > Y) } then
            FallEdge := FH.X2;
          if (X > FallEdge) then
          begin
            X := FallEdge;
            MirrorX := False;
            MoveDirection := mdLeft;
          end;
        end;

        if MoveType = mtJump then
        begin
          if FH.Next = nil then // .--------.
            JumpEdge := FH.X2;
          // -->  ----¢{
          if (FH.Next <> nil) and (FH.Next.IsWall) and (FH.Next.Y2 > Y) then
            FallEdge := FH.X2;

          if X > FallEdge then
          begin
            if Player.Y <= Y then
              DoJump := True;
            if (Player.Y > Y) and (JumpState = jsNone) then
              JumpState := jsFalling;
          end;
          if X > JumpEdge then
            DoJump := True;
          // -->  ¢z.....
          WallFH := TFootholdTree.This.FindWallL(Point(Round(X - 4), Round(Y - 4)));
          if (WallFH <> nil) and (FH.Z = WallFH.Z) then
          begin
            if (X > WallFH.X1 - 30) and (Player.Y <= Y) then
              DoJump := True;
            if X >= WallFH.X1 then
            begin
              X := WallFH.X2 - MoveSpeed;
              if JumpState = jsNone then
              begin
                MirrorX := False;
                MoveDirection := mdLeft;
              end;
            end;

          end;
        end;

        // walk right
        if (X >= FH.X2) and (FH.NextID <> 0) and (not FH.IsWall) and (not FH.Next.IsWall) then
        begin
          if (JumpState = jsNone) then
          begin
            FH := FH.Next;
            X := FH.X1;
            Y := FH.Y1;
            Z := FH.Z * 100000 + 70000;
          end;
        end;

      end;

  end;
  if MoveDirection = mdNone then
    X := Round(X);

end;

procedure TPlayerEx.DoDraw;
var
  WX, WY, NamePos, IDPos: Integer;
begin
  inherited;
end;

function HasEntry(Entry: string): Boolean;
begin
  Result := EquipData.ContainsKey(Entry);
end;

var
  BlinkNum: Integer;

procedure TAvatarPartEx.DoMove(const Movecount: Single);
begin

  X := trunc(Owner.X);
  Y := trunc(Owner.Y);

  State := Owner.StandType;
  Animate:=true;

 UpdateFrame;

  if Alpha = 0 then
    Dead;
  if Visible = False then
    Dead;
end;

procedure TAvatarPartEx.DoDraw;
var
  WX, WY, NamePos: Integer;
begin
  inherited;

  if ChangeFrame then
    ChangeFrame := False;

  if Visible then
    Moved := True;

end;


end.

