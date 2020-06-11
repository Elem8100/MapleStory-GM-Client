unit Android;

interface

uses
  Windows, SysUtils, StrUtils, Generics.Collections, Math, PXT.Sprites, Footholds, LadderRopes,
  ChatBalloon, Classes, Global, Tools, MapleMap, MapleCharacterEx, NameTag;

type
  TAndroidPlayer = class(TPlayerEx)
  public
    WallFH: TFoothold;
    FallEdge: Integer;
    JumpEdge: Integer;
    BelowFH: TFoothold;
    Distance: TPoint;
    FollowDistance: Integer;
    procedure SpawnNew;
    procedure Spawn(IDList: string);
    procedure DoMove(const Movecount: Single); override;
  end;

  TAndroidNameTag = class(TMedalTag)
  public
    TagName: string;
    class var
      AndroidNameTag: TAndroidNameTag;
    class procedure ReDraw; override;
    procedure DoMove(const Movecount: Single); override;
    procedure DoDraw; override;
    class procedure Delete; override;
    class procedure Create(ItemID: string); overload; override;
  end;

var
  AndroidPlayer: TAndroidPlayer;

implementation

uses
  MapleChair, WZIMGFile, WzUtils, MapleCharacter, PXT.Canvas;

procedure TAndroidPlayer.SpawnNew;
var
  BelowFH: TFoothold;
  Below: TPoint;
begin
  Inc(TPlayer._NewZ);
  AndroidPlayer := TAndroidPlayer.Create(SpriteEngine);
  AndroidPlayer.ImageLib := EquipImages;
  AndroidPlayer.NewZ := TPlayer._NewZ;
  AndroidPlayer.OtherPlayer := True;
  AndroidPlayer.X := Player.X;
  AndroidPlayer.Y := Player.Y - 80;
  Below := TFootholdTree.This.FindBelow(Point(Round(Player.X), Round(Player.Y) - 2), BelowFH);
  AndroidPlayer.FH := BelowFH;
  AndroidPlayer.JumpSpeed := 0.6;
  AndroidPlayer.JumpHeight := 9.5;
  AndroidPlayer.MaxFallSpeed := 8;
  AndroidPlayer.JumpState := jsFalling;
  AndroidPlayer.MoveType := mtJump;
  AndroidPlayer.FollowDistance := 100;
  AndroidPlayer.MoveSpeed := 2.1;
end;

procedure TAndroidPlayer.Spawn(IDList: string);
begin
  RemoveSprites;
  ShowHair := False;
  DressCap := False;
  var Explode: TArray<string>;
  Explode := IDList.Split(['-']);

  var List := TList<string>.Create;
  for var i := 0 to High(Explode) - 1 do
    List.Add(Explode[i]);
  List.Sort;

  for var I := 0 to List.Count - 1 do
    CreateEquip(List[i], SpriteEngine);
  List.Free;

end;

procedure TAndroidPlayer.DoMove(const Movecount: Single);
var
  Direction: Integer;
  X1, Y1, X2, Y2: Integer;
  NewAction: string;
  Below: TPoint;
  LadderRope: TLadderRope;
begin
  UpdateJump;
  X1 := FH.X1;
  Y1 := FH.Y1;
  X2 := FH.X2;
  Y2 := FH.Y2;

  Distance.X := Round(Abs(Player.X - X));
  Distance.Y := Round(Abs(Player.Y - Y));

  if Distance.X > FollowDistance then
  begin
    Action := WalkType;
    if Player.X > X then
    begin
      MirrorX := True;
      MoveDirection := mdRight;
    end;
    if Player.X < X then
    begin
      MirrorX := False;
      MoveDirection := mdLeft;
    end;
  end
  else
  begin
    Action := StandType;
    MoveDirection := mdNone;
  end;

  if (Player.Y < Y) and (not Player.InLadder) then
  begin
    case Distance.Y of
      100..150:
        begin
          if (JumpState = jsNone) and (Player.JumpState = jsNone) then
          begin
            Below := TFootholdTree.This.FindBelow(Point(Round(X), Round(Y - 80)), BelowFH);
            if Y - Below.Y <> 0 then
              DoJump := True;

          end;
        end;

      151..2000:
        begin
          if Player.JumpState = jsNone then
          begin
            X := Player.X;
            Y := Player.Y;
            DoJump := True;
          end;
        end;
    end;
  end;

  if Player.Y > Y then
  begin
    case Distance.Y of
      200..2000:
        begin
          Y := Y + 5;
          JumpState := jsFalling
        end;
    end;
  end;

  if (JumpState = jsFalling) then
  begin
    Below := TFootholdTree.This.FindBelow(Point(Round(X), Round(Y - VelocityY - 2)), BelowFH);
    if Y >= Below.Y - 3 then
    begin
      Y := Below.Y;
      JumpState := jsNone;
      FH := BelowFH;
      Z := FH.Z * 100000 + 50000;
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
            Z := FH.Z * 100000 + 50000;
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
            Z := FH.Z * 100000 + 50000;
          end;
        end;

      end;

  end;

  if JumpState <> jsNone then
    Action := 'jump';

end;

class procedure TAndroidNameTag.Delete;
begin
  if (AndroidNameTag <> nil) then
    AndroidNameTag.Dead;
end;

procedure TAndroidNameTag.DoMove(const MoveCount: Single);
begin
  if IsReDraw then
    GameCanvas.DrawTarget(TargetTexture, 300, 100,
      procedure
      begin
        TargetEvent;
      end);
  x := AndroidPlayer.X;
  y := AndroidPlayer.Y;
  Z := AndroidPlayer.Z;
end;

class procedure TAndroidNameTag.ReDraw;
begin
  if AndroidNameTag <> nil then
    AndroidNameTag.IsReDraw := True;
end;

procedure TAndroidNameTag.DoDraw;
var
  WX, WY: Integer;
begin
  if TMap.ShowChar then
  begin
    WX := Round(AndroidPlayer.X) - Round(Engine.WorldX);
    WY := Round(AndroidPlayer.Y) - Round(Engine.WorldY);
    GameCanvas.Draw(TargetTexture, WX - 150, WY - 28);
  end;
  if IsReDraw then
    IsReDraw := False;
end;

class procedure TAndroidNameTag.Create(ItemID: string);
begin
  AndroidNameTag := TAndroidNameTag.Create(SpriteEngine);

  with AndroidNameTag do
  begin
    TruncMove := True;
    Tag := 1;
    var TagNum := GetImgEntry('Etc.wz/Android/' + ItemID + '/info').Get('nameTag', '38');
    Entry := GetImgEntry('UI.wz/NameTag.img/pet/' + string(TagNum));
    if Entry = nil then
      Entry := GetImgEntry('UI.wz/NameTag.img/pet/38');

    if Entry.Get('c/_inlink') <> nil then
    begin
      var Data := Entry.Get('c/_inlink').Data;
      Data := StringReplace(Data, '/c', '', [rfReplaceAll]);
      Data := StringReplace(Data, 'pet/', '', [rfReplaceAll]);
      Entry := GetImgEntry('UI.wz/NameTag.img/pet/' + string(Data));
    end;
    DumpData(Entry, EquipData, EquipImages);
    InitData;
  end;

end;

end.

