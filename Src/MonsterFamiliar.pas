unit MonsterFamiliar;

interface

uses
  Windows, System.Types, SysUtils, StrUtils, PXT.Sprites, Generics.Collections, WZIMGFile, Math,
  Footholds, LadderRopes,  WZArchive, ChatBalloon, MapPortal,
  MapleCharacter, DamageNumber, MobDrop, Global, Tools, WzUtils, MapleMap, NameTag;

type
  TMoveDirection = (mdLeft, mdRight, mdNone);

  tdir = (dLeft, dRight, no);

  TMoveType = (mtStand, mtMove, mtJump, mtFly);

  TMonsterFamiliar = class(TJumperSprite)
  private
    FTime: Integer;
    WallFH: TFoothold;
    BelowFH: TFoothold;
    MoveDirection: TMoveDirection;
    MoveSpeed: Single;
    FPetName: string;
    FNameWidth: Integer;
    FIDWidth: Integer;
    MoveType: TMoveType;
    FallEdge: Integer;
    JumpEdge: Integer;
    Origin: TPoint;
    Path: string;
    UpPath: string;
    State: string;
    Frame: Integer;
    Delay: Integer;
    Distance: TPoint;
    OnLadder: Boolean;
    FollowDistance: Integer;
  public
    FH: TFoothold;
    class var
      MonsterFamiliar: TMonsterFamiliar;
    procedure DoMove(const Movecount: Single); override;
    class procedure Delete; virtual;
    class procedure Create(ID: string); overload;
  end;

  TFamiliarNameTag = class(TLabelRingTag)
  public
    TagName: string;
    class var
      FamiliarNameTag: TFamiliarNameTag;
    class procedure ReDraw; override;
    procedure DoMove(const Movecount: Single); override;
    procedure DoDraw; override;
    class procedure Delete; override;
    class procedure Create(ItemID: string); overload; override;
  end;

implementation
      uses PXT.Canvas,PXT.TypesEx;
class procedure TMonsterFamiliar.Delete;
begin
  if MonsterFamiliar <> nil then
  begin
    MonsterFamiliar.Dead;
    MonsterFamiliar := nil;
    for var Iter in EquipImages.Keys do
      if (LeftStr(Iter.GetPath, 3) = 'Mob') then
      begin
        EquipImages.Remove(Iter);
      //  EquipData.Remove(Iter.GetPath);
      end;
  end;
end;

class procedure TMonsterFamiliar.Create(ID: string);
begin
  var Entry: TWZIMGEntry;
  if HasImgFile('Mob/' + ID + '.img') then
    Entry := GetImgEntry('Mob/' + ID + '.img/');


  DumpData(Entry, EquipData, EquipImages);

  for var Iter in EquipData[Entry.GetPath].Children do
    for var Iter2 in EquipData[Iter.GetPath].Children do
      if IsNumber(Iter2.Name) then
      begin
        if ((Iter.Name = 'stand') or (Iter.Name = 'fly')) and (Iter2.Name = '0') then
        begin
          MonsterFamiliar := TMonsterFamiliar.Create(SpriteEngine);
          with MonsterFamiliar do
          begin
            ImageLib := EquipImages;
            TruncMove := True;
            Tag := 1;
            State := Iter.Name;
            Frame := Iter2.Name.ToInteger;
            UpPath := Entry.GetPath;
            ImageEntry := EquipData[Iter2.GetPath];
            FollowDistance := 130;
            var StartX := Player.X - FollowDistance;
            if StartX < TMap.Left then
              StartX := Player.X;
            var Pos := TFootholdTree.This.FindBelow(Point(Round(StartX), Round(Player.Y - 100)), BelowFH);
            MoveType := mtJump;
            X := Pos.x;
            Y := Pos.Y;
            FH := BelowFH;
            Z := FH.Z * 100000 + 50000;
            JumpSpeed := 0.6;
            JumpHeight := 9;
            MaxFallSpeed := 8;
            MoveDirection := mdNone;
            MoveSpeed := 2.5;
          end;
        end;
      end;

end;

procedure TMonsterFamiliar.DoMove(const Movecount: Single);
var
  Direction: Integer;
  X1, Y1, X2, Y2: Integer;
  NewAction: string;
  Below: TPoint;
  LadderRope: TLadderRope;
begin
  inherited;
  X1 := FH.X1;
  Y1 := FH.Y1;
  X2 := FH.X2;
  Y2 := FH.Y2;

  if HasEntryE(UpPath + '/' + State + '/' + Frame.ToString) then
  begin
    Path := UpPath + '/' + State + '/' + Frame.ToString;
    ImageEntry := EquipData[Path];
  end;

  if HasEntryE(UpPath + '/' + State + '/' + Frame.ToString + '/delay') then
    Delay := EquipData[UpPath + '/' + State + '/' + Frame.ToString + '/delay'].Data
  else
    Delay := 100;

  FTime := FTime + 17;
  if FTime > Delay then
  begin
    Frame := Frame + 1;
    if not HasEntryE(UpPath + '/' + State + '/' + Frame.ToString) then
      Frame := 0;
    FTime := 0;
  end;

  Distance.X := Round(Abs(Player.X - X));
  Distance.Y := Round(Abs(Player.Y - Y));

  if Distance.X > FollowDistance then
  begin
    State := 'move';
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
    State := 'stand';
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
  if MoveDirection = mdNone then
    X := Round(X);

  if ImageEntry.Get('origin') <> nil then
    Origin := ImageEntry.Get('origin').Vector;

  case MirrorX of
    True:
      Offset.X := Origin.X - PatternWidth;
    False:
      Offset.X := -Origin.X;
  end;
  Offset.Y := -Origin.Y;

end;

class procedure TFamiliarNameTag.Delete;
begin
  if (FamiliarNameTag <> nil) then
    FamiliarNameTag.Dead;
end;

procedure TFamiliarNameTag.DoMove(const MoveCount: Single);
begin
  if IsReDraw then
    GameCanvas.DrawTarget(TargetTexture,300,100,procedure
    begin
     TargetEvent;
    end);
  X := TMonsterFamiliar.MonsterFamiliar.X;
  y := TMonsterFamiliar.MonsterFamiliar.Y;
  Z := TMonsterFamiliar.MonsterFamiliar.Z;
end;

class procedure TFamiliarNameTag.ReDraw;
begin
  if FamiliarNameTag <> nil then
    FamiliarNameTag.IsReDraw := True;
end;

procedure TFamiliarNameTag.DoDraw;
var
  WX, WY: Integer;
begin
  if TMap.ShowChar then
  begin
    WX := Round(TMonsterFamiliar.MonsterFamiliar.X) - Round(Engine.WorldX);
    WY := Round(TMonsterFamiliar.MonsterFamiliar.Y) - Round(Engine.WorldY);
    GameCanvas.Draw(TargetTexture, WX - 150, WY - 28);
  end;
  if IsReDraw then
    IsReDraw := False;
end;

class procedure TFamiliarNameTag.Create(ItemID: string);
begin
  FamiliarNameTag := TFamiliarNameTag.Create(SpriteEngine);

  with FamiliarNameTag do
  begin
    TruncMove := True;
    Tag := 1;
    var TagNum := GetImgEntry('Character/Ring/' + ItemID + '.img/info').Get('nameTag', '');
    Entry := GetImgEntry('UI/NameTag.img/' + string(TagNum));
    DumpData(Entry, EquipData, EquipImages);
    InitData;
  end;

end;

end.

