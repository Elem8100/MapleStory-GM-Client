unit Mob2;

interface

uses
  Windows, System.types, SysUtils, StrUtils, AsphyreSprite, Generics.Collections,
  WZIMGFile, Math, Footholds, LadderRopes, AsphyreTypes, DX9Textures, WZArchive,
  ChatBalloon, MapPortal, MapleCharacter, DamageNumber, MobDrop, Global, Tools,WzUtils, MapleMap;

type

  TMoveDirection = (mdLeft, mdRight, mdNone);
  tdir = (dLeft, dRight, no);
  TMoveType = (mtStand, mtMove, mtJump, mtFly);

  TMob = class(TJumperSprite)
  private
    FFrame: Integer;
    FTime: Integer;
    FID: string;
    InfoID: string;
    Action: string;
    FH: TFoothold;
    WallFH: TFoothold;
    BelowFH: TFoothold;
    RX0: Integer;
    RX1: Integer;
    MoveDirection: TMoveDirection;
    MoveSpeed: Single;
    FlySpeed: Single;
    FMobName: string;
    Level: Integer;
    FNameWidth: Integer;
    FIDWidth: Integer;
    FrameCount: Integer;
    NoFlip: Boolean;
    MoveType: TMoveType;
    FallEdge: Integer;
    JumpEdge: Integer;
    CosY, SrcY: Integer;
    FGetHit1: Boolean;
    FHit: Boolean;
    AnimEnd: Boolean;
    Value: Integer;
    AnimRepeat: Boolean;
    AnimZigzag: Boolean;
    LT, RB: TPoint;
    Left, Top, Right, Bottom: Integer;
    FHead: TPoint;
    Origin: TPoint;
    FHP: Integer;
    FDie: Boolean;
    TargetIndex: Integer;
    FPathW: string;
  public
    Head: TPoint;
    property GetHit1: Boolean read FGetHit1 write FGetHit1;
    property Hit: Boolean read FHit write FHit;
    property Frame: Integer read FFrame write FFrame;
    property SelfID: string read FID write FID;
    // property Head: TPoint read FHead write FHead;
    property HP: Integer read FHP write FHP;
    property Die: Boolean read FDie write FDie;
    destructor Destroy; override;
    procedure DoMove(const Movecount: Single); override;
    procedure DoDraw; override;
    procedure TargetEvent(Sender: TObject);
    class procedure Drop(ID: string; PosX, PosY: Integer; aRX0: Integer = 0; aRX1: Integer = 0);
    class procedure CreateMapMobs;
    class var MobList: TList<string>;
  end;

implementation

function IDToInt(ID: string): string;
var
  s: Integer;
begin
  s := StrToInt(ID);
  Result := IntToStr(s);
end;

class procedure TMob.Drop(ID: string; PosX, PosY: Integer; aRX0: Integer = 0; aRX1: Integer = 0);
var
  c: Integer;
  Entry, Iter, Iter2: TWZIMGEntry;
  Pos: TPoint;
  Path1, PathW: string;
  WZ: TWZArchive;
  TestID: string;
begin
  Randomize;

  if MobWz.GetImgFile(ID + '.img') <> nil then
  begin
    Path1 := 'Mob/';
    PathW := 'Mob.wz/';
    WZ := MobWz;
  end
  else
  begin
    Path1 := 'Mob2/';
    PathW := 'Mob2.wz/';
    WZ := Mob2Wz;
  end;

  Entry := GetImgEntry(Path1 + ID + '.img/info/link');
  if not MobList.Contains(ID) then
  begin
    MobList.Add(ID);
    DumpData(WZ.GetImgFile(ID + '.img').Root, WzData, Images);
    if Entry <> nil then
      DumpData(WZ.GetImgFile(Entry.Data + '.img').Root, WzData, Images);
  end;

  if Entry <> nil then
    TestID := Entry.Data
  else
    TestID := ID;
  if (WZ.GetImgFile(TestID + '.img').Root.Get('stand/0') = nil) and (WZ.GetImgFile(TestID + '.img').Root.Get('fly/0') = nil) then
    Exit;

  with TMob.Create(SpriteEngine) do
  begin
    FPathW := PathW;
    InfoID := ID;
    if Entry <> nil then
      FID := Entry.Data
    else
      FID := ID;

    Value := 1;

    for Iter in WZ.GetImgFile(FID + '.img').Root.Children do
    begin
      c := 0;
      for Iter2 in Iter.Children do
        if CharInSet(Iter2.Name[1], ['0' .. '9']) then
          Inc(c);
      Data.AddOrSetValue(FID + Iter.Name + '/FrameCount', c - 1);
    end;

    Entry := GetImgEntry(Path1 + InfoID + '.img/info');
    if Entry.Get('speed') <> nil then
      MoveSpeed := (1 + Entry.Get('speed').Data / 100) * 2
    else
      MoveSpeed := 2;

    if Entry.Get('flySpeed') <> nil then
      FlySpeed := (1 + Entry.Get('flySpeed').Data / 100) * 2
    else
      FlySpeed := 2;

    Level := Entry.Get('level', '1');
    FMobName := StringWZ.GetImgFile('Mob.img').Root.Get(IDToInt(InfoID) + '/' + 'name', '');
    // WzData.AddOrSetValue(FID, FMobName);

    FNameWidth := FontsAlt[1].TextWidth('Lv.' + IntToStr(Level) + '  ' + FMobName);
    FIDWidth := FontsAlt[1].TextWidth('ID: ' + InfoID);

    MoveType := mtMove;
    Action := 'stand';

    Entry := WZ.GetImgFile(FID + '.img').Root;

    if Entry.Child['move'] = nil then
    begin
      MoveSpeed := 0;
      MoveType := mtStand;
    end;

    if Entry.Child['jump'] <> nil then
      MoveType := mtJump;

    if Entry.Child['fly'] <> nil then
    begin
      MoveType := mtFly;
      Action := 'fly';
      CosY := Random(256);
    end;

    ImageLib := Images;
    ImageEntry := WzData[FPathW + FID + '.img/' + Action + '/0'];

    Pos := TFootholdTree.This.FindBelow(Point(Round(PosX), Round(PosY - 3)), BelowFH);
    // X := PosX;
    // Y := PosY;
    X := Pos.X;
    Y := Pos.Y;
    SrcY := Trunc(Y);
    FH := BelowFH;
    Z := FH.Z * 100000+6000;
    JumpSpeed := 0.6;
    JumpHeight := 9;
    MaxFallSpeed := 8;
    RX0 := aRX0;
    RX1 := aRX1;
    Width := PatternWidth;
    Height := PatternHeight;
    MirrorX := Boolean(Random(2));
    MoveDirection := mdNone;
    Collisioned := True;
    AnimRepeat := True;
    HP := 2000000;
    TruncMove := True;
    DoAnimate := False;
    if Entry.Get('info/noFlip', '0') then
      MirrorX := False;
    if ImageEntry.Get('origin') <> nil then
      Origin := ImageEntry.Get('origin').Vector;
    case MirrorX of
      True:
        begin
          Offset.X := Origin.X - PatternWidth;
          // MoveDirection := mdRight;
        end;
      False:
        begin
          Offset.X := -Origin.X;
          // MoveDirection := mdLeft;
        end;
    end;
    Offset.Y := -Origin.Y;

    TargetIndex := GameTargets.Add(1, FNameWidth + 4, 20, apf_A8R8G8B8, True, True);
    GameDevice.RenderTo(TargetEvent, 0, True, GameTargets[TargetIndex]);

  end;

end;

var
  DropList: TList<string>;

class procedure TMob.CreateMapMobs;
var
  Iter, Iter2: TWZIMGEntry;
  I: Integer;
begin

  // addcoin;
  DropList.AddRange(['09000000', '09000001', '09000002', '09000003']);
  for I := 2000000 to 2000021 do
    DropList.Add('0' + IntToStr(I));
  // Randomize;
  for Iter in TMap.ImgFile.Child['life'].Children do
  begin
    if Iter.Child['type'] <> nil then
    begin
      if Iter.Get('type', '') = 'm' then
        TMob.Drop(Iter.Get('id', ''), Iter.Get('x', ''), Iter.Get('cy', ''), Iter.Get('rx0', ''), Iter.Get('rx1', ''));
    end
    else
    begin
      for Iter2 in Iter.Children do
        if Iter2.Get('type', '') = 'm' then
          TMob.Drop(Iter2.Get('id', ''), Iter2.Get('x', ''), Iter2.Get('cy', ''), Iter2.Get('rx0', ''), Iter2.Get('rx1', ''));
    end;
  end;

end;

procedure TMob.DoMove(const Movecount: Single);
var
  Direction, AnimDelay: Integer;
  X1, Y1, X2, Y2: Integer;
  NewAction: string;
  Below: TPoint;
  VarF: TFoothold;
  a1: Integer;
  AniAlpha: Single;
  _Head: TPoint;
begin

  inherited;

  X1 := FH.X1;
  Y1 := FH.Y1;
  X2 := FH.X2;
  Y2 := FH.Y2;
  if Data.ContainsKey(FID + Action + '/FrameCount') then
    FrameCount := Data[FID + Action + '/FrameCount'];

  ImageEntry := WzData[FPathW + FID + '.img/' + Action + '/' + IntToStr(Frame)];
  AnimDelay := ImageEntry.Get('delay', '100');
  a1 := ImageEntry.Get('a1', '255');

  if ImageEntry.Child['lt'] <> nil then
  begin
    LT := ImageEntry.Child['lt'].Vector;
    RB := ImageEntry.Child['rb'].Vector;
    case MirrorX of
      True:
        begin
          Right := Round(X) - LT.X;
          Left := Round(X) - RB.X;
        end;
      False:
        begin
          Left := Round(X) + LT.X;
          Right := Round(X) + RB.X;
        end;
    end;
    Top := Round(Y) + LT.Y;
    Bottom := Round(Y) + RB.Y;
  end;
  CollideRect := Rect(Left, Top, Right, Bottom);

  if ImageEntry.Child['head'] <> nil then
  begin
    _Head := ImageEntry.Child['head'].Vector;
    if (MirrorX <> CharFlip) or (not GetHit1) then
    begin
      case MirrorX of
        True:
          Head.X := Round(X) - _Head.X - 20;
        False:
          Head.X := Round(X) + _Head.X - 20;
      end;
    end
    else
    begin
      case MirrorX of
        False:
          Head.X := Round(X) + _Head.X - 20;
        True:
          Head.X := Round(X) - _Head.X - 20;
      end;
    end;
    Head.Y := Round(Y) + _Head.Y;
  end;

  if (Action = 'hit1') or (Action = 'die1') then
    AnimRepeat := False
  else
    AnimRepeat := True;

  if WzData.ContainsKey(FPathW + FID + '.img/' + Action + '/zigzag') then
    AnimZigzag := True
  else
    AnimZigzag := False;

  FTime := FTime + 17;

  if FTime > AnimDelay then
  begin
    FTime := 0;
    if AnimZigzag then
    begin
      Frame := Frame + Value;
      if (Frame >= FrameCount) or (Frame <= 0) then
        Value := -Value;
    end
    else
    begin

      Frame := Frame + 1;
      AnimEnd := False;
      if Frame > FrameCount then
      begin
        if AnimRepeat then
          Frame := 0
        else
        begin
          Frame := Frame - 1;
          AnimEnd := True;
        end;
      end;
    end;

  end;

  if (not Die) then

    if (MoveType <> mtStand) and (MoveType <> mtFly) then
      case Random(200) of

        50:
          begin
            MirrorX := False;
            MoveDirection := mdLeft;
          end;

        100:
          begin
            MirrorX := True;
            MoveDirection := mdRight;
          end;

        150:
          begin
            MoveDirection := mdNone;
          end;

        199:
          begin
            if not GetHit1 then
              if (MoveType = mtJump) then
              begin
                DoJump := True;
              end;
          end;

      end;

  if (not GetHit1) and (not Die) then

    if JumpState <> jsNone then
    begin
      NewAction := 'jump';
      Frame := 0;
    end
    else
    begin
      case MoveDirection of
        mdLeft, mdRight:
          NewAction := 'move';
        mdNone:
          NewAction := 'stand';
      end;
    end;

  if (AnimEnd) and (Action = 'hit1') then
  begin
    FTime := 0;
    Frame := 0;
    GetHit1 := False;
  end;

  if (Hit) then
  begin
    if (GetHit1) then
    begin
      if (Action = 'hit1') or (Action = 'die1') then
      begin
        if Frame = 0 then
          TDamageNumber.Create(Damage, Head.X, Head.Y);
        Hit := False;
      end;
    end;
    // no push
    if (not GetHit1) then
    begin
      TDamageNumber.Create(Damage, Head.X, Head.Y);
      Hit := False;
    end;

  end;

  if (GetHit1) and (not Die) then
  begin

    if Action <> 'hit1' then
    begin
      AnimEnd := False;
      Frame := 0;
      FTime := 0;
      NewAction := 'hit1';
    end;

    // -->player   -->mob
    if (MirrorX = CharFlip) and (FTime = 0) then
    begin
      MirrorX := not MirrorX;
      case MoveDirection of
        mdLeft:
          MoveDirection := mdRight;
        mdRight:
          MoveDirection := mdLeft;
        mdNone:
          begin
            if MirrorX then
              MoveDirection := mdRight
            else
              MoveDirection := mdLeft;

          end;
      end;
    end;
    // player-->  <--Mob
    if (MirrorX <> CharFlip) and (FTime = 0) then
    begin
      if MoveDirection = mdNone then
        if MirrorX then
          MoveDirection := mdRight
        else
          MoveDirection := mdLeft;
    end;

  end;

  if (Die) then
  begin
    if Action <> 'die1' then
    begin
      AnimEnd := False;
      Frame := 0;
      if WzData[FPathW + FID + '.img/die1'].Get('0') = nil then
        Frame := 1;
      FTime := 0;
      NewAction := 'die1';

    end;

  end;

  if a1 <> -1 then
  begin
    AniAlpha := 255 - (255 - a1) * FTime / AnimDelay;
    if (AniAlpha < 255) and (AniAlpha > 0) then
      Alpha := Trunc(AniAlpha);
    // if Alpha <=10 then Dead;
  end;

  if (AnimEnd) and (Action = 'die1') then
  begin
    TMobDrop.Drop(Trunc(X), Trunc(Y), Random(6), DropList);
    Dead;
  end;

  if WzData.ContainsKey(FPathW + FID + '.img/' + NewAction + '/' + IntToStr(Frame)) then
    Action := NewAction
  else if not GetHit1 then
    Frame := 0;

  if (JumpState = jsFalling) then
  begin
    Below := TFootholdTree.This.FindBelow(Point(Round(X), Round(Y - VelocityY - 2)), BelowFH);
    if Y >= Below.Y - 3 then
    begin
      Y := Below.Y;
      // MaxFallSpeed :=10;
      JumpState := jsNone;
      FH := BelowFH;
      Z := FH.Z * 100000 + 6000;
    end;
  end;
  // if (FH.Prev=0) and (FH.Next=0) then Exit;

  case MoveDirection of
    mdLeft:
      begin

        Direction := GetAngle256(X1, Y1, X2, Y2);
        if (not FH.IsWall) and (not GetHit1) and (not Die) then
        begin
          X := X + (Sin256(Direction) * MoveSpeed);
          Y := Y - (Cos256(Direction) * MoveSpeed);
        end;

        if (GetHit1) and (not Die) and (FTime < 300) then
        begin
          X := X + (Sin256(Direction) * -1.3);
          Y := Y - (Cos256(Direction) * -1.3);
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
          if X <= RX0 then
          begin
            X := RX0;
            MirrorX := True;
            MoveDirection := mdRight;
          end;
          // .--------.
          if FH.Prev = nil then
            JumpEdge := FH.X1;
          // ¢z--- <--
          if (FH.Prev <> nil) and (FH.Prev.IsWall) and (FH.Prev.Y1 > Y) then
            FallEdge := FH.X1;

          if X < FallEdge then
            JumpState := jsFalling;
          if X < JumpEdge then
            DoJump := True;
          // -->  ---¢{  <--
          WallFH := TFootholdTree.This.FindWallR(Point(Round(X + 4), Round(Y - 4)));
          if (WallFH <> nil) and (FH.Z = WallFH.Z) then
          begin
            if X < WallFH.X1 + 30 then
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
            Z := FH.Z * 100000 + 6000;
          end;
        end;

      end;

    mdRight:
      begin

        Direction := GetAngle256(X2, Y2, X1, Y1);
        if (not FH.IsWall) and (not GetHit1) and (not Die) then
        begin
          X := X + (Sin256(Direction) * MoveSpeed);
          Y := Y - (Cos256(Direction) * MoveSpeed);
        end;

        if (GetHit1) and (not Die) and (FTime < 300) then
        begin
          X := X + (Sin256(Direction) * -1.3);
          Y := Y - (Cos256(Direction) * -1.3);
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
          if X >= RX1 then
          begin
            X := RX1;
            MirrorX := False;
            MoveDirection := mdLeft;
          end;
          if FH.Next = nil then // .--------.
            JumpEdge := FH.X2;
          // -->  ----¢{
          if (FH.Next <> nil) and (FH.Next.IsWall) and (FH.Next.Y2 > Y) then
            FallEdge := FH.X2;

          if X > FallEdge then
            JumpState := jsFalling;
          if X > JumpEdge then
            DoJump := True;
          // -->  ¢z.....
          WallFH := TFootholdTree.This.FindWallL(Point(Round(X - 4), Round(Y - 4)));
          if (WallFH <> nil) and (FH.Z = WallFH.Z) then
          begin
            if X > WallFH.X1 - 30 then
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
            Z := FH.Z * 100000 + 6000;
          end;
        end;

      end;

  end;

  if MoveType = mtFly then
  begin
    case Random(250) of
      50:
        MirrorX := True;
      200:
        MirrorX := False;
    end;
    if X <= RX0 then
      MirrorX := True;
    if X >= RX1 then
      MirrorX := False;

    case MirrorX of
      True:
        X := X + 1.5 * FlySpeed;
      False:
        X := X - 1.5 * FlySpeed;
    end;
    Inc(CosY, 7);
    Y := SrcY - Trunc(Cos256(CosY) * 16);
  end;

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

destructor TMob.Destroy;
begin

  inherited;
end;

procedure TMob.DoDraw;
var
  WX, WY, NamePos, IDPos: Integer;
begin
  if TMap.ShowMob then
    inherited;

  WX := Round(X - Engine.WorldX);
  WY := Round(Y - Engine.WorldY);
  NamePos := WX - FNameWidth div 2;
  IDPos := WX - FIDWidth div 2;

  if TMap.ShowMobName then
  begin
    // if gametargets[TargetIndex]<> nil then
    GameCanvas.Draw(GameTargets[TargetIndex], NamePos - 3, WY + 2, 1, False, 255, 255, 255, 255);
  end;

  if TMap.ShowID then
  begin
    GameCanvas.FillRect(IDPos - 3, WY + 18, FIDWidth + 4, 15, cRGB1(0, 0, 0, 160));

    FontsAlt[1].TextOut('ID: ' + InfoID, IDPos - 1, WY + 18, cRGB1(255, 255, 255));
  end;
  {
    if WzData.ContainsKey(ImageName + '/lt.x') then
    begin
    case MirrorX of
    True:
    begin
    Left  := X - WzData[ImageName + '/lt.x'];
    Right := X - WzData[ImageName + '/rb.x'];
    end;
    False:
    begin
    Left  := X + WzData[ImageName + '/lt.x'];
    Right := X + WzData[ImageName + '/rb.x'];
    end;
    end;
    Top := Y + WzData[ImageName + '/lt.y'];
    Bottom := Y + WzData[ImageName + '/rb.y'];
    end;
    MainForm.MyCanvas.Rectangle(Rect(left-xx,top-yy,right-xx,bottom-yy),argb(255,0,255,0),0,fxblend);
  }
end;

procedure TMob.TargetEvent(Sender: TObject);
begin
  GameCanvas.FillRect(0, 0, FNameWidth + 4, 15, cRGB1(0, 0, 0, 190));
  GameCanvas.Flush;
  FontsAlt[1].TextOut('Lv.' + IntToStr(Level) + '  ' + FMobName, 2, 0, clWhite1);
end;

initialization

TMob.MobList := TList<string>.Create;
DropList := TList<string>.Create;

finalization
 TMob.MobList.Free;
 DropList.Free;


end.
