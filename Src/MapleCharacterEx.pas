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
var
  Path, AfterImagePath: string;
  BodyDelay, FaceDelay: Integer;
  FaceFrameCount: Integer;
  Flip: Integer;
  AdjX: Integer;
  Move: TPoint;
  Dir: string;
  Part: TPartName;
  SkillAction: string;
const
  C = 'Character.wz/';
begin

  X := Owner.X;
  Y := Owner.Y;

  State := Owner.StandType;


  // if (not Visible) and (GetPart(ID) <> 'Weapon') then
  // Moved := False;
//  if not Visible then
  //  Exit;

 // if (Image = 'head') and (FTime = 0) then
  //  ChangeFrame := True;

  // X := Foot.X-1;
  // Y := Foot.Y;

 // FState := State;

  // jump ->re stand
  if (Owner.JumpState = jsNone) and (State = 'jump') then
  begin
   // State := 'stand1';
  //  if WeaponWalkType.contains('stand2') then
   //   State := 'stand2'
  //  else
    State := Owner.Standtype;
  end;

  if (State = 'proneStab') then
    AnimRepeat := False
  else
    AnimRepeat := True;

  if AnimEnd then
  begin

    if State = 'proneStab' then
    begin
      FTime := 0;
      Frame := 0;
      State := 'prone';
    end;
  end;

  Inc(AlertCount);
  if (AlertCount > 300) and (State = 'alert') then
  begin
   // FTime := 0;
    Frame := 1;
    State := Owner.StandType;
    AlertCount := 0;
  end;

  if (State = 'stand1') or (State = 'stand2') or (State = 'alert') then
    AnimZigzag := True
  else
    AnimZigzag := False;

  Part := GetPart(ID);

  if (Part = Weapon) and (FTime = 0) then
  begin
    AfterImagePath := 'Character.wz/Afterimage/' + Owner.AfterImageStr + '.img/0/' + State + '/' +
      IntToStr(Frame) + '/0';
    if HasEntry(AfterImagePath) then
    begin
      PlaySounds('Weapon', 'swordL/Attack');

    end;
  end;


  // MirrorX := NewFlip;
  {
  if (not Owner.InLadder) and (not IsAttack) and (TSkill.PlayEnded) then
  begin
    if Keyboard.Key[DIK_LEFT] then
    begin
      MirrorX := False;
      CharFlip := False;
    end;
    if Keyboard.Key[DIK_RIGHT] then
    begin
      MirrorX := True;
      CharFlip := True;
    end;
  end;
   }
  if not Animate then
  begin
    if NEWFRAME <= FrameCount then
      Frame := NEWFRAME;
  end;

  if Owner.ResetAction then
  begin

    Frame := 1;
    State := Owner.NewAction;
    Inc(Counter);
  end;

  if not HasEntry(C + GetDir(ID) + ID + '.img/' + WpNum + State + '/' + IntToStr(Frame) + '/' +
    Image) and (not CharData.ContainsKey(State + '/' + IntToStr(Frame))) then
    Frame := 0;

  FrameCount := CharData['body/' + State + '/FrameCount'];
  BodyDelay := CharData['body/' + State + '/' + IntToStr(Frame) + '/delay'];

  FaceFrameCount := CharData['face/' + Expression + '/FrameCount'];
  FaceDelay := CharData['face/' + Expression + '/' + IntToStr(FaceFrame) + '/delay'];

  Dir := GetDir(ID);
  if (Image <> 'face') and (GetPart(ID) <> FaceAcc) then
  begin
    if Part = CashWeapon then
      WpNum := Owner.WeaponNum + '/'
    else
      WpNum := '';

    if CharData.ContainsKey(State + '/' + IntToStr(Frame)) then
      Path := C + Dir + ID + '.img/' + WpNum + CharData[State + '/' + IntToStr(Frame)] + '/' + Image
    else
      Path := C + Dir + ID + '.img/' + WpNum + State + '/' + IntToStr(Frame) + '/' + Image;
  end
  else
    Path := C + Dir + ID + '.img/' + Expression + '/' + IntToStr(FaceFrame) + '/' + Image;


  if (Image <> 'face') and (GetPart(ID) <> FaceAcc) then
  begin
    if (HasEntry(Path)) then
    begin
      ImageEntry := EquipData[Path];
      Alpha := 255;
    end
    else
      Alpha := 0;
  end
  else if (Image = 'face') or (Part = FaceAcc) then
  begin
    if (HasEntry(Path)) then
    begin
      ImageEntry := EquipData[Path];
      // Visible := True;
      Alpha := 255;
    end
    else
      Alpha := 0;
  end;

  if (Image = 'face') or (Part = Glass) or (Part = FaceAcc) then
  begin
    if (State = 'ladder') or (State = 'rope') or (MidStr(Path, 10, 9) = 'swingOF/1') or (MidStr(Path,
      10, 9) = 'swingTF/0') then
      Alpha := 0
    else
      Alpha := 255;
  end;

  if not Owner.DressCap then
  begin
    if Part = Cap then
      Self.Visible := False;

    if Owner.ShowHair then
   //   if (Image = 'hairOverHead') or (Image = 'backHair') then
     //   Visible := True;
      if Part = Hair then
        Self.Visible := True;
  end;

  if (Owner.DressCap) and (Owner.ShowHair) then
  begin

    if Part = Hair then
      case Owner.CapType of
        0:
          Self.Visible := True;
        1:
          begin
            if (Self.Image = 'hairOverHead') or (Self.Image = 'backHair') then
              Self.Visible := False;
          end;
        2:
          Self.Visible := False;
      end;
  end;



 // if Part = 'SitTamingMob' then
   // Visible := True;

  if (Image = 'ear') or (Image = 'lefEar') or (Image = 'highlefEar') then
    Alpha := 0;

  //Player.MirrorX := MirrorX;

  //if (TTamingMob.IsUse) or (TMapleChair.IsUse) then
 // begin
  //  if (Part = Weapon) or (Part = CashWeapon) then
  //    Visible := False;
 // end;

  if HasEntry(Path + '/z') then
    Z := 100 + Owner.Z - TAvatarParts.ZMap.IndexOf(EquipData[Path + '/z'].Data);

  if Owner.InLadder then
  begin

  end
  else
    Animate := True;

  if Animate then
    FTime := FTime + 17;
  if FTime > BodyDelay then
  begin

    if Counter > 50 then
    begin
      Owner.ResetAction := False;
      Counter := 0;
    end;
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

  if Expression <> 'blink' then
  begin
    Inc(FaceCount);
    if (FaceCount >= 550) then
    begin
      Expression := 'blink';
      FaceCount := 0;
    end;
  end;

  Inc(BlinkCount);
  if (BlinkCount >= 220) then
  begin
    DoFaceAnim := True;
    BlinkCount := 0;
  end;

  case Random(1500) of
    100:
      BlinkNum := 1;
    500:
      BlinkNum := 2;
  end;

  if DoFaceAnim then
    FaceTime := FaceTime + 17;

  if FaceTime > FaceDelay then
  begin
    Inc(FaceFrame);
    if FaceFrame > FaceFrameCount then
    begin
      FaceFrame := 0;
      Inc(BlinkTime);
      if BlinkTime >= BlinkNum then
      begin
        DoFaceAnim := False;
        BlinkTime := 0;
      end;
    end;
    FaceTime := 0;
  end;

  if CharData.ContainsKey(State + '/' + Frame.ToString) then
  begin
    SkillAction := CharData[State + '/' + Frame.ToString];
    if (SkillAction = 'hide/0') or (SkillAction = 'blink/0') then
      Alpha := 0;
    if (Image = 'face') or (Part = Glass) or (Part = FaceAcc) then
    begin
      if (SkillAction = 'swingOF/1') or (SkillAction = 'swingTF/0') then
        Alpha := 0
    end;
  end;

  with Owner do
  begin

    if HasEntry(Path + '/origin') then
    begin
      case MirrorX of
        True:
          begin
            Self.Flip := -1;
            if Owner.InLadder then
              AdjX := 3
            else
              AdjX := 0;
            Origin.X := EquipData[Path + '/origin'].Vector.X - Self.ImageWidth + AdjX;
          end;
        False:
          begin
            Self.Flip := 1;
            Origin.X := -EquipData[Path + '/origin'].Vector.X;
          end;
      end;
      Origin.Y := -EquipData[Path + '/origin'].Vector.Y;
    end;

    if HasEntry(Path + '/map/brow') then
    begin
      Brow.X := -EquipData[Path + '/map/brow'].Vector.X * Self.Flip;
      Brow.Y := -EquipData[Path + '/map/brow'].Vector.Y;
      if Image = 'head' then
        HeadBrow := Brow;

      Self.Offset.X := Origin.X + HeadNeck.X - BodyNeck.X - HeadBrow.X + Brow.X {- TTamingMob.Navel.X};
      Self.Offset.Y := Origin.Y + HeadNeck.Y - BodyNeck.Y - HeadBrow.Y + Brow.Y {- TTamingMob.Navel.Y};
      BrowPos := Self.Offset;
    end;

    if HasEntry(Path + '/map/neck') then
    begin
      Neck.X := -EquipData[Path + '/map/neck'].Vector.X * Self.Flip;
      Neck.Y := -EquipData[Path + '/map/neck'].Vector.Y;
      if Image = 'body' then
        BodyNeck := Neck;
      if Image = 'head' then
        HeadNeck := Neck;
    end;

    if HasEntry(Path + '/map/hand') then
    begin
      Hand.X := -EquipData[Path + '/map/hand'].Vector.X * Self.Flip;
      Hand.Y := -EquipData[Path + '/map/hand'].Vector.Y;
      if Image = 'arm' then
        ArmHand := Hand;
      if Image = 'body' then
        BodyHand := Hand;
      Self.Offset.X := Origin.X + Hand.X + ArmNavel.X - ArmHand.X - BodyNavel.X;
      Self.Offset.Y := Origin.Y + Hand.Y + ArmNavel.Y - ArmHand.Y - BodyNavel.Y;
    end;

    if HasEntry(Path + '/map/handMove') then
    begin
      HandMove.X := -EquipData[Path + '/map/handMove'].Vector.X * Self.Flip;
      HandMove.Y := -EquipData[Path + '/map/handMove'].Vector.Y;
      if Image = 'lHand' then
        lHandMove := HandMove;

      Self.Offset.X := Origin.X + HandMove.X - lHandMove.X;
      Self.Offset.Y := Origin.Y + HandMove.Y - lHandMove.Y;
    end;

    if HasEntry(Path + '/map/navel') then
    begin
      Navel.X := -EquipData[Path + '/map/navel'].Vector.X * Self.Flip;
      Navel.Y := -EquipData[Path + '/map/navel'].Vector.Y;
      if Image = 'arm' then
        ArmNavel := Navel;
      if Image = 'body' then
        BodyNavel := Navel;

      Self.Offset.X := Origin.X + Navel.X - BodyNavel.X {- TTamingMob.Navel.X};
      Self.Offset.Y := Origin.Y + Navel.Y - BodyNavel.Y {- TTamingMob.Navel.Y};
    end;

  end;

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
  if GameMode = gmView then
    Exit;
  if ChangeFrame then
    ChangeFrame := False;
//  if TMap.ShowChar then
   // inherited;

  if Visible then
    Moved := True;

end;

initialization
 // WeaponWalkType := TList<string>.Create;
 // PlayerEqpList := TList<string>.Create;
 // TPlayer.EquipLoadedList := TList<string>.Create;
    //  TAvatarPart.ZMap := TList<string>.Create;



finalization
 // WeaponWalkType.Free;
 // PlayerEqpList.Free;
 // TPlayer.EquipLoadedList.Free;

end.

