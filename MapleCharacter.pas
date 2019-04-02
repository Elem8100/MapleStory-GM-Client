unit MapleCharacter;

interface

uses
  Windows, SysUtils, StrUtils, Generics.Collections, System.Types, WZIMGFile,
  Math, AsphyreSprite, Footholds, LadderRopes, ChatBalloon, MapPortal,
  DirectInput, Classes, AsphyreKeyboard, AsphyreRenderTargets, DamageNumber,
  Skill, AsphyreTypes, AbstractTextures, Global, Tools, MapleMap, WzUtils;

type
  TDir = (dLeft, dRight, no);

  TLadderType = (rtLadder, rtRope);

  TPartName = (Head, Body, Cap, Face, Hair, Glove, FaceAcc, Glass, EarRing, Cape, Coat, Longcoat, Pants, Shield, Shoes, Weapon, CashWeapon, Chairs, SitTamingMob, WalkTamingMob, TamingMob);

  TAvatarParts = class;

  TPlayer = class(TJumperSprite)
    AvatarEngine: TSpriteEngine;
    FH: TFoothold;
    PartList: TList<string>;
    InLadder: Boolean;
    OnLadder: Boolean;
    FallFlag: Boolean;
    FallCounter, OffY: Integer;
    OnPortal: Boolean;
    Dir: TDir;
    SpeedL, SpeedR: Single;
    CurrentX: Single;
    Action: string;
    PlayerName: string;
    NameWidth: Integer;
    FAttack: Boolean;
    SkillDone: Boolean;
    sTime: Integer;
    NameTagTargetIndex: Integer;
    CurrentPortal: TPortalInfo;
    Portal: TPortalInfo;
    LadderType: TLadderType;
    PartOwner: TPlayer;
    class var
      AvatarTargetIndex: Integer;
      AvatarPanelIndex: Integer;
    procedure LoadEquip(EquipID: string);
    procedure TargetEvent(Sender: TObject);
    procedure DoMove(const Movecount: Single); override;
    procedure DoDraw; override;
    constructor Create(const AParent: TSprite); override;
    destructor Destroy; override;
  end;

  TAvatarParts = class(TSpriteEx)
  private
    FTime: Integer;
    FaceTime: Integer;
    FAnimDelay: Integer;
    ID: string;
    State: string;
    Expression: string;
    WpNum: string;
    Frame: Integer;
    DoFaceAnim: Boolean;
    FaceCount: Integer;
    AlertCount: Integer;
    FaceFrame: Integer;
    BlinkCount: Integer;
    BlinkTime: Integer;
    Image: string;
    FrameCount: Integer;
    AnimRepeat: Boolean;
    AnimEnd: Boolean;
    AnimZigzag: Boolean;
    Value: Integer;
    Owner: TPlayer;
    class var
      MoveX, MoveY: Double;
      ChangeFrame: Boolean;
      Origin: TPoint;
      Neck, Navel, Hand, Brow, HandMove: TPoint;
      ArmHand, ArmNavel, BodyNeck, BodyNavel, BodyHand, lHandMove, HeadBrow, HeadNeck: TPoint;
  public
    class var
      BrowPos: TPoint;
      ZMap: TList<string>;
      property
      AnimDelay: Integer Read FAnimDelay Write FAnimDelay;
    procedure DoMove(const Movecount: Single); override;
    procedure DoDraw; override;
  end;

procedure CreatePlayer;

function GetPart(ID: string): TPartName;

function GetDir(ID: string): string;

var
 // NewID: string = '99999';
  DeleteID: string = '99999';
  DressLongcoat, DressCoat, DressPants, DressNormalWeapon, DressCashWeapon, ShowHair: Boolean;
  DressCape, DressShoes, DressEarring, DressGlass, DressFaceAcc, DressGlove, DressShield, DressFace: Boolean;
  AttackActions, AttackOFs, WeaponWalkType: TList<string>;
  PlayerEqpList: TList<string>;
  AttackAction, NewState: string;
  NEWFRAME: Integer;
  NewFaceState: string = 'blink';
  ChangeState: Boolean;
  Counter: Integer;
  Animate: Boolean = False;
  CharFlip: Boolean;
  DressCap: Boolean;
  CapType: Integer;
  Player: TPlayer;
  AfterImageStr: string;
  WeaponNum: string;
  FState: string;

implementation

uses
  MainUnit, Morph, AfterImage, MapleChair, MapleEffect, TamingMob,Pet,MonsterFamiliar;

procedure CreatePlayer;
var
  Portals: TPortalInfo;
  PX, PY: Integer;
  BelowFH: TFoothold;
  Below: TPoint;
begin
  Player := TPlayer.Create(SpriteEngine);
  Player.Z := 20000;
  Player.Offset.Y := -79;
  Player.Offset.X := -40;
  Player.JumpSpeed := 0.6;
  Player.JumpHeight := 9.5;
  Player.MaxFallSpeed := 8;
  Player.Alpha := 0;
  Player.Tag := 1;

  for Portals in TMapPortal.PortalList do
    if (Portals.PortalType = 0) then
    begin
      PX := Portals.X;
      PY := Portals.Y;
      Break;
    end;
  Player.X := PX;
  Player.Y := PY;
  Below := TFootholdTree.This.FindBelow(Point(PX, PY - 2), BelowFH);
  Player.FH := BelowFH;
  Player.JumpState := jsFalling;
  Player.TruncMove := True;

end;

var
  DestX, DestY: Double;

function GetDir(ID: string): string;
begin
  case ID.ToInteger div 10000 of
    0, 1:
      Result := '';
    2:
      Result := 'Face/';
    3, 4:
      Result := 'Hair/';
    101, 102, 103:
      Result := 'Accessory/';
    100:
      Result := 'Cap/';
    110:
      Result := 'Cape/';
    104:
      Result := 'Coat/';
    108:
      Result := 'Glove/';
    105:
      Result := 'Longcoat/';
    106:
      Result := 'Pants/';
    109:
      Result := 'Shield/';
    107:
      Result := 'Shoes/';
    121..160, 170:
      Result := 'Weapon/';
    190..199:
      Result := 'TamingMob/';
  end;
end;

function GetPart(ID: string): TPartName;
begin
  case ID.ToInteger div 10000 of
    0:
      Result := Body;
    1:
      Result := Head;
    2:
      Result := Face;
    3, 4:
      Result := Hair;
    101:
      Result := FaceAcc;
    102:
      Result := Glass;
    103:
      Result := EarRing;
    100:
      Result := Cap;
    110:
      Result := Cape;
    104:
      Result := Coat;
    108:
      Result := Glove;
    105:
      Result := Longcoat;
    106:
      Result := Pants;
    109:
      Result := Shield;
    107:
      Result := Shoes;
    121..160:
      Result := Weapon;
    170:
      Result := CashWeapon;
    190..197, 199:
      Result := WalkTamingMob;
    198:
      Result := SitTamingMob
  end;
end;

function GetWeaponNum(ID: string): string;
var
  AID: Integer;
begin
  AID := ID.ToInteger;
  Result := ((AID div 10000) - 100).ToString;
end;

function GetAfterImageStr(ID: string): string;
var
  AID, Num: Integer;
begin
  AID := ID.ToInteger;
  Num := (AID div 10000) - 100;
  case Num of
    22, 23, 26, 28, 30, 31, 33, 34, 47:
      Result := 'swordOL';
    36:
      Result := 'cane';
    21, 25, 32, 37, 38, 55, 69:
      Result := 'mace';
    24, 27:
      Result := 'swordOS';
    39, 40:
      Result := 'swordTS';
    41, 42:
      Result := 'axe';
    43:
      Result := 'spear';
    44:
      Result := 'poleArm';
    45:
      Result := 'bow';
    46:
      Result := 'crossBow';
    48, 58:
      Result := 'knuckle';
    49:
      Result := 'gun';
    52:
      Result := 'dualBow';
    53:
      Result := 'cannon';
    54:
      Result := 'swordTK';
    56:
      Result := 'swordZB';
    57:
      Result := 'swordZL'
  end;
end;

constructor TPlayer.Create(const AParent: TSprite);
var
  I, j: Integer;
  Iter: TWZIMGEntry;
  S: TStringArray;
const
  DefaultEqps: array[0..7] of string = ('01302030', '00002000', '01062055', '01072054', '01040005', '00020000', '00030020', '00012000');
begin
  inherited;
  AvatarTargets := TAsphyreRenderTargets.Create();
  AvatarEngine := TSpriteEngine.Create(nil);
  AvatarEngine.Canvas := GameCanvas;
  AvatarEngine.WorldX := 21 - 400;
  AvatarEngine.WorldY := 20 - 400;

  AvatarTargetIndex := AvatarTargets.Add(1, 1024, 1024, apf_A8R8G8B8, True, True);
  AvatarPanelIndex := AvatarTargets.Add(1, 4096, 4096, apf_A8R8G8B8, True, True);
  PartOwner := Self;
  TAvatarParts.ZMap := TList<string>.Create;
  PartList := TList<string>.Create;
  for Iter in BaseWZ.GetImgFile('zmap.img').Root.Children do
    TAvatarParts.ZMap.Add(Iter.Name);

  for I := 2000 to 2011 do
  begin
    CharData.Add('0000' + I.ToString + '/swingTF/0/arm', 'swingTF');
    CharData.Add('0000' + I.ToString + '/swingT2/0/arm', 'swingT2');
    CharData.Add('0000' + I.ToString + '/swingOF/0/arm', 'swingOF/0');
    CharData.Add('0000' + I.ToString + '/swingOF/1/arm', 'swingOF/1');
    CharData.Add('0000' + I.ToString + '/swingOF/2/arm', 'swingOF/2');
  end;

  LoadEquip('00002000'); // body
  LoadEquip('01302030'); // weapon
  LoadEquip('01702115'); // cash weapon
  if HasImgFile('Character.wz/Weapon/01702528.img') then
    LoadEquip('01702528');
  if HasImgFile('Character.wz/Weapon/01702357.img') then
    LoadEquip('01702357');
  if HasImgFile('Character.wz/Weapon/01702330.img') then
    LoadEquip('01702330');

  LoadEquip('01092000'); // shield
  LoadEquip('01062055'); // pants
  LoadEquip('01072054'); // shoes
  LoadEquip('01040005'); // coat
  LoadEquip('01102000'); // cape
  if HasImgFile('Character.wz/Longcoat/01052773.img') then
    LoadEquip('01052773')  // longcoat
  else
    LoadEquip('01050007'); // longcoat
  LoadEquip('01081003'); // glove

  LoadEquip('01002071');  //Cap
  if HasImgFile('Character.wz/Cap/01002953.img') then
    LoadEquip('01002953');
  if HasImgFile('Character.wz/Cap/01004782.img') then
    LoadEquip('01004782');

  LoadEquip('00020000'); // face
  //hair
  if HasImgFile('Character.wz/Hair/00040920.img') then
    LoadEquip('00040920');
  LoadEquip('00030020');

  LoadEquip('00012000'); // head
  LoadEquip('01021000'); // glass
  LoadEquip('01032000'); // earring
  LoadEquip('01010000'); // Face acc

  ShowHair := True;
  DressShoes := True;
  DressFace := True;
  DressNormalWeapon := True;

  for I := 0 to 7 do

    PlayerEqpList.Add(DefaultEqps[I]);

  for I := 0 to PartList.Count - 1 do
    with TAvatarParts.Create(AvatarEngine) do
    begin
      Owner := PartOwner;
      ImageLib := EquipImages;
      if EquipData.ContainsKey(PartList[I]) then
        ImageEntry := EquipData[PartList[I]];
      TruncMove := True;
      Tag := 1;
      Value := 1;
      State := 'stand1';
      Expression := 'blink';
      S := Explode('/', PartList[I]);
      // Body,head
      if LeftStr(S[1], 1) = '0' then
      begin
        ID := LeftStr(S[1], 8);
        Image := S[4];
      end
      else
      begin
        ID := LeftStr(S[2], 8);
        Image := S[5];
      end;
      // cash weapon
      if GetPart(ID) = CashWeapon then
      begin
        ID := LeftStr(S[2], 8);
        Image := S[6];
      end;
      Visible := False;
      for j := 0 to 7 do
      begin

        if GetPart(ID) = GetPart(DefaultEqps[j]) then
          Visible := True;
      end;
      // if GetPart(ID) = 'CashWeapon' then
      // Visible := False;
    end;

  AttackAction := AttackActions[0];
  // usecashweapon:=true;
  TAfterImage.Load(AfterImageStr, '0');
  TDamageNumber.Style := 'NoRed1';
  TDamageNumber.Load('');
  {
    LoadSkill('2321008');
    LoadSkill('2121007');
    LoadSkill('2221007');
    LoadSkill('5121001');
    LoadSkill('2301005');
    LoadSkill('1121008');
    LoadSkill('21120005');
    LoadSkill('21110006');
    LoadSkill('12111005');
    LoadSkill('3111003');
  }

end;

destructor TPlayer.Destroy;
begin
  TAvatarParts.ZMap.Free;
  PartList.Free;
  FreeAndNil(AvatarTargets);
  AvatarEngine.Free;
  inherited Destroy;
end;

procedure TPlayer.DoMove(const Movecount: Single);
var
  Direction, AnimDelay: Integer;
  X1, Y1, X2, Y2, FallEdge: Integer;
  NewAction: string;
  Below: TPoint;
  BelowFH, WallFH: TFoothold;
  LadderPos: TPoint;
  LadderRope: TLadderRope;
  PX, PY, Delay: Integer;
  NextPortal: TPortalInfo;
begin
  inherited;
  if GameMode = gmView then
    Exit;
  GameDevice.RenderTo(TargetEvent, 0, True, AvatarTargets[AvatarTargetIndex]);

  // Engine.Move(1);
  X1 := FH.X1;
  Y1 := FH.Y1;
  X2 := FH.X2;
  Y2 := FH.Y2;

  if (Keyboard.Key[DIK_LMENU]) and (not InLadder) and (not FAttack) { and(not OnPortal) } then
    DoJump := True;

  LadderRope := TLadderRope.Find(Point(Round(X), Round(Y) + OffY), OnLadder);

  if (Keyboard.Key[DIK_UP]) then
  begin
    OffY := -3;
    if OnLadder then
    begin
      Dir := no;
      InLadder := True;
      JumpState := jsNone;
      X := LadderRope.X;
      Y := Y - 1.5;
    end;

    if (InLadder) and (Y < LadderRope.Y1) then
    begin
      if (LadderRope.uf = 0) then
        Y := LadderRope.Y1;
      if (LadderRope.uf = 1) then
      begin
        Below := TFootholdTree.This.FindBelow(Point(Round(X), Round(Y - 5)), BelowFH);
        Y := Below.Y;
        FH := BelowFH;
        Z := FH.Z * 100000 + 60000;
        InLadder := False;
        Dir := no;
      end;
    end;

  end;

  if (Keyboard.Key[DIK_DOWN]) and (JumpState = jsNone) then
  begin
    OffY := 0;
    if OnLadder then
    begin
      Dir := no;
      InLadder := True;
      JumpState := jsNone;
      X := LadderRope.X;
      Y := Y + 1.5;
    end;
    if (Y > LadderRope.Y2) and (InLadder) then
    begin
      InLadder := False;
      JumpState := jsFalling;

    end;
  end;
  if InLadder then
    Z := LadderRope.Page * 100000 + 60000;

  case LadderRope.L of
    0:
      LadderType := rtRope;
    1:
      LadderType := rtLadder;
  end;

  Portal := TMapPortal.Find(Point(Round(X), Round(Y)), OnPortal);
  if (Keyboard.Key[DIK_UP]) and (JumpState = jsNone) and (OnPortal) and (not TMap.FadeScreen.DoFade) then
  begin
    if Portal.ToMap <> '999999999' then
      if (Portal.PortalType = 2) or (Portal.PortalType = 1) then
      begin
        PlaySounds('Game', 'Portal');
        CurrentX := X;
        CurrentPortal := Portal;
        TMap.FadeScreen.DoFade := True;
        TMap.FadeScreen.AlphaCounter := 5;
        TMap.FadeScreen.AValue := 5;
      end;
  end;

  if (TMap.FadeScreen.DoFade) then
  begin
    Inc(TMap.FadeScreen.AlphaCounter, TMap.FadeScreen.AValue);
    if (TMap.FadeScreen.AValue = 5) then
      X := CurrentX;
    if (TMap.FadeScreen.AlphaCounter = 255) then
    begin
      begin
        TMap.ID := Add9(Portal.ToMap);
        if TMap.ID <> '' then
          TMap.ReLoad := True;
        TMap.FadeScreen.AValue := -6;
      end;
    end;

    if { (FadeScreen.AValue =-5) and } (TMap.FadeScreen.AlphaCounter = 249) then
    begin
      for NextPortal in TMapPortal.PortalList do
        if (NextPortal.ToMap + CurrentPortal.ToName = NextPortal.ToMap + NextPortal.PortalName) then
        begin
          PX := NextPortal.X;
          PY := NextPortal.Y;
          Below := TFootholdTree.This.FindBelow(Point(PX, PY - 5), BelowFH);
          FH := BelowFH;
          X := PX;
          Y := PY - 2;
          if TPet.Pet <> nil then
          begin
            TPet.Pet.X := x;
            TPet.Pet.Y := y-50;
            Tpet.Pet.JumpState := jsFalling;
          end;

          if TMonsterFamiliar.MonsterFamiliar <> nil then
          begin
            TMonsterFamiliar.MonsterFamiliar.X := x;
            TMonsterFamiliar.MonsterFamiliar.Y := y-50;
            TMonsterFamiliar.MonsterFamiliar.JumpState := jsFalling;
          end;

          Z := FH.Z * 100000 + 60000;
          SpriteEngine.WorldX := PX - DisplaySize.X / 2;
          SpriteEngine.WorldY := PY - (DisplaySize.Y / 2) - 100;
          if SpriteEngine.WorldX > TMap.Right then
            SpriteEngine.WorldX := TMap.Right;
          if SpriteEngine.WorldX < TMap.Left then
            SpriteEngine.WorldX := TMap.Left;
          if SpriteEngine.WorldY > TMap.Bottom then
            SpriteEngine.WorldY := TMap.Bottom;
          if SpriteEngine.WorldY < TMap.Top then
            SpriteEngine.WorldY := TMap.Top;
          // Reset := True;
          Break;
        end;
    end;

  end;

  if TMap.FadeScreen.AlphaCounter <= 2 then
    TMap.FadeScreen.DoFade := False;
  // ctrl + left
  if (Keyboard.Key[DIK_LMENU]) and (Keyboard.Key[DIK_LEFT]) and (InLadder) then
    DoJump := True;
  // ctrl +right
  if (Keyboard.Key[DIK_LMENU]) and (Keyboard.Key[DIK_RIGHT]) and (InLadder) then
    DoJump := True;

  if JumpState = jsJumping then
    InLadder := False;
  // left
  if (Keyboard.Key[DIK_LEFT]) and (SpeedR = 0) then
  begin
    Dir := dLeft;
    if (JumpState <> jsFalling) then
    begin
      SpeedL := SpeedL + 1.5;
      if SpeedL > 2.5 then
        SpeedL := 2.5;
    end;
  end
  else
  begin
    SpeedL := SpeedL - 0.25;
    if SpeedL < 0 then
      SpeedL := 0;
  end;

  // right
  if (Keyboard.Key[DIK_RIGHT]) and (SpeedL = 0) then
  begin
    Dir := dRight;
    if (JumpState <> jsFalling) then
    begin
      SpeedR := SpeedR + 1.5;
      if SpeedR > 2.5 then
        SpeedR := 2.5;
    end;
  end
  else
  begin
    SpeedR := SpeedR - 0.25;
    if SpeedR < 0 then
      SpeedR := 0;
  end;

  DestX := DisplaySize.X / 2 - X + Engine.WorldX;
  if Abs(DestX) > 1 then
    Engine.WorldX := Engine.WorldX - DestX * (12 / DisplaySize.X);

  DestY := (DisplaySize.Y / 2) + (TMap.OffsetY + 60) - Y;
  if Abs(DestY + Engine.WorldY) > 1 then
    Engine.WorldY := Engine.WorldY - 0.01 * (DestY + Engine.WorldY);

  if Engine.WorldX < TMap.Left then
    Engine.WorldX := TMap.Left;
  if Engine.WorldX > TMap.Right - DisplaySize.X then
    Engine.WorldX := TMap.Right - DisplaySize.X;
  if Engine.WorldY < TMap.Top then
    Engine.WorldY := TMap.Top;
  if Engine.WorldY > TMap.Bottom - DisplaySize.Y then
    Engine.WorldY := TMap.Bottom - DisplaySize.Y;

  if TMap.Right - TMap.Left < DisplaySize.X then
    Engine.WorldX := TMap.Left - ((DisplaySize.X - TMap.Info['MapWidth']) div 2);

  if (Dir = dLeft) and (SpeedR = 0) and (not InLadder) { and (X > MapLeft+20) } then
  begin

    if (OnPortal) and (TMap.FadeScreen.AValue = 5) then
      Exit;
    if (Keyboard.Key[DIK_LMENU]) and (not FAttack) then
      DoJump := True;
    if (X < TMap.Left + 20) or (FAttack) and (JumpState = jsNone) then
      SpeedL := 0;
    if not TTamingMob.IsUse then
      if (FState = 'prone') or (FState = 'proneStab') or (not SkillEnded) then
        SpeedL := 0;

    Direction := GetAngle256(X1, Y1, X2, Y2);
    if (not FH.IsWall) then
    begin
      X := X + (Sin256(Direction) * SpeedL);
      Y := Y - (Cos256(Direction) * SpeedL);
    end;
    FallEdge := -999999;
    if FH.Prev = nil then
      FallEdge := FH.X1 - 10;

    // Wall down
    if (FH.Prev <> nil) and (FH.Prev.IsWall) and (FH.Prev.Y1 > Y) then
      FallEdge := FH.X1;

    if (JumpState = jsNone) and (X < FallEdge) then
      JumpState := jsFalling;
    Below := TFootholdTree.This.FindBelow(Point(Round(X + 10), Round(Y - 5)), BelowFH);
    WallFH := TFootholdTree.This.FindWallR(Point(Round(X + 4), Round(Y - 4)));
    if (WallFH <> nil) and (X <= WallFH.X1) and (BelowFH.Z = WallFH.Z) then
    begin
      X := WallFH.X1 + 1;
      SpeedL := 0;
    end;
    // walk left
    if (X <= FH.X1) and (FH.PrevID <> 0) and (not FH.IsWall) and (not FH.Prev.IsWall) then
    begin
      if (JumpState = jsNone) then
      begin
        FH := FH.Prev;
        X := FH.X2;
        Y := FH.Y2;
        Z := FH.Z * 100000 + 60000;
      end;
    end;

  end;

  // walk right
  if (Dir = dRight) and (SpeedL = 0) and (not InLadder) { and (X < MapRight+800-20) } then
  begin

    if (OnPortal) and (TMap.FadeScreen.AValue = 5) then
      Exit;
    if (Keyboard.Key[DIK_LMENU]) and (not FAttack) then
      DoJump := True;
    if (X > TMap.Right - 20) or (FAttack) and (JumpState = jsNone) then
      SpeedR := 0;

    if not TTamingMob.IsUse then
      if (FState = 'prone') or (FState = 'proneStab') or (not SkillEnded) then
        SpeedR := 0;

    Direction := GetAngle256(X2, Y2, X1, Y1);
    if (not FH.IsWall) then
    begin
      X := X + (Sin256(Direction) * SpeedR);
      Y := Y - (Cos256(Direction) * SpeedR);
    end;
    FallEdge := 999999;
    if (FH.Next = nil) then
      FallEdge := FH.X2 + 5;
    // Wall down
    if (FH.Next <> nil) and (FH.Next.IsWall) and (FH.Next.Y2 > Y) then
      FallEdge := FH.X2;

    if (JumpState = jsNone) and (X > FallEdge) then
      JumpState := jsFalling;
    Below := TFootholdTree.This.FindBelow(Point(Round(X - 10), Round(Y - 5)), BelowFH);
    WallFH := TFootholdTree.This.FindWallL(Point(Round(X - 4), Round(Y - 4)));
    if (WallFH <> nil) and (X >= WallFH.X1) and (BelowFH.Z = WallFH.Z) then
    begin
      X := WallFH.X1 - 1;
      SpeedR := 0;
    end;
    // walk right
    if (X >= FH.X2) and (FH.NextID <> 0) and (not FH.IsWall) and (not FH.Next.IsWall) then
    begin
      if (JumpState = jsNone) then
      begin
        FH := FH.Next;
        X := FH.X1;
        Y := FH.Y1;
        Z := FH.Z * 100000 + 60000;
      end;
    end;

  end;

  if (JumpState = jsFalling) and (FallFlag) then
  begin
    Below := TFootholdTree.This.FindBelow(Point(Round(X), Round(Y - VelocityY - 6)), BelowFH);
    if Y >= Below.Y - 3 then
    begin
      Y := Below.Y;
      MaxFallSpeed := 10;
      JumpState := jsNone;
      FH := BelowFH;
      Z := FH.Z * 100000 + 60000;
    end;
  end;

  if (Keyboard.Key[DIK_LMENU]) then
    if (Keyboard.Key[DIK_DOWN]) and (JumpState = jsNone) and (not InLadder)
    { and (FHGE.Input_GetKeyState(HGEK_Down)) } then
    begin
      FallFlag := False;
      Below := TFootholdTree.This.FindBelow(Point(Round(X), Round(Y + 4)), BelowFH);

      if (BelowFH.X2 > BelowFH.X1) then
        if (Below.Y <> 99999) then
        begin
          // Y := Y + 6;
          JumpState := jsFalling;
        end;
    end;

  if not FallFlag then
    Inc(FallCounter);
  if FallCounter > VelocityY + 3 then
  begin
    FallFlag := True;
    FallCounter := 0;
  end;

  if (JumpState = jsJumping) then
  begin
    Below := TFootholdTree.This.FindBelow(Point(Round(X), Round(Y - 10)), BelowFH);
    if (BelowFH.X2 < BelowFH.X1) then
      JumpState := jsFalling;
  end;

end;

procedure TPlayer.LoadEquip(EquipID: string);
var
  Child: TWZIMGEntry;
  Iter, Iter2, Iter3, Iter4, Entry: TWZIMGEntry;
  Path, Dir: string;
  Part: TPartName;
  SameName: TList<string>;
begin
  Dir := GetDir(EquipID);
  Part := GetPart(EquipID);
  Entry := CharacterWZ.GetImgFile(Dir + EquipID + '.img').Root;
 // if not EquipImages.ContainsKey(Entry) then
  DumpData(Entry, EquipData, EquipImages);

  if Part = Weapon then
  begin
    AfterImageStr := GetAfterImageStr(EquipID);
    TAfterImage.Load(AfterImageStr, '0');
    WeaponNum := GetWeaponNum(EquipID);
    AttackActions.Clear;
    AttackOFs.Clear;
    WeaponWalkType.Clear;
  end;

  for Iter in Entry.Children do
  begin
    if (Dir = 'Weapon/') and (Iter.Name <> 'info') then
    begin
      if (LeftStr(Iter.Name, 4) = 'stab') or (LeftStr(Iter.Name, 5) = 'swing') then
      begin
        if RightStr(Iter.Name, 1) <> 'F' then
          AttackActions.Add(Iter.Name)
        else
          AttackOFs.Add(Iter.Name);
      end;
      if (LeftStr(Iter.Name, 4) = 'walk') or (LeftStr(Iter.Name, 5) = 'stand') then
        WeaponWalkType.Add(Iter.Name);
    end;

    if Part = Body then
      CharData.AddOrSetValue('body/' + Iter.Name + '/FrameCount', Iter.Children.Count - 1);
    if Part = Face then
      CharData.AddOrSetValue('face/' + Iter.Name + '/FrameCount', Iter.Children.Count - 1);

    for Iter2 in Iter.Children do
    begin
      if Part = Body then
        CharData.AddOrSetValue('body/' + Iter.Name + '/' + Iter2.Name + '/delay', Abs(Iter2.Get('delay', 0)));
      if Part = Face then
        CharData.AddOrSetValue('face/' + Iter.Name + '/' + Iter2.Name + '/delay', Iter2.Get('delay', 0));

      if (Iter2.Child['action'] <> nil) and (Iter2.Child['frame'] <> nil) then
      begin
        CharData.AddOrSetValue(Iter.Name + '/' + Iter2.Name, Iter2.Get('action', '') + '/' + IntToStr(Iter2.Get('frame', '')));
      end;
    end;
  end;

  if TMap.FirstLoad then
    Exit;
  SameName := TList<string>.Create;
  for Iter in Entry.Children do
    for Iter2 in Iter.Children do
    begin
      if Iter2.Name = 'hairShade' then
        Continue;
      for Iter3 in Iter2.Children do
      begin
        if (Iter3.Name = 'hairShade') or (Iter3.Name = '006') then
          Continue;
        if (Iter3.DataType = mdtCanvas) or (Iter3.DataType = mdtUOL) then
          if not SameName.contains(Iter3.Name) then
          begin
            SameName.Add(Iter3.Name);
            PartList.Add(GetEntryPath(Iter3));
          end;
      end;
    end;

  if Part = CashWeapon then
  begin
    Entry := GetImgEntry('Character.wz/' + Dir + EquipID + '.img/' + WeaponNum);

    for Iter in Entry.Children do
      for Iter2 in Iter.Children do
      begin
        for Iter3 in Iter2.Children do
        begin
          if (Iter3.DataType = mdtCanvas) or (Iter3.DataType = mdtUOL) then
            if not SameName.contains(Iter3.Name) then
            begin
              SameName.Add(Iter3.Name);
              PartList.Add(Iter3.GetPath);
            end;
        end;
      end;

  end;
  SameName.Free;

end;

procedure TPlayer.DoDraw;
var
  WX, WY, NamePos, IDPos: Integer;
begin
  WX := Round(TAvatarParts.MoveX - Engine.WorldX);
  WY := Round(TAvatarParts.MoveY - Engine.WorldY);
  GameCanvas.Draw(AvatarTargets[AvatarTargetIndex], WX - 180 - 400, WY - 180 - 400, 1, False, 255, 255, 255, 255);
end;

procedure TPlayer.TargetEvent;
begin
  AvatarEngine.Draw;
  AvatarEngine.Move(1);
  AvatarEngine.Dead;
end;

function HasEntry(Entry: string): Boolean;
begin
  Result := EquipData.ContainsKey(Entry);
end;

var
  BlinkNum: Integer;

procedure TAvatarParts.DoMove(const Movecount: Single);

  function IsAttack: Boolean;
  begin
    if (LeftStr(State, 4) = 'stab') or (LeftStr(State, 5) = 'swing') or ((LeftStr(State, 5) = 'shoot')) then
      Result := True
    else
      Result := False;
  end;

  function IsSkillAttack: Boolean;
  begin
    if (CharData.ContainsKey(SkillID + '/action')) and (State = CharData[SkillID + '/action']) then
      Result := True
    else
      Result := False;
  end;

  function ArrowKeyDown: Boolean;
  begin
    if (not Keyboard.Key[DIK_LEFT]) and (not Keyboard.Key[DIK_RIGHT]) and (not Keyboard.Key[DIK_UP]) and (not Keyboard.Key[DIK_DOWN]) then
      Result := False
    else
      Result := True;
  end;

var
  Path, AfterImagePath: string;
  BodyDelay, FaceDelay: Integer;
  FaceFrameCount: Integer;
  Flip: Integer;
  AdjX: Integer;
  Move: TPoint;
  Dir: string;
  Part: TPartName;
const
  C = 'Character.wz/';
begin
  inherited;
  if GameMode = gmView then
    Exit;
  if TMorph.IsUse then
  begin
    MoveX := -99999;
    Exit;
  end;

  // if (not Visible) and (GetPart(ID) <> 'Weapon') then
  // Moved := False;
//  if not Visible then
  //  Exit;

  if (Image = 'head') and (FTime = 0) then
    ChangeFrame := True;
  if HasEntry('Character.wz/00002000.img/' + State + '/' + IntToStr(Frame) + '/move') then
  begin
    Move := EquipData['Character.wz/00002000.img/' + State + '/' + IntToStr(Frame) + '/move'].Vector;
  end
  else
  begin
    if TMapleChair.IsUse then
    begin
      Move := TMapleChair.BodyRelMove;
    end
    else
    begin
      Move.X := 0;
      Move.Y := 0;
    end;
  end;
  if MirrorX then
    MoveX := Round(Owner.X - 1 - Move.X)
  else
    MoveX := Round(Owner.X - 1 + Move.X);

  MoveY := Round(Owner.Y + Move.Y);

  // X := Foot.X-1;
  // Y := Foot.Y;

  Owner.FAttack := IsAttack;
  FState := State;

  if TTamingMob.IsUse then
  begin
    Owner.FAttack := False;
    if State <> 'fly' then
      Frame := 0;
    if TTamingMob.CharacterAction = 'StabT2' then
      TTamingMob.CharacterAction := 'stabT2';
    if TTamingMob.CharacterAction <> 'hideBody' then
      State := TTamingMob.CharacterAction;
    if (Part = Weapon) or (Part = CashWeapon) then
      Exit;
  end;

  if ((Keyboard.Key[DIK_LEFT]) or (Keyboard.Key[DIK_RIGHT])) and (not TTamingMob.IsUse) then
  begin
    if (LeftStr(State, 4) <> 'walk') and (Owner.JumpState = jsNone) and (not Owner.InLadder) and (not IsAttack) and (SkillEnded) then
    begin
      FTime := 0;
      Frame := 0;
      //State := 'walk1';
      if WeaponWalkType.contains('walk2') then
        State := 'walk2'
      else
        State := 'walk1';

    end;
  end;

  if (Keyboard.KeyReleased[DIK_LEFT]) or (Keyboard.KeyReleased[DIK_RIGHT]) then
    if (not Owner.InLadder) and (Owner.JumpState = jsNone) and (not IsAttack) and (SkillEnded) then
    begin
      Frame := 0;
      //State := 'stand1';
      if WeaponWalkType.contains('stand2') then
        State := 'stand2'
      else
        State := 'stand1';
    end;

  if (Owner.JumpState <> jsNone) and (not IsAttack) and (not TTamingMob.IsUse) then
  begin
    Frame := 0;
    State := 'jump';
  end;
  // jump ->re stand
  if (Owner.JumpState = jsNone) and (State = 'jump') and (not Keyboard.Key[DIK_LMENU]) then
  begin
   // State := 'stand1';
    if WeaponWalkType.contains('stand2') then
      State := 'stand2'
    else
      State := 'stand1';
  end;
  // press jump+ left(right) key
  if (Keyboard.Key[DIK_LMENU]) and (not IsAttack) and (not TTamingMob.IsUse) then
  begin
    if (Keyboard.Key[DIK_RIGHT]) or (Keyboard.Key[DIK_LEFT]) then
    begin
      Frame := 0;
      State := 'jump';
    end;
  end;

  if (not Owner.InLadder) and (Owner.JumpState = jsNone) and (not TTamingMob.IsUse) then
  begin
    if (not IsAttack) and (SkillEnded) then
    begin
      if (Keyboard.Key[DIK_DOWN]) and (not Keyboard.Key[DIK_LCONTROL]) and (State <> 'proneStab') then
        State := 'prone';

      if (Keyboard.Key[DIK_LCONTROL]) and (State <> 'proneStab') and (SkillEnded) then
      begin
        AnimEnd := False;
        Frame := 0;
        FTime := 0;
        State := 'proneStab';
      end;
    end;

    if (Keyboard.KeyReleased[DIK_DOWN]) and (SkillEnded) then
      if WeaponWalkType.contains('stand2') then
        State := 'stand2'
      else
        State := 'stand1';
  end;

  if (not Owner.InLadder) then
  begin
    if (State = 'rope') or (State = 'ladder') then
    begin
      Frame := 0;
    //  State := 'stand1';
      if WeaponWalkType.contains('stand2') then
        State := 'stand2'
      else
        State := 'stand1';
    end;
  end;

  if (Owner.InLadder) then
  begin
    case Owner.LadderType of
      rtLadder:
        State := 'ladder';
      rtRope:
        State := 'rope';
    end;
  end;

  if (IsAttack) or (State = 'proneStab') or (IsSkillAttack) then
    AnimRepeat := False
  else
    AnimRepeat := True;

  if AnimEnd then
  begin
    if (IsSkillAttack) or (IsAttack) then
    begin
      Value := 1;
      FTime := 0;
      Frame := 0;
      State := 'alert';
      AlertCount := 0;
      StartSkill := False;
    end;
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
    if WeaponWalkType.contains('stand2') then
      State := 'stand2'
    else
      State := 'stand1';
    AlertCount := 0;
  end;

  if (State = 'stand1') or (State = 'stand2') or (State = 'alert') then
    AnimZigzag := True
  else
    AnimZigzag := False;

  if (Keyboard.Key[DIK_LCONTROL]) and (not Keyboard.Key[DIK_DOWN]) and (not IsAttack) and (not Owner.InLadder) and (SkillEnded) and (not TTamingMob.IsUse) then
  begin
    AnimEnd := False;
    Frame := 0;
    FTime := 0;
    State := AttackAction;
  end;

  Inc(Owner.sTime);
  if Owner.sTime > 100 then
    Owner.sTime := 0;
  {
    if (SkillEnded) and (not IsSkillAttack) and (not InLadder) and (sTime = 0) then
    begin
    if (Keyboard.Key[DIK_F]) then
    CreateSkill('2321008', 0, 0);

    if (Keyboard.Key[DIK_G]) then
    CreateSkill('2121007', 0, 0);

    if (Keyboard.Key[DIK_B]) then
    CreateSkill('2221007', 0, 0);

    if (Keyboard.Key[DIK_V]) then
    CreateSkill('5121001', 0, 0);

    if (Keyboard.Key[DIK_A]) then
    CreateSkill('2301005', 0, 0);
    if (Keyboard.Key[DIK_S]) then
    CreateSkill('1121008', 0, 0);

    if (Keyboard.Key[DIK_D]) then
    CreateSkill('21110006', 0, 0);
    if (Keyboard.Key[DIK_Z]) then
    CreateSkill('21120005', 0, 0);
    if (Keyboard.Key[DIK_X]) then
    CreateSkill('12111005', 0, 0);

    if (Keyboard.Key[DIK_C]) then
    CreateSkill('3111003', 0, 0);
    end;
  }
  if (StartSkill) and (not SkillEnded) then
  begin
    if CharData.ContainsKey(SkillID + '/action') then
      if State <> CharData[SkillID + '/action'] then
      begin
        AnimEnd := False;
        Frame := 0;
        FTime := 0;
        State := CharData[SkillID + '/action'];
      end;
  end;

  Part := GetPart(ID);

  if (Part = Weapon) and (FTime = 0) then
  begin
    AfterImagePath := 'Character.wz/Afterimage/' + AfterImageStr + '.img/0/' + State + '/' + IntToStr(Frame) + '/0';
    if HasEntry(AfterImagePath) then
    begin
      PlaySounds('Weapon', 'swordL/Attack');
      TAfterImage.Create(AfterImagePath);
    end;
  end;

  // Expression := NewFaceState;
  with Keyboard do
  begin
    if Key[DIK_F1] then
      Expression := 'hit';
    if Key[DIK_F2] then
      Expression := 'smile';
    if Key[DIK_F3] then
      Expression := 'troubled';
    if Key[DIK_F4] then
      Expression := 'cry';
    if Key[DIK_F5] then
      Expression := 'angry';
    if Key[DIK_F6] then
      Expression := 'bewildered';
    if Key[DIK_F7] then
      Expression := 'stunned';
  end;

  // MirrorX := NewFlip;
  if (not Owner.InLadder) and (not IsAttack) and (SkillEnded) then
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

  if not Animate then
  begin
    if NEWFRAME <= FrameCount then
      Frame := NEWFRAME;
  end;

  if ChangeState then
  begin

    Frame := 1;
    State := NewState;
    Inc(Counter);
  end;

  if not HasEntry(C + GetDir(ID) + ID + '.img/' + WpNum + State + '/' + IntToStr(Frame) + '/' + Image) and (not IsAttack) and (not CharData.ContainsKey(State + '/' + IntToStr(Frame))) then
    Frame := 0;

  FrameCount := CharData['body/' + State + '/FrameCount'];
  BodyDelay := CharData['body/' + State + '/' + IntToStr(Frame) + '/delay'];

  FaceFrameCount := CharData['face/' + Expression + '/FrameCount'];
  FaceDelay := CharData['face/' + Expression + '/' + IntToStr(FaceFrame) + '/delay'];

  Dir := GetDir(ID);
  if (Image <> 'face') and (GetPart(ID) <> FaceAcc) then
  begin
    if Part = CashWeapon then
      WpNum := WeaponNum + '/'
    else
      WpNum := '';

    if CharData.ContainsKey(State + '/' + IntToStr(Frame)) then
      Path := C + Dir + ID + '.img/' + WpNum + CharData[State + '/' + IntToStr(Frame)] + '/' + Image
    else
      Path := C + Dir + ID + '.img/' + WpNum + State + '/' + IntToStr(Frame) + '/' + Image;
  end
  else
    Path := C + Dir + ID + '.img/' + Expression + '/' + IntToStr(FaceFrame) + '/' + Image;
 {
  if Part = GetPart(NewID) then
  begin
    ID := NewID;
    Visible := True;
    //NewID := '99999'
  end;
  }

  for var I := 0 to PlayerEqpList.Count - 1 do
    if Part = GetPart(PlayerEqpList[I]) then
    begin
      ID := PlayerEqpList[I];
      Visible := True
    end;

  if Part = GetPart(DeleteID) then
  begin
    Visible := False;
   // DeleteID := '99999';
  end;

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
    if (State = 'ladder') or (State = 'rope') or (MidStr(Path, 10, 9) = 'swingOF/1') or (MidStr(Path, 10, 9) = 'swingTF/0') then
      Alpha := 0
    else
      Alpha := 255;
  end;

  if not DressCap then
  begin
    if Part = Cap then
      Visible := False;

    if ShowHair then
   //   if (Image = 'hairOverHead') or (Image = 'backHair') then
     //   Visible := True;
      if Part = Hair then
        Visible := True;
  end;

  if (DressCap) and (ShowHair) then
  begin

    if Part = Hair then
      case CapType of
        0:
          Visible := True;
        1:
          begin
            if (Image = 'hairOverHead') or (Image = 'backHair') then
              Visible := False;
          end;
        2:
          Visible := False;
      end;
  end;

  case Part of
    Weapon:
      begin
        if DressCashWeapon then
          Visible := False;
        if not DressNormalWeapon then
          Visible := False;
      end;

    CashWeapon:
      begin
        if DressNormalWeapon then
          Visible := False;
        if not DressCashWeapon then
          Visible := False;
      end;

    Longcoat:
      begin
        if (DressCoat) or (DressPants) then
          Visible := False;
        if not DressLongcoat then
          Visible := False;
      end;

    Coat, Pants:
      if DressLongcoat then
        Visible := False;

    Cape:
      if not DressCape then
        Visible := False;

    Glove:
      if not DressGlove then
        Visible := False;

    EarRing:
      if not DressEarring then
        Visible := False;

    Glass:
      if not DressGlass then
        Visible := False;

    FaceAcc:
      if not DressFaceAcc then
        Visible := False;

    Shield:
      if not DressShield then
        Visible := False;

    Shoes:
      if not DressShoes then
        Visible := False;

    Face:
      if not DressFace then
        Visible := False;

  end;



 // if Part = 'SitTamingMob' then
   // Visible := True;

  if (Image = 'ear') or (Image = 'lefEar') or (Image = 'highlefEar') then
    Alpha := 0;

  Player.MirrorX := MirrorX;

  if (State = 'stand1') or (State = 'stand2') or (State = 'alert') or (State = 'sit') then
    TMapleChair.CanUse := True
  else
    TMapleChair.CanUse := False;

  if TMapleChair.IsUse then
  begin

    State := TMapleChair.CharacterAction;

  end;

  if (TTamingMob.IsUse) or (TMapleChair.IsUse) then
  begin
    if (Part = Weapon) or (Part = CashWeapon) then
      Visible := False;
  end;

  if HasEntry(Path + '/z') then
    Z := 100 + Owner.Z - ZMap.IndexOf(EquipData[Path + '/z'].Data);

  if Owner.InLadder then
  begin
    if (Keyboard.KeyReleased[DIK_UP]) or (Keyboard.KeyReleased[DIK_DOWN]) then
      Animate := False;
    if (Keyboard.Key[DIK_UP]) or (Keyboard.Key[DIK_DOWN]) then
      Animate := True;
  end
  else
    Animate := True;

  if Animate then
    FTime := FTime + 17;
  if FTime > BodyDelay then
  begin

    if Counter > 50 then
    begin
      ChangeState := False;
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

  if HasEntry(Path + '/origin') then
  begin
    case MirrorX of
      True:
        begin
          Flip := -1;
          if Owner.InLadder then
            AdjX := 3
          else
            AdjX := 0;
          Origin.X := EquipData[Path + '/origin'].Vector.X - ImageWidth + AdjX;
        end;
      False:
        begin
          Flip := 1;
          Origin.X := -EquipData[Path + '/origin'].Vector.X;
        end;
    end;
    Origin.Y := -EquipData[Path + '/origin'].Vector.Y;
  end;

  if HasEntry(Path + '/map/brow') then
  begin
    Brow.X := -EquipData[Path + '/map/brow'].Vector.X * Flip;
    Brow.Y := -EquipData[Path + '/map/brow'].Vector.Y;
    if Image = 'head' then
      HeadBrow := Brow;

    Offset.X := Origin.X + HeadNeck.X - BodyNeck.X - HeadBrow.X + Brow.X - TTamingMob.Navel.X;
    Offset.Y := Origin.Y + HeadNeck.Y - BodyNeck.Y - HeadBrow.Y + Brow.Y - TTamingMob.Navel.Y;
    BrowPos := Offset;
  end;

  if HasEntry(Path + '/map/neck') then
  begin
    Neck.X := -EquipData[Path + '/map/neck'].Vector.X * Flip;
    Neck.Y := -EquipData[Path + '/map/neck'].Vector.Y;
    if Image = 'body' then
      BodyNeck := Neck;
    if Image = 'head' then
      HeadNeck := Neck;
  end;

  if HasEntry(Path + '/map/hand') then
  begin
    Hand.X := -EquipData[Path + '/map/hand'].Vector.X * Flip;
    Hand.Y := -EquipData[Path + '/map/hand'].Vector.Y;
    if Image = 'arm' then
      ArmHand := Hand;
    if Image = 'body' then
      BodyHand := Hand;
    Offset.X := Origin.X + Hand.X + ArmNavel.X - ArmHand.X - BodyNavel.X;
    Offset.Y := Origin.Y + Hand.Y + ArmNavel.Y - ArmHand.Y - BodyNavel.Y;
  end;

  if HasEntry(Path + '/map/handMove') then
  begin
    HandMove.X := -EquipData[Path + '/map/handMove'].Vector.X * Flip;
    HandMove.Y := -EquipData[Path + '/map/handMove'].Vector.Y;
    if Image = 'lHand' then
      lHandMove := HandMove;

    Offset.X := Origin.X + HandMove.X - lHandMove.X;
    Offset.Y := Origin.Y + HandMove.Y - lHandMove.Y;
  end;

  if HasEntry(Path + '/map/navel') then
  begin
    Navel.X := -EquipData[Path + '/map/navel'].Vector.X * Flip;
    Navel.Y := -EquipData[Path + '/map/navel'].Vector.Y;
    if Image = 'arm' then
      ArmNavel := Navel;
    if Image = 'body' then
      BodyNavel := Navel;

    Offset.X := Origin.X + Navel.X - BodyNavel.X - TTamingMob.Navel.X;
    Offset.Y := Origin.Y + Navel.Y - BodyNavel.Y - TTamingMob.Navel.Y;
  end;

end;

procedure TAvatarParts.DoDraw;
var
  WX, WY, NamePos: Integer;
begin
  if GameMode = gmView then
    Exit;
  if ChangeFrame then
    ChangeFrame := False;
  if TMap.ShowChar then
    inherited;

  if Image = 'head' then
  begin

    if Keyboard.Key[DIK_LCONTROL] then
      case Random(10) of
        0..8:
          AttackAction := AttackActions[Random(AttackActions.Count)];
        9:
          begin
            if AttackOFs.Count > 0 then
              AttackAction := AttackOFs[Random(AttackOFs.Count)]
            else
              AttackAction := AttackActions[Random(AttackActions.Count)];
          end;
      end;

  end;

  if Visible then
    Moved := True;

end;

initialization
  AttackActions := TList<string>.Create;
  AttackOFs := TList<string>.Create;
  WeaponWalkType := TList<string>.Create;
  PlayerEqpList := TList<string>.Create;

finalization
  AttackActions.Free;
  AttackOFs.Free;
  WeaponWalkType.Free;
  PlayerEqpList.Free;

end.

