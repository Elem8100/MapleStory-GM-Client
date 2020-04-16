unit MapleCharacter;

interface

uses
  Windows, SysUtils, StrUtils, Generics.Collections, System.Types, WZIMGFile, Math, PXT.Sprites,
  Footholds, LadderRopes, ChatBalloon, MapPortal, DirectInput, Classes, AsphyreKeyboard,
  AsphyreRenderTargets, DamageNumber, Skill, AsphyreTypes, AbstractTextures, Global, Tools, MapleMap,
  WzUtils, ColorUtils, PXT.Types, PXT.Graphics, PXT.Canvas;

type
  TDir = (dLeft, dRight, no);

  TLadderType = (rtLadder, rtRope);

  TPartName = (Head, Body, Cap, Face, Hair, Glove, FaceAcc, Glass, EarRing, Cape, Coat, Longcoat,
    Pants, Shield, Shoes, Weapon, CashWeapon, Chairs, SitTamingMob, WalkTamingMob, TamingMob);

  TAvatarParts = class;

  TPlayer = class(TJumperSprite)
    AvatarEngine: TSpriteEngine;
    FH: TFoothold;
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
    NameTagTargetIndex: Integer;
    CurrentPortal: TPortalInfo;
    Portal: TPortalInfo;
    LadderType: TLadderType;
    StandType, WalkType: string;
    SpriteList: TList<TAvatarParts>;
    ShowHair: Boolean;
    DressCap: Boolean;
    CapType: Integer;
    WeaponNum: string;
    ResetAction: Boolean;
    NewAction: string;
    AfterImageStr: string;
    AttackAction, Str: string;
    AttackActions, AttackOFs: TList<string>;
    Flip: Boolean;
    OtherPlayer: Boolean;
    Neck, Navel, Hand, Brow, HandMove: TPoint;
    ArmHand, ArmNavel, BodyNeck, BodyNavel, BodyHand, lHandMove, HeadBrow, HeadNeck: TPoint;
    TamingNavel: TPoint;
    MoveX, MoveY: Double;
    BrowPos: TPoint;
    NewZ: Integer;
    class var
      AvatarTargetTexture: TTexture;
      AvatarPanelIndex: Integer;
      EquipDumpList: TList<string>;
      _NewZ: Integer;
    procedure CreateEquip(EquipID: string; UseEngine: TSpriteEngine);
    procedure RemoveSprites;
    procedure SpawnNew;
    procedure Spawn(EquipID: string);
    procedure TargetEvent;
    procedure DoMove(const Movecount: Single); override;
    procedure DoDraw; override;
    constructor Create(const AParent: TSprite); override;
    destructor Destroy; override;
  end;

  TAvatarParts = class(TSpriteEx)
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
    ChangeFrame: Boolean;
    Origin: TPoint;
    Flip: Integer;
    MoveOffset: TPoint;
    Animate: Boolean;
    Counter: Integer;
  public
    class var
      ZMap: TList<string>;
      property
      AnimDelay: Integer Read FAnimDelay Write FAnimDelay;
    function IsAttack: Boolean;
    procedure UpdateFrame;
    procedure DoMove(const Movecount: Single); override;
    procedure DoDraw; override;
  end;

function GetPart(ID: string): TPartName;

function GetDir(ID: string): string;

function GetWeaponNum(ID: string): string;

function GetAfterImageStr(ID: string): string;

var
  PlayerEqpList: TList<string>;
  Player: TPlayer;

implementation

uses
  MainUnit, Morph, AfterImage, MapleChair, MapleEffect, TamingMob, Pet, MonsterFamiliar,
  MapleCharacterEx, Android;

procedure TPlayer.SpawnNew;
var
  I, j: Integer;
  Iter: TWZIMGEntry;
  Portals: TPortalInfo;
  PX, PY: Integer;
  BelowFH: TFoothold;
  Below: TPoint;
const
  DefaultEqps: array[0..7] of string = ('01302030', '00002000', '01062055', '01072054', '01040005',
    '00020000', '00030020', '00012000');
begin
  GameCanvas.DrawTarget(AvatarTargetTexture, 1024, 1024,
    procedure
    begin
    end);
  Player := TPlayer.Create(SpriteEngine);
  Player.AvatarEngine := TSpriteEngine.Create(nil);
  Player.AvatarEngine.Canvas := GameCanvas;
  Player.AvatarEngine.WorldX := 21 - 400;
  Player.AvatarEngine.WorldY := 20 - 400;

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

  TAvatarParts.ZMap := TList<string>.Create;

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

  for I := 0 to 7 do
  begin
    Player.CreateEquip(DefaultEqps[I], Player.AvatarEngine);
    PlayerEqpList.Add(DefaultEqps[I]);
  end;

  Player.AttackAction := Player.AttackActions[0];
  TAfterImage.Load(Player.AfterImageStr, '0');
  TDamageNumber.Style := 'NoRed1';
  TDamageNumber.Load('');

end;

procedure TPlayer.Spawn(EquipID: string);
begin

  CreateEquip(EquipID, AvatarEngine);
end;

procedure TPlayer.CreateEquip(EquipID: string; UseEngine: TSpriteEngine);
var
  Iter, Iter2, Iter3, Entry: TWZIMGEntry;
  Path, Dir: string;
  Part: TPartName;
  SameName: TList<string>;
begin
  Dir := GetDir(EquipID);
  Part := GetPart(EquipID);
  Entry := CharacterWZ.GetImgFile(Dir + EquipID + '.img').Root;

  if not EquipDumpList.contains(EquipID) then
  begin
    DumpData(Entry, EquipData, EquipImages);
    EquipDumpList.Add(EquipID);
  end;
  var LPath := 'Character.wz/Weapon/';

  case Part of
    Weapon:
      begin
        AfterImageStr := GetAfterImageStr(EquipID);
        TAfterImage.Load(AfterImageStr, '0');
        WeaponNum := GetWeaponNum(EquipID);
        AttackActions.Clear;
        AttackOFs.Clear;
        if HasEntryE(LPath + EquipID + '.img/stand1') then
          StandType := 'stand1'
        else if HasEntryE(LPath + EquipID + '.img/stand2') then
          StandType := 'stand2';

        if HasEntryE(LPath + EquipID + '.img/walk1') then
          WalkType := 'walk1'
        else if HasEntryE(LPath + EquipID + '.img/walk2') then
          WalkType := 'walk2';
      end;
    Cap:
      begin
        DressCap := True;
        var Data := GetImgEntry('Character.wz/Cap/' + EquipID + '.img/info/vslot').Data;
        //no Cover
        if (Data = 'Cp') or (Data = 'CpH5') then
          CapType := 0;
        //stand cover
        if Data = 'CpH1H5' then
          CapType := 1;
        //cover all
        if Length(Data) > 12 then
        begin
          if LeftStr(Data, 6) = 'CpH1H3' then
            CapType := 1
          else
            CapType := 2;
        end;
      end;
    Hair:
      ShowHair := True;
    CashWeapon:
      begin
        for var I := 69 downto 30 do
        begin
          if HasEntryE(LPath + EquipID + '.img/' + I.ToString + '/stand1') then
            StandType := 'stand1'
          else if HasEntryE(LPath + EquipID + '.img/' + I.ToString + '/stand2') then
            StandType := 'stand2';

          if HasEntryE(LPath + EquipID + '.img/' + I.ToString + '/walk1') then
            WalkType := 'walk1'
          else if HasEntryE(LPath + EquipID + '.img/' + I.ToString + '/walk2') then
            WalkType := 'walk2';
          if HasEntryE(LPath + EquipID + '.img/' + I.ToString) then
            WeaponNum := I.ToString;
        end;
        AttackActions.Clear;
        AttackOFs.Clear;
        AfterImageStr := GetAfterImageStr('01' + WeaponNum + '1234');
        for Iter in GetImgEntry('Character.wz/Afterimage/' + AfterImageStr + '.img/0').Children do
          if (LeftStr(Iter.Name, 4) = 'stab') or (LeftStr(Iter.Name, 5) = 'swing') then
          begin
            if (RightStr(Iter.Name, 2) <> 'D1') and (RightStr(Iter.Name, 2) <> 'D2') then
            begin
              if RightStr(Iter.Name, 1) <> 'F' then
                AttackActions.Add(Iter.Name)
              else
                AttackOFs.Add(Iter.Name);
            end;
          end;

        TAfterImage.Load(AfterImageStr, '0');
        Entry := GetImgEntry('Character.wz/' + Dir + EquipID + '.img/' + WeaponNum);
      end;

  end;

  var Sprite: TAvatarParts;
  var S: TStringArray;
  SameName := TList<string>.Create;

  for Iter in Entry.Children do
  begin
    case Part of
      Weapon:
        begin
          if Iter.Name <> 'info' then
            if (LeftStr(Iter.Name, 4) = 'stab') or (LeftStr(Iter.Name, 5) = 'swing') then
            begin
              if RightStr(Iter.Name, 1) <> 'F' then
                AttackActions.Add(Iter.Name)
              else
                AttackOFs.Add(Iter.Name);
            end;
        end;
      Body:
        begin
          CharData.AddOrSetValue('body/' + Iter.Name + '/FrameCount', Iter.Children.Count - 1);
        end;
      Face:
        begin
          CharData.AddOrSetValue('face/' + Iter.Name + '/FrameCount', Iter.Children.Count - 1);
        end;
    end;

    for Iter2 in Iter.Children do
    begin
      if Part = Body then
        CharData.AddOrSetValue('body/' + Iter.Name + '/' + Iter2.Name + '/delay', Abs(Iter2.Get('delay', 0)));
      if Part = Face then
        CharData.AddOrSetValue('face/' + Iter.Name + '/' + Iter2.Name + '/delay', Iter2.Get('delay', 0));
      if (Iter2.Child['action'] <> nil) and (Iter2.Child['frame'] <> nil) then
      begin
        CharData.AddOrSetValue(Iter.Name + '/' + Iter2.Name, Iter2.Get('action', '') + '/' +
          IntToStr(Iter2.Get('frame', '')));
      end;
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
            if OtherPlayer then
              Sprite := TAvatarPartEx.Create(UseEngine)
            else
              Sprite := TAvatarParts.Create(UseEngine);
            with Sprite do
            begin
              if OtherPlayer then
                Visible := True
              else
              begin
                if (TMapleChair.IsUse) or (TTamingMob.IsUse) then
                begin
                  if (Part = CashWeapon) or (Part = Weapon) then
                    Visible := False
                  else
                    Visible := True
                end
                else
                  Visible := False;
              end;
              Owner := Self;
              ImageLib := EquipImages;
              Path := Iter3.GetPath;
              if EquipData.ContainsKey(Path) then
                ImageEntry := EquipData[Path];
              TruncMove := True;
              Tag := 1;
              Value := 1;
              State := 'stand1';
              MirrorX := Self.MirrorX;
              Expression := 'blink';
              Animate := True;
              AnimRepeat := True;
              S := Explode('/', Path);
                // Body,head
              if Part <> CashWeapon then
              begin
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
              end
              else
              begin
                ID := LeftStr(S[2], 8);
                Image := S[6];
              end;
            end;
            SpriteList.Add(Sprite);

          end;
      end;

    end;
  end;

  ResetAction := True;
  if OtherPlayer then
    NewAction := StandType
  else
  begin
    if (TMapleChair.IsUse) or (TTamingMob.IsUse) then
      NewAction := 'sit'
    else
      NewAction := StandType;
  end;

  if (InLadder) then
  begin
    case LadderType of
      rtLadder:
        NewAction := 'ladder';
      rtRope:
        NewAction := 'rope';
    end;
  end;

  SameName.Free;
end;

procedure TPlayer.RemoveSprites;
begin
  for var Iter in SpriteList do
    Iter.Dead;
  SpriteList.Clear;
end;

var
  DestX, DestY: Double;

function GetDir(ID: string): string;
begin
  case ID.ToInteger div 10000 of
    0, 1:
      Result := '';
    2, 5:
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
    2, 5:
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
begin
  inherited;

  SpriteList := TList<TAvatarParts>.Create;
  AttackActions := TList<string>.Create;
  AttackOFs := TList<string>.Create;
  Z := 20000;
  Offset.Y := -79;
  Offset.X := -40;
  JumpSpeed := 0.6;
  JumpHeight := 9.5;
  MaxFallSpeed := 8;
  Alpha := 0;
  Tag := 1;
  JumpState := jsFalling;
  StandType := 'stand1';
  WalkType := 'walk1';
  TruncMove := True;
end;

destructor TPlayer.Destroy;
begin

  SpriteList.Free;
  AttackActions.Free;
  AttackOFs.Free;
  if not OtherPlayer then
  begin
    TAvatarParts.ZMap.Free;
   // FreeAndNil(AvatarTargets);
    AvatarEngine.Free;
  end;
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
  AvatarTargetTexture.Clear;
  AvatarTargetTexture.BeginScene;
  GameCanvas.BeginScene;
  TargetEvent;
  GameCanvas.EndScene;
  AvatarTargetTexture.EndScene;

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
            TPet.Pet.X := X;
            TPet.Pet.Y := Y - 50;
            TPet.Pet.JumpState := jsFalling;
          end;

          if TMonsterFamiliar.MonsterFamiliar <> nil then
          begin
            TMonsterFamiliar.MonsterFamiliar.X := X;
            TMonsterFamiliar.MonsterFamiliar.Y := Y - 50;
            TMonsterFamiliar.MonsterFamiliar.JumpState := jsFalling;
          end;

          if AndroidPlayer <> nil then
          begin
            AndroidPlayer.X := Player.x;
            AndroidPlayer.Y := Player.Y;
            AndroidPlayer.JumpState := jsFalling;
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
      if (Action = 'prone') or (Action = 'proneStab') or (not TSkill.PlayEnded) then
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
      if (Action = 'prone') or (Action = 'proneStab') or (not TSkill.PlayEnded) then
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

procedure TPlayer.DoDraw;
var
  WX, WY, NamePos, IDPos: Integer;
begin
  if not OtherPlayer then
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

    WX := Round(MoveX - Engine.WorldX);
    WY := Round(MoveY - Engine.WorldY);
    GameCanvas.Draw(AvatarTargetTexture, WX - 180 - 400, WY - 180 - 400);
  end;
end;

procedure TPlayer.TargetEvent;
begin
  if not OtherPlayer then
  begin
    AvatarEngine.Draw;
    AvatarEngine.Move(1);
    AvatarEngine.Dead;
  end;
end;

function HasEntry(Entry: string): Boolean;
begin
  Result := EquipData.ContainsKey(Entry);
end;

var
  BlinkNum: Integer;

procedure TAvatarParts.UpdateFrame;
var
  Path, AfterImagePath: string;
  BodyDelay, FaceDelay: Integer;
  FaceFrameCount: Integer;
  AdjX: Integer;
  Dir: string;
  Part: TPartName;
  SkillAction: string;
const
  C = 'Character.wz/';
begin
  Part := GetPart(ID);

  if (State = 'stand1') or (State = 'stand2') or (State = 'alert') then
    AnimZigzag := True
  else
    AnimZigzag := False;

  if ((Part = Weapon) or (Part = CashWeapon)) and (FTime = 0) then
  begin
    AfterImagePath := 'Character.wz/Afterimage/' + Owner.AfterImageStr + '.img/0/' + State + '/' +
      IntToStr(Frame) + '/0';
    if HasEntry(AfterImagePath) then
    begin
      PlaySounds('Weapon', 'swordL/Attack');
      TAfterImage.Create(AfterImagePath);
    end;
  end;

  if (Image = 'head') and (FTime = 0) then
    ChangeFrame := True;
  if HasEntry('Character.wz/00002000.img/' + State + '/' + IntToStr(Frame) + '/move') then
  begin
    MoveOffset := EquipData['Character.wz/00002000.img/' + State + '/' + IntToStr(Frame) + '/move'].Vector;
  end
  else
  begin
    if (TMapleChair.IsUse) and (not Owner.OtherPlayer) then
    begin
      MoveOffset := TMapleChair.BodyRelMove;
    end
    else
    begin
      MoveOffset.X := 0;
      MoveOffset.Y := 0;
    end;
  end;
  if MirrorX then
    Owner.MoveX := Round(Owner.X - 1 - MoveOffset.X)
  else
    Owner.MoveX := Round(Owner.X - 1 + MoveOffset.X);

  Owner.MoveY := Round(Owner.Y + MoveOffset.Y);

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
    Image) and (not IsAttack) and (not CharData.ContainsKey(State + '/' + IntToStr(Frame))) then
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
      Visible := False;

    if Owner.ShowHair then
   //   if (Image = 'hairOverHead') or (Image = 'backHair') then
     //   Visible := True;
      if Part = Hair then
        Visible := True;
  end;

  if (Owner.DressCap) and (Owner.ShowHair) then
  begin

    if Part = Hair then
      case Owner.CapType of
        0:
          Visible := True;
        1:
          begin
            if (Image = 'hairOverHead') or (Image = 'backHair') then
              Self.Visible := False;
          end;
        2:
          Visible := False;
      end;
  end;



 // if Part = 'SitTamingMob' then
   // Visible := True;

  if (Image = 'ear') or (Image = 'lefEar') or (Image = 'highlefEar') then
    Alpha := 0;

  if HasEntry(Path + '/z') then
  begin
    if not Owner.OtherPlayer then
      Z := 100 + Owner.Z - ZMap.IndexOf(EquipData[Path + '/z'].Data)
    else
      Z := 100 + (200 * Owner.NewZ) + Owner.Z - ZMap.IndexOf(EquipData[Path + '/z'].Data);
  end;
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

  if (Part = Weapon) and (LeftStr(ID, 4) = '0121') then
  begin
    if (Image = 'weapon') or (Image = 'weapon1') or (Image = 'weapon2') then
      Visible := False;
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

    if OtherPlayer then
    begin
      TamingNavel.X := 0;
      TamingNavel.Y := 0;
    end
    else
    begin
      TamingNavel.X := TTamingMob.Navel.X;
      TamingNavel.Y := TTamingMob.Navel.Y;
    end;

    if HasEntry(Path + '/map/brow') then
    begin
      Brow.X := -EquipData[Path + '/map/brow'].Vector.X * Self.Flip;
      Brow.Y := -EquipData[Path + '/map/brow'].Vector.Y;
      if Image = 'head' then
        HeadBrow := Brow;

      Self.Offset.X := Origin.X + HeadNeck.X - BodyNeck.X - HeadBrow.X + Brow.X - TamingNavel.X;
      Self.Offset.Y := Origin.Y + HeadNeck.Y - BodyNeck.Y - HeadBrow.Y + Brow.Y - TamingNavel.Y;
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

    if Image = 'body' then
      BrowPos := BodyNeck + TTamingMob.Navel;

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

      Self.Offset.X := Origin.X + Navel.X - BodyNavel.X - TamingNavel.X;
      Self.Offset.Y := Origin.Y + Navel.Y - BodyNavel.Y - TamingNavel.Y;
    end;

  end;

end;

function TAvatarParts.IsAttack: Boolean;
begin
  if (LeftStr(State, 4) = 'stab') or (LeftStr(State, 5) = 'swing') or ((LeftStr(State, 5) = 'shoot')) then
    Result := True
  else
    Result := False;
end;

procedure TAvatarParts.DoMove(const Movecount: Single);

  function IsSkillAttack: Boolean;
  begin
    if (CharData.ContainsKey(TSkill.ID + '/action')) and (Owner.Action = CharData[TSkill.ID + '/action']) then
      Result := True
    else
      Result := False;
  end;

  function ArrowKeyDown: Boolean;
  begin
    if (not Keyboard.Key[DIK_LEFT]) and (not Keyboard.Key[DIK_RIGHT]) and (not Keyboard.Key[DIK_UP])
      and (not Keyboard.Key[DIK_DOWN]) then
      Result := False
    else
      Result := True;
  end;

var
  Part: TPartName;
begin
  inherited;

  if GameMode = gmView then
    Exit;
  if TMorph.IsUse then
  begin
    Owner.MoveX := -99999;
    Exit;
  end;

  Owner.FAttack := IsAttack;
  Owner.Action := State;
  Part := GetPart(ID);
  if TTamingMob.IsUse then
  begin
    Owner.FAttack := False;
    if State <> 'fly' then
      Frame := 0;
    if TTamingMob.CharacterAction = 'StabT2' then
      TTamingMob.CharacterAction := 'stabT2';
    if TTamingMob.CharacterAction <> 'hideBody' then
      State := TTamingMob.CharacterAction;
   // if (Part = Weapon) or (Part = CashWeapon) then
     // Exit;
  end;

  if ((Keyboard.Key[DIK_LEFT]) or (Keyboard.Key[DIK_RIGHT])) and (not TTamingMob.IsUse) then
  begin
    if (LeftStr(State, 4) <> 'walk') and (Owner.JumpState = jsNone) and (not Owner.InLadder) and (not
      IsAttack) and (TSkill.PlayEnded) then
    begin
      FTime := 0;
      Frame := 0;

      State := Owner.walktype;
    end;
  end;

  if (Keyboard.KeyReleased[DIK_LEFT]) or (Keyboard.KeyReleased[DIK_RIGHT]) then
    if (not Owner.InLadder) and (Owner.JumpState = jsNone) and (not IsAttack) and (TSkill.PlayEnded) then
    begin
      Frame := 0;
      State := Owner.standtype;
    end;

  if (Owner.JumpState <> jsNone) and (not IsAttack) and (not TTamingMob.IsUse) then
  begin
    Frame := 0;
    State := 'jump';
  end;
  // jump ->re stand
  if (Owner.JumpState = jsNone) and (State = 'jump') and (not Keyboard.Key[DIK_LMENU]) then
    State := Owner.standtype;

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
    if (not IsAttack) and (TSkill.PlayEnded) then
    begin
      if (Keyboard.Key[DIK_DOWN]) and (not Keyboard.Key[DIK_LCONTROL]) and (State <> 'proneStab') then
        State := 'prone';

      if (Keyboard.Key[DIK_LCONTROL]) and (State <> 'proneStab') and (TSkill.PlayEnded) then
      begin
        TSkill.Attacking := False;
        AnimEnd := False;
        Frame := 0;
        FTime := 0;
        State := 'proneStab';
      end;
    end;

    if (Keyboard.KeyReleased[DIK_DOWN]) and (TSkill.PlayEnded) then
      State := Owner.standtype;
  end;

  if (not Owner.InLadder) then
  begin
    if (State = 'rope') or (State = 'ladder') then
    begin
      Frame := 0;
      State := Owner.standtype;
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
      TSkill.Start := False;
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
    State := Owner.standtype;
    AlertCount := 0;
  end;

  if (Keyboard.Key[DIK_LCONTROL]) and (not Keyboard.Key[DIK_DOWN]) and (not IsAttack) and (not Owner.InLadder)
    and (TSkill.PlayEnded) and (not TTamingMob.IsUse) then
  begin
    TSkill.Attacking := False;
    AnimEnd := False;
    Frame := 0;
    FTime := 0;
    State := Owner.AttackAction;
  end;

  if (TSkill.Start) and (not TSkill.PlayEnded) then
  begin
    if CharData.ContainsKey(TSkill.ID + '/action') then
      if State <> CharData[TSkill.ID + '/action'] then
      begin
        AnimEnd := False;
        Frame := 0;
        FTime := 0;
        State := CharData[TSkill.ID + '/action'];
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
  if (not Owner.InLadder) and (not IsAttack) and (TSkill.PlayEnded) then
  begin
    if Keyboard.Key[DIK_LEFT] then
    begin
      MirrorX := False;
      Owner.Flip := False;
    end;
    if Keyboard.Key[DIK_RIGHT] then
    begin
      MirrorX := True;
      Owner.Flip := True;
    end;
  end;

  if Owner.InLadder then
  begin
    if (Keyboard.KeyReleased[DIK_UP]) or (Keyboard.KeyReleased[DIK_DOWN]) then
      Animate := False;
    if (Keyboard.Key[DIK_UP]) or (Keyboard.Key[DIK_DOWN]) then
      Animate := True;
  end
  else
    Animate := True;

  if (TTamingMob.IsUse) or (TMapleChair.IsUse) then
  begin
    if (Part = Weapon) or (Part = CashWeapon) then
      Visible := False;
  end
  else
    Visible := True;

  Player.MirrorX := MirrorX;

  if (State = 'stand1') or (State = 'stand2') or (State = 'alert') or (State = 'sit') then
    TMapleChair.CanUse := True
  else
    TMapleChair.CanUse := False;

  if TMapleChair.IsUse then
  begin
    State := TMapleChair.CharacterAction;
  end;

  UpdateFrame;

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
    inherited DoDraw;

  if Visible then
    Moved := True;

end;

initialization
  PlayerEqpList := TList<string>.Create;
  TPlayer.EquipDumpList := TList<string>.Create;

finalization
  PlayerEqpList.Free;
  TPlayer.EquipDumpList.Free;

end.

