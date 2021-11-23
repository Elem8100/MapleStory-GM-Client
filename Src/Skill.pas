unit Skill;

interface

uses
  Windows, System.Types, SysUtils, StrUtils, PXT.Sprites, Generics.Collections,
  WZIMGFile, Global, DamageNumber, Footholds, Tools, WzUtils;

type
  TSkill = class(TSprite)
  public
    class var
      HotKeyList: TDictionary<Cardinal, string>;
      AllIDs: TDictionary<string, string>;
      LoadedList: TList<string>;
      PlayEnded: Boolean;
      Start: Boolean;
      Entry: TWZIMGEntry;
      ID: string;
      MultiStrike: Boolean;
      DamageWaitTime: Integer;
      TotalTime: Integer;
      Attacking: Boolean;
    class procedure Load(ID: string);
    class procedure Create(ID: string); overload;
    procedure DoMove(const Movecount: Single); override;
  end;

  //only play animation
  TSkillSprite = class(TSpriteEx)
  private
    FTime: Integer;
    Frame: Integer;
    Origin: TPoint;
    FID: string;
    BallSpeed: Single;
    EffectName: string;
    ParentPath: string;
    Counter: Integer;
    AnimRepeat, AnimEnd: Boolean;
    Left, Top, Right, Bottom: Integer;
    MoveWithPlayer: Boolean;
  public
    procedure DoMove(const Movecount: Single); override;
  end;

  // collision wit Mob
  TSkillCollision = class(TSpriteEx)
    ID: string;
    IDEntry: TWZIMGEntry;
    LT, RB: TPoint;
    Left, Top, Right, Bottom: Integer;
    Counter: Integer;
    Num: Integer;
    StartTime: Integer;
    procedure DoMove(const Movecount: Single); override;
    procedure DoCollision(const Sprite: TSprite); override;
  end;

implementation

uses
  MapleCharacter, Mob2, MapleMap;

function IsSkillAttack: Boolean;
begin
  if (CharData.ContainsKey(TSkill.ID + '/action')) and (Player.Action = CharData[TSkill.ID + '/action']) then
    Result := True
  else
    Result := False;
end;

procedure TSkill.DoMove(const Movecount: Single);
begin
  if (TSkill.PlayEnded) {and (not IsSkillAttack)}                 and (not
    Player.InLadder) {and (player.sTime = 0)} then
  begin
    for var KeyName in TSkill.HotKeyList.Keys do
      if Keyboard.Key[KeyName] then
      begin
        TSkill.Attacking := True;
        TSkill.Create(TSkill.hotkeylist[KeyName]);
      end;
  end;

end;

class procedure TSkill.Load(ID: string);
var
  Iter, Entry: TWZIMGEntry;
begin
  if HasImgFile('Skill/' + GetJobImg(ID) + '.img') then
    Entry := GetImgEntry('Skill/' + GetJobImg(ID) + '.img/skill/' + ID);

  DumpData(Entry, EquipData, EquipImages);
  for Iter in Entry.Children do
  begin
    if Iter.Name = 'action' then
      CharData.AddOrSetValue(ID + '/action', Iter.Get('0', 'alert'));

    if Iter.Name = 'tile' then
      CharData.Add(ID + '/tileCount', Iter.Children.Count - 1);
  end;

end;

function GetDamageWaitTime(ID: string): Integer;
begin
  case ID.ToInteger of

    1311012, 4341052, 5101004, 65121008, 400040006:
      Result := 5;

    61111101, 142121031, 400041024, 5221026, 101120202:
      Result := 10;

    1321012, 2211010, 2301005, 15111022, 25121005, 101100100, 101110202, 101110203, 155111211, 400051042:
      Result := 15;

    3121015, 11101008, 31221002, 41121017:
      Result := 20;

    15121002, 1121008, 1001005, 1121015, 2211002, 5121017, 61121104, 61121105, 65111100, 41121018, 65121100:
      Result := 25;

    5321000, 25121007, 400041021:
      Result := 30;

    2221007, 65121002:
      Result := 35;

    5121016, 5121052, 15121052, 101120102, 36121011, 400021002:
      Result := 40;

    5221052, 27111101, 32121004:
      Result := 45;

    27111303, 27121202:
      result := 50;

    31221052:
      Result := 55;

    2321008, 2121007, 41121052, 400001014:
      Result := 70;

    61121052:
      Result := 85;

    5121013:
      Result := 95;

    4341011:
      Result := 120;

    36121052:
      Result := 160;
  else
    Result := 10;
  end;

end;

class procedure TSkill.Create(ID: string);
var
  Entry: TWZIMGEntry;
  WX, WY, MoveY, Rnd: Integer;
  Below: TPoint;
  BelowFH: TFoothold;
const
  Effects: array[0..10] of string = ('effect', 'effect0', 'effect1', 'effect2',
    'effect3', 'screen', 'screen0', 'ball', 'keydown', 'keydown0', 'keydowned');
begin
  Randomize;
  TSkill.ID := ID;
  TSkill.PlayEnded := False;
  PlaySounds('Skill', ID + '/Use');
 // Entry := GetImgEntry('Skill/' + GetJobID(AID) + '.img/skill/' + AID);
  if HasImgFile('Skill/' + GetJobImg(ID) + '.img') then
    Entry := GetImgEntry('Skill/' + GetJobImg(ID) + '.img/skill/' + ID);

  TSkill.Entry := Entry;

  case ID.ToInteger of
    1121008, 1001005, 1311012:
      TSkill.MultiStrike := False;
  else
    TSkill.MultiStrike := True;
  end;

  var Count: Integer;
  DamageWaitTime := GetDamageWaitTime(ID);
  //�s��6��
  if TSkill.MultiStrike then
  begin
    Count := 1;
    TotalTime := DamageWaitTime + 6 * 7;
    for var Iter in SpriteEngine.SpriteList do
      if Iter is TMob then
      begin
        for var i := 1 to 6 do
        begin
          TMob(Iter).MobCollision[i] := TMobCollision.Create(SpriteEngine);
          TMob(Iter).MobCollision[i].ImageLib := EquipImages;
          TMob(Iter).MobCollision[i].Owner := TMob(Iter);
          TMob(Iter).MobCollision[i].StartTime := DamageWaitTime + i * 7;
          TMob(Iter).MobCollision[i].Index := i - 1;
          TMob(Iter).MobCollision[i].Collisioned := True;
        end;
      end;
  end
  else
    Count := 6;

  //���ǥ�
  for var i := 1 to Count do
  begin
    with TSkillCollision.Create(SpriteEngine) do
    begin
      ImageLib := EquipImages;
      Collisioned := True;
      //ID := ID;
      IDEntry := Entry;
      StartTime := DamageWaitTime + i * 6;
    end;
  end;

  for var Iter in SpriteEngine.SpriteList do
    if Iter is TMob then
      Iter.Collisioned := True;

  for var i := 0 to 10 do
  begin
    if Entry.Get(Effects[i] + '/0') = nil then
    begin
      Exit;

    end;
    if Entry.Get(Effects[i]) <> nil then
      with TSkillSprite.Create(SpriteEngine) do
      begin
        MoveWithPlayer := True;
        ParentPath := GetEntryPath(Entry.Get(Effects[i]));
        if Entry.Get(Effects[i] + '/0/0') <> nil then
          ParentPath := GetEntryPath(Entry.Get(Effects[i] + '/0'));

        ImageLib := EquipImages;
        ImageEntry := EquipData[ParentPath + '/0'];
        X := Player.X;
        Y := Player.Y;
        if Effects[i] = 'ball' then
        begin
          if Entry.Get('common/bulletSpeed') <> nil then
            BallSpeed := 1000 / Integer(Entry.Get('common/bulletSpeed').Data);

          Y := Player.Y - 27;
          if Player.Flip then
            BallSpeed := BallSpeed
          else
            BallSpeed := -BallSpeed;
          AnimRepeat := True;
        end;
        Width := 800;
        Height := 800;
        Visible := False;
        FID := ID;
        EffectName := Effects[i];

        if Entry.Get(Effects[i]).Get('z', '-999') = '-999' then
          Z := 150 + Player.Z
        else
          Z := Player.Z + Entry.Get(Effects[i]).Get('z', 0);

      end;
  end;

  WX := Trunc(SpriteEngine.WorldX);
  WY := Trunc(SpriteEngine.WorldY);

  if Entry.Get('tile') <> nil then // ' + '/0/0') then
  begin
    if Entry.Get('tile/0') = nil then
      Exit;
    if Entry.Get('tile/0/0') = nil then
      Exit;
    for var i := 0 to 6 do
    begin
      MoveY := 20;
      if (i mod 2) = 0 then
        MoveY := 300;

      Below := TFootholdTree.This.FindBelow(Point(240 + WX + i * 120, WY + MoveY), BelowFH);
      if BelowFH <> nil then
        with TSkillSprite.Create(SpriteEngine) do
        begin
          FID := ID;
          MoveWithPlayer := False;
          Rnd := CharData[ID + '/tileCount'];
          ImageLib := EquipImages;
          ParentPath := GetEntryPath(Entry.Get('tile/' + IntToStr(Random(Rnd))));
          ImageEntry := EquipData[ParentPath + '/0'];
          Width := 400;
          Height := 400;
          X := Below.X;
          Y := Below.Y;
          Z := Player.Z;
          Visible := False;
          EffectName := 'tile';
        end;

    end;

  end;

end;

procedure TSkillSprite.DoMove(const Movecount: Single);
var
  AnimDelay: Integer;
begin

  inherited;
  Visible := True;
  if (FTime = 0) and (Frame = 0) and (not TSkill.PlayEnded) then
    TSkill.Start := True;

  ImageEntry := EquipData[ParentPath + '/' + Frame.ToString];
  AnimDelay := ImageEntry.Get('delay', 100);

  MirrorX := Player.Flip;
  if (MoveWithPlayer) and (EffectName <> 'ball') then
  begin
    X := Player.X;
    Y := Player.Y;
  end;
  FTime := FTime + 17;
  if FTime > AnimDelay then
  begin
    FTime := 0;
    Frame := Frame + 1;
    AnimEnd := False;
    if not EquipData.ContainsKey(ParentPath + '/' + Frame.ToString) then
    begin
      // if Frame> FrameCount then
      // Frame := 0;
      if AnimRepeat then
        Frame := 0
      else
      begin
        Frame := Frame - 1;
        AnimEnd := True;
      end;
    end;
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

  if EffectName <> 'ball' then
  begin
    TSkill.PlayEnded := AnimEnd;
    if AnimEnd then
      Dead;
  end;

  if EffectName = 'ball' then
  begin
    Inc(Counter);
    if Counter > 15 then
      X := X + BallSpeed;
    if Counter > 180 then
      Dead;
  end;

end;

procedure TSkillCollision.DoMove(const Movecount: Single);
begin
  inherited;
  MirrorX := Player.Flip;

  if EquipData.ContainsKey(IDEntry.GetPath + '/common/lt') then
  begin
    LT := EquipData[IDEntry.GetPath + '/common/lt'].Vector;
    RB := EquipData[IDEntry.GetPath + '/common/rb'].Vector;
    case Player.Flip of
      True:
        begin
          Right := Round(Player.X) - LT.X + 18;
          Left := Round(Player.X) - RB.X;

        end;
      False:
        begin
          Left := Round(Player.X) + LT.X;
          Right := Round(Player.X) + RB.X;
        end;
    end;
    Top := Round(Player.Y) + LT.Y;
    Bottom := Round(Player.Y) + RB.Y;
  end;
  CollideRect := Rect(Left, Top, Right, Bottom);
  Inc(Counter);
  if TSkill.MultiStrike then
  begin
    if Counter > TSkill.TotalTime + 1 then
      Dead;
  end
  else
  begin
    if Counter > StartTime then
    begin
      Collision;
      Dead;
    end;
  end;

end;

procedure TSkillCollision.DoCollision(const Sprite: TSprite);
begin
  if TSkill.MultiStrike then
    Exit;

  if Sprite is TMob then
  begin
    with TMob(Sprite) do
    begin
      Collisioned := False;
      if HP > 0 then
      begin
        Hit := True;
        Damage := 500000 + Random(700000);
        HP := HP - Damage;
        if HasImgEntry('Sound/Mob.img/' + SelfID + '/Damage') then
          PlaySounds('Mob', SelfID + '/Damage')
        else if HasImgEntry('Sound/Mob.img/' + SelfID + '/Hit1') then
          PlaySounds('Mob', SelfID + '/Hit1');
               // if can pushed
        if WzData.ContainsKey('Mob/' + SelfID + '.img/hit1') then
          GetHit1 := True;

      end;
      if (HP <= 0) and (not Die) then
      begin
        PlaySounds('Mob', SelfID + '/Die');
        Die := True;
        //  Collisioned := False;
          // Dead;
      end;

    end;

    Collisioned := False;
    Dead;
  end;

end;

initialization
  TSkill.HotKeyList := TDictionary<Cardinal, string>.Create;
  TSkill.LoadedList := TList<string>.Create;

  TSkill.PlayEnded := True;


finalization
  TSkill.HotKeyList.Free;
  TSkill.LoadedList.Free;

end.

