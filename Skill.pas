unit Skill;

interface

uses
  Windows, System.Types, SysUtils, StrUtils, AsphyreSprite, Generics.Collections, WZIMGFile, Global,
  DamageNumber, Footholds, Tools, WzUtils;

type
  TSkill = class(TSprite)
  public
    class var
      HotKeyList: TDictionary<Cardinal, string>;
      LoadedList: TList<string>;
      PlayEnded: Boolean;
      Start: Boolean;
      Has001Wz: Boolean;
      Entry: TWZIMGEntry;
      ID: string;
      MultiStrike: Boolean;
      DamageWaitTime:Integer;
      TotalTime:Integer;
      Attacking:Boolean;
    class procedure Load(ID: string);
    class procedure Create(ID: string); overload;
    procedure DoMove(const Movecount: Single); override;
  end;

  //only play animation
  TSkillSprite = class(TSpriteEx)
  private
    FTime: Integer;
    Frame: Integer;
    Origin:TPoint;
    FID: string;
    BallSpeed: Integer;
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
  MainUnit, MapleCharacter, Mob2;

function GetJobID(ID: string): string;
begin
  Result := IntToStr(StrToInt(ID) div 10000);
end;

function IsSkillAttack: Boolean;
begin
  if (CharData.ContainsKey(TSkill.ID + '/action')) and (FState = CharData[TSkill.ID + '/action']) then
    Result := True
  else
    Result := False;
end;

procedure TSkill.DoMove(const Movecount: Single);
begin
  if (TSkill.PlayEnded) {and (not IsSkillAttack)}     and (not Player.InLadder) {and (player.sTime = 0)} then
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
  if HasImgFile('Skill.wz/' + GetJobID(ID) + '.img') then
    Entry := GetImgEntry('Skill.wz/' + GetJobID(ID) + '.img/skill/' + ID)
  else
    Entry := GetImgEntry('Skill001.wz/' + GetJobID(ID) + '.img/skill/' + ID);

  DumpData(Entry, EquipData, EquipImages);
  for Iter in Entry.Children do
  begin
    if Iter.Name = 'action' then
      CharData.AddOrSetValue(ID + '/action', Iter.Get('0', 'alert'));

    if Iter.Name = 'tile' then
      CharData.Add(ID + '/tileCount', Iter.Children.Count - 1);
  end;

end;

function GetDamageWaitTime(ID:string):Integer;
begin
  case ID.ToInteger of
     1121008:Result:=25;
     2321008: Result:=70;
     36121052:Result:=160;
     36121011:Result:=40;
  end;

end;

class procedure TSkill.Create(ID: string);
var
  Entry: TWZIMGEntry;
  WX, WY, MoveY, Rnd: Integer;
  Below: TPoint;
  BelowFH: TFoothold;
const
  Effects: array[0..6] of string = ('effect', 'effect0', 'effect1', 'effect2', 'effect3', 'screen', 'ball');
begin
  Randomize;
  TSkill.ID := ID;
  TSkill.PlayEnded := False;
  PlaySounds('Skill', ID + '/Use');
 // Entry := GetImgEntry('Skill/' + GetJobID(AID) + '.img/skill/' + AID);
  if HasImgFile('Skill.wz/' + GetJobID(ID) + '.img') then
    Entry := GetImgEntry('Skill.wz/' + GetJobID(ID) + '.img/skill/' + ID)
  else
    Entry := GetImgEntry('Skill001.wz/' + GetJobID(ID) + '.img/skill/' + ID);
  TSkill.Entry := Entry;
  TSkill.MultiStrike := True;

  var Count: Integer;
  DamageWaitTime := GetDamageWaitTime(ID);
  //連打6次
  if TSkill.MultiStrike then
  begin
    Count := 1;
    TotalTime:=  DamageWaitTime +6*7;
    for var Iter in SpriteEngine.SpriteList do
      if Iter is TMob then
      begin
        for var i := 1 to 6 do
        begin
          TMob(Iter).MobCollision[i] := TMobCollision.Create(SpriteEngine);
          TMob(Iter).MobCollision[i].ImageLib := EquipImages;
          TMob(Iter).MobCollision[i].Owner := TMob(Iter);
          TMob(Iter).MobCollision[i].StartTime :=  DamageWaitTime + i * 7;
          TMob(Iter).MobCollision[i].Index := i - 1;
          TMob(Iter).MobCollision[i].Collisioned := True;
        end;
      end;
  end
  else
    Count := 6;

  //順序打
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

  for var i := 0 to 6 do
  begin
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
          Y := Player.Y - 27;
          if CharFlip then
            BallSpeed := 5
          else
            BallSpeed := -5;
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

  MirrorX := CharFlip;
  if MoveWithPlayer then
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
    if Counter > 80 then
      Dead;
  end;

end;

procedure TSkillCollision.DoMove(const Movecount: Single);
begin
  inherited;
  MirrorX := CharFlip;

  if EquipData.ContainsKey(IDEntry.GetPath + '/common/lt') then
  begin
    LT := EquipData[IDEntry.GetPath + '/common/lt'].Vector;
    RB := EquipData[IDEntry.GetPath + '/common/rb'].Vector;
    case CharFlip of
      True:
        begin
          Right := Round(Player.X) - LT.X + 18;
          Left := Round(Player.X) - RB.X;
          ;
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
    if Counter > TSkill.TotalTime+1 then
      Dead;
  end
  else
  begin
    if Counter >  StartTime then
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
        if HasImgEntry('Sound.wz/Mob.img/' + SelfID + '/Damage') then
          PlaySounds('Mob', SelfID + '/Damage')
        else if HasImgEntry('Sound.wz/Mob.img/' + SelfID + '/Hit1') then
          PlaySounds('Mob', SelfID + '/Hit1');
               // if can pushed
        if WzData.ContainsKey('Mob.wz/' + SelfID + '.img/hit1') or WzData.ContainsKey('Mob2.wz/' +
          SelfID + '.img/hit1') then
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

