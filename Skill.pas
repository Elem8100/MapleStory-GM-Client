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
      LoadedList:TList<string>;
      PlayEnded: Boolean;
      Start: Boolean;
    class procedure Load(ID: string);
    procedure DoMove(const Movecount: Single); override;
  end;

  TSkillSprite = class(TSpriteEx)
  private
    FTime: Integer;
    Frame: Integer;
    ID: string;
    BallSpeed: Integer;
    EffectName: string;
    ParentPath: string;
    Counter: Integer;
    AnimRepeat, AnimEnd: Boolean;
    Left, Top, Right, Bottom: Integer;
  public
    procedure DoMove(const Movecount: Single); override;
    procedure DoCollision(const Sprite: TSprite); override;
  end;

procedure CreateSKill(AID: string; X, Y: Integer);

implementation

uses
  MainUnit, MapleCharacter;

function GetJobID(ID: string): string;
begin
  Result := IntToStr(StrToInt(ID) div 10000);
end;

function IsSkillAttack: Boolean;
begin
  if (CharData.ContainsKey(SkillID + '/action')) and (FState = CharData[SkillID + '/action']) then
    Result := True
  else
    Result := False;
end;

procedure TSkill.DoMove(const Movecount: Single);
begin
  if (TSkill.PlayEnded) {and (not IsSkillAttack)}    and (not Player.InLadder) {and (player.sTime = 0)} then
  begin
    for var KeyName in TSkill.HotKeyList.Keys do
      if Keyboard.Key[KeyName] then
        createskill(TSkill.hotkeylist[KeyName], 0, 0);

  end;

end;

class procedure TSkill.Load(ID: string);
var
  Iter, Entry: TWZIMGEntry;
begin
  Entry := GetImgEntry('Skill/' + GetJobID(ID) + '.img/skill/' + ID);
  DumpData(Entry, EquipData, EquipImages);
  for Iter in Entry.Children do
  begin
    if Iter.Name = 'action' then
      CharData.AddOrSetValue(ID + '/action', Iter.Get('0', 'alert'));

    if Iter.Name = 'tile' then
      CharData.Add(ID + '/tileCount', Iter.Children.Count - 1);
  end;

end;

procedure CreateSKill(AID: string; X, Y: Integer);
var
  Entry: TWZIMGEntry;
  I: Integer;
  WX, WY, MoveY, Rnd: Integer;
  Below: TPoint;
  BelowFH: TFoothold;
const
  Effects: array[0..6] of string = ('effect', 'effect0', 'effect1', 'effect2', 'effect3', 'screen', 'ball');
begin
  SkillID := AID;
  TSkill.PlayEnded := False;
  PlaySounds('Skill', AID + '/Use');
  Entry := GetImgEntry('Skill/' + GetJobID(AID) + '.img/skill/' + AID);
  for I := 0 to 6 do
  begin
    if Entry.Get(Effects[I]) <> nil then
      with TSkillSprite.Create(SpriteEngine) do
      begin
        ParentPath := GetEntryPath(Entry.Get(Effects[I]));
        if Entry.Get(Effects[I] + '/0/0') <> nil then
          ParentPath := GetEntryPath(Entry.Get(Effects[I] + '/0'));

        ImageLib := EquipImages;
        ImageEntry := EquipData[ParentPath + '/0'];
        X := Player.X;
        Y := Player.Y;
        if Effects[I] = 'ball' then
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
        ID := AID;
        EffectName := Effects[I];

        if EffectName = 'effect' then
          Z := 150 + Player.Z
        else
          Z := Player.Z - 150;

        // Collisioned := True;
      end;
  end;

  WX := Trunc(SpriteEngine.WorldX);
  WY := Trunc(SpriteEngine.WorldY);

  if Entry.Get('tile') <> nil then // ' + '/0/0') then
  begin
    for I := 0 to 6 do
    begin
      MoveY := 20;
      if (I mod 2) = 0 then
        MoveY := 300;

      Below := TFootholdTree.This.FindBelow(Point(40 + WX + I * 120, WY + MoveY), BelowFH);
      if BelowFH <> nil then
        with TSkillSprite.Create(SpriteEngine) do
        begin
          Rnd := CharData[AID + '/tileCount'];
          ImageLib := EquipImages;
          ParentPath := GetEntryPath(Entry.Get('tile/' + IntToStr(Random(Rnd))));
          ImageEntry := EquipData[ParentPath + '/0'];
          Width := 400;
          Height := 400;
          X := Below.X;
          Y := Below.Y;
          Z := 20000;
          Visible := False;
          ID := AID;
          EffectName := 'tile';
          // Collisioned := True;
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

  ImageEntry := EquipData[ParentPath + '/' + IntToStr(Frame)];
  AnimDelay := ImageEntry.Get('delay', 100);

  MirrorX := CharFlip;

  FTime := FTime + 17;
  if FTime > AnimDelay then
  begin
    FTime := 0;
    Frame := Frame + 1;
    AnimEnd := False;
    if not EquipData.ContainsKey(ParentPath + '/' + IntToStr(Frame)) then
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

  case MirrorX of
    True:
      Offset.X := ImageEntry.Get('origin').Vector.X - ImageWidth;
    False:
      Offset.X := -ImageEntry.Get('origin').Vector.X;
  end;
  Offset.Y := -ImageEntry.Get('origin').Vector.Y;

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

procedure TSkillSprite.DoCollision(const Sprite: TSprite);
begin
  {
    if (FTime=0) and (Frame=0) then

    if Sprite is TMob then
    begin
    with TMob(Sprite) do
    begin
    if HP > 0 then
    begin
    Hit := True;
    Damage := 5000 + Random(7000);
    HP := HP - Damage;
    PlaySounds('Mob',SelfID + '/Damage');
    //if can pushed
    if WZData.ContainsKey(SelfID + '/hit1') then
    GetHit1 := True;

    end;
    if (HP <=0) and (not Die) then
    begin
    PlaySounds('Mob', SelfID + '/Die');
    Die := True;
    Collisioned := False;
    //Dead;
    end;

    end;

    Collisioned := False;
    end;
  }
end;

initialization
  TSkill.HotKeyList := TDictionary<Cardinal, string>.Create;
  TSkill.LoadedList:=TList<string>.Create;

  TSkill.PlayEnded := True;

finalization
  TSkill.HotKeyList.Free;
  TSkill.LoadedList.Free;
end.

