unit AfterImage;

interface

uses
  Windows, System.Types, SysUtils, StrUtils, PXT.Sprites, Generics.Collections,
  WZIMGFile, Global, MapleCharacter, Mob2, {DamageNumber,} Tools, WzUtils;

type
  TAfterImage = class(TSpriteEx)
  private
    FTime: Integer;
    Frame: Integer;
    ImagePath: string;
    Path: string;
    LT, RB: TPoint;
    Left, Top, Right, Bottom: Integer;
  public
    class var
      ColorInt: Integer;
    procedure DoMove(const Movecount: Single); override;
    procedure DoCollision(const Sprite: TSprite); override;
    class procedure Load(AfterImageStr, ColorType: string);
    class procedure Create(PathName: string); overload;
  end;

implementation

class procedure TAfterImage.Load(AfterImageStr, ColorType: string);
var
  Entry: TWZIMGEntry;
begin
  Entry := GetImgEntry('Character/Afterimage/' + AfterImageStr + '.img/' + ColorType);
  if not EquipData.ContainsKey(Entry.GetPath) then
    DumpData(Entry, EquipData, EquipImages);
end;

class procedure TAfterImage.Create(PathName: string);
begin

  with TAfterImage.Create(SpriteEngine) do
  begin
    ImageLib := EquipImages;
    ImageEntry := EquipData[PathName];
    Visible := False;
    Collisioned := True;
    Path := LeftStr(PathName, Length(PathName) - 1);
  end;

end;

procedure TAfterImage.DoMove(const Movecount: Single);
var
  Delay, a1: Integer;
  AniAlpha: Single;
const
  c = 'Character/Afterimage/';
begin
  inherited;
  Visible := True;
  X := Player.X - 10;
  Y := Player.Y;
  Z := 150 + Player.Z;
  ImagePath := Path + Frame.ToString;
  ImageEntry := EquipData[ImagePath];

  Delay := ImageEntry.Get('delay', 100);
  a1 := ImageEntry.Get('a1', '-1');
  MirrorX := Player.Flip;

  if EquipData.ContainsKey(c + Player.AfterImageStr + '.img/0/' + Player.Action + '/lt') then
  begin
    LT := EquipData[c + Player.AfterImageStr + '.img/0/' + Player.Action + '/lt'].Vector;
    RB := EquipData[c + Player.AfterImageStr + '.img/0/' + Player.Action + '/rb'].Vector;
    case Player.Flip of
      True:
        begin
          Right := Round(X) - LT.X + 18;
          Left := Round(X) - RB.X;
          ;
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
  Collision;

  FTime := FTime + 17;
  if FTime > Delay then
  begin
    Frame := Frame + 1;
    if not EquipData.ContainsKey(Path + Frame.ToString) then
      Dead;
    FTime := 0;
  end;

  if a1 <> -1 then
  begin
    AniAlpha := 255 - (255 - a1) * FTime / Delay;
    if (AniAlpha < 255) and (AniAlpha > 0) then
      Alpha := Trunc(AniAlpha);
    if Alpha <= 20 then
      Dead;
  end;

  case MirrorX of
    True:
      Offset.X := ImageEntry.Get('origin').Vector.X - ImageWidth + 18;
    False:
      Offset.X := -ImageEntry.Get('origin').Vector.X;
  end;
  Offset.Y := -ImageEntry.Get('origin').Vector.Y;

end;

procedure TAfterImage.DoCollision(const Sprite: TSprite);
begin
  if (FTime = 0) and (Frame = 0) then
    if Sprite is TMob then
    begin
      with TMob(Sprite) do
      begin
        if HP > 0 then
        begin
          Hit := True;
          Damage := 50000 + Random(700000);
          HP := HP - Damage;
          if HasImgEntry('Sound/Mob.img/' + SelfID + '/Damage') then
            PlaySounds('Mob', SelfID + '/Damage')
          else if HasImgEntry('Sound/Mob.img/' + SelfID + '/Hit1') then
            PlaySounds('Mob', SelfID + '/Hit1');
               // if can pushed
          if WzData.ContainsKey('Mob/' + SelfID + '.img/hit1') or WzData.ContainsKey('Mob/' + SelfID + '.img/hit1') then
            GetHit1 := True;

        end;
        if (HP <= 0) and (not Die) then
        begin
          PlaySounds('Mob', SelfID + '/Die');
          Die := True;
          Collisioned := False;
          // Dead;
        end;

      end;

      Collisioned := False;
    end;

end;

end.

