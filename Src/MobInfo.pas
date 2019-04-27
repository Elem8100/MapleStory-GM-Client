unit MobInfo;

interface

uses
  Windows, SysUtils, StrUtils, AsphyreSprite, Generics.Collections,
  WZIMGFile, Global, Tools, WzUtils;

type
  TMobInfo = class
  private
  class var
    TargetIndex: Integer;
    ToName: TDictionary<string, string>;
    Category: TDictionary<string, string>;
  public
    class procedure TargetEvent(Sender: TObject);
    class procedure ReDrawTarget;
    class procedure Create; overload;
  end;

implementation

uses MapleMap, Mob2, AsphyreRenderTargets, WZArchive, AsphyreTypes;

function S1(S: string): string;
begin
  case S[1] of
    'L': Result := '雷';
    'F': Result := '火';
    'I': Result := '冰';
    'S': Result := '毒';
    'D': Result := '暗';
    'P': Result := '物理';
    'H': Result := '聖';
  end;
end;

function S2(S: string): string;
begin
  case S[2] of
    '1': Result := '免疫';
    '2': Result := '抵抗';
    '3': Result := '弱點';
  end;
end;

function ElemName(S: string): string;
var
  A: string;
  I, Num: Integer;
  D: string;
begin
  Result := '';
  Num := Length(S) div 2;
  for I := 1 to Num do
  begin
    A := MidStr(S, I * 2 - 1, 2);
    if I < Num then
      D := '、'
    else
      D := '';
    Result := Result + S1(A) + S2(A) + D;
  end;
end;

class procedure TMobInfo.TargetEvent(Sender: TObject);
var
  I, j: Integer;
  Iter: TWZIMGEntry;
  mData, D, ID, Name, str: string;
  Wz: TWzArchive;
begin
  for I := 0 to TMob.Moblist.Count - 1 do
  begin
    j := 0;
    GameCanvas.FillRect(70 + I * 160 - 3, 8, 140, 250, cRGB1(0, 0, 0, 200));
    GameCanvas.Flush;
    ID := TMob.Moblist[I];
    FontsAlt[1].TextOut('ID: ' + ID, 70 + I * 160, 10, $FFFFFFFF);
    Name := StringWz.GetImgFile('Mob.img').Root.Get(IDToInt(ID) + '/' + 'name', '');
    FontsAlt[1].TextOut('Name: ' + Name, 70 + I * 160, 26, $FFFFFFFF);
    if Mobwz.GetImgFile(ID + '.img') <> nil then
      Wz := Mobwz
    else
      Wz := Mob2WZ;

    for Iter in Wz.GetImgFile(TMob.Moblist[I] + '.img').Root.Get('info').Children do
    begin
      if (Iter.Name = 'category') and (Category.ContainsKey(Iter.Data)) then
        mData := Category[Iter.Data]
      else if (Iter.Name = 'elemAttr') then
        mData := ElemName(Iter.Data)
      else if (Iter.Name = 'boss') or (Iter.Name = 'firstAttack') then
        mData := ''
      else
        mData := Iter.Data;
      if (Iter.Name = 'PDRate') or (Iter.Name = 'MDRate') then
        D := '%'
      else
        D := '';
      if ToName.ContainsKey(Iter.Name) then
      begin
        Inc(j);
        str := ToName[Iter.Name] + mData + D;
        FontsAlt[1].TextOut(str, 70 + I * 160, 26 + j * 15, $FFFFFFFF);
      end;
    end;

  end;
end;

class procedure TMobInfo.ReDrawTarget;
begin
  GameDevice.RenderTo(TargetEvent, 0, True, GameTargetMobInfo[TargetIndex]);
end;

class procedure TMobInfo.Create;
begin
  GameTargetMobInfo := TAsphyreRenderTargets.Create();
  TargetIndex := GameTargetMobInfo.Add(1, 1920, 600, apf_A8R8G8B8, True);
  ToName := TDictionary<string, string>.Create;
  Category := TDictionary<string, string>.Create;
  Category.Add('1', '動物型');
  Category.Add('2', '植物型');
  Category.Add('3', '魚類型');
  Category.Add('4', '爬蟲類型');
  Category.Add('5', '精靈型');
  Category.Add('6', '惡魔型');
  Category.Add('7', '不死型');
  Category.Add('8', '無機物型');

  ToName.Add('level', 'Lv: ');
  ToName.Add('exp', 'Exp: ');
  ToName.Add('maxMP', 'MP: ');
  ToName.Add('maxHP', 'HP: ');
  ToName.Add('speed', 'Speed: ');
  ToName.Add('acc', 'Acc: ');
  ToName.Add('pushed', 'KB: ');
  ToName.Add('category', 'Category: ');
  ToName.Add('eva', 'Eva: ');
  ToName.Add('elemAttr', 'elemAttr: ');
  ToName.Add('MADamage', 'MAD: ');
  ToName.Add('MDDamage', 'MDD: ');
  ToName.Add('PADamage', 'PAD: ');
  ToName.Add('PDDamage', 'PDD: ');
  ToName.Add('PDRate', 'PDrate: ');
  ToName.Add('MDRate', 'MDrate: ');
  ToName.Add('boss', 'Boss');
  ToName.Add('firstAttack', 'firstAttack');
  ToName.Add('charismaEXP', '領導經驗: ');
  ToName.Add('hpRecovery', 'hpRecovery: ');
  ToName.Add('mpRecovery', 'mpRecovery: ');
end;

initialization

finalization

TMobInfo.ToName.Free;
TMobInfo.Category.Free;
FreeAndNil(GameTargetMobInfo);

end.
