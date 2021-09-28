unit MobInfo;

interface

uses
  Windows, SysUtils, StrUtils,  Generics.Collections, WZIMGFile, Global, Tools,
  WzUtils;

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

uses
  MapleMap, Mob2,  WZArchive, PXT.Types,PXT.TypesEx;

function S1(S: string): string;
begin
  case S[1] of
    'L':
      Result := '雷';
    'F':
      Result := '火';
    'I':
      Result := '冰';
    'S':
      Result := '毒';
    'D':
      Result := '暗';
    'P':
      Result := '物理';
    'H':
      Result := '聖';
  end;
end;

function S2(S: string): string;
begin
  case S[2] of
    '1':
      Result := '免疫';
    '2':
      Result := '抵抗';
    '3':
      Result := '弱點';
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
  Wz: TWZArchive;
begin
  for I := 0 to TMob.Moblist.Count - 1 do
  begin
    j := 0;
    GameCanvas.FillRect(FloatRect(70 + I * 160 - 3, 8, 140, 250), cRGB1(0, 0, 0, 200));
    GameCanvas.Flush;
    ID := TMob.Moblist[I];
    //FontsAlt[1].TextOut('ID: ' + ID, 70 + I * 160, 10, $FFFFFFFF);
    GameFont.Draw(Point2f(70 + I * 160, 10),'ID: ' + ID,$FFFFFFFF);
    Name := GetImgFile('String/Mob.img').Root.Get(IDToInt(ID) + '/' + 'name', '');
    //FontsAlt[1].TextOut('名稱: ' + Name, 70 + I * 160, 26, $FFFFFFFF);
    GameFont.Draw(Point2f(70 + I * 160, 26),'名稱: ' + Name,$FFFFFFFF);

  

    for Iter in GetImgFile('Mob/'+TMob.Moblist[I] + '.img').Root.Get('info').Children do
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
        //FontsAlt[1].TextOut(str, 70 + I * 160, 26 + j * 15, $FFFFFFFF);
        GameFont.Draw(Point2f(70 + I * 160, 26+j),str,$FFFFFFFF);
      end;
    end;

  end;
end;

class procedure TMobInfo.ReDrawTarget;
begin
  //GameDevice.RenderTo(TargetEvent, 0, True, GameTargetMobInfo[TargetIndex]);
end;

class procedure TMobInfo.Create;
begin


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

  ToName.Add('level', '等級: ');
  ToName.Add('exp', '經驗值: ');
  ToName.Add('maxMP', 'MP: ');
  ToName.Add('maxHP', 'HP: ');
  ToName.Add('speed', '速度: ');
  ToName.Add('acc', '命中率: ');
  ToName.Add('pushed', 'KB: ');
  ToName.Add('category', '分類: ');
  ToName.Add('eva', '迴避率: ');
  ToName.Add('elemAttr', '屬性: ');
  ToName.Add('MADamage', '魔法攻擊: ');
  ToName.Add('MDDamage', '魔法防禦: ');
  ToName.Add('PADamage', '物理攻擊: ');
  ToName.Add('PDDamage', '物理防禦: ');
  ToName.Add('PDRate', '物理減傷: ');
  ToName.Add('MDRate', '魔法減傷: ');
  ToName.Add('boss', 'Boss');
  ToName.Add('firstAttack', '主動攻擊');
  ToName.Add('charismaEXP', '領導經驗: ');
  ToName.Add('hpRecovery', '每10秒HP回復: ');
  ToName.Add('mpRecovery', '每10秒MP回復: ');
end;

initialization

finalization
  TMobInfo.ToName.Free;
  TMobInfo.Category.Free;

end.

