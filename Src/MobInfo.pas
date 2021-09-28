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
      Result := '�p';
    'F':
      Result := '��';
    'I':
      Result := '�B';
    'S':
      Result := '�r';
    'D':
      Result := '�t';
    'P':
      Result := '���z';
    'H':
      Result := '�t';
  end;
end;

function S2(S: string): string;
begin
  case S[2] of
    '1':
      Result := '�K��';
    '2':
      Result := '���';
    '3':
      Result := '�z�I';
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
      D := '�B'
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
    //FontsAlt[1].TextOut('�W��: ' + Name, 70 + I * 160, 26, $FFFFFFFF);
    GameFont.Draw(Point2f(70 + I * 160, 26),'�W��: ' + Name,$FFFFFFFF);

  

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
  Category.Add('1', '�ʪ���');
  Category.Add('2', '�Ӫ���');
  Category.Add('3', '������');
  Category.Add('4', '��������');
  Category.Add('5', '���F��');
  Category.Add('6', '�c�]��');
  Category.Add('7', '������');
  Category.Add('8', '�L������');

  ToName.Add('level', '����: ');
  ToName.Add('exp', '�g���: ');
  ToName.Add('maxMP', 'MP: ');
  ToName.Add('maxHP', 'HP: ');
  ToName.Add('speed', '�t��: ');
  ToName.Add('acc', '�R���v: ');
  ToName.Add('pushed', 'KB: ');
  ToName.Add('category', '����: ');
  ToName.Add('eva', '�j�ײv: ');
  ToName.Add('elemAttr', '�ݩ�: ');
  ToName.Add('MADamage', '�]�k����: ');
  ToName.Add('MDDamage', '�]�k���m: ');
  ToName.Add('PADamage', '���z����: ');
  ToName.Add('PDDamage', '���z���m: ');
  ToName.Add('PDRate', '���z���: ');
  ToName.Add('MDRate', '�]�k���: ');
  ToName.Add('boss', 'Boss');
  ToName.Add('firstAttack', '�D�ʧ���');
  ToName.Add('charismaEXP', '��ɸg��: ');
  ToName.Add('hpRecovery', '�C10��HP�^�_: ');
  ToName.Add('mpRecovery', '�C10��MP�^�_: ');
end;

initialization

finalization
  TMobInfo.ToName.Free;
  TMobInfo.Category.Free;

end.

