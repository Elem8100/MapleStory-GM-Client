unit DamageNumber;

interface

uses
  Windows, SysUtils, StrUtils, PXT.Sprites, Generics.Collections, WZIMGFile, Classes, Global,
  WzUtils;

type
  TDamageNumber = class(TSpriteEx)
  public
    Number: Integer;
    Counter: Integer;
    Alpha: Integer;
    class var
      Style: string;
      UseNewDamage: Boolean;
    procedure DoDraw; override;
    procedure DoMove(const Movecount: Single); override;
    class procedure Load(Num: string);
    class procedure Create(ANumber, AX, AY: Integer); overload;
  end;

implementation

uses
  MapleCharacter, MapleMap,PXT.Canvas,PXT.Graphics;

procedure TDamageNumber.DoDraw;
var
  I: Integer;
  Char: string;
begin
  for I := 1 to Length(Number.ToString) do
  begin
    Char := MidStr(Number.ToString, I, 1);
    if UseNewDamage then
    begin
       //style='1/NoRed1'
      if EquipData.ContainsKey('Effect/DamageSkin.img/'+ Style + '/' + Char) then
        ImageEntry := EquipData['Effect/DamageSkin.img/' + Style + '/' + Char]
      else
        ImageEntry := EquipData['Effect/BasicEff.img/damageSkin/' + Style + '/' + Char];
    end
    else
      ImageEntry := EquipData['Effect/BasicEff.img/' + Style + '/' + Char];
    GameCanvas.DrawColor1(EquipImages[ImageEntry], X + I * 29 - ImageEntry.Get('origin').Vector.X - Engine.WorldX,
      Y - ImageEntry.Get('origin').Vector.Y - Engine.WorldY,  False,ARGB(Alpha,255,255,255));
  end;
end;

procedure TDamageNumber.DoMove(const Movecount: Single);
begin
  inherited;
  Y := Y - 0.5;
  Inc(Counter);
  if Counter > 30 then
    Alpha := Alpha - 6;
  if Alpha < 10 then
    Dead;
end;

class procedure TDamageNumber.Load(Num: string);
const
  StyleList: array[0..7] of string = ('NoBlue0', 'NoBlue1', 'NoCri0', 'NoCri1', 'NoRed0', 'NoRed1',
    'NoViolet0', 'NoViolet1');
begin
  var Entry := GetImgEntry('Effect/BasicEff.img/');
  if UseNewDamage then
  begin
    //num=1/NoRed1
    if GetImgEntry('Effect/DamageSkin.img/' + Num) <> nil then
      DumpData(GetImgEntry('Effect/DamageSkin.img/' + Num), EquipData, EquipImages)
    else
      DumpData(Entry.Get2('damageSkin/' + Num), EquipData, EquipImages);
  end
  else
  begin
    for var I := 0 to 7 do
      DumpData(Entry.Child[StyleList[I]], EquipData, EquipImages);
  end;
end;

class procedure TDamageNumber.Create(ANumber, AX, AY: Integer);
var
  Len, Mid: Integer;
begin
  Len := Length(ANumber.ToString);
  Mid := (Len * 28) div 2;
  with TDamageNumber.Create(SpriteEngine) do
  begin
    Number := ANumber;
    X := AX - Mid;
    Y := AY;
    Z := Player.Z;
    Alpha := 255;
   // Style := AStyle;
  end;
end;

end.

