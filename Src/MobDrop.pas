unit MobDrop;

interface

uses
  Windows, SysUtils, StrUtils, AsphyreSprite, Generics.Collections,
  WZIMGFile, Classes, Global, Footholds, Tools, WzUtils,System.Types;

type
  TMobDrop = class(TJumperSprite)
  private
    class var Value: Single;
  public
    FH: TFoothold;
    Number: Integer;
    Style: string;
    TimeCount: Integer;
    Frame: Integer;
    FTime: Integer;
    MoveX: Single;
    CosY: Single;
    ParentPath: string;
    ID: string;
    constructor Create(const AParent: TSprite); override;
    procedure DoMove(const Movecount: Single); override;
    class procedure Drop(AX, AY: Integer; Num: Integer; DropList: Tlist<string>); overload;
  end;

implementation
        uses MapleCharacter;
constructor TMobDrop.Create(const AParent: TSprite);
begin
  inherited;
  ImageLib := EquipImages;
  DoJump := True;
  JumpSpeed := 0.3;
  JumpHeight := 9;
  MaxFallSpeed := 8;
  // DrawMode:=1;
  CosY := Random(100);
end;

function GetItemDir(ID: string): string;
begin
  case StrToInt(ID) div 1000000 of
    5: Result := 'Cash';
    2: Result := 'Consume';
    4: Result := 'Etc';
    3: Result := 'Install';
    9: Result := 'Special';
  end;
end;

procedure AddItem(ID: string);
var
  Entry: TWZIMGEntry;
begin
  if (LeftStr(ID, 2) <> '01') then
  begin
    Entry := GetImgEntry('Item/' + GetItemDir(ID) + '/' + LeftStr(ID, 4) + '.img/' + ID);
    Data.Add(ID + '/drop', '0');
    DumpData(Entry, EquipData, EquipImages);
  end;
end;

class procedure TMobDrop.Drop(AX, AY: Integer; Num: Integer; DropList: Tlist<string>);
var
  Rand, I: Integer;
  IconPath, Dir: string;
begin

  Value := Num div 2 + 1;
  for I := 0 to Num do
  begin
    if I < 0 then
      Value := Value + 1
    else
      Value := Value - 1;
    with TMobDrop.Create(SpriteEngine) do
    begin
      Rand := Random(DropList.Count);
      if not Data.ContainsKey(DropList[Rand] + '/drop') then
        AddItem(DropList[Rand]);
      ID := DropList[Rand];
      Dir := GetItemDir(ID);
      IconPath := 'Item.wz/' + Dir + '/' + LeftStr(ID, 4) + '.img/' + ID + '/info/iconRaw';
      if LeftStr(ID, 4) <> '0900' then
        ImageEntry := EquipData[IconPath];
      DrawMode := 1;
      Angle := Random(100);
      if LeftStr(ID, 4) = '0900' then
      begin
        ParentPath := 'Item.wz/Special/0900.img/' + ID + '/iconRaw';
        IconPath := ParentPath + '/0';
        ImageEntry := EquipData[IconPath];
        DrawMode := 0;
      end;
      X := AX;
      Y := AY;
      Z := Player.Z;
      MoveX := Value * 0.5;
      Offset.X := -ImageEntry.Get('origin').Vector.X;
      Offset.Y := -ImageEntry.Get('origin').Vector.Y + 5;
    end;
  end;
end;

procedure TMobDrop.DoMove(const Movecount: Single);
var
  Below: TPoint;
  BelowFH: TFoothold;
begin
  inherited;
  X := X + MoveX;
  if (JumpState <> jsNone) then
    Angle := Angle + 0.5;
  if (JumpState = jsFalling) then
  begin
    Below := TFootholdTree.This.FindBelow(Point(Round(X), Round(Y - VelocityY - 2)), BelowFH);
    if Y >= Below.Y - 10 then
    begin
      Y := Below.Y;
      JumpState := jsNone;
      DrawMode := 0;
      Offset.X := -12;
      FH := BelowFH;
      Z := FH.Z * 100000 + 6000;
    end;
  end;

  if (LeftStr(ID, 4) = '0900') then
  begin
    ImageEntry := EquipData[ParentPath + '/' + IntToStr(Frame)];
    FTime := FTime + 17;
    if FTime > 100 then
    begin
      Frame := Frame + 1;
      if Frame > 3 then
        Frame := 0;
      FTime := 0;
    end;
    Offset.X := -ImageEntry.Get('origin').Vector.X;
    Offset.Y := -ImageEntry.Get('origin').Vector.Y + 3;
  end;

  if JumpState = jsNone then
  begin
    MoveX := 0;
    CosY := CosY + 0.055;
    Y := Y - Cos(CosY) * 0.15;
  end;

  Inc(TimeCount);
  if TimeCount>1000 then
    Alpha:=Alpha- 7;
  if Alpha < 10 then
    Dead;
end;

end.
