unit Reactor;

interface

uses
  Windows, SysUtils, StrUtils, WZArchive, Generics.Collections, WZDirectory, Classes, Global,
  PXT.Sprites, WZIMGFile, WzUtils, FootHolds;

type
  TReactor = class(TSpriteEX)
  private
    ID: string;
    Entry: TWZIMGEntry;
    BelowFH: TFoothold;
    FH: TFoothold;
    Frame: Integer;
    Delay: Integer;
    FTime: Integer;
    Origin: TPoint;
    OriginType: Integer;
  public
    procedure DoMove(const Movecount: Single); override;
    procedure DoDraw; override;
    class procedure Spawn(ID: string);
    class procedure Remove;
  end;

implementation

uses
  MapleCharacter;

class procedure TReactor.Spawn(ID: string);
var
  LEntry: TWZIMGEntry;
  Pos: TPoint;
begin
  LEntry := GetImgEntry('Reactor.wz/' + ID + '.img/');
  if LEntry.Get('0') = nil then
    Exit;
  if not EquipData.ContainsKey(LEntry.GetPath) then
    DumpData(LEntry, EquipData, EquipImages);
  with TReactor.Create(SpriteEngine) do
  begin
    ImageLib := EquipImages;
    Entry := LEntry;
    ImageEntry := EquipData[Entry.GetPath + '/0/0'];
    Pos := TFootholdTree.This.FindBelow(Point(Round(Player.X), Round(Player.Y - 50)), BelowFH);
    X := Pos.X;
    Y := Pos.Y;
    Z := BelowFH.Z * 100000 + 6000;
    Width := ImageWidth;
    Height := ImageHeight;
    if Entry.Get('0/1') = nil then
      OriginType := 0
    else
      OriginType := 1;
  end;
end;

procedure TReactor.DoMove(const Movecount: Single);
begin
  inherited;
  ImageEntry := EquipData[Entry.GetPath + '/0/' + Frame.ToString];
  Delay := ImageEntry.Get('delay', '100');
  FTime := FTime + 17;
  if FTime > Delay then
  begin
    Frame := Frame + 1;
    if not HasEntryE(Entry.GetPath + '/0/' + Frame.ToString) then
      Frame := 0;
    FTime := 0;
  end;
  if ImageEntry.Get('origin') <> nil then
    Origin := ImageEntry.Get('origin').Vector;
  Offset.X := -Origin.X;

  if OriginType = 0 then
    Offset.Y := -Height
  else
    Offset.Y := -Origin.Y;
end;

procedure TReactor.DoDraw;
begin
  inherited;
end;

class procedure TReactor.Remove;
begin
  for var Iter in SpriteEngine.SpriteList do
    if (Iter is TReactor) then
    begin
      Iter.Dead;
      var s := Iter;
      s := nil;
    end;
end;

end.

