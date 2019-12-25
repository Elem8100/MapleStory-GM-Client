unit MapleEffect;

interface

uses
  Windows, SysUtils, StrUtils, AsphyreSprite, Generics.Collections, WZIMGFile,
  Classes, Global, WzUtils;

type
  TEffectType = (Cash, Chair, Equip);

  TSetEffect = class(TSpriteEx)
    Path: string;
    FFrame: Integer;
    FTime: Integer;
    Delay: Integer;
    Origin: TPoint;
    Default: Integer;
    class var
      AllList: TDictionary<string, string>;
      UseList: TDictionary<string, TSetEffect>;
    procedure DoMove(const Movecount: Single); override;
    class procedure LoadList;
    class procedure Delete(ID: string); overload;
    class procedure Create(ID: string); overload;
  end;

  TItemEffect = class(TSpriteEx)
  public
    Path: string;
    FFrame: Integer;
    FTime: Integer;
    Delay: Integer;
    Origin: TPoint;
    Default: Integer;
    IsCash: Boolean;
    EffType: TEffectType;
    class var
      AllList: TList<string>;
      Entry: TWZIMGEntry;
      UseList: TDictionary<string, TItemEffect>;
    procedure DoMove(const Movecount: Single); override;
    class procedure LoadList;
    class procedure Delete(ID: string); overload;
    class procedure Delete(EffectType: TEffectType); overload;
    class procedure Create(ID: string; ItemEff: Boolean = False); overload;
  end;

implementation

uses
  MapleCharacter, Footholds, ChairformUnit,  MapleChair;

class procedure TSetEffect.LoadList;
begin
  for var Iter in EffectWz.GetImgFile('SetEff.img').Root.Children do
    for var Iter2 in Iter.Children do
      if Iter2.Name = 'info' then
        for var Iter3 in Iter2.Children do
          for var Iter4 in Iter3.Children do
            TSetEffect.AllList.AddOrSetvalue('0' + string(Iter4.Data), Iter.Name);
end;

class procedure TSetEffect.Create(ID: string);
begin
  var Entry := GetImgEntry('Effect.wz/SetEff.img/' + IDToInt(AllList[ID]));
  DumpData(Entry, EquipData, EquipImages);

  var SetEffect := TSetEffect.Create(SpriteEngine);
  with SetEffect do
  begin
    ImageLib := EquipImages;
    TruncMove := True;
    Tag := 1;
    for var Iter in Entry.Children do
      for var Iter2 in Iter.Children do
      begin
        if (Iter2.Name[1] in ['0'..'9']) and (Iter2.DataType = mdtCanvas) then
        begin
          Path := TWZIMGEntry(Iter2.Parent).GetPath;
          ImageEntry := EquipData[Iter2.GetPath];
        end;
      end;
   end;
  UseList.AddOrSetValue(ID, SetEffect);
end;

procedure TSetEffect.DoMove(const MoveCount: Single);
var
  Pos, OffY: Integer;
  BrowPos, BodyRelMove: TPoint;
begin
  inherited;
  if HasEntryE(Path + '/' + Player.Action + '/' + FFrame.ToString) then
  begin
    ImageEntry := EquipData[Path + '/' + Player.Action + '/' + FFrame.ToString];
    Default := 1;
    Visible := True;
  end
  else if HasEntryE(Path + '/' + FFrame.ToString) then
  begin
    ImageEntry := EquipData[Path + '/' + FFrame.ToString];
    Default := 0;
    Visible := True;
  end
  else
    Visible := False;
  Delay := ImageEntry.Get('delay', '100');
  FTime := FTime + 17;
  if FTime > Delay then
  begin
    FFrame := FFrame + 1;
    case Default of
      1:
        if not HasEntryE(Path + '/' + Player.Action + '/' + FFrame.ToString) then
          FFrame := 0;
      0:
        if not HasEntryE(Path + '/' + FFrame.ToString) then
          FFrame := 0;
    end;

    FTime := 0;
  end;
  X := Trunc(Player.X - 10);
  Pos := TWZIMGEntry(ImageEntry.Parent).Get('pos', '-1');

  if Pos = 1 then
    Y := Trunc(Player.Y) - 50
  else
    Y := Trunc(Player.Y);

  Z := Player.z + TWZIMGEntry(ImageEntry.Parent).Get('z', '-1');
 // if EffType = Chair then
  //  Z := Player.z + TWZIMGEntry(ImageEntry).Get('z', '0');
  MirrorX := Player.MirrorX;

  if ImageEntry.Get('origin') <> nil then
    Origin := ImageEntry.Get('origin').Vector;

  BrowPos := player.BrowPos;
  BodyRelMove := TMapleChair.BodyRelMove;
  OffY := 30;

  case MirrorX of
    True:
      Offset.X := Origin.X - PatternWidth - BrowPos.X + BodyRelMove.X+3;
    False:
      Offset.X := -Origin.X - BrowPos.X + 12 + BodyRelMove.X;
  end;
  Offset.Y := -Origin.Y - BrowPos.Y + OffY + BodyRelMove.Y;

end;

class procedure TSetEffect.Delete(ID: string);
begin
  if TSetEffect.UseList.ContainsKey(ID) then
  begin
    TSetEffect.UseList[ID].Dead;
    TSetEffect.UseList.Remove(ID);
  end;
end;

class procedure TItemEffect.LoadList;
begin
  for var Iter in EffectWz.GetImgFile('ItemEff.img').Root.Children do
    TItemEffect.AllList.Add('0' + Iter.Name);
end;

class procedure TItemEffect.Delete(ID: string);
begin
  if TItemEffect.UseList.ContainsKey(ID) then
  begin
    TItemEffect.UseList[ID].Dead;
    TItemEffect.UseList.Remove(ID);
  end;
end;

class procedure TItemEffect.Delete(EffectType: TEffectType);
begin
  for var Iter in SpriteEngine.SpriteList do
    if (Iter is TItemEffect) and (TItemEffect(Iter).EffType = EffectType) then
      Iter.Dead;
end;

procedure TItemEffect.DoMove(const MoveCount: Single);
var
  Pos, OffY: Integer;
  BrowPos, BodyRelMove: TPoint;
begin
  inherited;

  if HasEntryE(Path + '/' + Player.Action + '/' + FFrame.ToString) then
  begin
    ImageEntry := EquipData[Path + '/' + Player.Action + '/' + FFrame.ToString];
    Default := 1;
    Visible := True;
  end
  else if HasEntryE(Path + '/default/' + FFrame.ToString) then
  begin
    ImageEntry := EquipData[Path + '/default/' + FFrame.ToString];
    Default := 0;
    Visible := True;
  end
  else if HasEntryE(Path + '/0/' + FFrame.ToString) then
  begin
    ImageEntry := EquipData[Path + '/0/' + FFrame.ToString];
    Default := 2;
    Visible := True;
  end
  else if HasEntryE(Path + '/' + FFrame.ToString) then
  begin
    ImageEntry := EquipData[Path + '/' + FFrame.ToString];
    Default := 3;
    Visible := True;
  end
  else
    Visible := False;
  Delay := ImageEntry.Get('delay', '100');

  FTime := FTime + 17;
  if FTime > Delay then
  begin
    FFrame := FFrame + 1;

    case Default of
      1:
        if not HasEntryE(Path + '/' + Player.Action + '/' + FFrame.ToString) then
          FFrame := 0;
      0:
        if not HasEntryE(Path + '/default/' + FFrame.ToString) then
          FFrame := 0;
      2:
        if not HasEntryE(Path + '/0/' + FFrame.ToString) then
          FFrame := 0;
      3:
        if not HasEntryE(Path + '/' + FFrame.ToString) then
          FFrame := 0;
    end;

    FTime := 0;
  end;

  X := Trunc(Player.X - 10);
  Pos := TWZIMGEntry(ImageEntry.Parent).Get('pos', '-1');

  if Pos = 1 then
    Y := Trunc(Player.Y) - 50
  else
    Y := Trunc(Player.Y);

  Z := Player.z + TWZIMGEntry(ImageEntry.Parent).Get('z', '0');
  if EffType = Chair then
    Z := Player.z + TWZIMGEntry(ImageEntry).Get('z', '0')-1;
  MirrorX := Player.MirrorX;

  if ImageEntry.Get('origin') <> nil then
    Origin := ImageEntry.Get('origin').Vector;

  if EffType = Chair then
  begin
    BrowPos.X := 0;
    BrowPos.Y := 0;
    BodyRelMove.X := 0;
    BodyRelMove.Y := 0;
    OffY := 0;
  end
  else
  begin
    BrowPos := player.BrowPos;
    BodyRelMove := TMapleChair.BodyRelMove;
    OffY := 30;
  end;

  case MirrorX of
    True:
      Offset.X := Origin.X - PatternWidth + 12 - BrowPos.X + BodyRelMove.X;
    False:
      Offset.X := -Origin.X - BrowPos.X + 2 + BodyRelMove.X;
  end;
  Offset.Y := -Origin.Y - BrowPos.Y + OffY + BodyRelMove.Y;

end;

class procedure TItemEffect.Create(ID: string; ItemEff: Boolean = False);
begin
  if ID = '01048000' then
    Exit;
  if ID = '01049000' then
    Exit;
  if ItemEff then
    Entry := GetImgEntry('Effect.wz/ItemEff.img/' + IDToInt(ID))
  else
    Entry := GetImgEntry('Item.wz/Cash/0501.img/' + ID);

  DumpData(Entry, EquipData, EquipImages);
  if LeftStr(ID, 4) = '0301' then
  begin
    for var Iter in Entry.Children do
      if Iter.Name[1] in ['0'..'9'] then
        with TItemEffect.Create(SpriteEngine) do
        begin
          EffType := Chair;
          ImageLib := EquipImages;
          TruncMove := True;
          Tag := 1;
          for var Iter2 in Iter.Children do
          begin
            if Iter2.DataType = mdtCanvas then
            begin
              Path := TWZIMGEntry(Iter2.Parent).GetPath;
              ImageEntry := EquipData[Iter2.GetPath];
            end;
            for var Iter3 in Iter2.Children do
              if Iter3.DataType = mdtCanvas then
              begin
                Path := TWZIMGEntry(Iter3.Parent).GetPath;
                ImageEntry := EquipData[Iter3.GetPath];
              end;
          end;

        end;
  end
  else
  begin
    var ItemEffect := TItemEffect.Create(SpriteEngine);
    with ItemEffect do
    begin
      if LeftStr(ID, 4) = '0501' then
        EffType := Cash
      else
        EffType := Equip;

      ImageLib := EquipImages;
      Path := Entry.GetPath + '/effect';
      for var Iter in Entry.Get('effect').Children do
        if Iter.Get('0') <> nil then
        begin
          ImageEntry := EquipData[Iter.GetPath + '/0'];
          Break;
        end;
      TruncMove := True;
      Tag := 1;
    end;
    if ItemEff then
      UseList.Add(ID, ItemEffect);
  end;
end;

initialization
  TSetEffect.AllList := TDictionary<string, string>.Create;
  TSetEffect.UseList := TDictionary<string, TSetEffect>.Create;

  TItemEffect.AllList := TList<string>.Create;
  TItemEffect.UseList := TDictionary<string, TItemEffect>.Create;

finalization
  TSetEffect.AllList.Free;
  TSetEffect.UseList.Free;
  TItemEffect.UseList.Free;
  TItemEffect.AllList.Free;

end.

