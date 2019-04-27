unit MapleChair;

interface

uses
  Windows, SysUtils, StrUtils, AsphyreSprite, Generics.Collections, WZIMGFile,
  Classes, Global, WzUtils;

type
  TMapleChair = class(TSpriteEx)
  public
    Path: string;
    FFrame: Integer;
    FTime: Integer;
    Delay: Integer;
    Origin: TPoint;
    class var
      CanUse: Boolean;
      IsUse: Boolean;
      HasSitAction: Boolean;
      UseTamingNavel: Boolean;
      BodyRelMove: TPoint;
      CharacterAction: string;

    procedure DoMove(const Movecount: Single); override;
    class procedure Delete;
    class procedure Create(ID: string); overload;
  end;

implementation

uses
  MapleCharacter,TamingMob, Footholds, ChairformUnit, MapleEffect, AvatarUnit;

class procedure TMapleChair.Delete;
begin
  TMapleChair.IsUse := False;

  for var Iter in SpriteEngine.SpriteList do
    if Iter is TMapleChair then
      Iter.Dead;
  for var Iter in EquipImages.Keys do
    if LeftStr(Iter.GetPath, 20) = 'Item.wz/Install/0301' then
    begin
      EquipImages.Remove(Iter);
      EquipData.Remove(Iter.GetPath);
    end;

  for var Iter in EquipImages.Keys do
    if LeftStr(Iter.GetPath, 27) = 'Character.wz/TamingMob/0198'  then
    begin
       EquipImages.Remove(Iter);
       EquipData.Remove(Iter.GetPath);
    end;
  BodyRelMove.X := 0;
  BodyRelMove.Y := 0;
end;

procedure TMapleChair.DoMove(const MoveCount: Single);
begin
  inherited;

  ImageEntry := EquipData[Path + '/' + FFrame.ToString];
  Delay := ImageEntry.Get('delay', '100');

  FTime := FTime + 17;
  if FTime > Delay then
  begin
    FFrame := FFrame + 1;
    if not EquipData.ContainsKey(Path + '/' + FFrame.ToString) then
      FFrame := 0;
    FTime := 0;
  end;

  if ImageEntry.Get('origin') <> nil then
    Origin := ImageEntry.Get('origin').Vector;

  if UseTamingNavel then
  begin
    case MirrorX of
      True:
        Offset.X := Origin.X - PatternWidth-TTamingMob.Navel.X;
      False:
        Offset.X := -Origin.X-TTamingMob.Navel.X;
    end;
    Offset.Y := -Origin.Y-TTamingMob.Navel.Y;
  end
  else

  begin
    case MirrorX of
      True:
        Offset.X := Origin.X - PatternWidth;
      False:
        Offset.X := -Origin.X;
    end;
    Offset.Y := -Origin.Y;
  end;

end;

class procedure TMapleChair.Create(ID: string);
var
  Below: TPoint;
  BelowFH: TFoothold;
begin

  BodyRelMove.X := 0;
  BodyRelMove.Y := 0;
  HasSitAction := False;
  UseTamingNavel := False;
  var Entry: TWZIMGEntry;
  if ItemWZ.GetImgFile('Install/0301.img') <> nil then
    Entry := GetImgEntry('Item.wz/Install/0301.img/')
  else
  begin
    case ID[5] of
      '0', '1', '2', '3', '4', '6', '7', '8':
        Entry := GetImgEntry('Item.wz/Install/0301' + ID[5] + '.img/');

      '5':
        begin
          case ID[6] of
            '0'..'9':
              Entry := GetImgEntry('Item.wz/Install/03015' + ID[6] + '.img/');
          end;
        end;

    end;
  end;

  CharacterAction := 'sit';
  if (Entry.Get(ID + '/info/tamingMob') <> nil) then
  begin
    var TamingMobID: string;
    if Entry.Get(ID + '/info/tamingMob/0')<> nil then
      TamingMobID := Entry.Get(ID + '/info/tamingMob/0').Data
    else
      TamingMobID := Entry.Get(ID + '/info/tamingMob').Data;
    if HasImgEntry('Character.wz/TamingMob/' + '0' + TamingMobID + '.img/sit/0') then
    begin
      if HasImgEntry('Character.wz/TamingMob/' + '0' + TamingMobID + '.img/sit/0/0') then
        if GetImgEntry('Character.wz/TamingMob/' + '0' + TamingMobID + '.img/sit/0/0').Canvas.Width = 4 then
          UseTamingNavel := True;

      TTamingMob.IsChairTaming := True;
      TTamingMob.Create('0' + TamingMobID);
      if HasImgEntry('Character.wz/TamingMob/' + '0' + TamingMobID + '.img/characterAction/sit') then
      begin
        HasSitAction := True;
        CharacterAction := GetImgEntry('Character.wz/TamingMob/' + '0' + TamingMobID + '.img/characterAction/sit').Data
      end
      else
        CharacterAction := 'sit';
    end;
  end;


  if (Entry.Get(ID + '/effect') = nil)  and (Entry.Get(ID + '/effect2') = nil) then
    Exit;

   if (Entry.Get(ID + '/effect/0') <> nil)   then
  if Entry.Get(ID + '/info/customChair/randomChairInfo/0') = nil then
    if (Entry.Get(ID + '/effect/0').Canvas.Width = 1) and (Entry.Get(ID + '/effect/0/_inlink') = nil) and (Entry.Get(ID + '/effect/0/_outlink') = nil) then
      if (Entry.Get(ID + '/effect/1') = nil) and (Entry.Get(ID + '/effect2') = nil) then
        Exit;

  DumpData(Entry.Get(ID), EquipData, EquipImages);

  if Entry.Get(ID + '/info/bodyRelMove') <> nil then
    BodyRelMove := Entry.Get(ID + '/info/bodyRelMove').Vector;

  if Entry.Get(ID + '/info/SitAction') <> nil then
  begin
    HasSitAction := True;
    CharacterAction := string(Entry.Get(ID + '/info/SitAction').Data);
  end
  else
    CharacterAction := 'sit';

  var HasSitEmotion: Boolean;
  if Entry.Get(ID + '/info/SitEmotion') <> nil then
    HasSitEmotion := True;
  var Entry2: TWZIMGEntry;

  if Entry.Get(ID + '/info/customChair/randomChairInfo/0') <> nil then
    Entry2 := Entry.Get(ID + '/info/customChair/randomChairInfo/0')
  else
    Entry2 := Entry.Get(ID);

  for var Iter in Entry2.Children do
  begin
    if (LeftStr(Iter.Name, 6) = 'effect') or (RightStr(Iter.Name, 6) = 'effect') then
    begin
      if Iter.Get('0') = nil then
        Continue;

      var MapleChair := TMapleChair.Create(SpriteEngine);

      MapleChair.ImageLib := EquipImages;
      MapleChair.Path := Iter.GetPath;
      MapleChair.ImageEntry := EquipData[MapleChair.Path + '/0'];

      Below := TFootholdTree.This.FindBelow(Point(Round(Player.X), Round(Player.Y - 50)), BelowFH);
      MapleChair.MirrorX := Player.MirrorX;
      MapleChair.TruncMove := True;

      var BodyRelMoveX: Integer;
      if MapleChair.MirrorX then
        BodyRelMoveX := -BodyRelMove.X
      else
        BodyRelMoveX := BodyRelMove.X;

      var Pos := Iter.Get('pos', '-1');

      case Variant(Pos) of
        -1: //no pos data
          begin

            if HasSitAction then
            begin
              MapleChair.X := Trunc(Player.X);
              MapleChair.Y := Below.Y;
            end
            else
            begin
              MapleChair.X := Trunc(Player.X) + BodyRelMoveX;
              MapleChair.Y := Below.Y + BodyRelMove.Y;
            end;
          end;
        0:
          begin
            UseTamingNavel := True;
            if HasSitAction then
            begin
              MapleChair.X := Trunc(Player.X);
              MapleChair.Y := Below.Y
            end
            else
            begin
              MapleChair.X := Trunc(Player.X) + BodyRelMoveX;
              MapleChair.Y := Below.Y + BodyRelMove.Y;
            end;
          end;
        1:
          begin
            if HasSitAction then
            begin
              UseTamingNavel := False;
              MapleChair.X := Trunc(Player.X);
              MapleChair.Y := Below.Y;
            end
            else
            begin
              MapleChair.X := Trunc(Player.X) + BodyRelMoveX;
              MapleChair.Y := Below.Y - 50 + BodyRelMove.Y;
            end;
          end;
        2, 3:
          begin
            UseTamingNavel := False;
            MapleChair.X := Trunc(Player.X);
            MapleChair.Y := Below.Y;
          end;
      end;

      if Entry.Get(ID + '/info/customChair/randomChairInfo/0') <> nil then
      begin
        MapleChair.X := Trunc(Player.X);
        MapleChair.Y := Below.Y;
      end;

      if Iter.Get('z') <> nil then
      begin
        if IsNumber(Iter.Get('z').Data) then
          MapleChair.Z := Player.Z + Iter.Get('z').Data
        else
        begin
          if TAvatarParts.ZMap.contains(Iter.Get('z').Data) then
            MapleChair.Z := Player.Z + TAvatarParts.ZMap.IndexOf(Iter.Get('z').Data)
          else
            MapleChair.Z := Player.Z - 1;
        end;
      end
      else
        MapleChair.Z := Player.Z - 1;

    end;
  end;

end;


end.

