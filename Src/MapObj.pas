unit MapObj;

interface

uses
  Windows, SysUtils, StrUtils, PXT.Sprites, Generics.Collections, WZIMGFile, Global,
  Tools, Math, WzUtils,PXT.Types;

type
  TMapObj = class(TSpriteEx)
  private
    FDelta: Real;
    FFrame: Integer;
    FTime: Integer;
    Origin: TPoint;
    InfoPath: string;
    AX,AY:Single;
    MoveP,MoveW,MoveH:Integer;
  public
    procedure DoMove(const Movecount: Single); override;
    procedure DoDraw; override;
    class procedure Create; overload;
  end;

  TFlowSprite = class(TBackgroundSprite)
  private
    FFrame: Integer;
    FHasAnim: Boolean;
    FTime: Integer;
    RX: Integer;
    RY: Integer;
    CX, CY: Integer;
    Flow: Integer;
    Origin: TPoint;
    InfoPath: string;
  public
    procedure DoMove(const Movecount: Single); override;
  end;

implementation

uses
  MapleMap;

class procedure TMapObj.Create;
var
  Entry, Iter: TWZIMGEntry;
  Layer, _Flow: Integer;
  oS, L0, L1, L2: string;
begin

  for Layer := 0 to 7 do
  begin
    for Iter in TMap.ImgFile.Child[Layer.ToString].Child['obj'].Children do
    begin
      oS := Iter.Get('oS', '');
      L0 := Iter.Get('l0', '');
      L1 := Iter.Get('l1', '');
      L2 := Iter.Get('l2', '');
      if L1 = 'cherryBlossom' then
        Continue;
      if (oS = 'guide') and (L0 = 'tutorial') and (L1 = 'key') then
        Continue;

      if HasImgFile('Map/Obj/' + oS + '.img') then
        Entry := GetImgEntry('Map/Obj/' + oS + '.img/' + L0 + '/' + L1 + '/' + L2);

     // else if HasImgFile('Map001.wz/Obj/' + oS + '.img') then
      //  Entry := GetImgEntry('Map001.wz/Obj/' + oS + '.img/' + L0 + '/' + L1 + '/' + L2)
     // else
       // Entry := GetImgEntry('Map2.wz/Obj/' + oS + '.img/' + L0 + '/' + L1 + '/' + L2);
      if not WzData.ContainsKey(Entry.GetPath) then
        DumpData(Entry, WzData, Images);

      _Flow := Iter.Get('flow', '0');

      if _Flow = 0 then
        with TMapObj.Create(SpriteEngine) do
        begin
          InfoPath := GetEntryPath(Entry);
          ImageLib := Images;
          ImageEntry := WzData[InfoPath + '/0'];
          X := Iter.Get('x', '0');
          Y := Iter.Get('y', '0');
          AX:=X;
          AY:=Y;
          Z := Layer * 100000 + Iter.Get('z', '0');
          MirrorX := Boolean(Iter.Get('f', '0'));
          Width := PatternWidth;
          Height := PatternHeight;
          TruncMove := True;

          if not WzData.ContainsKey(InfoPath + '/1') then
            Moved := False;

          if ImageEntry.Get('moveType', '0') then
            Moved := True;

          if ImageEntry.Get('moveP', '0') then
            MoveP:= ImageEntry.Get('moveP').Data;
          if ImageEntry.Get('moveW', '0') then
            MoveW:= ImageEntry.Get('moveW').Data;
          if ImageEntry.Get('moveH', '0') then
            MoveH:= ImageEntry.Get('moveH').Data;


          if ImageEntry.Get('moveR', '0') then
          begin
            Moved := True;
            DrawMode := 1;
          end;

          if ImageEntry.Get('blend', '0') then
            BlendMode := TBlendingEffect.Add;

          Origin := ImageEntry.Get('origin').Vector;
          case MirrorX of
            True:
              Offset.X := Origin.X - PatternWidth;
            False:
              Offset.X := -Origin.X;
          end;
          Offset.Y := -Origin.Y;
        end;

      if _Flow > 0 then
        with TFlowSprite.Create(SpriteEngine) do
        begin
          InfoPath := GetEntryPath(Entry);
          ImageLib := Images;
          ImageEntry := WzData[InfoPath + '/0'];
          X := Iter.Get('x', '0');
          Y := Iter.Get('y', '0');
          Z := Layer * 1000 + Iter.Get('z', '0');
          MirrorX := Boolean(Iter.Get('f', '0'));
          RX := Iter.Get('rx', '0');
          RY := Iter.Get('ry', '0');
          CX := Iter.Get('cx', '0');
          CY := Iter.Get('cy', '0');
          Flow := _Flow;

          if CX = 0 then
            Width := TMap.Info['MapWidth']
          else
            Width := CX;

          if CY = 0 then
            Height := TMap.Info['MapHeight']
          else
            Height := CY;

          Offset.X := ImageEntry.Get('origin').Vector.X;
          Offset.Y := ImageEntry.Get('origin').Vector.Y;
          SetMapSize(1, 1);
          Tiled := True;
          if Flow = 1 then
            TileMode := tmHorizontal;
          if Flow = 2 then
            TileMode := tmVertical;
        end;

    end;
  end;

end;

procedure TMapObj.DoMove(const Movecount: Single);
var
  Delay, a0, a1, MoveType, MoveR, Flow: Integer;
  AniAlpha: Single;
begin
  // if FHasAnim = False  then Exit;
  inherited;
  if WzData.ContainsKey(InfoPath + '/spine') then
    Exit;
  ImageEntry := WzData[InfoPath + '/' + IntToStr(FFrame)];

  Delay := ImageEntry.Get('delay', '100');
  a0 := ImageEntry.Get('a0', '-1');
  a1 := ImageEntry.Get('a1', '-1');

  FTime := FTime + 17;
  if FTime > Delay then
  begin
    FFrame := FFrame + 1;
    if not WzData.ContainsKey(InfoPath + '/' + IntToStr(FFrame)) then
      FFrame := 0;
    FTime := 0;
  end;

  if (a0 <> -1) and (a1 = -1) then
    Alpha := ImageEntry.Get('a0', '255');

  AniAlpha := a0 - (a0 - a1) * FTime / Delay;
  if FTime > 0 then
    Alpha := Trunc(AniAlpha);

  MoveType := ImageEntry.Get('moveType', '0');

  if Boolean(MoveType) then
  begin

    FDelta := FDelta + 0.017;
    case MoveType of
      1:
        begin
          if Boolean(MoveP) then
            X := AX + MoveW * Cos(FDelta * 1000 * 2 * Pi /MoveP)
          else
            X := AX + MoveW * Cos(FDelta);
        end;
      2:
        begin
          if Boolean(MoveP) then
            Y := AY + MoveH * Cos(FDelta * 2 * Pi * 1000 / MoveP)
          else
            Y := AY + MoveH * Cos(FDelta);
        end;
      3:
        begin

        end;
    end;

  end;

  if ImageEntry.Get('origin') <> nil then
    Origin := ImageEntry.Get('origin').Vector;

  case MirrorX of
    True:
      Offset.X := Origin.X - PatternWidth;
    False:
      Offset.X := -Origin.X;
  end;
  Offset.Y := -Origin.Y;
  MoveR := ImageEntry.Get('moveR', '0');
  if Boolean(MoveR) then
  begin
    Angle := Angle + (17 / MoveR) * Pi * 2;
    Offset.X := 0;
    Offset.Y := 0;
  end;

end;

procedure TMapObj.DoDraw;
begin
  if TMap.ShowObj then
    inherited;
end;

procedure TFlowSprite.DoMove(const Movecount: Single);
var
  Delay, MoveType, MoveP, MoveH, MoveW, MoveR: Integer;
begin

  case Flow of
    1:
      X := X - RX * 5 / 60;
    2:
      Y := Y - RY * 5 / 60;
  end;

  ImageEntry := WzData[InfoPath + '/' + IntToStr(FFrame)];
  Delay := ImageEntry.Get('delay', '100');

  FTime := FTime + 17;
  if FTime > Delay then
  begin
    FFrame := FFrame + 1;
    if not WzData.ContainsKey(InfoPath + '/' + IntToStr(FFrame)) then
      FFrame := 0;
    FTime := 0;
  end;

  if ImageEntry.Get('origin') <> nil then
    Origin := ImageEntry.Get('origin').Vector;

  case MirrorX of
    True:
      Offset.X := -Origin.X + PatternWidth;
    False:
      Offset.X := Origin.X;
  end;
  Offset.Y := Origin.Y;

end;

initialization


end.

