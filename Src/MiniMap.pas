unit MiniMap;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, Graphics,
  ACtrlImages, StdCtrls, WZIMGFile, WZArchive, StrUtils, Generics.Collections,
  WzUtils, AControls, ACtrlEngine, ACtrlForms, ACtrlButtons, Global, PXT.Canvas,
  PXT.Graphics, PXT.Types;

type
  TMiniMap = class(TAForm)
  public
    TargetTexture: TTexture;
    TargetIndex: Integer;
    PicWidth, PicHeight: Integer;
    OffX, OffY, cx, cy: Integer;
    LWidth: integer;
    PlayerMark: TWZIMGEntry;
    HasMiniMap: Boolean;
    AddHeight, AddWidth: Integer;
    procedure DrawUIVersion1;
    procedure DrawUIVersion3;
    procedure TargetEvent;
    procedure Paint(DC: HDC); override;
    procedure ReDraw;
  end;

var
  AMiniMap: TMiniMap;

implementation

uses
  MapleMap, MapleCharacter, UI.Utils, Math;

procedure TMiniMap.Paint(DC: HDC);
var
  x, y, px, py: Integer;
begin
  if not TMap.ShowMiniMap then
    Exit;
  x := ClientLeft;
  y := ClientTop;

  if HasMiniMap then
  begin
    Engine.Canvas.Draw(TargetTexture, x, y);
    px := Round(Player.X + cx) div 16;
    py := Round(Player.Y + cy) div 16;
    if UIVersion = 1 then
      AEngine.Canvas.Draw(UIImages[PlayerMark], x + px + OffX + 2 + 8, y + py + OffY + 50 + 15)
    else
      AEngine.Canvas.Draw(UIImages[PlayerMark], x + px + OffX + 2, y + py + OffY + 50);
  end;
  inherited;

end;

procedure TMiniMap.DrawUIVersion1;
begin
  var Entry := GetImgEntry('UI/UIWindow.img/MiniMap/MaxMap');

  DumpData(Entry, UIData, UIImages);
  var MiniMap: TWZIMGEntry;

  if TMap.HasMiniMap then
  begin
    cx := TMap.ImgFile.Get('miniMap/centerX').Data;
    cy := TMap.ImgFile.Get('miniMap/centerY').Data;
    MiniMap := TMap.MiniMapEntry;//.Get('canvas');
    DumpData(MiniMap, UIData, UIImages);
    PicHeight := MiniMap.Canvas.Height;
    PicWidth := lwidth;
    OffX := (picwidth - MiniMap.Canvas.Width) div 2;
    var Left := ((picWidth + 13) - TMap.MiniMapWidth) div 2;
    Engine.Canvas.FillRect(FloatRect(7, 72, Left, PicHeight), ARGB(128, 255, 255, 255));
    Engine.Canvas.FillRect(FloatRect(offx + 13 + TMap.MiniMapWidth, 72, Left, PicHeight), ARGB(128, 255, 255, 255));

    Engine.Canvas.FillRect(FloatRect(offx + 13, 72, TMap.MiniMapWidth, PicHeight), ARGB(128, 0, 0, 0));
    Engine.Canvas.Draw(UIImages[MiniMap], 9 + OffX + 3, 72);
  end
  else
  begin
    cx := 0;
    cy := 0;
    OffX := 0;
    OffY := 0;
    PicWidth := 150;
    PicHeight := 100;
    AEngine.Canvas.FillRect(FloatRect(9, 62, PicWidth, PicHeight), ARGB(180, 0, 0, 0));
  end;

  for var x := 0 to PicWidth + 10 do
  begin
    AEngine.Canvas.Draw(UIImages[Entry.Get2('n')], 4 + x + 3, 0);
    AEngine.Canvas.Draw(UIImages[Entry.Get2('s')], 4 + x + 3, PicHeight + 62 + 10);
  end;

  for var y := 0 to PicHeight - 1 do
  begin
    AEngine.Canvas.Draw(UIImages[Entry.Get('w')], 1, 72 + y);
    AEngine.Canvas.Draw(UIImages[Entry.Get('e')], PicWidth + 18, 72 + y);
  end;
  AEngine.Canvas.Draw(UIImages[Entry.Get('nw')], 1, 0); //left top
  AEngine.Canvas.Draw(UIImages[Entry.Get('ne')], PicWidth + 18, 0); //right top
  AEngine.Canvas.Draw(UIImages[Entry.Get('sw')], 1, PicHeight + 72); // right bottom
  AEngine.Canvas.Draw(UIImages[Entry.Get('se')], PicWidth + 18, PicHeight + 72); // left botton

  DumpData(GetImgEntry('Map/MapHelper.img/minimap'), UIData, UIImages);

  var NpcMark := GetImgEntry('Map/MapHelper.img/minimap/npc');
  for var iter in TMap.ImgFile.Get('life').Children do
  begin
    if (iter.Get('type', '') = 'n') and (iter.Get('hide', '') <> '1') then
      AEngine.Canvas.Draw(UIImages[NpcMark], ((iter.Get('x').Data + cx) div 16) + OffX + 12, ((iter.Get('y').Data + cy) div 16) + 65);
  end;
  var PortalMark := GetImgEntry('Map/MapHelper.img/minimap/portal');
  for var iter in TMap.ImgFile.Get('portal').Children do
    if (iter.Get('pt').Data = 2) or (iter.Get('pt').Data = 7) then
      AEngine.Canvas.Draw(UIImages[PortalMark], ((iter.Get('x').Data + cx) div 16) + OffX + 10, ((iter.Get('y').Data + cy) div 16) + 63);
  var MapMarkName := TMap.ImgFile.Get('info/mapMark').Data;
  if MapMarkName <> 'None' then
  begin
    var MapMarkPic := GetImgEntry('Map/MapHelper.img/mark/' + MapMarkName);
    DumpData(MapMarkPic, UIData, UIImages);
    AEngine.Canvas.Draw(UIImages[MapMarkPic], 7, 22);
  end;
  PlayerMark := GetImgEntry('Map/MapHelper.img/minimap/user');

  var FontSetting: TFontSettings;

  if ISKMS then
    FontSetting := TFontSettings.Create('Tahoma', 12, TFontWeight.Normal)
  else
    FontSetting := TFontSettings.Create('Arial', 12, TFontWeight.Normal);

  FontSetting.Effect.BorderType := TFontBorder.Normal;
  FontSetting.Effect.BorderOpacity := 0.6;
  FontSetting.Weight := TFontWeight.SemiBold;
  GameFont.FontSettings := FontSetting;
  if TMap.MapNameList.ContainsKey(TMap.ID) then
  begin
    GameFont.Draw(Point2f(49, 26), TMap.MapNameList[TMap.ID].StreetName, $FFFFFFFF);
    GameFont.Draw(Point2f(49, 43), TMap.MapNameList[TMap.ID].MapName, $FFFFFFFF);
  end;

end;

procedure TMiniMap.DrawUIVersion3;
begin
  var Entry := GetImgEntry('UI/UIWindow2.img/MiniMap/MaxMap');
  DumpData(Entry, UIData, UIImages);
  var MiniMap: TWZIMGEntry;

  if TMap.HasMiniMap then
  begin
    cx := TMap.ImgFile.Get('miniMap/centerX').Data;
    cy := TMap.ImgFile.Get('miniMap/centerY').Data;
    MiniMap := TMap.MiniMapEntry;//.Get('canvas');
    DumpData(MiniMap, UIData, UIImages);
    PicHeight := MiniMap.Canvas.Height;
    PicWidth := lwidth;
    OffX := (picwidth - MiniMap.Canvas.Width) div 2;
    Engine.Canvas.FillRect(FloatRect(9, 62, PicWidth, PicHeight), ARGB(180, 0, 0, 0));
    Engine.Canvas.Draw(UIImages[MiniMap], 9 + OffX, 62);
  end
  else
  begin
    cx := 0;
    cy := 0;
    OffX := 0;
    OffY := 0;
    PicWidth := 150;
    PicHeight := 100;
    AEngine.Canvas.FillRect(FloatRect(9, 62, PicWidth, PicHeight), ARGB(180, 0, 0, 0));
  end;

  for var x := 0 to PicWidth - 111 do
  begin
    AEngine.Canvas.Draw(UIImages[Entry.Get2('n')], 64 + x, 0);
    AEngine.Canvas.Draw(UIImages[Entry.Get2('s')], 64 + x, PicHeight + 62);
  end;
  for var y := 0 to PicHeight - 24 do
  begin
    AEngine.Canvas.Draw(UIImages[Entry.Get('w')], 0, 67 + y);
    AEngine.Canvas.Draw(UIImages[Entry.Get('e')], PicWidth + 9, 67 + y);
  end;
  AEngine.Canvas.Draw(UIImages[Entry.Get('nw')], 0, 0); //left top
  AEngine.Canvas.Draw(UIImages[Entry.Get('ne')], PicWidth - 46, 0); //right top
  AEngine.Canvas.Draw(UIImages[Entry.Get('sw')], 0, PicHeight + 44); // right bottom
  AEngine.Canvas.Draw(UIImages[Entry.Get('se')], PicWidth - 46, PicHeight + 44); // left botton
  DumpData(GetImgEntry('Map/MapHelper.img/minimap'), UIData, UIImages);

  var NpcMark := GetImgEntry('Map/MapHelper.img/minimap/npc');
  for var iter in TMap.ImgFile.Get('life').Children do
  begin
    if (iter.Get('type', '') = 'n') and (iter.Get('hide', '') <> '1') then
      AEngine.Canvas.Draw(UIImages[NpcMark], ((iter.Get('x').Data + cx) div 16) + OffX + 4, ((iter.Get('y').Data + cy) div 16) + 50);
  end;
  var PortalMark := GetImgEntry('Map/MapHelper.img/minimap/portal');
  for var iter in TMap.ImgFile.Get('portal').Children do
    if (iter.Get('pt').Data = 2) or (iter.Get('pt').Data = 7) then
      AEngine.Canvas.Draw(UIImages[PortalMark], ((iter.Get('x').Data + cx) div 16) + OffX + 2, ((iter.Get('y').Data + cy) div 16) + 48);
  var MapMarkName := TMap.ImgFile.Get('info/mapMark').Data;
  if MapMarkName <> 'None' then
  begin
    var MapMarkPic := GetImgEntry('Map/MapHelper.img/mark/' + MapMarkName);
    DumpData(MapMarkPic, UIData, UIImages);
    AEngine.Canvas.Draw(UIImages[MapMarkPic], 7, 17);
  end;
  PlayerMark := GetImgEntry('Map/MapHelper.img/minimap/user');

  var FontSetting: TFontSettings;

  if ISKMS then
    FontSetting := TFontSettings.Create('Tahoma', 12, TFontWeight.Normal)
  else
    FontSetting := TFontSettings.Create('Arial', 12, TFontWeight.Normal);

  FontSetting.Effect.BorderType := TFontBorder.None;
  FontSetting.Effect.BorderOpacity := 1;
  FontSetting.Weight := TFontWeight.SemiBold;
  GameFont.FontSettings := FontSetting;
  if TMap.MapNameList.ContainsKey(TMap.ID) then
  begin
    GameFont.Draw(Point2f(50, 20), TMap.MapNameList[TMap.ID].StreetName, $FFFFFFFF);
    GameFont.Draw(Point2f(50, 37), TMap.MapNameList[TMap.ID].MapName, $FFFFFFFF);
  end;

end;

procedure TMiniMap.TargetEvent;
begin

  if UIVersion = 1 then
    DrawUIVersion1
  else
    DrawUIVersion3;

end;

procedure TMiniMap.ReDraw;
var
  Length1, Length2, Length: Single;
begin
  HasMiniMap := TMap.HasMiniMap;
  var FontSetting: TFontSettings;
  if ISKMS then
    FontSetting := TFontSettings.Create('Tahoma', 12, TFontWeight.Normal)
  else
    FontSetting := TFontSettings.Create('Arial', 12, TFontWeight.Normal);
  FontSetting.Weight := TFontWeight.SemiBold;
  GameFont.FontSettings := FontSetting;
  if TMap.MapNameList.ContainsKey(TMap.ID) then
  begin
    Length1 := GameFont.ExtentByPixels(TMap.MapNameList[TMap.ID].StreetName).Right;
    Length2 := GameFont.ExtentByPixels(TMap.MapNameList[TMap.ID].MapName).Right;
  end;
  Length := Max(Length1, Length2);

  if UIVersion = 1 then
  begin
    AddWidth := -50;
    AddHeight := 12
  end
  else
  begin
    AddWidth := 0;
    AddHeight := 0;
  end;
  LWidth := Round(Max(Length, TMap.MiniMapWidth + AddWidth)) + 40;

  GameCanvas.DrawTarget(TargetTexture, LWidth + 50, TMap.MiniMapHeight + 80 + AddHeight,
    procedure
    begin
      TargetEvent;
    end);
  Width := LWidth + 20;
  Height := TMap.MiniMapHeight + 40;

  if UIVersion = 1 then
  begin
    const Path = 'UI/UIWindow.img/MiniMap/';
    CreateImage(Path + 'title', 1, 1, 9, 9);
    //TMS
    if UIImage[Path + 'title'].Height = 14 then
      UIImage[Path + 'title'].Top := 5;

    CreateButton(Path + 'BtMap', 0, 6);
    UIButton[Path + 'BtMap'].Left := Lwidth-18;

    CreateButton('UI/Basic.img/BtMax', 0, 6);
    UIButton['UI/Basic.img/BtMax'].Left := Lwidth-32;

    CreateButton('UI/Basic.img/BtMin', 0, 6);
    UIButton['UI/Basic.img/BtMin'].Left := Lwidth-45;

  end;

  if UIVersion = 3 then
  begin
    UIOwner := 'Form10';
    const Path = 'UI/UIWindow2.img/MiniMap/';
    CreateButton(Path + 'BtNpc', 0, 5);
    UIButton[Path + 'BtNpc'].Left := Lwidth - 30;
  //
    CreateButton(Path + 'BtMap', 0, 5);
    UIButton[Path + 'BtMap'].Left := Lwidth - 70;
  //
    CreateButton(Path + 'BtBig', 0, 5);
    UIButton[Path + 'BtBig'].Left := Lwidth - 85;
    UIButton[Path + 'BtBig'].Enabled := False;
  //
    CreateButton(Path + 'BtMax', 0, 5);
    UIButton[Path + 'BtMax'].Left := Lwidth - 100;
    UIButton[Path + 'BtMax'].Enabled := False;
   //
    CreateButton(Path + 'BtMin', 0, 5);
    UIButton[Path + 'BtMin'].Left := Lwidth - 115;
  //
    const Bt =['BtNpc', 'BtMap', 'BtBig', 'BtMax', 'BtMin'];
    for var i := 0 to 4 do
    begin
      if HasMiniMap then
        UIButton[Path + Bt[i]].Visible := True
      else
        UIButton[Path + Bt[i]].Visible := False;
    end;

  end;
end;

end.

