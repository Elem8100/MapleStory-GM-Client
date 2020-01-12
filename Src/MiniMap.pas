unit MiniMap;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, ZGameFonts, Graphics,
  AbstractTextures, ACtrlImages, StdCtrls, WZIMGFile, WZArchive,  StrUtils,
  Generics.Collections, DX9Textures, WzUtils, AsphyreRenderTargets, AControls, ACtrlEngine,
  ACtrlForms, ACtrlButtons, Global;

type

  TMiniMap = class(TAForm)
  public
    Targets: TAsphyreRenderTargets;
    TargetIndex: Integer;
    PicWidth, PicHeight:Integer;
    procedure TargetEvent(Sender: TObject);
    procedure Paint(DC: HDC); override;
    constructor Create(AOwner: TComponent); override;
  end;

var


  UIImages: TObjectDictionary<TWZIMGEntry, TDX9LockableTexture>;
   UIData: TObjectDictionary<string, TWZIMGEntry>;
implementation

uses
  AbstractCanvas, AsphyreFactory, AsphyreTypes, AsphyreDb, AbstractDevices, AsphyreImages,
   AsphyreTimer, DX9Providers,  Vectors2, Vectors2px;
  // Asphyre GUI Engine


procedure TMiniMap.Paint(DC: HDC);
var
 x,y:integer;
begin
  X := ClientLeft;
  Y := ClientTop;
  Engine.Canvas.Draw(Targets[TargetIndex], x,y, 1, False, 255, 255, 255, 255);
end;

procedure TMiniMap.TargetEvent;
begin

  var Entry := GetImgEntry('UI.wz/UIWindow2.img/MiniMap/MaxMap');
  DumpData(Entry, UIData, UIImages);
  var ID := '100000000';
  var ImgFile: TWZIMGEntry := MapWz.GetImgFile('Map/Map' + LeftStr(ID, 1) + '/' + ID + '.img').Root;
  var MiniMap: TWZIMGEntry;
  var cx, cy, Offx, OffY: Integer;
  if ImgFile.Get('miniMap') <> nil then
  begin
    cx := ImgFile.Get('miniMap/centerX').Data;
    cy := ImgFile.Get('miniMap/centerY').Data;
    MiniMap := ImgFile.Get('miniMap/canvas');
    DumpData(MiniMap, UIData, UIImages);
    PicHeight := MiniMap.Canvas.Height;
    if MiniMap.Canvas.Width < 200 then
    begin
      PicWidth := 200;
      Offx := (200 - MiniMap.Canvas.Width) div 2;
    end
    else
    begin
      PicWidth := MiniMap.Canvas.Width;
      Offx := 0;
    end;
   Engine.Canvas.FillRect(9, 62, PicWidth, PicHeight, cRGB1(0, 0, 0, 180));
    Engine.Canvas.Draw(UIImages[MiniMap], 9 + Offx, 62, 1, False, 255, 255, 255, 255);
  end
  else
  begin
    cx := 0;
    cy := 0;
    Offx := 0;
    OffY := 0;
    PicWidth := 150;
    PicHeight := 100;
     AEngine.Canvas.FillRect(9, 62, PicWidth, PicHeight, cRGB1(0, 0, 0, 180));
  end;
 // Targets := TAsphyreRenderTargets.Create();
  //TargetIndex := Targets.Add(1, PicWidth + 150, PicHeight + 150, apf_A8R8G8B8, True, True);
  for var x := 0 to PicWidth - 111 do
  begin
     AEngine.Canvas.Draw(UIImages[Entry.Get2('n')], 64 + x, 0, 1, False, 255, 255, 255, 255);
     AEngine.Canvas.Draw(UIImages[Entry.Get2('s')], 64 + x, PicHeight + 62, 1, False, 255, 255, 255, 255);
  end;
  for var y := 0 to PicHeight - 24 do
  begin
     AEngine.Canvas.Draw(UIImages[Entry.Get('w')], 0, 67 + y, 1, False, 255, 255, 255, 255);
     AEngine.Canvas.Draw(UIImages[Entry.Get('e')], PicWidth + 9, 67 + y, 1, False, 255, 255, 255, 255);
  end;
   AEngine.Canvas.Draw(UIImages[Entry.Get('nw')], 0, 0, 1, False, 255, 255, 255, 255); //left top
   AEngine.Canvas.Draw(UIImages[Entry.Get('ne')], PicWidth - 46, 0, 1, False, 255, 255, 255, 255); //right top
   AEngine.Canvas.Draw(UIImages[Entry.Get('sw')], 0, PicHeight + 44, 1, False, 255, 255, 255, 255); // right bottom
   AEngine.Canvas.Draw(UIImages[Entry.Get('se')], PicWidth - 46, PicHeight + 44, 1, False, 255, 255, 255,
    255); // left botton
  DumpData(GetImgEntry('Map.wz/MapHelper.img/minimap'), UIData, UIImages);

  var NpcMark := GetImgEntry('Map.wz/MapHelper.img/minimap/npc');
  for var iter in ImgFile.Get('life').Children do
  begin
    if (iter.Get('type', '') = 'n') and (iter.Get('hide', '') <> '1') then
       AEngine.Canvas.Draw(UIImages[NpcMark], ((iter.Get('x').Data + cx) div 16) + Offx + 4, ((iter.Get('y').Data
        + cy) div 16) + 50, 1, False, 255, 255, 255, 255);
  end;
  var PortalMark := GetImgEntry('Map.wz/MapHelper.img/minimap/portal');
  for var iter in ImgFile.Get('portal').Children do
    if (iter.Get('pt').Data = 2) or (iter.Get('pt').Data = 7) then
       AEngine.Canvas.Draw(UIImages[PortalMark], ((iter.Get('x').Data + cx) div 16) + Offx + 2, ((iter.Get
        ('y').Data + cy) div 16) + 48, 1, False, 255, 255, 255, 255);
  var MapMarkName := ImgFile.Get('info/mapMark').Data;
  if MapMarkName <> 'None' then
  begin
    var MapMarkPic := GetImgEntry('Map.wz/MapHelper.img/mark/' + MapMarkName);
    DumpData(MapMarkPic, UIData, UIImages);
     AEngine.Canvas.Draw(UIImages[MapMarkPic], 7, 17, 1, False, 255, 255, 255, 255);
  end;
end;

constructor TMiniMap.Create(AOwner: TComponent);
var
  Num: Integer;
begin
  ControlState := ControlState + [csCreating];
  inherited Create(AOwner);
  if (AOwner <> nil) and (AOwner <> Self) and (AOwner is TWControl) then
  begin
    // Auto generate name
    Num := 1;
    while AOwner.FindComponent('Form' + IntToStr(Num)) <> nil do
      Inc(Num);
    Name := 'Form' + IntToStr(Num);
  end;
  ControlState := ControlState - [csCreating];

  Targets := TAsphyreRenderTargets.Create();
  TargetIndex := Targets.Add(1, 500,500, apf_A8R8G8B8, True, True);
  AEngine.Device.RenderTo(TargetEvent, 0, True, Targets[TargetIndex]);

end;


end.

