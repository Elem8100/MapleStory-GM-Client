unit NameTag;

interface

uses
  Windows, SysUtils, StrUtils, PXT.Sprites, Generics.Collections, WZIMGFile, Classes, Global,
  WzUtils, PXT.Graphics;

type
  TNameTag = class(TSpriteEx)
  public
    class var
      ReDraw: Boolean;
      CanUse: Boolean;
      PlayerName: string;
      NameWidth: Integer;
      TargetTexture: TTexture;
      IsUse: Boolean;
    procedure DoMove(const Movecount: Single); override;
    procedure DoDraw; override;
    class procedure Create(Name: string); overload;
  end;

  TMedalTag = class(TSpriteEx)
  private
    EastWidth: Integer;
    WestWidth: Integer;
    CenterWidth: Integer;
    CenterLength: Integer;
    TagWidth: Integer;
    FontColor: Cardinal;
    R, G, B: Byte;
  public
    MedalName: string;
    TargetIndex: Integer;
    IsReDraw: Boolean;
    Entry: TWZIMGEntry;
    TargetTexture: TTexture;
    procedure InitData;
    class var
      MedalTag: TMedalTag;
    procedure TargetEvent;
    procedure DoMove(const Movecount: Single); override;
    procedure DoDraw; override;
    class procedure ReDraw; virtual;
    class procedure Delete; virtual;
    class procedure Create(ItemID: string); overload; virtual;
  end;

  TNickNameTag = class(TMedalTag)
  public
    class var
      NickNameTag: TNickNameTag;
    class procedure ReDraw; override;
    procedure DoDraw; override;
    class procedure Delete; override;
    class procedure Create(ItemID: string); overload; override;
  end;

  TLabelRingTag = class(TMedalTag)
  public
    class var
      LabelRingTag: TLabelRingTag;
    class procedure ReDraw; override;
    procedure DoDraw; override;
    class procedure Delete; override;
    class procedure Create(ItemID: string); overload; override;
  end;

implementation

uses
  MapleCharacter, MapleChair, ShowOptionUnit, MapleMap, PXT.Types, PXT.TypesEx, PXT.Canvas;

procedure TNameTag.DoMove(const MoveCount: Single);
begin
  inherited;
  if ReDraw then
  begin
    NameWidth := Round(GameFont.ExtentByPixels(PlayerName).Right);
    GameCanvas.DrawTarget(TargetTexture, NameWidth + 10, 25,
      procedure
      begin
        var FontSettings: TFontSettings;
        if ISKMS then
          FontSettings := TFontSettings.Create('Tahoma', 10, TFontWeight.Normal)
        else
          FontSettings := TFontSettings.Create('Arial', 11, TFontWeight.Normal);

        FontSettings.Effect.BorderType := TFontBorder.None;
        GameFont.FontSettings := FontSettings;

        var NamePos := NameWidth div 2;
        if TMap.ShowChar then
        begin
          GameCanvas.FillRoundRect(FloatRect(0, 2, NameWidth + 8, 15), cRGB1(0, 0, 0, 150), 3, 6);
          GameFont.Draw(Point2f(3, 2), PlayerName, $FFFFFFFF);
        end;
      end);
  end;
  X := Player.X;
  Y := Player.Y;
  Z := Player.Z;
end;

procedure TNameTag.DoDraw;
var
  WX, WY, NamePos: Integer;
begin
  if not TNameTag.IsUse then
    Exit;
  if TMap.ShowChar then
  begin
    WX := Round(Player.X) - Round(Engine.WorldX);
    WY := Round(Player.Y) - Round(Engine.WorldY);
    NamePos := NameWidth div 2;
    GameCanvas.Draw(TargetTexture, WX - NamePos - 8, WY);
  end;
  if ReDraw then
    ReDraw := False;
end;

class procedure TNameTag.Create(Name: string);
begin
  PlayerName := Name;
  NameWidth := Round(GameFont.ExtentByPixels(PlayerName).Right);
  GameCanvas.DrawTarget(TargetTexture, NameWidth + 10, 25,
    procedure
    begin
      var NamePos := NameWidth div 2;
      if TMap.ShowChar then
      begin
        GameCanvas.FillRoundRect(FloatRect(0, 2, NameWidth + 8, 15), cRGB1(0, 0, 0, 150), 3, 6);
        var FontSettings: TFontSettings;
        if ISKMS then
          FontSettings := TFontSettings.Create('Tahoma', 10, TFontWeight.Normal)
        else
          FontSettings := TFontSettings.Create('Arial', 11, TFontWeight.Normal);
        FontSettings.Effect.BorderType := TFontBorder.None;
        GameFont.FontSettings := FontSettings;
        GameFont.Draw(Point2f(3, 2), PlayerName, $FFFFFFFF);
      end;
    end);

  with TNameTag.Create(SpriteEngine) do
  begin
    Tag := 1;
    TruncMove := True;
  end;
end;


//TMedalTag

class procedure TMedalTag.Delete;
begin
  if MedalTag <> nil then
    MedalTag.Dead;
end;

procedure TMedalTag.DoMove(const MoveCount: Single);
begin
  inherited;
  if IsReDraw then
    GameCanvas.DrawTarget(TargetTexture, 300, 100,
      procedure
      begin
        TargetEvent;
      end);
  X := Player.X;
  Y := Player.Y;
  Z := Player.Z;
end;

class procedure TMedalTag.ReDraw;
begin
  if MedalTag <> nil then
    MedalTag.IsReDraw := True;
end;

procedure TMedalTag.DoDraw;
var
  WX, WY: Integer;
begin
  if TMap.ShowChar then
  begin
    WX := Round(Player.X) - Round(Engine.WorldX);
    WY := Round(Player.Y) - Round(Engine.WorldY);
    GameCanvas.Draw(TargetTexture, WX - 150, WY + -8);
  end;
  if IsReDraw then
    IsReDraw := False;
end;

procedure FixAlphaChannel(Texture: TTexture);
var
  x, y: Integer;
  A, R, G, B: Word;
  P: PLongWord;
  Surface: TRasterSurface;
  SurfParams: TRasterSurfaceParameters;
begin
  var Width := Texture.Parameters.Width;
  var Height := Texture.Parameters.Height;
  Surface := RasterSurfaceInit(Width, Height, TPixelFormat.BGRA8);
  SurfParams := Surface.Parameters;
  Texture.Save(Surface, 0, ZeroPoint2i, ZeroIntRect);
  Texture.Clear;

  for y := 0 to Texture.Parameters.Height - 1 do
  begin
    P := SurfParams.Scanline[y];
    for x := 0 to Texture.Parameters.Width - 1 do
    begin
      R := GetR(P^);
      G := GetG(P^);
      B := GetB(P^);
      A := GetA(P^);
      if A > 150 then
        A := 255;
      P^ := cRGB1(R, G, B, A);
      Inc(P);
    end;
  end;

  Texture.Copy(Surface, 0, ZeroPoint2i, ZeroIntRect);
  Surface.Free;
end;

procedure TMedalTag.TargetEvent;
begin
  if TMap.ShowChar then
  begin
    var WestImage := EquipData[Entry.GetPath + '/w'];
    var WestX := 150 - (CenterLength + EastWidth + WestWidth) div 2;

    FixAlphaChannel(EquipImages[WestImage]);
    Engine.Canvas.Draw(EquipImages[WestImage], WestX, -WestImage.Get('origin').Vector.Y + 38, False);

    var CenterImage := EquipData[Entry.GetPath + '/c'];
    var Count := CenterLength div CenterWidth;
    FixAlphaChannel(EquipImages[CenterImage]);
    for var i := 1 to Count do
      Engine.Canvas.Draw(EquipImages[CenterImage], WestX + ((i - 1) * CenterWidth) + WestWidth, -
        CenterImage.Get('origin').Vector.Y + 38);

    var OffX: Integer;
    case CenterWidth of
      1:
        OffX := 0;
      2:
        OffX := 1;
      3..5:
        OffX := 4;
      6..13:
        OffX := 5;

      14:
        OffX := 12;
      20:
        OffX := 18;
    end;

    var EastImage := EquipData[Entry.GetPath + '/e'];
    FixAlphaChannel(EquipImages[EastImage]);
    GameCanvas.Draw(EquipImages[EastImage], WestX + CenterLength + WestWidth - OffX, -EastImage.Get('origin').Vector.Y
      + 38);

    var FontSettings: TFontSettings;
    if ISKMS then
      FontSettings := TFontSettings.Create('Tahoma', 10, TFontWeight.Normal)
    else
      FontSettings := TFontSettings.Create('Arial', 11, TFontWeight.Normal);

    FontSettings.Effect.BorderType := TFontBorder.None;
    GameFont.FontSettings := FontSettings;
    GameFont.Draw(Point2f(WestX + WestWidth + 2, 36), MedalName, ARGB(255, R, G, B));

  end;
end;

procedure TMedalTag.InitData;
begin
  EastWidth := Entry.Get2('e').Canvas.Width;
  WestWidth := Entry.Get2('w').Canvas.Width;
  CenterWidth := Entry.Get2('c').Canvas.Width;
  var FontSetting := TFontSettings.Create('Arial', 12, TFontWeight.Normal);
  FontSetting.Effect.BorderType := TFontBorder.None;
  GameFont.FontSettings := FontSetting;
  CenterLength := Round(GameFont.Extent(MedalName).x) + 5;
  TagWidth := CenterLength + EastWidth + WestWidth + 30;

  var TagHeight := Entry.Get('w').Canvas.Height + 30;

  if Entry.Get('clr') <> nil then
    FontColor := 16777216 + Integer(Entry.Get('clr').Data)
  else
    FontColor := 16777215;
  R := GetR(FontColor);
  G := GetG(FontColor);
  B := GetB(FontColor);
  GameCanvas.DrawTarget(TargetTexture, 300, 100,
    procedure
    begin
      TargetEvent;
    end);

end;

class procedure TMedalTag.Create(ItemID: string);
begin
  MedalTag := TMedalTag.Create(SpriteEngine);
  with MedalTag do
  begin
    TruncMove := True;
    Tag := 1;
    var TagNum := GetImgEntry('Character.wz/Accessory/' + ItemID + '.img/info').Get('medalTag', '');
    Entry := GetImgEntry('UI.wz/NameTag.img/medal/' + string(TagNum));
    DumpData(Entry, EquipData, EquipImages);
    MedalName := GetImgEntry('String.wz/Eqp.img/Eqp/Accessory/' + RightStr(ItemID, 7)).Get('name',
      '');
    InitData;
  end;

end;
//NickNameTag

class procedure TNickNameTag.Delete;
begin
  if NickNameTag <> nil then
  begin
    NickNameTag.Dead;

  end;
end;

procedure TNickNameTag.DoDraw;
var
  WX, WY: Integer;
begin
  if TMap.ShowChar then
  begin
    WX := Round(Player.X) - Round(Engine.WorldX);
    WY := Round(Player.Y) - Round(Engine.WorldY);
    GameCanvas.Draw(TargetTexture, WX - 150 - Player.BrowPos.X + TMapleChair.BodyRelMove.X, WY - 110
      - Player.BrowPos.Y + TMapleChair.BodyRelMove.Y);
  end;
  if IsReDraw then
    IsReDraw := False;
end;

class procedure TNickNameTag.ReDraw;
begin
  if NickNameTag <> nil then
    NickNameTag.IsReDraw := True;
end;

class procedure TNickNameTag.Create(ItemID: string);
begin
  NickNameTag := TNickNameTag.Create(SpriteEngine);

  with NickNameTag do
  begin
    TruncMove := True;
    Tag := 1;
    var TagNum := GetImgEntry('Item.wz/Install/0370.img/' + ItemID + '/info').Get('nickTag', '');
    Entry := GetImgEntry('UI.wz/NameTag.img/nick/' + string(TagNum));
    DumpData(Entry, EquipData, EquipImages);
    MedalName := GetImgEntry('String.wz/Ins.img/' + RightStr(ItemID, 7)).Get('name', '');
    InitData;
  end;

end;

//Label Ring Tag
class procedure TLabelRingTag.Delete;
begin
  if LabelRingTag <> nil then
  begin
    LabelRingTag.Dead;
  end;
end;

procedure TLabelRingTag.DoDraw;
var
  WX, WY: Integer;
begin
  if TMap.ShowChar then
  begin
    WX := Round(Player.X) - Round(Engine.WorldX);
    WY := Round(Player.Y) - Round(Engine.WorldY);
    GameCanvas.Draw(TargetTexture, WX - 150, WY - 28);
  end;
  if IsReDraw then
    IsReDraw := False;
end;

class procedure TLabelRingTag.ReDraw;
begin
  if LabelRingTag <> nil then
    LabelRingTag.IsReDraw := True;
end;

class procedure TLabelRingTag.Create(ItemID: string);
begin
  LabelRingTag := TLabelRingTag.Create(SpriteEngine);

  with LabelRingTag do
  begin
    TruncMove := True;
    Tag := 1;
    var TagNum := GetImgEntry('Character.WZ/Ring/' + ItemID + '.img/info').Get('nameTag', '');
    Entry := GetImgEntry('UI.wz/NameTag.img/' + string(TagNum));
    DumpData(Entry, EquipData, EquipImages);
    MedalName := ShowOptionForm.Edit1.Text;
    InitData;
  end;

end;

initialization
  TNameTag.IsUse := True;

end.

