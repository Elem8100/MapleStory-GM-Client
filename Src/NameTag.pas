unit NameTag;

interface

uses
  Windows, SysUtils, StrUtils, AsphyreSprite, Generics.Collections, WZIMGFile,
  Classes, Global, WzUtils;

type
  TNameTag = class(TSpriteEx)
  public
    class var
      ReDraw: Boolean;
      CanUse: Boolean;
      PlayerName: string;
      NameWidth: Integer;
      TargetIndex: Integer;
    procedure DoMove(const Movecount: Single); override;
    procedure DoDraw; override;
    class procedure TargetEvent(Sender: TObject);
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
    procedure InitData;
    class var
      MedalTag: TMedalTag;
    procedure TargetEvent(Sender: TObject);
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
  MapleCharacter,ShowOptionUnit, MapleMap, AsphyreTypes, DX9Textures;

procedure TNameTag.DoMove(const MoveCount: Single);
begin
  inherited;
  if ReDraw then
    GameDevice.RenderTo(TargetEvent, 0, True, GameTargets[TargetIndex]);
  x := Player.X;
  y := Player.Y;
  Z := Player.Z;
end;

procedure TNameTag.DoDraw;
var
  WX, WY, NamePos: Integer;
begin
  if TMap.ShowChar then
  begin
    WX := Round(Player.X) - Round(Engine.WorldX);
    WY := Round(Player.Y) - Round(Engine.WorldY);
    NamePos := NameWidth div 2;
    GameCanvas.Draw(GameTargets[TargetIndex], WX - NamePos, WY, 1, False, 255, 255, 255, 255);
  end;
  if ReDraw then
    ReDraw := False;
end;

class procedure TNameTag.Create(Name: string);
begin
  PlayerName := Name;
  NameWidth := FontsAlt[1].TextWidth(PlayerName) + 5;
  TargetIndex := GameTargets.Add(1, NameWidth, 15, apf_A8R8G8B8, True, True);
  GameDevice.RenderTo(TargetEvent, 0, True, GameTargets[TargetIndex]);
  with TNameTag.Create(SpriteEngine) do
  begin
    TruncMove := True;
  end;
end;

class procedure TNameTag.TargetEvent;
var
  NamePos: Integer;
begin
  NamePos := NameWidth div 2;
  if TMap.ShowChar then
  begin
    GameCanvas.FillRect(0, 2, NameWidth + 4, 15, cRGB1(0, 0, 0, 160));

    GameCanvas.Flush;
    FontsAlt[1].TextOut(PlayerName, 3, 1, $FFFFFFFF);
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
    GameDevice.RenderTo(TargetEvent, 0, True, AvatarTargets[TargetIndex]);
  x := Player.X;
  y := Player.Y;
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
    GameCanvas.Draw(AvatarTargets[TargetIndex], WX - 150, WY+5, 1, False, 255, 255, 255, 255);
  end;
  if IsReDraw then
    IsReDraw := False;
end;

procedure FixAlphaChannel(Texture: TDX9LockableTexture);
var
  x, y: Integer;
  A, R, G, B: Word;
  P: PLongWord;
  pDest: Pointer;
  nPitch: Integer;
begin
  Texture.Lock(Rect(0, 0, Texture.Width, Texture.Height), pDest, nPitch);
  P := pDest;
  for y := 0 to Texture.Height - 1 do
  begin
    for x := 0 to Texture.Width - 1 do
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
  Texture.Unlock;
end;

procedure TMedalTag.TargetEvent;
begin
  if TMap.ShowChar then
  begin
    var WestImage := EquipData[Entry.GetPath + '/w'];
    var WestX := 150 - (CenterLength + EastWidth + WestWidth) div 2;

    FixAlphaChannel(EquipImages[WestImage]);
    GameCanvas.Draw(EquipImages[WestImage], WestX, -WestImage.Get('origin').Vector.Y + 38, 1, False, 255, 255, 255, 255);

    var CenterImage := EquipData[Entry.GetPath + '/c'];
    var Count := CenterLength div CenterWidth;
    FixAlphaChannel(EquipImages[CenterImage]);
    for var i := 1 to Count do
      GameCanvas.Draw(EquipImages[CenterImage], WestX + ((i - 1) * CenterWidth) + WestWidth, -CenterImage.Get('origin').Vector.Y + 38, 1, False, 255, 255, 255, 255);

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
    GameCanvas.Draw(EquipImages[EastImage], WestX + CenterLength + WestWidth - OffX, -EastImage.Get('origin').Vector.Y + 38, 1, False, 255, 255, 255, 255);

    GameCanvas.Flush;
    FontsAlt[1].TextOut(MedalName, WestX + WestWidth + 2, 36, ARGB(255, R, G, B));
    GameCanvas.Flush;
  end;
end;

procedure TMedalTag.InitData;
begin
  EastWidth := Entry.Get2('e').Canvas.Width;
  WestWidth := Entry.Get2('w').Canvas.Width;
  CenterWidth := Entry.Get2('c').Canvas.Width;
  CenterLength := FontsAlt[1].TextWidth(MedalName) + 5;
  TagWidth := CenterLength + EastWidth + WestWidth + 30;

  var TagHeight := Entry.Get('w').Canvas.Height + 30;

  if Entry.Get('clr') <> nil then
    FontColor := 16777216 + Integer(Entry.Get('clr').Data)
  else
    FontColor := 16777215;
  R := GetR(FontColor);
  G := GetG(FontColor);
  B := GetB(FontColor);
  TargetIndex := AvatarTargets.Add(1, 300, 100, apf_A8R8G8B8, True, True);
  GameDevice.RenderTo(TargetEvent, 0, True, AvatarTargets[TargetIndex]);

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
    MedalName := GetImgEntry('String.wz/Eqp.img/Eqp/Accessory/' + RightStr(ItemID, 7)).Get('name', '');
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
    GameCanvas.Draw(AvatarTargets[TargetIndex], WX - 150, WY - 150, 1, False, 255, 255, 255, 255)
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
    GameCanvas.Draw(AvatarTargets[TargetIndex], WX - 150, WY - 28, 1, False, 255, 255, 255, 255)
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

end.

