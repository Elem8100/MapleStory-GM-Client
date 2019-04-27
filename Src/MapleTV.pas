unit MapleTV;

interface

uses
  Windows, types, SysUtils, StrUtils, AsphyreSprite, Generics.Collections,
  WZIMGFile, Global,WzUtils;

type

  TMapleTV = class(TSpriteEx)
  private
    FTime: Integer;
    AD, ADCount, Frame: Integer;
    ImagePath: string;
    procedure DoMove(const Movecount: Single); override;
  public
    class procedure CreateSelf(PosX, PosY, msgX, msgY, adX, adY, ZLayer: Integer);
  end;

implementation

class procedure TMapleTV.CreateSelf(PosX, PosY, msgX, msgY, adX, adY, ZLayer: Integer);
var
  ACount: Integer;
  Entry, Iter: TWZImgEntry;
begin
  if UIWz.GetImgFile('MapleTV.img') = nil then
    Exit;
  Entry := GetImgEntry('UI/MapleTV.img/TVbasic/0');
  DumpData(Entry, WzData, Images);

  with TSpriteEx.Create(SpriteEngine) do
  begin
    ImageLib := Images;
    ImageEntry := Entry;
    if msgX <= adX then
      X := PosX + msgX
    else
      X := PosX + msgX + adX;
    Y := PosY + msgY + PatternHeight;
    Z := 20000;
    Width := PatternWidth;
    Height := PatternHeight;
    Offset.X := -Entry.Child['origin'].Vector.X;
    Offset.Y := -Entry.Child['origin'].Vector.Y;
  end;
  ACount := 0;

  Entry := GetImgEntry('UI/MapleTV.img/TVmedia');
  for Iter in Entry.Children do
    Inc(ACount);

  DumpData(Entry, WzData, Images);

  with TMapleTV.Create(SpriteEngine) do
  begin
    ImageLib := Images;
    ImagePath := 'UI.wz/MapleTV.img/TVmedia/0/0';
    ImageEntry := GetImgEntry('UI/MapleTV.img/TVmedia/0/0');
    X := PosX + adX;
    Y := PosY + adY + PatternHeight;
    Z := ZLayer + 50;
    ADCount := ACount;
    Width := PatternWidth;
    Height := PatternHeight;
    Offset.X := -WzData[ImagePath + '/origin'].Vector.X;
    Offset.Y := -WzData[ImagePath + '/origin'].Vector.Y;
  end;
end;

procedure TMapleTV.DoMove(const Movecount: Single);
var
  Delay: Integer;
const
  S1 = 'UI.wz/MapleTV.img/TVmedia/';
begin
  inherited;
  ImagePath := S1 + IntToStr(AD) + '/' + IntToStr(Frame);
  ImageEntry := WzData[ImagePath];
  Delay := WzData[ImagePath + '/delay'].Data;

  FTime := FTime + 17;
  if FTime > Delay then
  begin
    Frame := Frame + 1;
    if not WzData.ContainsKey(S1 + IntToStr(AD) + '/' + IntToStr(Frame)) then
    begin
      Frame := 0;
      AD := Random(ADCount);
    end;
    FTime := 0;
  end;
  Offset.X := -WzData[ImagePath + '/origin'].Vector.X;
  Offset.Y := -WzData[ImagePath + '/origin'].Vector.Y;
end;

end.
