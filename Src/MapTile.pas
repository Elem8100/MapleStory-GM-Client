unit MapTile;

interface

uses
  Windows, SysUtils, StrUtils, AsphyreSprite, Generics.Collections, WZIMGFile, Global, Tools,
  WzUtils;

type
  TMapTile = class(TSpriteEx)
  public
    procedure DoDraw; override;
    class procedure Create; overload;
  end;

implementation

uses
  MapleMap;

class procedure TMapTile.Create;
var
  Layer: Integer;
  Entry, Iter: TWZIMGEntry;
  tS, u, no: string;
  P: TPoint;
begin
  for Layer := 0 to 7 do
  begin
    tS := TMap.ImgFile.Child[Layer.ToString].Child['info'].Get('tS', '');
    for Iter in TMap.ImgFile.Child[Layer.ToString].Child['tile'].Children do
    begin
      u := Iter.Get('u', '');
      no := Iter.Get('no', '');
     // Entry := MapWZ.GetImgFile('Tile/' + tS + '.img').Root.Child[u].Child[no];
      Entry := GetImgEntry('Map.wz/Tile/' + tS + '.img/' + u + '/' + no);
      if not WzData.ContainsKey(Entry.GetPath) then
        DumpData(Entry, WzData, Images);

      with TMapTile.Create(SpriteEngine) do
      begin
        ImageLib := Images;
        ImageEntry := Entry;
        TruncMove := True;
        Moved := False;
        X := Iter.Get('x', '');
        Y := Iter.Get('y', '');
        Z := (Layer * 100000) + Entry.Get('z', '0') + 1000;
        Width := ImageWidth;
        Height := ImageHeight;
        P := Entry.Get('origin').Vector;
        Offset.X := -P.X;
        Offset.Y := -P.Y;
      end;

    end;
  end;

end;

procedure TMapTile.DoDraw;
begin
  if TMap.ShowTile then
    inherited;
end;

end.

