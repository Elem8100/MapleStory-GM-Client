unit MapPortal;

interface

uses
  Windows, SysUtils, StrUtils, WZArchive, Generics.Collections, WZIMGFile, WZDirectory, Classes,
  PXT.Sprites, PXT.TypesEx, Global, MapleMap, WzUtils, System.Types;

type
  TPortalInfo = record
    ToMap, PortalName, ToName: string;
    X, Y, PortalType: Integer;
  end;

  TMapPortal = class(TSpriteEx)
  private
    FTime: Integer;
    FFrame: Integer;
    InfoPath: string;
    FPortalName: string;
    FPortalType: Integer;
    FToName: string;
    FToMap: Integer;
    Origin: TPoint;
    class var
      PortalInfo: TPortalInfo;
  public
    procedure DoMove(const Movecount: Single); override;
    procedure DoDraw; override;
    class var
      PortalList: TList<TPortalInfo>;
    class function Find(P: TPoint; var OnPortal: Boolean): TPortalInfo;
    class procedure Create; overload;
  end;

implementation

uses
  PXT.Types;

class function TMapPortal.Find(P: TPoint; var OnPortal: Boolean): TPortalInfo;
var
  Portal: TPortalInfo;
begin
  OnPortal := False;
  for Portal in PortalList do
    if (P.X > Portal.X - 15) and (P.X < Portal.X + 15) and (P.Y < Portal.Y + 12) and (P.Y > Portal.Y - 12) then
    begin
      Result := Portal;
      OnPortal := True;
    end;
end;

class procedure TMapPortal.Create;
var
  PType: Integer;
  Iter, Entry: TWZIMGEntry;
  P: TPoint;
begin
  if PortalList = nil then
    PortalList := TList<TPortalInfo>.Create
  else
    PortalList.Clear;

  if GetImgEntry('Map/MapHelper.img/portal/game/pv/default') = nil then
    Entry := GetImgEntry('Map/MapHelper.img/portal/game/pv')
  else
    Entry := GetImgEntry('Map/MapHelper.img/portal/game/pv/default');
  DumpData(Entry, WzData, Images);

  for Iter in TMap.ImgFile.Child['portal'].Children do
  begin
    PType := Iter.Get('pt', '');
    PortalInfo.X := Iter.Get('x', '0');
    PortalInfo.Y := Iter.Get('y', '0');
    PortalInfo.ToMap := Iter.Get('tm', '0');
    PortalInfo.ToName := Iter.Get('tn', '0');
    PortalInfo.PortalName := Iter.Get('pn', '0');
    PortalInfo.PortalType := Iter.Get('pt', '0');
    PortalList.Add(PortalInfo);

    if PType = 2 then
    begin
      with TMapPortal.Create(SpriteEngine) do
      begin
        ImageLib := Images;
        InfoPath := GetEntryPath(Entry);
        ImageEntry := WzData[InfoPath + '/0'];
        X := Iter.Get('x', '0');
        Y := Iter.Get('y', '0');
        ;
        Z := 1000000;
        FPortalName := Iter.Get('pn', '');
        FToMap := Iter.Get('tm', '');
        FToName := Iter.Get('tn', '');
        Width := PatternWidth;
        Height := PatternHeight + 100;
        TruncMove := False;
        P := ImageEntry.Get('origin').Vector;
        Offset.X := -P.X;
        Offset.Y := -P.Y;
      end;
    end;
  end;

end;

procedure TMapPortal.DoMove(const Movecount: Single);
begin
  inherited;

  ImageEntry := WzData[InfoPath + '/' + IntToStr(FFrame)];
  FTime := FTime + 17;
  if FTime > 100 then
  begin
    FFrame := FFrame + 1;
    if not WzData.ContainsKey(InfoPath + '/' + IntToStr(FFrame)) then
      FFrame := 0;
    FTime := 0;
  end;

  Origin := ImageEntry.Get('origin').Vector;
  Offset.X := -Origin.X;
  Offset.Y := -Origin.Y;
end;

procedure TMapPortal.DoDraw;
var
  WX, WY: Integer;
begin
  if TMap.ShowPortal then
    inherited;

  WX := Round(X - Engine.WorldX);
  WY := Round(Y - Engine.WorldY);
  if TMap.ShowPortalInfo then
  begin
    GameFont.Draw(Point2f(WX - 50, WY - 120), 'Name:  ' + FPortalName, cRGB1(255, 0, 0));
    GameFont.Draw(Point2f(WX - 50, WY - 105), 'ToMap:  ' + IntToStr(FToMap), cRGB1(255, 0, 0));
    GameFont.Draw(Point2f(WX - 50, WY - 90), 'ToName:  ' + FToName, cRGB1(255, 0, 0));
  end;
end;

initialization

finalization
  TMapPortal.PortalList.Free;

end.

