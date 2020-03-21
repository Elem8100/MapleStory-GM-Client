unit ChatBalloon;

interface

uses
  Windows, SysUtils, StrUtils, PXT.Sprites, Generics.Collections, Classes, WZIMGFile,
   Global, WzUtils, PXT.Graphics;

type
  TBalloonInfo = record
    ImageEntry: TWZIMGEntry;
    Width, Height: Integer;
    Origin: TPoint;
  end;

  TChatBalloon = class(TSpriteEx)
  private
    Col, Row, OffH, BWidth: Integer;
    MaxChars: Integer;
    Part1, Part2, Part3: array[0..30] of TBalloonInfo;
    Arrow, C, E, N, NE, NW, S, SE, SW, W: TBalloonInfo;
    FStyle: Integer;
    Directory: string;
    Entry: TWZIMGEntry;
    FMsg: string;
    Counter: Integer;
    TargetTexture: TTexture;
    procedure TextOut(X, Y, MaxWidth, FontHeight: Integer);
    function GetData(TileName: string): TBalloonInfo;
    class function GetS(var Remaining: string; const Width: Integer): string;
  public
    property Style: Integer read FStyle write FStyle;
    property Msg: string read FMsg write FMsg;
    procedure SetStyle(BalloonStyle: Integer; Dir: string = '');
    procedure DoDraw; override;
    procedure DoMove(const Movecount: Single); override;
    procedure TargetEvent;
    destructor Destroy;
  end;

implementation

uses
  PXT.Types,PXT.Canvas;

class function TChatBalloon.GetS(var Remaining: string; const Width: Integer): string;
var
  PixelCount: Integer;
  Index: Integer;
  OK: Boolean;
  NextWord: string;
begin
  Remaining := Trim(Remaining);
  Result := '';
  OK := True;
  PixelCount := 0;

  while (Length(Remaining) > 0) and OK do
  begin
    if CharInSet(Remaining[1], ['a'..'z', 'A'..'Z']) then
      Index := Pos(' ', Remaining)
    else
      Index := Pos('', Remaining);

    if Index > 0 then
    begin
      NextWord := Copy(Remaining, 1, Index - 1);
      if PixelCount + Round(GameFont.ExtentByPixels(' ' + NextWord).Right)
        {FontsAlt[3].TextWidth(' ' + NextWord)}   < Width then
      begin
        Result := Result + ' ' + NextWord;
        Inc(PixelCount, Round(GameFont.ExtentByPixels(' ' + NextWord).Right)
          {FontsAlt[3].TextWidth(' ' + NextWord)}   - 5);
        Delete(Remaining, 1, Index)
      end
      else
        OK := False;
    end
    else

    begin
      if Length(Result) = 0 then
      begin
        while (Length(Remaining) > 0) and (PixelCount + Round(GameFont.ExtentByPixels(Remaining[1]).Right
          {FontsAlt[0].TextWidth(Remaining[1]}) < Width) do
        begin
          Result := Result + Remaining[1];
          Inc(PixelCount, Round(GameFont.ExtentByPixels(Remaining[1]).Right){FontsAlt[3].TextWidth(Remaining[1])});
          Delete(Remaining, 1, 1)
        end
      end
      else
      begin
        if PixelCount + Round(GameFont.ExtentByPixels(' ' + Remaining).Right)
          {FontsAlt[3].TextWidth(' ' + Remaining)}   < Width then
        begin
          Result := Result + ' ' + Remaining;
          Remaining := ' '
        end
      end;
      OK := False;
    end
  end;
  Result := Trim(Result);
end;

destructor TChatBalloon.Destroy;
begin
  Entry.Free;
  TargetTexture.Free;
  inherited Destroy;
end;

procedure TChatBalloon.DoDraw;
begin
  if FMsg <> '' then
    GameCanvas.Draw(TargetTexture, Round(X - 70 - Engine.WorldX), Round(Y - 500 - Engine.WorldY));
end;

procedure TChatBalloon.DoMove;
begin
  Inc(Counter);
  if (Counter mod 30) = 0 then
    if FMsg <> '' then
      GameCanvas.DrawTarget(TargetTexture, 150, 512,
        procedure
        begin
          TargetTexture.Clear(FloatColor($00404040));
          TargetEvent;
        end);

end;

procedure TChatBalloon.SetStyle(BalloonStyle: Integer; Dir: string = '');
var
  I: Integer;
begin
  Directory := Dir;
  FStyle := BalloonStyle;
  if Directory = '' then
    Entry := GetImgEntry('UI.wz/ChatBalloon.img/' + FStyle.ToString)
  else
    Entry := GetImgEntry('UI.wz/ChatBalloon.img/' + Directory + '/' + FStyle.ToString);
  DumpData(Entry, WzData, Images);
  GameCanvas.DrawTarget(TargetTexture, 150, 512,
    procedure
    begin

    end);

  Arrow := GetData('arrow');
  C := GetData('c');
  E := GetData('e');
  N := GetData('n');
  NE := GetData('ne');
  NW := GetData('nw');
  S := GetData('s');
  SE := GetData('se');
  SW := GetData('sw');
  W := GetData('w');
  // MaxChars :=14;
  BWidth := 90;
  Col := Round(BWidth div N.Width) + 1;
  MaxChars := (N.Width * Col) div 8;
  Part1[0] := C;
  Part1[1] := NW;
  Part2[0] := C;
  Part2[1] := W;
  Part3[0] := C;
  Part3[1] := SW;
  for I := 2 to Col do
  begin
    Part1[I] := N;
    Part2[I] := C;
    Part3[I] := S;
  end;
  Part1[Col + 1] := NE;
  Part2[Col + 1] := E;
  Part3[Col + 1] := SE;
end;

procedure TChatBalloon.TextOut(X, Y, MaxWidth, FontHeight: Integer);
begin
  var FontSetting := TFontSettings.Create('Arial', 11);
  FontSetting.Effect.BorderType := TFontBorder.None;
  FontSetting.Effect.BorderOpacity := 1;
  FontSetting.Weight := TFontWeight.Thin;
  GameFont.FontSettings := FontSetting;
   for var I := 0 to Round(GameFont.ExtentByPixels(FMsg).Right){FontsAlt[3].TextWidth(FMsg)}   div 80 + 1 do
    GameFont.Draw(Point2f(X - 5, Y + I * 13), GetS(FMsg, 80), ARGB(255,125,0, 0));
end;

function TChatBalloon.GetData(TileName: string): TBalloonInfo;
begin
  if Entry.Parent.Name = 'ChatBalloon.img' then
    Result.ImageEntry := GetImgEntry('UI/ChatBalloon.img/' + IntToStr(FStyle) + '/' + TileName)
  else
    Result.ImageEntry := GetImgEntry('UI/ChatBalloon.img/' + Directory + '/' + IntToStr(FStyle) + '/' + TileName);
  Result.Width := Entry.Get(TileName).Canvas.Width;
  Result.Height := Entry.Get(TileName).Canvas.Height;
  Result.Origin.X := Entry.Get(TileName).Get('origin').Vector.X;
  Result.Origin.Y := Entry.Get(TileName).Get('origin').Vector.Y;
end;

procedure TChatBalloon.TargetEvent;
var
  I, J, Cx1, Cx2, Cx3, Mid: Integer;
begin
  Row := Round(GameFont.ExtentByPixels(FMsg).Right) {FontsAlt[3].TextWidth(FMsg)}   div 80 + 1;
  OffH := Row * C.Height + C.Origin.Y + S.Height;
  Cx1 := 0;
  Cx2 := 0;
  Cx3 := 0;
  Mid := Round(Col * N.Width / 2);
  for I := 1 to Col + 1 do
  begin
    Cx1 := Cx1 + Part1[I - 1].Width;
    GameCanvas.Draw(Images[Part1[I].ImageEntry], Cx1 - NW.Origin.X - Mid + 70, -Part1[I].Origin.Y - OffH + 500);
    Cx2 := Cx2 + Part2[I - 1].Width;
    for J := 0 to Row - 1 do
      GameCanvas.Draw(Images[Part2[I].ImageEntry], Cx2 - W.Origin.X - Mid + 70, -Part2[I].Origin.Y +
        (J * C.Height) - OffH + 500);
    Cx3 := Cx3 + Part3[I - 1].Width;
    GameCanvas.Draw(Images[Part3[I].ImageEntry], Cx3 - SW.Origin.X - Mid + 70, -Part3[I].Origin.Y +
      (J * C.Height) - OffH + 500);
  end;
  GameCanvas.Draw(Images[Arrow.ImageEntry], 70, Arrow.Origin.Y + (J * C.Height) - OffH + 500);
  {
    I2 :=0;
    for I := 0 to Length(FMsg) div MaxChars+1  do
    begin
    I2 := I2 + MaxChars;
    Characters := MaxChars;
    if Length(FMsg) < Characters then
    Characters := Length(FMsg);
    FontsAlt[0].TextOut(MidStr(FMsg, I2-MaxChars+1, Characters),X-Mid+12,-offh+ Y+I*15, ARGB(255,125,0,0));
    // canvas.TextOut(100,50 + I * 15, MidStr(Msg, I2-MaxChars+1, Characters));
    end;
  }
  TextOut(-Mid + 12 + 77, -OffH + 500 - 4, 80, 14);
end;

end.

