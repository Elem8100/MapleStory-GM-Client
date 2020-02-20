unit UI.StatusBar3;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, Graphics,
  ACtrlImages, StdCtrls, WZIMGFile, WZArchive, StrUtils, Generics.Collections,
  WzUtils,  AControls, ACtrlEngine, ACtrlForms, ACtrlButtons, Global,
  PXT.Canvas, PXT.Graphics, PXT.Types;

type
  TStatus = class(TAForm)
  public
    TargetTexture: TTexture;
    PosX: Integer;
    procedure Paint(DC: HDC); override;
    procedure ReDraw(ReDumpData: Boolean = False);
    procedure NumberTextout(X, Y: Integer; Number1, Number2: Int64);
    constructor Create(AOwner: TComponent); override;
    class procedure CreateUI;
    class var
      Instance: TStatus;
  end;

implementation

uses
  UI.Utils;

procedure TStatus.Paint(DC: HDC);
begin
  var x := ClientLeft;
  var y := ClientTop;
  Engine.Canvas.Draw(TargetTexture, x, y);
end;

procedure TStatus.NumberTextout(X, Y: Integer; Number1, Number2: Int64);
begin
  var Char: string;
  var GraphicNumber := GetImgEntry('UI.wz/StatusBar3.img/mainBar/status/gauge/number');
  var W: Integer;

  var Middle := -60 + ((Length(Number1.ToString) * 7) + (Length(Number2.ToString) * 7)) div 2;
  for var I := 1 to Length(Number1.ToString) do
  begin
    Char := MidStr(Number1.ToString, I, 1);
    W := GraphicNumber.Get(Char).Canvas.Width;
    PosX := PosX + W;
    GameCanvas.Draw(UIImages[GraphicNumber.Get(Char)], X + PosX - W - Middle, Y);
  end;

  GameCanvas.Draw(UiImages[GraphicNumber.Get('\')], X + PosX - Middle, Y + 1);
  for var I := 1 to Length(Number2.ToString) do
  begin
    Char := MidStr(Number2.ToString, I, 1);
    W := GraphicNumber.Get(Char).Canvas.Width;
    PosX := PosX + W;
    GameCanvas.Draw(UIImages[GraphicNumber.Get(Char)], 7 + X + posX - W - Middle, Y);
  end;

end;

procedure TStatus.ReDraw(ReDumpData: Boolean = False);
begin
  var Entry := GetImgEntry('UI.wz/StatusBar3.img/mainBar/status');
  if ReDumpData then
    DumpData(Entry, UIData, UIImages);
  Width := 205;
  Height := 70;
  PosX := 0;
  GameCanvas.DrawTarget(TargetTexture, 210, 94,
    procedure
    begin
      GameCanvas.Draw(UIImages[Entry.Get('backgrnd')], 2, 24);
      GameCanvas.Draw(UIImages[Entry.Get('gauge/hp/layer:0')], 24, 28);
      GameCanvas.Draw(UIImages[Entry.Get('gauge/mp/layer:0')], 24, 44);
      GameCanvas.Draw(UIImages[Entry.Get('layer:cover')], -1, 0);
      NumberTextout(50, 30, 1000002, 3258880022);
      PosX := 0;
      NumberTextout(50, 46, 25, 25);
    end);

end;

constructor TStatus.Create(AOwner: TComponent);
var
  Num: Integer;
begin
  ControlState := ControlState + [csCreating];
  inherited Create(AOwner);
  if (AOwner <> nil) and (AOwner <> Self) and (AOwner is TWControl) then
  begin
    Num := 1;
    while AOwner.FindComponent('Form' + IntToStr(Num)) <> nil do
      Inc(Num);
    Name := 'Form' + IntToStr(Num);
  end;
  ControlState := ControlState - [csCreating];
  ReDraw(True);
end;

class procedure TStatus.CreateUI;
begin
  Instance := TStatus.Create(UIEngine.Root);
  with Instance do
  begin
    Left := 300 + 1000;
    Top := 300 + 1000;
    CanMove := False;
  end;
end;

end.

