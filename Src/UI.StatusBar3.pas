unit UI.StatusBar3;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, Graphics, ACtrlImages, StdCtrls,
  WZIMGFile, WZArchive, StrUtils, Generics.Collections, WzUtils, AControls, ACtrlEngine, ACtrlForms,
  ACtrlButtons, Global, PXT.Canvas, PXT.Graphics, PXT.Types;

type
  TStatus = class(TAForm)
  public
    TargetTexture: TTexture;
    PosX: Integer;
    Level: Integer;
    procedure Paint(DC: HDC); override;
    procedure ReDraw(ReDumpData: Boolean = False);
    procedure NumberTextout(X, Y: Integer; Number1, Number2: Int64);
    constructor Create(AOwner: TComponent); override;
    class procedure CreateUI;
    class var
      Instance: TStatus;
  end;

  TMenuForm = class(TAForm)
    class procedure CreateInstance;
    class var
      Instance: TMenuForm;
  end;

implementation

uses
  UI.Utils, ShowOptionUnit, mainunit;

procedure TStatus.Paint(DC: HDC);
begin
  var x := (ClientLeft + Displaysize.X div 2) - 70;
  var y := ClientTop + displaysize.Y - 70;
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
      GameCanvas.Draw(UIImages[Entry.Get('layer:lv')], 24, 8);
      for var I := 1 to Length(Level.ToString) do
      begin
        var Char := MidStr(Level.ToString, I, 1);
        GameCanvas.Draw(UIImages[Entry.Get('lvNumber/' + Char)], 35 + I * 7, 8);
      end;
      var FontSettings: TFontSettings;
      FontSettings := TFontSettings.Create('Arial', 12);
      FontSettings.Effect.BorderType := TFontBorder.None;
      FontSettings.Weight := TFontWeight.Thin;
      GameFont.FontSettings := FontSettings;
      GameFont.Draw(Point2f(85, 3), ShowOptionForm.Edit1.Text, $FFFFFFFF);

    end);

end;

constructor TStatus.Create(AOwner: TComponent);
begin
  inherited;
  Level := 255;
  ReDraw(True);
end;

class procedure TStatus.CreateUI;
begin
  Instance := TStatus.Create(UIEngine.Root);
  with Instance do
  begin
    Left := 0 + 1000;
    Top := 0 + 1000;
    CanMove := False;
  end;
  TMenuform.CreateInstance;
end;

class procedure TMenuForm.CreateInstance;
begin
  Instance := TMenuForm.Create(UIEngine.Root);
  with Instance do
  begin
    Left := 300 + 1000;
    Top := 300 + 1000;
    Width := 200;
    Height := 200;
  end;

  CreateButtons('UI.wz/StatusBar3.img/mainBar/menu', ['button:CashShop', 'button:Event',
    'button:Character', 'button:Community','button:setting','button:Menu']);
end;

end.

