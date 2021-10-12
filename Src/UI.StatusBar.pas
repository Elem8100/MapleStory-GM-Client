unit UI.StatusBar;

interface

uses
  PXT.Types, Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs,
  Graphics, ACtrlImages, StdCtrls, WZIMGFile, WZArchive, StrUtils,
  Generics.Collections, WzUtils, AControls, ACtrlEngine, ACtrlForms,
  ACtrlButtons, Global, PXT.Canvas, PXT.Graphics;

type
  TStatusBar = class(TAForm)
  public
    TargetTexture: TTexture;
    PosX: Integer;
    Level: Integer;
    procedure Paint(DC: HDC); override;
    procedure ReDraw(ReDumpData: Boolean = False);
    procedure NumberTextout(X, Y: Integer; NumberStr: string);
    constructor Create(AOwner: TComponent); override;
    class procedure CreateUI;
    class var
      Instance: TStatusBar;
  end;

implementation

uses
  UI.Utils, ShowOptionUnit;

procedure TStatusBar.Paint(DC: HDC);
begin
  var x := -2;
  var y := ClientTop + displaysize.Y - 73;

  Engine.Canvas.Draw(TargetTexture, x, y);
end;

procedure TStatusBar.NumberTextout(X, Y: Integer; NumberStr: string);
begin
  var Char, Str: string;
  var GraphicNumber := GetImgEntry('UI/StatusBar.img/number');
  var W, OffH: Integer;

  var Middle := -60 + (Length(NumberStr) * 7) div 2;
  for var I := 1 to Length(NumberStr) do
  begin
    Char := MidStr(NumberStr, I, 1);
    if (Char = '[') or (Char = ']') then
      OffH := -1
    else
      OffH := 0;
    case Char[1] of
      '[':
        Str := 'Lbracket';
      ']':
        Str := 'Rbracket';
      '%':
        Str := 'percent';
      '/':
        Str := 'slash';
    else
      Str := Char;

    end;

    if Str <> '.' then
      W := GraphicNumber.Get2(Str).Canvas.Width + 1
    else
      W := 5;
    PosX := PosX + W;

    if Str <> '.' then
      GameCanvas.Draw(UIImages[GraphicNumber.Get(Str)], X + PosX - W, Y + OffH);
    if Str = '.' then
    begin
      var FontSettings: TFontSettings;
      if ISKMS then
        FontSettings := TFontSettings.Create('Tahoma', 15, TFontWeight.Normal)
      else
        FontSettings := TFontSettings.Create('Arial', 15, TFontWeight.Normal);
      FontSettings.Effect.BorderType := TFontBorder.None;
      GameFont.FontSettings := FontSettings;
      GameFont.Draw(Point2f(X + posx - W, Y - 8), '.', $FFFFFFFF);
    end;
  end;
end;

procedure TStatusBar.ReDraw(ReDumpData: Boolean = False);
begin

  var Entry := GetImgEntry('UI/StatusBar.img/');
  if ReDumpData then
  begin
    DumpData(Entry, UIData, UIImages);
    DumpData(GetImgEntry('UI/Basic.img/LevelNo'), UIData, UIImages);
  end;
  Width := 800;
  Height := 75;
  PosX := 0;
  GameCanvas.DrawTarget(TargetTexture, 800, 75,
    procedure
    begin
      GameCanvas.Draw(UIImages[Entry.Get('base/backgrnd')], 2, 3);
      GameCanvas.Draw(UIImages[Entry.Get('base/backgrnd2')], 2, 3);
      GameCanvas.Draw(UIImages[Entry.Get('gauge/bar')], 220, 40);
      GameCanvas.Draw(UIImages[Entry.Get('gauge/graduation')], 220, 40);
      NumberTextout(242, 44, '[5688/5688]');
      NumberTextout(290, 44, '[7322/7322]');
      NumberTextout(345, 44, '567899[99.98%]');

      var OffX: Integer;
      if Length(Level.ToString) = 3 then
        OffX := 25
      else
        OffX := 30;
      for var I := 1 to Length(Level.ToString) do
      begin
        var Char := MidStr(Level.ToString, I, 1);
        GameCanvas.Draw(UIImages[GetImgEntry('UI/Basic.img/LevelNo/' + Char)], OffX + I * 13, 51);
      end;

      var FontSettings: TFontSettings;
      if ISKMS then
        FontSettings := TFontSettings.Create('Tahoma', 10, TFontWeight.Normal)
      else
        FontSettings := TFontSettings.Create('Arial', 11, TFontWeight.Normal);

      FontSettings.Effect.BorderType := TFontBorder.None;
      GameFont.FontSettings := FontSettings;
      GameFont.Draw(Point2f(90, 42), 'Hunter', $FFFFFFFF);
      GameFont.Draw(Point2f(90, 55), ShowOptionForm.Edit1.Text, $FFFFFFFF);

    end);

end;

constructor TStatusBar.Create(AOwner: TComponent);
begin
  inherited;
  Level := 250;
  ReDraw(True);
end;

procedure HideForms(Forms: array of string);
begin
  for var i := 0 to High(Forms) do
    UIForm['UI/StatusBar3.img/mainBar/submenu/title/' + Forms[i]].Visible := False;
end;

class procedure TStatusBar.CreateUI;
begin
  CreateForm('UI/StatusBar.img/base/backgrnd00', 'UI/StatusBar.img/base/backgrnd', 798, 698);
  CreateForm('UI/StatusBar.img/base/backgrnd11', 'UI/StatusBar.img/base/backgrnd', 1598, 698);
  CreateForm('UI/StatusBar.img/base/backgrnd22', 'UI/StatusBar.img/base/backgrnd', 2398, 698);
  UIForm['UI/StatusBar.img/base/backgrnd00'].Width := 0;
  UIForm['UI/StatusBar.img/base/backgrnd11'].Width := 0;
  UIForm['UI/StatusBar.img/base/backgrnd22'].Width := 0;

  Instance := TStatusBar.Create(UIEngine.Root);
  with Instance do
  begin
    Left := 0 + 1000;
    Top := 0 + 1000;
    CanMove := False;
  end;
  CreateEmptyForm('UI/StatusBar.img/mainBar', 0, 698, 800, 72, false);
  CreateImage('UI/StatusBar.img/base/chatTarget', 1, 1, 1, 9);
  CreateLabel('UI/chatTarget', 'To All', 5, 11, lcWhite);
  CreateButton('BtMin11', 'UI/Basic.img/BtMin', 535, 12);
  CreateEmptyForm('UI/StatusBar.img/Right', 570, 698, 500, 72, False);
  CreateImage('UI/StatusBar.img/base/box', 1, 1, 0, 8);
  CreateButton('UI/StatusBar.img/BtClaim', 0, 8);
  CreateImage('UI/StatusBar.img/base/iconMemo', 1, 1, 26, 12);
  //
  CreateButton('UI/StatusBar.img/EquipKey', 45, 8);
  CreateButton('UI/StatusBar.img/InvenKey', 75, 8);
  CreateButton('UI/StatusBar.img/StatKey', 105, 8);
  CreateButton('UI/StatusBar.img/SkillKey', 135, 8);
  CreateButton('UI/StatusBar.img/KeySet', 165, 8);
  CreateButton('UI/StatusBar.img/QuickSlotD', 195, 8);
  //
  CreateButton('UI/StatusBar.img/BtShop', 0, 36);
  CreateButton('UI/StatusBar.img/BtNPT', 58, 36);
  CreateButton('UI/StatusBar.img/BtMenu', 114, 36);
  CreateButton('UI/StatusBar.img/BtShort', 170, 36);
  CreateForm('UI/StatusBar.img/base/quickSlot', 875, 622);
  UIForm['UI/StatusBar.img/base/quickSlot'].CanMove:=False;
end;

end.

