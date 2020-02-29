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
    procedure NumberTextout(X, Y: Integer; NumberStr:string);
    constructor Create(AOwner: TComponent); override;
    class procedure CreateUI;
    class var
      Instance: TStatus;
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

procedure TStatus.NumberTextout(X, Y: Integer; NumberStr: string);
begin
  var Char: string;
  var GraphicNumber := GetImgEntry('UI.wz/StatusBar3.img/mainBar/status/gauge/number');
  var W: Integer;

  var Middle := -60 + (Length(NumberStr) * 7) div 2;
  for var I := 1 to Length(NumberStr) do
  begin
    Char := MidStr(NumberStr, I, 1);
    W := GraphicNumber.Get(Char).Canvas.Width;
    PosX := PosX + W;
    GameCanvas.Draw(UIImages[GraphicNumber.Get(Char)], X + PosX - W - Middle, Y);
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
      NumberTextout(50, 30, '1000002\3258880022');
      PosX := 0;
      NumberTextout(50, 46, '25\25668');
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
  CreateEmptyForm('UI.wz/StatusBar3.img/mainBar/InfoBar', 300, 300, 200, 200);
  var Path := 'UI.wz/StatusBar3.img/mainBar/submenu';
  CreateButtons('UI.wz/StatusBar3.img/mainBar/menu', ['button:CashShop', 'button:Event',
    'button:Character', 'button:Community', 'button:setting', 'button:Menu']);

  //event
  CreateEmptyForm(Path + '/title/event', 199, 200, 200, 200);
  CreateImage('UI.wz/StatusBar3.img/mainBar/submenu/backgrnd/0', 1, 1, 0, 0);
  CreateImage('UI.wz/StatusBar3.img/mainBar/submenu/backgrnd/1', 1, 35, 0, 30);
  CreateImage('UI.wz/StatusBar3.img/mainBar/submenu/backgrnd/2', 1, 1, 0, 65);
  CreateImage(Path + '/title/event', 1, 1, 0, 0);
  CreateButton(Path + '/event/button:schedule');
  //character
  CreateEmptyForm(Path + '/title/character', 250, 200, 200, 200);
  CreateImage('UI.wz/StatusBar3.img/mainBar/submenu/backgrnd/0', 1, 1, 0, 0);
  CreateImage('UI.wz/StatusBar3.img/mainBar/submenu/backgrnd/1', 1, 120, 0, 30);
  CreateImage('UI.wz/StatusBar3.img/mainBar/submenu/backgrnd/2', 1, 1, 0, 150);
  CreateImage(Path + '/title/character', 1, 1, 0, 0);
  CreateButtons(Path + '/character', ['button:character', 'button:Stat', 'button:Skill',
    'button:Equip', 'button:Item']);
   //menu
  CreateEmptyForm(Path + '/title/menu', 450, 200, 200, 400);
  CreateImage('UI.wz/StatusBar3.img/mainBar/submenu/backgrnd/0', 1, 1, 0, 0);
  CreateImage('UI.wz/StatusBar3.img/mainBar/submenu/backgrnd/1', 1, 280, 0, 30);
  CreateImage('UI.wz/StatusBar3.img/mainBar/submenu/backgrnd/2', 1, 1, 0, 310);
  CreateImage(Path + '/title/menu', 1, 1, 0, 0);
  CreateButtons(Path + '/menu', ['button:quest', 'button:medal', 'button:union',
    'button:MonsterCollection', 'button:auction', 'button:battleStats', 'button:achievement',
    'button:Help', 'button:Claim', 'button:Fishing']);
  //community
  CreateEmptyForm(Path + '/title/community', 650, 200, 200, 400);
  CreateImage('UI.wz/StatusBar3.img/mainBar/submenu/backgrnd/0', 1, 1, 0, 0);
  CreateImage('UI.wz/StatusBar3.img/mainBar/submenu/backgrnd/1', 1, 60, 0, 30);
  CreateImage('UI.wz/StatusBar3.img/mainBar/submenu/backgrnd/2', 1, 1, 0, 90);
  CreateImage(Path + '/title/community', 1, 1, 0, 0);
  CreateButtons(Path + '/community', ['button:friends', 'button:bossParty', 'button:guild']);
  //setting
  CreateEmptyForm(Path + '/title/setting', 650, 500, 200, 400);
  CreateImage('UI.wz/StatusBar3.img/mainBar/submenu/backgrnd/0', 1, 1, 0, 0);
  CreateImage('UI.wz/StatusBar3.img/mainBar/submenu/backgrnd/1', 1, 50, 0, 30);
  CreateImage('UI.wz/StatusBar3.img/mainBar/submenu/backgrnd/2', 1, 1, 0, 80);
  CreateImage(Path + '/title/setting', 1, 1, 0, 0);
  CreateButtons(Path + '/setting', ['button:channel', 'button:option', 'button:keysetting','button:GameQuit']);


  uibutton['UI.wz/StatusBar3.img/mainBar/menu/button:CashShop'].OnClick :=
    procedure(sender: tobject)
    begin
      uiform['UI.wz/StatusBar3.img/mainBar/submenu'].Visible := false;
    end;
end;

end.

