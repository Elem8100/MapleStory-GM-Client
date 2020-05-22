unit UI.StatusBar3.MainBar;

interface

uses
  PXT.Types, Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, Graphics, ACtrlImages,
  StdCtrls, WZIMGFile, WZArchive, StrUtils, Generics.Collections, WzUtils, AControls, ACtrlEngine,
  ACtrlForms, ACtrlButtons, Global, PXT.Canvas, PXT.Graphics;

type
  TStatusBar3MainBar = class(TAForm)
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
      Instance: TStatusBar3MainBar;
  end;

  TEXPBar = class(TAForm)
  public
    TargetTexture: TTexture;
    PosX: Integer;
    Exp: int64;
    Percent: Double;
    procedure ReDraw(ReDumpData: Boolean = False);
    procedure Paint(DC: HDC); override;
    class var
      Instance: TEXPBar;
  end;

implementation

uses
  UI.Utils, ShowOptionUnit, UI.UIWindow4.Stat, UI.UIWindow2.UserInfo, UI.UIWindow2.EventList,
  UI.UIWindow2.Item;

procedure TEXPBar.Paint(DC: HDC);
begin
  var y := ClientTop + displaysize.Y - 70;
  Engine.Canvas.Draw(TargetTexture, 0, y);

end;

procedure TEXPBar.ReDraw(ReDumpData: Boolean = False);
begin
  var Entry := GetImgEntry('UI.wz/StatusBar3.img/mainBar/EXPBar');
  if ReDumpData then
    DumpData(Entry, UIData, UIImages);
  var Entry1: TWzIMGEntry;
  GameCanvas.DrawTarget(TargetTexture, 1920, 25,
    procedure
    begin

      case DisplaySize.X of
        800..1023:
          Entry1 := Entry.Get('800');
        1024..1279:
          Entry1 := Entry.Get('1024');
        1280..1359:
          Entry1 := Entry.Get('1280');
        1360..1919:
          Entry1 := Entry.Get('1366');
        1920..4000:
          Entry1 := Entry.Get('1920');
      end;
      Engine.Canvas.Draw(UIImages[Entry1.Get('layer:back')], 0, 0);
      Engine.Canvas.Draw(UIImages[Entry1.Get('layer:gauge')], 15, 2);
      Engine.Canvas.Draw(UIImages[Entry1.Get('layer:cover')], 97, 1);

      var EXPStr := Exp.ToString + '[' + Percent.ToString + '%' + ']';
      var Middle := (DisplaySize.X div 2) - ((Length(EXPStr) * 7) div 2);
      PosX := 0;
      for var I := 1 to Length(EXPStr) do
      begin
        var Char := MidStr(EXPStr, I, 1);
        var Entry2 := Entry.Get('number/' + Char);
        var W := Entry2.Canvas.Width;
        var OffsetY := Entry2.Get('origin').Vector.Y;
        PosX := posX + W;
        GameCanvas.Draw(UIImages[Entry2], {x+} PosX - W + Middle, -OffsetY + 1{ y});
      end;

    end);

end;

procedure TStatusBar3MainBar.Paint(DC: HDC);
begin
  var x := (ClientLeft + Displaysize.X div 2) - 100;
  var y := ClientTop + displaysize.Y - 79;
  Engine.Canvas.Draw(TargetTexture, x, y);
end;

procedure TStatusBar3MainBar.NumberTextout(X, Y: Integer; NumberStr: string);
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

procedure TStatusBar3MainBar.ReDraw(ReDumpData: Boolean = False);
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
      var FontSettings := TFontSettings.Create('Arial', 12, TFontWeight.Normal);
      FontSettings.Effect.BorderType := TFontBorder.None;
      GameFont.FontSettings := FontSettings;
      GameFont.Draw(Point2f(85, 3), ShowOptionForm.Edit1.Text, $FFFFFFFF);

    end);

end;

constructor TStatusBar3MainBar.Create(AOwner: TComponent);
begin
  inherited;
  Level := 255;
  ReDraw(True);
end;

procedure HideForms(Forms: array of string);
begin
  for var i := 0 to High(Forms) do
    UIForm['UI.wz/StatusBar3.img/mainBar/submenu/title/' + Forms[i]].Visible := False;
end;

class procedure TStatusBar3MainBar.CreateUI;
begin
  Instance := TStatusBar3MainBar.Create(UIEngine.Root);
  with Instance do
  begin
    Left := 0 + 1000;
    Top := 0 + 1000;
    CanMove := False;
  end;
  CreateEmptyForm('UI.wz/StatusBar3.img/mainBar/menu', 617, 720, 200, 30, false);

  CreateButtons('UI.wz/StatusBar3.img/mainBar/menu', ['button:CashShop', 'button:Event',
    'button:Character', 'button:Community', 'button:setting', 'button:Menu']);

  var Path := 'UI.wz/StatusBar3.img/mainBar/submenu/';
  //event
  CreateEmptyForm(Path + 'title/event', 610, 630, 100, 100);
  CreateImage('event0', Path + 'backgrnd/0', 1, 1, 0, 0);
  CreateImage('event1', Path + 'backgrnd/1', 1, 15, 0, 30);
  CreateImage('event2', Path + 'backgrnd/2', 1, 1, 0, 45);
  CreateImage(Path + 'title/event', 1, 1, 0, 0);
  CreateButton(Path + 'event/button:schedule');
  //character
  CreateEmptyForm(Path + 'title/character', 650, 525, 100, 200);
  CreateImage('char0', Path + 'backgrnd/0', 1, 1, 0, 0);
  CreateImage('char1', Path + 'backgrnd/1', 1, 120, 0, 30);
  CreateImage('char2', Path + 'backgrnd/2', 1, 1, 0, 150);
  CreateImage(Path + 'title/character', 1, 1, 0, 0);
  CreateButtons(Path + 'character', ['button:character', 'button:Stat', 'button:Skill', 'button:Equip', 'button:Item']);
  UIButton[Path + 'character/button:Stat'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      CreateStatForm;
      UIForm['UI.wz/StatusBar3.img/mainBar/submenu/title/character'].Visible := False;
    end;

  UIButton[Path + 'character/button:character'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      CreateUserInfoForm;
      UIForm['UI.wz/StatusBar3.img/mainBar/submenu/title/character'].Visible := False;
    end;

     UIButton[Path + 'character/button:Item'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      CreateItemForm;
      UIForm['UI.wz/StatusBar3.img/mainBar/submenu/title/character'].Visible := False;
    end;

  UIButton[Path + 'event/button:schedule'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      CreateEventListForm;
      UIForm['UI.wz/StatusBar3.img/mainBar/submenu/title/event'].Visible := False;
    end;

  //community
  CreateEmptyForm(Path + 'title/community', 680, 585, 100, 120);
  CreateImage('community0', Path + 'backgrnd/0', 1, 1, 0, 0);
  CreateImage('community1', Path + 'backgrnd/1', 1, 60, 0, 30);
  CreateImage('community2', Path + 'backgrnd/2', 1, 1, 0, 90);
  CreateImage(Path + 'title/community', 1, 1, 0, 0);
  CreateButtons(Path + 'community', ['button:friends', 'button:bossParty', 'button:guild']);
  //setting
  CreateEmptyForm(Path + 'title/setting', 705, 595, 100, 120);
  CreateImage('setting0', Path + 'backgrnd/0', 1, 1, 0, 0);
  CreateImage('setting1', Path + 'backgrnd/1', 1, 50, 0, 30);
  CreateImage('setting2', Path + 'backgrnd/2', 1, 1, 0, 80);
  CreateImage(Path + 'title/setting', 1, 1, 0, 0);
  CreateButtons(Path + 'setting', ['button:channel', 'button:option', 'button:keysetting'{, 'button:GameQuit'}]);
  //menu
  CreateEmptyForm(Path + 'title/menu', 745, 365, 100, 370);
  CreateImage('menu0', Path + 'backgrnd/0', 1, 1, 0, 0);
  CreateImage('menu1', Path + 'backgrnd/1', 1, 280, 0, 30);
  CreateImage('menu2', Path + 'backgrnd/2', 1, 1, 0, 310);
  CreateImage(Path + 'title/menu', 1, 1, 0, 0);
  CreateButtons(Path + 'menu', ['button:quest', 'button:medal', 'button:union',
    'button:MonsterCollection', 'button:auction', 'button:battleStats', 'button:achievement',
    'button:Help', 'button:Claim', 'button:Fishing']);

  TEXPBar.Instance := TExpBar.Create(UIEngine.Root);
  with TEXPBar.Instance do
  begin
    Width := 0;
    Height := 0;
    Left := -400 + 1000;
    Top := 60 + 1000;
    Exp := 248976301855987123;
    Percent := 99.54;
    Redraw(True);
  end;
  HideForms(['character', 'menu', 'event', 'setting', 'community']);
  var PathButton := 'UI.wz/StatusBar3.img/mainBar/menu/button:';

  UIButton[PathButton + 'Event'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      const Path = 'UI.wz/StatusBar3.img/mainBar/submenu/title/event';
      HideForms(['character', 'menu', 'community', 'setting']);
      UIForm[Path].Visible := not UIForm[Path].Visible;
    end;

  UIButton[PathButton + 'Character'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      const Path = 'UI.wz/StatusBar3.img/mainBar/submenu/title/character';
      HideForms(['event', 'menu', 'community', 'setting']);
      UIForm[Path].Visible := not UIForm[Path].Visible;
    end;
  UIButton[PathButton + 'Community'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      const Path = 'UI.wz/StatusBar3.img/mainBar/submenu/title/community';
      HideForms(['character', 'menu', 'event', 'setting']);
      UIForm[Path].Visible := not UIForm[Path].Visible;
    end;

  UIButton[PathButton + 'setting'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      const Path = 'UI.wz/StatusBar3.img/mainBar/submenu/title/setting';
      HideForms(['character', 'menu', 'event', 'community']);
      UIForm[Path].Visible := not UIForm[Path].Visible;
    end;

  UIButton[PathButton + 'Menu'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      const Path = 'UI.wz/StatusBar3.img/mainBar/submenu/title/menu';
      HideForms(['character', 'setting', 'event', 'community']);
      UIForm[Path].Visible := not UIForm[Path].Visible;
    end;

end;

end.

