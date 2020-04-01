unit UI.StatusBar3.Chat;

interface

uses
  PXT.Types, Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, Graphics, ACtrlImages,
  StdCtrls, WZIMGFile, WZArchive, StrUtils, Generics.Collections, WzUtils, AControls, ACtrlEngine,
  ACtrlForms, ACtrlButtons, Global, PXT.Canvas, PXT.Graphics;

procedure CreateUIStatusBar3Chat;

implementation

uses
  UI.Utils, ACtrlLabels;

procedure CreateUIStatusBar3Chat;
begin
  CreateEmptyForm('StatusBar3Chat', 100, 130, 410, 545);
  CreateButton('UI.wz/StatusBar3.img/chat/common/chatTarget/all', 6, 420);
  CreateButton('UI.wz/StatusBar3.img/chat/common/chatTarget/party', 56, 420);
  CreateButton('UI.wz/StatusBar3.img/chat/common/chatTarget/friend', 106, 420);
  CreateButton('UI.wz/StatusBar3.img/chat/common/chatTarget/guild', 156, 420);
  CreateButton('UI.wz/StatusBar3.img/chat/common/chatTarget/association', 206, 420);
  CreateImage('UI.wz/StatusBar3.img/chat/ingame/view/min/top', 1, 1, 0, 451);
  CreateImage('UI.wz/StatusBar3.img/chat/ingame/view/min/center', 1, 80, 0, 451);
  CreateImage('UI.wz/StatusBar3.img/chat/ingame/view/min/bottom', 1, 1, 0, 531);

  var OnDrag: Boolean;
  UIImage['UI.wz/StatusBar3.img/chat/ingame/view/min/top'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      GameCursor.Change('67');
      OnDrag := True;
    end;

  UIImage['UI.wz/StatusBar3.img/chat/ingame/view/min/top'].OnMouseUp :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      GameCursor.Change('67');
      OnDrag := False;
    end;

  UIImage['UI.wz/StatusBar3.img/chat/ingame/view/min/top'].OnMouseMove :=
    procedure(Sender: TObject; Shift: TShiftState; X, Y: Integer)
    begin
      if OnDrag then
      begin
        const Top = 'UI.wz/StatusBar3.img/chat/ingame/view/min/top';
        const Center = 'UI.wz/StatusBar3.img/chat/ingame/view/min/center';
        UIImage[Top].Top := Y - UIImage[Top].Parent.Top + 1000 - 5;
        UIImage[Center].Top := UIImage[Top].Top + 15;
        UIImage[Center].ScaleY := 516 - UIImage[Top].Top;
        const buttons =['all', 'party', 'friend', 'guild', 'association'];
        for var i := 0 to 4 do
          UIButton['UI.wz/StatusBar3.img/chat/common/chatTarget/' + Buttons[i]].Top := UIImage[Top].Top - 16;
      end;
    end;

  UIImage['UI.wz/StatusBar3.img/chat/ingame/view/min/top'].OnMouseEnter :=
    procedure(Sender: TObject)
    begin
      GameCursor.Change('67');
    end;

  UIImage['UI.wz/StatusBar3.img/chat/ingame/view/min/top'].OnMouseLeave :=
    procedure(Sender: TObject)
    begin
      GameCursor.Change('0');
    end;

  CreateImage('UI.wz/StatusBar3.img/chat/ingame/input/layer:backgrnd', 0.713, 1, 0, 546);
  CreateImage('UI.wz/StatusBar3.img/chat/ingame/input/layer:chatEnter', 0.6, 1, 0, 546);
end;

end.

