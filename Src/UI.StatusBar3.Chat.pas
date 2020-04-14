unit UI.StatusBar3.Chat;

interface

uses
  PXT.Types, Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, Graphics, ACtrlImages,
  StdCtrls, WZIMGFile, WZArchive, StrUtils, Generics.Collections, WzUtils, AControls, ACtrlEngine,
  ACtrlForms, ACtrlButtons, Global, PXT.Canvas, PXT.Graphics;

type
  TChatViewImage = class(TAImage)
  public
    TargetTexture: TTexture;
    procedure Redraw;
    procedure Paint(DC: HDC); override;
    class var
      StrList: TList<string>;
      Instance:TChatViewImage;
  end;

procedure CreateUIStatusBar3Chat;

implementation

uses
  UI.Utils, ACtrlLabels;


procedure TChatViewImage.Paint(DC: HDC);
begin
  var x := ClientLeft;
  var y := ClientTop;
  if TargetTexture.Initialized then

  Engine.Canvas.Draw(TargetTexture, x, y);
end;

procedure TChatViewImage.Redraw;
begin

end;

procedure CreateUIStatusBar3Chat;
begin
  CreateEmptyForm('StatusBar3Chat', 100, 130, 410, 595);
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
        if UIImage[Top].Top < 50 then
          UIImage[Top].Top := 50;
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

  CreateImage('ChatTop', 'UI.wz/StatusBar3.img/chat/ingame/view/min/top', 1, 1, 0, 561);
  CreateImage('ChatBottom', 'UI.wz/StatusBar3.img/chat/ingame/view/min/bottom', 1, 1, 0, 561);
  CreateImage('UI.wz/StatusBar3.img/chat/ingame/input/layer:chatEnter', 0.52, 1.18, 2, 545);
  const Path = 'UI.wz/StatusBar3.img/chat/ingame/input/layer:chatEnter';
  if UIData.ContainsKey(Path) then
  begin
    if UIData[Path].Canvas.Width = 450 then
      UIImage[Path].ScaleX := 0.52 //GMS
    else
      UIImage[Path].ScaleX := 0.546; //TMS
  end;
  CreateButton('ChatAll', 'UI.wz/StatusBar3.img/chat/common/chatTarget/all', 7, 552);
  CreateButton('UI.wz/StatusBar3.img/chat/ingame/input/button:chat', 295, 552);
  CreateButton('UI.wz/StatusBar3.img/chat/ingame/input/button:itemLink', 316, 552);
  if HasImgEntry('UI.wz/StatusBar3.img/chat/ingame/input/button:chatEmoticon') then
  begin
    CreateButton('UI.wz/StatusBar3.img/chat/ingame/input/button:chatEmoticon', 337, 552);
    CreateButton('UI.wz/StatusBar3.img/chat/ingame/input/button:help', 358, 552);
    CreateButton('UI.wz/StatusBar3.img/chat/ingame/input/button:outChat', 379, 552);
  end
  else
  begin
    CreateButton('UI.wz/StatusBar3.img/chat/ingame/input/button:help', 337, 552);
    CreateButton('UI.wz/StatusBar3.img/chat/ingame/input/button:outChat', 358, 552);
  end;

  TChatViewImage.Instance := TChatViewImage.Create(UIEngine.AForm(UIOwner));
  with TChatViewImage.Instance do
  begin
    Width := 200;
    Height := 200;
    Left := 10;
    Top := 60;
  end;

  CreateEdit('StatusBar3/Chat', 62, 553, 230, $FFFFFFFF, $FFFFFFFF);

  ActiveEdit := UIEdit['StatusBar3/Chat'];

  ActiveEdit.SetFocus;
  ActiveEdit.MaxLength := 45;
end;

initialization
  TChatViewImage.StrList := TList<string>.Create;

end.

