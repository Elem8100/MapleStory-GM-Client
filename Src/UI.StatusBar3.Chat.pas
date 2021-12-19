unit UI.StatusBar3.Chat;

interface

uses
  PXT.Types, Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs,
  Graphics, ACtrlImages, StdCtrls, WZIMGFile, WZArchive, StrUtils,
  Generics.Collections, WzUtils, AControls, ACtrlEngine, ACtrlForms,
  ACtrlButtons, Global, PXT.Canvas, PXT.Graphics;

type
  TChatViewImage = class(TAImage)
  public
    procedure Paint(DC: HDC); override;
    class var
      Pos: Integer;
      Y1: Integer;
      TargetTexture: TTexture;
      StrList: TList<string>;
      Instance: TChatViewImage;
    class procedure Redraw;
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
    Engine.Canvas.DrawPortion(TargetTexture, x, y + y1, 0, Y1, 410, 595, False, $FFFFFFFF);

end;

class procedure TChatViewImage.Redraw;
begin
  Pos := 0;
  const EditBox = UIEdit['StatusBar3/Chat'];
  TChatViewImage.StrList.Add(EditBox.Text);
  EditBox.Text := '  ';
  EdiTbox.SelStart := 0;
  Pos := TChatViewImage.StrList.Count * 13;
  if HasImgEntry('UI/StatusBar3.img/chat/ingame') then
    TChatViewImage.Y1 := 465 - Round(UIImage['UI/StatusBar3.img/chat/ingame/view/min/center'].ScaleY)
  else
    TChatViewImage.Y1 := 465 - Round(UIImage['UI/StatusBar3.img/chat/min/center'].ScaleY);

  GameCanvas.DrawTargetStatic(TChatViewImage.TargetTexture, 410, 595,
    procedure
    begin
      var FontSettings: TFontSettings;
      if ISKMS then
        FontSettings := TFontSettings.Create('Tahoma', 10, TFontWeight.Normal)
      else
        FontSettings := TFontSettings.Create('Arial', 11, TFontWeight.Normal);
      FontSettings.Effect.BorderType := TFontBorder.None;
      GameFont.FontSettings := FontSettings;

      for var i := TChatViewImage.StrList.Count - 1 downto 0 do
        GameFont.Draw(Point2f(20, 480 + (13 * I) - Pos), TChatViewImage.StrList[i], $FFFFFFFF);
    end);
end;

procedure CreateUIStatusBar3Chat;
begin
  CreateEmptyForm('StatusBar3Chat', 0, 130, 410, 595);
  UIForm['StatusBar3Chat'].CanMove := False;
  CreateButton('UI/StatusBar3.img/chat/common/chatTarget/all', 6, 420);
  CreateButton('UI/StatusBar3.img/chat/common/chatTarget/party', 56, 420);
  CreateButton('UI/StatusBar3.img/chat/common/chatTarget/friend', 106, 420);
  CreateButton('UI/StatusBar3.img/chat/common/chatTarget/guild', 156, 420);
  CreateButton('UI/StatusBar3.img/chat/common/chatTarget/association', 206, 420);
  var ChatTopPath, ChatCenterPath: string;
  if HasImgEntry('UI/StatusBar3.img/chat/ingame') then
  begin
    ChatTopPath := 'UI/StatusBar3.img/chat/ingame/view/min/top';
    ChatCenterPath := 'UI/StatusBar3.img/chat/ingame/view/min/center';
    CreateImage('UI/StatusBar3.img/chat/ingame/view/min/top', 1, 1, 0, 451);
    CreateImage('UI/StatusBar3.img/chat/ingame/view/min/center', 1, 80, 0, 451);
    CreateImage('UI/StatusBar3.img/chat/ingame/view/min/bottom', 1, 1, 0, 531);

  end
  else
  begin
    ChatTopPath := 'UI/StatusBar3.img/chat/min/top';
    ChatCenterPath := 'UI/StatusBar3.img/chat/min/center';
    CreateImage('UI/StatusBar3.img/chat/min/top', 1, 1, 0, 451);
    CreateImage('UI/StatusBar3.img/chat/min/center', 1, 80, 0, 451);
    CreateImage('UI/StatusBar3.img/chat/min/bottom', 1, 1, 0, 531);
  end;
  var OnDrag: Boolean;
  var MouseDownY: Integer;

  UIImage[ChatTopPath].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      MouseDownY := GetMouseDownY(ChatTopPath, Y);
      GameCursor.Change('67');
      OnDrag := True;
    end;

  UIImage[ChatTopPath].OnMouseEnter :=
    procedure(Sender: TObject)
    begin
      GameCursor.Change('67');
    end;

  UIImage[ChatTopPath].OnMouseLeave :=
    procedure(Sender: TObject)
    begin
      GameCursor.Change('0');
    end;
  if not UIImage.ContainsKey('ChatViewImage') then
  begin
    TChatViewImage.Instance := TChatViewImage.Create(UIEngine.AForm(UIOwner));
    with TChatViewImage.Instance do
    begin
      Width := 200;
      Height := 200;
      Left := 10;
      Top := 60;
    end;
    UIImage.Add('ChatViewImage', TChatViewImage.Instance);
  end;

  UIImage[ChatTopPath].OnMouseMove :=
    procedure(Sender: TObject; Shift: TShiftState; X, Y: Integer)
    begin
      if OnDrag then
      begin
       // const Top =  ChatTopPath;// 'UI/StatusBar3.img/chat/ingame/view/min/top';
       // const Center = 'UI/StatusBar3.img/chat/ingame/view/min/center';
        MoveImage(ChatTopPath, Y, MouseDownY);
        if UIImage[ChatTopPath].Top < 50 then
          UIImage[ChatTopPath].Top := 50;

        if UIImage[ChatTopPath].Top > 495 then
          UIImage[ChatTopPath].Top := 495;
        UIImage[ChatCenterPath].Top := UIImage[ChatTopPath].Top + 15;
        UIImage[ChatCenterPath].ScaleY := 516 - UIImage[ChatTopPath].Top;
        const buttons =['all', 'party', 'friend', 'guild', 'association'];
        for var i := 0 to 4 do
          UIButton['UI/StatusBar3.img/chat/common/chatTarget/' + Buttons[i]].Top := UIImage[ChatTopPath].Top - 16;
        TChatViewImage.Y1 := 465 - Round(UIImage[ChatCenterPath].ScaleY);
      end;
    end;

  UIImage[ChatTopPath].OnMouseUp :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      GameCursor.Change('67');
      OnDrag := False;
    end;

  CreateEmptyAttachForm('Input/ChatEnter', 'StatusBar3Chat', 0, 500, 410, 80, True);
  CreateImage('ChatTop', ChatTopPath, 1, 1, 0, 61);

  if HasImgEntry('UI/StatusBar3.img/chat/ingame') then
  begin
    CreateImage('ChatBottom', 'UI/StatusBar3.img/chat/ingame/view/min/bottom', 1, 1, 0, 61);
    CreateImage('UI/StatusBar3.img/chat/ingame/input/layer:chatEnter', 0.52, 1.18, 2, 45);
    const Path = 'UI/StatusBar3.img/chat/ingame/input/layer:chatEnter';
    if UIData.ContainsKey(Path) then
    begin
      if UIData[Path].Canvas.Width = 450 then
        UIImage[Path].ScaleX := 0.52 //GMS
      else
        UIImage[Path].ScaleX := 0.546; //TMS
    end;
    CreateButton('ChatAll', 'UI/StatusBar3.img/chat/common/chatTarget/all', 7, 52);
    CreateButton('UI/StatusBar3.img/chat/ingame/input/button:chat', 295, 52);
    CreateButton('UI/StatusBar3.img/chat/ingame/input/button:itemLink', 316, 52);
    if HasImgEntry('UI/StatusBar3.img/chat/ingame/input/button:chatEmoticon') then
    begin
      CreateButton('UI/StatusBar3.img/chat/ingame/input/button:chatEmoticon', 337, 52);
      CreateButton('UI/StatusBar3.img/chat/ingame/input/button:help', 358, 52);
      CreateButton('UI/StatusBar3.img/chat/ingame/input/button:outChat', 379, 52);
    end
    else
    begin
      CreateButton('UI/StatusBar3.img/chat/ingame/input/button:help', 337, 52);
      CreateButton('UI/StatusBar3.img/chat/ingame/input/button:outChat', 358, 52);
    end;
  end
  else
  begin
    CreateImage('ChatBottom', 'UI/StatusBar3.img/chat/min/bottom', 1, 1, 0, 61);
    CreateImage('UI/StatusBar3.img/chat/min/chatEnter', 0.52, 1.18, 2, 45);
    const Path = 'UI/StatusBar3.img/chat/min/chatEnter';
    if UIData.ContainsKey(Path) then
    begin
      if UIData[Path].Canvas.Width = 450 then
        UIImage[Path].ScaleX := 0.52 //GMS
      else
        UIImage[Path].ScaleX := 0.546; //TMS
    end;

  end;
  CreateEdit('StatusBar3/Chat', 62, 53, 230, $FFFFFFFF, $FFFFFFFF);

  ActiveEdit := UIEdit['StatusBar3/Chat'];

  ActiveEdit.SetFocus;
  ActiveEdit.MaxLength := 45;
end;

initialization
  TChatViewImage.StrList := TList<string>.Create;


finalization
  TChatViewImage.StrList.Free;

end.

