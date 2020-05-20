unit UI.UIWindow2.UserInfo;

interface

uses
  PXT.Types, Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, Graphics, ACtrlImages,
  StdCtrls, WZIMGFile, WZArchive, StrUtils, Generics.Collections, WzUtils, AControls, ACtrlEngine,
  ACtrlForms, ACtrlButtons, Global, PXT.Canvas, PXT.Graphics;

type
  TUserInfoAvatarImage = class(TAImage)
  public
    procedure Paint(DC: HDC); override;
    class var
      Show: Boolean;
      Instance: TUserInfoAvatarImage;
  end;

procedure CreateUserInfoForm;

implementation

uses
  UI.Utils, ACtrlLabels, MainUnit, MapleCharacter, MapleChair, Tamingmob;

procedure TUserInfoAvatarImage.Paint(DC: HDC);
begin
  var x := ClientLeft;
  var y := ClientTop;
  var WX := Round(Player.X - SpriteEngine.WorldX - 50 + TMapleChair.BodyRelMove.X - TTamingMob.Navel.X);
  var WY := Round(Player.y - SpriteEngine.WorldY - 80 + TMapleChair.BodyRelMove.Y - TTamingMob.Navel.Y);
  Engine.Canvas.DrawPortion(AvatarPanelTexture, x, y, WX, WY, WX + 93, WY + 80, False, $FFFFFFFF);
end;

var
  Fame: Integer;

procedure CreateUserInfoForm;
begin
  const Path = 'UI.wz/UIWindow2.img/UserInfo/character/';
  CreateForm(Path + 'backgrnd', 617, 320);
  CreateButton('UserInfoFormClose', 'UI.wz/Basic.img/BtClose3', 250, 7);
  UIButton['UserInfoFormClose'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      UIForm['UI.wz/UIWindow2.img/UserInfo/character/backgrnd'].Visible := False;
      TUserInfoAvatarImage.Show := False;
    end;

  CreateImage(Path + 'backgrnd2');
  TUserInfoAvatarImage.Show := True;
  if not UIImage.ContainsKey('UserInfoAvatarImage') then
  begin
    TUserInfoAvatarImage.Instance := TUserInfoAvatarImage.Create(UIEngine.AForm(UIOwner));
    with TUserInfoAvatarImage.Instance do
    begin
      Width := 200;
      Height := 200;
      Left := 12;
      Top := 49;
    end;
    UIImage.Add('UserInfoAvatarImage', TUserInfoAvatarImage.Instance);
  end;

  CreateImage(Path + 'name');
  CreateLabel('UserInfoName', 'SuperGM', 26, 137, lcWhite);
  CreateLabel('UserInfoLv', '255', 152, 69);
  CreateLabel('UserInfoJob', 'SuperGM', 152, 87);
  CreateLabel('UserInfoFame', '555', 152, 105);
  CreateButton(Path + 'BtPopUp');
  CreateButton(Path + 'BtPopDown');
  UIButton[Path + 'BtPopUp'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      Fame := UILabel['UserInfoFame'].Text.ToInteger;
      Inc(Fame);
      UILabel['UserInfoFame'].Text := Fame.ToString;
    end;
  UIButton[Path + 'BtPopDown'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      Fame := UILabel['UserInfoFame'].Text.ToInteger;
      Dec(Fame);
      UILabel['UserInfoFame'].Text := Fame.ToString;
    end;

  CreateButton(Path + 'BtFamily');
  CreateButton(Path + 'BtParty');
  CreateButton(Path + 'BtTrad');
  CreateButton(Path + 'BtItem');
  //
  CreateButton(Path + 'BtPersonality');
  CreateButton(Path + 'BtCollect');
  CreateButton(Path + 'BtRide');
  CreateButton(Path + 'BtPet');
  CreateButton(Path + 'BtDamage');
  CreateAttachForm('UI.wz/UIWindow2.img/UserInfo/personality/backgrnd', Path + 'backgrnd', -288, 50);
  CreateImage('UI.wz/UIWindow2.img/UserInfo/personality/backgrnd2');
  CreateImage('UI.wz/UIWindow2.img/UserInfo/personality/backgrnd3');
  CreateImage('UI.wz/UIWindow2.img/UserInfo/personality/backgrnd4');
  UIButton[Path + 'BtPersonality'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      const Path = 'UI.wz/UIWindow2.img/UserInfo/personality/backgrnd';
      UIForm[Path].Visible := not UIForm[Path].Visible;
    end;

end;

end.

