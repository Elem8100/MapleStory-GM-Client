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

procedure HideForms(Forms: array of string);
begin
  for var i := 0 to High(Forms) do
    UIForm['UI.wz/UIWindow2.img/UserInfo/' + Forms[i] + '/backgrnd'].Visible := False;
end;

procedure CreateUserInfoForm;
begin
  const Path = 'UI.wz/UIWindow2.img/UserInfo/character/';
  CreateForm(Path + 'backgrnd', 517, 320);
  CreateButton('UserInfoFormClose', 'UI.wz/Basic.img/BtClose3', 250, 7);
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
  // personality
  CreateAttachForm('UI.wz/UIWindow2.img/UserInfo/personality/backgrnd', Path + 'backgrnd', 0, 191);
  CreateImage('UI.wz/UIWindow2.img/UserInfo/personality/backgrnd2');
  CreateImage('UI.wz/UIWindow2.img/UserInfo/personality/backgrnd3');
  CreateImage('UI.wz/UIWindow2.img/UserInfo/personality/backgrnd4');
  // collect
  CreateAttachForm('UI.wz/UIWindow2.img/UserInfo/collect/backgrnd', Path + 'backgrnd', 0, 191);
  CreateImage('UI.wz/UIWindow2.img/UserInfo/collect/backgrnd2');
  //ride
  CreateAttachForm('UI.wz/UIWindow2.img/UserInfo/ride/backgrnd', Path + 'backgrnd', 0, 191);
  CreateImage('UI.wz/UIWindow2.img/UserInfo/ride/backgrnd2');
  CreateImage('UI.wz/UIWindow2.img/UserInfo/ride/backgrnd3');
  // pet
  CreateAttachForm('UI.wz/UIWindow2.img/UserInfo/pet/backgrnd', Path + 'backgrnd', 0, 191);
  CreateImage('UI.wz/UIWindow2.img/UserInfo/pet/backgrnd2');
  CreateImage('UI.wz/UIWindow2.img/UserInfo/pet/backgrnd3');
  // damage
  CreateAttachForm('UI.wz/UIWindow2.img/UserInfo/damage/backgrnd', Path + 'backgrnd', 0, 191);
  CreateImage('UI.wz/UIWindow2.img/UserInfo/damage/backgrnd2');
  CreateImage('UI.wz/UIWindow2.img/UserInfo/damage/backgrnd3');
  //
   UIButton['UserInfoFormClose'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      UIForm['UI.wz/UIWindow2.img/UserInfo/character/backgrnd'].Visible := False;
      HideForms(['personality','collect', 'ride', 'pet', 'damage']);
      TUserInfoAvatarImage.Show := False;
    end;

  UIButton[Path + 'BtPersonality'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      const Path = 'UI.wz/UIWindow2.img/UserInfo/personality/backgrnd';
      HideForms(['collect', 'ride', 'pet', 'damage']);
      UIForm[Path].Visible := not UIForm[Path].Visible;
    end;
  UIButton[Path + 'BtCollect'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      const Path = 'UI.wz/UIWindow2.img/UserInfo/collect/backgrnd';
      HideForms(['personality', 'ride', 'pet', 'damage']);
      UIForm[Path].Visible := not UIForm[Path].Visible;
    end;
  UIButton[Path + 'BtRide'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      const Path = 'UI.wz/UIWindow2.img/UserInfo/ride/backgrnd';
      HideForms(['personality', 'collect', 'pet', 'damage']);
      UIForm[Path].Visible := not UIForm[Path].Visible;
    end;

  UIButton[Path + 'BtPet'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      const Path = 'UI.wz/UIWindow2.img/UserInfo/pet/backgrnd';
      HideForms(['personality', 'ride', 'collect', 'damage']);
      UIForm[Path].Visible := not UIForm[Path].Visible;
    end;
  UIButton[Path + 'BtDamage'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      const Path = 'UI.wz/UIWindow2.img/UserInfo/damage/backgrnd';
      HideForms(['personality', 'ride', 'pet', 'collect']);
      UIForm[Path].Visible := not UIForm[Path].Visible;
    end;

  CreateForm('UI.wz/GuildUI.img/backgrnd1', 317, 220);
  UIForm['UI.wz/GuildUI.img/backgrnd1'].Visible := False;
  CreateImage('UI.wz/GuildUI.img/backgrnd2');
  CreateImage('UI.wz/GuildUI.img/noGuild/backgrnd');
  CreateButton('UI.wz/GuildUI.img/noGuild/button:Search');
  CreateButton('UI.wz/GuildUI.img/noGuild/button:Make');
  UIButton[Path + 'BtFamily'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      const path = 'UI.wz/GuildUI.img/backgrnd1';
      UIForm[Path].Visible := not UIForm[Path].Visible;
      UIForm[Path].BringToFront;
    end;
  CreateButton('GuildFormClose', 'UI.wz/Basic.img/BtClose3', 514, 5);
  UIButton['GuildFormClose'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      UIForm['UI.wz/GuildUI.img/backgrnd1'].Visible := False;
    end;

end;

end.

