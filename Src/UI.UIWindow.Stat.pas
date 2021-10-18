unit UI.UIWindow.Stat;

interface

uses
  PXT.Types, Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs,
  Graphics, ACtrlImages, StdCtrls, WZIMGFile, WZArchive, StrUtils,
  Generics.Collections, WzUtils, AControls, ACtrlEngine, ACtrlForms,
  ACtrlButtons, Global, PXT.Canvas, PXT.Graphics;

procedure CreateStatForm;

implementation

uses
  UI.Utils, ACtrlLabels;

var
  HyperStatPoint: Integer;

procedure CreateStatForm;
begin
  CreateForm('UI/UIWindow.img/Stat/backgrnd', 417, 200);
  CreateButton('StatFormClose', 'UI/Basic.img/BtClose', 155, 6);
  UIButton['StatFormClose'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      UIForm['UI/UIWindow.img/Stat/backgrnd'].Visible := False;
      UIForm['UI/UIWindow.img/Stat/backgrnd2'].Visible:=False;
    end;

  CreateLabel('Stat/Name', 'SuperGM', 60, 32);
  CreateImage('UI/UIWindow.img/Stat/job/main/3', 1, 1, 62, 55);
  CreateImage('UI/UIWindow.img/Stat/job/sub/3/310', 1, 1, 62, 68);
  CreateLabel('Stat/Level', '255', 60, 79);
  CreateLabel('Stat/Guild', '-', 60, 97);
  CreateLabel('Stat/HP', '5688/5688', 60, 115);
  CreateButton('Stat/HP/BtUp', 'UI/Basic.img/BtUp', 153, 117);
  CreateLabel('Stat/MP', '7322/7322', 60, 133);
  CreateButton('Stat/MP/BtUp', 'UI/Basic.img/BtUp', 153, 135);

  CreateLabel('Stat/Exp', '567899(99.98%)', 60, 152);
  CreateLabel('Stat/Fame', '0', 60, 169);
  CreateLabel('Stat/AbilityPoint', '99', 70, 215);
  CreateButton('UI/UIWindow.img/Stat/BtAuto',95,197);
  //
  CreateLabel('Stat/STR', '2356', 60, 245);
  CreateButton('Stat/Str/BtUp', 'UI/Basic.img/BtUp', 153, 246);
  CreateLabel('Stat/DEX', '1879', 60, 263);
  CreateButton('Stat/DEX/BtUp', 'UI/Basic.img/BtUp', 153, 264);
  CreateLabel('Stat/INT', '465', 60, 281);
  CreateButton('Stat/INT/BtUp', 'UI/Basic.img/BtUp', 153, 282);
  CreateLabel('Stat/LUK', '650', 60, 299);
  CreateButton('Stat/LUK/BtUp', 'UI/Basic.img/BtUp', 153, 300);
  CreateButton('UI/UIWindow.img/Stat/BtDetail', 113, 324);
  //
  CreateAttachForm('UI/UIWindow.img/Stat/backgrnd2', 'UI/UIWindow.img/Stat/backgrnd', 174, 144);
  UIButton['UI/UIWindow.img/Stat/BtDetail'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      UIForm['UI/UIWindow.img/Stat/backgrnd2'].Visible := not UIForm['UI/UIWindow.img/Stat/backgrnd2'].Visible;
    end;
  CreateLabel('Stat/Attack', '1238~2559', 77, 8);
  CreateLabel('Stat/WeaponDef', '983', 77, 26);
  CreateLabel('Stat/Magic', '1337', 77, 44);
  CreateLabel('Stat/MagicDef', '680', 77, 62);
  CreateLabel('Stat/Accuracy', '8015', 77, 80);
  CreateLabel('Stat/AvoidAbility', '471', 77, 98);
  CreateLabel('Stat/Hands', '841', 77, 116);
  CreateLabel('Stat/Speed', '349', 77, 134);
  CreateLabel('Stat/Jump', '652', 77, 152);
  CreateButton('Stat/Form2','UI/Basic.img/BtHide',150,183);
  UIButton['Stat/Form2'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      UIForm['UI/UIWindow.img/Stat/backgrnd2'].Visible := False;
    end;
end;

end.

