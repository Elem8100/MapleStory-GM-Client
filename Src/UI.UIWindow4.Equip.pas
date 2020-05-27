unit UI.UIWindow4.Equip;

interface

uses
  PXT.Types, Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, Graphics, ACtrlImages,
  StdCtrls, WZIMGFile, WZArchive, StrUtils, Generics.Collections, WzUtils, AControls, ACtrlEngine,
  ACtrlForms, ACtrlButtons, Global, PXT.Canvas, PXT.Graphics;

procedure CreateEquipForm;

implementation

uses
  UI.Utils, ACtrlLabels;

var
  HasLoad: Boolean;
  TabIndex: Integer;
  S1: array of Integer;
  S2: array of string;

procedure SelectTab(Index: Integer);
var
  I: Integer;
begin
  const Path = 'UI.wz/UIWindow4.img/Equip/';
  for I := 0 to 3 do
    UIImage[Path + 'Tab/enabled/' + I.ToString].Visible := False;
  UIImage[Path + 'Tab/enabled/' + Index.ToString].Visible := True;

  //Equip
  UIButton[Path + 'Equip/BtSlot'].Visible := False;
  UIButton[Path + 'Equip/BtZeroCash'].Visible := False;
  UIButton[Path + 'Equip/BtZeroWp'].Visible := False;
  for I := 0 to High(S1) do
    UIImage[Path + 'Equip/Slots/' + S1[I].ToString].Visible := False;
  UIImage[Path + 'Equip/Slots/30_2'].Visible := False;
  UIImage[Path + 'Equip/backgrnd'].Visible := False;
  //cash
  UIButton[Path + 'Cash/BtShop'].Visible := False;
  for I := 101 to 117 do
  begin
    if I = 114 then
      Continue;
    UIImage[Path + 'Cash/Slots/' + I.ToString].Visible := False;
  end;
  UIImage[Path + 'Cash/Slots/150'].Visible := False;
  UIImage[Path + 'Cash/Slots/151'].Visible := False;
  UIImage[Path + 'Cash/Slots/face'].Visible := False;
  UIImage[Path + 'Cash/Slots/hair'].Visible := False;
  UIImage[Path + 'Cash/backgrnd'].Visible := False;

  //pet
  UIImage[Path + 'Pet/backgrnd'].Visible := False;
  UIImage[Path + 'Pet/PetFood/0'].Visible := False;
  UIImage[Path + 'Pet/ItemRoot/0'].Visible := False;
  UIButton[Path + 'Pet/BtConsumeSetting'].Visible := False;
  UIButton[Path + 'Pet/BtException'].Visible := False;
  for I := 0 to High(S2) do
    UIImage[Path + 'Pet/Slots/' + S2[I]].Visible := False;
  //AD
  UIButton[Path + 'Android/BtShop'].Visible := False;
  for I := 1200 to 1206 do
    UIImage[Path + 'Android/Slots/' + I.ToString].Visible := False;
  UIImage[Path + 'Android/backgrnd'].Visible := False;

  case Index of
    0:
      begin
        //Equip
        UIButton[Path + 'Equip/BtSlot'].Visible := True;
        UIButton[Path + 'Equip/BtZeroCash'].Visible := True;
        UIButton[Path + 'Equip/BtZeroWp'].Visible := True;
        for I := 0 to High(S1) do
          UIImage[Path + 'Equip/Slots/' + S1[I].ToString].Visible := True;
        UIImage[Path + 'Equip/Slots/30_2'].Visible := True;
        UIImage[Path + 'Equip/backgrnd'].Visible := True;
      end;
    1:
      begin
        //cash
        UIButton[Path + 'Cash/BtShop'].Visible := True;
        for I := 101 to 117 do
        begin
          if I = 114 then
            Continue;
          UIImage[Path + 'Cash/Slots/' + I.ToString].Visible := True;
        end;
        UIImage[Path + 'Cash/Slots/150'].Visible := True;
        UIImage[Path + 'Cash/Slots/151'].Visible := True;
        UIImage[Path + 'Cash/Slots/face'].Visible := True;
        UIImage[Path + 'Cash/Slots/hair'].Visible := True;
        UIImage[Path + 'Cash/backgrnd'].Visible := True;
      end;
    2:
      begin
        //pet
        UIImage[Path + 'Pet/backgrnd'].Visible := True;
        UIImage[Path + 'Pet/PetFood/0'].Visible := True;
        UIImage[Path + 'Pet/ItemRoot/0'].Visible := True;
        UIButton[Path + 'Pet/BtConsumeSetting'].Visible := True;
        UIButton[Path + 'Pet/BtException'].Visible := True;
        for I := 0 to High(S2) do
          UIImage[Path + 'Pet/Slots/' + S2[I]].Visible := True;
      end;
    3:
      begin
       //AD
        UIButton[Path + 'Android/BtShop'].Visible := True;
        for I := 1200 to 1206 do
          UIImage[Path + 'Android/Slots/' + I.ToString].Visible := True;

        UIImage[Path + 'Android/backgrnd'].Visible := True;
      end;

  end;
end;

procedure CreateEquipForm;
begin
  const Path = 'UI.wz/UIWindow4.img/Equip/';
  CreateForm(Path + 'backgrnd', 617, 320);
  CreateButton('EquipFormClose', 'UI.wz/Basic.img/BtClose3', 190, 7);

  CreateImage(Path + 'backgrnd2');
  for var I := 0 to 3 do
  begin
    CreateImage(Path + 'Tab/disabled/' + I.ToString);
    CreateImage(Path + 'Tab/enabled/' + I.ToString);
  end;

  CreateImage(Path + 'tabbar');
  UIButton['EquipFormClose'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      UIForm['UI.wz/UIWindow4.img/Equip/backgrnd'].Visible := False;
    end;
  //Equip
  CreateButtons(Path + 'Equip', ['BtSlot', 'BtZeroCash', 'BtZeroWp']);
  S1 := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 16, 17, 21, 22, 23, 26, 27, 28, 29, 30, 31,
    49, 50, 51, 52, 53, 54, 55, 56, 61, 65];
  for var I := 0 to High(S1) do
    CreateImage(Path + 'Equip/Slots/' + S1[I].ToString);
  CreateImage(Path + 'Equip/Slots/30_2');
  CreateImage(Path + 'Equip/backgrnd');
  //cash
  CreateButton(Path + 'Cash/BtShop');
  for var I := 101 to 117 do
  begin
    if I = 114 then
      Continue;
    CreateImage(Path + 'Cash/Slots/' + I.ToString);
  end;
  CreateImage(Path + 'Cash/Slots/150');
  CreateImage(Path + 'Cash/Slots/151');
  CreateImage(Path + 'Cash/Slots/face');
  CreateImage(Path + 'Cash/Slots/hair');
  CreateImage(Path + 'Cash/backgrnd');
  //pet
  CreateImage(Path + 'Pet/backgrnd');
  CreateImage(Path + 'Pet/PetFood/0');
  CreateImage(Path + 'Pet/ItemRoot/0');
  CreateButtons(Path + 'Pet', ['BtConsumeSetting', 'BtException']);
  S2 := ['114', '124', '125', '130', '138', '200', '201', '202', 'Pet1', 'Pet2', 'Pet3', 'Skill1', 'Skill2', 'Skill3'];
  for var I := 0 to High(S2) do
    CreateImage(Path + 'Pet/Slots/' + S2[I]);
  //AD
  CreateButton(Path + 'Android/BtShop');
  for var I := 1200 to 1206 do
    CreateImage(Path + 'Android/Slots/' + I.ToString);
  CreateImage(Path + 'Android/backgrnd');

  if not HasLoad then
  begin
    SelectTab(0);
    HasLoad := True;
  end;

  UIImage[Path + 'Tab/disabled/0'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      SelectTab(0);
    end;
  UIImage[Path + 'Tab/disabled/1'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      SelectTab(1);
    end;
  UIImage[Path + 'Tab/disabled/2'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      SelectTab(2);
    end;
  UIImage[Path + 'Tab/disabled/3'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      SelectTab(3);
    end;

end;

end.

