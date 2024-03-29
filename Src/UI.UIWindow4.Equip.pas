﻿unit UI.UIWindow4.Equip;

interface

uses
  PXT.Types, Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs,
  Graphics, ACtrlImages, StdCtrls, WZIMGFile, WZArchive, StrUtils,
  Generics.Collections, WzUtils, AControls, ACtrlEngine, ACtrlForms,
  ACtrlButtons, Global, PXT.Canvas, PXT.Graphics;

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
  const Path = 'UI/UIWindow4.img/Equip/';
  for I := 0 to 3 do
    UIImage[Path + 'Tab/enabled/' + I.ToString].Visible := False;
  UIImage[Path + 'Tab/enabled/' + Index.ToString].Visible := True;

  //Equip
  UIButton[Path + 'Equip/BtSlot'].Visible := False;
  UIButton[Path + 'Equip/BtZeroCash'].Visible := False;
  UIButton[Path + 'Equip/BtZeroWp'].Visible := False;
  for I := 0 to High(S1) do
    if UIImage.ContainsKey(Path + 'Equip/Slots/' + S1[I].ToString) then
      UIImage[Path + 'Equip/Slots/' + S1[I].ToString].Visible := False;
  if UIImage.ContainsKey(Path + 'Equip/Slots/30_2') then
    UIImage[Path + 'Equip/Slots/30_2'].Visible := False;
  UIImage[Path + 'Equip/backgrnd'].Visible := False;

  //cash
  if HasImgEntry('UI/UIWindow4.img/Equip/Cash/BtShop') then
    UIButton[Path + 'Cash/BtShop'].Visible := False
  else
    UIButton['UI/UIWindow4.img/Equip/Cash/BtBeautyRoom'].Visible := False;

  for I := 101 to 128 do
  begin
    if I = 114 then
      Continue;
    if UIImage.ContainsKey(Path + 'Cash/Slots/' + I.ToString) then
      UIImage[Path + 'Cash/Slots/' + I.ToString].Visible := False;
  end;

  if UIImage.ContainsKey(Path + 'Cash/Slots/150') then
    UIImage[Path + 'Cash/Slots/150'].Visible := False;
  if UIImage.ContainsKey(Path + 'Cash/Slots/151') then
    UIImage[Path + 'Cash/Slots/151'].Visible := False;
  UIImage[Path + 'Cash/Slots/face'].Visible := False;
  UIImage[Path + 'Cash/Slots/hair'].Visible := False;
  UIImage[Path + 'Cash/backgrnd'].Visible := False;
  ;
  //pet
  UIImage[Path + 'Pet/backgrnd'].Visible := False;
  UIImage[Path + 'Pet/PetFood/0'].Visible := False;
  UIImage[Path + 'Pet/ItemRoot/0'].Visible := False;
  UIButton[Path + 'Pet/BtConsumeSetting'].Visible := False;
  UIButton[Path + 'Pet/BtException'].Visible := False;
  for I := 0 to High(S2) do
    if UIImage.ContainsKey(Path + 'Pet/Slots/' + S2[I]) then
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
          if UIImage.ContainsKey(Path + 'Equip/Slots/' + S1[I].ToString) then
            UIImage[Path + 'Equip/Slots/' + S1[I].ToString].Visible := True;
        UIImage[Path + 'Equip/Slots/30_2'].Visible := True;
        UIImage[Path + 'Equip/backgrnd'].Visible := True;
      end;
    1:
      begin
        //cash
        if HasImgEntry('UI/UIWindow4.img/Equip/Cash/BtShop') then
          UIButton[Path + 'Cash/BtShop'].Visible := True
        else
          UIButton['UI/UIWindow4.img/Equip/Cash/BtBeautyRoom'].Visible := True;

        for I := 101 to 128 do
        begin
          if I = 114 then
            Continue;
          if UIImage.ContainsKey(Path + 'Cash/Slots/' + I.ToString) then
            UIImage[Path + 'Cash/Slots/' + I.ToString].Visible := True;
        end;
        if UIImage.ContainsKey(Path + 'Cash/Slots/150') then
          UIImage[Path + 'Cash/Slots/150'].Visible := True;
        if UIImage.ContainsKey(Path + 'Cash/Slots/151') then
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
          if UIImage.ContainsKey(Path + 'Pet/Slots/' + S2[I]) then
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
  const Path = 'UI/UIWindow4.img/Equip/';
  CreateForm(Path + 'backgrnd', 317, 220);
  CreateButton('EquipFormClose', 'UI/Basic.img/BtClose3', 210, 5);

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
      UIForm['UI/UIWindow4.img/Equip/backgrnd'].Visible := False;
    end;


  //Equip
  CreateButtons(Path + 'Equip', ['BtSlot', 'BtZeroCash', 'BtZeroWp']);
  S1 := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 16, 17, 21, 22, 23, 26, 27, 28, 29, 30, 31, 33, 34, 35, 36, 37, 38, 50, 51, 52, 53, 54, 55, 56, 61, 65];
  for var I := 0 to High(S1) do
    CreateImage(Path + 'Equip/Slots/' + S1[I].ToString);
  CreateImage(Path + 'Equip/Slots/30_2');
  CreateImage(Path + 'Equip/backgrnd');

  //cash

  if HasImgEntry('UI/UIWindow4.img/Equip/Cash/BtShop') then
    CreateButton(Path + 'Cash/BtShop')
  else
    CreateButton('UI/UIWindow4.img/Equip/Cash/BtBeautyRoom');
  for var I := 101 to 128 do
  begin
    if I = 114 then
      Continue;
    CreateImage(Path + 'Cash/Slots/' + I.ToString);
  end;
  CreateImages(Path + 'Cash/Slots', ['150', '151', 'face', 'hair']);
  CreateImage(Path + 'Cash/backgrnd');
  //pet
  CreateImage(Path + 'Pet/backgrnd');
  CreateImage(Path + 'Pet/PetFood/0');
  CreateImage(Path + 'Pet/ItemRoot/0');
  CreateButtons(Path + 'Pet', ['BtConsumeSetting', 'BtException']);
  S2 := ['114', '121', '122', '123', '124', '125', '126', '130', '138', '200', '201', '202', 'Pet1', 'Pet2', 'Pet3', 'Skill1', 'Skill2', 'Skill3'];
  for var I := 0 to High(S2) do
    CreateImage(Path + 'Pet/Slots/' + S2[I]);


  //AD
  CreateButton(Path + 'Android/BtShop');
  CreateImages(Path + 'Android/Slots', ['1200', '1201', '1202', '1203', '1204', '1205', '1206']);
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

