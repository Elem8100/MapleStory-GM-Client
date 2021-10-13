unit UI.UIWindow.Equip;

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


procedure CreateEquipForm;
begin
  CreateForm('UI/UIWindow.img/Equip/backgrnd', 317, 320);
  UIForm['UI/UIWindow.img/Equip/backgrnd'].Visible := False;
  CreateButton('EquipFormClose', 'UI/Basic.img/BtClose', 155, 6);
  UIButton['EquipFormClose'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      UIForm['UI/UIWindow.img/Equip/backgrnd'].Visible := False;
      UIForm['UI/UIWindow.img/Equip/pet'].Visible := False;
    end;

  CreateButton('UI/UIWindow.img/Equip/BtDetail', 100, 281);
  UIButton['UI/UIWindow.img/Equip/BtDetail'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      UIForm['UI/UIWindow.img/Equip/pet'].Visible := not UIForm['UI/UIWindow.img/Equip/pet'].Visible;
    end;

  CreateAttachForm('UI/UIWindow.img/Equip/pet', 'UI/UIWindow.img/Equip/backgrnd', 175, 123);
  CreateButton('UI/UIWindow.img/Equip/BtPet1', 28, 165);
  CreateButton('UI/UIWindow.img/Equip/BtPet2', 63, 165);
  CreateButton('UI/UIWindow.img/Equip/BtPet3', 98, 165);
  CreateButton('UI/UIWindow.img/Equip/BtPetEquipHide', 161, 167);
  UIButton['UI/UIWindow.img/Equip/BtPetEquipHide'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      UIForm['UI/UIWindow.img/Equip/pet'].Visible := False;
    end;
end;

end.

