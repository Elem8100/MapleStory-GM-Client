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

procedure CreateEquipForm;
begin
  const Path = 'UI.wz/UIWindow4.img/Equip/';
  CreateForm(Path + 'backgrnd', 617, 320);
  CreateButton('EquipFormClose', 'UI.wz/Basic.img/BtClose3', 190, 7);

  CreateImage(Path + 'backgrnd2');

  for var i := 0 to 3 do
  begin
    CreateImage(Path + 'Tab/disabled/' + i.ToString);
    CreateImage(Path + 'Tab/enabled/' + i.ToString);
  end;
  CreateAttachForm(Path + 'equip/backgrnd', Path + 'backgrnd', 55, 69, True);
  UIButton['EquipFormClose'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      UIForm['UI.wz/UIWindow4.img/Equip/backgrnd'].Visible := False;
    end;
  UIform[path + 'backgrnd'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin

      UIForm['UI.wz/UIWindow4.img/Equip/equip/backgrnd'].BringToFront;
    end;

  if not HasLoad then
  begin
//    SelectTab(0);
    HasLoad := True;
  end;

end;

end.

