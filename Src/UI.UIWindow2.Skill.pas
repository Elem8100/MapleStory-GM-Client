unit UI.UIWindow2.Skill;

interface

uses
  PXT.Types, Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, Graphics, ACtrlImages,
  StdCtrls, WZIMGFile, WZArchive, StrUtils, Generics.Collections, WzUtils, AControls, ACtrlEngine,
  ACtrlForms, ACtrlButtons, Global, PXT.Canvas, PXT.Graphics;

procedure CreateSkillForm;

implementation

uses
  UI.Utils, ACtrlLabels;

var
  HasLoad: Boolean;

procedure SelectTab(Index: Integer);
var
  I: Integer;
begin
  const Path = 'UI/UIWindow2.img/Skill/main/';
  for I := 0 to 5 do
    UIImage[Path + 'Tab/enabled/' + I.ToString].Visible := False;
  UIImage[Path + 'Tab/enabled/' + Index.ToString].Visible := True;

  case Index of
    0:
      begin

      end;
    1:
      begin

      end;
    2:
      begin

      end;
    3:
      begin

      end;

  end;
end;

procedure CreateSkillForm;
begin
  const Path = 'UI/UIWindow2.img/Skill/main/';
  CreateForm(Path + 'backgrnd', 417, 220);
  CreateButton('SkillFormClose', 'UI/Basic.img/BtClose3', 295, 7);

  CreateImage(Path + 'backgrnd2');
  CreateImage(Path + 'backgrnd3');
  for var I := 0 to 5 do
  begin
    CreateImage(Path + 'Tab/disabled/' + I.ToString);
    CreateImage(Path + 'Tab/enabled/' + I.ToString);
  end;

  UIButton['SkillFormClose'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      UIForm['UI/UIWindow2.img/Skill/main/backgrnd'].Visible := False;
    end;

  CreateButtons('UI/UIWindow2.img/Skill/main', ['BtHyper', 'BtGuildSkill', 'BtRide', 'BtMacro']);

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

  UIImage[Path + 'Tab/disabled/4'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      SelectTab(4);
    end;
  UIImage[Path + 'Tab/disabled/5'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      SelectTab(5);
    end;

end;

end.

