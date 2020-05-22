unit UI.UIWindow2.Item;

interface

uses
  PXT.Types, Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, Graphics, ACtrlImages,
  StdCtrls, WZIMGFile, WZArchive, StrUtils, Generics.Collections, WzUtils, AControls, ACtrlEngine,
  ACtrlForms, ACtrlButtons, Global, PXT.Canvas, PXT.Graphics;

procedure CreateItemForm;

implementation

uses
  UI.Utils, ACtrlLabels, MainUnit, MapleCharacter, MapleChair, Tamingmob;

procedure SelectTab(Index: Integer);
begin
  const Path = 'UI.wz/UIWindow2.img/Item/Tab/enabled/';
  for var i := 0 to 4 do
    UIImage[Path + i.ToString].Visible := False;
  UIImage[Path + Index.ToString].Visible := True;
end;

procedure CreateItemForm;
begin
  const Path = 'UI.wz/UIWindow2.img/Item/';
  CreateForm(Path + 'backgrnd', 417, 200);
  CreateButton('ItemFormClose', 'UI.wz/Basic.img/BtClose3', 150, 5);
  UIButton['ItemFormClose'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      UIForm['UI.wz/UIWindow2.img/Item/backgrnd'].Visible := False;
    end;
  CreateImage(Path + 'backgrnd2');
  CreateImage(Path + 'backgrnd3');
  for var i := 0 to 4 do
  begin
    CreateImage(Path + 'Tab/disabled/' + i.ToString);
    CreateImage(Path + 'Tab/enabled/' + i.ToString);
  end;
  SelectTab(0);
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
  CreateButton(Path + 'BtCoin3');
  CreateButton(Path + 'BtFull3');
  CreateButton(Path + 'BtSmall3');
  CreateButton(Path + 'BtPoint0');

  CreateButton(Path + 'BtBits3');
  CreateButton(Path + 'BtDisassemble3');
  CreateButton(Path + 'BtExtract3');
  CreateButton(Path + 'BtGather3');
  CreateButton(Path + 'BtSort3');
  CreateButton(Path + 'BtToad3');
  CreateButton(Path + 'BtUpgrade3');
  CreateButton(Path + 'BtAppraise3');
  CreateButton(Path + 'BtPot3');
end;

end.

