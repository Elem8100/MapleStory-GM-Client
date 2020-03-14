unit UI.UIWindow4.Stat;

interface

uses
  PXT.Types, Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, Graphics, ACtrlImages,
  StdCtrls, WZIMGFile, WZArchive, StrUtils, Generics.Collections, WzUtils, AControls, ACtrlEngine,
  ACtrlForms, ACtrlButtons, Global, PXT.Canvas, PXT.Graphics;

procedure CreateStatForm;

implementation

uses
  UI.Utils, ACtrlLabels;

procedure CreateStatForm;
begin
  const Path = 'UI.wz/UIWindow4.img/Stat/main/';
  CreateForm(Path + 'backgrnd', 617, 320, True);
  CreateImage(Path + 'backgrnd2');
  CreateImage(Path + 'backgrnd3');
  CreateButton(Path + 'BtAuto');
  CreateLabel('Stat/Name', 'SuperGM', 75, 30);
  CreateLabel('Stat/Class', 'SuperGM', 75, 48);
  CreateLabel('Stat/Guild', 'Ravers', 75, 66);
  CreateLabel('Stat/Fame', '123500', 75, 84);
  CreateLabel('Stat/Damage', '17920099~25000000', 75, 102);
  CreateLabel('Stat/HP', '19999/19999', 75, 120);
  CreateLabel('Stat/MP', '波丁爸庫員', 75, 138);
  CreateLabel('Stat/AbilityPoint', '999', 70, 180);
  CreateLabel('Stat/Str', '波丁爸庫員', 75, 208);
 //// CreateLabel('Stat/Dex', '波丁爸庫員', 80, 100);
 // CreateLabel('Stat/Int', '波丁爸庫員', 80, 100);
//  CreateLabel('Stat/Luk', '123', 100, 80);

  CreateButton(Path + 'BtDetailOpen');
  CreateButton(Path + 'BtHyperStatOpen');
  UIButton[Path + 'backgrnd'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      UIButton['UI.wz/UIWindow4.img/Stat/main/backgrnd'].Parent.Visible := False;
    end;
end;

end.

