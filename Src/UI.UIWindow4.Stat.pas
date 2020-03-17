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
  CreateForm(Path + 'backgrnd', 617, 320);
  CreateButton('StatFormClose', 'UI.wz/Basic.img/BtClose3', 190, 7);

  CreateImage(Path + 'backgrnd2');
  CreateImage(Path + 'backgrnd3');
  CreateButton(Path + 'BtAuto');
  CreateLabel('Stat/Name', 'SuperGM', 75, 30);
  CreateLabel('Stat/Class', 'SuperGM', 75, 48);
  CreateLabel('Stat/Guild', 'Ravers', 75, 66);
  CreateLabel('Stat/Fame', '123500', 75, 84);
  CreateLabel('Stat/Damage', '17920099~25000000', 75, 102);
  CreateLabel('Stat/HP', '19999/19999', 75, 120);
  CreateLabel('Stat/MP', '29999/29999', 75, 138);
  CreateLabel('Stat/AbilityPoint', '999', 70, 180);
  CreateLabel('Stat/Str', '22417', 75, 207);
  CreateLabel('Stat/Dex', '16885', 75, 225);
  CreateLabel('Stat/Int', '3325', 75, 243);
  CreateLabel('Stat/Luk', '9123', 75, 261);
  CreateButton(Path + 'BtDetailOpen');
  CreateButton(Path + 'BtHyperStatOpen');
  //stat detail
  CreateAttachForm('UI.wz/UIWindow4.img/Stat/detail/backgrnd', Path + 'backgrnd', 212, 0);
  CreateImage('UI.wz/UIWindow4.img/Stat/detail/backgrnd2');
  CreateImage('UI.wz/UIWindow4.img/Stat/detail/backgrnd3');
  CreateImage('UI.wz/UIWindow4.img/Stat/detail/abilityTitle/rare/0');
  CreateImage('UI.wz/UIWindow4.img/Stat/detail/backgrnd4');
  CreateImage('UI.wz/UIWindow4.img/Stat/detail/metierLine/activated/0');
  CreateImage('UI.wz/UIWindow4.img/Stat/detail/metierLine/activated/1');
  CreateImage('UI.wz/UIWindow4.img/Stat/detail/metierLine/activated/2');

  CreateLabel('detailStat/Damage', '226845', 76, 42);

  CreateLabel('detail/DamageBonus', '69%', 76, 60,lcRed);

  CreateLabel('detail/BossDamage', '54%', 175, 60);
  {
  CreateLabel('detail/FinalDamage', '88%', 75, 84);
  CreateLabel('detail/IgnoreDefence', '299%', 75, 102);
  CreateLabel('detail/CriticalRate', '66%', 75, 120,lcRed);
  CreateLabel('detail/CritDamage', '55.00%', 75, 138);
  CreateLabel('detail/StatusResistance', '2687', 75, 207);
  CreateLabel('detail/KnockbackResistance', '4%', 75, 225);
  CreateLabel('detail/Defence', '36871', 75, 138);
  CreateLabel('detail/Speed', '105%', 70, 180);
  CreateLabel('detail/Jump', '107%', 75, 207);
   }

  CreateLabel('metierLine0','Item Drop Rate +20%', 23, 227,lcWhite);
  CreateLabel('metierLine1', 'STR +10, DEX +10', 23, 245,lcWhite);
  CreateLabel('metierline2', 'Mesos obtained +999%', 23, 264,lcWhite);
  UIButton['StatFormClose'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      UIForm['UI.wz/UIWindow4.img/Stat/main/backgrnd'].Visible := False;
      UIForm['UI.wz/UIWindow4.img/Stat/detail/backgrnd'].Visible := False;
    end;

  UIButton[Path + 'BtDetailOpen'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      const Path = 'UI.wz/UIWindow4.img/Stat/detail/backgrnd';
      UIForm[Path].Visible := not UIForm[Path].Visible;
    end;

end;

end.

