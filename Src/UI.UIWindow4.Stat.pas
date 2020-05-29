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

var
  HyperValues: array[0..18] of Integer;
  HyperStatPoint:Integer;

procedure CreateStatForm;
var
  HyperImages: array[0..18] of TAImage;
begin
  const Path = 'UI.wz/UIWindow4.img/Stat/main/';
  CreateForm(Path + 'backgrnd', 417, 200);
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
  CreateLabel('detail/DamageBonus', '69%', 76, 60, lcRed);
  CreateLabel('detail/BossDamage', '54%', 170, 60);
  CreateLabel('detail/FinalDamage', '88%', 76, 78);
  CreateLabel('detail/IgnoreDefence', '99%', 170, 78, lcRed);
  CreateLabel('detail/CriticalRate', '66%', 76, 96);
  CreateLabel('detail/CritDamage', '55.00%', 76, 114);
  CreateLabel('detail/StatusResistance', '76', 76, 132);
  CreateLabel('detail/KnockbackResistance', '4%', 170, 132);
  CreateLabel('detail/Defence', '36871', 76, 150);
  CreateLabel('detail/Speed', '105%', 76, 168);
  CreateLabel('detail/Jump', '107%', 170, 168);
  CreateLabel('metierLine0', 'Item Drop Rate +20%', 23, 227, lcWhite);
  CreateLabel('metierLine1', 'STR +10, DEX +10', 23, 245, lcWhite);
  CreateLabel('metierline2', 'Mesos obtained +999%', 23, 264, lcWhite);
  CreateLabel('detail/HonorExp', '8371', 76, 287);
  CreateButton('UI.wz/UIWindow4.img/Stat/detail/BtAbility');
  CreateButton('UI.wz/UIWindow4.img/Stat/detail/BtHpUp');

   UIButton['UI.wz/UIWindow4.img/Stat/detail/BtHpUp'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
       UIForm['UI.wz/UIWindow4.img/Stat/detail/backgrnd'].Visible := False;
    end;


  UIButton[Path + 'BtDetailOpen'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      const Path = 'UI.wz/UIWindow4.img/Stat/detail/backgrnd';
      UIForm[Path].Visible := not UIForm[Path].Visible;
    end;
  //hyper stat
  CreateAttachForm('UI.wz/UIWindow4.img/HyperStat/Window/backgrnd', Path + 'backgrnd', -188, 0);
  CreateImage('UI.wz/UIWindow4.img/HyperStat/Window/backgrnd2');
  CreateImage('UI.wz/UIWindow4.img/HyperStat/Window/backgrnd3');

  UIButton[Path + 'BtHyperStatOpen'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      const Path = 'UI.wz/UIWindow4.img/HyperStat/Window/backgrnd';
      UIForm[Path].Visible := not UIForm[Path].Visible;
    end;

  for var i := 0 to 18 do
  begin
    CreateImage('UI.wz/UIWindow4.img/HyperStat/Window/statList/800004' + LeftPad(i, 2), 1, 1, 15, 43 + i * 18);
    HyperImages[i] := UIImage['UI.wz/UIWindow4.img/HyperStat/Window/statList/800004' + LeftPad(i, 2)];
    if (HyperImages[i].Top < 30) or (HyperImages[i].Top > 250) then
      HyperImages[i].Visible := False;
    CreateLabel('HyperValue' + i.ToString, '0', 130, 42 + i * 18);
    CreateButton('HyperButton' + i.ToString, 'UI.wz/UIWindow4.img/HyperStat/Window/BtLVup', 147, 43 + i * 18);
    UIButton['HyperButton' + i.ToString].Visible := HyperImages[i].Visible;
    UILabel['HyperValue' + i.ToString].Visible := HyperImages[i].Visible;
    UIButton['HyperButton' + i.ToString].Tag := i;
    UIButton['HyperButton' + i.ToString].OnMouseDown :=
      procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
      begin
        var ButtonTag := TAButton(Sender).Tag;
        HyperValues[ButtonTag] := UILabel['HyperValue' + ButtonTag.ToString].Text.ToInteger;
        Inc(HyperValues[ButtonTag]);
        UILabel['HyperValue' + ButtonTag.ToString].Text := HyperValues[ButtonTag].ToString;
        HyperStatPoint:=UILabel['HyperStatPoint'].Text.ToInteger;
        Dec(HyperStatPoint);
        UILabel['HyperStatPoint'].Text:=HyperStatPoint.ToString;
      end;
  end;
  CreateImage('HyperStatVScr', 'UI.wz/Basic.img/VScr9/enabled/base', 1, 17.7, 163, 42);
  CreateImage('HyperStatVScr/prev0', 'UI.wz/Basic.img/VScr9/enabled/prev0', 1, 1, 163, 42);
  CreateImage('HyperStatVScr/next0', 'UI.wz/Basic.img/VScr9/enabled/next0', 1, 1, 163, 242);
  CreateImage('HyperStatVScr/thumb0', 'UI.wz/Basic.img/VScr9/enabled/thumb0', 1, 1, 163, 53);
  UIImage['HyperStatVScr/thumb0'].Width := 11;
  UIImage['HyperStatVScr/thumb0'].Height := 26;
  UIButton['StatFormClose'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      UIForm['UI.wz/UIWindow4.img/Stat/main/backgrnd'].Visible := False;
      UIForm['UI.wz/UIWindow4.img/Stat/detail/backgrnd'].Visible := False;
      UIForm['UI.wz/UIWindow4.img/HyperStat/Window/backgrnd'].Visible := False;
    end;
  var OnDrag: Boolean;
  UIImage['HyperStatVScr/thumb0'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      OnDrag := True;
    end;
  UIImage['HyperStatVScr/thumb0'].OnMouseUP :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      OnDrag := False;
    end;
  var Thumb := UIImage['HyperStatVScr/thumb0'];
  Thumb.OnMouseMove :=
    procedure(Sender: TObject; Shift: TShiftState; X, Y: Integer)
    begin
      if OnDrag then
      begin
        Thumb.Top := Y - Thumb.Parent.Top + 1000 - 10;
        if Thumb.Top < 53 then
          Thumb.Top := 53;
        if Thumb.Top > 216 then
          Thumb.Top := 216;
        for var i := 0 to 18 do
        begin
          HyperImages[i].Top := 80 + (18 * i) - Trunc(Thumb.Top * 0.78) div 18 * 18;
          if (HyperImages[i].Top > 30) and (HyperImages[i].Top < 250) then
            HyperImages[i].Visible := True
          else
            HyperImages[i].Visible := False;
          UIButton['HyperButton' + i.ToString].Top := HyperImages[i].Top-1;
          UILabel['HyperValue' + i.ToString].Top := HyperImages[i].Top - 2;
          UIButton['HyperButton' + i.ToString].Visible := HyperImages[i].Visible;
          UILabel['HyperValue' + i.ToString].Visible := HyperImages[i].Visible;
        end;
      end;
    end;
   CreateButton('UI.wz/UIWindow4.img/HyperStat/Window/BtReduce');
   UIButton['UI.wz/UIWindow4.img/HyperStat/Window/BtReduce'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      UIForm['UI.wz/UIWindow4.img/HyperStat/Window/backgrnd'].Visible := False;
    end;

   CreateButton('UI.wz/UIWindow4.img/HyperStat/Window/BtReset');
   CreateLabel('HyperStatPoint', '99', 156, 266);
end;


end.

