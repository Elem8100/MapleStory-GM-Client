unit UI.UIWindow2.EventList;

interface

uses
  PXT.Types, Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, Graphics, ACtrlImages,
  StdCtrls, WZIMGFile, WZArchive, StrUtils, Generics.Collections, WzUtils, AControls, ACtrlEngine,
  ACtrlForms, ACtrlButtons, Global, PXT.Canvas, PXT.Graphics;

procedure CreateEventListForm;

implementation

uses
  UI.Utils, ACtrlLabels, MainUnit, MapleCharacter, MapleChair, Tamingmob;

procedure CreateEventListForm;
begin
  const Path = 'UI/UIWindow2.img/EventList/main/';
  CreateForm(Path + 'backgrnd', 417, 200);
  CreateButton('EventListFormClose', 'UI/Basic.img/BtClose3', 394, 5);
  UIButton['EventListFormClose'].OnMouseDown :=
    procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer)
    begin
      UIForm['UI/UIWindow2.img/EventList/main/backgrnd'].Visible := False;
    end;
  CreateImage(Path + 'backgrnd2');
  CreateImage('EventNormal1', Path + 'event/normal', 1, 1, 11, 87);
  CreateImage('EventNormal2', Path + 'event/normal', 1, 1, 11, 212);
  CreateImage('EventNormal3', Path + 'event/normal', 1, 1, 11, 337);
  CreateImage('EventListVScr', 'UI/Basic.img/VScr9/enabled/base', 1, 30.5, 395, 90);
  CreateImage('EventListVScr/prev0', 'UI/Basic.img/VScr9/enabled/prev0', 1, 1, 395, 87);
  CreateImage('EventListVScr/next0', 'UI/Basic.img/VScr9/enabled/next0', 1, 1, 395, 448);
  CreateImage('EventListVScr/thumb0', 'UI/Basic.img/VScr9/enabled/thumb0', 1, 1, 395, 98);
end;

end.

