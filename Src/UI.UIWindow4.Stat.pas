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
  CreateLabel('aa', '', 100, 100);
end;

end.

