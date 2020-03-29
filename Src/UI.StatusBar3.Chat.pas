unit UI.StatusBar3.Chat;

interface

uses
  PXT.Types, Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, Graphics, ACtrlImages,
  StdCtrls, WZIMGFile, WZArchive, StrUtils, Generics.Collections, WzUtils, AControls, ACtrlEngine,
  ACtrlForms, ACtrlButtons, Global, PXT.Canvas, PXT.Graphics;

procedure CreateMainChat;

implementation

uses
  UI.Utils, ACtrlLabels;


procedure CreateMainChat;
begin
  CreateEmptyForm('MainChat', 100, 430, 409, 400);

  CreateButton('UI.wz/StatusBar3.img/chat/common/chatTarget/all',6,0);
  CreateButton('UI.wz/StatusBar3.img/chat/common/chatTarget/party',56,0);
  CreateButton('UI.wz/StatusBar3.img/chat/common/chatTarget/friend',106,0);
  CreateButton('UI.wz/StatusBar3.img/chat/common/chatTarget/guild',156,0);
  CreateButton('UI.wz/StatusBar3.img/chat/common/chatTarget/association',206,0);
  CreateImage('UI.wz/StatusBar3.img/chat/ingame/view/min/top',1,1,0,33);


end;


end.

