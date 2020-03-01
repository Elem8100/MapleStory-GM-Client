unit RenderFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TRenderForm = class(TForm)
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  RenderForm: TRenderForm;

implementation

uses
  Global, PXT.Types, UI.Utils;
{$R *.dfm}

procedure TRenderForm.FormResize(Sender: TObject);
begin
  if FDevice.Initialized then
    FDevice.Resize(Point2i(ClientWidth, ClientHeight));

  var Path := 'UI.wz/StatusBar3.img/mainBar/menu';
  if UIForm.ContainsKey(Path) then
  begin
    case DisplaySize.X of
      800:
        begin
          UIForm[Path].Left := 1000 + 505;
          UIForm[Path].Top := 1000 + 552;
        end;
      1024:
        begin
          UIForm[Path].Left := 1000 + 617;
          UIForm[Path].Top := 1000 + 720;
        end;
    else
      begin
        UIForm[Path].Left := (1000+Displaysize.X  div 2)+105;
        UIForm[Path].Top := (Displaysize.Y + 1000 div 2)+452;
      end;
    end;
    var Path2:= 'UI.wz/StatusBar3.img/mainBar/submenu/title/';
    UIForm[Path2+'event'].Left:= UIForm[Path].Left-10;
    UIForm[Path2+'event'].Top:= UIForm[Path].Top-90;
    UIForm[Path2+'character'].Left:= UIForm[Path].Left+20;
    UIForm[Path2+'character'].Top:= UIForm[Path].Top-195;
    UIForm[Path2+'community'].Left:= UIForm[Path].Left+55;
    UIForm[Path2+'community'].Top:= UIForm[Path].Top-135;
    UIForm[Path2+'setting'].Left:= UIForm[Path].Left+90;
    UIForm[Path2+'setting'].Top:= UIForm[Path].Top-125;
    UIForm[Path2+'menu'].Left:= UIForm[Path].Left+125;
    UIForm[Path2+'menu'].Top:= UIForm[Path].Top-355;
  end;

end;

end.

