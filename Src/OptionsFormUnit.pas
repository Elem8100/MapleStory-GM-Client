unit OptionsFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, PXT.Canvas, PXT.Graphics, PXT.Types;

type
  TOptionsForm = class(TForm)
    CheckBox1: TCheckBox;
    procedure CheckBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
  public

    { Public declarations }
  end;

var
  OptionsForm: TOptionsForm;

implementation

uses
  BassHandler, MapleMap, Global;
{$R *.dfm}

procedure TOptionsForm.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
    TMap.ActiveBass.Mute
  else
    TMap.ActiveBass.ReStart;
  ActiveControl := nil;
end;

procedure TOptionsForm.FormClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TOptionsForm.FormCreate(Sender: TObject);
begin
  Left := ((Screen.Width - Width) div 2);
  Top := (Screen.Height - Height) div 2;
end;

procedure TOptionsForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_MENU then
    Key := 0;
end;

end.

