unit OptionsFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, PXT.Canvas, PXT.Graphics, PXT.Types;

type
  TOptionsForm = class(TForm)
    CheckBox1: TCheckBox;
    ScanlineCheckBox: TCheckBox;
    procedure CheckBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
  private
    Hasload: Boolean;
    { Private declarations }
  public
    ScanlineTexture: TTexture;
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

procedure TOptionsForm.FormActivate(Sender: TObject);
var
  LPixels: array of TIntColor;
begin
  if HasLoad then
    Exit;
  HasLoad := True;
  var Parameters: TTextureParameters;
  FillChar(Parameters, SizeOf(TTextureParameters), 0);
  Parameters.Width := 4000;
  Parameters.Height := 4000;
  Parameters.Format := TPixelFormat.RGBA8;
  Parameters.Attributes := TextureMipMapping; //TextureDynamic;
  ScanlineTexture := TextureInit(FDevice, Parameters);
  SetLength(LPixels, Parameters.Width * Parameters.Height);
  for var J := 0 to Parameters.Height - 1 do
  begin
    var LPixel: PIntColor := @LPixels[J * Parameters.Width];
    for var I := 0 to Parameters.Width - 1 do
    begin
      if J mod 2 = 1 then
        LPixel^ := $FFFFFFFF
      else
        LPixel^ := $FFAEAEAE;
      Inc(LPixel);
    end;
  end;
  ScanlineTexture.Update(@LPixels[0], Parameters.Width * SizeOf(TIntColor), 0, ZeroIntRect)
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

