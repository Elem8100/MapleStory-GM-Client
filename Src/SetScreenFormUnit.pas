unit SetScreenFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,PXT.Graphics;

type
  TSetScreenForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    Button16: TButton;
    Button17: TButton;
    ScanlineCheckBox: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    Hasload: Boolean;
    { Private declarations }
  public
     ScanlineTexture: TTexture;
    { Public declarations }
  end;

var
  SetScreenForm: TSetScreenForm;

implementation

uses
  MainUnit, RenderFormUnit, Global, PXT.Types, MapleMap, MapBack;
{$R *.dfm}

function RenderScale(Width, Height: Integer): TPoint;
begin
  RenderForm.ClientWidth := Width;
  RenderForm.ClientHeight := Height;
  RenderForm.Left := 217;
  RenderForm.Top := 78;
  Result.X := Width;
  Result.Y := Height;
end;

procedure TSetScreenForm.Button1Click(Sender: TObject);
begin
  MainForm.BorderStyle := bsSingle;
  var Size: TPoint;
  case TButton(Sender).Tag of
    0:
      begin
        DisplaySize := Point2i(800, 600);
        Size := RenderScale(1024, 768);
      end;
    1:
      begin
        DisplaySize := Point2i(800, 600);
        Size := RenderScale(1366, 768);
      end;
    2:
      begin
        DisplaySize := Point2i(800, 600);
        Size := RenderScale(1600, 900);
      end;
    3:
      begin
        DisplaySize := Point2i(800, 600);
        Size := RenderScale(1600, 1200);
      end;
    4:
      begin
        DisplaySize := Point2i(800, 600);
        Size := RenderScale(1920, 1080);
      end;
    5:
      begin
        DisplaySize := Point2i(800, 600);
        Size := RenderScale(2560, 1440);
      end;
    6:
      begin
        DisplaySize := Point2i(1024, 768);
        Size := RenderScale(1600, 900);
      end;
    7:
      begin
        DisplaySize := Point2i(1024, 768);
        Size := RenderScale(1920, 1080);
      end;
    8:
      begin
        DisplaySize := Point2i(1024, 768);
        Size := RenderScale(2560, 1440);
      end;
    9:
      begin
        DisplaySize := Point2i(1280, 720);
        Size := RenderScale(1600, 900);
      end;
    10:
      begin
        DisplaySize := Point2i(1280, 720);
        Size := RenderScale(1920, 1080);
      end;
    11:
      begin
        DisplaySize := Point2i(1280, 720);
        Size := RenderScale(2560, 1440);
      end;
    12:
      begin
        DisplaySize := Point2i(1366, 768);
        Size := RenderScale(1600, 900);
      end;
    13:
      begin
        DisplaySize := Point2i(1366, 768);
        Size := RenderScale(1920, 1080);
      end;
    14:
      begin
        DisplaySize := Point2i(1366, 768);
        Size := RenderScale(2560, 1440);
      end;
    15:
      begin
        DisplaySize := Point2i(1600, 900);
        Size := RenderScale(1920, 1080);
      end;
    16:
      begin
        DisplaySize := Point2i(1600, 900);
        Size := RenderScale(2560, 1440);
      end;
  end;

  MainForm.Width := Size.X + 232;
  MainForm.Height := Size.Y + 143;

  MainForm.Shape1.Width := Size.X + 4;
  MainForm.Shape1.Height := Size.Y + 4;
  MainForm.AdvGroupBox2.Width := Size.X + 5;
  MainForm.PageControl1.Height := Size.Y - 130;
  SpriteEngine.VisibleWidth := Size.X;
  SpriteEngine.VisibleHeight := Size.Y;
  for var I := 0 to 1 do
  begin
    BackEngine[I].VisibleWidth := DisplaySize.X;
    BackEngine[I].VisibleHeight := DisplaySize.Y;
  end;
  MainForm.CreateTexture(MainForm.FullScreenTexture, DisplaySize.X, DisplaySize.Y, False);
  TMap.OffsetY := (DisplaySize.Y - 600) div 2;
  TMapBack.ResetPos := True;
  MainForm.Left := (Screen.Width - MainForm.Width) div 2;
  MainForm.Top := (Screen.Height - MainForm.Height) div 2;
  ActiveControl := nil;

  MainForm.ScreenMode := smScale;
end;

procedure TSetScreenForm.FormActivate(Sender: TObject);
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

procedure TSetScreenForm.FormClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TSetScreenForm.FormCreate(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TSetScreenForm.FormDestroy(Sender: TObject);
begin
  ScanlineTexture.Free;
end;

procedure TSetScreenForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_MENU then
    Key := 0;
end;

end.

