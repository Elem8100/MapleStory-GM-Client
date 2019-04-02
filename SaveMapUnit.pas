unit SaveMapUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Jpeg,PngImage, MapleMap;

type
  TSaveMapForm = class(TForm)
    Label1: TLabel;
    ComboBox1: TComboBox;
    Label2: TLabel;
    ComboBox2: TComboBox;
    Label3: TLabel;
    ComboBox3: TComboBox;
    Button1: TButton;
    Label4: TLabel;
    ComboBox4: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    procedure MapTargetEvent(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SaveMapForm: TSaveMapForm;

implementation

uses MainUnit, Global, LockRenderTarget, AsphyreTypes, MapBack;
{$R *.dfm}

procedure TSaveMapForm.MapTargetEvent(Sender: TObject);
begin
  BackEngine[0].Draw;
  SpriteEngine.Draw;
  BackEngine[1].Draw;
end;

function StrReplace(const oldChars, newChars: array of Char; const str: string): string;
var
  i: Integer;
begin
  Assert(Length(oldChars) = Length(newChars));
  Result := str;
  for i := 0 to high(oldChars) do
    Result := StringReplace(Result, oldChars[i], newChars[i], [rfReplaceAll])
end;

procedure TSaveMapForm.Button1Click(Sender: TObject);
type
  TRGB32 = record
    B, G, R, A: Byte;
  end;

  TRGB32Array = array [0 .. MaxInt div SizeOf(TRGB32) - 1] of TRGB32;
  PRGB32Array = ^TRGB32Array;

var
  Index: Integer;
  pDest: Pointer;
  nPitch: Integer;
  A, R, G, B: Byte;
  i, j: Integer;
  MapName: string;
  PSrcTex: PLongWord;
  Line: PRGB32Array;
  Bmp: TBitmap;
  Jpg: TJpegImage;
  Png:TPngImage;
  MapWidth, MapHeight: Integer;
  Color: Cardinal;
begin

  FreeAndNil(LockRenderTargets);
  MapWidth := TMap.Right - TMap.Left;

  if TMap.Info.ContainsKey('VRLeft') then
    MapHeight := TMap.Bottom - TMap.Top
  else
    MapHeight := TMap.SaveMapBottom - TMap.Top;

  SpriteEngine.WorldX := TMap.Left;
  SpriteEngine.WorldY := TMap.Top;

  SpriteEngine.VisibleWidth := MapWidth;
  SpriteEngine.VisibleHeight := MapHeight;

  for i := 0 to 1 do
  begin
    BackEngine[i].VisibleWidth := MapWidth;
    BackEngine[i].VisibleHeight := MapHeight;
  end;

  LockRenderTargets := TLockableRenderTarget.Create;
  LockRenderTargets.SetSize(MapWidth, MapHeight);
  Index := LockRenderTargets.Add(1, MapWidth, MapHeight, apf_A8R8G8B8, True, True);

  TMap.SaveMap := True;
  BackEngine[0].Move(1);
  BackEngine[1].Move(1);
  case ComboBox3.ItemIndex of
    0:
      Color := $FFC8C8C8;
    1:
      Color := $FFFFFFFF;
    2:
      Color := $FF000000;
    3:
      Color := $FF00C8FF;
  end;
  GameDevice.RenderTo(MapTargetEvent, Color, True, LockRenderTargets[Index]);

  TMap.SaveMap := False;
  TMapBack.ResetPos := True;

  Bmp := TBitmap.Create;
  Bmp.PixelFormat := pf32Bit;
  Bmp.AlphaFormat := afPremultiplied;
  Bmp.Width := MapWidth;
  Bmp.Height := MapHeight;

  LockRenderTargets.Lock(Index, pDest, nPitch);
  PSrcTex := pDest;
  for j := 0 to MapHeight - 1 do
  begin
    Line := Bmp.Scanline[j];
    for i := 0 to MapWidth - 1 do
    begin
      A := GetA(PSrcTex^);
      R := GetR(PSrcTex^);
      G := GetG(PSrcTex^);
      B := GetB(PSrcTex^);
      Line[i].B := B;
      Line[i].G := G;
      Line[i].R := R;
      Line[i].A := A;
      Inc(PSrcTex);
    end;
  end;

  LockRenderTargets.UnLock;

  SpriteEngine.VisibleWidth := DisplaySize.x;
  SpriteEngine.VisibleHeight := DisplaySize.y;
  for i := 0 to 1 do
  begin
    BackEngine[i].VisibleWidth := DisplaySize.x;
    BackEngine[i].VisibleHeight := DisplaySize.y;
  end;

  if MainForm.StringIDs.ContainsKey(TMap.SaveMapID) then
    MapName := MainForm.StringIDs[TMap.SaveMapID];
  MapName := StrReplace(['<', '>'], ['(', ')'], MapName);

  var FileExt:string;
  var FileName:=ExtractFilePath(ParamStr(0)) + TMap.SaveMapID + '-' + MapName;

  case ComboBox4.ItemIndex of
    0:
    begin
      FileExt:='.jpg';
      Jpg := TJpegImage.Create;
      Jpg.Assign(Bmp);
      Jpg.CompressionQuality := 100;
      Jpg.SaveToFile(FileName+'.jpg');
    end;
    1:
    begin
      fileExt:='.png';
      Png := TPngImage.Create;
      Png.Assign(Bmp);
      Png.SaveToFile(FileName+'.png');
     end;
    2:
    begin
      fileExt:='.bmp';
      Bmp.SaveToFile(FileName+'.bmp');
    end;
  end;

  FreeAndNil(Bmp);
  FreeAndNil(Jpg);
  FreeAndNil(Png);
  ComboBox1.ItemIndex := 0;
  ComboBox2.ItemIndex := 0;
  SaveMapForm.Close;
  MessageDlg('Save  ' + TMap.SaveMapID + '-' + MapName + FileExt+'Completed', mtinformation, [mbOk], 0);
end;

procedure TSaveMapForm.FormCreate(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TSaveMapForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 Key:=0;
end;

end.
