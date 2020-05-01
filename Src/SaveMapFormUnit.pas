unit SaveMapFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Jpeg, PngImage, MapleMap, StrUtils;

type
  TSaveMapForm = class(TForm)
    Label1: TLabel;
    ComboBox1: TComboBox;
    Label2: TLabel;
    ComboBox2: TComboBox;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private

    { Private declarations }
  public
    { Public declarations }
  end;

var
  SaveMapForm: TSaveMapForm;

implementation

uses
  MainUnit, Global, AsphyreTypes, MapBack, PXT.Graphics, PXT.Types;
{$R *.dfm}

function StrReplace(const oldChars, newChars: array of Char; const str: string): string;
begin
  Assert(Length(oldChars) = Length(newChars));
  Result := str;
  for var i := 0 to high(oldChars) do
    Result := StringReplace(Result, oldChars[i], newChars[i], [rfReplaceAll])
end;

procedure TSaveMapForm.Button1Click(Sender: TObject);
var
  i: Integer;
  MapName: string;
  MapWidth, MapHeight: Integer;
begin
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
  TMap.SaveMap := True;
  var SaveTexture: TTexture;
  var Params: TTextureParameters;
  FillChar(Params, SizeOf(TTextureParameters), 0);
  Params.Width := MapWidth;
  Params.Height := MapHeight;
  Params.Format := TPixelFormat.RGBA8;
  Params.Attributes := TextureDrawable or  TexturePremultipliedAlpha;
  SaveTexture := TextureInit(FDevice, Params);

  FDevice.BeginScene;
  SaveTexture.BeginScene;
  GameCanvas.BeginScene;
  BackEngine[0].Draw;
  SpriteEngine.Draw;
  BackEngine[1].Draw;
  BackEngine[0].Move(1);
  BackEngine[1].Move(1);
  GameCanvas.EndScene;
  SaveTexture.EndScene;
  FDevice.EndScene;
  //repeat
  FDevice.BeginScene;
  SaveTexture.BeginScene;
  GameCanvas.BeginScene;
  BackEngine[0].Draw;
  SpriteEngine.Draw;
  BackEngine[1].Draw;
  BackEngine[0].Move(1);
  BackEngine[1].Move(1);
  GameCanvas.EndScene;
  SaveTexture.EndScene;
  FDevice.EndScene;

  TMap.SaveMap := False;
  TMapBack.ResetPos := True;

  SpriteEngine.VisibleWidth := DisplaySize.x;
  SpriteEngine.VisibleHeight := DisplaySize.y;
  for i := 0 to 1 do
  begin
    BackEngine[i].VisibleWidth := DisplaySize.x;
    BackEngine[i].VisibleHeight := DisplaySize.y;
  end;

  if TMap.MapNamelist.ContainsKey(TMap.SaveMapID) then
    MapName := TMap.MapNameList[TMap.SaveMapID].MapName;

  MapName := StrReplace(['<', '>'], ['(', ')'], MapName);
  if MapName.Contains('(') then
    MapName := LeftStr(MapName, 9);

  var FileName := ExtractFilePath(ParamStr(0)) + TMap.SaveMapID + '-' + MapName;
  SaveTexture.SaveToFile(FileName + '.png', nil, 0, ZeroIntRect);
  SaveTexture.Free;
  ComboBox1.ItemIndex := 0;
  ComboBox2.ItemIndex := 0;
  SaveMapForm.Close;
  MessageDlg('Àx¦s  ' + TMap.SaveMapID + '-' + MapName + '.PNG' + '§¹¦¨', mtinformation, [mbOk], 0);
end;

procedure TSaveMapForm.FormCreate(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TSaveMapForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Key := 0;
end;

end.

