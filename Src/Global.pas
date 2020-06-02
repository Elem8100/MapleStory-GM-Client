unit Global;

interface

uses
  Windows, SysUtils, StrUtils, PXT.Sprites, WZArchive, Generics.Collections, WZIMGFile,
  WZDirectory, Classes, Math, AsphyreFontsAlt, AbstractCanvas, LockRenderTarget, BassHandler,
  DX9Textures, Vectors2px, AbstractDevices, AsphyreKeyboard, AsphyreRenderTargets, Tools,
  System.Types, ACtrlEngine,PXT.Types,
  PXT.Graphics,PXT.Canvas;

type
  TGameMode = (gmPlay, gmView);

  TTimers = class
  private
    class var
      TimerList: TDictionary<string, Integer>;
  public
    class procedure AddTimer(Name: string);
    class procedure DoTick(Interval: Integer; TimerName: string; Proc: TProc);
    class procedure Create; overload;
  end;

var
  WzPath: string;
  FDevice: TDevice;
  GameDevice2: TDevice;
  GameDevice3: TDevice;
  DisplaySize: TPoint2i;
  GameFont: TTextRenderer;
  GameCanvas: TGameCanvas;
  AvatarPanelTexture: TTexture;
  GameTargetMobInfo: TAsphyreRenderTargets = nil;
  IsKMS: Boolean;
  UIEngine: TControlEngine = nil;
  SpriteEngine: TSpriteEngine;
  BackEngine: array[0..1] of TSpriteEngine;
 // FontsAlt: TAsphyreFontsAlt;
  Keyboard: TAsphyreKeyboard;
  MobWZ, Mob2WZ, Mob001WZ, NPCWZ, MapWz, Map2Wz, Map001Wz,Map002Wz, MorphWz, StringWZ, SoundWZ, Sound2Wz,
    CharacterWZ, BaseWZ, UIWZ, ReactorWz, EffectWz, SkillWZ, Skill001Wz, ItemWZ, EtcWZ: TWZArchive;
  GameMode: TGameMode;
  Sounds: TObjectList<TBassHandler>;
  Damage: Integer;
  NewPosition, CurrentPosition, SpriteEngineVelX: Double;
  NewPositionY, CurrentPositionY, SpriteEngineVelY: Double;
  CharData, Data: TDictionary<string, Variant>;
  EquipData, WzData: TObjectDictionary<string, TWZIMGEntry>;
  Images: TDictionary<TWZIMGEntry, TTexture>;
  EquipImages: TDictionary<TWZIMGEntry, TTexture>;
function LeftPad(Value:Integer; Length:integer=8): string;

function IsNumber(AStr: string): Boolean;

procedure PlaySounds(Img, Path: string);

function Cos256(I: Integer): Double;

function Sin256(I: Integer): Double;

function GetAngle256(const X1, Y1, X2, Y2: Integer): Integer;

function TrimS(Stemp: string): string;

function IDToInt(ID: string): string;

function Add7(Name: string): string;

function Add9(Name: string): string;

implementation

var
  CosTable256: array[0..255] of Double;

function LeftPad(Value:Integer; Length:integer=8): string;
begin
  Result := RightStr(StringOfChar('0',Length) + Value.ToString, Length );
end;

function IsNumber(AStr: string): Boolean;
var
  Value: Double;
  Code: Integer;
begin
  Val(AStr, Value, Code);
  Result := Code = 0;
end;

class procedure TTimers.AddTimer(Name: string);
begin
  TimerList.Add(Name, 0);
end;

class procedure TTimers.Create;
begin
  TimerList := TDictionary<string, Integer>.Create;
end;

class procedure TTimers.DoTick(Interval: Integer; TimerName: string; Proc: TProc);
begin
  if GetTickcount - TimerList[TimerName] > Interval then
  begin
    Proc;
    TimerList[TimerName] := GetTickcount;
  end;
end;

procedure PlaySounds(Img, Path: string);
var
  NewSound: TBassHandler;
  Entry: TWZIMGEntry;
begin
  Entry := SoundWZ.GetImgFile(Img + '.img').Root.Get(Path);
  if Entry = nil then
    Exit;

  if Entry.DataType = mdtUOL then
  begin
    Entry := TWZIMGEntry(Entry.Parent).Get(Entry.Data);
    if Entry.DataType = mdtUOL then
      Entry := TWZIMGEntry(Entry.Parent).Get(Entry.Data);
  end;

  NewSound := TBassHandler.Create(SoundWZ.Reader.Stream, Entry.Sound.Offset, Entry.Sound.DataLength);
  NewSound.Play;
  Sounds.Add(NewSound);

end;

function Cos256(I: Integer): Double;
begin
  Result := CosTable256[I and 255];
end;

function Sin256(I: Integer): Double;
begin
  Result := CosTable256[(I + 192) and 255];
end;

procedure InitCosTable;
var
  I: Integer;
begin
  for I := 0 to 255 do
    CosTable256[I] := Cos((I / 256) * 2 * PI);
end;

function GetAngle256(const X1, Y1, X2, Y2: Integer): Integer;
const
  PiConv256 = -128.0 / PI; // ~ 40.743665431
begin
  if (X2 = X1) then
  begin
    Result := 128;
    if (Y1 < Y2) then
      Result := 0;
    Exit;
  end;
  Result := Round(ArcTan2(X2 - X1, Y2 - Y1) * PiConv256) and $FF;
end;

function IDToInt(ID: string): string;
var
  S: Integer;
begin
  S := ID.ToInteger;
  Result := S.ToString;
end;

function TrimS(Stemp: string): string;
const
  Remove =[' ', '.', '/', #13, #10];
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(Stemp) do
  begin
    if not (Stemp[I] in Remove) then
      Result := Result + Stemp[I];
  end;
end;

function Add7(Name: string): string;
begin
  case Length(Name) of
    4:
      Result := '000' + Name;
    5:
      Result := '00' + Name;
    6:
      Result := '0' + Name;
    7:
      Result := Name;
  end;
end;

function Add9(Name: string): string;
begin
  case Length(Name) of
    1:
      Result := '00000000' + Name;
    5:
      Result := '0000' + Name;
    7:
      Result := '00' + Name;
    9:
      Result := Name;
  end;
end;

initialization
  TTimers.Create;
  InitCosTable;
  Sounds := TObjectList<TBassHandler>.Create;
  WzData := TObjectDictionary<string, TWZIMGEntry>.Create;
  EquipData := TObjectDictionary<string, TWZIMGEntry>.Create;
  CharData := TDictionary<string, Variant>.Create;
  Data := TDictionary<string, Variant>.Create;

finalization
  TTimers.TimerList.Free;

end.

