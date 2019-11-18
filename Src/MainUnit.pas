unit MainUnit;

interface

{$IF CompilerVersion >= 21.0}
{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$IFEND}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, ToolWin, ComCtrls, AdvGroupBox,
  AdvToolBtn, ToolPanels, FolderDialog, AdvGrid, Grids, BaseGrid, WZArchive, WZDirectory, Generics.Collections,
  WZIMGFile, AdvObj, KeyHandler, WZReader, StrUtils, PngImage, Jpeg, {, Reactor,}
  Footholds, bass, BassHandler, MapPortal, AdvUtil, Mob2, Npc, {UI}
  MapleCharacter, {Boss,} Vectors2px, AbstractTextures, AbstractDevices, AbstractCanvas,
  AsphyreTimer, AsphyreSprite, AsphyreKeyboard, AsphyreFontsAlt, DirectInput, AsphyreFactory,
  DX9Providers, AsphyreTypes, Global, DX9Textures, AsphyreRenderTargets, LockRenderTarget, MapleMap,
  WzUtils, System.Types, Vcl.Buttons, scControls, scExtControls;

type
  TMainForm = class(TForm)
    LoadMapButton: TButton;
    AdvToolPanel1: TAdvToolPanel;
    AdvGroupBox2: TAdvGroupBox;
    Shape1: TShape;
    SearchMapEdit: TEdit;
    Label1: TLabel;
    OpenMSFolder: TButton;
    Label2: TLabel;
    FolderDialog1: TFolderDialog;
    Image1: TImage;
    Shape2: TShape;
    ComboKey: TComboBox;
    Label3: TLabel;
    ComboBox1: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    ComboBox2: TComboBox;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Grid: TAdvStringGrid;
    WorldMapGrid: TAdvStringGrid;
    scScrollPanel1: TscScrollPanel;
    PicInfoButton: TSpeedButton;
    SaveMapButton: TSpeedButton;
    AvatarButton1: TSpeedButton;
    DropMobButton1: TSpeedButton;
    DropNpcButton1: TSpeedButton;
    ChairButton1: TSpeedButton;
    TamingMobButton: TSpeedButton;
    CashButton: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton1: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    MedalButton: TSpeedButton;
    NickNameButton: TSpeedButton;
    LabelRingButton: TSpeedButton;
    PetButton: TSpeedButton;

    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure LoadMapButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure SearchMapEditChange(Sender: TObject);
    procedure OpenMSFolderClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var KEY: Word; Shift: TShiftState);
    procedure ComboKeyChange(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ComboBox1CloseUp(Sender: TObject);
    procedure ComboBox2CloseUp(Sender: TObject);
    procedure ComboKeyCloseUp(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure AdvToolPanel1Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure WorldMapGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure ChairButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure CashButtonClick(Sender: TObject);
    procedure TamingMobButtonClick(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure DropMobButton1Click(Sender: TObject);
    procedure DropNpcButton1Click(Sender: TObject);
    procedure SaveMapButtonClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure PicInfoButtonClick(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure MedalButtonClick(Sender: TObject);
    procedure AvatarButton1Click(Sender: TObject);
    procedure NickNameButtonClick(Sender: TObject);
    procedure LabelRingButtonClick(Sender: TObject);
    procedure FamiliarButtonClick(Sender: TObject);
    procedure PetButtonClick(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
  private
    OldX, OldY: Integer;
    MoveOn: Boolean;
    CircleList: TList<TShape>;
    procedure OnDeviceCreate(Sender: TObject; Param: Pointer; var Handled: Boolean);
    procedure TimerEvent(Sender: TObject);
    procedure RenderEvent(Sender: TObject);
    procedure RenderAvatar(Sender: TObject);
    procedure RenderAvatarPanel(Sender: TObject);
    procedure CirCleMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    { Private declarations }
  public
    StringIDs: TDictionary<string, string>;

    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  AddMobUnit, SaveMapUnit, ImageInfoUnit, RenderFormUnit, MapBack, MobInfo, AvatarUnit,
  ShowOptionUnit, Tools, AddNpcFormUnit, ChairformUnit, MorphFormUnit, MedalTagFormUnit,
  NickNameTagFormUnit, DamageSkinFormUnit, WorldMapFormUnit, CashFormUnit, TamingMobFormUnit,
  NameTag, MapleEffect, TamingMob, MapleChair, LabelRingFormUnit, PetFormUnit, Pet, FamiliarFormUnit,
  MonsterFamiliar, SkillFormUnit, Skill;
{$R *.dfm}

procedure TMainForm.FamiliarButtonClick(Sender: TObject);
begin
  if (not HasImgFile('String.wz/FamiliarSkill.img')) and (not HasImgFile('String.wz/Familiar.img')) then
  begin
    MessageDlg('Old Wz not supported', mtInformation, [mbOK], 0);
    Exit;
  end
  else
    FamiliarForm.Show;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Randomize;
  DisplaySize := Point(800, 600);
  GameMode := gmPlay;
  // Indicate that we're using DirectX 9
  Factory.UseProvider(idDirectX9);
  // Create Asphyre components in run-time.
  GameDevice := Factory.CreateDevice();
  GameCanvas := Factory.CreateCanvas();
  GameTargets := TAsphyreRenderTargets.Create();
  Keyboard := TAsphyreKeyboard.Create(MainForm);
  Keyboard.Foreground := False;
  GameDevice.WindowHandle := Self.Handle;
  GameDevice.Size := DisplaySize;
  GameDevice.Windowed := True;
  GameDevice.VSync := True;
  GameCanvas.MipMapping := True;
  GameCanvas.Antialias := True;
  GameDevice.VertexProcessing := vptSoftware;
  EventDeviceCreate.Subscribe(OnDeviceCreate, 0);
  CircleList := TList<TShape>.Create;
  // Attempt to initialize Asphyre device.
  if (not GameDevice.Initialize()) then
  begin
    ShowMessage('Failed to initialize Asphyre device.');
    Application.Terminate();
    Exit;
  end;
  Images := TObjectDictionary<TWZIMGEntry, TDX9LockableTexture>.Create([doOwnsValues]);
  EquipImages := TObjectDictionary<TWZIMGEntry, TDX9LockableTexture>.Create([doOwnsValues]);
  SpriteEngine := TSpriteEngine.Create(nil);
  SpriteEngine.Canvas := GameCanvas;
  SpriteEngine.VisibleWidth := 800;
  SpriteEngine.VisibleHeight := 600;
  for var i := 0 to 1 do
  begin
    BackEngine[i] := TSpriteEngine.Create(nil);
    BackEngine[i].Canvas := GameCanvas;
    BackEngine[i].VisibleWidth := 850;
  end;
  // Create rendering timer.
  Timer.OnTimer := TimerEvent;
  // Timer.OnProcess := ProcessEvent;
  Timer.Speed := 60.0;
  Timer.MaxFPS := 4000;
  Timer.Enabled := True;
  TWZReader.EncryptionIV := 0;
  TTimers.AddTimer('FreeSounds');
  TTimers.AddTimer('DropMobTicks');

  var strList := TList<string>.Create;
  var n: DWORD := 0;
  var Mode: TDevMode;

  while EnumDisplaySettings(nil, n, Mode) do
  begin
    var Res := IntToStr(Mode.dmPelsWidth) + 'X' + IntToStr(Mode.dmPelsHeight);
    if not strList.contains(Res) then
      if Mode.dmPelsWidth >= 800 then
        ComboBox1.Items.Add(Res);
    Inc(n);
    strList.Add(Res);
  end;
  strList.Free;

  if ComboBox1.Items.Count <= 2 then
  begin
    var List := ['800X600', '1024X768', '1152X864', '1280X720', '1280X768', '1280X800', '1280X960',
      '1280X1024', '1360X768', '1366X768', '1600X900', '1600X1024', '1600X1200', '1680X1050',
      '1920X1080', '1440X900', '1400X1050', '2560X1080', '2560X1440', '3440X1440', '3840X1080',
      '3840X1600', '3840X2160'];
    for var I in List do
      ComboBox1.Items.Add(i);
  end;

  ComboBox1.ItemIndex := 1;
  Application.HintPause := 0;
  Application.HintHidePause := 5010;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(LockRenderTargets);
  FreeAndNil(GameCanvas);
  FreeAndNil(GameDevice);
  FreeAndNil(GameTargets);
  FreeAndNil(SpriteEngine);
  FreeAndNil(Keyboard);
  BackEngine[0].Free;
  BackEngine[1].Free;
  TFootholdTree.This.Free;
  Keyboard.Free;
  FreeAndNil(FontsAlt);
  FreeAndNil(CharData);
  MapWz.Free;
  if Map2Wz <> nil then
    Map2Wz.Free;
  if Map001Wz <> nil then
    Map001Wz.Free;
  NPCWZ.Free;
  StringWZ.Free;
  UIWZ.Free;
  SkillWZ.Free;
  if Skill001Wz <> nil then
    Skill001Wz.Free;
  MobWZ.Free;
  if Mob2WZ <> nil then
    Mob2WZ.Free;
  if Mob001WZ <> nil then
    Mob001WZ.Free;
  ItemWZ.Free;
  CharacterWZ.Free;
  EffectWz.Free;
  ReactorWz.Free;
  SoundWZ.Free;
  MorphWz.Free;
  BaseWZ.Free;
  EtcWZ.Free;
  Images.Free;
  EquipImages.Free;
  EquipData.Free;
  WzData.Free;
  CharData.Free;
  Sounds.Free;
  Sound2Wz.Free;
  Data.Free;
  CircleList.Free;
  // DropList
  StringIDs.Free;

  //ReportMemoryLeaksOnShutdown := True;
end;

procedure TMainForm.OnDeviceCreate(Sender: TObject; Param: Pointer; var Handled: Boolean);
begin
  FontsAlt := TAsphyreFontsAlt.Create(Self);
  FontsAlt.Add();
  FontsAlt[0].SIZE := 13;
  FontsAlt[0].Style := [feBold];
  FontsAlt[0].FontName := 'Tahoma';
  //
  FontsAlt.Add();
  FontsAlt[1].SIZE := 15;
  FontsAlt[1].FontName := 'Tahoma';
  //
  FontsAlt.Add();
  FontsAlt[2].SIZE := 12;
  FontsAlt[2].FontName := 'Tahoma';
  //
  FontsAlt.Add();
  FontsAlt[3].SIZE := 13;
  FontsAlt[3].FontName := 'Tahoma';
   //
  FontsAlt.Add();
  FontsAlt[4].SIZE := 15;
  FontsAlt[4].Style := [feBold];
  FontsAlt[4].FontName := 'Tahoma';

  FontsAlt.UpdateAll;

end;

procedure TMainForm.FormKeyDown(Sender: TObject; var KEY: Word; Shift: TShiftState);
begin
  if (GameDevice.Windowed = False) and (KEY = VK_ESCAPE) then
  begin
    FontsAlt.UpdateAll;
    GameDevice.Windowed := True;
    Width := DisplaySize.X;
    Height := DisplaySize.Y;
    Width := DisplaySize.X + 232;
    Height := DisplaySize.Y + 143;
    Left := (Screen.Width - Width) div 2;
    Top := (Screen.Height - Height) div 2;
    TNpc.ReDrawTarget := True;
    TMedalTag.ReDraw;
    TNickNameTag.ReDraw;
    TLabelRingTag.ReDraw;
    TPetNameTag.ReDraw;
    TFamiliarNameTag.ReDraw;
  end;
  if KEY = VK_MENU then
    KEY := 0;
end;

procedure TMainForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MoveOn := True;
  OldX := X;
  OldY := Y;
end;

procedure TMainForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if MoveOn then
  begin
    Left := (Left - OldX) + X;
    Top := (Top - OldY) + Y;
  end;
end;

procedure TMainForm.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MoveOn := False;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  ComboBox1.OnChange(Self);
end;

function Timex: Real;
{$J+}
const
  Start: Int64 = 0;
  frequency: Int64 = 0;
{$J-}
var
  Counter: Int64;
begin
  if Start = 0 then
  begin
    QueryPerformanceCounter(Start);
    QueryPerformanceFrequency(frequency);
    Result := 0;
  end;
  Counter := 0;
  QueryPerformanceCounter(Counter);
  Result := (Counter - Start) / frequency;
end;

var
  CurrentTime: Double;
  Accumulator: Double;
  NewTime, DeltaTime: Double;

procedure TMainForm.TamingMobButtonClick(Sender: TObject);
begin
  TamingMobForm.Show;
end;

procedure TMainForm.TimerEvent(Sender: TObject);
const
  dt = 1 / 60;
begin
  NewTime := Timex;
  DeltaTime := NewTime - CurrentTime;
  if DeltaTime > 0.016666 then
    DeltaTime := 0.016666;
  CurrentTime := NewTime;
  Accumulator := Accumulator + DeltaTime;

  while (Accumulator >= dt) do
  begin
    if TMap.ReLoad then
    begin
      TMap.LoadMap(TMap.ID);
      TMap.ReLoad := False;
    end;
    SpriteEngine.Dead;
    SpriteEngine.Move(1);
    Keyboard.Update;
    NewPosition := SpriteEngine.WorldX;
    SpriteEngineVelX := NewPosition - CurrentPosition;
    CurrentPosition := SpriteEngine.WorldX;
    NewPositionY := SpriteEngine.WorldY;
    SpriteEngineVelY := NewPositionY - CurrentPositionY;
    CurrentPositionY := SpriteEngine.WorldY;

    BackEngine[0].Move(1);
    BackEngine[1].Move(1);
    Accumulator := Accumulator - dt;
  end;

  if GameMode = gmView then
  begin
    if (Keyboard.KEY[DIK_LEFT]) then
      SpriteEngine.WorldX := SpriteEngine.WorldX - 6;
    if (Keyboard.KEY[DIK_RIGHT]) then
      SpriteEngine.WorldX := SpriteEngine.WorldX + 6;
    if (Keyboard.KEY[DIK_UP]) then
      SpriteEngine.WorldY := SpriteEngine.WorldY - 6;
    if (Keyboard.KEY[DIK_DOWN]) then
      SpriteEngine.WorldY := SpriteEngine.WorldY + 6;

    if SpriteEngine.WorldX > TMap.Right - DisplaySize.X then
      SpriteEngine.WorldX := TMap.Right - DisplaySize.X;
    if SpriteEngine.WorldX < TMap.Left then
      SpriteEngine.WorldX := TMap.Left;
    if SpriteEngine.WorldY > TMap.Bottom - DisplaySize.Y then
      SpriteEngine.WorldY := TMap.Bottom - DisplaySize.Y;
    if SpriteEngine.WorldY < TMap.Top then
      SpriteEngine.WorldY := TMap.Top;
  end;

  GameDevice.Render(RenderForm.Handle, RenderEvent, $000000);

  if AvatarForm.Active then
  begin
    GameDevice.RenderTo(RenderAvatar, 0, True, AvatarTargets[TPlayer.AvatarPanelIndex]);
    GameDevice.Render(AvatarForm.Panel1.Handle, RenderAvatarPanel, $FFFFFFFF);
  end;

  if (TMapleChair.IsUse) then
    if (Keyboard.KeyPressed[DIK_LEFT]) or (Keyboard.KeyPressed[DIK_RIGHT]) then
    begin
      TMapleChair.Delete;
      TTamingMob.Delete;
      TItemEffect.Delete(Chair);
    end;

end;

procedure TMainForm.CirCleMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
var
  Portals: TPortalInfo;
  PX, PY: Integer;
  BelowFH: TFoothold;
  Below: TPoint;
begin

  var ID := TShape(Sender).HelpKeyword;
  if Length(ID) < 1 then
    Exit;
  if TMap.ID = ID then
    Exit;
  WorldMapForm.Canvas.Font.Size := 18;
  WorldMapForm.Canvas.TextOut(150, 150, 'Loading...');

  TMap.ID := ID;
  TMap.LoadMap(TMap.ID);
  if not SaveMapButton.Enabled then
  begin
    for var i := 0 to ComponentCount - 1 do
      if (Components[i] is TSpeedButton) then
        TSpeedButton(Components[i]).Enabled := True;
    OpenMSFolder.Enabled := False;
    ComboKey.Enabled := False;
  end;

  for Portals in TMapPortal.PortalList do
    if (Portals.PortalType = 0) then
    begin
      PX := Portals.X;
      PY := Portals.Y;
      Break;
    end;
  Player.X := PX;
  Player.Y := PY - 10;
  Below := TFootholdTree.This.FindBelow(Point(PX, PY - 2), BelowFH);
  Player.FH := BelowFH;
  Player.JumpState := jsFalling;
  SpriteEngine.WorldX := PX - DisplaySize.X / 2;
  SpriteEngine.WorldY := PY - (DisplaySize.Y / 2) - 100;
  if SpriteEngine.WorldX > TMap.Right then
    SpriteEngine.WorldX := TMap.Right;
  if SpriteEngine.WorldX < TMap.Left then
    SpriteEngine.WorldX := TMap.Left;
  if SpriteEngine.WorldY > TMap.Bottom then
    SpriteEngine.WorldY := TMap.Bottom;
  if SpriteEngine.WorldY < TMap.Top then
    SpriteEngine.WorldY := TMap.Top;

  TMap.SaveMapID := TMap.ID;
 // Caption := Variant(GetTickCount - T);
  WorldMapForm.Repaint;

  Timer.Enabled := True;

  if not SaveMapButton.Enabled then
  begin
    for var i := 0 to ComponentCount - 1 do
      if (Components[i] is TButton) then
        TButton(Components[i]).Enabled := True;
    OpenMSFolder.Enabled := False;
    ComboKey.Enabled := False;
  end;

  ActiveControl := nil;

end;

procedure TMainForm.WorldMapGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  for var Iter in CircleList do
    Iter.Free;
  CircleList.Clear;

  if not WorldMapForm.Showing then
    WorldMapForm.Show;

  var Entry := GetImgEntry('Map.wz/WorldMap/' + WorldMapGrid.Cells[0, ARow] + '/');
  var Bmp := Entry.Get('BaseImg/0').Canvas.DumpBmp;
  var W, H: Integer;
  W := Bmp.Width;
  H := Bmp.Height;
  WorldMapForm.Width := W + 6;
  WorldMapForm.Height := H + 33;
  WorldMapForm.Image1.Picture.Assign(Bmp);
  Bmp.Free;
  var Origin := Entry.Get('BaseImg/0/origin').Vector;

  for var Iter in Entry.Get('MapList').Children do
  begin
    var Spot := Iter.Get('spot').Vector;
    var Circle := TShape.Create(Self);
    var ID: string := Add9(Iter.Get('mapNo/0', ''));
    Circle.Parent := WorldMapForm;
    Circle.Left := W - Origin.X + Spot.X - 10;
    Circle.Top := H - Origin.Y + Spot.Y - 12;
    Circle.Shape := stCircle;
    Circle.Brush.Color := clLime;
    Circle.ShowHint := True;
    if StringIDs.ContainsKey(ID) then
      Circle.Hint := ID + '-' + StringIDs[ID];

    Circle.Width := 23;
    Circle.Height := 25;
    Circle.HelpKeyword := ID;
    Circle.OnMouseDown := CirCleMouseDown;
    CircleList.Add(Circle);
  end;
  ActiveControl := nil;
end;

procedure TMainForm.RenderEvent(Sender: TObject);
begin
  TTimers.DoTick(1000, 'FreeSounds',
    procedure
    begin
      for var Sound in Sounds do
        if not Sound.IsPlaying then
          Sounds.Remove(Sound);
    end);

  if TMap.FirstLoad then
    TTimers.DoTick(7500, 'DropMobTicks',
      procedure
      begin
        var Count: Integer := 0;
        for var Iter in SpriteEngine.SpriteList do
          if Iter is TMob then
            Inc(Count);
        if Count < (TMap.WzMobCount div 2) then
          TMob.CreateMapMobs;
      end);

  BackEngine[0].Draw;
  SpriteEngine.Draw;
  BackEngine[1].Draw;

  if TMap.ShowFPS then
    FontsAlt[0].TextOut('FPS: ' + IntToStr(Timer.FrameRate), 10, 10, cRGB1(255, 0, 0));
  if TMap.ShowMobInfo then
    GameCanvas.Draw(GameTargetMobInfo[0], 0, 20, 1, False, 255, 255, 255, 255);
  if TMap.FadeScreen.DoFade then
    GameCanvas.FillRect(0, 0, DisplaySize.X, DisplaySize.Y, cRGB1(0, 0, 0, TMap.FadeScreen.AlphaCounter));
  if TMap.ShowFoothold then
    TFootholdTree.This.DrawFootHolds;
  if TMap.ShowMusic then
    FontsAlt[4].TextOut('Bgm: ' + TMap.BgmPath, 10, 10, cRGB1(255, 0, 0));

end;

procedure TMainForm.RenderAvatar(Sender: TObject);
begin
  SpriteEngine.DrawEx(['TPlayer', 'TItemEffect', 'TSetEffect']);
end;

procedure TMainForm.RenderAvatarPanel(Sender: TObject);
begin
  var RX := DisplaySize.X / 260; // panel w
  var RY := DisplaySize.Y / 200;
  var WX := Round(Player.X - SpriteEngine.WorldX - 130 + TMapleChair.BodyRelMove.X - TTamingMob.Navel.X);
  var WY := Round(Player.y - SpriteEngine.WorldY - 160 + TMapleChair.BodyRelMove.Y - TTamingMob.Navel.Y);

  GameCanvas.UseTexturePx(AvatarTargets[TPlayer.AvatarPanelIndex], pxBounds4(WX, WY, 4096, 4096));
  GameCanvas.TexMap(pBounds4s(0, 0, 4096 * RX, 4096 * RY, 1), clWhite4);
end;

procedure TMainForm.LabelRingButtonClick(Sender: TObject);
begin
  LabelRingForm.Show;
end;

procedure TMainForm.LoadMapButtonClick(Sender: TObject);
var
  T: Int64;
  Portals: TPortalInfo;
  PX, PY: Integer;
  BelowFH: TFoothold;
  Below: TPoint;
begin
  Timer.Enabled := False;

  T := GetTickCount;

  TMap.LoadMap(TMap.ID);
  for Portals in TMapPortal.PortalList do
    if (Portals.PortalType = 0) then
    begin
      PX := Portals.X;
      PY := Portals.Y;
      Break;
    end;
  Player.X := PX;
  Player.Y := PY - 10;
  Below := TFootholdTree.This.FindBelow(Point(PX, PY - 2), BelowFH);
  Player.FH := BelowFH;
  Player.JumpState := jsFalling;

  if TPet.Pet <> nil then
  begin
    TPet.Pet.X := Player.x;
    TPet.Pet.Y := Player.y;
    TPet.Pet.JumpState := jsFalling;
  end;

  if TMonsterFamiliar.MonsterFamiliar <> nil then
  begin
    TMonsterFamiliar.MonsterFamiliar.X := Player.x;
    TMonsterFamiliar.MonsterFamiliar.Y := Player.Y;
    TMonsterFamiliar.MonsterFamiliar.JumpState := jsFalling;
  end;

  SpriteEngine.WorldX := PX - DisplaySize.X / 2;
  SpriteEngine.WorldY := PY - (DisplaySize.Y / 2) - 100;
  if SpriteEngine.WorldX > TMap.Right then
    SpriteEngine.WorldX := TMap.Right;
  if SpriteEngine.WorldX < TMap.Left then
    SpriteEngine.WorldX := TMap.Left;
  if SpriteEngine.WorldY > TMap.Bottom then
    SpriteEngine.WorldY := TMap.Bottom;
  if SpriteEngine.WorldY < TMap.Top then
    SpriteEngine.WorldY := TMap.Top;
  TMap.SaveMapID := TMap.ID;
 // Caption := Variant(GetTickCount - T);

  Timer.Enabled := True;

  if not SaveMapButton.Enabled then
  begin
    for var i := 0 to ComponentCount - 1 do
      if (Components[i] is TSpeedButton) then
        TSpeedButton(Components[i]).Enabled := True;
    OpenMSFolder.Enabled := False;
    ComboKey.Enabled := False;
  end;

  ActiveControl := nil;
end;

procedure TMainForm.MedalButtonClick(Sender: TObject);
begin
  MedalTagForm.Show;
end;

procedure TMainForm.NickNameButtonClick(Sender: TObject);
begin
  NickNameForm.Show;
end;

procedure TMainForm.OpenMSFolderClick(Sender: TObject);
var
  ID, Name, Path: string;
  Iter, Iter2: TWZIMGEntry;
  Dir: TWZDirectory;
  Img: TWZFile;
  RowCount: Integer;
begin
  if FolderDialog1.Execute then
  begin
    if FileExists(FolderDialog1.Directory + '\String.wz') then
    begin
      Grid.Clear;
      Path := FolderDialog1.Directory;
      if MapWz <> nil then
        FreeAndNil(MapWz);
      if Map2Wz <> nil then
        FreeAndNil(Map2Wz);

      if MobWZ <> nil then
        FreeAndNil(MobWZ);
      if Mob2WZ <> nil then
        FreeAndNil(Mob2WZ);
      if NPCWZ <> nil then
        FreeAndNil(NPCWZ);
      if StringWZ <> nil then
        FreeAndNil(StringWZ);
      if SoundWZ <> nil then
        FreeAndNil(SoundWZ);

      with Grid.Canvas do
      begin
        Font.Size := 20;
        Font.Color := clBlack;
        Brush.Color := clGrayText;
        TextOut(20, 100, 'Loading...');
      end;
      WzPath := Path;
      StringWZ := TWZArchive.Create(Path + '\String.wz');

      MapWz := TWZArchive.Create(Path + '\Map.wz');
      if FileExists(Path + '\Map2.wz') then
        Map2Wz := TWZArchive.Create(Path + '\Map2.wz');
      if FileExists(Path + '\Map001.wz') then
        Map001Wz := TWZArchive.Create(Path + '\Map001.wz');

      MobWZ := TWZArchive.Create(Path + '\Mob.wz');
      if FileExists(Path + '\Mob2.wz') then
        Mob2WZ := TWZArchive.Create(Path + '\Mob2.wz');
      if FileExists(Path + '\Mob001.wz') then
        Mob001WZ := TWZArchive.Create(Path + '\Mob001.wz');

      NPCWZ := TWZArchive.Create(Path + '\Npc.wz');
      SoundWZ := TWZArchive.Create(Path + '\Sound.wz');
      if FileExists(Path + '\Sound2.wz') then
        Sound2Wz := TWZArchive.Create(Path + '\Sound2.wz');

      CharacterWZ := TWZArchive.Create(Path + '\Character.wz');
      BaseWZ := TWZArchive.Create(Path + '\Base.wz');
      UIWZ := TWZArchive.Create(Path + '\UI.wz');
      ReactorWz := TWZArchive.Create(Path + '\Reactor.wz');
      EffectWz := TWZArchive.Create(Path + '\Effect.wz');
      SkillWZ := TWZArchive.Create(Path + '\Skill.wz');
      if FileExists(Path + '\Skill001.wz') then
      begin
        Skill001Wz := TWZArchive.Create(Path + '\Skill001.wz');
        TSkill.Has001Wz := True;
      end;
      ItemWZ := TWZArchive.Create(Path + '\Item.wz');
      MorphWz := TWZArchive.Create(Path + '\Morph.wz');
      EtcWZ := TWZArchive.Create(Path + '\Etc.wz');

      TSetEffect.LoadList;
      TItemEffect.LoadList;
      TTamingMob.LoadSaddleList;

      if StringIDs <> nil then
        StringIDs.Free;

      StringIDs := TDictionary<string, string>.Create;
      for Iter in StringWZ.GetImgFile('Map.img').Root.Children do
        for Iter2 in Iter.Children do
        begin
          ID := Add9(Iter2.Name);
          Name := Iter2.Get('mapName', '');
          StringIDs.AddOrSetValue(ID, Name);
        end;
      RowCount := -1;
      Grid.BeginUpdate;

      for Dir in TWZDirectory(MapWz.Root.Entry['Map']).SubDirs do
        for Img in Dir.Files do
        begin
          ID := LeftStr(Img.Name, 9);
          if StringIDs.ContainsKey(ID) then
          begin
            Inc(RowCount);
            Grid.RowCount := RowCount + 1;
            Grid.Cells[0, RowCount] := ID + '  ' + StringIDs[ID];
          end;
        end;

      Grid.RemoveDuplicates(0, True);
      Grid.SortByColumn(0);
      Grid.RemoveRows(0, 1);
      Grid.EndUpdate;
      LoadMapButton.Enabled := True;
      SearchMapEdit.Enabled := True;
      PageControl1.Enabled := True;
      Grid.Enabled := True;

      FreeAndNil(GameDevice);
      GameDevice := Factory.CreateDevice();
      GameDevice.WindowHandle := Self.Handle;
      GameDevice.Size := DisplaySize;
      GameDevice.Windowed := True;
      GameDevice.VSync := True;
      GameCanvas.MipMapping := True;
      // GameCanvas.Antialias := True;
      GameDevice.VertexProcessing := vptSoftware;
      EventDeviceCreate.Subscribe(OnDeviceCreate, 0);
      if (not GameDevice.Initialize()) then
      begin
        ShowMessage('Failed to initialize Asphyre device.');
        Application.Terminate();
        Exit;
      end;
      RenderForm.Show;
      RenderForm.Parent := MainForm;
    end
    else
    begin
      ShowMessage('wrong folder,  .wz files not found');
    end;
  end;
  ActiveControl := nil;
end;

procedure TMainForm.PageControl1Change(Sender: TObject);
begin
  case PageControl1.TabIndex of
    0:
      begin
        WorldMapForm.Close;
        LoadMapButton.Enabled := True;
      end;

    1:
      begin
        WorldMapForm.Show;
        LoadMapButton.Enabled := False;
      end;
  end;
end;

procedure TMainForm.PetButtonClick(Sender: TObject);
begin
  PetForm.Show;
end;

procedure TMainForm.SaveMapButtonClick(Sender: TObject);
begin
  SaveMapForm.Show;
end;

procedure TMainForm.PicInfoButtonClick(Sender: TObject);
begin
  ImageInfoForm.Close;
  ImageInfoForm.Show;
end;

procedure TMainForm.DropMobButton1Click(Sender: TObject);
begin
  AddMobForm.Show;
end;

procedure TMainForm.DropNpcButton1Click(Sender: TObject);
begin
  AddNpcForm.Show;
end;

procedure TMainForm.AdvToolPanel1Click(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TMainForm.SearchMapEditChange(Sender: TObject);
begin
  Grid.NarrowDown(SearchMapEdit.Text);
end;

procedure TMainForm.SpeedButton1Click(Sender: TObject);
begin
  ShowOptionForm.Show;
end;

procedure TMainForm.SpeedButton2Click(Sender: TObject);
begin
  DamageSkinForm.Show;
end;

procedure TMainForm.SpeedButton3Click(Sender: TObject);
begin
  FontsAlt.UpdateAll;
  GameDevice.Windowed := False;
  TMobInfo.ReDrawTarget;
  TNpc.ReDrawTarget := True;
  TNameTag.ReDraw := True;
  TMedalTag.ReDraw;
  TNickNameTag.ReDraw;
  TLabelRingTag.ReDraw;
  TPetNameTag.ReDraw;
  TFamiliarNameTag.ReDraw;
  ActiveControl := nil;
end;

procedure TMainForm.SpeedButton4Click(Sender: TObject);
begin
  MorphForm.Show;
end;

procedure TMainForm.SpeedButton5Click(Sender: TObject);
begin
  FamiliarForm.Show;
end;

procedure TMainForm.SpeedButton6Click(Sender: TObject);
begin
  SkillForm.Show;
end;

procedure TMainForm.AvatarButton1Click(Sender: TObject);
begin
  AvatarForm.Show;
end;

procedure TMainForm.CashButtonClick(Sender: TObject);
begin
  CashForm.Show;
end;

procedure TMainForm.ChairButton1Click(Sender: TObject);
begin
  ChairForm.Show;
end;

procedure TMainForm.ComboBox1Change(Sender: TObject);
var
  I: Integer;
begin
  FontsAlt.UpdateAll;

  var S := Explode('X', ComboBox1.Items[ComboBox1.ItemIndex]);
  DisplaySize.X := S[0].toInteger;
  DisplaySize.Y := S[1].toInteger;

  Width := DisplaySize.X + 232;
  Height := DisplaySize.Y + 143;
  RenderForm.ClientWidth := DisplaySize.X;
  RenderForm.ClientHeight := DisplaySize.Y;
  Shape1.Width := DisplaySize.X + 4;
  Shape1.Height := DisplaySize.Y + 4;
  AdvGroupBox2.Width := DisplaySize.X + 5;
  PageControl1.Height := DisplaySize.Y - 130;
  SpriteEngine.VisibleWidth := DisplaySize.X;
  SpriteEngine.VisibleHeight := DisplaySize.Y;
  for I := 0 to 1 do
  begin
    BackEngine[I].VisibleWidth := DisplaySize.X;
    BackEngine[I].VisibleHeight := DisplaySize.Y;
  end;
  GameDevice.Size := DisplaySize;
  TMap.OffsetY := (DisplaySize.Y - 600) div 2;
  TMapBack.ResetPos := True;
  TNpc.ReDrawTarget := True;
  TNameTag.ReDraw := True;
  if Player <> nil then
  begin
    TMedalTag.ReDraw;
    TNickNameTag.ReDraw;
    TLabelRingTag.ReDraw;
    TPetNameTag.ReDraw;
    TFamiliarNameTag.ReDraw;
  end;
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
  ActiveControl := nil;
end;

procedure TMainForm.ComboBox1CloseUp(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TMainForm.ComboBox2Change(Sender: TObject);
begin
  case ComboBox2.ItemIndex of
    0:
      GameMode := gmPlay;
    1:
      GameMode := gmView;
  end;
  ActiveControl := nil;
end;

procedure TMainForm.ComboBox2CloseUp(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TMainForm.ComboKeyChange(Sender: TObject);
begin
  case ComboKey.ItemIndex of
    0:
      TWZReader.EncryptionIV := 0;
    1:
      TWZReader.EncryptionIV := GMS_IV;
    2:
      TWZReader.EncryptionIV := GENERAL_IV;
  end;
  ActiveControl := nil;
end;

procedure TMainForm.ComboKeyCloseUp(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TMainForm.GridClickCell(Sender: TObject; ARow, ACol: Integer);
var
  Entry: TWZIMGEntry;
  MapID, MapLink, LeftNum: string;
  Bmp: TBitmap;
begin
  MapID := LeftStr(Grid.Rows[ARow].Text, 9);
  LeftNum := LeftStr(MapID, 1);
  Entry := GetImgEntry('Map.wz/Map/Map' + LeftNum + '/' + MapID + '.img/info/link');
  if Entry = nil then
    TMap.ID := MapID
  else
    TMap.ID := Entry.Data;

  LeftNum := LeftStr(TMap.ID, 1);
  Image1.Picture := nil;
  Entry := GetImgEntry('Map.wz/Map/Map' + LeftNum + '/' + TMap.ID + '.img/miniMap');
  if Entry <> nil then
  begin
    Bmp := Entry.Get2('canvas').Canvas.DumpBmp;
    Image1.Picture.Assign(Bmp);
    Bmp.Free;
  end;
  ActiveControl := nil;
end;

initialization


end.

