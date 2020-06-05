unit MainUnit;

interface

{$IF CompilerVersion >= 21.0}
{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$IFEND}

uses
  PXT.Types, Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, FolderDialog, Vcl.Grids, AdvObj, BaseGrid, AdvGrid, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.Buttons, scControls, scExtControls, Vcl.Controls, AdvGroupBox, Vcl.ExtCtrls, ToolPanels,
  Vcl.Forms, Vcl.Dialogs, ToolWin, AdvToolBtn, WZArchive, WZDirectory, Generics.Collections,
  WZIMGFile, KeyHandler, WZReader, StrUtils, PngImage, Jpeg, {, Reactor,}
  Footholds, bass, BassHandler, MapPortal, AdvUtil, Mob2, Npc, {UI}
  MapleCharacter, {Boss,} Vectors2px, AbstractTextures, AbstractDevices, AbstractCanvas,
  AsphyreTimer, PXT.Sprites, AsphyreKeyboard, AsphyreFontsAlt, DirectInput, AsphyreFactory,
  DX9Providers, AsphyreTypes, Global, AsphyreRenderTargets, LockRenderTarget, MapleMap, WzUtils,
  System.Types, PXT.Graphics, PXT.Headers;

type
  TScreenMode = (smNormal, smScale, smFullScreen);

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
    DamageButton: TSpeedButton;
    DisplayButton: TSpeedButton;
    FullscreenButton: TSpeedButton;
    MorphButton: TSpeedButton;
    MedalButton: TSpeedButton;
    NickNameButton: TSpeedButton;
    LabelRingButton: TSpeedButton;
    PetButton: TSpeedButton;
    FamiliarButton: TSpeedButton;
    SkillButton: TSpeedButton;
    OptionButton: TSpeedButton;
    AndroidButton: TSpeedButton;
    ScreeenSetButton: TSpeedButton;
    ConsumeButton: TSpeedButton;
    CashButton2: TSpeedButton;
    EtcButton: TSpeedButton;
    PlayActionButton: TSpeedButton;
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
    procedure DamageButtonClick(Sender: TObject);
    procedure CashButtonClick(Sender: TObject);
    procedure TamingMobButtonClick(Sender: TObject);
    procedure FullscreenButtonClick(Sender: TObject);
    procedure DropMobButton1Click(Sender: TObject);
    procedure DropNpcButton1Click(Sender: TObject);
    procedure SaveMapButtonClick(Sender: TObject);
    procedure DisplayButtonClick(Sender: TObject);
    procedure PicInfoButtonClick(Sender: TObject);
    procedure MorphButtonClick(Sender: TObject);
    procedure MedalButtonClick(Sender: TObject);
    procedure AvatarButton1Click(Sender: TObject);
    procedure NickNameButtonClick(Sender: TObject);
    procedure LabelRingButtonClick(Sender: TObject);
    procedure FamiliarButtonClick(Sender: TObject);
    procedure PetButtonClick(Sender: TObject);
    procedure SkillButtonClick(Sender: TObject);
    procedure OptionButtonClick(Sender: TObject);
    procedure AndroidButtonClick(Sender: TObject);
    procedure ScreeenSetButtonClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure ConsumeButtonClick(Sender: TObject);
    procedure CashButton2Click(Sender: TObject);
    procedure EtcButtonClick(Sender: TObject);
    procedure PlayActionButtonClick(Sender: TObject);
  private
    OldX, OldY: Integer;
    MoveOn: Boolean;
    CircleList: TList<TShape>;
    LoadWorldMapDone: Boolean;
    HasShow: Boolean;
    MonitorWidth, MonitorHeight: Integer;
    procedure TimerEvent(Sender: TObject);
    procedure RenderEvent;
    procedure CirCleMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    { Private declarations }
  public
    ScreenMode: TScreenMode;
    FullScreenTexture: TTexture;
    CheckBoardtexture: TTexture;
    procedure CreateTexture(var Texture: TTexture; Width, Height: Integer; PremultipliedAlpha: Boolean);
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  MobFormUnit, SaveMapFormUnit, ImageInfoUnit, RenderFormUnit, MapBack, MobInfo, ShowOptionUnit,
  Tools, NpcFormUnit, ChairformUnit, MorphFormUnit, MedalTagFormUnit, NickNameTagFormUnit,
  DamageSkinFormUnit, WorldMapFormUnit, CashFormUnit, TamingMobFormUnit, NameTag, MapleEffect,
  TamingMob, MapleChair, LabelRingFormUnit, PetFormUnit, Pet, FamiliarFormUnit, MonsterFamiliar,
  SkillFormUnit, Skill, OptionsFormUnit, AvatarFormUnit, AndroidFormUnit, Android, MiniMap,
  ACtrlEngine, SetScreenFormUnit, ConsumeFormUnit, CashForm2Unit, EtcFormUnit, PlayActionFormUnit,
  UI.Utils, acontrols, UI.Statusbar3.MainBar, UI.StatusBar3.Chat, UI.UIWindow2.UserInfo,
  UI.UIWindow2.Item;
{$R *.dfm}

procedure TMainForm.FamiliarButtonClick(Sender: TObject);
begin
  if (not HasImgFile('String.wz/FamiliarSkill.img')) and (not HasImgFile('String.wz/Familiar.img')) then
  begin
    MessageDlg('Older versions of .wz are not supported', mtInformation, [mbOK], 0);
    Exit;
  end
  else
    FamiliarForm.Show;

end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(SpriteEngine);
  FreeAndNil(Keyboard);
  BackEngine[0].Free;
  BackEngine[1].Free;
  TFootholdTree.This.Free;
  Keyboard.Free;
  FreeAndNil(CharData);
  MapWz.Free;
  if Map2Wz <> nil then
    Map2Wz.Free;
  if Map001Wz <> nil then
    Map001Wz.Free;
  if Map002Wz <> nil then
    Map002Wz.Free;
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
  {
  AvatarPanelTexture.Free;
  FullScreenTexture.Free;
  CheckBoardtexture.Free;
  GameCanvas.Free;
  FDevice.Free;
  GameDevice2.Free;
  GameDevice3.Free;
  GameFont.Free;
   }
  //UIEngine.Free;
  // DropList
 // ReportMemoryLeaksOnShutdown := True;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var KEY: Word; Shift: TShiftState);
begin

  if (BorderStyle = bsNone) and (KEY = VK_ESCAPE) then
  begin
    BorderStyle := bsSingle;
    RenderForm.Width := DisplaySize.X;
    RenderForm.Height := DisplaySize.Y;
    RenderForm.Left := 217;
    RenderForm.Top := 78;
    Width := DisplaySize.X + 232;
    Height := DisplaySize.Y + 143;
    Left := (Screen.Width - Width) div 2;
    Top := (Screen.Height - Height) div 2;
    Shape1.Width := displaySize.X + 4;
    Shape1.Height := displaySize.Y + 4;
    AdvGroupBox2.Width := displaySize.X + 5;
    PageControl1.Height := displaySize.Y - 130;
    ScreenMode := smNormal;
  end;
  if KEY = VK_MENU then
    KEY := 0;
  if ActiveEdit <> nil then
    ActiveEdit.KeyDown(KEY, Shift);

  if (KEY = VK_RETURN) then
  begin
    if UIForm['Input/ChatEnter'].Visible = False then
    begin
      UIForm['Input/ChatEnter'].Visible := True;
      UIEdit['StatusBar3/Chat'].Text := '  ';
      UIEdit['StatusBar3/Chat'].SelStart := 0;
      UIEdit['StatusBar3/Chat'].SetFocus;
    end
    else
    begin
      if Trims(UIEdit['StatusBar3/Chat'].Text) = '' then
        UIForm['Input/ChatEnter'].Visible := False
      else
        TChatViewImage.Redraw;
    end;
  end;
end;

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if ActiveEdit <> nil then
    ActiveEdit.KeyPress(Key);
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

procedure TMainForm.FormMouseUp;
begin
  MoveOn := False;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  LPixels: array of TIntColor;
begin
  if HasShow then
    Exit;
  HasShow := True;
  RenderForm.Show;
  RenderForm.Parent := MainForm;
  Randomize;
  DisplaySize := Point2i(1024, 768);
  GameMode := gmPlay;

  FDevice := DeviceInit(TDeviceBackend.Default, RenderForm.Handle, Point2i(1024, 768), PXT.Types.TPixelFormat.BGRA8,
    PXT.Types.TPixelFormat.Unknown, 0, DeviceAttributes([TDeviceAttribute.VSync]));
  GameDevice2 := DeviceInitShared(FDevice, AvatarForm.Panel1.Handle, Point2i(260, 200), PXT.Types.TPixelFormat.BGRA8,
    PXT.Types.TPixelFormat.Unknown, 0, DeviceAttributes([TDeviceAttribute.VSync]));
  GameDevice2.Resize(Point2i(260, 200));

  GameDevice3 := DeviceInitShared(FDevice, AvatarForm.Panel2.Handle, Point2i(512, 512), PXT.Types.TPixelFormat.BGRA8,
    PXT.Types.TPixelFormat.Unknown, 0, DeviceAttributes([TDeviceAttribute.VSync]));
  GameDevice3.Resize(Point2i(512, 512));
  if Screen.MonitorCount > 0 then
  begin
    MonitorWidth := Screen.Monitors[0].Width;
    MonitorHeight := Screen.Monitors[0].Height;
  end;

  GameCanvas.Create(FDevice);
  CreateTexture(AvatarPanelTexture, 4096, 4096, True);

  var Parameters: TTextureParameters;
  FillChar(Parameters, SizeOf(TTextureParameters), 0);
  Parameters.Width := 512;
  Parameters.Height := 512;
  Parameters.Format := PXT.Types.TPixelFormat.RGBA8;
  Parameters.Attributes := TextureMipMapping;
  CheckBoardTexture := TextureInit(FDevice, Parameters);
  SetLength(LPixels, Parameters.Width * Parameters.Height);
  for var J := 0 to Parameters.Height - 1 do
  begin
    var LPixel: PIntColor := @LPixels[J * Parameters.Width];
    for var I := 0 to Parameters.Width - 1 do
    begin
      if (I = 0) or (J = 0) or (I = Parameters.Width - 1) or (J = Parameters.Height - 1) then
        LPixel^ := $FFFFFFFF // black border
      else if ((I div 8) + (J div 8)) mod 2 = 0 then // put checkboard pattern
        LPixel^ := $FFCDCDCD
      else
        LPixel^ := $FFFFFFFF;
      Inc(LPixel);
    end;
  end;
  CheckBoardTexture.Update(@LPixels[0], Parameters.Width * SizeOf(TIntColor), 0, ZeroIntRect);

  GameFont := TextRendererInit(GameCanvas, Point2i(512, 512));
  GameFont.FontSettings := TFontSettings.Create('Segoe UI', 12.0, TFontWeight.Normal);
  Keyboard := TAsphyreKeyboard.Create(MainForm);
  Keyboard.Foreground := False;
  UIEngine := TControlEngine.Create(MainForm, FDevice, GameCanvas);
  UIEngine.ImageLib := UIImages;
  UIEngine.Parent := RenderForm;

  CircleList := TList<TShape>.Create;

  Images := TDictionary<TWZIMGEntry, TTexture>.Create;
  EquipImages := TDictionary<TWZIMGEntry, TTexture>.Create;

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
 // Timer.Enabled := True;
  TWZReader.EncryptionIV := 0;

  TTimers.AddTimer('FreeSounds');
  TTimers.AddTimer('DropMobTicks');
  TTimers.AddTimer('aaa');
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

  case ScreenMode of
    smScale:
      begin
        FullScreenTexture.Clear;
        FullScreenTexture.BeginScene;
        GameCanvas.BeginScene;
        RenderEvent;
        GameCanvas.EndScene;
        FullScreenTexture.EndScene;

        FDevice.BeginScene;
        FDevice.Clear([TClearLayer.Color], FloatColor($0));
        GameCanvas.BeginScene;
        GameCanvas.DrawStretch(FullScreenTexture, 0, 0, RenderForm.ClientWidth, RenderForm.ClientHeight);
        if SetScreenForm.ScanlineCheckBox.Checked then
          GameCanvas.Draw(SetScreenForm.ScanlineTexture, 0, 0, TBlendingEffect.Multiply);
        GameCanvas.EndScene;
        FDevice.EndScene;
      end;
    smFullScreen:
      begin
        FullScreenTexture.Clear;
        FullScreenTexture.BeginScene;
        GameCanvas.BeginScene;
        RenderEvent;
        GameCanvas.EndScene;
        FullScreenTexture.EndScene;

        FDevice.BeginScene;
        FDevice.Clear([TClearLayer.Color], FloatColor($FFFFC800));
        GameCanvas.BeginScene;
        GameCanvas.DrawStretch(FullScreenTexture, 0, 0, MonitorWidth, MonitorHeight);
        if SetScreenForm.ScanlineCheckBox.Checked then
          GameCanvas.Draw(SetScreenForm.ScanlineTexture, 0, 0, TBlendingEffect.Multiply);
        GameCanvas.EndScene;
        FDevice.EndScene;
      end;
    smNormal:
      begin
        FDevice.BeginScene;
        FDevice.Clear([TClearLayer.Color], FloatColor($FFFFC800));
        GameCanvas.BeginScene;
        RenderEvent;
        if SetScreenForm.ScanlineCheckBox.Checked then
          GameCanvas.Draw(SetScreenForm.ScanlineTexture, 0, 0, TBlendingEffect.Multiply);
        GameCanvas.EndScene;
        FDevice.EndScene;
      end;
  end;

  if AvatarForm.Active or TUserInfoAvatarImage.Show then
  begin
    AvatarPanelTexture.Clear;
    AvatarPanelTexture.BeginScene;
    GameCanvas.BeginScene;
    SpriteEngine.DrawEx(['TPlayer', 'TItemEffect', 'TSetEffect']);
    GameCanvas.EndScene;
    AvatarPanelTexture.EndScene;

    var WX := Round(Player.X - SpriteEngine.WorldX - 130 + TMapleChair.BodyRelMove.X - TTamingMob.Navel.X);
    var WY := Round(Player.y - SpriteEngine.WorldY - 160 + TMapleChair.BodyRelMove.Y - TTamingMob.Navel.Y);
    GameDevice2.BeginScene;
    GameDevice2.Clear([TClearLayer.Color], FloatColor($FFFFFFFF));
    GameCanvas.BeginScene;
    GameCanvas.Draw(CheckBoardtexture, 0, 0);
    GameCanvas.DrawPortion(AvatarPanelTexture, 0, 0, WX, WY, WX + 280, WY + 200, False, $FFFFFFFF);
    GameCanvas.EndScene;
    GameDevice2.EndScene;
    if AvatarForm.PageControl1.ActivePageIndex = 5 then
    begin
      var WX2 := Round(Player.X - SpriteEngine.WorldX - 155);
      var WY2 := Round(Player.Y - SpriteEngine.WorldY - 160);
      GameDevice3.BeginScene;
      GameDevice3.Clear([TClearLayer.Color], FloatColor($FFFFFFFF));
      GameCanvas.BeginScene;
      GameCanvas.Draw(CheckBoardtexture, 0, 0);
      var X := AvatarForm.TrackBarX.Position;
      var Y := AvatarForm.TrackBarY.Position;
      var Width := AvatarForm.TrackBarW.Position;
      var Height := AvatarForm.TrackBarH.Position;
      GameCanvas.DrawPortion(AvatarPanelTexture, 100, 150, WX2, WY2, WX2 + 250, WY2 + 230, False, $FFFFFFFF);
      Gamecanvas.FrameRect(FloatRect(100 + X, 150 + Y, Width, Height), ColorRect($FFFF0000), 2);
      GameCanvas.EndScene;
      GameDevice3.EndScene;
    end;
  end;

  if (TMapleChair.IsUse) then
    if (Keyboard.KeyPressed[DIK_LEFT]) or (Keyboard.KeyPressed[DIK_RIGHT]) then
    begin
      TMapleChair.Delete;
      TTamingMob.Delete;
      TItemEffect.Delete(Chair);
    end;

end;

procedure TMainForm.RenderEvent;
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

 // if TMap.ShowFPS then
  //GameFont.Draw(Point2f(10, 10), 'FPS: ' + IntToStr(Timer.FrameRate), cRGB1(255, 0, 0));
  // if TMap.ShowMobInfo then
   // GameCanvas.Draw(GameTargetMobInfo[0], 0, 20, 1, False, 255, 255, 255, 255);
  if TMap.FadeScreen.DoFade then
    GameCanvas.FillRect(FloatRect(0, 0, DisplaySize.X, DisplaySize.Y), cRGB1(0, 0, 0, TMap.FadeScreen.AlphaCounter));
  if TMap.ShowFoothold then
    TFootholdTree.This.DrawFootHolds;
  if TMap.ShowMusic then
  begin
    var FontSettings := TFontSettings.Create('Tahoma', 12, TFontWeight.Normal);
    GameFont.FontSettings := FontSettings;
    GameFont.Draw(Point2f(10, 50), '音樂: ' + TMap.BgmPath, $FFFF0000);
  end;
  if UIVersion = 3 then
  begin
    if TMap.ShowUI then
    begin
      UIEngine.Render(Canvas.Handle);
      if TSlots.PickUpItem <> nil then
      begin
        var Mx := Mouse.CursorPos.X - MainForm.Left - 230;
        var MY := Mouse.CursorPos.Y - MainForm.Top - 120;
        GameCanvas.Draw(UIImages[TSlots.PickUpItem], Mx, MY);
      end;
      GameCursor.Draw;
     end;
  end;

end;

procedure TMainForm.CirCleMouseDown;
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
  var LeftNum := LeftStr(ID, 1);

  if TMap.Has002Wz then
  begin
    if not HasImgFile('Map002.wz/Map/Map' + LeftNum + '/' + ID + '.img') then
      Exit;
  end
  else
  begin
    if not HasImgFile('Map.wz/Map/Map' + LeftNum + '/' + ID + '.img') then
      Exit;
  end;

  WorldMapForm.Canvas.Font.Size := 18;
  WorldMapForm.Canvas.TextOut(150, 150, 'Loading...');
  TMap.ReLoad := True;
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
  if AndroidPlayer <> nil then
  begin
    AndroidPlayer.X := Player.x;
    AndroidPlayer.Y := Player.Y;
    AndroidPlayer.JumpState := jsFalling;
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
  if not LoadWorldMapDone then
  begin
    WorldMapForm.Top := (Screen.Height - WorldMapForm.Height) div 2;
    LoadWorldMapDone := True;
  end;
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

    if TMap.MapNameList.ContainsKey(ID) then
      Circle.Hint := ID + '-' + TMap.MapNameList[ID].MapName;

    Circle.Width := 23;
    Circle.Height := 25;
    Circle.HelpKeyword := ID;
    Circle.OnMouseDown := CirCleMouseDown;
    CircleList.Add(Circle);
  end;
  ActiveControl := nil;
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
  if AndroidPlayer <> nil then
  begin
    AndroidPlayer.X := Player.x;
    AndroidPlayer.Y := Player.Y;
    AndroidPlayer.JumpState := jsFalling;
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
  if UIVersion = 3 then
    RenderForm.Cursor := crNone;
  RenderForm.FormResize(Sender);
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
  ID, MapName, StreetName, Path: string;
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
      if Mob001WZ <> nil then
        FreeAndNil(Mob001WZ);
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

      if StringWZ.GetImgFile('Mob.img').Root.Get('100100/name', '') = 'Snail' then
        TNpc.FontSize := 13 //GMS
      else
        TNpc.FontSize := 12; //TMS
      if StringWZ.GetImgFile('Mob.img').Root.Get('100100/name', '') = '달팽이' then
        IsKMS := True;

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
      if FileExists(Path + '\Map002.wz') then
      begin
        Map002Wz := TWZArchive.Create(Path + '\Map002.wz');
        TMap.Has002Wz := True;
      end;

      NPCWZ := TWZArchive.Create(Path + '\Npc.wz');
      SoundWZ := TWZArchive.Create(Path + '\Sound.wz');
      if FileExists(Path + '\Sound2.wz') then
        Sound2Wz := TWZArchive.Create(Path + '\Sound2.wz');

      CharacterWZ := TWZArchive.Create(Path + '\Character.wz');
      BaseWZ := TWZArchive.Create(Path + '\Base.wz');
      UIWZ := TWZArchive.Create(Path + '\UI.wz');
      if HasImgFile('UI.wz/UIWindow4.img') then
        UIVersion := 3;
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

      var MapNameRec: TMapNameRec;
      for Iter in StringWZ.GetImgFile('Map.img').Root.Children do
        for Iter2 in Iter.Children do
        begin
          ID := Add9(Iter2.Name);
          MapNameRec.ID := ID;
          MapNameRec.StreetName := Iter2.Get('streetName', '');
          MapNameRec.MapName := Iter2.Get('mapName', '');
          TMap.MapNameList.AddOrSetValue(ID, MapNameRec);
        end;
      RowCount := -1;
      Grid.BeginUpdate;

      var MapDir: TWZDirectory;

      if TMap.Has002Wz then
        MapDir := TWZDirectory(Map002Wz.Root.Entry['Map'])
      else
        MapDir := TWZDirectory(MapWz.Root.Entry['Map']);

      for Dir in MapDir.SubDirs do
        for Img in Dir.Files do
        begin
          ID := LeftStr(Img.Name, 9);
          if TMap.MapNameList.ContainsKey(ID) then
          begin
            Inc(RowCount);
            Grid.RowCount := RowCount + 1;
            Grid.Cells[0, RowCount] := ID + '  ' + TMap.MapNameList[ID].MapName;
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

    end
    else
    begin
      ShowMessage('Wrong folder, WZ file could not be found');
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

procedure TMainForm.ScreeenSetButtonClick(Sender: TObject);
begin
  SetScreenForm.Show;
end;

procedure TMainForm.PicInfoButtonClick(Sender: TObject);
begin
  ImageInfoForm.Close;
  ImageInfoForm.Show;
end;

procedure TMainForm.PlayActionButtonClick(Sender: TObject);
begin
  PlayActionForm.Show;
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
  Grid.NarrowDown(Trim(SearchMapEdit.Text));
end;

procedure TMainForm.DisplayButtonClick(Sender: TObject);
begin
  ShowOptionForm.Show;
end;

procedure TMainForm.DamageButtonClick(Sender: TObject);
begin
  DamageSkinForm.Show;
end;

procedure TMainForm.FullscreenButtonClick(Sender: TObject);
begin
  if Screen.MonitorCount > 0 then
  begin
    BorderStyle := bsNone;
    Left := Screen.Monitors[0].Left;
    Top := Screen.Monitors[0].Top;
    Width := Screen.Monitors[0].Width;
    Height := Screen.Monitors[0].Height;
    RenderForm.Left := Left;
    RenderForm.Top := Top;
    RenderForm.Width := Width;
    RenderForm.Height := Height;
    ScreenMode := smFullScreen;
  end;

  ActiveControl := nil;
end;

procedure TMainForm.MorphButtonClick(Sender: TObject);
begin
  MorphForm.Show;
end;

procedure TMainForm.SkillButtonClick(Sender: TObject);
begin
  SkillForm.Show;
end;

procedure TMainForm.EtcButtonClick(Sender: TObject);
begin
  Etcform.Show;
end;

procedure TMainForm.ConsumeButtonClick(Sender: TObject);
begin
  ConsumeForm.Show;
end;

procedure TMainForm.OptionButtonClick(Sender: TObject);
begin
  OptionsForm.Show;
end;

procedure TMainForm.AndroidButtonClick(Sender: TObject);
begin
  AndroidForm.Show;
end;

procedure TMainForm.AvatarButton1Click(Sender: TObject);
begin
  AvatarForm.Show;
end;

procedure TMainForm.CashButton2Click(Sender: TObject);
begin
  CashForm2.Show;
end;

procedure TMainForm.CashButtonClick(Sender: TObject);
begin
  CashForm.Show;
end;

procedure TMainForm.ChairButton1Click(Sender: TObject);
begin
  ChairForm.Show;
end;

procedure TMainForm.CreateTexture(var Texture: TTexture; Width, Height: Integer; PremultipliedAlpha: Boolean);
begin
  var Parameters: TTextureParameters;
  FillChar(Parameters, SizeOf(TTextureParameters), 0);
  Parameters.Width := Width;
  Parameters.Height := Height;
  if PremultipliedAlpha then
    Parameters.Attributes := TextureDrawable or TexturePremultipliedAlpha
  else
    Parameters.Attributes := TextureDrawable;
  Parameters.Format := PXT.Types.TPixelFormat.BGRA8;
  if Texture.Initialized then
    Texture.Free;
  Texture := TextureInit(FDevice, Parameters);
end;

procedure TMainForm.ComboBox1Change(Sender: TObject);
var
  I: Integer;
begin

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
  CreateTexture(FullScreenTexture, DisplaySize.X, DisplaySize.Y, False);

  TMap.OffsetY := (DisplaySize.Y - 600) div 2;
  TMapBack.ResetPos := True;

  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
  ScreenMode := smNormal;
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
  if TMap.Has002Wz then
    Entry := GetImgEntry('Map002.wz/Map/Map' + LeftNum + '/' + MapID + '.img/info/link')
  else
    Entry := GetImgEntry('Map.wz/Map/Map' + LeftNum + '/' + MapID + '.img/info/link');
  if Entry = nil then
    TMap.ID := MapID
  else
    TMap.ID := Entry.Data;

  LeftNum := LeftStr(TMap.ID, 1);
  Image1.Picture := nil;
  if TMap.Has002Wz then
    Entry := GetImgEntry('Map002.wz/Map/Map' + LeftNum + '/' + TMap.ID + '.img/miniMap')
  else
    Entry := GetImgEntry('Map.wz/Map/Map' + LeftNum + '/' + TMap.ID + '.img/miniMap');

  if Entry <> nil then
  begin
    TMap.HasMiniMap := True;
    if (TMap.Has002Wz) and (Entry.Get('canvas/_outlink') <> nil) then
    begin
      if (Entry.Get('canvas/_outlink') <> nil) then
      begin
        var Data: string := Entry.Get('canvas/_outlink').Data;
        var S: TArray<string> := Data.Split(['/']);
        TMap.MiniMapEntry := GetImgEntry('Map002.wz/Map/' + S[2] + '/' + S[3] + '/' + S[4]);
        Bmp := TMap.MiniMapEntry.Get('canvas').Canvas.DumpBmp;
      end
      else
      begin
        TMap.MiniMapEntry := Entry;
        Bmp := Entry.Get2('canvas').Canvas.DumpBmp;
      end;
    end
    else
    begin
      if (Entry.Get('canvas/_outlink') <> nil) then
      begin
        var Data: string := Entry.Get('canvas/_outlink').Data;
        var S: TArray<string> := Data.Split(['/']);
        TMap.MiniMapEntry := GetImgEntry('Map.wz/Map/' + S[2] + '/' + S[3] + '/' + S[4]);
        Bmp := TMap.MiniMapEntry.Get('canvas').Canvas.DumpBmp;
      end
      else
      begin
        TMap.MiniMapEntry := Entry;
        Bmp := Entry.Get2('canvas').Canvas.DumpBmp;
      end;
    end;
    Image1.Picture.Assign(Bmp);
    TMap.MiniMapWidth := Bmp.Width;
    TMap.MiniMapHeight := Bmp.Height;
    Bmp.Free;
  end
  else
    TMap.HasMiniMap := False;
  ActiveControl := nil;
end;

initialization


end.

