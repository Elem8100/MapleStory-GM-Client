unit UI.Utils;

interface

uses
  Windows, System.Types, messages, controls, SysUtils, StrUtils, AsphyreSprite, Generics.Collections,
  WZIMGFile, Math, AbstractTextures, WZArchive, ACtrlEditBoxes, AsphyreTypes, DX9Textures, AControls,
  ACtrlEngine, ACtrlForms, ACtrlButtons, TypInfo, ACtrlImages, ACtrlDropPanels, ACtrlLabels, Tools,
  WZDirectory, WZReader, KeyHandler, Global, Classes, AsphyreFonts, ACtrlTypes, PXT.Graphics,
  PXT.Canvas;

type
  TLabelColor = (lcBlack, lcRed, lcWhite);

procedure CreateUIs(EntryName: string; X, Y: Integer; wClose: Boolean = True);

procedure CreateButton(ImageEntry: string; X: Integer = 0; Y: Integer = 0); overload;

procedure CreateButton(ButtonName, ImageEntry: string; X: Integer = 0; Y: Integer = 0); overload;

procedure CreateButtons(Dir: string; BtName: array of string; X: Integer = 0; Y: Integer = 0);

procedure CreateButtonAll(EntryName: string; IgnoreDir: array of string; X, Y: Integer);

procedure CreateImages(Dir: string; ImageName: array of string; X: Integer = 0; Y: Integer = 0);

procedure CreateForm(ImageEntry: string; X, Y: Integer);

procedure CreateEmptyForm(EntryName: string; X, Y, AWidth, AHeight: Integer; ACanMove: Boolean = True);

procedure CreateAttachForm(ImageEntry, AAttachFormName: string; X, Y: Integer; AVisible: Boolean = False);

procedure CreateFormEx(EntryName: string; X, Y: Integer);

procedure CreateEdit(EntryName: string; X, Y, AWidth: Integer);

procedure CreateLabel(EntryName, AText: string; X, Y: Integer; LabelColor: TLabelColor = lcBlack);

procedure CreateImage(ImageEntry: string; AScaleX: Single = 1; AScaleY: Single = 1; X: Integer = 0;
  Y: Integer = 0); overload;

procedure CreateImage(UIName, ImageEntry: string; AScaleX: Single = 1; AScaleY: Single = 1; X:
  Integer = 0; Y: Integer = 0); overload;

procedure CreateGrid(AImagePath: string; X, Y, Col, Row: Integer; OwnerName: string);

procedure CreateDummy(W, H, X, Y: Integer);

procedure ShowForm(Name: string);

function HasUI(Key: TWZIMGEntry): Boolean;

type
  TAttachForm = class(TAForm)
  public
    OffsetX, OffsetY: Integer;
    AttachFormName: string;
    procedure Paint(DC: HDC); override;
  end;

  TGameCursor = class
  private
    Frame: Integer;
    CursorNumber: string;
    FTime: Integer;
    Origin: TPoint;
    Offset: TPoint;
    ImagEntry: TWZImgEntry;
  public
    procedure Change(Number:string);
    procedure Draw;
  end;

var
  GameCursor: TGameCursor;
  UIImages: TObjectDictionary<TWZIMGEntry, TTexture>;
  UIData: TObjectDictionary<string, TWZIMGEntry>;
  UIButton: TDictionary<string, TAButton>;
  UIForm: TDictionary<string, TAForm>;
  UIImage: TDictionary<string, TAImage>;
  UILabel: TDictionary<string, TALabel>;
 // UITab: TDictionary<string, TUITab>;
  UIOwner: string;

implementation

uses
  WzUtils, ColorUtils, minimap, RenderFormUnit;

procedure TAttachForm.Paint(DC: HDC);
begin
  if UIForm.ContainsKey(AttachFormName) then
  begin
    Left := UIForm[AttachFormName].Left + OffsetX;
    Top := UIForm[AttachFormName].Top + OffsetY;
    inherited;
  end;
end;

procedure TGameCursor.Draw;
begin
  var ImageEntry := UIData['UI.wz/Basic.img/Cursor/' + CursorNumber + '/' + Frame.ToString];
  var Delay := ImageEntry.Get('delay', '100');
  FTime := FTime + 17;
  if FTime > Delay then
  begin
    Frame := Frame + 1;
    if not UIData.ContainsKey('UI.wz/Basic.img/Cursor/' + CursorNumber + '/' + Frame.ToString) then
      Frame := 0;
    FTime := 0;
  end;
  if ImageEntry.Get('origin') <> nil then
    Origin := ImageEntry.Get('origin').Vector;

  Offset.X := -Origin.X + 3;
  Offset.Y := -Origin.Y + 3;
  var Pos := Mouse.CursorPos;
  Pos := RenderForm.ScreenToClient(Pos);
  var X := Trunc(Pos.X * (Global.DisplaySize.X / RenderForm.ClientWidth));
  var Y := Trunc(Pos.y * (Global.DisplaySize.y / RenderForm.Clientheight));
  GameCanvas.Draw(UIImages[ImageEntry], X + Offset.X, Y + Offset.Y);
end;

procedure TGameCursor.Change(Number: string);
begin
 if Frame<>0 then
   Frame:=0;
  CursorNumber:=Number;
end;

function ToName(S: string): string;
begin
  while (Pos('/', S) > 0) do
    S[Pos('/', S)] := '_';
  while (Pos('.', S) > 0) do
    S[Pos('.', S)] := '_';

  Result := S;
end;

function HasUI(Key: TWZIMGEntry): Boolean;
begin
  Result := UIImages.ContainsKey(Key);
end;

procedure CreateForm(ImageEntry: string; X, Y: Integer);
var
  Entry: TWZIMGEntry;
  Form: TAForm;
begin
  if UIForm.ContainsKey(ImageEntry) then
  begin
    UIForm[ImageEntry].Show;
    Exit;
  end;
  Entry := GetImgEntry(ImageEntry);
  if not UIData.ContainsKey(Entry.GetPath) then
    DumpData(Entry, UIData, UIImages);
  Form := TAForm.Create(UIEngine.Root);
  with Form do
  begin
    ImageEntry := Entry;
    Width := Entry.Canvas.Width;
    Height := Entry.Canvas.Height;
    Left := X + -Entry.Child['origin'].Vector.X + 1000;
    Top := Y + -Entry.Child['origin'].Vector.Y + 1000;
  end;
  UIForm.Add(ImageEntry, Form);
end;

procedure CreateAttachForm(ImageEntry, AAttachFormName: string; X, Y: Integer; AVisible: Boolean = False);
begin
  if UIForm.ContainsKey(ImageEntry) then
  begin
    UIForm[ImageEntry].Visible := AVisible;
    Exit;
  end;
  var Entry := GetImgEntry(ImageEntry);
  if not UIdata.ContainsKey(Entry.GetPath) then
    DumpData(Entry, UIData, UIImages);
  var Form := TAttachForm.Create(UIEngine.Root);
  with Form do
  begin
    ImageEntry := Entry;
    AttachFormName := AAttachFormName;
    Width := Entry.Canvas.Width;
    Height := Entry.Canvas.Height;
    OffsetX := X;
    OffsetY := Y;
    Visible := AVisible;
  end;
  UIForm.Add(ImageEntry, Form);
end;

procedure CreateEmptyForm(EntryName: string; X, Y, AWidth, AHeight: Integer; ACanMove: Boolean = True);
var
  Form: TAForm;
begin
  if not UIForm.ContainsKey(EntryName) then
  begin
    Form := TAForm.Create(UIEngine.Root);
    with Form do
    begin
      Left := X + 1000;
      Top := Y + 1000;
      Width := AWidth;
      Height := AHeight;
      CanMove := ACanMove;
    end;
    UIForm.Add(EntryName, Form);
  end
  else
    UIForm[EntryName].Show;
end;

procedure CreateFormEx(EntryName: string; X, Y: Integer);
var
  Entry: TWZIMGEntry;
  Form: TAForm;
begin
  Entry := GetImgEntry(EntryName);
  DumpData(Entry, UIData, UIImages);
  Form := TAForm.Create(UIEngine.AForm(UIOwner));
  with Form do
  begin
    ImageEntry := Entry;
    Name := ToName(Entry.GetPath);
    Width := Entry.Canvas.Width;
    Height := Entry.Canvas.Height;
    Left := X + -Entry.Child['origin'].Vector.X;
    Top := Y + -Entry.Child['origin'].Vector.Y;
    CanMove := False;
    BorderWidth := 0;
    ShadowWidth := 0;
  end;
  UIForm.Add(EntryName, Form);
end;

procedure CreateButton(ImageEntry: string; X: Integer = 0; Y: Integer = 0);
begin
  if UIButton.ContainsKey(ImageEntry) then
    Exit;
  var Entry := GetImgEntry(ImageEntry);
  if Entry = nil then
    Exit;
  if not UIData.ContainsKey(Entry.GetPath) then
    DumpData(Entry, UIData, UIImages);
  var Button := TAButton.Create(UIEngine.AForm(UIOwner));
  with Button do
  begin
    ImageEntry := Entry.Get('normal/0');
    Width := Entry.Get2('normal/0').Canvas.Width;
    Height := Entry.Get2('normal/0').Canvas.Height;
    Left := X + -Entry.Get('normal/0/origin').Vector.X;
    Top := Y + -Entry.Get('normal/0/origin').Vector.Y;
    ImageHover := Entry.Get('mouseOver/0');
    ImagePressed := Entry.Get('pressed/0');
    ImageDisabled := Entry.Get('disabled/0');
  end;
  UIButton.AddOrSetValue(ImageEntry, Button);
end;

procedure CreateButton(ButtonName, ImageEntry: string; X: Integer = 0; Y: Integer = 0);
begin
  if UIButton.ContainsKey(ButtonName) then
    Exit;
  var Entry := GetImgEntry(ImageEntry);
  if Entry = nil then
    Exit;
  if not UIData.ContainsKey(Entry.GetPath) then
    DumpData(Entry, UIData, UIImages);
  var Button := TAButton.Create(UIEngine.AForm(UIOwner));
  with Button do
  begin
    ImageEntry := Entry.Get('normal/0');
    Width := Entry.Get2('normal/0').Canvas.Width;
    Height := Entry.Get2('normal/0').Canvas.Height;
    Left := X + -Entry.Get('normal/0/origin').Vector.X;
    Top := Y + -Entry.Get('normal/0/origin').Vector.Y;
    ImageHover := Entry.Get('mouseOver/0');
    ImagePressed := Entry.Get('pressed/0');
    ImageDisabled := Entry.Get('disabled/0');
  end;
  UIButton.AddOrSetValue(ButtonName, Button);
end;

procedure CreateButtons(Dir: string; BtName: array of string; X: Integer = 0; Y: Integer = 0);
var
  I: Integer;
begin
  for I := 0 to High(BtName) do
    CreateButton(Dir + '/' + BtName[I], X, Y);
end;

procedure CreateImage(ImageEntry: string; AScaleX: Single = 1; AScaleY: Single = 1; X: Integer = 0; Y: Integer = 0);
begin
  if UIImage.ContainsKey(ImageEntry) then
    Exit;
  var Entry := GetImgEntry(ImageEntry);
  if not UIData.ContainsKey(Entry.GetPath) then
    DumpData(Entry, UIData, UIImages);
  var Image := TAImage.Create(UIEngine.AForm(UIOwner));
  with Image do
  begin
    ImageEntry := Entry;
    Width := Entry.Canvas.Width;
    Height := Entry.Canvas.Height;
    Left := X + -Entry.Child['origin'].Vector.X;
    Top := Y + -Entry.Child['origin'].Vector.Y;
    ScaleX := AScaleX;
    ScaleY := AScaleY;
    CanMoveHandle := False;
  end;
  UIImage.AddOrSetValue(ImageEntry, Image);
end;

procedure CreateImage(UIName, ImageEntry: string; AScaleX: Single = 1; AScaleY: Single = 1; X:
  Integer = 0; Y: Integer = 0);
begin
  if UIImage.ContainsKey(UIName) then
    Exit;
  var Entry := GetImgEntry(ImageEntry);
  if not UIData.ContainsKey(Entry.GetPath) then
    DumpData(Entry, UIData, UIImages);
  var Image := TAImage.Create(UIEngine.AForm(UIOwner));
  with Image do
  begin
    ImageEntry := Entry;
    Width := Entry.Canvas.Width;
    Height := Entry.Canvas.Height;
    Left := X + -Entry.Child['origin'].Vector.X;
    Top := Y + -Entry.Child['origin'].Vector.Y;
    ScaleX := AScaleX;
    ScaleY := AScaleY;
    CanMoveHandle := False;
  end;
  UIImage.AddOrSetValue(UIName, Image);
end;

procedure CreateImages(Dir: string; ImageName: array of string; X: Integer = 0; Y: Integer = 0);
var
  I: Integer;
begin
  for I := 0 to High(ImageName) do
    CreateImage(Dir + ImageName[I], X, Y);
end;

procedure CreateUIs(EntryName: string; X, Y: Integer; wClose: Boolean = True);
var
  Entry, Iter: TWZIMGEntry;
  Ax: Integer;
  FormEntry: string;
begin
  Entry := GetImgEntry(EntryName);

  for Iter in Entry.Children do
  begin
    if Iter.Name = 'backgrnd' then
    begin
      FormEntry := Iter.GetPath;
      CreateForm(FormEntry, X, Y);
    end;
    if (Iter.Name = 'backgrnd1') or (Iter.Name = 'backgrnd2') or (Iter.Name = 'backgrnd3') then
      CreateImage(Iter.GetPath);

    if LeftStr(Iter.Name, 2) = 'Bt' then
      CreateButton(Iter.GetPath);

  end;

end;

procedure CreateButtonAll(EntryName: string; IgnoreDir: array of string; X, Y: Integer);
var
  Entry, Iter: TWZIMGEntry;
  I: Integer;
begin
  Entry := GetImgEntry(EntryName);

  for Iter in Entry.Children do
    if (LeftStr(Iter.Name, 2) = 'Bt') then
    begin
      case High(IgnoreDir) of
        0:
          if (Iter.Name = IgnoreDir[0]) then
            Continue;
        1:
          if (Iter.Name = IgnoreDir[0]) or (Iter.Name = IgnoreDir[1]) then
            Continue;
        2:
          if (Iter.Name = IgnoreDir[0]) or (Iter.Name = IgnoreDir[1]) or (Iter.Name = IgnoreDir[2]) then
            Continue;
        3:
          if (Iter.Name = IgnoreDir[0]) or (Iter.Name = IgnoreDir[1]) or (Iter.Name = IgnoreDir[2])
            or (Iter.Name = IgnoreDir[3]) then
            Continue;
      end;
      CreateButton(Iter.GetPath, X, Y);
    end;

end;

procedure CreateEdit(EntryName: string; X, Y, AWidth: Integer);
begin
  with TAEditBox.Create(UIEngine.AForm(UIOwner)) do // UIEngine.AForm(ParentName)
  begin
    Color.SetFillColor(cRGB4(0, 0, 0, 0));
   // Zfont := ZFonta;
    BorderWidth := 0;
    Left := X;
    Top := Y;
    Width := AWidth;
    Height := 40;
    Name := ToName(EntryName);

    SelStart := 0;
  end;
end;

procedure CreateLabel(EntryName: string; AText: string; X, Y: Integer; LabelColor: TLabelColor = lcBlack);
var
  ALabel: TALabel;
begin
  if not UILabel.ContainsKey(EntryName) then
  begin
    ALabel := TALabel.Create(UIEngine.AForm(UIOwner));
    with ALabel do
    begin
      case LabelColor of
        lcBlack:
          FontColor := ARGB(255, 80, 80, 80);
        lcRed:
          FontColor := ARGB(255, 220, 0, 0);
        lcWhite:
          FontColor := $FFFFFFFF;
      end;
      Left := X;
      Top := Y;
      Width := 100; //AWidth;
      Height := 17;
      Text := AText;
      CanMoveHandle := False;
    end;
    UILabel.Add(EntryName, ALabel);
  end;

end;

procedure CreateGrid(AImagePath: string; X, Y, Col, Row: Integer; OwnerName: string);
var
  I: Integer;
  Entry: TWZIMGEntry;
  // IDs:array[0..5] of string=('0100100','0100101','01');
begin

  for I := 1000000 to 1000010 do
  begin
    Entry := CharacterWZ.GetImgFile('Cap/' + '0' + IntToStr(I) + '.img').Root.Get('info/icon');
    UIImages.Add(Entry, Entry.Canvas.Dump(ceNone, 0));

  end;

  with TADropPanel.Create(UIEngine.AForm(ToName(OwnerName))) do // UIEngine.AForm(ParentName)
  begin
    //
    // ImageName := AImageName;
    // itemcolor.SetFillColor(crgb4(0,0,0,0));

    RowWidth := 30;
    RowHeight := 30;
    Name := ToName(AImagePath);
    Color.SetFillColor(cRGB4(250, 0, 0, 0));
    BorderWidth := 1;
    Rows := Row;
    Columns := Col;
    Left := X;
    Top := Y;
    // Margin:=5;
    Items[1].ID := 105;
    // Items[1].Image :='01000001';
    Items[2].ID := 105;
    // Items[2].Image :='01000005';
  end;
end;

procedure CreateDummy(W, H, X, Y: Integer);
var
  Entry: TWZIMGEntry;
  Form: TAForm;
  Ax: Integer;
  fColor: tfillcolor;
begin

  fColor := tfillcolor.Create;
 //fcolor.SetFillColor(200,100,100,100);
  Form := TAForm.Create(UIEngine.Root);
  with Form do
  begin
    ImageEntry := nil;
    //Name := 'c';//ToName(GetPathN(Entry));
    //UIOwner := Name;
    Width := W; //Entry.Canvas.Width;
    Height := H; //Entry.Canvas.Height;
    Left := X; //X + -Entry.Child['origin'].Vector.X + 1000;
    Top := Y; //Y + -Entry.Child['origin'].Vector.Y + 1000;
    BorderWidth := 0;
    ShadowWidth := 0;
    Color := fColor;
   // Ax := Width - 22;
  end;
  fColor.Free;
end;

procedure ShowForm(Name: string);
begin
  UIForm[Name].BringToFront;
  UIForm[Name].Visible := not UIForm[Name].Visible;
end;

var
  MouseDownPosY: Integer;
  DragEnabled: Boolean;

initialization
  UIData := TObjectDictionary<string, TWZIMGEntry>.Create;
  UIImages := TObjectDictionary<TWZIMGEntry, TTexture>.Create;
  UIButton := TDictionary<string, TAButton>.Create;
  UIForm := TDictionary<string, TAForm>.Create;
  UIImage := TDictionary<string, TAImage>.Create;
  UILabel := TDictionary<string, TALabel>.Create;
  //UITab := TDictionary<string, TUITab>.Create;

  GameCursor := TGameCursor.Create;
  GameCursor.CursorNumber := '2';

end.

