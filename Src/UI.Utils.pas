unit UI.Utils;

interface

uses
  Windows, Types, controls, SysUtils, StrUtils, AsphyreSprite, Generics.Collections, WZIMGFile, Math,
   AbstractTextures, WZArchive, ACtrlEditBoxes, AsphyreTypes, DX9Textures, AControls,
  ACtrlEngine, ACtrlForms, ACtrlButtons, TypInfo, ACtrlImages, ACtrlDropPanels, ACtrlLabels, Tools,
  WZDirectory, WZReader, KeyHandler, Global, Classes, AsphyreFonts, ACtrlTypes,PXT.Graphics;

procedure CreateUIs(EntryName: string; X, Y: Integer; wClose: Boolean = True);


procedure CreateButton(EntryName: string; X: Integer = 0; Y: Integer = 0);

procedure CreateButtons(Dir: string; BtName: array of string; X: Integer = 0; Y: Integer = 0);

procedure CreateButtonAll(EntryName: string; IgnoreDir: array of string; X, Y: Integer);

procedure CreateImages(Dir: string; ImageName: array of string; X: Integer = 0; Y: Integer = 0);

procedure CreateForm(EntryName: string; X, Y: Integer; BtClose: Boolean = True);
procedure CreateEmptyForm(EntryName: string; X, Y,AWidth,AHeight: Integer;ACanMove:Boolean=True);

procedure CreateFormEx(EntryName: string; X, Y: Integer);

procedure CreateEdit(EntryName: string; X, Y, AWidth: Integer);

procedure CreateLabel(EntryName, AText: string; X, Y: Integer; AWidth: Integer = 100; Black: Boolean = True);

procedure CreateImage(EntryName: string;AScaleX:Single=1;AScaleY:Single=1; X: Integer = 0; Y: Integer = 0);

procedure CreateGrid(AImagePath: string; X, Y, Col, Row: Integer; OwnerName: string);

procedure CreateDummy(W, H, X, Y: Integer);

procedure ShowForm(Name: string);

function HasUI(Key: TWZIMGEntry): Boolean;

var

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
   WzUtils, ColorUtils,minimap;

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

procedure CreateForm(EntryName: string; X, Y: Integer; BtClose: Boolean = True);
var
  Entry: TWZIMGEntry;
  Form: TAForm;
  Ax: Integer;
begin
  Entry := GetImgEntry(EntryName);
  DumpData(Entry, UIData, UIImages);
  Form := TAForm.Create(UIEngine.Root);
  with Form do
  begin
    ImageEntry := Entry;
    Width := Entry.Canvas.Width;
    Height := Entry.Canvas.Height;
    Left := X + -Entry.Child['origin'].Vector.X + 1000;
    Top := Y + -Entry.Child['origin'].Vector.Y + 1000;
    Ax := Width - 22;
  end;
  if BtClose then
    CreateButton('UI/Basic.img/BtClose3', Ax, 7);
  UIForm.Add(EntryName, Form);
end;

procedure CreateEmptyForm(EntryName: string; X, Y,AWidth,AHeight: Integer;ACanMove:Boolean=True);
var
  Form: TAForm;
begin

  Form := TAForm.Create(UIEngine.Root);
  with Form do
  begin
    Left := X + 1000;
    Top := Y + 1000;
    Width := AWidth;
    Height := AHeight;
    CanMove:=ACanMove;
  end;
  UIForm.Add(EntryName, Form);
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

procedure CreateButton(EntryName: string; X: Integer = 0; Y: Integer = 0);
var
  Entry: TWZIMGEntry;
  Button: TAButton;
begin
  Entry := GetImgEntry(EntryName);
  DumpData(Entry, UIData, UIImages);

  Button := TAButton.Create(UIEngine.AForm(UIOwner));
  with Button do
  begin
    ImageEntry := Entry.Get('normal/0');
    Width := Entry.Get('normal/0').Canvas.Width;
    Height := Entry.Get('normal/0').Canvas.Height;
    Left := X + -Entry.Get('normal/0/origin').Vector.X;
    Top := Y + -Entry.Get('normal/0/origin').Vector.Y;
    ImageHover := Entry.Get('mouseOver/0');
    ImagePressed := Entry.Get('pressed/0');
    ImageDisabled := Entry.Get('disabled/0');
  end;
  UIButton.AddOrSetValue(EntryName, Button);
end;

procedure CreateButtons(Dir: string; BtName: array of string; X: Integer = 0; Y: Integer = 0);
var
  I: Integer;
begin
  for I := 0 to High(BtName) do
    CreateButton(Dir +'/'+ BtName[I], X, Y);
end;

procedure CreateImage(EntryName: string;AScaleX:Single=1;AScaleY:Single=1; X: Integer = 0; Y: Integer = 0);
var
  Entry: TWZIMGEntry;
  Image: TAImage;
begin
  Entry := GetImgEntry(EntryName);
  if not UIData.ContainsKey(Entry.GetPath) then
    DumpData(Entry, UIData, UIImages);
  Image := TAImage.Create(UIEngine.AForm(UIOwner));
  with Image do
  begin
    ImageEntry := Entry;
    Width := Entry.Canvas.Width;
    Height := Entry.Canvas.Height;
    Left := X + -Entry.Child['origin'].Vector.X;
    Top := Y + -Entry.Child['origin'].Vector.Y;
    ScaleX:=AScaleX;
    ScaleY:=AScaleY;
    CanMoveHandle := False;
   end;
  UIImage.AddOrSetValue(EntryName, Image);
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
      CreateForm(FormEntry, X, Y, wClose);
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

procedure CreateLabel(EntryName: string; AText: string; X, Y: Integer; AWidth: Integer = 100; Black: Boolean = True);
var
  ALabel: TALabel;
begin
  ALabel := TALabel.Create(UIEngine.AForm(UIOwner));
  with ALabel do // UIEngine.AForm(ParentName)
  begin
    Color.SetFillColor(cRGB4(0, 0, 0, 0));
  //  Zfont := ZFontb;
  //  if Black then
    //  FontColor.SetFontColor(cRGB1(85, 85, 85), cRGB1(85, 85, 85))
    //else
    //  FontColor.SetFontColor($FFFF0000, $FFFF0000);
    BorderWidth := 0;
    Left := X;
    Top := Y;
    Width := AWidth;
    Height := 17;
    Text := AText;
    CanMoveHandle := False;
    TextHorizontalAlign := hLeft;
    Name := ToName(EntryName);
  end;
  UILabel.Add(EntryName, ALabel);
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

end.



