unit AvatarUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Buttons, AdvUtil, PNGMapleCanvasEx,
  WZArchive, Vcl.Grids, AdvObj, BaseGrid, AdvGrid, Vcl.StdCtrls, StrUtils, Generics.Collections,
  Generics.Defaults, hyieutils, iexBitmaps, hyiedefs, iesettings, iexLayers, iexRulers, ieview,
  pngimage, iemview, Vcl.ComCtrls;

type
  TAvatarForm = class(TForm)
    SpeedButton9: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton16: TSpeedButton;
    SpeedButton15: TSpeedButton;
    SpeedButton14: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton1: TSpeedButton;
    Panel1: TPanel;
    Inventory: TAdvStringGrid;
    SpeedButton17: TSpeedButton;
    SpeedButton18: TSpeedButton;
    SpeedButton19: TSpeedButton;
    SpeedButton20: TSpeedButton;
    SaveButton: TButton;
    DeleteButton: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Shape1: TShape;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SpeedButton9Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure InventoryButtonClick(Sender: TObject; ACol, ARow: Integer);
    procedure AvatarViewImageSelect(Sender: TObject; idx: Integer);
    procedure PageControl1Change(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
  private
    HasShow: Boolean;
    Wz: TWZArchive;
    IconList: TObjectDictionary<string, TObjectList<TBmpEx>>;
    ImageGrids: array[1..20] of TImageEnMView;
    AvatarView: TImageEnMView;
    HasLoaded: TList<Integer>;
    PartIndex: Integer;
    DeleteIdx: Integer;
    DeleteFileName: string;
    procedure ImageGridSelect(Sender: TObject; idx: Integer);
    procedure AddInventory(ID: string; Icon: TBitmap; Name: string; ARow: Integer);
    procedure AvatarTargetEvent(Sender: TObject);
     { Private declarations }
  public
    procedure AddEqps(EqpID: string);
    { Public declarations }
  end;

var
  AvatarForm: TAvatarForm;

implementation

uses
  WZIMGFile, WZDirectory, MapleEffect, Global, MapleCharacter, LockRenderTarget, AsphyreTypes,
  WzUtils;

{$R *.dfm}

const
  AvatarWidth = 100;
  AvatarHeight = 100;

procedure TAvatarForm.AddInventory(ID: string; Icon: TBitmap; Name: string; ARow: Integer);
begin
  Inventory.Cells[0, ARow] := ID;
  Inventory.CreatePicture(1, ARow, True, noStretch, 0, haCenter, vaCenter).Assign(Icon);
  Inventory.Cells[2, ARow] := Name;
  if (Leftstr(ID,4)<>'0000')  and (Leftstr(ID,4)<>'0001')then
     Inventory.AddButton(3, ARow, 20, 20, 'X', haCenter, vaCenter);
end;

procedure TAvatarForm.AddEqps(EqpID: string);
begin

  Player.LoadEquip(EqpID);

  var NewID := EqpID;
  DeleteID := '999999';
  var Part := GetPart(NewID);

  if (Part = Cap) then
  begin
    DressCap := True;
    var Data := GetImgEntry('Character.wz/Cap/' + NewID + '.img/info/vslot').Data;
    //no Cover
    if (Data = 'Cp') or (Data = 'CpH5') then
      CapType := 0;
    //stand cover
    if Data = 'CpH1H5' then
      CapType := 1;
    //cover all
    if Length(Data) > 12 then
      CapType := 2;
  end;

  if (Part = Hair) then
    ShowHair := True;

  var NewPart := GetPart(NewID);

  for var i := PlayerEqpList.Count - 1 downto 0 do
  begin

    var OldPart := GetPart(PlayerEqpList[i]);

    if NewPart = OldPart then
    begin
      TSetEffect.Delete(PlayerEqpList[i]);
      TItemEffect.Delete(PlayerEqpList[i]);
      PlayerEqpList.Delete(i);
    end;

    if (NewPart = Weapon) and (OldPart = CashWeapon) then
      PlayerEqpList.Delete(i);
    if (NewPart = CashWeapon) and (OldPart = Weapon) then
      PlayerEqpList.Delete(i);

    if (NewPart = Coat) or (NewPart = Pants) then
      if (OldPart = Longcoat) then
        PlayerEqpList.Delete(i);

    if (NewPart = Longcoat) then
      if (OldPart = Coat) or (OldPart = Pants) then
        PlayerEqpList.Delete(i);

  end;
  PlayerEqpList.Add(NewID);

  if TItemEffect.AllList.contains(EqpID) then
    TItemEffect.Create(EqpID, True);

  if TSetEffect.AllList.ContainsKey(EqpID) then
    TSetEffect.Create(EqpID);

  case Part of
    Weapon:
      begin
        DressNormalWeapon := True;
        DressCashWeapon := False;
      end;
    CashWeapon:
      begin
        DressCashWeapon := True;
        DressNormalWeapon := False;
      end;
    Coat:
      begin
        DressCoat := True;
        DressLongcoat := False;
      end;
    Pants:
      begin
        DressPants := True;
        DressLongcoat := False;
      end;
    Longcoat:
      begin
        DressLongcoat := True;
        DressPants := False;
        DressCoat := False;
      end;
    Cape:
      DressCape := True;
    Glove:
      DressGlove := True;
    EarRing:
      DressEarring := True;
    Glass:
      DressGlass := True;
    FaceAcc:
      DressFaceAcc := True;
    Shield:
      DressShield := True;
    Shoes:
      DressShoes := True;
    Face:
      DressFace := True;
  end;

  if Part = SitTamingMob then
    Exit;
  ChangeState := True;

  if WeaponWalkType.contains('stand2') then
    NewState := 'stand2'
  else
    NewState := 'stand1';

  var TrimID: string;
  if LeftStr(EqpID, 3) = '000' then
    TrimID := RightStr(EqpID, 5)
  else
    TrimID := RightStr(EqpID, 7);

  var Dir := GetDir(EqpID);
  var Name := StringWZ.GetImgFile('Eqp.img').Root.Get('Eqp/' + Dir + TrimID + '/name', '');
  var Entry := CharacterWZ.GetImgFile(Dir + EqpID + '.img').Root;

  var Bmp: TBitmap;
  case Part of
    Head:
      Bmp := Entry.Get2('front/head').Canvas.DumpBmp;
    Body:
      Bmp := Entry.Get2('stand1/0/body').Canvas.DumpBmp;
    Hair:
      Bmp := Entry.Get2('default/hairOverHead').Canvas.DumpBmp;
    Face:
      Bmp := Entry.Get2('default/face').Canvas.DumpBmp;
  else
    Bmp := Entry.Get2('info/icon').Canvas.DumpBmp;
  end;

  var Row := 0;
  for var i := 1 to Inventory.RowCount - 1 do
  begin
   // if Inventory.Cells[0, i]='' then continue;
    if GetPart(Inventory.Cells[0, i]) = Part then
      AddInventory(NewID, Bmp, Name, i)
    else
      Inc(Row);
  end;

  if Row = Inventory.RowCount - 1 then
  begin
    Inventory.RowCount := Row + 2;
    AddInventory(NewID, Bmp, Name, Row + 1);
  end;

  for var i := Inventory.RowCount - 1 downto 1 do
  begin
    var InvPart := GetPart(Inventory.Cells[0, i]);
    if (Part = Coat) or (Part = Pants) then
      if (InvPart = Longcoat) then
      begin
        Inventory.RemoveRows(i, 1);
        Break;
      end;

    if (Part = Weapon) and (InvPart = CashWeapon) then
    begin
      Inventory.RemoveRows(i, 1);
      Break;
    end;

    if (Part = CashWeapon) and (InvPart = Weapon) then
    begin
      Inventory.RemoveRows(i, 1);
      Break;
    end;

    if (Part = Longcoat) then
      if (InvPart = Coat) or (InvPart = Pants) then
        Inventory.RemoveRows(i, 1);
  end;
  Bmp.Free;

end;

procedure TAvatarForm.ImageGridSelect(Sender: TObject; idx: Integer);
begin
  AddEqps(ImageGrids[PartIndex].ImageInfoText[idx]);
  ActiveControl := nil;
end;

procedure TAvatarForm.InventoryButtonClick(Sender: TObject; ACol, ARow: Integer);
begin
  var Part := GetPart(Inventory.Cells[0, ARow]);
  DeleteID := Inventory.Cells[0, ARow];
  PlayerEqpList.Remove(DeleteID);
  if Part = Cap then
    DressCap := False;
  if Part = Hair then
    ShowHair := False;
  if TItemEffect.AllList.contains(DeleteID) then
    TItemEffect.Delete(DeleteID);
  if TSetEffect.AllList.containsKey(DeleteID) then
    TSetEffect.Delete(DeleteID);

  Inventory.RemoveRows(ARow, 1);
end;

procedure TAvatarForm.DeleteButtonClick(Sender: TObject);
begin
  AvatarView.DeleteImage(DeleteIdx);
  DeleteFile(DeleteFileName);
end;

procedure TAvatarForm.PageControl1Change(Sender: TObject);
begin
  case PageControl1.TabIndex of
    0:
      begin
        SaveButton.Enabled := True;
        DeleteButton.Enabled := False;
      end;

    1:
      begin
        SaveButton.Enabled := False;
        DeleteButton.Enabled := True;
        AvatarView.Clear;
        AvatarView.FillFromDirectory(ExtractFilePath(ParamStr(0)) + 'Images\');
      end;
  end;
end;

procedure TAvatarForm.AvatarTargetEvent;
begin
  var WX := Round(Player.X - SpriteEngine.WorldX - 55);
  var WY := Round(Player.Y - SpriteEngine.WorldY - 90);
  GameCanvas.UseTexturePx(AvatarTargets[TPlayer.AvatarPanelIndex], pxBounds4(WX, WY, AvatarWidth, AvatarHeight));
  GameCanvas.TexMap(pBounds4s(0, 0, AvatarWidth, AvatarHeight, 1), clWhite4);
end;

procedure TAvatarForm.AvatarViewImageSelect(Sender: TObject; idx: Integer);
begin
  DeleteIdx := idx;
  DeleteFileName := AvatarView.ImageFileName[idx];
  var ImageName := ExtractFileName(AvatarView.ImageFileName[idx]);
  var Explode: TArray<string> := ImageName.Split(['-']);

  for var Iter in TItemEffect.UseList.Keys do
    TItemEffect.UseList[Iter].Dead;
  TItemEffect.UseList.Clear;

  for var Iter in TSetEffect.UseList.Keys do
    TSetEffect.UseList[Iter].Dead;
  TSetEffect.UseList.Clear;

  Inventory.RemoveRows(1, 20);

 // playereqpList.Clear;
  DressFace := False;
  ShowHair := False;
  DressNormalWeapon := False;
  DressCashWeapon := False;
  DressCoat := False;
  DressLongcoat := False;
  DressPants := False;
  DressCap := False;
  DressCape := False;
  DressCape := False;
  DressGlove := False;
  DressEarring := False;
  DressGlass := False;
  DressFaceAcc := False;
  DressShield := False;
  DressShoes := False;

  for var i := 0 to High(Explode) - 1 do
    AvatarForm.AddEqps(Explode[i]);
  ActiveControl := nil;
end;

procedure TAvatarForm.SaveButtonClick(Sender: TObject);
type
  TRGB32 = record
    B, G, R, A: Byte;
  end;

  TRGB32Array = array[0..MaxInt div SizeOf(TRGB32) - 1] of TRGB32;

  PRGB32Array = ^TRGB32Array;
var
  Index: Integer;
  pDest: Pointer;
  nPitch: Integer;
  A, R, G, B: Byte;
  MapName: string;
  PSrcTex: PLongWord;
  Line: PRGB32Array;
  Bmp: TBitmap;
 // Jpg: TJpegImage;
  Color: Cardinal;
begin
  FreeAndNil(LockRenderTargets);
  LockRenderTargets := TLockableRenderTarget.Create;
  LockRenderTargets.SetSize(AvatarWidth, AvatarHeight);
  Index := LockRenderTargets.Add(1, AvatarWidth, AvatarHeight, apf_A8R8G8B8, True, True);
  GameDevice.RenderTo(AvatarTargetEvent, ARGB(255, 255, 255, 255), True, LockRenderTargets[Index]);
  Bmp := TBitmap.Create;
  Bmp.PixelFormat := pf32bit;
  Bmp.AlphaFormat := afPremultiplied;
  Bmp.Width := AvatarWidth;
  Bmp.Height := AvatarHeight;

  LockRenderTargets.Lock(Index, pDest, nPitch);
  PSrcTex := pDest;
  for var j := 0 to AvatarHeight - 1 do
  begin
    Line := Bmp.Scanline[j];
    for var i := 0 to AvatarWidth - 1 do
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

  var ImageName: string;
  for var i := 1 to Inventory.RowCount - 1 do
    ImageName := ImageName + Inventory.Cells[0, i] + '-';
  ForceDirectories(ExtractFilePath(ParamStr(0)) + 'Images');
  var FileName := ExtractFilePath(ParamStr(0)) + 'Images\' + ImageName + '.bmp';
  Bmp.SaveToFile(FileName);
  FreeAndNil(Bmp);

end;

procedure TAvatarForm.FormCreate(Sender: TObject);
begin
  IconList := TObjectDictionary<string, TObjectList<TBmpEx>>.Create([doOwnsValues]);
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
  HasLoaded := TList<Integer>.Create;

  for var i := 1 to 20 do
  begin
    ImageGrids[i] := TImageEnMView.Create(AvatarForm);
    ImageGrids[i].Parent := PageControl1.Pages[0];
    ImageGrids[i].Visible := False;
    ImageGrids[i].Align := alClient;
    ImageGrids[i].AlignWithMargins := True;
    ImageGrids[i].Margins.Left := 3;
    ImageGrids[i].Margins.Right := 3;
    ImageGrids[i].Margins.Top := 3;
    ImageGrids[i].Margins.Bottom := 3;
    ImageGrids[i].BorderStyle := bsNone;
    ImageGrids[i].Background := clWhite;
    ImageGrids[i].ThumbWidth := 35;
    ImageGrids[i].ThumbHeight := 35;
    ImageGrids[i].ThumbnailOptionsEx := [ietxShowIconForUnknownFormat, ietxShowIconWhileLoading,
      ietxEnableInternalIcons];
    ImageGrids[i].DefaultInfoText := iedtNone;
    ImageGrids[i].MultiSelectionOptions := [];
    ImageGrids[i].ShowText := False;
    ImageGrids[i].SelectionColor := clRed;
    ImageGrids[i].OnImageSelect := ImageGridSelect;
  end;

  ImageGrids[1].Visible := True;
  AvatarView := TImageEnMView.Create(AvatarForm);
  AvatarView.Parent := PageControl1.Pages[1];
  AvatarView.Align := alClient;
  AvatarView.AlignWithMargins := True;
  AvatarView.Margins.Left := 3;
  AvatarView.Margins.Right := 3;
  AvatarView.Margins.Top := 3;
  AvatarView.Margins.Bottom := 3;
  AvatarView.ThumbWidth := 102;
  AvatarView.ThumbHeight := 102;
  AvatarView.BorderStyle := bsNone;
  AvatarView.Background := clWhite;
  AvatarView.ThumbnailOptionsEx := [ietxShowIconForUnknownFormat, ietxShowIconWhileLoading, ietxEnableInternalIcons];
  AvatarView.DefaultInfoText := iedtNone;
  AvatarView.MultiSelectionOptions := [];
  AvatarView.ShowText := False;
  AvatarView.ThumbnailsBackground := RGB(200, 200, 200);
  AvatarView.ThumbsRounded := 100;
  AvatarView.SelectionColor := clRed;
  AvatarView.OnImageSelect := AvatarViewImageSelect;

  Inventory.ColumnHeaders.Add('ID');
  Inventory.ColumnHeaders.Add('Icon');
  Inventory.ColumnHeaders.Add('Name');
  Inventory.ColumnHeaders.Add('Delete');
  Inventory.RowHeights[0] := 22;
  Inventory.ColWidths[0] := 80;
  Inventory.ColWidths[1] := 40;  //icon
  Inventory.ColWidths[2] := 105;
  Inventory.ColWidths[3] := 47;
  for var i := 0 to Inventory.ColCount - 1 do
  begin
    Inventory.CellProperties[i, 0].Alignment := taCenter;
    Inventory.CellProperties[i, 0].FontSize := 15;
  end;
end;

procedure TAvatarForm.FormDestroy(Sender: TObject);
begin
  IconList.Free;
  if Wz <> nil then
    Wz.Free;
  HasLoaded.Free;
end;

procedure TAvatarForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_MENU then
    Key := 0;
end;

procedure TAvatarForm.FormShow(Sender: TObject);
begin
  if HasShow then
    Exit;
  HasShow := True;
  var DefaultEqps := ['01302030', '01062055', '01072054', '01040005', '00030020', '00020000', '00002000', '00012000'];
  var TrimID: string;
  var Bmp: TBitmap;

  for var i := 0 to 7 do
  begin
    if LeftStr(DefaultEqps[i], 3) = '000' then
      TrimID := RightStr(DefaultEqps[i], 5)
    else
      TrimID := RightStr(DefaultEqps[i], 7);

    var Dir := GetDir(DefaultEqps[i]);
    var Name := StringWZ.GetImgFile('Eqp.img').Root.Get('Eqp/' + Dir + TrimID + '/name', '');

    var Entry := CharacterWZ.GetImgFile(Dir + DefaultEqps[i] + '.img').Root;

    if Dir = '' then
    begin
      if LeftStr(DefaultEqps[i], 4) = '0000' then
        Bmp := Entry.Get2('stand1/0/body').Canvas.DumpBmp
      else
        Bmp := Entry.Get2('front/head').Canvas.DumpBmp;
    end
    else
    begin
      if Dir = 'Hair/' then
        Bmp := Entry.Get2('default/hairOverHead').Canvas.DumpBmp
      else if Dir = 'Face/' then
        Bmp := Entry.Get2('default/face').Canvas.DumpBmp
      else
        Bmp := Entry.Get2('info/icon').Canvas.DumpBmp;
    end;
    AddInventory(DefaultEqps[i], Bmp, Name, i + 1);
    Bmp.Free;
  end;

end;

function NoIMG(const Name: string): string; inline;
begin
  Result := ChangeFileExt(Name, '');
end;

procedure TAvatarForm.SpeedButton9Click(Sender: TObject);
var
  Num: Integer;
  Name, ID, CharacterDir, Part: string;
  Dir: TWZDirectory;
  img: TWZFile;
  Bmp: TBmpEx;
const
  Icons: array[0..2] of string = ('icon', 'face', 'hairOverHead');

  function Left4: string;
  begin
    Result := LeftStr(img.Name, 4);
  end;

  function InRange(Lo, Hi: Integer): Boolean;
  begin
    Result := (Num >= Lo) and (Num <= Hi);
  end;

begin
  PageControl1.TabIndex := 0;
  SaveButton.Enabled := True;
  case TSpeedButton(Sender).Tag of
    20:
      begin
        CharacterDir := '';
        Part := 'Head';
      end;
    1:
      begin
        CharacterDir := '';
        Part := 'Body';
      end;

    2:
      begin
        CharacterDir := 'Weapon';
        Part := 'Weapon1';
      end;
    3:
      begin
        CharacterDir := 'Weapon';
        Part := 'Weapon2';
      end;
    4:
      begin
        CharacterDir := 'Cap';
        Part := 'Cap1';
      end;
    5:
      begin
        CharacterDir := 'Cap';
        Part := 'Cap2';
      end;
    6:
      begin
        CharacterDir := 'Coat';
        Part := 'Coat';
      end;
    7:
      begin
        CharacterDir := 'Pants';
        Part := 'Pants';
      end;
    8:
      begin
        CharacterDir := 'Longcoat';
        Part := 'Longcoat';
      end;
    9:
      begin
        CharacterDir := 'Cape';
        Part := 'Cape';
      end;
    10:
      begin
        CharacterDir := 'Shield';
        Part := 'Shield';
      end;
    11:
      begin
        CharacterDir := 'Glove';
        Part := 'Glove';
      end;
    12:
      begin
        CharacterDir := 'Shoes';
        Part := 'Shoes';
      end;
    13:
      begin
        CharacterDir := 'Hair';
        Part := 'Hair1';
      end;
    14:
      begin
        CharacterDir := 'Hair';
        Part := 'Hair2';
      end;
    15:
      begin
        CharacterDir := 'Face';
        Part := 'Face1';
      end;
    16:
      begin
        CharacterDir := 'Face';
        Part := 'Face2';
      end;
    17:
      begin
        CharacterDir := 'Accessory';
        Part := 'FaceAcc';
      end;
    18:
      begin
        CharacterDir := 'Accessory';
        Part := 'Glass';
      end;
    19:
      begin
        CharacterDir := 'Accessory';
        Part := 'Earring';
      end;
  end;
  PartIndex := TSpeedButton(Sender).Tag;

  for var i := 1 to 20 do
    ImageGrids[i].Visible := False;
  ImageGrids[PartIndex].Visible := True;
  with ImageGrids[PartIndex].GetCanvas do
  begin
    Font.Size := 24;
    TextOut(100, 100, 'Loading...')
  end;

  if (PartIndex = 13) or (PartIndex = 14) then
  begin
    ImageGrids[PartIndex].ThumbWidth := 55;
    ImageGrids[PartIndex].ThumbHeight := 55;
  end
  else
  begin
    ImageGrids[PartIndex].ThumbWidth := 35;
    ImageGrids[PartIndex].ThumbHeight := 35;
  end;

  if not IconList.ContainsKey(Part) then
  begin
    if Wz <> nil then
      Wz.Free;
    Wz := TWZArchive.Create(WzPath + '\Character.wz');
    var List := TObjectList<TBmpEx>.Create;
   // Dir := TWZDirectory(Wz.Root.Entry[CharacterDir]);

    if CharacterDir = '' then
      Dir := TWZDirectory(Wz.Root)
    else
      Dir := TWZDirectory(Wz.Root.Entry[CharacterDir]);
    for img in Dir.Files do
    begin

      if not IsNumber(img.Name[1]) then
        Continue;
      ID := Trim(NoIMG(img.Name));
      if (ID = '01702653') or (ID = '01702700') or (ID = '01702220') then
        Continue;
      if Left4 = '0135' then
        Continue;
      if Left4 = '0169' then
        Continue;
      if Left4 = '0150' then
        Continue;
      if Left4 = '0151' then
        Continue;
      if Left4 = '0160' then
        Continue;

      Name := StringWZ.GetImgFile('Eqp.img').Root.Get('Eqp/' + CharacterDir + '/' + IDToInt(ID) + '/name', '');
      if PartIndex in [4, 5, 13, 14, 15, 16] then
        Num := ID.ToInteger div 1000;
      case PartIndex of
        20:
          if Left4 <> '0001' then
            Continue;
        1:
          if Left4 <> '0000' then
            Continue;
        2: // weapon
          if Left4 = '0170' then
            Continue;
        3: // cash weapon
          if Left4 <> '0170' then
            Continue;
        4: // Cap1
          if not InRange(1000, 1003) then
            Continue;
        5: // cap2
          if not InRange(1004, 1006) then
            Continue;
        13: // Hair1
          if not InRange(30, 36) then
            Continue;
        14: // Hair2
          if not InRange(37, 48) then
            Continue;
        15: // Face1
          if not InRange(20, 23) then
            Continue;
        16: // Face2
          if not InRange(24, 28) then
            Continue;
        17:
          if Left4 <> '0101' then
            Continue;
        18:
          if Left4 <> '0102' then
            Continue;
        19:
          if Left4 <> '0103' then
            Continue;
      end;

      with Wz.ParseFile(img) do
      begin
        for var Iter in Root.Children do
        begin
          if (Iter.Name = 'front') and (PartIndex = 20) then
          begin
            Bmp := Iter.Get2('head').Canvas.DumpBmpEx;
            Bmp.ID := ID;
            Bmp.Name := Name;
            List.Add(Bmp);
          end;
          if (Iter.Name = 'stand1') and (PartIndex = 1) then
          begin
            Bmp := Iter.Get2('0/body').Canvas.DumpBmpEx;
            Bmp.ID := ID;
            Bmp.Name := Name;
            List.Add(Bmp);
          end;

          for var i := 0 to 2 do
          begin
            if Iter.Child[Icons[i]] <> nil then
            begin
              Bmp := Iter.Get2(Icons[i]).Canvas.DumpBmpEx;
              Bmp.ID := ID;
              Bmp.Name := Name;
              List.Add(Bmp);
            end;
          end;
        end;
        Free;
      end;

    end;

    IconList.Add(Part, List);

  end;

  IconList[Part].Sort(TComparer<TBmpEx>.Construct(
    function(const Left, Right: TBmpEx): Integer
    begin
      Result := Left.ID.ToInteger - Right.ID.ToInteger;
    end));

  var Index := -1;
  if not HasLoaded.contains(PartIndex) then
  begin
    HasLoaded.Add(PartIndex);
    for var Iter in IconList[Part] do
    begin
      ImageGrids[PartIndex].AppendImage(Iter);
      Inc(Index);
      ImageGrids[PartIndex].ImageInfoText[Index] := Iter.ID;
    end;
    ImageGrids[PartIndex].ViewX := 0;
    ImageGrids[PartIndex].ViewY := 0;
  end;
  ActiveControl := nil;
end;

end.

