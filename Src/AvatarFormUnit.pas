unit AvatarFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.Buttons, AdvUtil, PNGMapleCanvasEx, WZArchive, Vcl.Grids,
  AdvObj, BaseGrid, AdvGrid, Vcl.StdCtrls, StrUtils, Generics.Collections,
  Generics.Defaults, ieview, pngimage, iemview, Vcl.ComCtrls, ColorUtils,
  WZIMGFile, CurvyControls;

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
    TabSheet3: TTabSheet;
    ColorGrid: TAdvStringGrid;
    TabSheet4: TTabSheet;
    SearchGrid: TAdvStringGrid;
    Edit1: TEdit;
    Label1: TLabel;
    CurvyPanel1: TCurvyPanel;
    IDLabel: TLabel;
    NameLabel: TLabel;
    Button1: TButton;
    Image1: TImage;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    AllFrameListBox: TListBox;
    Label2: TLabel;
    SaveAllButton: TButton;
    SaveSingleButton: TButton;
    Panel2: TPanel;
    TrackBarW: TTrackBar;
    TrackBarH: TTrackBar;
    TrackBarX: TTrackBar;
    TrackBarY: TTrackBar;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Shape2: TShape;
    LabelW: TLabel;
    LabelH: TLabel;
    LabelX: TLabel;
    LabelY: TLabel;
    ExpressionListBox: TComboBox;
    Label7: TLabel;
    TabSheet7: TTabSheet;
    InfoGrid: TAdvStringGrid;
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
    procedure ColorGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure TabSheet4Show(Sender: TObject);
    procedure SearchGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure Button1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure SaveAllButtonClick(Sender: TObject);
    procedure SaveSingleButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AllFrameListBoxClick(Sender: TObject);
    procedure TrackBarWChange(Sender: TObject);
    procedure ExpressionListBoxChange(Sender: TObject);
  private
    HasShow: Boolean;
    HasShowSearchGrid: Boolean;
    SearchEqpID: string;
    Wz: TWZArchive;
    IconList: TObjectDictionary<string, TObjectList<TBmpEx>>;
    ImageGrids: array[1..20] of TImageEnMView;
    AvatarView: TImageEnMView;
    HasLoaded: TList<Integer>;
    PartIndex: Integer;
    DeleteIdx: Integer;
    DeleteID: string;
    DeleteFileName: string;
    DumpRowCount: Integer;
    ColList2: TStringList;
    RowList2: TDictionary<Integer, string>;
    Row1, Row2: Integer;
    WzListObj:TObjectList<TWZArchive>;
    procedure DumpData2(Entry: TWZIMGEntry);
    procedure Dump2(Entry: TWZIMGEntry);
    procedure ImageGridSelect(Sender: TObject; idx: Integer);
    procedure AddInventory(ID: string; Icon: TBitmap; Name: string; ARow: Integer);
    procedure ResetColorGrid;
    procedure DumpEqpString(Entry: TWZIMGEntry);
     { Private declarations }
  public
    SaveAllFrames: Boolean;
    SaveSingleFrame: Boolean;
    ChangeExpressionListBox: Boolean;
    Frame: Integer;
    AllFrames: array of string;
    procedure AddEqps(EqpID: string);
    { Public declarations }
  end;

var
  AvatarForm: TAvatarForm;

implementation

uses
  WZDirectory, MapleEffect, Global, MapleCharacter, PXT.TypesEx, WzUtils,
  MapleCharacterEx, PXT.Types;

{$R *.dfm}

procedure TAvatarForm.Dump2(Entry: TWZIMGEntry);
var
  E: TWZIMGEntry;
  Data: string;
begin

  case Entry.DataType of
    mdtInt, mdtVector, mdtShort, mdtString, mdtFloat, mdtDouble, mdtInt64:
      begin
        if Entry.DataType = mdtVector then
          Data := 'x:' + IntToStr(Entry.Vector.X) + '  ' + 'y:' + IntToStr(Entry.Vector.Y)
        else
          Data := Entry.Data;

        ColList2.Add((Entry).GetPathD + '=' + Data + ',  ');
      end;
  end;

  for E in Entry.Children do
    if Entry.DataType <> mdtCanvas then
      Dump2(E);

end;

procedure TAvatarForm.DumpData2(Entry: TWZIMGEntry);
var
  i: Integer;
  S, FinalStr: string;
begin

  ColList2.BeginUpdate;
  Inc(Row2);
  Dump2(Entry);

  S := Entry.GetPathD + '.';
  for i := 0 to ColList2.Count - 1 do
  begin
    ColList2[i] := StringReplace(ColList2[i], S, '', [rfReplaceAll]);
    FinalStr := FinalStr + ColList2[i];
  end;
  ColList2.EndUpdate;
  Delete(FinalStr, Length(FinalStr) - 2, 1);
  RowList2.Add(Row2, FinalStr);
  ColList2.Clear;
end;

procedure TAvatarForm.AddInventory(ID: string; Icon: TBitmap; Name: string; ARow: Integer);
begin
  Inventory.Cells[0, ARow] := ID;
  Inventory.CreatePicture(1, ARow, True, noStretch, 0, haCenter, vaCenter).Assign(Icon);
  Inventory.Cells[2, ARow] := Name;
  if (LeftStr(ID, 4) <> '0000') and (LeftStr(ID, 4) <> '0001') then
    Inventory.AddButton(3, ARow, 20, 20, 'X', haCenter, vaCenter);
end;

procedure TAvatarForm.AllFrameListBoxClick(Sender: TObject);
begin
  SaveSingleFrame := True;
  SaveSingleButton.Enabled := True;
end;

procedure TAvatarForm.AddEqps(EqpID: string);
begin

  var NewID := EqpID;
  var Part := GetPart(NewID);
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
    TItemEffect.Create(EqpID, Equip);

  if TSetEffect.AllList.ContainsKey(EqpID) then
    TSetEffect.Create(EqpID);

  if Part = SitTamingMob then
    Exit;

  var Path := 'Character/Weapon/';

  var TrimID: string;
  if LeftStr(EqpID, 3) = '000' then
    TrimID := RightStr(EqpID, 5)
  else
    TrimID := RightStr(EqpID, 7);

  var Dir := GetDir(EqpID);
  var Name := GetImgFile('String/Eqp.img').Root.Get('Eqp/' + Dir + TrimID + '/name', '');
  var Entry := GetImgFile('Character/' + Dir + EqpID + '.img').Root;

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

  var Row := -1;
  for var i := 0 to Inventory.RowCount - 1 do
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

  for var i := Inventory.RowCount - 1 downto 0 do
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
  Inventory.SortByColumn(0);

  Player.RemoveSprites;
  for var i := 0 to Inventory.RowCount - 1 do
    Player.Spawn(Inventory.Cells[0, i]);
  ResetColorGrid;
  ActiveControl := nil;
end;

procedure TAvatarForm.InventoryButtonClick(Sender: TObject; ACol, ARow: Integer);
begin
  var Part := GetPart(Inventory.Cells[0, ARow]);
  DeleteID := Inventory.Cells[0, ARow];
  PlayerEqpList.Remove(DeleteID);

  if TItemEffect.AllList.contains(DeleteID) then
    TItemEffect.Delete(DeleteID);
  if TSetEffect.AllList.containsKey(DeleteID) then
    TSetEffect.Delete(DeleteID);

  Inventory.RemoveRows(ARow, 1);
  case Part of
    Hair:
      Player.ShowHair := False;
    Cap:
      Player.DressCap := False;
  end;

  Inventory.SortByColumn(0);
  Player.RemoveSprites;
  for var i := 0 to Inventory.RowCount - 1 do
    Player.Spawn(Inventory.Cells[0, i]);

  ResetColorGrid;
  SaveSingleFrame := False;
  SaveAllFrames := False;
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
        AvatarForm.SaveSingleFrame := False;
      end;

    1:
      begin
        DeleteButton.Enabled := True;
        AvatarView.Parent := PageControl1.Pages[1];
        AvatarView.Clear;
        AvatarView.FillFromDirectory(ExtractFilePath(ParamStr(0)) + 'Images\');
        AvatarForm.SaveSingleFrame := False;
      end;
    2:  //
      begin
        SaveButton.Enabled := True;
        AvatarForm.SaveSingleFrame := False;
      end;
    3:
      AvatarForm.SaveSingleFrame := False;
    4:
      begin
        AvatarView.Parent := PageControl1.Pages[4];
        AvatarView.Clear;
        AvatarView.FillFromDirectory(ExtractFilePath(ParamStr(0)) + 'Images\');
        SaveButton.Enabled := True;
        AvatarForm.SaveSingleFrame := False;
      end;
    5:
      Label2.Caption := '';
    6:   //
      begin
        Row2 := -1;
        InfoGrid.AssignCells(Inventory);
        for var Row := 0 to Inventory.RowCount do
        begin
          if Inventory.CellTypes[1, Row - 1] = ctPicture then
            InfoGrid.CreateBitmap(1, Row - 1, False, haCenter, vaCenter).Assign(Inventory.CellGraphics[1, Row - 1].CellBitmap);
        end;
        InfoGrid.ColWidths[0] := 72;
        InfoGrid.ColWidths[1] := 60;
        InfoGrid.ColWidths[2] := 120;
        InfoGrid.ColWidths[3] := 520;

        InfoGrid.RemoveRows(0, 2);
        for var i := 0 to InfoGrid.RowCount - 1 do
        begin
          var ID := InfoGrid.Cells[0, i];
          DumpData2(GetImgFile('Character/' + GetDir(ID) + ID + '.img').Root.Child['info']);
        end;
        for var i in RowList2.Keys do
          InfoGrid.Cells[3, i] := RowList2[i];
        RowList2.Clear;

      end;

  end;
end;

procedure TAvatarForm.AvatarViewImageSelect(Sender: TObject; idx: Integer);
begin
  case PageControl1.TabIndex of
    1:
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
        Player.ShowHair := False;
        Player.DressCap := False;

        for var i := 0 to High(Explode) - 1 do
          AvatarForm.AddEqps(Explode[i]);
        Inventory.SortByColumn(0);

        Player.RemoveSprites;
        for var i := 0 to Inventory.RowCount - 1 do
          Player.Spawn(Inventory.Cells[0, i]);

        ResetColorGrid;
        ActiveControl := nil;
      end;
    4:
      begin
        var ImageName := ExtractFileName(AvatarView.ImageFileName[idx]);
        TPlayerEx.Spawn(ImageName);
        ActiveControl := nil;
      end;
  end;
end;

procedure TAvatarForm.Button1Click(Sender: TObject);
begin
  AddEqps(SearchEqpID);
  Inventory.SortByColumn(0);
  Player.RemoveSprites;
  for var i := 0 to Inventory.RowCount - 1 do
    Player.Spawn(Inventory.Cells[0, i]);

  ResetColorGrid;

end;

procedure TAvatarForm.SaveAllButtonClick(Sender: TObject);
begin
  SaveAllFrames := True;
  SaveSingleFrame := False;
  Frame := 0;
end;

procedure TAvatarForm.SaveSingleButtonClick(Sender: TObject);
begin
  SaveSingleFrame := True;
  SaveAllFrames := False;
  var WX := Round(Player.X - SpriteEngine.WorldX - 155) + TrackBarX.Position;
  var WY := Round(Player.Y - SpriteEngine.WorldY - 160) + TrackBarY.Position;
  var Index := AllFrameListBox.ItemIndex;
  ForceDirectories(ExtractFilePath(ParamStr(0)) + 'Export');
  var FileName := ExtractFilePath(ParamStr(0)) + 'Export\' + AllFrameListBox.Items[Index] + '.png';
  Label2.Caption := 'Save to:  ' + FileName;
  AvatarPanelTexture.SaveToFile(FileName, nil, 0, IntRectBDS(WX, WY, WX + TrackBarW.Position, WY + TrackBarH.Position));
end;

procedure TAvatarForm.ResetColorGrid;
begin
  ColorGrid.Clear;
  for var Row := 0 to Inventory.RowCount do
  begin
    if Inventory.CellTypes[1, Row - 1] = ctPicture then
    begin

      for var Col := 0 to 18 do
      begin
        ColorGrid.CreateBitmap(Col, Row, False, haLeft, vaCenter).Assign(Inventory.CellGraphics[1, Row - 1].CellBitmap);
        case Col of
          0..10:
            TColorFunc.HSVvar(ColorGrid.CellGraphics[Col, Row].CellBitmap, Col * 30, 0, 0);
          11:
            TColorFunc.HSVvar(ColorGrid.CellGraphics[Col, Row].CellBitmap, 0, 25, 0);
          12:
            TColorFunc.HSVvar(ColorGrid.CellGraphics[Col, Row].CellBitmap, 0, -100, 0);
          13:
            TColorFunc.Contrast3(ColorGrid.CellGraphics[Col, Row].CellBitmap, 50, -90, True, False, False);
          14:
            TColorFunc.Contrast3(ColorGrid.CellGraphics[Col, Row].CellBitmap, 50, -90, False, True, False);
          15:
            TColorFunc.Contrast3(ColorGrid.CellGraphics[Col, Row].CellBitmap, 50, -90, False, False, True);
          16:
            TColorFunc.Contrast3(ColorGrid.CellGraphics[Col, Row].CellBitmap, 50, -90, True, True, False);
          17:
            TColorFunc.Contrast3(ColorGrid.CellGraphics[Col, Row].CellBitmap, 50, -90, True, False, True);
          18:
            TColorFunc.Negative(ColorGrid.CellGraphics[Col, Row].CellBitmap);
        end;

      end;

    end;
  end;

end;

procedure TAvatarForm.ColorGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  var Top := ColorGrid.CellRect(ACol, ARow).Location.Y;
  var ID := Inventory.Cells[0, ARow - 1];

  if Length(ID) > 2 then
  begin
    var Dir := GetDir(ID);
    var Entry :TWZImgEntry;

    if TItemEffect.AllList.Contains(ID)  then
       Entry :=  GetImgEntry('Effect/ItemEff.img/' + IDToInt(ID))
    else
       Entry := GetImgFile('Character/' + Dir + ID + '.img').Root;

    case ACol of
      0:
        DumpData(Entry, EquipData, EquipImages, ceHue, 0);
      1:
        DumpData(Entry, EquipData, EquipImages, ceHue, 30);
      2:
        DumpData(Entry, EquipData, EquipImages, ceHue, 60);
      3:
        DumpData(Entry, EquipData, EquipImages, ceHue, 90);
      4:
        DumpData(Entry, EquipData, EquipImages, ceHue, 120);
      5:
        DumpData(Entry, EquipData, EquipImages, ceHue, 150);
      6:
        DumpData(Entry, EquipData, EquipImages, ceHue, 180);
      7:
        DumpData(Entry, EquipData, EquipImages, ceHue, 210);
      8:
        DumpData(Entry, EquipData, EquipImages, ceHue, 240);
      9:
        DumpData(Entry, EquipData, EquipImages, ceHue, 270);
      10:
        DumpData(Entry, EquipData, EquipImages, ceHue, 300);
      11:
        DumpData(Entry, EquipData, EquipImages, ceSaturation, 25);
      12:
        DumpData(Entry, EquipData, EquipImages, ceSaturation, -100);
      13:
        DumpData(Entry, EquipData, EquipImages, ceContrast1, 0);
      14:
        DumpData(Entry, EquipData, EquipImages, ceContrast2, 0);
      15:
        DumpData(Entry, EquipData, EquipImages, ceContrast3, 0);
      16:
        DumpData(Entry, EquipData, EquipImages, ceContrast4, 0);
      17:
        DumpData(Entry, EquipData, EquipImages, ceContrast5, 0);
      18:
        DumpData(Entry, EquipData, EquipImages, ceNegative, 0);
    end;
  end;

end;

procedure TAvatarForm.SaveButtonClick(Sender: TObject);
begin
  var ImageName: string;
  for var i := 0 to Inventory.RowCount - 1 do
    ImageName := ImageName + Inventory.Cells[0, i] + '-';
  ForceDirectories(ExtractFilePath(ParamStr(0)) + 'Images');
  var FileName := ExtractFilePath(ParamStr(0)) + 'Images\' + ImageName + '.png';
  var WX := Round(Player.X - SpriteEngine.WorldX - 55);
  var WY := Round(Player.Y - SpriteEngine.WorldY - 90);
  AvatarPanelTexture.SaveToFile(FileName, nil, 0, IntRectBDS(WX, WY, WX + 100, WY + 100));
end;

procedure TAvatarForm.SearchGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin

  var EqpID := SearchGrid.Cells[1, ARow];
  IDLabel.Caption := EqpID;
  NameLabel.Caption := SearchGrid.Cells[2, ARow];
  var Dir := GetDir(EqpID);
  var Part := GetPart(EqpID);
  var Entry:=GetImgEntry('Character/'+Dir+ EqpID + '.img/');
  var Bmp: TBitmap;
  case Part of
    Face:
      begin
        Bmp := Entry.Get2('default/face').Canvas.DumpBmp;
      end;
    Hair:
      begin
        if Entry.Get('default/hairOverHead') <> nil then
          Bmp := Entry.Get2('default/hairOverHead').Canvas.DumpBmp
        else
          Bmp := Entry.Get2('default/hair').Canvas.DumpBmp

      end;
  else
    Bmp := Entry.Get2('info/icon').Canvas.DumpBmp;

  end;

  Image1.Picture.Bitmap.Assign(Bmp);
  Bmp.Free;
  SearchEqpID := EqpID;
  ActiveControl := nil;
end;

procedure TAvatarForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  AvatarForm.SaveSingleFrame := False;
  AvatarForm.SaveAllFrames := False;
end;

procedure TAvatarForm.FormCreate(Sender: TObject);
begin
  IconList := TObjectDictionary<string, TObjectList<TBmpEx>>.Create([doOwnsValues]);
  WzListObj:=TObjectList<TWZArchive>.Create;
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
  HasLoaded := TList<Integer>.Create;
  RowList2 := TDictionary<Integer, string>.Create;
  ColList2 := TStringList.Create;

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
    ImageGrids[i].ThumbnailsBackground := clBtnface;
    ImageGrids[i].ThumbWidth := 35;
    ImageGrids[i].ThumbHeight := 35;
    ImageGrids[i].ThumbnailOptionsEx := [ietxShowIconForUnknownFormat, ietxShowIconWhileLoading, ietxEnableInternalIcons];
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
  AvatarView.ThumbnailsBackground := clwhite;
  AvatarView.ThumbnailsBorderColor := RGB(200, 200, 200);
  AvatarView.ThumbnailsBorderWidth := 1;
  AvatarView.ThumbsRounded := 100;
  AvatarView.SelectionColor := clRed;
  AvatarView.OnImageSelect := AvatarViewImageSelect;
  {
  Inventory.ColumnHeaders.Add('ID');
  Inventory.ColumnHeaders.Add('Icon');
  Inventory.ColumnHeaders.Add('Name');
  Inventory.ColumnHeaders.Add('Del');
  Inventory.RowHeights[0] := 22;
  }
  Inventory.ColWidths[0] := 75;
  Inventory.ColWidths[1] := 50;  //icon
  Inventory.ColWidths[2] := 140;
  Inventory.ColWidths[3] := 32;

  DumpRowCount := -1;
end;

procedure TAvatarForm.FormDestroy(Sender: TObject);
begin
  IconList.Free;
 // if Wz <> nil then
  //  Wz.Free;
  RowList2.Free;
  ColList2.Free;
  HasLoaded.Free;
   WzListObj.Free;
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
    var Name := GetImgFile('String/Eqp.img').Root.Get('Eqp/' + Dir + TrimID + '/name', '');

    var Entry := GetImgFile('Character/' + Dir + DefaultEqps[i] + '.img').Root;

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
    AddInventory(DefaultEqps[i], Bmp, Name, i);
    Bmp.Free;
  end;
  Inventory.SortByColumn(0);
  ResetColorGrid;
  AllFrames := ['walk1.0', 'walk1.0', 'walk1.1', 'walk1.2', 'walk1.3', 'walk2.0', 'walk2.1', 'walk2.2', 'walk2.3', 'stand1.0', 'stand1.1', 'stand1.2', 'stand2.0', 'stand2.1', 'stand2.2', 'alert.0', 'alert.1', 'alert.2', 'swingO1.0', 'swingO1.1', 'swingO1.2', 'swingO2.0', 'swingO2.1', 'swingO2.2', 'swingO3.0', 'swingO3.1', 'swingO3.2', 'swingOF.0', 'swingOF.1', 'swingOF.2', 'swingOF.3', 'swingT1.0', 'swingT1.1', 'swingT1.2', 'swingT2.0', 'swingT2.1', 'swingT2.2', 'swingT3.0', 'swingT3.1', 'swingT3.2',
    'swingTF.0', 'swingTF.1', 'swingTF.2', 'swingTF.3', 'swingP1.0', 'swingP1.1', 'swingP1.2', 'swingP2.0', 'swingP2.1', 'swingP2.2', 'swingPF.0', 'swingPF.1', 'swingPF.2', 'swingPF.3', 'stabO1.0', 'stabO1.1', 'stabO2.0', 'stabO2.1', 'stabOF.0', 'stabOF.1', 'stabOF.2', 'stabT1.0', 'stabT1.1', 'stabT1.2', 'stabT2.0', 'stabT2.1', 'stabT2.2', 'stabTF.0', 'stabTF.1', 'stabTF.2', 'stabTF.3', 'shoot1.0', 'shoot1.1', 'shoot1.2', 'shoot2.0', 'shoot2.1', 'shoot2.2', 'shoot2.3', 'shoot2.4', 'shootF.0', 'shootF.1', 'shootF.2', 'proneStab.0', 'proneStab.1', 'prone.0', 'heal.0', 'heal.1', 'heal.2', 'fly.0', 'fly.1', 'jump.0', 'sit.0', 'ladder.0', 'ladder.1', 'rope.0', 'rope.1', 'rope.1'];
  for var i in AllFrames do
    AllFrameListBox.Items.Add(i);
  AllFrameListBox.Items.Delete(0);
  AllFrameListBox.Items.Delete(AllFrameListBox.Count - 1);
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
  SaveAllFrames := False;
  SaveSingleFrame := False;
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

  if is64Bit then
    if not IconList.ContainsKey(Part) then
    begin
     // if Wz <> nil then
     //   Wz.Free;
      var List := TObjectList<TBmpEx>.Create;
      for var Files in WzList2 do
      begin
        if Files.Value = 'Character/' + CharacterDir then
        begin
          Wz := TWzArchive.Create(Files.Key);
          WZListObj.Add(Wz);
          for img in WZ.Root.Files do
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

            Name := GetImgFile('String/Eqp.img').Root.Get('Eqp/' + CharacterDir + '/' + IDToInt(ID) + '/name', '');
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
                if not InRange(30, 56) then
                  Continue;
              14: // Hair2
                if not InRange(57, 85) then
                  Continue;
              15: // Face1
                if not InRange(20, 23) then
                  Continue;
              16: // Face2
                if not InRange(24, 58) then
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
           // listbox1.Items.Add(img.Name);
        end;
      end;
      IconList.Add(Part, List);
    end;

  if not IS64Bit then
    if not IconList.ContainsKey(Part) then
    begin
      //if Wz <> nil then
       // Wz.Free;
      Wz := TWZArchive.Create(WzPath + '\Character.wz');
      WZListObj.Add(Wz);
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

        Name := GetImgFile('String/Eqp.img').Root.Get('Eqp/' + CharacterDir + '/' + IDToInt(ID) + '/name', '');
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
            if not InRange(37, 61) then
              Continue;
          15: // Face1
            if not InRange(20, 23) then
              Continue;
          16: // Face2
            if not InRange(24, 51) then
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
    ImageGrids[PartIndex].LockUpdate;
    for var Iter in IconList[Part] do
    begin
      ImageGrids[PartIndex].AppendImage(Iter);
      Inc(Index);
      ImageGrids[PartIndex].ImageInfoText[Index] := Iter.ID;
    end;
    ImageGrids[PartIndex].UnlockUpdate;
    ImageGrids[PartIndex].ViewX := 0;
    ImageGrids[PartIndex].ViewY := 0;
  end;

  ActiveControl := nil;
end;

procedure TAvatarForm.TabSheet4Show(Sender: TObject);
begin
  if HasShowSearchGrid then
    Exit;
  HasShowSearchGrid := True;
  SearchGrid.Canvas.FillRect(Rect(0, 0, 500, 800));
  SearchGrid.Canvas.Font.Size := 18;
  SearchGrid.Canvas.TextOut(60, 160, 'Loading...');
  AvatarForm.searchGrid.BeginUpdate;
  DumpEqpString(GetImgFile('String/Eqp.img').Root.Child['Eqp']);
  AvatarForm.searchGrid.EndUpdate;
end;

procedure TAvatarForm.TrackBarWChange(Sender: TObject);
begin
  case TTrackBar(Sender).Tag of
    0:
      LabelW.Caption := TrackBarW.Position.ToString;
    1:
      LabelH.Caption := TrackBarH.Position.ToString;
    2:
      LabelX.Caption := TrackBarX.Position.ToString;
    3:
      LabelY.Caption := TrackBarY.Position.ToString;
  end;
end;

procedure TAvatarForm.DumpEqpString(Entry: TWZIMGEntry);
var
  E: TWZIMGEntry;
begin
  if Entry.Name = 'name' then
  begin
    Inc(DumpRowCount);
    SearchGrid.RowCount := DumpRowCount + 1;
    if Length(Entry.Parent.Name) = 5 then
    begin
      SearchGrid.Cells[1, DumpRowCount] := '000' + Entry.Parent.Name;
      SearchGrid.Cells[2, DumpRowCount] := Entry.Data
    end
    else
    begin
      SearchGrid.Cells[1, DumpRowCount] := '0' + Entry.Parent.Name;
      SearchGrid.Cells[2, DumpRowCount] := Entry.Data;
    end;
  end;

  for E in Entry.Children do
    if (Entry.Name <> 'Android') and (Entry.Name <> 'ArcaneForce') and (Entry.Name <> 'Bits') and (Entry.Name <> 'Dragon') and (Entry.Name <> 'Mechanic') and (Entry.Name <> 'PetEquip') and (Entry.Name <> 'Skillskin') and (Entry.Name <> 'Taming') and (Entry.Name <> 'MonsterBattle') and (LeftStr(Entry.Name, 3) <> '135') and (LeftStr(Entry.Name, 3) <> '150') and (LeftStr(Entry.Name, 3) <> '151') and (LeftStr(Entry.Name, 3) <> '160') and (LeftStr(Entry.Name, 3) <> '169') and (LeftStr(Entry.Name, 3) <> '111') and (LeftStr(Entry.Name, 3) <> '114') then
      DumpEqpString(E);

end;

procedure TAvatarForm.Edit1Change(Sender: TObject);
begin
  SearchGrid.NarrowDown(Trim(Edit1.Text));
end;

procedure TAvatarForm.ExpressionListBoxChange(Sender: TObject);
begin
  ChangeExpressionListBox := True;
end;

end.

