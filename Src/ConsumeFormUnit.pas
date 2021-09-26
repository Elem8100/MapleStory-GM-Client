unit ConsumeFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, CurvyControls, Vcl.Grids, AdvObj, BaseGrid,
  AdvGrid, Vcl.ComCtrls, ieview, iemview, PNGMapleCanvasEx, Generics.Collections,
  Generics.Defaults, WZArchive, AdvUtil;

type
  TConsumeForm = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    CurvyPanel1: TCurvyPanel;
    IDLabel: TLabel;
    NameLabel: TLabel;
    Image1: TImage;
    Button1: TButton;
    ConsumeGrid: TAdvStringGrid;
    Edit1: TEdit;
    TabSheet3: TTabSheet;
    ConsumeEffectGrid: TAdvStringGrid;
    Button2: TButton;
    procedure FormActivate(Sender: TObject);
    procedure ConsumeGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure TabSheet2Show(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ConsumeGridClick(Sender: TObject);
    procedure TabSheet3Show(Sender: TObject);
    procedure ConsumeEffectGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure Button2Click(Sender: TObject);
  private
    HasLoad: Boolean;
    HasShowImageGrid: Boolean;
    HasShowEffectGrid: Boolean;
    ImageGrid: TImageEnMView;
    IconList: TObjectList<TBmpEx>;
    Wz: TWZArchive;
    procedure ImageGridSelect(Sender: TObject; idx: Integer);
    { Private declarations }
  public
    procedure ImageAssignIcon(AID, DirName: string; AIDLabel, ANameLabel: TLabel; AImage: TImage; IsEtc: Boolean = False);
    procedure CreateImageGrid(var AImageGrid: TImageEnMView; AOwner: TComponent; AParent: TWinControl);
    procedure DumpIcons(AImageGrid: TImageEnMView; DirName: string; var AWZ: TWZArchive; var AIconList: TObjectList<TBmpEx>);
    { Public declarations }
  end;

var
  ConsumeForm: TConsumeForm;

implementation

uses
  WzUtils, Global, StrUtils, WZDirectory, MobDrop, MapleCharacter, MapleEffect;
{$R *.dfm}

procedure TConsumeForm.CreateImageGrid(var AImageGrid: TImageEnMView; AOwner: TComponent; AParent: TWinControl);
begin
  AImageGrid := TImageEnMView.Create(AOwner);
  AImageGrid.Parent := AParent;
  AImageGrid.Visible := True;
  AImageGrid.Align := alClient;
  AImageGrid.AlignWithMargins := True;
  AImageGrid.Margins.Left := 3;
  AImageGrid.Margins.Right := 3;
  AImageGrid.Margins.Top := 3;
  AImageGrid.Margins.Bottom := 3;
  AImageGrid.BorderStyle := bsNone;
  AImageGrid.Background := clWhite;
  AImageGrid.ThumbWidth := 35;
  AImageGrid.ThumbHeight := 35;
  AImageGrid.ThumbnailOptionsEx := [ietxShowIconForUnknownFormat, ietxShowIconWhileLoading, ietxEnableInternalIcons];
  AImageGrid.DefaultInfoText := iedtNone;
  AImageGrid.MultiSelectionOptions := [];
  AImageGrid.ShowText := False;
  AImageGrid.SelectionColor := clRed;
end;

procedure TConsumeForm.ImageAssignIcon(AID, DirName: string; AIDLabel, ANameLabel: TLabel; Aimage: TImage; IsEtc: Boolean = False);
begin

  var Left4 := LeftStr(AID, 4);
  if GetImgEntry('Item/' + DirName + '/' + Left4 + '.img/' + AID + '/info/icon') <> nil then
  begin
    var PNG := GetImgEntry('Item/' + DirName + '/' + Left4 + '.img/' + AID + '/info/icon', True).Canvas.DumpPNG;
    Aimage.Picture.Assign(PNG);
    PNG.Free;
  end;
  AIDLabel.Caption := AID;
  if IsEtc then
    ANameLabel.Caption := GetImgFile('String/' + DirName + '.img').Root.Get('Etc/' + IDToInt(AID) + '/name', '')
  else
    ANameLabel.Caption := GetImgFile('String/' + DirName + '.img').Root.Get(IDToInt(AID) + '/name', '');
end;

procedure TConsumeForm.Button1Click(Sender: TObject);
begin
  if Trim(IDLabel.Caption) <> '' then
    TMobDrop.Drop(Round(Player.X), Round(Player.Y), 0, Trim(IDLabel.Caption));
  ActiveControl := nil;
end;

procedure TConsumeForm.Button2Click(Sender: TObject);
begin
  TItemEffect.Delete(Consume);
  ActiveControl := nil;
end;

procedure TConsumeForm.ConsumeEffectGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  var ID := ConsumeEffectGrid.Cells[1, ARow];
  TItemEffect.Delete(Consume);
  TItemEffect.Create(ID, Consume);
  ActiveControl := nil;
end;

procedure TConsumeForm.ConsumeGridClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TConsumeForm.ConsumeGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  ImageAssignIcon(ConsumeGrid.Cells[1, ARow], 'Consume', IDLabel, NameLabel, Image1);
  ActiveControl := nil;
end;

procedure TConsumeForm.Edit1Change(Sender: TObject);
begin
  ConsumeGrid.NarrowDown(Trim(Edit1.Text));
  ActiveControl := nil;
end;

procedure TConsumeForm.DumpIcons(AImageGrid: TImageEnMView; DirName: string; var AWZ: TWZArchive; var AIconList: TObjectList<TBmpEx>);
begin
  with AImageGrid.GetCanvas do
  begin
    Font.Size := 24;
    TextOut(100, 100, 'Loading...')
  end;

  AIconList := TObjectList<TBmpEx>.Create;
  if Is64Bit then
    for var Files in ItemWzList do
    begin
      if Files.Value = 'Item/' + DirName then
      begin
        var WZ := TWZArchive.Create(Files.Key);
        ItemWZListA.Add(WZ);
        for var img in WZ.Root.Files do
        begin
          if not IsNumber(img.Name[1]) then
            Continue;
          with WZ.ParseFile(img) do
          begin
            for var Iter in Root.Children do
            begin
              if not IsNumber(Iter.Name) then
                Continue;
              if Iter.Get('info/icon') <> nil then
              begin
                var Bmp := Iter.Get2('info/icon').Canvas.DumpBmpEx;
                Bmp.ID := Iter.Name;
          //Bmp.Name := Name;
                AIconList.Add(Bmp);
              end;
            end;
            Free;
          end;
        end;
      end;
    end;

  if not is64Bit then
  begin
    AWZ := TWZArchive.Create(WzPath + '\Item.wz');
    var Dir := TWZDirectory(AWZ.Root.Entry[DirName]);
    for var img in Dir.Files do
    begin
      if not IsNumber(img.Name[1]) then
        Continue;
      with AWZ.ParseFile(img) do
      begin
        for var Iter in Root.Children do
        begin
          if not IsNumber(Iter.Name) then
            Continue;
          if Iter.Get('info/icon') <> nil then
          begin
            var Bmp := Iter.Get2('info/icon').Canvas.DumpBmpEx;
            Bmp.ID := Iter.Name;
          //Bmp.Name := Name;
            AIconList.Add(Bmp);
          end;
        end;
        Free;
      end;
    end;
  end;

  AIconList.Sort(TComparer<TBmpEx>.Construct(
    function(const Left, Right: TBmpEx): Integer
    begin
      Result := Left.ID.ToInteger - Right.ID.ToInteger;
    end));
  var Index := -1;
  AImageGrid.LockUpdate;
  for var Iter in AIconList do
  begin
    AImageGrid.AppendImage(Iter);
    Inc(Index);
    AImageGrid.ImageInfoText[Index] := Iter.ID;
  end;
  AImageGrid.UnlockUpdate;
  AImageGrid.ViewX := 0;
  AImageGrid.ViewY := 0;
end;

procedure TConsumeForm.FormActivate(Sender: TObject);
begin
  ActiveControl := nil;
  Edit1.Clear;
  if HasLoad then
    Exit;
  HasLoad := True;
  DumpIcons(ImageGrid, 'Consume', WZ, IconList);
end;

procedure TConsumeForm.FormClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TConsumeForm.FormCreate(Sender: TObject);
begin
  CreateImageGrid(ImageGrid, ConsumeForm, PageControl1.Pages[0]);
  ImageGrid.OnImageSelect := ImageGridSelect;
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TConsumeForm.ImageGridSelect(Sender: TObject; idx: Integer);
begin
  ImageAssignIcon(ImageGrid.ImageInfoText[idx], 'Consume', IDlabel, NameLabel, Image1);
  ActiveControl := nil;
end;

procedure TConsumeForm.TabSheet2Show(Sender: TObject);
begin
  if HasShowImageGrid then
    Exit;
  HasShowImageGrid := True;

  ConsumeGrid.Canvas.Font.Size := 18;
  ConsumeGrid.Canvas.TextOut(60, 0, 'Loading...');

  var RowCount := -1;
  ConsumeGrid.BeginUpdate;
  for var Iter in GetImgFile('String/Consume.img').Root.Children do
  begin
    Inc(RowCount);
    ConsumeGrid.RowCount := RowCount + 1;
    ConsumeGrid.Cells[1, RowCount] := LeftPad(Iter.Name.ToInteger);
    ConsumeGrid.Cells[2, RowCount] := Iter.Get('name', '');
  end;
  ConsumeGrid.SortByColumn(1);
  ConsumeGrid.EndUpdate;
end;

procedure TConsumeForm.TabSheet3Show(Sender: TObject);
begin
  if HasShowEffectGrid then
    Exit;
  HasShowEffectGrid := True;
  ConsumeEffectGrid.Canvas.Font.Size := 18;
  ConsumeEffectGrid.Canvas.TextOut(60, 0, 'Loading...');

  var RowCount := -1;
  ConsumeEffectGrid.BeginUpdate;
  for var Iter in GetImgFile('Effect/ItemEff.img').Root.Children do
  begin
    if LeftStr(Iter.Name, 1) = '2' then
    begin
      Inc(RowCount);
      ConsumeEffectGrid.RowCount := RowCount + 1;
      var ID := '0' + Iter.Name;
      ConsumeEffectGrid.Cells[1, RowCount] := ID;
      var Left4 := LeftStr(ID, 4);

      if GetImgEntry('Item/Consume/' + Left4 + '.img/' + ID + '/info/icon') <> nil then
      begin
        var Bmp := GetImgEntry('Item/Consume/' + Left4 + '.img/' + ID + '/info/icon', True).Canvas.DumpBmp;
        ConsumeEffectGrid.CreateBitmap(2, RowCount, False, haCenter, vaCenter).Assign(Bmp);
        Bmp.Free;
      end;
      ConsumeEffectGrid.Cells[3, RowCount] := GetImgFile('String/Consume.img').Root.Get(IDToInt(ID) + '/name', '');
    end;
  end;
  ConsumeEffectGrid.SortByColumn(1);
  ConsumeEffectGrid.EndUpdate;
end;

procedure TConsumeForm.FormDestroy(Sender: TObject);
begin
  if WZ <> nil then
    WZ.Free;
  IconList.Free;
end;

procedure TConsumeForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_MENU then
    Key := 0;
end;

end.

