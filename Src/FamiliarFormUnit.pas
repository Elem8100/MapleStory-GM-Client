unit FamiliarFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, AdvObj, BaseGrid, AdvGrid, Vcl.StdCtrls,
  Vcl.ComCtrls, AdvUtil;

type
  TFamiliarForm = class(TForm)
    Button1: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    FamiliarGrid: TAdvStringGrid;
    DyeGrid: TAdvStringGrid;
    Edit1: TEdit;
    Label1: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure FamiliarGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FamiliarGridClick(Sender: TObject);
    procedure DyeGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure Edit1Change(Sender: TObject);
  private
    HasLoad: Boolean;
    IsGMS: Boolean;
    FamiliarID: string;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FamiliarForm: TFamiliarForm;

implementation

uses
  WzUtils, WZIMGFile, WZDirectory, Global, MonsterFamiliar, ColorUtils;
{$R *.dfm}

procedure TFamiliarForm.Button1Click(Sender: TObject);
begin
  TFamiliarNameTag.Delete;
  TMonsterFamiliar.Delete;
  ActiveControl := nil;
end;

procedure TFamiliarForm.DyeGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  var Entry: TWZIMGEntry;

  if HasImgFile('Mob/' + FamiliarID + '.img') then
    Entry := GetImgEntry('Mob/' + FamiliarID + '.img/');
  if Entry <> nil then
    TColorFunc.SetSpriteColor<TWZIMGEntry>(Entry, ARow, True);
end;

procedure TFamiliarForm.Edit1Change(Sender: TObject);
begin
  FamiliarGrid.NarrowDown(Trim(Edit1.Text));
end;

procedure TFamiliarForm.FamiliarGridClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TFamiliarForm.FamiliarGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  var ID := FamiliarGrid.Cells[1, ARow];

  if IsGMS then
    FamiliarID := GetImgEntry('Etc/FamiliarInfo.img/' + ID + '/mob').Data
  else
    FamiliarID := GetImgEntry('Character/Familiar/' + ID + '.img/' + 'info/MobID').Data;
  TMonsterFamiliar.Delete;
  FamiliarID := Add7(FamiliarID);
  TMonsterFamiliar.Create(FamiliarID);

  TFamiliarNameTag.Delete;
  TFamiliarNameTag.Create('01112146');
  TFamiliarNameTag.FamiliarNameTag.MedalName := GetImgFile('String/Mob.img').Root.Get(FamiliarID +
    '/name', '');
  TFamiliarNameTag.FamiliarNameTag.InitData;
  TFamiliarNameTag.ReDraw;
  if  FamiliarGrid.CellTypes[2, ARow] = ctBitmap   then
  TColorFunc.SetGridColor(FamiliarGrid.CellGraphics[2, ARow].CellBitmap, DyeGrid);
  ActiveControl := nil;
end;

procedure TFamiliarForm.FormActivate(Sender: TObject);
begin
  if HasLoad then
    Exit;
  HasLoad := True;
  FamiliarGrid.Canvas.Font.Size := 18;
  FamiliarGrid.Canvas.TextOut(60, 0, 'Loading...');

  if HasImgFile('Etc/FamiliarInfo.img') then
    IsGMS := True
  else
    IsGMS := False;

  var RowCount := -1;
  FamiliarGrid.BeginUpdate;
  var ImgList:=GetImgList('Character/Familiar');

  for var img in ImgList do
  begin
    var ID := NoIMG(img.Name);
    var CardID: string;
    if IsGMS then
    begin
      if GetImgEntry('Etc/FamiliarInfo.img/' + ID) <> nil then
      begin
        Inc(RowCount);
        FamiliarGrid.RowCount := RowCount + 1;
        FamiliarGrid.Cells[1, RowCount] := ID;
        CardID := GetImgEntry('Etc/FamiliarInfo.img/' + ID + '/consume').Data
      end
      else
        Continue;
    end
    else
    begin
      Inc(RowCount);
      FamiliarGrid.RowCount := RowCount + 1;
      FamiliarGrid.Cells[1, RowCount] := ID;
      if GetImgEntry('Character/Familiar/' + img.Name + '/info/monsterCardID') <> nil then
        CardID := GetImgEntry('Character/Familiar/' + img.Name + '/info/monsterCardID').Data;
    end;

    if HasImgEntry('String/Consume.img/' + CardID) then
      FamiliarGrid.Cells[3, RowCount] := GetImgEntry('String/Consume.img/' + CardID).Get('Name',
        '');

    if GetImgEntry('Item/Consume/0287.img/' + '0' + CardID + '/info/icon') <> nil then
    begin
      var Entry := GetImgEntry('Item/Consume/0287.img/' + '0' + CardID + '/info/icon', True);
      var Bmp := Entry.Canvas.DumpBmp;
      FamiliarGrid.CreateBitmap(2, RowCount, False, haCenter, vaCenter).Assign(Bmp);
      Bmp.Free;
    end
    else if GetImgEntry('Item/Consume/0238.img/' + '0' + CardID + '/info/iconRaw') <> nil then
    begin
      var Entry := GetImgEntry('Item/Consume/0238.img/' + '0' + CardID + '/info/iconRaw', True);
      var Bmp := Entry.Canvas.DumpBmp;
      FamiliarGrid.CreateBitmap(2, RowCount, False, haCenter, vaCenter).Assign(Bmp);
      Bmp.Free;
    end;
  end;
  ImgList.Free;
  FamiliarGrid.SortByColumn(1);
  FamiliarGrid.EndUpdate;
end;

procedure TFamiliarForm.FormClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TFamiliarForm.FormCreate(Sender: TObject);
begin
  Left := ((Screen.Width - Width) div 2) + 400;
  Top := (Screen.Height - Height) div 2;
end;

procedure TFamiliarForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_MENU then
    Key := 0;
end;

end.

