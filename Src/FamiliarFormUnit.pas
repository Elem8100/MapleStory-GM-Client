unit FamiliarFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, AdvObj, BaseGrid, AdvGrid, Vcl.StdCtrls,
  Vcl.ComCtrls;

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

  if HasImgFile('Mob.wz/' + FamiliarID + '.img') then
    Entry := GetImgEntry('Mob.wz/' + FamiliarID + '.img/')
  else if HasImgFile('Mob001.wz/' + FamiliarID + '.img') then
    Entry := GetImgEntry('Mob001.wz/' + FamiliarID + '.img/')
  else
    Entry := GetImgEntry('Mob2.wz/' + FamiliarID + '.img/');
  if Entry <> nil then
    TColorFunc.SetSpriteColor<TWZIMGEntry>(Entry, ARow, True);
end;

procedure TFamiliarForm.Edit1Change(Sender: TObject);
begin
   FamiliarGrid.NarrowDown(TrimS(Edit1.Text));
end;

procedure TFamiliarForm.FamiliarGridClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TFamiliarForm.FamiliarGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  var ID := FamiliarGrid.Cells[1, ARow];

  if IsGMS then
    FamiliarID := GetImgEntry('Etc.wz/FamiliarInfo.img/' + ID + '/mob').Data
  else
    FamiliarID := GetImgEntry('Character.wz/Familiar/' + ID + '.img/' + 'info/MobID').Data;
  TMonsterFamiliar.Delete;
  FamiliarID := Add7(FamiliarID);
  TMonsterFamiliar.Create(FamiliarID);

  TFamiliarNameTag.Delete;
  TFamiliarNameTag.Create('01112146');
  TFamiliarNameTag.FamiliarNameTag.MedalName := StringWZ.GetImgFile('Mob.img').Root.Get(FamiliarID + '/name', '');
  TFamiliarNameTag.FamiliarNameTag.InitData;
  TFamiliarNameTag.ReDraw;
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

  if HasImgFile('Etc.wz/FamiliarInfo.img') then
    IsGMS := True
  else
    IsGMS := False;

  var RowCount := -1;
  FamiliarGrid.BeginUpdate;
  for var img in TWZDirectory(CharacterWZ.Root.Entry['Familiar']).Files do
  begin

    var ID := NoIMG(img.Name);
    Inc(RowCount);
    FamiliarGrid.RowCount := RowCount + 1;
    FamiliarGrid.Cells[1, RowCount] := ID;

    var CardID: string;
    if IsGMS then
      CardID := GetImgEntry('Etc.wz/FamiliarInfo.img/' + ID + '/consume').Data
    else
      CardID := GetImgEntry('Character.wz/Familiar/' + img.Name + '/info/monsterCardID').Data;

    if HasImgEntry('String.wz/Consume.img/' + CardID) then
      FamiliarGrid.Cells[3, RowCount] := GetImgEntry('String.wz/Consume.img/' + CardID).Get('Name', '');

    var Entry := GetImgEntry('Item.wz/Consume/0287.img/' + '0' + CardID + '/info/icon', True);
    if Entry <> nil then
    begin
      var Bmp := Entry.Canvas.DumpBmp;
      FamiliarGrid.CreateBitmap(2, RowCount, False, haCenter, vaCenter).Assign(Bmp);
      Bmp.Free;
    end;

  end;
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

