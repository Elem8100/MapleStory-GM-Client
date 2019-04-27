unit AddMobUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  AdvUtil, Vcl.Grids, AdvObj, BaseGrid, AdvGrid, WZIMGFile, WZArchive, Math,
  MapleMap, WzUtils;

type
  TAddMobForm = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Edit2: TEdit;
    MobGrid: TAdvStringGrid;
    Image1: TImage;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure MobGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure Button1Click(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClick(Sender: TObject);
    procedure MobGridClick(Sender: TObject);
  private
    MobID: string;
    WZ: TWZArchive;
    HasLoad: Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AddMobForm: TAddMobForm;

implementation

uses
  MainUnit, Mob2, MapleCharacter, MobInfo, Global;
{$R *.dfm}

procedure TAddMobForm.MobGridClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TAddMobForm.MobGridClickCell(Sender: TObject; ARow, ACol: Integer);
var
  Bmp: Tbitmap;
  Entry: TWZImgEntry;
  Path, SpriteID: string;
begin
  MobID := MobGrid.Cells[1, ARow];
  Label1.Caption := MobGrid.Cells[2, ARow];
  Image1.Picture := nil;
  if MobWz.GetImgFile(MobID + '.img') <> nil then
  begin
    Path := 'Mob/';
    WZ := MobWz;
  end
  else
  begin
    Path := 'Mob2/';
    WZ := Mob2Wz;
  end;
  if WZ.GetImgFile(MobID + '.img') = nil then
    Exit;

  Entry := GetImgEntry(Path + MobID + '.img/info/link');
  if Entry <> nil then
    SpriteID := Entry.Data
  else
    SpriteID := MobID;

  if GetImgEntry(Path + SpriteID + '.img/stand/0') <> nil then
    Bmp := GetImgEntry(Path + SpriteID + '.img/stand/0', True).Canvas.DumpBmp
  else if GetImgEntry(Path + SpriteID + '.img/fly/0') <> nil then
    Bmp := GetImgEntry(Path + SpriteID + '.img/fly/0', True).Canvas.DumpBmp
  else
    Exit;

  Image1.Picture.Assign(Bmp);
  Bmp.Free;
  ActiveControl := nil;
end;

procedure TAddMobForm.Button1Click(Sender: TObject);
var
  I: Integer;
  Range: Integer;
begin
  if Label1.Caption = '' then
    Exit;
  Randomize;

  if WZ.GetImgFile(MobID + '.img') = nil then
    Exit;
  if (IsNumber(Edit1.Text)) and (Edit1.Text <> '') then
    for I := 0 to StrToInt(Edit1.Text) - 1 do
    begin
      Range := RandomRange(Round(Player.X - 100), Round(Player.X + 100));
      if (Range > TMap.Left) and (Range < TMap.Right) then
        TMob.Drop(MobID, Range, Round(Player.Y) - 100, TMap.Left, TMap.Right);
    end;
  TMobInfo.ReDrawTarget;
  ActiveControl := nil;
end;

procedure TAddMobForm.Edit2Change(Sender: TObject);
begin
  MobGrid.NarrowDown(Edit2.Text);
  ActiveControl := nil;
end;

procedure TAddMobForm.FormActivate(Sender: TObject);
begin
  ActiveControl := nil;
  Edit2.Clear;

  if HasLoad then
    Exit;
  HasLoad := True;
  MobGrid.Canvas.Font.Size := 18;
  MobGrid.Canvas.TextOut(60, 0, 'Loading...');

  var RowCount := -1;
  MobGrid.BeginUpdate;
  for var Iter in StringWZ.GetImgFile('Mob.img').Root.Children do
  begin
    Inc(RowCount);
    MobGrid.RowCount := RowCount + 1;
    MobGrid.Cells[1, RowCount] := Add7(Iter.Name);
    MobGrid.Cells[2, RowCount] := Iter.Get('name', '');
  end;
  MobGrid.SortByColumn(1);
  MobGrid.EndUpdate;
end;

procedure TAddMobForm.FormClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TAddMobForm.FormCreate(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TAddMobForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_Menu then
    Key := 0;
end;

end.

