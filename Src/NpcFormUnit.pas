unit NpcFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Grids, AdvObj, BaseGrid, AdvGrid,
  Vcl.StdCtrls, WZIMGFile, WZArchive, WzUtils, Math, AdvUtil, Vcl.ComCtrls;

type
  TAddNpcForm = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    Button1: TButton;
    Edit2: TEdit;
    Label4: TLabel;
    Button2: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    NpcGrid: TAdvStringGrid;
    DyeGrid: TAdvStringGrid;
    procedure NpcGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure NpcGridClick(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure DyeGridClickCell(Sender: TObject; ARow, ACol: Integer);
  private
    NpcID: string;
    WZ: TWZArchive;
    HasLoad: Boolean;
    SelectRow: Integer;
    NpcSelectRow:Integer;
     { Private declarations }
  public
    { Public declarations }
  end;

var
  AddNpcForm: TAddNpcForm;

implementation

uses
  Npc, Global, MapleMap, MapleCharacter, ColorUtils;
{$R *.dfm}

procedure TAddNpcForm.NpcGridClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TAddNpcForm.NpcGridClickCell(Sender: TObject; ARow, ACol: Integer);
var
  Bmp: TBitmap;
  Entry: TWZIMGEntry;
  Path, SpriteID: string;
begin
  SelectRow := 0;
  NpcSelectRow:=Arow;
  NpcID := NpcGrid.Cells[1, ARow];

  Image1.Picture := nil;
  if NPCWZ.GetImgFile(NpcID + '.img') <> nil then
  begin
    Path := 'Npc/';
    WZ := NPCWZ;
  end
  else
  begin
    Path := 'Npc2/';
    //WZ := Npc2Wz;
  end;
  if WZ.GetImgFile(NpcID + '.img') = nil then
    Exit;

  Entry := GetImgEntry(Path + NpcID + '.img/info/link');
  if Entry <> nil then
    SpriteID := Entry.Data
  else
    SpriteID := NpcID;

  if GetImgEntry(Path + SpriteID + '.img/stand/0') <> nil then
    Bmp := GetImgEntry(Path + SpriteID + '.img/stand/0', True).Canvas.DumpBmp
  else if GetImgEntry(Path + SpriteID + '.img/fly/0') <> nil then
    Bmp := GetImgEntry(Path + SpriteID + '.img/fly/0', True).Canvas.DumpBmp
  else
    Exit;

  Image1.Picture.Assign(Bmp);
  Bmp.Free;

  TColorFunc.SetGridColor(Image1.Picture.Bitmap, DyeGrid);

  ActiveControl := nil;
end;

procedure TAddNpcForm.Button1Click(Sender: TObject);
var
  Range: Integer;
  RandomFlip: Integer;
begin

  if NpcGrid.Cells[2, NpcSelectRow]='' then
    Exit;

  Randomize;
  RandomFlip := Random(2);
  if WZ.GetImgFile(NpcID + '.img') = nil then
    Exit;
  Range := RandomRange(Round(Player.X - 100), Round(Player.X + 100));
  if (Range > TMap.Left) and (Range < TMap.Right) then
  begin
    TNpc.Drop(NpcID, Range, Round(Player.Y) - 100, RandomFlip);
    TNpc.SummonedList.Add(NpcID);
  end;
  TNpc.ReDrawTarget := True;

  var NpcEntry := GetImgEntry('Npc.wz/' + NpcID + '.img/');
  TColorFunc.SetSpriteColor<TWZIMGEntry>(NpcEntry, SelectRow);
  var Entry := GetImgEntry('Npc.wz/' + NpcID + '.img/info/link');
  if Entry <> nil then
    TColorFunc.SetSpriteColor<TWZIMGEntry>(Entry, SelectRow);
  ActiveControl := nil;
end;

procedure TAddNpcForm.Button2Click(Sender: TObject);
begin
  for var Iter in SpriteEngine.SpriteList do
    if Iter is TNpc then
    begin
      for var I := 0 to TNpc.SummonedList.Count - 1 do
      begin
        if TNpc(Iter).LocalID = TNpc.SummonedList[I] then
        begin
          TNpc(Iter).Dead;

        end;
      end;
    end;
  TNpc.ReDrawTarget := True;
  ActiveControl := nil;
end;

procedure TAddNpcForm.DyeGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin

  SelectRow := ARow;
  var NpcEntry := GetImgEntry('Npc.wz/' + NpcID + '.img/');
  TColorFunc.SetSpriteColor(NpcEntry, ARow);
  var Entry := GetImgEntry('Npc.wz/' + NpcID + '.img/info/link');
  if Entry <> nil then
    TColorFunc.SetSpriteColor<TWZIMGEntry>(Entry, ARow);
  ActiveControl := nil;
end;

procedure TAddNpcForm.Edit2Change(Sender: TObject);
begin
  NpcGrid.NarrowDown(TrimS(Edit2.Text));

end;

procedure TAddNpcForm.FormActivate(Sender: TObject);
begin
  ActiveControl := nil;
  Edit2.Clear;
  if HasLoad then
    Exit;
  HasLoad := True;
  NpcGrid.Canvas.Font.Size := 18;
  NpcGrid.Canvas.TextOut(60, 0, 'Loading...');

  var RowCount := -1;
  NpcGrid.BeginUpdate;
  for var Iter in StringWZ.GetImgFile('Npc.img').Root.Children do
  begin
    Inc(RowCount);
    NpcGrid.RowCount := RowCount + 1;
    NpcGrid.Cells[1, RowCount] := Add7(Iter.Name);
    NpcGrid.Cells[2, RowCount] := Iter.Get('name', '');
  end;
  NpcGrid.SortByColumn(1);
  NpcGrid.EndUpdate;
end;

procedure TAddNpcForm.FormClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TAddNpcForm.FormCreate(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TAddNpcForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_MENU then
    Key := 0;
end;

end.

