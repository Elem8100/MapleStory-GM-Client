unit MobFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, AdvUtil, Vcl.Grids, AdvObj,
  BaseGrid, AdvGrid, WZIMGFile, WZArchive, Math, MapleMap, WzUtils, Vcl.ComCtrls, Strutils;

type
  TAddMobForm = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Image1: TImage;
    Button2: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Boss1: TTabSheet;
    MobGrid: TAdvStringGrid;
    Edit2: TEdit;
    Label4: TLabel;
    DumpBossIDButton: TButton;
    Boss1Grid: TAdvStringGrid;
    Boss2: TTabSheet;
    Boss2Grid: TAdvStringGrid;
    TabSheet2: TTabSheet;
    DyeGrid: TAdvStringGrid;
    TabSheet3: TTabSheet;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure MobGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure Button1Click(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClick(Sender: TObject);
    procedure MobGridClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure DumpBossIDButtonClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure Boss1GridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure Boss2GridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure DyeGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure FormDestroy(Sender: TObject);
  private
    MobID: string;
    HasLoad: Boolean;
    HasLoadBoss1: Boolean;
    HasLoadBoss2: Boolean;
    SelectRow: Integer;
    SpriteID:string;
    MobName:string;
    RowList: TStringList;
    procedure Dump2(Entry: TWZIMGEntry);
    procedure SelectMob;
    procedure SetMobColor(Row: Integer);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AddMobForm: TAddMobForm;

implementation

uses
  Mob2, MapleCharacter, MobInfo, Global, ColorUtils, WZDirectory;
{$R *.dfm}



procedure TAddMobForm.Dump2(Entry: TWZIMGEntry);
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
        RowList.Add(Entry.GetPathD + '=' + Data);
      end;
  end;
  for E in Entry.Children do
    if Entry.DataType <> mdtCanvas then
      Dump2(E);
end;

procedure TAddMobForm.MobGridClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TAddMobForm.SelectMob;
var
  Bmp: TBitmap;
  Entry: TWZIMGEntry;

begin
  Image1.Picture := nil;

  if GetImgFile('Mob/'+MobID + '.img') = nil then
    Exit;

  Entry := GetImgEntry('Mob/'+ MobID + '.img/info/link');
  if Entry <> nil then
     SpriteID := Entry.Data
  else
    SpriteID := MobID;

  if GetImgEntry('Mob/' + SpriteID + '.img/stand/0') <> nil then
    Bmp := GetImgEntry('Mob/' + SpriteID + '.img/stand/0', True).Canvas.DumpBmp
  else if GetImgEntry('Mob/' + SpriteID + '.img/fly/0') <> nil then
    Bmp := GetImgEntry('Mob/' + SpriteID + '.img/fly/0', True).Canvas.DumpBmp
  else
    Exit;

  Image1.Picture.Assign(Bmp);
  Bmp.Free;

  ActiveControl := nil;
end;

procedure TAddMobForm.MobGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  SelectRow := 0;
  //NpcSelectRow:=Arow;
  MobID := MobGrid.Cells[1, ARow];
  MobName:=  MobGrid.Cells[2, ARow];
  SelectMob;
  TColorFunc.SetGridColor(Image1.Picture.Bitmap, DyeGrid);

end;

procedure TAddMobForm.PageControl1Change(Sender: TObject);
begin

  if PageControl1.TabIndex = 2 then
  begin
    Memo1.Clear;
    RowList.Clear;
    RowList.Add('ID='+SpriteID);
    RowList.Add('Name='+MobName);
    Dump2(GetImgEntry('Mob/' + SpriteID + '.img/'));
    var S := GetImgEntry('Mob/' + SpriteID + '.img/').GetPathD + '.';
    for var i := 0 to Rowlist.Count - 1 do
    begin
      Rowlist[i] := StringReplace(RowList[i], S, '', [rfReplaceAll]);
      Memo1.Lines.Add(Rowlist.Strings[i]);
    end;
  end;

  Exit;
  case PageControl1.TabIndex of
    1:
      begin
        if HasLoadBoss1 then
          Exit;
        HasLoadBoss1 := True;
        Boss1Grid.LoadFromCSV(ExtractFilePath(ParamStr(0)) + 'Boss1.txt');
      end;
    2:
      begin
        if HasLoadBoss2 then
          Exit;
        HasLoadBoss2 := True;
        Boss2Grid.LoadFromCSV(ExtractFilePath(ParamStr(0)) + 'Boss2.txt');
      end;
  end;
end;

procedure TAddMobForm.Boss1GridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  MobID := Boss1Grid.Cells[1, ARow];
  SelectMob;
end;

procedure TAddMobForm.Boss2GridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  MobID := Boss2Grid.Cells[1, ARow];
  SelectMob;
end;

procedure TAddMobForm.Button1Click(Sender: TObject);
var
  I: Integer;
  Range: Integer;
begin

  Randomize;

  if GetImgFile('Mob/'+MobID + '.img') = nil then
    Exit;
  if (IsNumber(Edit1.Text)) and (Edit1.Text <> '') then
    for I := 0 to StrToInt(Edit1.Text) - 1 do
    begin
      Range := RandomRange(Round(Player.X - 100), Round(Player.X + 100));
      if (Range > TMap.Left) and (Range < TMap.Right) then
      begin
        TMob.Drop(MobID, Range, Round(Player.Y) - 100, TMap.Left, TMap.Right);
        TMob.SummonedList.Add(MobID);
      end;
    end;
  TMobInfo.ReDrawTarget;
  SetMobColor(SelectRow);
  ActiveControl := nil;
end;

procedure TAddMobForm.Button2Click(Sender: TObject);
begin
  for var Iter in SpriteEngine.SpriteList do
    if Iter is TMob then
    begin
      for var I := 0 to TMob.SummonedList.Count - 1 do
      begin
        if TMob(Iter).InfoID = TMob.SummonedList[I] then
        begin
          TMob(Iter).Dead;

          TMob.MobList.Remove(TMob.SummonedList[I]);

        end;
      end;
    end;
  TMobInfo.ReDrawTarget;
  ActiveControl := nil;
end;

procedure DumpBossID(Wz: TWZArchive);
begin
  var Row: Integer;
  var ID: string;
  AddMobForm.boss1Grid.BeginUpdate;
  for var img in Wz.Root.Files do
  begin
    with Wz.ParseFile(img) do
    begin

      for var Iter in Root.Child['info'].Children do
      begin

        if (Iter.Name = 'boss') then
          for var Iter2 in Root.Child['info'].Children do
          begin
            if (Iter2.Name = 'maxHP') and (Length(Iter2.data) > 9) then
            begin
              AddMobForm.boss2grid.RowCount := AddMobForm.boss2grid.RowCount + 1;
              Row := AddMobForm.boss2grid.RowCount - 1;
              ID := NoIMG(Iter.Parent.parent.Name);
              AddMobForm.Boss2grid.Cells[1, Row] := ID;
              AddMobForm.Boss2grid.Cells[2, Row] := StringWZ.GetImgFile('Mob.img').Root.Get(IDToInt(ID)
                + '/name', '');
            end
            else if (Iter2.Name = 'maxHP') and (Length(Iter2.data) <= 9) then
            begin
              AddMobForm.boss1grid.RowCount := AddMobForm.boss1grid.RowCount + 1;
              Row := AddMobForm.boss1grid.RowCount - 1;
              ID := NoIMG(Iter.Parent.parent.Name);
              AddMobForm.Boss1Grid.Cells[1, Row] := ID;
              AddMobForm.Boss1Grid.Cells[2, Row] := StringWZ.GetImgFile('Mob.img').Root.Get(IDToInt(ID)
                + '/name', '');
            end;
          end;

      end;
     // Free;
    end;
  end;
  AddMobForm.boss1grid.EndUpdate;
end;

procedure TAddMobForm.DumpBossIDButtonClick(Sender: TObject);
begin
  DumpBossID(MobWZ);
  DumpBossID(Mob001WZ);
  DumpBossID(Mob2WZ);
  Boss1Grid.SortByColumn(1);
  Boss2Grid.SortByColumn(1);
  Boss1Grid.SaveToCSV(ExtractFilePath(ParamStr(0)) + 'Boss1.txt');
  Boss2Grid.SaveToCSV(ExtractFilePath(ParamStr(0)) + 'Boss2.txt');
end;

procedure TAddMobForm.SetMobColor(Row: Integer);
var
  Entry: TWZIMGEntry;
begin

  TColorFunc.SetSpriteColor<TWZIMGEntry>(GetImgFile('Mob/'+MobID + '.img').Root, Row);
  Entry := GetImgEntry('Mob/' + MobID + '.img/info/link');
  if Entry <> nil then
  begin
    var EntryID: string := Entry.Data;
    TColorFunc.SetSpriteColor<TWZIMGEntry>(GetImgFile('Mob/'+Entry.Data + '.img').Root, Row);
  end;
end;

procedure TAddMobForm.DyeGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  SelectRow := ARow;
  SetMobColor(ARow);
  ActiveControl := nil;
end;

procedure TAddMobForm.Edit2Change(Sender: TObject);
begin
  MobGrid.NarrowDown(Trim(Edit2.Text));
end;

procedure TAddMobForm.FormActivate(Sender: TObject);
begin
  ActiveControl := nil;
  Edit2.Clear;
  RowList := TStringList.Create;
  if HasLoad then
    Exit;
  HasLoad := True;
  MobGrid.Canvas.Font.Size := 18;
  MobGrid.Canvas.TextOut(60, 0, 'Loading...');

  var RowCount := -1;
  MobGrid.BeginUpdate;
  for var Iter in GetImgFile('String/Mob.img').Root.Children do
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

procedure TAddMobForm.FormDestroy(Sender: TObject);
begin
  RowList.Free;
end;

procedure TAddMobForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_MENU then
    Key := 0;
end;

end.

