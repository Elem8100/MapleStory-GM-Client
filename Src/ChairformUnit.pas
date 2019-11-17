unit ChairformUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, AdvObj, BaseGrid, AdvGrid, Generics.Collections,
  StrUtils, Vcl.StdCtrls, AdvUtil;

type
  TChairForm = class(TForm)
    ChairGrid: TAdvStringGrid;
    procedure ChairGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure ChairGridClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    HasLoad: Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ChairForm: TChairForm;

implementation

{$R *.dfm}

uses
  MapleChair, Global, MapleEffect, MapleCharacter, WZIMGFile, WZDirectory, WzUtils, TamingMob, Morph;

function IDToInt(ID: string): string;
begin
  var S := ID.ToInteger;
  Result := S.ToString;
end;

procedure TChairForm.ChairGridClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TChairForm.ChairGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  if TMorph.IsUse then
    Exit;
  var ChairID := ChairGrid.Cells[1, ARow];
  TMapleChair.Delete;
  TTamingMob.Delete;
  TItemEffect.Delete(Chair);
  TMapleChair.Create(ChairID);

  if TItemEffect.AllList.contains(ChairID) then
    TItemEffect.Create(ChairID, True);
  TMapleChair.IsUse := True;

  ActiveControl := nil;
end;

procedure TChairForm.FormActivate(Sender: TObject);
type
  TRec = record
    Desc, Name: string;
  end;
var
  Rec: TRec;
begin
  if HasLoad then
    Exit;
  HasLoad := True;
  ChairGrid.Canvas.Font.Size := 18;
  ChairGrid.Canvas.TextOut(60, 0, 'Loading...');

  var Dict := TDictionary<string, TRec>.Create;

  for var Iter in StringWZ.GetImgFile('Ins.img').Root.Children do
    if LeftStr(Iter.Name, 3) = '301' then
    begin

      Rec.Desc := Iter.Get('desc', '');
      Rec.Name := Iter.Get('name', '');
      Dict.Add(Iter.Name, Rec);
    end;

  var Row := -1;

  ChairGrid.BeginUpdate;
  var Entry: TWZIMGEntry;

  for var img in TWZDirectory(ItemWZ.Root.Entry['Install']).Files do
  begin
    if LeftStr(img.Name, 4) <> '0301' then
      Continue;

    for var Iter in ItemWZ.GetImgFile('Install/' + img.Name).Root.Children do
    begin
      if Iter.Name = '03018051' then
        Continue;
      Inc(Row);
      ChairGrid.RowCount := Row + 1;
      ChairGrid.Cells[1, Row] := Iter.Name;
      if Dict.ContainsKey(IDToInt(Iter.Name)) then
      begin
        ChairGrid.Cells[3, Row] := Dict[IDToInt(Iter.Name)].Name;
     // ChairGrid.Cells[4, Row] := Dict[IDToInt(Iter.Name)].Desc;
      end;
      if Iter.Get('info/icon') <> nil then
      begin
        var Bmp := Iter.Get2('info/icon').Canvas.DumpBmp;
        ChairGrid.CreateBitmap(2, Row, False, haCenter, vaCenter).Assign(Bmp);
        Bmp.Free;
      end;
    end;

  end;

  ChairGrid.SortByColumn(1);
  ChairGrid.EndUpdate;

  Dict.Free;

end;

procedure TChairForm.FormClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TChairForm.FormCreate(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TChairForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_MENU then
    Key := 0;
end;

end.

