unit MorphFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, AdvObj,
  BaseGrid, AdvGrid, Generics.Collections, Vcl.StdCtrls, AdvUtil;

type
  TMorphForm = class(TForm)
    MorphGrid: TAdvStringGrid;
    Button1: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MorphGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure Button1Click(Sender: TObject);
    procedure MorphGridClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MorphForm: TMorphForm;

implementation

{$R *.dfm}

uses
  Morph,WZIMGFile, WZDirectory, WzUtils, Global, StrUtils,MapleChair,TamingMob;

function Add4(Name: string): string;
begin
  case Length(Name) of
    0:
      Result := '';
    1:
      Result := '000' + Name;
    2:
      Result := '00' + Name;
    3:
      Result := '0' + Name;
    4:
      Result := Name;
  end;
end;

procedure TMorphForm.Button1Click(Sender: TObject);
begin
  TMorph.Delete;
  TMorph.IsUse := False;
  ActiveControl := nil;
end;

procedure TMorphForm.FormClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TMorphForm.FormCreate(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TMorphForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_MENU then
    Key := 0;
end;

procedure TMorphForm.FormShow(Sender: TObject);
type
  TRec = record
    Desc, Name: string;
  end;
var
  Rec: TRec;
  Dict: TDictionary<string, TRec>;
  imgs: TList<string>;
  Name, Desc, Path, ItemDir, InfoData, MorphID: string;
  Iter, Iter2, Iter3, Child, Source: TWZIMGEntry;
  Row, i: Integer;
  Bmp, Bmp2: TBitmap;
  img: TWZFile;
begin
  if MorphGrid.Cells[1, 1] <> '' then
    Exit;
  Row := -1;
  Dict := TDictionary<string, TRec>.Create;
  imgs := TList<string>.Create;

  for Iter in StringWZ.GetImgFile('Consume.img').Root.Children do
  begin
    Rec.Desc := Iter.Get('desc', '');
    Rec.Name := Iter.Get('name', '');
    Dict.Add(Iter.Name, Rec);
  end;

  for img in MorphWz.Root.Files do
    imgs.Add(LeftStr(img.Name, 4));

  for Iter in ItemWZ.GetImgFile('Consume/0221.img').Root.Children do
  begin
    Inc(Row);
    MorphGrid.RowCount := Row + 1;
    MorphGrid.Cells[1, Row] := (Iter.Name);
    if Dict.ContainsKey(IDToInt(Iter.Name)) then
    begin
      MorphGrid.Cells[3, Row] := Dict[IDToInt(Iter.Name)].Name;
    //  MorphGrid.Cells[6, Row] := Dict[IDToInt(Iter.Name)].Desc;
    end;

    Bmp := Iter.Get2('info/icon').Canvas.DumpBmp;
    MorphGrid.CreateBitmap(2, Row, False, haCenter, vaCenter).Assign(Bmp);
    Bmp.Free;

    MorphID := Add4(Iter.Get('spec/morph', ''));

    if imgs.contains(MorphID) then
    begin
      MorphGrid.Cells[4, Row] := MorphID + '.img';
      Bmp := MorphWz.GetImgFile(MorphID + '.img').Root.Get2('walk/0').Canvas.DumpBmp;
      if (Bmp.Height > 100) then
      begin
        MorphGrid.RowHeights[Row] := 100;
        MorphGrid.CreatePicture(5, Row, False, StretchWithAspectRatio, 1, haCenter, vaCenter).Assign(Bmp);
      end
      else
      begin
        MorphGrid.RowHeights[Row] := Bmp.Height;
        MorphGrid.CreateBitmap(5, Row, False, haCenter, vaCenter).Assign(Bmp);
      end;
      Bmp.Free;
    end;

  end;
  Dict.Free;
  imgs.Free;
end;

procedure TMorphForm.MorphGridClick(Sender: TObject);
begin
    ActiveControl := nil;
end;

procedure TMorphForm.MorphGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  if (TMapleChair.IsUse) or (TTamingMob.IsUse) then
    Exit;
  var MorphNum := MorphGrid.Cells[4, ARow];
  if MorphNum='' then
    Exit;

  TMorph.Delete;
  TMorph.Create(MorphNum);
  TMorph.IsUse := True;
  ActiveControl := nil;
end;

end.

