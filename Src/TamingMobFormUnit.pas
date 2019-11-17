unit TamingMobFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, AdvObj, BaseGrid, AdvGrid, StrUtils, Vcl.StdCtrls,
  AdvUtil;

type
  TTamingMobForm = class(TForm)
    TamingMobGrid: TAdvStringGrid;
    Button1: TButton;
    procedure TamingMobGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure Button1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClick(Sender: TObject);
    procedure TamingMobGridClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    HasLoad: Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TamingMobForm: TTamingMobForm;

implementation

{$R *.dfm}
uses
  TamingMob, MapleChair, Morph, MapleEffect, Global, MapleCharacter, WZIMGFile, WZDirectory, WzUtils;

function NoIMG(const Name: string): string; inline;
begin
  Result := ChangeFileExt(Name, '');
end;

procedure TTamingMobForm.Button1Click(Sender: TObject);
begin
  TTamingMob.Delete;
  TItemEffect.Delete(Chair);
  TMapleChair.Delete;

  ChangeState := True;
  if WeaponWalkType.contains('stand2') then
    NewState := 'stand2'
  else
    NewState := 'stand1';
  ActiveControl := nil;
end;

procedure TTamingMobForm.FormActivate(Sender: TObject);
begin
  if HasLoad then
    Exit;
  HasLoad := True;
  TamingMobGrid.Canvas.Font.Size := 18;
  TamingMobGrid.Canvas.TextOut(60, 0, 'Loading...');

  var RowCount := -1;
  TamingMobGrid.BeginUpdate;
  for var img in TWZDirectory(CharacterWZ.Root.Entry['TamingMob']).Files do
  begin
    if LeftStr(img.Name, 4) = '0191' then
      Continue;
    if LeftStr(img.Name, 4) = '0198' then
      Continue;

    var ID := NoIMG(img.Name);
    Inc(RowCount);
    TamingMobGrid.RowCount := RowCount + 1;
    TamingMobGrid.Cells[1, RowCount] := ID;
    if HasImgEntry('String.wz/Eqp.img/Eqp/Taming/' + IDToInt(ID)) then
      TamingMobGrid.Cells[3, RowCount] := GetImgEntry('String.wz/Eqp.img/Eqp/Taming/' + IDToInt(ID)).Get('Name', '');

    var Entry := GetImgEntry('Character.WZ/TamingMob/' + img.Name + '/info/icon', True);
    if Entry <> nil then
    begin
      var Bmp := Entry.Canvas.DumpBmp;
      TamingMobGrid.CreateBitmap(2, RowCount, False, haCenter, vaCenter).Assign(Bmp);
      Bmp.Free;
    end;

  end;
  TamingMobGrid.SortByColumn(1);
  TamingMobGrid.EndUpdate;
end;

procedure TTamingMobForm.FormClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TTamingMobForm.FormCreate(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TTamingMobForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_MENU then
    Key := 0;
end;

procedure TTamingMobForm.TamingMobGridClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TTamingMobForm.TamingMobGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  if TMorph.IsUse then
    Exit;
  var ID := TamingMobGrid.Cells[1, ARow];
  TTamingMob.Delete;

  TMapleChair.Delete;
  TItemEffect.Delete(Chair);

  TTamingMob.IsChairTaming := False;
  TTamingMob.Create(ID);
  TTamingMob.IsUse := True;

  ActiveControl := nil;
end;

end.

