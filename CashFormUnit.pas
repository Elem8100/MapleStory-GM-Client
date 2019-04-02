unit CashFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, AdvObj,
  BaseGrid, AdvGrid, Vcl.StdCtrls;

type
  TCashForm = class(TForm)
    CashGrid: TAdvStringGrid;
    Button1: TButton;
    procedure FormShow(Sender: TObject);
    procedure CashGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Button1Click(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure CashGridClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CashForm: TCashForm;

implementation

uses
  WzUtils, MapleEffect, Global;
{$R *.dfm}

function IDToInt(ID: string): string;
begin
  var S := ID.ToInteger;
  Result := S.ToString;
end;

procedure TCashForm.Button1Click(Sender: TObject);
begin
  TItemEffect.Delete(Cash);
  ActiveControl := nil;
end;

procedure TCashForm.CashGridClick(Sender: TObject);
begin
 ActiveControl := nil;
end;

procedure TCashForm.CashGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  var ID := CashGrid.Cells[1, ARow];
  TItemEffect.Delete(Cash);

  TItemEffect.Create(ID);

  ActiveControl := nil;
end;

procedure TCashForm.FormClick(Sender: TObject);
begin
   ActiveControl := nil;
end;

procedure TCashForm.FormCreate(Sender: TObject);
begin
   Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TCashForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_MENU then
    Key := 0;
end;

procedure TCashForm.FormShow(Sender: TObject);
begin
  if CashGrid.Cells[1, 1] <> '' then
    Exit;
  var Entry := GetImgEntry('Item.wz/Cash/0501.img/');
  var RowCount := -1;

  for var Iter in Entry.Children do
  begin
    if Iter.Name = '05010044' then
      Continue;
    if Iter.Name = '05012000' then
      Continue;
    if Iter.Name = '05012001' then
      Continue;
    if Iter.Name = '05010099' then
      Continue;

    Inc(RowCount);
    CashGrid.RowCount := RowCount + 1;
    CashGrid.Cells[1, RowCount] := Iter.Name;
    if HasImgEntry('String.wz/Cash.img/' + IDToInt(Iter.Name)) then
      CashGrid.Cells[3, RowCount] := GetImgEntry('String.wz/Cash.img/' + IDToInt(Iter.Name)).Get('Name', '');
    if Iter.Get('info/icon') <> nil then
    begin
      var Bmp := Iter.Get2('info/icon').Canvas.DumpBmp;
      CashGrid.CreateBitmap(2, RowCount, False, haCenter, vaCenter).Assign(Bmp);
      Bmp.Free;
    end;

  end;

end;

end.

