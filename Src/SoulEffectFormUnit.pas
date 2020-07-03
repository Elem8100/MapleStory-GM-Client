unit SoulEffectFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids,
  AdvObj, BaseGrid, AdvGrid, Vcl.StdCtrls;

type
  TSoulEffectForm = class(TForm)
    SoulEffectGrid: TAdvStringGrid;
    Button1: TButton;
    procedure FormActivate(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SoulEffectGridClick(Sender: TObject);
    procedure SoulEffectGridClickCell(Sender: TObject; ARow, ACol: Integer);
  private
    HasLoad: Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SoulEffectForm: TSoulEffectForm;

implementation

uses
  WZUtils, Global, StrUtils, WZDirectory, MapleEffect;
{$R *.dfm}

procedure TSoulEffectForm.Button1Click(Sender: TObject);
begin
  TItemEffect.Delete(Soul);
  ActiveControl := nil;
end;

procedure TSoulEffectForm.FormActivate(Sender: TObject);
begin
  if HasLoad then
    Exit;
  HasLoad := True;
  SoulEffectGrid.Canvas.Font.Size := 18;
  SoulEffectGrid.Canvas.TextOut(60, 0, 'Loading...');

  var RowCount := -1;
  SoulEffectGrid.BeginUpdate;
  for var Iter in EtcWZ.GetImgFile('SoulCollection.img').Root.Children do
  begin
    Inc(RowCount);
    SoulEffectGrid.RowCount := RowCount + 1;
    var ID: string := Iter.Get('soulList/0/0').Data;
    SoulEffectGrid.Cells[1, RowCount] := '0' + ID;
    SoulEffectGrid.Cells[4, RowCount] := Iter.Get('soulSkill').Data;
    if GetImgEntry('Item.wz/Consume/0259.img/' + '0' + ID + '/info/icon') <> nil then
    begin
      var Bmp := GetImgEntry('Item.wz/Consume/0259.img/' + '0' + ID + '/info/icon', True).Canvas.DumpBmp;
      SoulEffectGrid.CreateBitmap(2, RowCount, False, haCenter, vaCenter).Assign(Bmp);
      Bmp.Free;
      SoulEffectGrid.Cells[3, RowCount] := StringWZ.GetImgFile('Consume.img').Root.Get(ID + '/name', '');
    end;
  end;
  SoulEffectGrid.SortByColumn(1);
  SoulEffectGrid.EndUpdate;
end;

procedure TSoulEffectForm.FormClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TSoulEffectForm.FormCreate(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TSoulEffectForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_MENU then
    Key := 0;
end;

procedure TSoulEffectForm.SoulEffectGridClick(Sender: TObject);
begin
   ActiveControl := nil;
end;

procedure TSoulEffectForm.SoulEffectGridClickCell(Sender: TObject; ARow,
  ACol: Integer);
begin
  var ID := SoulEffectGrid.Cells[4, ARow];
  TItemEffect.Delete(Soul);
  TItemEffect.Create(ID, Soul);
  ActiveControl := nil;
end;

end.

