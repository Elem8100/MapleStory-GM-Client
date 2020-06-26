unit TotemEffectFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids,
  AdvObj, BaseGrid, AdvGrid, Vcl.StdCtrls;

type
  TTotemEffectForm = class(TForm)
    TotemEffectGrid: TAdvStringGrid;
    Button1: TButton;
    procedure FormActivate(Sender: TObject);
    procedure TotemEffectGridClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure TotemEffectGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    HasLoad: Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TotemEffectForm: TTotemEffectForm;

implementation

uses
  WZUtils, Global, StrUtils, WZDirectory, MapleEffect;
{$R *.dfm}

procedure TTotemEffectForm.Button1Click(Sender: TObject);
begin
  TItemEffect.Delete(Totem);
  ActiveControl := nil;
end;

procedure TTotemEffectForm.FormActivate(Sender: TObject);
begin
  if HasLoad then
    Exit;
  HasLoad := True;
  TotemEffectGrid.Canvas.Font.Size := 18;
  TotemEffectGrid.Canvas.TextOut(60, 0, 'Loading...');

  var RowCount := -1;
  TotemEffectGrid.BeginUpdate;
  for var Iter in EffectWZ.GetImgFile('ItemEff.img').Root.Children do
  begin
    if LeftStr(Iter.Name, 2) = '12' then
    begin
      Inc(RowCount);
      TotemEffectGrid.RowCount := RowCount + 1;
      var ID := '0' + Iter.Name;
      TotemEffectGrid.Cells[1, RowCount] := ID;
      if GetImgEntry('Character.wz/Totem/' + ID + '.img' + '/info/icon') <> nil then
      begin
        var Bmp := GetImgEntry('Character.wz/Totem/' + ID + '.img' + '/info/icon', True).Canvas.DumpBmp;
        TotemEffectGrid.CreateBitmap(2, RowCount, False, haCenter, vaCenter).Assign(Bmp);
        Bmp.Free;
      end;
      TotemEffectGrid.Cells[3, RowCount] := StringWZ.GetImgFile('Eqp.img').Root.Get('Eqp/Accessory/' + IDToInt(ID) + '/name', '');
    end;
  end;
  TotemEffectGrid.SortByColumn(1);
  TotemEffectGrid.EndUpdate;
end;

procedure TTotemEffectForm.FormClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TTotemEffectForm.FormCreate(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TTotemEffectForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_MENU then
    Key := 0;
end;

procedure TTotemEffectForm.TotemEffectGridClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TTotemEffectForm.TotemEffectGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  var ID := TotemEffectGrid.Cells[1, ARow];
  TItemEffect.Delete(Totem);
  TItemEffect.Create(ID, True);
  ActiveControl := nil;
end;

end.

