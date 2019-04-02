unit DamageSkinFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, WZIMGFile, WZArchive,
  WzUtils, Vcl.Grids, AdvObj, BaseGrid, AdvGrid, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TDamageSkinForm = class(TForm)
    DamageGrid: TAdvStringGrid;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormShow(Sender: TObject);
    procedure DamageGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DamageGridClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DamageSkinForm: TDamageSkinForm;

implementation
   uses DamageNumber;
{$R *.dfm}

procedure TDamageSkinForm.DamageGridClick(Sender: TObject);
begin
   ActiveControl:= nil;
end;

procedure TDamageSkinForm.DamageGridClickCell(Sender: TObject; ARow,
  ACol: Integer);
begin
  TDamageNumber.UseNewDamage:=True;
  var DamageStyle:=DamageGrid.Cells[1, ARow];

  //style=1/Red1
  TDamageNumber.Style:=DamageStyle;
  TDamageNumber.Load(DamageStyle);
  Label1.Caption:=DamageStyle;
  Image1.Picture.Assign(DamageGrid.CellGraphics[2,ARow].CellBitmap);
  ActiveControl:= nil;
end;

procedure TDamageSkinForm.FormClick(Sender: TObject);
begin
 ActiveControl := nil;
end;

procedure TDamageSkinForm.FormCreate(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TDamageSkinForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   if Key = VK_MENU then
    Key := 0;
end;

procedure TDamageSkinForm.FormShow(Sender: TObject);
begin
   if DamageGrid.Cells[1, 1] <> '' then
    Exit;
  var Entry := GetImgEntry('Effect.wz/BasicEff.img/damageSkin');
  if Entry=nil then
  begin
    MessageDlg('Old WZ not supported', mtinformation, [mbOk], 0);
    Exit;
  end;


  var Rowcount := -1;

  for var Iter in Entry.Children do
    for var Iter2 in Iter.Children do
      if (Iter2.Name = 'NoCri1') or (Iter2.Name = 'NoRed1') then
      begin
        Inc(Rowcount);
        DamageGrid.RowCount := Rowcount + 1;
        DamageGrid.Cells[1, Rowcount] := Iter.Name + '/' + Iter2.Name;

        if Iter2.Child['5']<> nil then
        begin
          var Bmp:=Iter2.Get2('5').Canvas.DumpBmp;
          DamageGrid.CreateBitmap(2, RowCount, False, haCenter, vaCenter).Assign(Bmp);
          Bmp.Free;
        end;
      end;

end;

end.

