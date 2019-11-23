unit PetFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, AdvObj,
  BaseGrid, AdvGrid, AdvUtil;

type
  TPetForm = class(TForm)
    PetGrid: TAdvStringGrid;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure PetGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure PetGridClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormClick(Sender: TObject);
  private
    HasLoad: Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PetForm: TPetForm;

implementation
 uses
  Pet,WzUtils, WZIMGFile,  WZDirectory, Global;
{$R *.dfm}

procedure TPetForm.Button1Click(Sender: TObject);
begin
  TPet.Delete;
  TPetNameTag.Delete;
end;

procedure TPetForm.FormActivate(Sender: TObject);
begin
   if HasLoad then
    Exit;
  HasLoad := True;
  PetGrid.Canvas.Font.Size := 18;
  PetGrid.Canvas.TextOut(60, 0, 'Loading...');

  var RowCount := -1;
  PetGrid.BeginUpdate;
  for var img in TWZDirectory(ItemWZ.Root.Entry['Pet']).Files do
  begin

    var ID := NoIMG(img.Name);
    Inc(RowCount);
    PetGrid.RowCount := RowCount + 1;
    PetGrid.Cells[1, RowCount] := ID;
    if HasImgEntry('String.wz/Pet.img/' + ID) then
      PetGrid.Cells[3, RowCount] := GetImgEntry('String.wz/Pet.img/' + ID).Get('Name', '');

    var Entry := GetImgEntry('Item.wz/Pet/' + img.Name + '/info/iconD', True);
    if Entry <> nil then
    begin
      var Bmp := Entry.Canvas.DumpBmp;
      PetGrid.CreateBitmap(2, RowCount, False, haCenter, vaCenter).Assign(Bmp);
      Bmp.Free;
    end;

  end;
  PetGrid.SortByColumn(1);
  PetGrid.EndUpdate;
end;

procedure TPetForm.FormClick(Sender: TObject);
begin
   ActiveControl := nil;
end;

procedure TPetForm.FormCreate(Sender: TObject);
begin
  Left := ((Screen.Width - Width) div 2)+400;
  Top := (Screen.Height - Height) div 2;
end;

procedure TPetForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_MENU then
    Key := 0;
end;

procedure TPetForm.PetGridClick(Sender: TObject);
begin
   ActiveControl := nil;
end;

procedure TPetForm.PetGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  var ID := PetGrid.Cells[1, ARow];
  TPetNameTag.Delete;
  TPet.Delete;
  TPet.Create(ID);

  TPetNameTag.Create('01142008');
  TPetNameTag.PetNameTag.MedalName:=PetGrid.Cells[3, ARow];
  TPetNameTag.PetNameTag.InitData;
  TPetNameTag.ReDraw;
  ActiveControl := nil;

end;

end.
