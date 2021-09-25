unit PetFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, AdvObj, BaseGrid, AdvGrid, Vcl.StdCtrls,
  Vcl.ComCtrls, AdvUtil;

type
  TPetForm = class(TForm)
    Button1: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    PetGrid: TAdvStringGrid;
    DyeGrid: TAdvStringGrid;
    Label1: TLabel;
    Edit2: TEdit;
    PetEquipGrid: TAdvStringGrid;
    Label2: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PetGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure PetGridClick(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Button1Click(Sender: TObject);
    procedure DyeGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure Edit2Change(Sender: TObject);
    procedure PetEquipGridClickCell(Sender: TObject; ARow, ACol: Integer);
  private
    HasLoad: Boolean;
    SelectRow: Integer;
    PetSelectRow: Integer;
    { Private declarations }
  public
    PetID: string;
    { Public declarations }
  end;

var
  PetForm: TPetForm;

implementation

uses
  Pet, WzUtils, WZIMGFile, WZDirectory, Global, ColorUtils;
{$R *.dfm}

procedure TPetForm.Button1Click(Sender: TObject);
begin
  TPet.Delete;
  TPetNameTag.Delete;
  ActiveControl := nil;
end;

procedure TPetForm.DyeGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  SelectRow := ARow;
  var Entry := GetImgEntry('Item/Pet/' + PetID + '.img/');
  if Entry <> nil then
    TColorFunc.SetSpriteColor<TWZIMGEntry>(Entry, ARow, True);
  ActiveControl := nil;
end;

procedure TPetForm.Edit2Change(Sender: TObject);
begin
  PetGrid.NarrowDown(Trim(Edit2.Text));
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
  var ImgList := GetImgList('Item/Pet');
  for var img in ImgList do
  begin
    var ID := NoIMG(img.Name);
    Inc(RowCount);
    PetGrid.RowCount := RowCount + 1;
    PetGrid.Cells[1, RowCount] := ID;
    if HasImgEntry('String/Pet.img/' + ID) then
      PetGrid.Cells[3, RowCount] := GetImgEntry('String/Pet.img/' + ID).Get('Name', '');

    var Entry := GetImgEntry('Item/Pet/' + img.Name + '/info/iconD', True);
    if Entry <> nil then
    begin
      var Bmp := Entry.Canvas.DumpBmp;
      PetGrid.CreateBitmap(2, RowCount, False, haCenter, vaCenter).Assign(Bmp);
      Bmp.Free;
    end;

  end;
  ImgList.Free;
  PetGrid.SortByColumn(1);
  PetGrid.EndUpdate;
end;

procedure TPetForm.FormClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TPetForm.FormCreate(Sender: TObject);
begin
  Left := ((Screen.Width - Width) div 2) + 400;
  Top := (Screen.Height - Height) div 2;
end;

procedure TPetForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_MENU then
    Key := 0;
end;

procedure TPetForm.PetEquipGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  var PetEquipID := PetEquipGrid.Cells[1, ARow];
  TPetEquip.Delete;
  TPetEquip.Create(PetEquipID);
end;

procedure TPetForm.PetGridClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TPetForm.PetGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  SelectRow := 0;
  PetSelectRow := ARow;
  PetID := PetGrid.Cells[1, ARow];
  TPetNameTag.Delete;
  TPet.Delete;
  TPetEquip.Delete;
  TPet.Create(PetID);

  TPetNameTag.Create(PetID);
  TPetNameTag.PetNameTag.MedalName := PetGrid.Cells[3, ARow];
  TPetNameTag.PetNameTag.InitData;
  TPetNameTag.ReDraw;
  TColorFunc.SetGridColor(PetGrid.CellGraphics[2, ARow].CellBitmap, DyeGrid);

  PetEquipGrid.ClearAll;
  var RowCount := -1;
  PetEquipGrid.BeginUpdate;
  var ImgList := GetImgList('Character/PetEquip');
  for var Img in ImgList do
  begin
    for var Iter in GetImgFile('Character/PetEquip/' + Img.Name).Root.Children do
    begin
      if (PEtID <> '') and (Iter.Name = PetID) then
      begin
        var ID := NoIMG(img.Name);
        Inc(RowCount);
        PetEquipGrid.RowCount := RowCount + 1;
        PetEquipGrid.Cells[1, RowCount] := ID;

        if HasImgEntry('String/Eqp.img/Eqp/PetEquip/' + IDToInt(ID)) then
          PetEquipGrid.Cells[3, RowCount] := GetImgEntry('String/Eqp.img/Eqp/PetEquip/' + IDToInt
            (ID)).Get('name', '');

        var Entry := GetImgEntry('Character/PetEquip/' + img.Name + '/info/icon', True);
        if Entry <> nil then
        begin
          var Bmp := Entry.Canvas.DumpBmp;
          PetEquipGrid.CreateBitmap(2, RowCount, False, haCenter, vaCenter).Assign(Bmp);
          Bmp.Free;
        end;
      end;
    end;

  end;
  ImgList.Free;

  PetEquipGrid.SortByColumn(1);
  PetEquipGrid.EndUpdate;
  ActiveControl := nil;
end;

end.

