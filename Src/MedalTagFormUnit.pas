unit MedalTagFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids,
  AdvObj, BaseGrid, AdvGrid, AdvUtil;

type
  TMedalTagForm = class(TForm)
    MedalGrid: TAdvStringGrid;
    Button1: TButton;
    procedure MedalGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure Button1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MedalGridClick(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    HasShow: Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MedalTagForm: TMedalTagForm;

implementation

{$R *.dfm}
uses
  NameTag, WZIMGFile, WZDirectory, WzUtils, Global, StrUtils;

procedure TMedalTagForm.Button1Click(Sender: TObject);
begin
  TMedalTag.Delete;
  ActiveControl := nil;
end;

procedure TMedalTagForm.FormActivate(Sender: TObject);
begin
  if HasShow then
    Exit;
  HasShow := True;
  MedalGrid.Canvas.Font.Size:=18;
  MedalGrid.Canvas.TextOut(90,100,'Loading...');

  var RowCount := -1;
  MedalGrid.BeginUpdate;
  for var img in TWZDirectory(CharacterWZ.Root.Entry['Accessory']).Files do
  begin
    if LeftStr(img.Name, 4) <> '0114' then
      Continue;
    if GetImgEntry('Character.WZ/Accessory/' + img.Name + '/info/medalTag') = nil then
      Continue;
    var TagNum := GetImgEntry('Character.WZ/Accessory/' + img.Name + '/info/medalTag').Data;
    if GetImgEntry('UI.wz/NameTag.img/medal/' + string(TagNum)) = nil then
      Continue;

    var ID := NoIMG(img.Name);
    Inc(RowCount);
    MedalGrid.RowCount := RowCount + 1;
    MedalGrid.Cells[1, RowCount] := ID;
    if HasImgEntry('String.wz/Eqp.img/Eqp/Accessory/' + IDToInt(ID)) then
      MedalGrid.Cells[3, RowCount] := GetImgEntry('String.wz/Eqp.img/Eqp/Accessory/' + IDToInt(ID)).Get('Name', '');

    var Entry := GetImgEntry('Character.WZ/Accessory/' + img.Name + '/info/icon', True);
    if Entry <> nil then
    begin
      var Bmp := Entry.Canvas.DumpBmp;
      MedalGrid.CreateBitmap(2, RowCount, False, haCenter, vaCenter).Assign(Bmp);
      Bmp.Free;
    end;

  end;
  MedalGrid.SortByColumn(1);
  MedalGrid.EndUpdate;

end;

procedure TMedalTagForm.FormClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TMedalTagForm.FormCreate(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TMedalTagForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_MENU then
    Key := 0;
end;

procedure TMedalTagForm.MedalGridClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TMedalTagForm.MedalGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  TMedalTag.Delete;
  TMedalTag.Create(MedalGrid.Cells[1, ARow]);
  ActiveControl := nil;
end;

end.

