unit LabelRingFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, AdvObj,
  BaseGrid, AdvGrid, Vcl.StdCtrls;

type
  TLabelRingForm = class(TForm)
    LabelRingGrid: TAdvStringGrid;
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    procedure FormActivate(Sender: TObject);
    procedure LabelRingGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClick(Sender: TObject);
    procedure LabelRingGridClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    HasShow: Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  LabelRingForm: TLabelRingForm;

implementation

{$R *.dfm}

uses
  NameTag, WZIMGFile, WZDirectory, WzUtils, Global, StrUtils;

procedure TLabelRingForm.Button1Click(Sender: TObject);
begin
  TNameTag.IsUse := True;
  TLabelRingTag.Delete;
  ActiveControl := nil;
end;

procedure TLabelRingForm.Edit1Change(Sender: TObject);
begin
    LabelRingGrid.NarrowDown(Trim(Edit1.Text));
end;

procedure TLabelRingForm.FormActivate(Sender: TObject);
begin
  if HasShow then
    Exit;
  HasShow := True;
  LabelRingGrid.Canvas.Font.Size := 18;
  LabelRingGrid.Canvas.TextOut(90, 100, 'Loading...');

  var RowCount := -1;
  LabelRingGrid.BeginUpdate;
  for var img in TWZDirectory(CharacterWZ.Root.Entry['Ring']).Files do
  begin
    if (LeftStr(img.Name, 6) <> '011121') and (LeftStr(img.Name, 6) <> '011151') then
      Continue;
    if GetImgEntry('Character.WZ/Ring/' + img.Name + '/info/nameTag') = nil then
      Continue;
    var TagNum := GetImgEntry('Character.WZ/Ring/' + img.Name + '/info/nameTag').Data;
    if GetImgEntry('UI.wz/NameTag.img/' + string(TagNum)) = nil then
      Continue;

    var ID := NoIMG(img.Name);
    Inc(RowCount);
    LabelRingGrid.RowCount := RowCount + 1;
    LabelRingGrid.Cells[1, RowCount] := ID;
    if HasImgEntry('String.wz/Eqp.img/Eqp/Ring/' + IDToInt(ID)) then
      LabelRingGrid.Cells[3, RowCount] := GetImgEntry('String.wz/Eqp.img/Eqp/Ring/' + IDToInt(ID)).Get('Name', '');

    var Entry := GetImgEntry('Character.WZ/Ring/' + img.Name + '/info/icon', True);
    if Entry <> nil then
    begin
      var Bmp := Entry.Canvas.DumpBmp;
      LabelRingGrid.CreateBitmap(2, RowCount, False, haCenter, vaCenter).Assign(Bmp);
      Bmp.Free;
    end;

  end;
  LabelRingGrid.SortByColumn(1);
  LabelRingGrid.EndUpdate;

end;

procedure TLabelRingForm.FormClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TLabelRingForm.FormCreate(Sender: TObject);
begin
  Left := ((Screen.Width - Width) div 2)+400;
  Top := (Screen.Height - Height) div 2;
end;

procedure TLabelRingForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_MENU then
    Key := 0;
end;

procedure TLabelRingForm.LabelRingGridClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TLabelRingForm.LabelRingGridClickCell(Sender: TObject; ARow,
  ACol: Integer);
begin
  TNameTag.IsUse := False;
  TLabelRingTag.Delete;
  TLabelRingTag.Create(LabelRingGrid.Cells[1, ARow]);
  ActiveControl := nil;
end;

end.

