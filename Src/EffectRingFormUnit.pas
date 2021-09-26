unit EffectRingFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, AdvObj, BaseGrid, AdvGrid,
  Vcl.StdCtrls, AdvUtil;

type
  TEffectRingForm = class(TForm)
    EffectRingGrid: TAdvStringGrid;
    Button1: TButton;
    procedure FormActivate(Sender: TObject);
    procedure EffectRingGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EffectRingGridClick(Sender: TObject);
  private
    HasShow: Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  EffectRingForm: TEffectRingForm;

implementation

{$R *.dfm}
uses
  WZIMGFile, WZDirectory, WzUtils, Global, StrUtils, MapleEffect;

procedure TEffectRingForm.Button1Click(Sender: TObject);
begin
  for var i in TSetEffect.UseList do
    TSetEffect.Delete(i.Key);
  TItemEffect.Delete(Ring);
  ActiveControl := nil;
end;

procedure TEffectRingForm.EffectRingGridClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TEffectRingForm.EffectRingGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  var ID := EffectRingGrid.Cells[1, ARow];
  if (ID = '01112127') or (ID = '01112804') or (ID = '01113021') or (ID = '01113228') then
    Exit;
  for var i in TSetEffect.UseList do
    TSetEffect.Delete(i.Key);
  TItemEffect.Delete(Ring);
  if TItemEffect.AllList.contains(ID) then
    TItemEffect.Create(ID, Equip);
  if TSetEffect.AllList.ContainsKey(ID) then
    TSetEffect.Create(ID);
  ActiveControl := nil;
end;

procedure TEffectRingForm.FormActivate(Sender: TObject);
begin
  if HasShow then
    Exit;
  HasShow := True;
  EffectRingGrid.Canvas.Font.Size := 18;
  EffectRingGrid.Canvas.TextOut(90, 100, 'Loading...');

  var RowCount := -1;
  EffectRingGrid.BeginUpdate;

  for var Iter in GetImgFile('Effect/ItemEff.img').Root.Children do
  begin
    if LeftStr(Iter.Name, 3) <> '111' then
      Continue;

    var ID := '0' + Iter.Name;
    if not HasImgFile('Character/Ring/' + ID + '.img') then
      Continue;
    Inc(RowCount);
    EffectRingGrid.RowCount := RowCount + 1;
    EffectRingGrid.Cells[1, RowCount] := ID;

    if HasImgEntry('String/Eqp.img/Eqp/Ring/' + IDToInt(ID)) then
      EffectRingGrid.Cells[3, RowCount] := GetImgEntry('String/Eqp.img/Eqp/Ring/' + IDToInt(ID)).Get
        ('Name', '');

    var Entry := GetImgEntry('Character/Ring/' + ID + '.img' + '/info/icon', True);
    if Entry <> nil then
    begin
      var Bmp := Entry.Canvas.DumpBmp;
      EffectRingGrid.CreateBitmap(2, RowCount, False, haCenter, vaCenter).Assign(Bmp);
      Bmp.Free;
    end;
  end;

  for var Iter in GetImgFile('Effect/SetEff.img').Root.Children do
    for var Iter2 in Iter.Children do
      if Iter2.Name = 'info' then
        for var Iter3 in Iter2.Children do
          for var Iter4 in Iter3.Children do
            if LeftStr(Iter4.Data, 3) = '111' then
            begin
              var ID := '0' + string(Iter4.Data);
              if not HasImgFile('Character/Ring/' + ID + '.img') then
                Continue;
              Inc(RowCount);
              EffectRingGrid.RowCount := RowCount + 1;
              EffectRingGrid.Cells[1, RowCount] := ID;

              if HasImgEntry('String/Eqp.img/Eqp/Ring/' + IDToInt(ID)) then
                EffectRingGrid.Cells[3, RowCount] := GetImgEntry('String/Eqp.img/Eqp/Ring/' +
                  IDToInt(ID)).Get('Name', '');

              var Entry := GetImgEntry('Character/Ring/' + ID + '.img' + '/info/icon', True);
              if Entry <> nil then
              begin
                var Bmp := Entry.Canvas.DumpBmp;
                EffectRingGrid.CreateBitmap(2, RowCount, False, haCenter, vaCenter).Assign(Bmp);
                Bmp.Free;
              end;
            end;

  EffectRingGrid.SortByColumn(1);
  EffectRingGrid.EndUpdate;

end;

procedure TEffectRingForm.FormClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TEffectRingForm.FormCreate(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TEffectRingForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_MENU then
    Key := 0;
end;

end.

