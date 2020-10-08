unit EffectRingFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, AdvObj, BaseGrid, AdvGrid;

type
  TEffectRingForm = class(TForm)
    EffectRingGrid: TAdvStringGrid;
    procedure FormActivate(Sender: TObject);
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
  WZIMGFile, WZDirectory, WzUtils, Global, StrUtils;

procedure TEffectRingForm.FormActivate(Sender: TObject);
begin
  if HasShow then
    Exit;
  HasShow := True;
  EffectRingGrid.Canvas.Font.Size := 18;
  EffectRingGrid.Canvas.TextOut(90, 100, 'Loading...');

  var RowCount := -1;
  EffectRingGrid.BeginUpdate;

  for var Iter in EffectWz.GetImgFile('ItemEff.img').Root.Children do
  begin
    if LeftStr(Iter.Name, 3) <> '111' then
      Continue;
    var ID := '0' + Iter.Name;
    if not HasImgFile('Character.WZ/Ring/' + ID + '.img') then
      Continue;
    Inc(RowCount);
    EffectRingGrid.RowCount := RowCount + 1;
    EffectRingGrid.Cells[1, RowCount] := ID;

    if HasImgEntry('String.wz/Eqp.img/Eqp/Ring/' + IDToInt(ID)) then
      EffectRingGrid.Cells[3, RowCount] := GetImgEntry('String.wz/Eqp.img/Eqp/Ring/' + IDToInt(ID)).Get
        ('Name', '');

    var Entry := GetImgEntry('Character.WZ/Ring/' + ID + '.img' + '/info/icon', True);
    if Entry <> nil then
    begin
      var Bmp := Entry.Canvas.DumpBmp;
      EffectRingGrid.CreateBitmap(2, RowCount, False, haCenter, vaCenter).Assign(Bmp);
      Bmp.Free;
    end;

  end;

  for var Iter in EffectWz.GetImgFile('SetEff.img').Root.Children do
    for var Iter2 in Iter.Children do
      if Iter2.Name = 'info' then
        for var Iter3 in Iter2.Children do
          for var Iter4 in Iter3.Children do
            if LeftStr(Iter4.Data, 3) = '111' then
            begin
              var ID := '0' + string(Iter4.Data);
              if not HasImgFile('Character.WZ/Ring/' + ID + '.img') then
                Continue;
              Inc(RowCount);
              EffectRingGrid.RowCount := RowCount + 1;
              EffectRingGrid.Cells[1, RowCount] := ID;

              if HasImgEntry('String.wz/Eqp.img/Eqp/Ring/' + IDToInt(ID)) then
                EffectRingGrid.Cells[3, RowCount] := GetImgEntry('String.wz/Eqp.img/Eqp/Ring/' +
                  IDToInt(ID)).Get('Name', '');

              var Entry := GetImgEntry('Character.WZ/Ring/' + ID + '.img' + '/info/icon', True);
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

end.

