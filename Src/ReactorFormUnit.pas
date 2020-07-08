unit ReactorFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, AdvObj, BaseGrid, AdvGrid, Vcl.ExtCtrls;

type
  TReactorForm = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    ReactorGrid: TAdvStringGrid;
    procedure FormActivate(Sender: TObject);
    procedure ReactorGridClickCell(Sender: TObject; ARow, ACol: Integer);
  private
    HasLoad: Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ReactorForm: TReactorForm;

implementation

uses
  WZUtils, Global, StrUtils, WZDirectory, MapleEffect;

{$R *.dfm}

procedure TReactorForm.FormActivate(Sender: TObject);
begin
  if HasLoad then
    Exit;
  HasLoad := True;
  ReactorGrid.Canvas.Font.Size := 18;
  ReactorGrid.Canvas.TextOut(60, 0, 'Loading...');

  var RowCount := -1;
  ReactorGrid.BeginUpdate;
  for var img in ReactorWZ.Root.Files do
  begin
    Inc(RowCount);
    ReactorGrid.RowCount := RowCount + 1;
    ReactorGrid.Cells[1, RowCount] := img.Name;
    var Entry := ReactorWZ.GetImgFile(img.Name).Root;
    var ReactorName: string;

    if Entry.Get('info/info') <> nil then
      ReactorName := Entry.Get('info/info').Data
    else if Entry.Get('info/viewName') <> nil then
      ReactorName := Entry.Get('info/viewName').Data;
    ReactorGrid.Cells[2, RowCount] := ReactorName;

  //   if Entry.Get('0/0') <> nil then
    //begin
      //  var Bmp := Iter.Get2('info/icon').Canvas.DumpBmp;
       // ChairGrid.CreateBitmap(2, Row, False, haCenter, vaCenter).Assign(Bmp);
       // Bmp.Free;
   // end;

  end;

  ReactorGrid.SortByColumn(1);
  ReactorGrid.EndUpdate;
end;

procedure TReactorForm.ReactorGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  var ID := ReactorGrid.Cells[1, ARow];
  var Entry := GetImgEntry('Reactor.wz/' + ID + '/0/0');
  if Entry <> nil then
  begin
    var Bmp := GetImgEntry('Reactor.wz/' + ID + '/0/0', True).Canvas.DumpBmp;
    Image1.Picture.Assign(Bmp);
    Bmp.Free;
  end;

end;

end.

