unit ImageInfoUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, AdvUtil, Vcl.Grids, AdvObj, BaseGrid,
  AdvGrid,StrUtils;

type
  TImageInfoForm = class(TForm)
    ImageInfoGrid: TAdvStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    HasLoad :Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ImageInfoForm: TImageInfoForm;

implementation

{$R *.dfm}

uses Global,WZIMGFile, WZDirectory, WzUtils;

procedure TImageInfoForm.FormCreate(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TImageInfoForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 Key:=0;
end;

procedure TImageInfoForm.FormShow(Sender: TObject);
begin
  ImageInfoGrid.ClearAll;
  var Row := 0;
  var Bmp: TBitmap;
  for var ImageEntry in Images.Keys do
  begin
    var Path := ImageEntry.GetPath;
    if (LeftStr(Path, 3) = 'Map') and (LeftStr(Path, 16) <> 'Map.wz/MapHelper') then
    begin
      Inc(Row);
      ImageInfoGrid.RowCount := Row + 1;
      ImageInfoGrid.Cells[1, Row] := GetImgEntry(Path, True).GetPath;
      Bmp := GetImgEntry(Path, True).Canvas.DumpBmp;
      if (Bmp.Height > 200) then
      begin
        ImageInfoGrid.RowHeights[Row] := 200;
        ImageInfoGrid.CreatePicture(2, Row, False, StretchWithAspectRatio, 1, haCenter, vaCenter).Assign(Bmp);
      end
      else
      begin
        ImageInfoGrid.RowHeights[Row] := Bmp.Height;
        ImageInfoGrid.CreateBitmap(2, Row, False, haCenter, vaCenter).Assign(Bmp);
      end;
      Bmp.Free;
    end;
  end;
  ImageInfoGrid.Fixedcolwidth := 0;
  ImageInfoGrid.RowHeights[0] := 20;
  ImageInfoGrid.RowHeights[1] := 53;
  ImageInfoGrid.ColWidths[1] := 200;
  ImageInfoGrid.ColWidths[2] := 320;
  ImageInfoGrid.ColumnHeaders.Clear;
  ImageInfoGrid.ColumnHeaders.Add('');
  ImageInfoGrid.ColumnHeaders.Add('Path');
  ImageInfoGrid.ColumnHeaders.Add('Pic');
  ImageInfoGrid.SortByColumn(1);
  for var i := 0 to ImageInfoGrid.ColCount - 1 do
    ImageInfoGrid.CellProperties[i, 0].Alignment := taCenter;

end;

end.
