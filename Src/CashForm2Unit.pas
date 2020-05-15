unit CashForm2Unit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, CurvyControls, Vcl.Grids, AdvObj,
  BaseGrid, AdvGrid, Vcl.ComCtrls, hyieutils, iexBitmaps, hyiedefs, iesettings, iexLayers, iexRulers,
  ieview, iemview, PNGMapleCanvasEx, Generics.Collections, Generics.Defaults, WZArchive;

type
  TCashForm2 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    CashGrid: TAdvStringGrid;
    Edit1: TEdit;
    CurvyPanel1: TCurvyPanel;
    IDLabel: TLabel;
    NameLabel: TLabel;
    Image1: TImage;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TabSheet2Show(Sender: TObject);
    procedure CashGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure Button1Click(Sender: TObject);
  private
    HasLoad: Boolean;
    HasShowImageGrid: Boolean;
    ImageGrid: TImageEnMView;
    IconList: TObjectList<TBmpEx>;
    Wz: TWZArchive;
    procedure ImageGridSelect(Sender: TObject; idx: Integer);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CashForm2: TCashForm2;

implementation

uses
  ConsumeFormUnit, Global, StrUtils, WZDirectory, MobDrop, MapleCharacter;
{$R *.dfm}

procedure TCashForm2.ImageGridSelect(Sender: TObject; idx: Integer);
begin
  ConsumeForm.ImageAssignIcon(ImageGrid.ImageInfoText[idx], 'Cash', IDlabel, NameLabel, Image1);
  ActiveControl := nil;

end;

procedure TCashForm2.TabSheet2Show(Sender: TObject);
begin
  if HasShowImageGrid then
    Exit;
  HasShowImageGrid := True;

  CashGrid.Canvas.Font.Size := 18;
  CashGrid.Canvas.TextOut(60, 0, 'Loading...');

  var RowCount := -1;
  CashGrid.BeginUpdate;
  for var Iter in StringWZ.GetImgFile('Cash.img').Root.Children do
  begin
    Inc(RowCount);
    CashGrid.RowCount := RowCount + 1;
    CashGrid.Cells[1, RowCount] := LeftPad(Iter.Name.ToInteger);
    CashGrid.Cells[2, RowCount] := Iter.Get('name', '');
  end;
  CashGrid.SortByColumn(1);
  CashGrid.EndUpdate;
end;

procedure TCashForm2.Button1Click(Sender: TObject);
begin
  if Trim(IDLabel.Caption) <> '' then
    TMobDrop.Drop(Round(Player.X), Round(Player.Y), 0, Trim(IDLabel.Caption));
end;

procedure TCashForm2.CashGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin

  ConsumeForm.ImageAssignIcon(CashGrid.Cells[1, ARow], 'Cash', IDLabel, NameLabel, Image1);
end;

procedure TCashForm2.FormActivate(Sender: TObject);
begin
  ActiveControl := nil;
  Edit1.Clear;
  if HasLoad then
    Exit;
  HasLoad := True;
  ConsumeForm.DumpIcons(ImageGrid, 'Cash', Wz, IconList);
end;

procedure TCashForm2.FormCreate(Sender: TObject);
begin
  ConsumeForm.CreateImageGrid(ImageGrid, CashForm2, PageControl1.Pages[0]);
  ImageGrid.OnImageSelect := ImageGridSelect;
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TCashForm2.FormDestroy(Sender: TObject);
begin
  Wz.Free;
  IconList.Free;
end;

end.

