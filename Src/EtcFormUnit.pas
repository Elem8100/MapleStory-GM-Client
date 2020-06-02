unit EtcFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, iexLayers, iexRulers, ieview, iemview, PNGMapleCanvasEx,
  Generics.Collections, Generics.Defaults, WZArchive, Vcl.StdCtrls, Vcl.ExtCtrls, CurvyControls,
  Vcl.Grids, AdvObj, BaseGrid, AdvGrid, Vcl.ComCtrls;

type
  TEtcForm = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    EtcGrid: TAdvStringGrid;
    Edit1: TEdit;
    CurvyPanel1: TCurvyPanel;
    IDLabel: TLabel;
    NameLabel: TLabel;
    Image1: TImage;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure TabSheet2Show(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure EtcGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EtcGridClick(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
  EtcForm: TEtcForm;

implementation

uses
  ConsumeFormUnit, Global, StrUtils, WZDirectory, MobDrop, MapleCharacter;
{$R *.dfm}

procedure TEtcForm.ImageGridSelect(Sender: TObject; idx: Integer);
begin
  ConsumeForm.ImageAssignIcon(ImageGrid.ImageInfoText[idx], 'Etc', IDlabel, NameLabel, Image1,True);
  ActiveControl := nil;
end;

procedure TEtcForm.TabSheet2Show(Sender: TObject);
begin
  if HasShowImageGrid then
    Exit;
  HasShowImageGrid := True;

  EtcGrid.Canvas.Font.Size := 18;
  EtcGrid.Canvas.TextOut(60, 0, 'Loading...');

  var RowCount := -1;
  EtcGrid.BeginUpdate;
  for var Iter in StringWZ.GetImgFile('Etc.img').Root.Child['Etc'].Children do
  begin
    Inc(RowCount);
    EtcGrid.RowCount := RowCount + 1;
    EtcGrid.Cells[1, RowCount] := LeftPad(Iter.Name.ToInteger);
    EtcGrid.Cells[2, RowCount] := Iter.Get('name', '');
  end;
  EtcGrid.SortByColumn(1);
  EtcGrid.EndUpdate;
end;

procedure TEtcForm.Button1Click(Sender: TObject);
begin
  if Trim(IDLabel.Caption) <> '' then
    TMobDrop.Drop(Round(Player.X), Round(Player.Y), 0, Trim(IDLabel.Caption));
  ActiveControl := nil;
end;

procedure TEtcForm.EtcGridClick(Sender: TObject);
begin
   ActiveControl := nil;
end;

procedure TEtcForm.EtcGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  ConsumeForm.ImageAssignIcon(EtcGrid.Cells[1, ARow], 'Etc', IDLabel, NameLabel, Image1,True);
  ActiveControl := nil;
end;

procedure TEtcForm.FormActivate(Sender: TObject);
begin
  ActiveControl := nil;
  Edit1.Clear;
  if HasLoad then
    Exit;
  HasLoad := True;
  ConsumeForm.DumpIcons(ImageGrid, 'Etc', Wz, IconList);
end;

procedure TEtcForm.FormClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TEtcForm.FormCreate(Sender: TObject);
begin
  ConsumeForm.CreateImageGrid(ImageGrid, EtcForm, PageControl1.Pages[0]);
  ImageGrid.OnImageSelect := ImageGridSelect;
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TEtcForm.FormDestroy(Sender: TObject);
begin
  Wz.Free;
  IconList.Free;
end;

procedure TEtcForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if Key = VK_MENU then
    Key := 0;
end;

end.

