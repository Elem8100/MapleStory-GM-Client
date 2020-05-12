unit ConsumeFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, CurvyControls, Vcl.Grids, AdvObj,
  BaseGrid, AdvGrid, Vcl.ComCtrls, hyieutils, iexBitmaps, hyiedefs, iesettings, iexLayers, iexRulers,
  ieview, iemview, PNGMapleCanvasEx, Generics.Collections, Generics.Defaults, WZArchive;

type
  TConsumeForm = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    CurvyPanel1: TCurvyPanel;
    IDLabel: TLabel;
    NameLabel: TLabel;
    Image1: TImage;
    Button1: TButton;
    ConsumeGrid: TAdvStringGrid;
    Edit1: TEdit;
    procedure FormActivate(Sender: TObject);
    procedure ConsumeGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure FormCreate(Sender: TObject);
    procedure TabSheet2Show(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    HasLoad: Boolean;
    HasShowImageGrid: Boolean;
    ImageGrid: TImageEnMView;
    IconList: TObjectList<TBmpEx>;
    Wz: TWZArchive;
    procedure ImageAssignIcon(ID: string);
    procedure ImageGridSelect(Sender: TObject; idx: Integer);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ConsumeForm: TConsumeForm;

implementation

uses
  WzUtils, Global, StrUtils, WZDirectory, MobDrop, MapleCharacter;
{$R *.dfm}

procedure TConsumeForm.ImageAssignIcon(ID: string);
begin
  var ConsumeID := ID;
  var Left4 := LeftStr(ConsumeID, 4);
  if GetImgEntry('Item.wz/Consume/' + Left4 + '.img/' + ConsumeID + '/info/icon') <> nil then
  begin
    var PNG := GetImgEntry('Item.wz/Consume/' + Left4 + '.img/' + ConsumeID + '/info/icon', True).Canvas.DumpPNG;
    Image1.Picture.Assign(PNG);
    PNG.Free;
  end;
  IDLabel.Caption := ConsumeID;
  NameLabel.Caption := StringWZ.GetImgFile('Consume.img').Root.Get(IDToInt(ID) + '/name', '');
end;

procedure TConsumeForm.Button1Click(Sender: TObject);
begin
  if Trim(IDLabel.Caption) <> '' then
    TMobDrop.Drop(Round(Player.X), Round(Player.Y), 0, Trim(IDLabel.Caption));
end;

procedure TConsumeForm.ConsumeGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  ImageAssignIcon(ConsumeGrid.Cells[1, ARow]);
end;

procedure TConsumeForm.Edit1Change(Sender: TObject);
begin
  ConsumeGrid.NarrowDown(Trim(Edit1.Text));
end;

procedure TConsumeForm.FormActivate(Sender: TObject);
begin
  ActiveControl := nil;
  Edit1.Clear;
  if HasLoad then
    Exit;
  HasLoad := True;
  with ImageGrid.GetCanvas do
  begin
    Font.Size := 24;
    TextOut(100, 100, 'Loading...')
  end;
  Wz := TWZArchive.Create(WzPath + '\Item.wz');
  var Dir := TWZDirectory(Wz.Root.Entry['Consume']);
  IconList := TObjectList<TBmpEx>.Create;
  for var img in Dir.Files do
  begin
    if not IsNumber(img.Name[1]) then
      Continue;
    with Wz.ParseFile(img) do
    begin
      for var Iter in Root.Children do
      begin
        if not IsNumber(Iter.Name) then
          Continue;
        if Iter.Get('info/icon') <> nil then
        begin
          var Bmp := Iter.Get2('info/icon').Canvas.DumpBmpEx;
          Bmp.ID := Iter.Name;
          Bmp.Name := Name;
          IconList.Add(Bmp);
        end;
      end;
      Free;
    end;
  end;
  IconList.Sort(TComparer<TBmpEx>.Construct(
    function(const Left, Right: TBmpEx): Integer
    begin
      Result := Left.ID.ToInteger - Right.ID.ToInteger;
    end));
  var Index := -1;
  ImageGrid.LockUpdate;
  for var Iter in IconList do
  begin
    ImageGrid.AppendImage(Iter);
    Inc(Index);
    ImageGrid.ImageInfoText[Index] := Iter.ID;
  end;
  ImageGrid.UnlockUpdate;
  ImageGrid.ViewX := 0;
  ImageGrid.ViewY := 0;
  Self.BringToFront;
end;

procedure TConsumeForm.ImageGridSelect(Sender: TObject; idx: Integer);
begin
  ImageAssignIcon(ImageGrid.ImageInfoText[idx]);
  ActiveControl := nil;
end;

procedure TConsumeForm.TabSheet2Show(Sender: TObject);
begin
  if HasShowImageGrid then
    Exit;
  HasShowImageGrid := True;

  ConsumeGrid.Canvas.Font.Size := 18;
  ConsumeGrid.Canvas.TextOut(60, 0, 'Loading...');

  var RowCount := -1;
  ConsumeGrid.BeginUpdate;
  for var Iter in StringWZ.GetImgFile('Consume.img').Root.Children do
  begin
    Inc(RowCount);
    ConsumeGrid.RowCount := RowCount + 1;
    ConsumeGrid.Cells[1, RowCount] := LeftPad(Iter.Name.ToInteger);
    ConsumeGrid.Cells[2, RowCount] := Iter.Get('name', '');
  end;
  ConsumeGrid.SortByColumn(1);
  ConsumeGrid.EndUpdate;
end;

procedure TConsumeForm.FormCreate(Sender: TObject);
begin
  ImageGrid := TImageEnMView.Create(ConsumeForm);
  ImageGrid.Parent := PageControl1.Pages[0];
  ImageGrid.Visible := True;
  ImageGrid.Align := alClient;
  ImageGrid.AlignWithMargins := True;
  ImageGrid.Margins.Left := 3;
  ImageGrid.Margins.Right := 3;
  ImageGrid.Margins.Top := 3;
  ImageGrid.Margins.Bottom := 3;
  ImageGrid.BorderStyle := bsNone;
  ImageGrid.Background := clWhite;
  ImageGrid.ThumbWidth := 35;
  ImageGrid.ThumbHeight := 35;
  ImageGrid.ThumbnailOptionsEx := [ietxShowIconForUnknownFormat, ietxShowIconWhileLoading, ietxEnableInternalIcons];
  ImageGrid.DefaultInfoText := iedtNone;
  ImageGrid.MultiSelectionOptions := [];
  ImageGrid.ShowText := False;
  ImageGrid.SelectionColor := clRed;
  ImageGrid.OnImageSelect := ImageGridSelect;
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TConsumeForm.FormDestroy(Sender: TObject);
begin
  Wz.Free;
  IconList.Free;
end;

end.

