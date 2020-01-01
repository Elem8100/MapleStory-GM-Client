unit AndroidFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, AdvObj, BaseGrid, AdvGrid, Vcl.StdCtrls,
  Vcl.ComCtrls, iemview;

type
  TAndroidForm = class(TForm)
    Button1: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    AndroidGrid: TAdvStringGrid;
    procedure FormActivate(Sender: TObject);
    procedure AndroidGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure AvatarViewImageSelect(Sender: TObject; idx: Integer);
    procedure PageControl1Change(Sender: TObject);
  private
    HasLoad: Boolean;
    AvatarView: TImageEnMView;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AndroidForm: TAndroidForm;

implementation

{$R *.dfm}
uses
  WZIMGFile, WZDirectory, WzUtils, Global, StrUtils, Generics.Collections, Android;

function Add4(Name: string): string;
begin
  case Length(Name) of
    1:
      Result := '000' + Name;
    2:
      Result := '00' + Name;
    3:
      Result := '0' + Name;
    4:
      Result := Name;
  end;
end;

function Add8(Name: string): string;
begin
  case Length(Name) of
    4:
      Result := '0000' + Name;
    5:
      Result := '000' + Name;
    7:
      Result := '0' + Name;

  end;
end;

function Add2(Name: string): string;
begin
  case Length(Name) of
    1:
      Result := '0' + Name;
    2:
      Result := Name;
  end;
end;

procedure TAndroidForm.AndroidGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  if AndroidPlayer = nil then
    AndroidPlayer.SpawnNew;
  var IDList := TList<string>.Create;

  var AndroidID := AndroidGrid.Cells[1, ARow];
  var Num := GetImgEntry('Character.wz/Android/' + AndroidID + '.img/' + 'info/android').Data;
  var ImgNum := Add4(Num) + '.img';
  for var Iter in EtcWZ.GetImgFile('Android/' + ImgNum).Root.Children do
  begin
    if (Iter.name = 'basic') then
    begin
      for var Iter2 in Iter.Children do
        IDList.Add(Add8(Iter2.Data));
    end;
    if (Iter.name = 'costume') then
    begin
      for var Iter2 in Iter.Children do
        for var Iter3 in Iter2.Children do
          if Iter3.Name = '0' then
            IDList.Add(Add8(Iter3.Data));
    end;
  end;
  //add head
  for var i in IDList do
    if LeftStr(i, 4) = '0000' then
      IDList.Add('000120' + Add2(RightStr(i, 2)));

  var Str: string;
  for var i in IDList do
    Str := Str + i + '-';

  AndroidPlayer.Spawn(Str);

  IDList.Free;
  ActiveControl := nil;
end;

procedure TAndroidForm.Button1Click(Sender: TObject);
begin
  TAndroidNameTag.Delete;
  if AndroidPlayer <> nil then
  begin
    AndroidPlayer.RemoveSprites;
    AndroidPlayer.Dead;
    AndroidPlayer := nil;
  end;

  ActiveControl := nil;
end;

procedure TAndroidForm.AvatarViewImageSelect(Sender: TObject; idx: Integer);
begin
  if AndroidPlayer = nil then
    AndroidPlayer.SpawnNew;
  var ImageName := ExtractFileName(AvatarView.ImageFileName[idx]);
  AndroidPlayer.Spawn(ImageName);
  ActiveControl := nil;
end;

procedure TAndroidForm.FormActivate(Sender: TObject);
begin
  if HasLoad then
    Exit;

  HasLoad := True;
  AvatarView := TImageEnMView.Create(PageControl1.Pages[1]);
  AvatarView.Parent := PageControl1.Pages[1];
  AvatarView.Align := alClient;
  AvatarView.AlignWithMargins := True;
  AvatarView.Margins.Left := 3;
  AvatarView.Margins.Right := 3;
  AvatarView.Margins.Top := 3;
  AvatarView.Margins.Bottom := 3;
  AvatarView.ThumbWidth := 102;
  AvatarView.ThumbHeight := 102;
  AvatarView.BorderStyle := bsNone;
  AvatarView.Background := clWhite;
  AvatarView.ThumbnailOptionsEx := [ietxShowIconForUnknownFormat, ietxShowIconWhileLoading, ietxEnableInternalIcons];
  AvatarView.DefaultInfoText := iedtNone;
  AvatarView.MultiSelectionOptions := [];
  AvatarView.ShowText := False;
  AvatarView.ThumbnailsBackground := RGB(200, 200, 200);
  AvatarView.ThumbsRounded := 100;
  AvatarView.SelectionColor := clRed;
  AvatarView.OnImageSelect := AvatarViewImageSelect;

  AndroidGrid.Canvas.Font.Size := 18;
  AndroidGrid.Canvas.TextOut(60, 0, 'Loading...');

  var RowCount := -1;
  AndroidGrid.BeginUpdate;
  for var img in TWZDirectory(CharacterWZ.Root.Entry['Android']).Files do
  begin
    if LeftStr(img.Name, 4) = '0167' then
      Continue;

    var ID := NoIMG(img.Name);
    Inc(RowCount);
    AndroidGrid.RowCount := RowCount + 1;
    AndroidGrid.Cells[1, RowCount] := ID;
    if HasImgEntry('String.wz/Eqp.img/Eqp/android/' + IDToInt(ID)) then
      AndroidGrid.Cells[3, RowCount] := GetImgEntry('String.wz/Eqp.img/Eqp/android/' + IDToInt(ID)).Get('Name', '');

    var Entry := GetImgEntry('Character.WZ/Android/' + img.Name + '/info/iconD', True);
    if Entry <> nil then
    begin
      var Bmp := Entry.Canvas.DumpBmp;
      AndroidGrid.CreateBitmap(2, RowCount, False, haCenter, vaCenter).Assign(Bmp);
      Bmp.Free;
    end;

  end;

  AndroidGrid.SortByColumn(1);
  AndroidGrid.EndUpdate;
end;

procedure TAndroidForm.FormClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TAndroidForm.FormCreate(Sender: TObject);
begin

  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TAndroidForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_MENU then
    Key := 0;
end;

procedure TAndroidForm.PageControl1Change(Sender: TObject);
begin
  case PageControl1.TabIndex of
    1:
      begin
        AvatarView.Clear;
        AvatarView.FillFromDirectory(ExtractFilePath(ParamStr(0)) + 'Images\');
      end;
  end;
end;

end.

