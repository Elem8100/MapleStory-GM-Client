unit ChatRingFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, AdvObj, BaseGrid, AdvGrid, Vcl.StdCtrls;

type
  TChatRingForm = class(TForm)
    ChatRingGrid: TAdvStringGrid;
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure ChatRingGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
  private
    HasShow: Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ChatRingForm: TChatRingForm;

implementation

{$R *.dfm}

uses
  NameTag, WZIMGFile, WZDirectory, WzUtils, Global, StrUtils, ChatBalloon;

procedure TChatRingForm.Button1Click(Sender: TObject);
begin
  ActiveControl := nil;
  TChatRingBalloon.IsUse := False;
  Label3.Caption := '';
end;

procedure TChatRingForm.ChatRingGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  TChatRingBalloon.IsUse := True;
  TChatRingBalloon.TagNum := GetImgEntry('Character.WZ/Ring/' + ChatRingGrid.Cells[1, ARow] + '.img'
    + '/info/chatBalloon').Data;
  Label3.Caption := ChatRingGrid.Cells[3, ARow];

  ActiveControl := nil;
end;

procedure TChatRingForm.Edit1Change(Sender: TObject);
begin
  ChatRingGrid.NarrowDown(Trim(Edit1.Text));
end;

procedure TChatRingForm.FormActivate(Sender: TObject);
begin
  if HasShow then
    Exit;
  HasShow := True;
  ChatRingGrid.Canvas.Font.Size := 18;
  ChatRingGrid.Canvas.TextOut(90, 100, 'Loading...');

  var RowCount := -1;
  ChatRingGrid.BeginUpdate;
  for var img in TWZDirectory(CharacterWZ.Root.Entry['Ring']).Files do
  begin
    if (LeftStr(img.Name, 6) <> '011122') and (LeftStr(img.Name, 6) <> '011150') and (LeftStr(img.Name,
      6) <> '011152') then
      Continue;
    if GetImgEntry('Character.WZ/Ring/' + img.Name + '/info/chatBalloon') = nil then
      Continue;
    var TagNum := GetImgEntry('Character.WZ/Ring/' + img.Name + '/info/chatBalloon').Data;
    if GetImgEntry('UI.wz/ChatBalloon.img/' + string(TagNum)) = nil then
      Continue;

    var ID := NoIMG(img.Name);
    Inc(RowCount);
    ChatRingGrid.RowCount := RowCount + 1;
    ChatRingGrid.Cells[1, RowCount] := ID;
    if HasImgEntry('String.wz/Eqp.img/Eqp/Ring/' + IDToInt(ID)) then
      ChatRingGrid.Cells[3, RowCount] := GetImgEntry('String.wz/Eqp.img/Eqp/Ring/' + IDToInt(ID)).Get
        ('Name', '');

    var Entry := GetImgEntry('Character.WZ/Ring/' + img.Name + '/info/icon', True);
    if Entry <> nil then
    begin
      var Bmp := Entry.Canvas.DumpBmp;
      ChatRingGrid.CreateBitmap(2, RowCount, False, haCenter, vaCenter).Assign(Bmp);
      Bmp.Free;
    end;

  end;
  ChatRingGrid.SortByColumn(1);
  ChatRingGrid.EndUpdate;

end;

procedure TChatRingForm.FormClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TChatRingForm.FormCreate(Sender: TObject);
begin

  Left := ((Screen.Width - Width) div 2) + 400;
  Top := (Screen.Height - Height) div 2;
end;

procedure TChatRingForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_MENU then
    Key := 0;
end;

end.

