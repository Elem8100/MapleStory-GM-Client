unit ChatRingFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, AdvObj, BaseGrid, AdvGrid;

type
  TChatRingForm = class(TForm)
    ChatRingGrid: TAdvStringGrid;
    procedure FormActivate(Sender: TObject);
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
  NameTag, WZIMGFile, WZDirectory, WzUtils, Global, StrUtils;

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
    if (LeftStr(img.Name, 6) <> '011122') and (LeftStr(img.Name, 6) <> '011150') then
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
      ChatRingGrid.Cells[3, RowCount] := GetImgEntry('String.wz/Eqp.img/Eqp/Ring/' + IDToInt(ID)).Get('Name',
        '');

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

end.

