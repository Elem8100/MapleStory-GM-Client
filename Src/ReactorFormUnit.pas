unit ReactorFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, AdvObj, BaseGrid, AdvGrid, Vcl.ExtCtrls,
  Vcl.StdCtrls;

type
  TReactorForm = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    ReactorGrid: TAdvStringGrid;
    Button1: TButton;
    Button2: TButton;
    procedure FormActivate(Sender: TObject);
    procedure ReactorGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    HasLoad: Boolean;
    ReactorID: string;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ReactorForm: TReactorForm;

implementation

uses
  WZUtils, Global, StrUtils, WZDirectory, MapleEffect, Reactor;

{$R *.dfm}

procedure TReactorForm.Button1Click(Sender: TObject);
begin
  TReactor.Spawn(ReactorID);
  ActiveControl := nil;
end;

procedure TReactorForm.Button2Click(Sender: TObject);
begin
  TReactor.Remove;
  ActiveControl := nil;
end;

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

    var Entry := ReactorWZ.GetImgFile(img.Name).Root;

    if (Entry.Get('0') = nil) or (Entry.Get('0/0') = nil) then
      Continue;
    if (Entry.Get('0/0') <> nil) and (Entry.Get('0/0/_inlink') = nil) and (Entry.Get('0/0').Canvas.Width
      <= 4) then
      Continue;
    Inc(RowCount);
    ReactorGrid.RowCount := RowCount + 1;
    ReactorGrid.Cells[1, RowCount] := NoIMG(img.Name);

    var ReactorName: string;

    if Entry.Get('info/info') <> nil then
      ReactorName := Entry.Get('info/info').Data
    else if Entry.Get('info/viewName') <> nil then
      ReactorName := Entry.Get('info/viewName').Data;
    ReactorGrid.Cells[2, RowCount] := ReactorName;
  end;
  ReactorGrid.SortByColumn(1);
  ReactorGrid.EndUpdate;
end;

procedure TReactorForm.FormClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TReactorForm.FormCreate(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TReactorForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_MENU then
    Key := 0;
end;

procedure TReactorForm.ReactorGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  ReactorID := ReactorGrid.Cells[1, ARow];
  var Entry := GetImgEntry('Reactor.wz/' + ReactorID + '.img/0/0');
  if Entry <> nil then
  begin
    var Bmp := GetImgEntry('Reactor.wz/' + ReactorID + '.img/0/0', True).Canvas.DumpBmp;
    Image1.Picture.Assign(Bmp);
    Bmp.Free;
  end;
end;

end.

