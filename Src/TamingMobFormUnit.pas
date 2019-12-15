unit TamingMobFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, AdvObj, BaseGrid, AdvGrid, StrUtils, Vcl.StdCtrls,
  Vcl.ComCtrls, ColorUtils;

type
  TTamingMobForm = class(TForm)
    Button1: TButton;
    PageControl1: TPageControl;
    Sheet1: TTabSheet;
    Sheet2: TTabSheet;
    TamingMobGrid: TAdvStringGrid;
    DyeGrid: TAdvStringGrid;
    Edit1: TEdit;
    Label1: TLabel;
    procedure TamingMobGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure Button1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClick(Sender: TObject);
    procedure TamingMobGridClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure DyeGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure Edit1Change(Sender: TObject);
  private
    HasLoad: Boolean;
    TamingMobID: string;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TamingMobForm: TTamingMobForm;

implementation

{$R *.dfm}
uses
  TamingMob, MapleChair, Morph, MapleEffect, Global, MapleCharacter, WZIMGFile, WZDirectory, WzUtils,
  Generics.Collections, Skill;

function NoIMG(const Name: string): string; inline;
begin
  Result := ChangeFileExt(Name, '');
end;

procedure TTamingMobForm.Button1Click(Sender: TObject);
begin
  TTamingMob.Delete;
  TItemEffect.Delete(Chair);
  TMapleChair.Delete;

  Player.ResetAction := True;
  Player.NewAction :=Player.StandType;
  ActiveControl := nil;
end;

procedure TTamingMobForm.DyeGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  if TMorph.IsUse then
    Exit;

  TTamingMob.Delete;
  TMapleChair.Delete;
  TItemEffect.Delete(Chair);

  TTamingMob.IsChairTaming := False;
  TTamingMob.IsUse := True;
  case ARow of
    0:
      TTamingMob.Create(TamingMobID);
    1..10:
      TTamingMob.Create(TamingMobID, ceHue, ARow * 30);
    11:
      TTamingMob.Create(TamingMobID, ceSaturation, 25);
    12:
      TTamingMob.Create(TamingMobID, ceSaturation, -100);
    13:
      TTamingMob.Create(TamingMobID, ceContrast1);
    14:
      TTamingMob.Create(TamingMobID, ceContrast2);
    15:
      TTamingMob.Create(TamingMobID, ceContrast3);
    16:
      TTamingMob.Create(TamingMobID, ceContrast4);
    17:
      TTamingMob.Create(TamingMobID, ceContrast5);
    18:
      TTamingMob.Create(TamingMobID, ceNegative);
  end;
  ActiveControl := nil;
end;

procedure TTamingMobForm.Edit1Change(Sender: TObject);
begin
  TamingMobGrid.NarrowDown(TrimS(Edit1.Text));
end;

procedure TTamingMobForm.FormActivate(Sender: TObject);
begin
  if HasLoad then
    Exit;
  HasLoad := True;
  TamingMobGrid.Canvas.Font.Size := 18;
  TamingMobGrid.Canvas.TextOut(60, 0, 'Loading...');

  var RowCount := -1;
  TamingMobGrid.BeginUpdate;
  for var img in TWZDirectory(CharacterWZ.Root.Entry['TamingMob']).Files do
  begin
    if LeftStr(img.Name, 4) = '0191' then
      Continue;
    if LeftStr(img.Name, 4) = '0198' then
      Continue;

    var ID := NoIMG(img.Name);
    Inc(RowCount);
    TamingMobGrid.RowCount := RowCount + 1;
    TamingMobGrid.Cells[1, RowCount] := ID;
    if HasImgEntry('String.wz/Eqp.img/Eqp/Taming/' + IDToInt(ID)) then
      TamingMobGrid.Cells[3, RowCount] := GetImgEntry('String.wz/Eqp.img/Eqp/Taming/' + IDToInt(ID)).Get('Name', '');

    var Entry := GetImgEntry('Character.WZ/TamingMob/' + img.Name + '/info/icon', True);
    if Entry <> nil then
    begin
      var Bmp := Entry.Canvas.DumpBmp;
      TamingMobGrid.CreateBitmap(2, RowCount, False, haCenter, vaCenter).Assign(Bmp);
      Bmp.Free;
    end;

  end;

  if TSkill.Has001Wz then
  begin
    var Dict := TDictionary<string, string>.Create;
    for var i := 11 to 26 do
      if HasImgFile('Skill001.wz/' + '8000' + i.ToString + '.img') then
      begin
        for var Iter in Skill001Wz.GetImgFile('8000' + i.ToString + '.img').Root.Child['skill'].Children do
          if Iter.Child['vehicleID'] <> nil then
            Dict.AddOrSetValue('0' + string(Iter.Child['vehicleID'].Data), Iter.Name);
      end;

    for var i := 0 to 9 do
      if HasImgFile('Skill001.wz/' + '80011' + i.ToString + '.img') then
      begin
        for var Iter in Skill001Wz.GetImgFile('80011' + i.ToString + '.img').Root.Child['skill'].Children do
          if Iter.Child['vehicleID'] <> nil then
            Dict.AddOrSetValue('0' + string(Iter.Child['vehicleID'].Data), Iter.Name);
      end;

    for var i := 0 to TamingMobGrid.RowCount - 1 do
    begin
      var TamingID := (TamingMobGrid.Cells[1, i]);
      if (TamingMobGrid.Cells[3, i] = '') and (Dict.ContainsKey(TamingID)) then
        TamingMobGrid.Cells[3, i] := StringWZ.GetImgFile('Skill.img').Root.Get(Dict[TamingID] + '/name', '');
    end;
    Dict.Free;
  end;

  TamingMobGrid.SortByColumn(1);
  TamingMobGrid.EndUpdate;

end;

procedure TTamingMobForm.FormClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TTamingMobForm.FormCreate(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TTamingMobForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_MENU then
    Key := 0;
end;

procedure TTamingMobForm.TamingMobGridClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TTamingMobForm.TamingMobGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  if TMorph.IsUse then
    Exit;
  TamingMobID := TamingMobGrid.Cells[1, ARow];
  TTamingMob.Delete;

  TMapleChair.Delete;
  TItemEffect.Delete(Chair);

  TTamingMob.IsChairTaming := False;
  TTamingMob.Create(TamingMobID);
  TTamingMob.IsUse := True;
  TColorFunc.SetGridColor(TamingMobGrid.CellGraphics[2, ARow].CellBitmap, DyeGrid);
  ActiveControl := nil;
end;

end.

