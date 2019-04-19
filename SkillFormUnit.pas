unit SkillFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, AdvObj, BaseGrid, AdvGrid, Vcl.StdCtrls,
  WZIMGFile, Vcl.Mask, scControls, scAdvancedControls;

type
  TSkillForm = class(TForm)
    SkillGrid: TAdvStringGrid;
    SelectGrid: TAdvStringGrid;
    Label1: TLabel;
    ComBobox1: TscAdvancedComboEdit;
    procedure SelectGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure ComBobox1CloseUp(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    HasLoaded: Boolean;
    IDs: array of string;
    SelectRow: Integer;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SkillForm: TSkillForm;

implementation

uses
  Skill, WzUtils, AsphyreSprite, Global;
{$R *.dfm}

function GetJobID(ID: string): string;
begin
  Result := (ID.ToInteger div 10000).toString;
end;

function KeyNameToInt(Key: string): Integer;
begin
  case Key[1] of
    'A':
      Result := $1E;
    'B':
      Result := $30;
    'C':
      Result := $2E;
    'D':
      Result := $20;
    'E':
      Result := $12;
    'F':
      Result := $21;
    'G':
      Result := $22;
    'H':
      Result := $23;
    'I':
      Result := $17;
    'J':
      Result := $24;
    'K':
      Result := $25;
    'L':
      Result := $26;
    'M':
      Result := $32;
    'N':
      Result := $31;
    'O':
      Result := $18;
    'P':
      Result := $19;
    'Q':
      Result := $10;
    'R':
      Result := $13;
    'S':
      Result := $1F;
    'T':
      Result := $14;
    'U':
      Result := $16;
    'V':
      Result := $2F;
    'W':
      Result := $11;
    'X':
      Result := $2D;
    'Y':
      Result := $15;
    'Z':
      Result := $2C;
  end;

  if Key.Length > 1 then
  begin
    case Key[1] of
      'i':  //insert
        Result := $D2;
      'h':  //home
        Result := $C7;
      'P':  //pgup
        if Key[3] = 'U' then
          Result := $C9
        else  //pgdn
          Result := $D1;
      'd':  //delete
        Result := $D3;
      'e':  //end
        Result := $CF;
    end;
  end;

end;

procedure TSkillForm.ComBobox1CloseUp(Sender: TObject);
begin
  if ComBobox1.ItemIndex = 32 then
    Exit;

  for var r := SkillGrid.RowCount - 1 downto 1 do
  begin
    if SkillGrid.cells[4, r] = ComBobox1.Text then
      SkillGrid.RemoveRows(r, 1);
    if SkillGrid.cells[1, r] = SelectGrid.Cells[1, SelectRow] then
      SkillGrid.RemoveRows(r, 1);
  end;

  SkillGrid.Cells[1, SkillGrid.RowCount] := SelectGrid.Cells[1, SelectRow];
  var ID := SelectGrid.Cells[1, SelectRow];
  var Entry: TWZIMGEntry;
  if HasImgFile('Skill.wz/' + GetJobID(ID) + '.img') then
    Entry := GetImgEntry('Skill.wz/' + GetJobID(ID) + '.img/skill/' + ID)
  else
    Entry := GetImgEntry('Skill001.wz/' + GetJobID(ID) + '.img/skill/' + ID);
  var Bmp := Entry.Get2('icon').Canvas.DumpBmp;
  var RowCount := SkillGrid.RowCount;
  SkillGrid.CreateBitmap(2, RowCount, False, haCenter, vaCenter).Assign(Bmp);
  SkillGrid.Cells[3, RowCount] := SelectGrid.Cells[3, SelectRow];
  SkillGrid.Cells[4, RowCount] := ComBobox1.Text;
  SkillGrid.RowCount := SkillGrid.RowCount + 1;
  Bmp.Free;

  for var i := 0 to SkillGrid.RowCount - 1 do
    SkillGrid.CellProperties[4, i].Alignment := taCenter;

  //ComBobox1.ClearSelection;
  TSkill.HotKeyList.Clear;
  for var i := 1 to SkillGrid.RowCount - 1 do
  begin
    var KeyName := SkillGrid.cells[4, i];
    var SkillID := SkillGrid.cells[1, i];
    TSkill.HotKeyList.AddOrSetValue(KeyNameToInt(KeyName), SkillID);
  end;

  if not TSkill.LoadedList.contains(ID) then
    TSkill.Load(ID);
  TSkill.LoadedList.Add(ID);
end;

procedure TSkillForm.FormActivate(Sender: TObject);
begin
  if HasLoaded then
    Exit;
  HasLoaded := True;
  SelectGrid.Canvas.Font.Size := 18;
  SelectGrid.Canvas.TextOut(90, 100, '載入中...');

  with TSkill.Create(SpriteEngine) do
  begin
    Tag := 1;
  end;

  IDs := ['2321008', '2121007', '2221007', '5121001', '2301005', '1121008', '21120005', '21110006',
    '12111005', '3111003', '41001000','155111211','2211010','36121052','36121011','400041021','4331000',
    '155121202','101110202','101120102','101120202','101100100','15121052','15121002','15111022',
    '31221052','31221002'];
  var RowCount := -1;
  SelectGrid.BeginUpdate;

  for var ID in IDs do
  begin
    if HasImgFile('Skill.wz/' + GetJobID(ID) + '.img') and HasImgEntry('Skill.wz/' + GetJobID(ID) +
      '.img/skill/' + ID) then
    begin
      Inc(RowCount);
      SelectGrid.RowCount := RowCount + 1;
      SelectGrid.Cells[1, RowCount] := ID;
      var Entry := GetImgEntry('Skill.wz/' + GetJobID(ID) + '.img/skill/' + ID);
      var Bmp := Entry.Get2('icon').Canvas.DumpBmp;
      SelectGrid.CreateBitmap(2, RowCount, False, haCenter, vaCenter).Assign(Bmp);
      Bmp.Free;
      SelectGrid.Cells[3, RowCount] := GetImgEntry('String.wz/Skill.img/' + ID).Get('name', '');
    end;

    if TSkill.Has001Wz then
      if HasImgFile('Skill001.wz/' + GetJobID(ID) + '.img') and HasImgEntry('Skill001.wz/' +
        GetJobID(ID) + '.img/skill/' + ID) then
      begin
        Inc(RowCount);
        SelectGrid.RowCount := RowCount + 1;
        SelectGrid.Cells[1, RowCount] := ID;
        var Entry := GetImgEntry('Skill01.wz/' + GetJobID(ID) + '.img/skill/' + ID);
        var Bmp := Entry.Get2('icon').Canvas.DumpBmp;
        SelectGrid.CreateBitmap(2, RowCount, False, haCenter, vaCenter).Assign(Bmp);
        Bmp.Free;
        SelectGrid.Cells[3, RowCount] := GetImgEntry('String.wz/Skill.img/' + ID).Get('name', '');
      end;

  end;

  SelectGrid.SortByColumn(1);
  SelectGrid.EndUpdate;
  SkillGrid.Cells[1, 0] := 'ID';
  SkillGrid.Cells[2, 0] := '圖示';
  SkillGrid.Cells[3, 0] := '名稱';
  SkillGrid.Cells[4, 0] := '快捷鍵';
  for var i := 0 to SkillGrid.ColCount - 1 do
    SkillGrid.CellProperties[i, 0].Alignment := taCenter;
end;

procedure TSkillForm.SelectGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  ComBobox1.Left := 312;
  ComBobox1.Top := SelectGrid.CellRect(ACol, ARow).Location.Y + 7;
  ComBobox1.ItemIndex := 32;
  ComBobox1.Enabled := True;
  SelectRow := ARow;

end;

end.

