unit SkillFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids,
  AdvObj, BaseGrid, AdvGrid, Vcl.StdCtrls, WZIMGFile, Vcl.Mask, scControls,
  scAdvancedControls, AdvUtil, Generics.Collections, WZArchive, WZDirectory;

type
  TSkillForm = class(TForm)
    SkillGrid: TAdvStringGrid;
    SelectGrid: TAdvStringGrid;
    Label1: TLabel;
    ComBobox1: TscAdvancedComboEdit;
    Label2: TLabel;
    procedure SelectGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure ComBobox1CloseUp(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    HasLoaded: Boolean;

    SelectRow: Integer;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SkillForm: TSkillForm;

implementation

uses
  Skill, WzUtils, Global, StrUtils;
{$R *.dfm}

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
  if HasImgFile('Skill/' + GetJobImg(ID) + '.img') then
    Entry := GetImgEntry('Skill/' + GetJobImg(ID) + '.img/skill/' + ID);
 // else
  //  Entry := GetImgEntry('Skill001.wz/' + GetJobID(ID) + '.img/skill/' + ID);
  var Bmp := Entry.Get2('icon').Canvas.DumpBmp;

  var RowCount := SkillGrid.RowCount;
  SkillGrid.CreateBitmap(2, RowCount, False, haCenter, vaCenter).Assign(Bmp);
  SkillGrid.Cells[3, RowCount] := SelectGrid.Cells[3, SelectRow];
  SkillGrid.Cells[4, RowCount] := ComBobox1.Text;
  SkillGrid.RowCount := SkillGrid.RowCount + 1;
  Bmp.Free;

  for var i := 0 to SkillGrid.RowCount - 1 do
    SkillGrid.CellProperties[4, i].Alignment := taCenter;

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
  ComBobox1.Visible := false;
end;

procedure TSkillForm.FormActivate(Sender: TObject);
begin
  if HasLoaded then
    Exit;
  HasLoaded := True;
  SelectGrid.Canvas.Font.Size := 18;
  SelectGrid.Canvas.TextOut(90, 100, 'Loading...');

  with TSkill.Create(SpriteEngine) do
  begin
    Tag := 1;
  end;

  {
  TSkill.AllIDs := TDictionary<string, string>.Create;
  TSkill.AllIDs.Add('2321008', ',100,0,0');
  TSkill.AllIDs.Add('2121007', '100,0,0');
  TSkill.AllIDs.Add('2221007', '100,0,0');
  TSkill.AllIDs.Add('2301005', '100,0,0');
  TSkill.AllIDs.Add('1121008', '100,0,0');
  TSkill.AllIDs.Add('21120005', '100,0,0');
  TSkill.AllIDs.Add('21110006', '100,0,0');
  TSkill.AllIDs.Add('155111211', '100,0,0');
  TSkill.AllIDs.Add('2211010', '100,0,0');
  TSkill.AllIDs.Add('36121052', '100,0,0');
  TSkill.AllIDs.Add('36121011', '100,0,0');
  TSkill.AllIDs.Add('400041021', '100,0,0');
  TSkill.AllIDs.Add('101110202', '100,0,0');
  TSkill.AllIDs.Add('101120102', '100,0,0');
  TSkill.AllIDs.Add('101120202', '100,0,0');
  TSkill.AllIDs.Add('101100100', '100,0,0');
  TSkill.AllIDs.Add('15121052', '100,0,0');
  TSkill.AllIDs.Add('15121002', '100,0,0');
  TSkill.AllIDs.Add('15111002', '100,0,0');
  TSkill.AllIDs.Add('31221052', '100,0,0');
  TSkill.AllIDs.Add('31221002', '100,0,0');
  TSkill.AllIDs.Add('142121031', '100,0,0');
  TSkill.AllIDs.Add('5221052', '100,0,0');
  TSkill.AllIDs.Add('400051040', '100,0,0');
  TSkill.AllIDs.Add('5221026', '100,0,0');
  TSkill.AllIDs.Add('400001014', '100,0,0');
  TSkill.AllIDs.Add('61121052', '100,0,0');
  TSkill.AllIDs.Add('61121104', '100,0,0');
   }

  var ImgList: TList<TWZFile> := GetImgList('Skill');
  var RowCount := -1;
  SelectGrid.BeginUpdate;
  for var Img in ImgList do
  begin
    if not IsNumber(Img.Name[1]) then
      Continue;
    if Img.Name[1] = '0' then
      Continue;

    if not HasImgEntry('Skill/' + Img.Name + '/skill') then
      Continue;
    for var IDs in GetImgEntry('Skill/' + img.Name + '/skill/').Children do
    begin
      if IDs.Child['hit'] = nil then
        Continue;
      if IDs.Child['common'] = nil then
        Continue;
      if IDs.Child['common'].Child['lt'] = nil then
        continue;
      if IDs.Child['effect'] = nil then
        Continue;
      Inc(RowCount);
      SelectGrid.RowCount := RowCount + 1;

      var Entry := GetImgEntry('Skill/' + GetJobImg(IDs.Name) + '.img/skill/' + IDs.Name);
      var Bmp := Entry.Get2('icon').Canvas.DumpBmp;
      SelectGrid.CreateBitmap(2, RowCount, False, haCenter, vaCenter).Assign(Bmp);
      Bmp.Free;
      if HasImgEntry('String/Skill.img/' + IDs.Name) then
        SelectGrid.Cells[3, RowCount] := GetImgEntry('String/Skill.img/' + IDs.Name).Get('name', '');
      SelectGrid.Cells[1, RowCount] := Ids.Name;
    end;
  end;
  SelectGrid.SortByColumn(1);
  SelectGrid.EndUpdate;
  SkillGrid.Cells[1, 0] := 'ID';
  SkillGrid.Cells[2, 0] := 'Icon';
  SkillGrid.Cells[3, 0] := 'Name';
  SkillGrid.Cells[4, 0] := 'Hotkey';
  for var i := 0 to SkillGrid.ColCount - 1 do
    SkillGrid.CellProperties[i, 0].Alignment := taCenter;
end;

procedure TSkillForm.FormClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TSkillForm.FormCreate(Sender: TObject);
begin
  Left := ((Screen.Width - Width) div 2);
  Top := (Screen.Height - Height) div 2;
end;

procedure TSkillForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_MENU then
    Key := 0;
end;

procedure TSkillForm.SelectGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  ComBobox1.Visible := True;
  ComBobox1.Left := 312;
  ComBobox1.Top := SelectGrid.CellRect(ACol, ARow).Location.Y + 7;
  ComBobox1.ItemIndex := 32;
  ComBobox1.Enabled := True;
  SelectRow := ARow;
  ActiveControl := nil;
end;

end.

