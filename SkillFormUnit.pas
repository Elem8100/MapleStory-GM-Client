unit SkillFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, AdvObj, BaseGrid, AdvGrid, Vcl.StdCtrls,
  WZIMGFile, Vcl.Mask, scControls, scAdvancedControls;

type
  TSkillForm = class(TForm)
    HotKeySetGrid: TAdvStringGrid;
    SkillGrid: TAdvStringGrid;
    SelectGrid: TAdvStringGrid;
    Label1: TLabel;
    ComBobox1: TscAdvancedComboEdit;
    procedure FormShow(Sender: TObject);
    procedure SelectGridClickCell(Sender: TObject; ARow, ACol: Integer);
    procedure ComBobox1CloseUp(Sender: TObject);
  private
    HasLoaded: Boolean;
    IDs: array of string;
    SelectRow: Integer;
    Entry: TWZIMGEntry;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SkillForm: TSkillForm;

implementation

uses
  Skill, WzUtils,AsphyreSprite,Global;
{$R *.dfm}

procedure TSkillForm.ComBobox1CloseUp(Sender: TObject);
begin
  if ComBobox1.ItemIndex = 32 then
    Exit;

  for var r := SkillGrid.RowCount-1 downto 1 do
  begin
    if SkillGrid.cells[4, r] = ComBobox1.Text then
      SkillGrid.RemoveRows(r, 1);

    if SkillGrid.cells[1, r] = HotKeySetGrid.Cells[1, 1] then
      SkillGrid.RemoveRows(r, 1);

  end;

  SkillGrid.Cells[1, SkillGrid.RowCount] := SelectGrid.Cells[1, SelectRow];
  var Bmp := Entry.Get2('icon').Canvas.DumpBmp;
  var RowCount := SkillGrid.RowCount;
  SkillGrid.CreateBitmap(2, RowCount, False, haCenter, vaCenter).Assign(Bmp);
  SkillGrid.Cells[3, RowCount] := SelectGrid.Cells[2, SelectRow];
  SkillGrid.Cells[4, RowCount] := ComBobox1.Text;
  SkillGrid.RowCount := SkillGrid.RowCount + 1;
  Bmp.Free;

  //ComBobox1.ClearSelection;
  TSkill.HotKeyList.Clear;
  for var i := 1 to SkillGrid.RowCount - 1 do
  begin
    var KeyName := SkillGrid.cells[4, i];
    var SkillID := SkillGrid.cells[1, i];
    if Length(KeyName) > 1 then
    begin
      case KeyName[1] of
        'i':
          TSkill.HotKeyList.AddOrSetValue(45, SkillID);
        'h':
          TSkill.HotKeyList.AddOrSetValue(36, SkillID);
        'P':
          if KeyName[3] = 'U' then
            TSkill.HotKeyList.AddOrSetValue(33, SkillID)
          else
            TSkill.HotKeyList.AddOrSetValue(34, SkillID);
        'd':
          TSkill.HotKeyList.AddOrSetValue(46, SkillID);
        'e':
          TSkill.HotKeyList.AddOrSetValue(35, SkillID);
      end;
    end
    else
      TSkill.HotKeyList.AddOrSetValue(Ord(KeyName[1]), SkillID);
  end;

end;

procedure TSkillForm.FormShow(Sender: TObject);
begin
  if HasLoaded then
    Exit;
  HasLoaded := True;
  TSkill.Skill:= TSkill.Create(SpriteEngine);


  IDs := ['2321008', '2121007', '2221007', '5121001', '2301005', '1121008', '21120005', '21110006',
    '12111005', '3111003'];
  var RowCount := -1;
  SelectGrid.BeginUpdate;
  for var i in IDs do
  begin
    Inc(RowCount);
    SelectGrid.RowCount := RowCount + 1;
    SelectGrid.Cells[1, RowCount] := i;
    SelectGrid.Cells[2, RowCount] := GetImgEntry('String.wz/Skill.img/' + i).Get('name', '');
  end;
  SelectGrid.SortByColumn(1);
  SelectGrid.EndUpdate;

  {
   LoadSkill('2321008');
    LoadSkill('2121007');
    LoadSkill('2221007');
    LoadSkill('5121001');
    LoadSkill('2301005');
    LoadSkill('1121008');
    LoadSkill('21120005');
    LoadSkill('21110006');
    LoadSkill('12111005');
    LoadSkill('3111003');
    }
end;

function GetJobID(ID: string): string;
begin
  Result := (ID.ToInteger div 10000).ToString;
end;

procedure TSkillForm.SelectGridClickCell(Sender: TObject; ARow, ACol: Integer);
begin
  ComBobox1.Left:=273;
  ComBobox1.Top := SelectGrid.CellRect(ACol,ARow).Location.Y+2;

  ComBobox1.ItemIndex := 32;
  ComBobox1.Enabled := True;
  SelectRow := ARow;
  var ID := SelectGrid.Cells[1, ARow];
  Entry := GetImgEntry('Skill.wz/' + GetJobID(ID) + '.img/skill/' + ID);
  var Bmp := Entry.Get2('icon').Canvas.DumpBmp;
  HotKeySetGrid.Cells[1, 1] := SelectGrid.Cells[1, ARow];
  HotKeySetGrid.CreateBitmap(2, 1, False, haCenter, vaCenter).Assign(Bmp);
  HotKeySetGrid.Cells[3, 1] := SelectGrid.Cells[2, ARow];
  Bmp.Free;
end;

end.

