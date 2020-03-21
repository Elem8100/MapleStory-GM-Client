unit WorldMapFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Generics.collections;

type
  TWorldMapForm = class(TForm)
    Image1: TImage;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    HasLoad: Boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WorldMapForm: TWorldMapForm;

implementation

{$R *.dfm}
uses
  MainUnit, WZIMGFile, WZDirectory, WzUtils, Global;

procedure TWorldMapForm.FormActivate(Sender: TObject);
begin
  if HasLoad then
    Exit;
  WorldMapForm.Top:=-2500;
  HasLoad := True;
  var WorldMapGrid := MainForm.WorldMapGrid;
  WorldMapGrid.ColWidths[0] := 0;
  WorldMapGrid.ColWidths[1] := 180;

  var Dict := TDictionary<string, string>.Create;
  //Dict.Add('BWorldMap.img', '楓之谷世界');
  Dict.Add('GWorldMap.img', '格蘭蒂斯');
  Dict.Add('MWorldMap.img', '鏡子世界');
  Dict.Add('SWorldMap.img', '新星世界');
  Dict.Add('WorldMap.img', '楓之谷世界');
  Dict.Add('WorldMap000.img', '楓之島 ');
  Dict.Add('WorldMap010.img', '維多利亞島');
  Dict.Add('WorldMap0101.img', '星光之塔');
  Dict.Add('WorldMap011.img', '鯨魚號');
  Dict.Add('WorldMap012.img', '奇幻村');
  Dict.Add('WorldMap0121.img', '墮落的世界樹');
  Dict.Add('WorldMap015.img', 'WorldMap015 ');
  Dict.Add('WorldMap016.img', '皇家神獸學院');
  Dict.Add('WorldMap017.img', '妖精學園艾里涅');
  Dict.Add('WorldMap018.img', '黃金海岸');
  Dict.Add('WorldMap019.img', '菇菇城堡');
  Dict.Add('WorldMap020.img', '冰原雪域山脈');
  Dict.Add('WorldMap021.img', '廢棄礦坑');
  Dict.Add('WorldMap022.img', '獅子王之城');
  Dict.Add('WorldMap030.img', '路德斯湖');
  Dict.Add('WorldMap031.img', '時間通道');
  Dict.Add('WorldMap032.img', '艾靈森林 ');
  Dict.Add('WorldMap033.img', '夢幻主題提公園 ');
  Dict.Add('WorldMap034.img', '童話村');
  Dict.Add('WorldMap035.img', '地球防衛本部');
  Dict.Add('WorldMap040.img', '水世界');
  Dict.Add('WorldMap041.img', '水世界2');
  Dict.Add('WorldMap050.img', '米納爾森林');
  Dict.Add('WorldMap051.img', '奈歐市 ');
  Dict.Add('WorldMap052.img', '岩壁巨人克洛宿斯');
  Dict.Add('WorldMap060.img', '武陵桃園');
  Dict.Add('WorldMap061.img', '黃金寺廟 ');
  Dict.Add('WorldMap070.img', '納希沙漠 ');
  Dict.Add('WorldMap071.img', '古城阿斯旺');
  Dict.Add('WorldMap072.img', '納希沙漠自由貿易地帶');
  Dict.Add('WorldMap080.img', '時間神殿');
  Dict.Add('WorldMap081.img', ' 未來之門');
  Dict.Add('WorldMap082.img', '奧術之河');
  Dict.Add('WorldMap0821.img', '消逝的旅途');
  Dict.Add('WorldMap0822.img', '啾啾艾爾蘭');
  Dict.Add('WorldMap08221.img', '寂寞的啾啾島');
  Dict.Add('WorldMap0823.img', '夢之都拉契爾恩');
  Dict.Add('WorldMap0824.img', '神秘森林阿爾卡娜 ');
  Dict.Add('WorldMap0825.img', '記憶沼澤魔菈拉斯');
  Dict.Add('WorldMap0826.img', '初始之海艾斯佩拉 ');
  Dict.Add('WorldMap090.img', '耶雷弗');
  Dict.Add('WorldMap100.img', '瑞恩');
  Dict.Add('WorldMap101.img', '里艾那海峽');
  Dict.Add('WorldMap110.img', ' 埃德爾斯坦');
  Dict.Add('WorldMap111.img', '機械墳場');
  Dict.Add('WorldMap120.img', 'Crystal Garden');
  Dict.Add('WorldMap130.img', '萬神殿');
  Dict.Add('WorldMap140.img', '赫力席母');
  Dict.Add('WorldMap141.img', '暴君之城');
  Dict.Add('WorldMap143.img', '克拉奇亞');
  Dict.Add('WorldMap152.img', '皇陵之巔');
  Dict.Add('WorldMap153.img', '克梅勒茲');
  Dict.Add('WorldMap154.img', '樹木村莊');
  Dict.Add('WorldMap155.img', '楓幣機器');
  Dict.Add('WorldMap160.img', '日本區');
  Dict.Add('WorldMap161.img', '菇菇神社');
  Dict.Add('WorldMap163.img', '楓城');
  Dict.Add('WorldMap164.img', '未來 ');
  Dict.Add('WorldMap167.img', '東京 ');
  Dict.Add('WorldMap169.img', '黑扉城');
  Dict.Add('WorldMap170.img', '克里提亞斯');
  Dict.Add('WorldMap171.img', '楓葉丘陵');
  Dict.Add('WorldMap172.img', 'Spring Vally');
  Dict.Add('WorldMap173.img', 'The afterlands');
  Dict.Add('WorldMap174.img', '伊露納');
  Dict.Add('WorldMap175.img', 'Beautyroid');
  Dict.Add('WorldMap176.img', '阿爾布盆地 ');
  Dict.Add('WorldMap180.img', '尖耳狐狸村');
  Dict.Add('WorldMap181.img', '狐狸峽谷 ');
  Dict.Add('WorldMap190.img', '蠻荒終結站');
  Dict.Add('WorldMap191.img', '亞修羅姆 ');
  Dict.Add('WorldMap200.img', '貝勒帝');
  Dict.Add('WorldMapJP.img', '維多利亞島 ');
  Dict.Add('WorldMapTW.img', '台灣');

  var Row := -1;
  for var Iter in TWZDirectory(MapWz.Root.Entry['WorldMap']).Files do
  begin
    Inc(Row);
    WorldMapGrid.RowCount := Row + 1;
    WorldMapGrid.Cells[0, Row] := Iter.Name;
    if Dict.ContainsKey(Iter.Name) then
      WorldMapGrid.Cells[1, Row] := Dict[Iter.Name]
    else
      WorldMapGrid.Cells[1, Row] := Iter.Name;
  end;
  WorldMapGrid.SortByColumn(0);
  Dict.Free;
end;

procedure TWorldMapForm.FormClick(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TWorldMapForm.FormCreate(Sender: TObject);
begin
  Left := (Screen.Width - Width) div 2;
  Top := (Screen.Height - Height) div 2;
end;

procedure TWorldMapForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_MENU then
    Key := 0;
end;

end.

