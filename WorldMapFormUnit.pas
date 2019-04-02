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
  HasLoad := True;
  var WorldMapGrid := MainForm.WorldMapGrid;
  WorldMapGrid.ColWidths[0] := 0;
  WorldMapGrid.ColWidths[1] := 180;

  var Dict := TDictionary<string, string>.Create;
  Dict.Add('BWorldMap.img', 'Maple World');
  Dict.Add('GWorldMap.img', 'Grandis');
  Dict.Add('MWorldMap.img', 'Mirror World');
  Dict.Add('SWorldMap.img', 'Star Planet');
  Dict.Add('WorldMap.img', 'Maple World');
  Dict.Add('WorldMap000.img', 'Maple Island');
  Dict.Add('WorldMap010.img', 'Victoria Island');
  Dict.Add('WorldMap0101.img', 'Kerning Tower');
  Dict.Add('WorldMap011.img', 'Nautilus');
  Dict.Add('WorldMap012.img', 'Sleepywood');
  Dict.Add('WorldMap0121.img', 'Dark World Tree');
  Dict.Add('WorldMap015.img', 'WorldMap015 ');
  Dict.Add('WorldMap016.img', 'Shinsoo International School');
  Dict.Add('WorldMap017.img', 'Elinel Fairy Academy');
  Dict.Add('WorldMap018.img', 'Gold Beach');
  Dict.Add('WorldMap019.img', 'Mushroom Castle');
  Dict.Add('WorldMap020.img', 'EI Nath Mts.');
  Dict.Add('WorldMap021.img', 'Dead Mine');
  Dict.Add('WorldMap022.img', 'lion King Castle');
  Dict.Add('WorldMap030.img', 'Ludus Lake');
  Dict.Add('WorldMap031.img', 'ClockTower LowerFloor');
  Dict.Add('WorldMap032.img', 'Ellin Forest ');
  Dict.Add('WorldMap033.img', 'Fantastic Theme World');
  Dict.Add('WorldMap034.img', 'Korea Folk Town');
  Dict.Add('WorldMap035.img', 'Omega Sector');
  Dict.Add('WorldMap040.img', 'Aqua Road');
  Dict.Add('WorldMap041.img', 'Twisted Aqua Road');
  Dict.Add('WorldMap050.img', 'Minar Forest');
  Dict.Add('WorldMap051.img', 'Neo City ');
  Dict.Add('WorldMap052.img', 'Stone Colossus');
  Dict.Add('WorldMap060.img', 'Mu Lung garden');
  Dict.Add('WorldMap061.img', 'Golden Temple ');
  Dict.Add('WorldMap070.img', 'Nihal Desert');
  Dict.Add('WorldMap071.img', 'Ancient city Azwan');
  Dict.Add('WorldMap072.img', 'Nihal Desert Trade Zone');
  Dict.Add('WorldMap080.img', 'Temple of Time');
  Dict.Add('WorldMap081.img', 'Gate of Future');
  Dict.Add('WorldMap082.img', 'Arcane River');
  Dict.Add('WorldMap0821.img', 'Vanishing Journey');
  Dict.Add('WorldMap0822.img', 'Chu Chu Island');
  Dict.Add('WorldMap08221.img', 'lonely Chu Chu Island');
  Dict.Add('WorldMap0823.img', 'Lachelein the Dreaming City');
  Dict.Add('WorldMap0824.img', 'Arcane');
  Dict.Add('WorldMap0825.img', 'Morass Swamp of Memory');
  Dict.Add('WorldMap0826.img', 'Esfera ');
  Dict.Add('WorldMap090.img', 'Ereve');
  Dict.Add('WorldMap100.img', 'Rien');
  Dict.Add('WorldMap101.img', 'Riena Strait');
  Dict.Add('WorldMap110.img', 'Edelstein');
  Dict.Add('WorldMap111.img', 'Scrapyard');
  Dict.Add('WorldMap120.img', 'Crystal Garden');
  Dict.Add('WorldMap130.img', 'Pantheon');
  Dict.Add('WorldMap140.img', 'Heliseum');
  Dict.Add('WorldMap141.img', 'Tyrant Castle');
  Dict.Add('WorldMap143.img', 'Masteria');
  Dict.Add('WorldMap152.img', '皇陵之巔');
  Dict.Add('WorldMap153.img', 'Commerci');
  Dict.Add('WorldMap154.img', 'Arboren');
  Dict.Add('WorldMap155.img', 'Meso gear');
  Dict.Add('WorldMap160.img', 'Zipangu');
  Dict.Add('WorldMap161.img', 'mushroom Shrine');
  Dict.Add('WorldMap163.img', 'Ninja Castle');
  Dict.Add('WorldMap164.img', '未來 ');
  Dict.Add('WorldMap167.img', '東京 ');
  Dict.Add('WorldMap169.img', 'Blackgate City');
  Dict.Add('WorldMap170.img', 'Kiitias');
  Dict.Add('WorldMap171.img', 'Momijigaoka');
  Dict.Add('WorldMap172.img', 'Spring Vally');
  Dict.Add('WorldMap173.img', 'The afterlands');
  Dict.Add('WorldMap174.img', 'Eluna');
  Dict.Add('WorldMap175.img', 'Beautyroid');
  Dict.Add('WorldMap176.img', 'Abrup Basin');
  Dict.Add('WorldMap180.img', 'Fox point Village');
  Dict.Add('WorldMap181.img', 'Fox Valley');
  Dict.Add('WorldMap190.img', 'Savage Terminal');
  Dict.Add('WorldMap191.img', 'Sanctuary ');
  Dict.Add('WorldMap200.img', 'Verdel');
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

