unit SelectFolderFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FolderDialog, Vcl.Grids, AdvObj, BaseGrid, AdvGrid,
  Vcl.StdCtrls, Generics.Collections;

type
  TSelectFolderForm = class(TForm)
    FolderDialog1: TFolderDialog;
    Button1: TButton;
    Grid: TAdvStringGrid;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GridButtonClick(Sender: TObject; ACol, ARow: Integer);
    procedure FormShow(Sender: TObject);
  private
    DirList: TList<string>;
    procedure SelectFolder(FolderPath: string);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SelectFolderForm: TSelectFolderForm;

implementation

uses
  MainUnit, WZArchive, WZDirectory, WZIMGFile, KeyHandler, Global, Npc, MapleMap, WzUtils,
  UI.Utils, Skill, MapleEffect, TamingMob, StrUtils;

{$R *.dfm}

procedure TSelectFolderForm.FormCreate(Sender: TObject);
begin
  DirList := TList<string>.Create;
  Left := (Screen.Width - Width) div 2-150;
  Top := (Screen.Height - Height) div 2-100;
end;

procedure TSelectFolderForm.FormDestroy(Sender: TObject);
begin
  DirList.Free;
end;

procedure TSelectFolderForm.FormShow(Sender: TObject);
begin
  Grid.LoadFromBinFile(ExtractFilePath(ParamStr(0)) + 'FolderList.dat');
  for var i := 0 to Grid.RowCount - 1 do
  begin
    Grid.CellProperties[1, i].FontSize := 18;
    if Grid.Cells[0, i] <> '' then
      DirList.Add(Grid.Cells[0, i]);
  end;
  Grid.RowCount := 10;
end;

procedure TSelectFolderForm.GridButtonClick(Sender: TObject; ACol, ARow: Integer);
begin
  SelectFolder(Grid.Cells[0, ARow]);
end;

procedure TSelectFolderForm.SelectFolder(FolderPath: string);
var
  ID, MapName, StreetName, Path: string;
  Iter, Iter2: TWZIMGEntry;
  Dir: TWZDirectory;
  Img: TWZFile;
  RowCount: Integer;
begin
  if FileExists(FolderPath + '\String.wz') then
  begin
    SelectFolderForm.Close;
    if not DirList.Contains(FolderPath) then
      Dirlist.Insert(0, FolderPath);
    if DirList.Count > 10 then
      DirList.Delete(10);
    var FileName := ExtractFilePath(ParamStr(0)) + 'FolderList.dat';

    for var i := 0 to dirlist.Count - 1 do
    begin
      Grid.AddButton(1, i, 55, 22, 'Load', hacenter, vacenter);
      Grid.Cells[0, i] := DirList[i];
    end;
    Grid.SaveToBinFile(FileName);
    MainForm.Grid.Clear;

    if MapWz <> nil then
      FreeAndNil(MapWz);
    if Map2Wz <> nil then
      FreeAndNil(Map2Wz);

    if MobWZ <> nil then
      FreeAndNil(MobWZ);
    if Mob2WZ <> nil then
      FreeAndNil(Mob2WZ);
    if Mob001WZ <> nil then
      FreeAndNil(Mob001WZ);
    if NPCWZ <> nil then
      FreeAndNil(NPCWZ);
    if StringWZ <> nil then
      FreeAndNil(StringWZ);
    if SoundWZ <> nil then
      FreeAndNil(SoundWZ);

    with MainForm.Grid.Canvas do
    begin
      Font.Size := 20;
      Font.Color := clBlack;
      Brush.Color := clGrayText;
      TextOut(20, 100, 'Loading...');
    end;
    WzPath := FolderPath;
    StringWZ := TWZArchive.Create(FolderPath + '\String.wz');

    if StringWZ.GetImgFile('Mob.img').Root.Get('100100/name', '') = 'Snail' then
      TNpc.FontSize := 11 //GMS
    else
      TNpc.FontSize := 12; //TMS
    if StringWZ.GetImgFile('Mob.img').Root.Get('100100/name', '') = '달팽이' then
      IsKMS := True;

    MapWz := TWZArchive.Create(FolderPath + '\Map.wz');
    if FileExists(FolderPath + '\Map2.wz') then
      Map2Wz := TWZArchive.Create(FolderPath + '\Map2.wz');
    if FileExists(FolderPath + '\Map001.wz') then
      Map001Wz := TWZArchive.Create(FolderPath + '\Map001.wz');

    MobWZ := TWZArchive.Create(FolderPath + '\Mob.wz');
    if FileExists(FolderPath + '\Mob2.wz') then
      Mob2WZ := TWZArchive.Create(FolderPath + '\Mob2.wz');
    if FileExists(FolderPath + '\Mob001.wz') then
      Mob001WZ := TWZArchive.Create(FolderPath + '\Mob001.wz');
    if FileExists(FolderPath + '\Map002.wz') then
    begin
      Map002Wz := TWZArchive.Create(FolderPath + '\Map002.wz');
      TMap.Has002Wz := True;
    end;

    NPCWZ := TWZArchive.Create(FolderPath + '\Npc.wz');
    SoundWZ := TWZArchive.Create(FolderPath + '\Sound.wz');
    if FileExists(FolderPath + '\Sound2.wz') then
      Sound2Wz := TWZArchive.Create(FolderPath + '\Sound2.wz');

    CharacterWZ := TWZArchive.Create(FolderPath + '\Character.wz');
    BaseWZ := TWZArchive.Create(FolderPath + '\Base.wz');
    UIWZ := TWZArchive.Create(FolderPath + '\UI.wz');
    if HasImgFile('UI.wz/UIWindow4.img') then
      UIVersion := -1;
    ReactorWz := TWZArchive.Create(FolderPath + '\Reactor.wz');
    EffectWz := TWZArchive.Create(FolderPath + '\Effect.wz');
    SkillWZ := TWZArchive.Create(FolderPath + '\Skill.wz');
    if FileExists(FolderPath + '\Skill001.wz') then
    begin
      Skill001Wz := TWZArchive.Create(FolderPath + '\Skill001.wz');
      TSkill.Has001Wz := True;
    end;
    ItemWZ := TWZArchive.Create(FolderPath + '\Item.wz');
    MorphWz := TWZArchive.Create(FolderPath + '\Morph.wz');
    EtcWZ := TWZArchive.Create(FolderPath + '\Etc.wz');

    TSetEffect.LoadList;
    TItemEffect.LoadList;
    TTamingMob.LoadSaddleList;

    var MapNameRec: TMapNameRec;
    for Iter in StringWZ.GetImgFile('Map.img').Root.Children do
      for Iter2 in Iter.Children do
      begin
        ID := Add9(Iter2.Name);
        MapNameRec.ID := ID;
        MapNameRec.StreetName := Iter2.Get('streetName', '');
        MapNameRec.MapName := Iter2.Get('mapName', '');
        TMap.MapNameList.AddOrSetValue(ID, MapNameRec);
      end;
    RowCount := -1;
    MainForm.Grid.BeginUpdate;

    var MapDir: TWZDirectory;

    if TMap.Has002Wz then
      MapDir := TWZDirectory(Map002Wz.Root.Entry['Map'])
    else
      MapDir := TWZDirectory(MapWz.Root.Entry['Map']);

    for Dir in MapDir.SubDirs do
      for Img in Dir.Files do
      begin
        ID := LeftStr(Img.Name, 9);
        if TMap.MapNameList.ContainsKey(ID) then
        begin
          Inc(RowCount);
          MainForm.Grid.RowCount := RowCount + 1;
          MainForm.Grid.Cells[0, RowCount] := ID + '  ' + TMap.MapNameList[ID].MapName;
        end;
      end;

    MainForm.Grid.RemoveDuplicates(0, True);
    MainForm.Grid.SortByColumn(0);
    MainForm.Grid.RemoveRows(0, 1);
    MainForm.Grid.EndUpdate;
    MainForm.LoadMapButton.Enabled := True;
    MainForm.SearchMapEdit.Enabled := True;
    MainForm.PageControl1.Enabled := True;
    MainForm.Grid.Enabled := True;

  end
  else
  begin
    ShowMessage('Wrong folder, WZ file could not be found');
  end;
end;

procedure TSelectFolderForm.Button1Click(Sender: TObject);
begin
  if FolderDialog1.Execute then
  begin
    SelectFolder(FolderDialog1.Directory);
  end;
end;

end.

