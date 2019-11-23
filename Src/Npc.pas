unit Npc;

interface

uses
  Windows, System.Types, SysUtils, StrUtils, AsphyreSprite, Generics.Collections, WZIMGFile, Global,
  Math, Footholds, LadderRopes, ChatBalloon, MapPortal, MapleTV, AsphyreTypes, DX9Textures, Tools,
  MapleMap, WzUtils;

type
  TNpc = class(TSpriteEx)
  private
    SpriteID: string;
    Action: string;
    FTime: Integer;
    Frame: Integer;
    RandMsg: Integer;
    Counter: Integer;
    NpcName: string;
    NpcFunc: string;
    HasFunc: Boolean;
    FNameWidth: Integer;
    ImagePath: string;
    Origin: TPoint;
    FFuncWidth: Integer;
    FIDWidth: Integer;
    FH: TFoothold;
    Actions: TList<string>;
    Balloon: TChatBalloon;
    Msgs: TList<string>;
    TargetIndex: Integer;
    TargetWidth: Integer;
  public
    LocalID: string;
    procedure DoMove(const Movecount: Single); override;
    procedure DoDraw; override;
    procedure TargetEvent(Sender: TObject);
    destructor Destroy; override;
    class var
      ReDrawTarget: Boolean;
      SummonedList: TList<string>;
    class procedure Create; overload;
    class procedure Drop(ID: string; PosX, PosY: Integer; Flip: Integer);
  end;

implementation

uses
  System.Character, MainUnit;

class procedure TNpc.Drop(ID: string; PosX, PosY, Flip: Integer);
var
  c, msgX, msgY, adX, adY: Integer;
  FHs, ImagePath, LifeType: string;
  Entry, Iter2, NpcEntry: TWZIMGEntry;
  DumpList: TList<string>;
  BelowFH: TFoothold;
  Pos: TPoint;
  TargetHeight: Integer;
begin
  if ID = '9300018' then
    Exit;
  if ID = '9010088' then
    Exit;
  DumpList := TList<string>.Create;
  Randomize;

  Entry := GetImgEntry('Npc.wz/' + ID + '.img/info/link');
  NpcEntry := GetImgEntry('Npc.wz/' + ID + '.img/');
  if not DumpList.contains(ID) then
  begin
    DumpList.Add(ID);
    DumpData(NpcEntry, WzData, Images);
    if Entry <> nil then
      DumpData(NPCWZ.GetImgFile(Entry.Data + '.img').Root, WzData, Images);
  end;

  with TNpc.Create(SpriteEngine) do
  begin
    LocalID := ID;

    if Entry <> nil then
      SpriteID := Entry.Data
    else
      SpriteID := ID;

    Frame := 0;
    Actions := TList<string>.Create;

    for Iter2 in NPCWZ.GetImgFile(SpriteID + '.img').Root.Children do
      if (Iter2.Name <> 'info') and (LeftStr(Iter2.Name, 9) <> 'condition') and (Iter2.Get('0', '-1') <> '-1') then
        Actions.Add(Iter2.Name);

    if Actions.Count = 0 then
      Exit;
    Action := Actions[0];
    if Flip = 1 then
      MirrorX := True;

    NpcName := GetImgEntry('String/Npc.img/' + IDToInt(LocalID)).Get('name', '');
    NpcFunc := GetImgEntry('String/Npc.img/' + IDToInt(LocalID)).Get('func', '');
    FNameWidth := FontsAlt[0].TextWidth(NpcName);
    FFuncWidth := FontsAlt[0].TextWidth(NpcFunc);
    if NpcFunc <> '' then
      HasFunc := True;

    FIDWidth := FontsAlt[0].TextWidth('ID: ' + LocalID);
    ImageLib := Images;
    ImagePath := 'Npc.wz/' + SpriteID + '.img/' + Action + '/0';
    ImageEntry := WzData[ImagePath];
    Pos := TFootholdTree.This.FindBelow(Point(Round(PosX), Round(PosY - 3)), BelowFH);
    X := Pos.X;
    Y := Pos.Y;
    FH := BelowFH;
    Z := FH.Z * 100000 + 7000;
    Width := PatternWidth;
    Height := PatternHeight;
    TruncMove := True;

    Msgs := TList<string>.Create;
    for Iter2 in StringWZ.GetImgFile('Npc.img').Root.Get(IDToInt(SpriteID)).Children do
      if IsNumber(Iter2.Name, 2) then
        Msgs.Add(Iter2.Data);

    if Msgs.Count > 0 then
    begin
      Balloon := TChatBalloon.Create(SpriteEngine);
      Balloon.SetStyle(0);
      Balloon.X := X;
      Balloon.Y := Y - Height - 20;
      Balloon.Z := Z + 1;
    end;
    Counter := Random(750);

    Entry := NPCWZ.GetImgFile(SpriteID + '.img').Root;
    if Entry.Get('info/MapleTV', '0') = 1 then
    begin
      msgX := Entry.Get('info/MapleTVmsgX', '0');
      msgY := Entry.Get('info/MapleTVmsgY', '0');
      adX := Entry.Get('info/MapleTVadX', '0');
      adY := Entry.Get('info/MapleTVadY', '0');
      TMapleTV.CreateSelf(Round(X), Round(Y), msgX, msgY, adX, adY, Z);
    end;

    if ImageEntry.Get('origin') <> nil then
      Origin := ImageEntry.Get('origin').Vector;
    Offset.X := -Origin.X;
    Offset.Y := -Origin.Y;
    if HasFunc then
      TargetHeight := 40
    else
      TargetHeight := 20;
    TargetWidth := Max(FFuncWidth, FNameWidth) + 5;
    TargetIndex := GameTargets.Add(1, TargetWidth, TargetHeight, apf_A8R8G8B8, True, True);
    GameDevice.RenderTo(TargetEvent, 0, True, GameTargets[TargetIndex]);
  end;
  DumpList.Free;
end;

class procedure TNpc.Create;
var
  Iter, Iter2: TWZIMGEntry;
begin
  for Iter in TMap.ImgFile.Child['life'].Children do
  begin
    if Iter.Child['type'] <> nil then
    begin
      if Iter.Get('type', '') = 'n' then
        TNpc.Drop(Iter.Get('id', ''), Iter.Get('x', ''), Iter.Get('cy', ''), Iter.Get('f', '0'));
    end
    else
    begin
      for Iter2 in Iter.Children do
        if Iter2.Get('type', '') = 'n' then
          TNpc.Drop(Iter2.Get('id', ''), Iter2.Get('x', ''), Iter2.Get('cy', ''), Iter2.Get('f', '0'));
    end;
  end;
end;

procedure TNpc.DoMove(const Movecount: Single);
var
  AnimDelay: Integer;
begin
  inherited;
  if ReDrawTarget then
    GameDevice.RenderTo(TargetEvent, 0, True, GameTargets[TargetIndex]);
  ImagePath := 'Npc.wz/' + SpriteID + '.img/' + Action + '/' + Frame.ToString;
  ImageEntry := WzData[ImagePath];
  AnimDelay := WzData[ImagePath].Get('delay', '100');

  FTime := FTime + 17;
  if FTime > AnimDelay then
  begin
    Frame := Frame + 1;
    if not WzData.ContainsKey('Npc.wz/' + SpriteID + '.img/' + Action + '/' + Frame.ToString) then
    begin
      Frame := 0;
      if Actions.Count > 1 then
      begin
        if Random(2) = 0 then
          Action := Actions[Random(Actions.Count)];
      end;
    end;
    FTime := 0;
  end;

  if ImageEntry.Get('origin') <> nil then
    Origin := ImageEntry.Get('origin').Vector;
  case MirrorX of
    True:
      Offset.X := Origin.X - PatternWidth;
    False:
      Offset.X := -Origin.X;
  end;
  Offset.Y := -Origin.Y;
end;

procedure TNpc.DoDraw;
var
  WX, WY, NamePos, IDPos: Integer;
begin
  if ReDrawTarget then
    ReDrawTarget := False;
  WX := Round(X - Engine.WorldX);
  WY := Round(Y - Engine.WorldY);
  if HasFunc then
    NamePos := WX - TargetWidth div 2
  else
    NamePos := WX - FNameWidth div 2;
  // FuncPos := WX - FNameWidth div 2;
  IDPos := WX - FIDWidth div 2;
  if TMap.ShowNPC then
  begin
    inherited;
    GameCanvas.Draw(GameTargets[TargetIndex], NamePos - 3, WY + 2, 1, False, 255, 255, 255, 255);
    Inc(Counter);
    if Counter > 700 then
    begin
      Counter := 0;
      RandMsg := Random(Msgs.Count);
    end;
    if Msgs.Count > 0 then
    begin
      if (Counter > 350) and (Counter < 700) then
        Balloon.Msg := Msgs[RandMsg]
      else
      begin
        Balloon.Msg := '';
      end;
    end;
  end;

  if TMap.ShowID then
  begin
    GameCanvas.FillRect(IDPos - 3, WY + 18, FIDWidth + 5, 15, cRGB1(0, 0, 0, 160));
    GameCanvas.Flush;
    FontsAlt[0].TextOut('ID: ' + SpriteID, IDPos, WY + 18, cRGB1(255, 255, 0));
  end;
end;

procedure TNpc.TargetEvent(Sender: TObject);
var
  NameMiddle, FuncMiddle: Integer;
begin
  if HasFunc then
  begin
    NameMiddle := (TargetWidth - FNameWidth) div 2;
    FuncMiddle := (TargetWidth - FFuncWidth) div 2;
    GameCanvas.FillRect(NameMiddle - 3, 0, FNameWidth + 5, 15, cRGB1(0, 0, 0, 190));
    GameCanvas.FillRect(FuncMiddle - 3, 17, FFuncWidth + 5, 15, cRGB1(0, 0, 0, 190));
    GameCanvas.Flush;
    FontsAlt[0].TextOut(NpcName, NameMiddle, 1, clYellow1);
    FontsAlt[0].TextOut(NpcFunc, FuncMiddle, 18, clYellow1);
  end
  else
  begin
    GameCanvas.FillRect(0, 0, FNameWidth + 5, 15, cRGB1(0, 0, 0, 190));
    GameCanvas.Flush;
    FontsAlt[0].TextOut(NpcName, 3, 1, clYellow1);
  end;
end;

destructor TNpc.Destroy;
begin
  Msgs.Free;
  Actions.Free;
  inherited;
end;

initialization
  TNpc.SummonedList := TList<string>.Create;

finalization
  TNpc.SummonedList.Free;

end.

