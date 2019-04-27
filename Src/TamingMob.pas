unit TamingMob;

interface

uses
  Windows, SysUtils, StrUtils, AsphyreSprite, Generics.Collections, WZIMGFile,
  Classes, Global, WzUtils;

type
  TTamingMob = class(TSpriteEx)
  public
    UpPath: string;
    Path, ID: string;
    State: string;
    Frame: Integer;
    ImageNum: string;
    FTime: Integer;
    Delay: Integer;
    Flip: Integer;
    Origin: TPoint;
    FixedImageNum: Boolean;
    IsSaddle: Boolean;
    PartIndex: string;
    class var
      Entry: TWZIMGEntry;
      IsUse: Boolean;
      Navel: TPoint;
      SaddleList: TDictionary<string, string>;
      ImageNumList: TDictionary<string, string>;
      Data: TDictionary<string, TWZIMGEntry>;
      CharacterAction: string;
      IsChairTaming: Boolean;
    procedure DoMove(const MoveCount: Single); override;
    class procedure LoadSaddleList;
    class procedure Delete;
    class procedure CreateSprites;
    class procedure CreateSaddle(ID: string);
    class procedure CreateTaming(ID: string);
    class procedure Create(ID: string); overload;
  end;

implementation

uses
  MapleCharacter, Footholds, ChairformUnit, AvatarUnit, MapleChair,
  AsphyreKeyboard, DirectInput, WZDirectory,MapleEffect;

function IDToInt(ID: string): string;
begin
  var S := ID.ToInteger;
  Result := S.ToString;
end;

class procedure TTamingMob.LoadSaddleList;
begin
  for var Img in TWZDirectory(CharacterWZ.Root.Entry['TamingMob']).Files do
    if LeftStr(Img.Name, 4) = '0191' then
      for var Iter in GetImgEntry('Character.wz/TamingMob/' + Img.Name + '/').Children do
        if Iter.Name[1] in ['0'..'9'] then
          SaddleList.AddOrSetValue('0' + Iter.Name, LeftStr(Img.Name, 8));

end;

class procedure TTamingMob.Delete;
begin
  TTamingMob.IsUse := False;

  for var Iter in SpriteEngine.SpriteList do
    if Iter is TTamingMob then
      Iter.Dead;

  for var Iter in EquipImages.Keys do
    if LeftStr(Iter.GetPath, 26)= 'Character.wz/TamingMob/019'  then
    begin
       EquipImages.Remove(Iter);
       EquipData.Remove(Iter.GetPath);
    end;

  TTamingMob.Navel.X := 0;
  TTamingMob.Navel.Y := 0;
end;

class procedure TTamingMob.CreateSprites;
begin
  DumpData(Entry, EquipData, EquipImages);
  var _State:string;
  var _Frame:string;

  if IsChairTaming then
  begin
    _State:='sit';
    _Frame:='0';
  end
  else
  begin
    _State:='walk1';
    _Frame:='1';
  end;

  for var Iter in EquipData[Entry.GetPath].Children do
    for var Iter2 in EquipData[Iter.GetPath].Children do
      if (Iter2.Name[1] in ['0'..'9']) then
      begin
        var Index := -1;
        for var Iter3 in EquipData[Iter2.GetPath].Children do
          if (Iter3.Name[1] in ['0'..'9']) and ((Iter3.DataType = mdtCanvas) or (Iter3.DataType = mdtUOL)) then
          begin
            Inc(Index);
            ImageNumList.AddOrSetValue(Entry.GetPath + '/' + Iter.Name + '/' + Iter2.Name + '/' + Index.ToString, Iter3.Name);
            if (Iter.Name = _State) and (Iter2.Name = _Frame) then
              with TTamingMob.Create(SpriteEngine) do
              begin
                ImageLib := EquipImages;
                TruncMove := True;
                Tag := 1;
                PartIndex := Index.ToString;
                State := Iter.Name;
                Frame := Iter2.Name.ToInteger;
                ImageNum := Iter3.Name;
                ID := LeftStr(Entry.Name, 8);
                UpPath := Entry.GetPath;
                ImageEntry := EquipData[Iter3.GetPath];
                if LeftStr(Entry.Parent.Name, 4) = '0191' then
                  IsSaddle := True;
                if (Iter.Name = 'walk1') and (Iter2.Name = '1') and (Iter2.Get('0') = nil) then
                  FixedImageNum := True;
                if (Length(Iter3.Name) >= 3) then
                  FixedImageNum := True;
              end;

          end;
      end;

end;

class procedure TTamingMob.CreateSaddle(ID: string);
begin
  Data.Clear;
  if SaddleList.ContainsKey(ID) then
    Entry := GetImgEntry('Character.wz/TamingMob/' + SaddleList[ID] + '.img/' + IDToInt(ID))
  else
    Exit;
    //add saddle delay
  for var Iter in GetImgEntry('Character.wz/TamingMob/' + ID + '.img/').Children do
    if Iter.Name <> 'info' then
      for var Iter2 in Iter.Children do
        Data.AddOrSetValue(Entry.GetPath + '/' + Iter.Name + '/' + Iter2.Name, Iter2.Get2('delay'));

  CreateSprites;
end;

class procedure TTamingMob.CreateTaming(ID: string);
begin
  Entry := GetImgEntry('Character.wz/TamingMob/' + ID + '.img/');
  CreateSprites;
end;

class procedure TTamingMob.Create(ID: string);
begin
  TTamingMob.CharacterAction := 'sit';
  ImageNumList.Clear;
  CreateSaddle(ID);
  CreateTaming(ID);
end;

procedure TTamingMob.DoMove(const MoveCount: Single);
begin
  inherited;
  if FixedImageNum then
    if ImageNumList.ContainsKey(UpPath + '/' + State + '/' + Frame.ToString + '/' + PartIndex) then
      ImageNum := ImageNumList[UpPath + '/' + State + '/' + Frame.ToString + '/' + PartIndex];

  if HasEntryE(UpPath + '/' + State + '/' + Frame.ToString + '/' + ImageNum) then
  begin
    Path := UpPath + '/' + State + '/' + Frame.ToString + '/' + ImageNum;
    ImageEntry := EquipData[Path];
    Visible := True;
  end
  else
  begin
    if (State='rope') or (State='ladder') then
     Visible:=False;
  end;

  if HasEntryE(UpPath + '/' + State + '/' + Frame.ToString + '/delay') then
    Delay := EquipData[UpPath + '/' + State + '/' + Frame.ToString + '/delay'].Data
  else
    Delay := 100;

  if IsSaddle then
    if Data.ContainsKey(UpPath + '/' + State + '/' + Frame.ToString) then
      Delay := Data[UpPath + '/' + State + '/' + Frame.ToString].Data;

  FTime := FTime + 17;
  if FTime > Delay then
  begin
    Frame := Frame + 1;
    if not HasEntryE(UpPath + '/' + State + '/' + Frame.ToString) then
      Frame := 0;
    FTime := 0;
  end;

  if TAvatarParts.ZMap.contains(EquipData[Path + '/z'].Data) then
    Z := 100 + Player.Z - TAvatarParts.ZMap.IndexOf(EquipData[Path + '/z'].Data)
  else
  begin
    var ZName := string(EquipData[Path + '/z'].Data);
    if ZName = 'tamingMobBack' then
      Z := Player.Z - 100
    else
    begin
      if ImageNum='0' then
        Z := Player.Z-1
      else
        Z := Player.Z;
      var List:=['01932524','01932422','01932454'];
      for var i in List do
       if ID=i then
          Z := Player.Z;
    end;
  end;

  if (Player.JumpState <> jsNone) then
  begin
    Frame := 0;
    State := 'jump';
  end;

  if Player.JumpState = jsNone then
  begin
    State := 'stand1';
    if (Keyboard.Key[DIK_LEFT]) or (Keyboard.Key[DIK_RIGHT]) then
      State := 'walk1';
    if Keyboard.Key[DIK_DOWN] then
      State := 'prone';
    if (State = 'prone') and (Keyboard.KeyReleased[DIK_DOWN]) then
      State := 'stand1';
    if (Keyboard.KeyReleased[DIK_LEFT]) or (Keyboard.KeyReleased[DIK_RIGHT]) then
      State := 'stand1';
  end;

  if (Player.InLadder) then
  begin
    case Player.LadderType of
      rtLadder:
        begin
          if (Keyboard.Key[DIK_UP]) or (Keyboard.Key[DIK_DOWN]) then
            State := 'ladder'
          else
          begin
            State := 'ladder';
            Frame := 0;
          end;
        end;

      rtRope:
        begin
          if (Keyboard.Key[DIK_UP]) or (Keyboard.Key[DIK_DOWN]) then
            State := 'rope'
          else
          begin
            State := 'rope';
            Frame := 0;
          end;
        end;
    end;
  end;

  if HasEntryE(UpPath + '/characterAction') then
  begin
    if HasEntryE(UpPath + '/characterAction/' + State) then
      CharacterAction := EquipData[UpPath + '/characterAction/' + State].Data
    else if HasEntryE(UpPath + '/characterAction/walk1') then
      CharacterAction := EquipData[UpPath + '/characterAction/walk1'].Data;
  end
  else
    CharacterAction := 'sit';
  if IsChairTaming then
    State:='sit';

  MirrorX := Player.MirrorX;
  if ImageEntry.Get('origin') <> nil then
    Origin := ImageEntry.Get('origin').Vector;

  Y := Trunc(Player.Y);
  case MirrorX of
    True:
      begin
        X := Trunc(Player.X + 1);
        Offset.X := Origin.X - PatternWidth;
        Flip := -1;
      end;
    False:
      begin
        X := Trunc(Player.X);
        Offset.X := -Origin.X;
        Flip := 1;
      end;
  end;
  Offset.Y := -Origin.Y;

  if HasEntryE(Path + '/map/navel') then
  begin
    var _Offset: TPoint;
    if CharacterAction = 'prone' then
    begin
      if MirrorX then
        _Offset.X := -3
      else
        _Offset.X := 3;
      _Offset.Y := 3;
    end
    else if CharacterAction = 'fly' then
    begin
      if MirrorX then
        _Offset.X := -3
      else
        _Offset.X := 3;
      _Offset.Y := 27;
    end
    else
    begin
      _Offset.X := 0;
      _Offset.Y := 17;
    end;

    if MirrorX then
      Navel.X := -EquipData[Path + '/map/navel'].Vector.X * Flip - _Offset.x
    else
      Navel.X := -EquipData[Path + '/map/navel'].Vector.X * Flip - 4 - _Offset.x;
    Navel.Y := -EquipData[Path + '/map/navel'].Vector.Y - _Offset.Y;
  end;

  var FixedIDs := ['01932377', '01902002', '1902002', '1902007', '01902007', '01932123', '01932181', '01932116', '01932081', '01992000', '01932418', '01932461', '01932507', '01932504', '01932505'];

  for var I in FixedIDs do
    if ID = I then
    begin
      case MirrorX of
        True:
          Offset.x := Origin.x - PatternWidth + Navel.x;
        False:
          Offset.x := -Origin.x + Navel.x;
      end;
      Offset.Y := -Origin.Y + Navel.Y - 50;
      Navel.X := 0;
      Navel.Y := 50;
    end;

end;

initialization
  TTamingMob.SaddleList := TDictionary<string, string>.Create;
  TTamingMob.ImageNumList := TDictionary<string, string>.Create;
  TTamingMob.Data := TDictionary<string, TWZIMGEntry>.Create;

finalization
  TTamingMob.SaddleList.Free;
  TTamingMob.ImageNumList.Free;
  TTamingMob.Data.Free;

end.

