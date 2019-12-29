unit WzUtils;

interface

uses
  Windows, SysUtils, StrUtils, AsphyreSprite, Generics.Collections, WZIMGFile, Global, Tools,
  DX9Textures, WZArchive, WZDirectory, ColorUtils;

function NoIMG(const Name: string): string; inline;

function GetImgEntry(Path: string; UseGet2: Boolean = False): TWZIMGEntry;

function HasImgEntry(Path: string): Boolean;

function GetEntryPath(Entry: TWZIMGEntry): string;

function GetImgFile(Path: string): TWZIMGFile;

function HasImgFile(Path: string): Boolean;

function GetEntryE(Path: string): TWZIMGEntry;

function HasEntryE(Path: string): Boolean;

function GetUOL(Entry: TWZIMGEntry): TWZIMGEntry;

procedure DumpData(Entry: TWZIMGEntry; ToData: TObjectDictionary<string, TWZIMGEntry>; ToImageLib:
  TObjectDictionary<TWZIMGEntry, TDX9LockableTexture>; ColorEffect: TColorEffect = ceNone; Value: Integer = 0);

implementation

type
  TNodeInfo = record
    OriNode: string;
    UOLNode: string;
    UOLEntry: TWZIMGEntry;
  end;

var
  NodeList1, NodeList2: TList<TNodeInfo>;

function SelectWz(Path: string): TWZArchive;
begin
  case Path[2] of
    'a':

      if Path[4] = '2' then
        Result := Map2Wz
      else if Path[6] = '1' then
        Result := Map001Wz
      else if Path[6] = '2' then
        Result := Map002Wz
      else
        Result := MapWz;

    'o':
      if Path[1] = 'S' then
        Result := SoundWZ
      else if Path[3] = 'r' then
        Result := MorphWz
      else if Path[4] = '2' then
        Result := Mob2WZ
      else if Path[4] = '0' then
        Result := Mob001WZ
      else
        Result := MobWZ;
    'p':
      Result := NPCWZ;
    'h':
      Result := CharacterWZ;
    'k':
      if Path[6] = '0' then
        Result := Skill001Wz
      else
        Result := SkillWZ;
    'I':
      Result := UIWZ;
    'e':
      Result := ReactorWz;
    'f':
      Result := EffectWz;
    't':
      if Path[1] = 'I' then
        Result := ItemWZ
      else if Path[3] = 'c' then
        Result := EtcWZ
      else
        Result := StringWZ;
  end;
end;

function NoIMG(const Name: string): string; inline;
begin
  Result := ChangeFileExt(Name, '');
end;

function GetUOL(Entry: TWZIMGEntry): TWZIMGEntry;
begin

  Result := TWZIMGEntry(Entry.Parent).Get2(Entry.GetPath);
end;

function GetImgEntry(Path: string; UseGet2: Boolean = False): TWZIMGEntry;
var
  S: TStringArray;
  ImgName: string;
  WZ: TWZArchive;
  Len: Integer;
begin
  WZ := SelectWz(Path);
  S := Explode('.img/', Path);
  Len := Pos('/', S[0]) + 1;
  ImgName := MidStr(S[0], Len, 100) + '.img';
  if UseGet2 then
    Result := WZ.GetImgFile(ImgName).Root.Get2(S[1])
  else
    Result := WZ.GetImgFile(ImgName).Root.Get(S[1]);
end;

function GetImgFile(Path: string): TWZIMGFile;
var
  S: TStringArray;
  ImgName: string;
  WZ: TWZArchive;
  Len: Integer;
begin
  WZ := SelectWz(Path);
  S := Explode('.img/', Path);
  Len := Pos('/', S[0]) + 1;
  ImgName := MidStr(S[0], Len, 100) + '.img';
  Result := WZ.GetImgFile(ImgName);
end;

function HasImgEntry(Path: string): Boolean;
begin
  Result := GetImgEntry(Path) <> nil;
end;

function HasImgFile(Path: string): Boolean;
begin
  Result := GetImgFile(Path + '/') <> nil;
end;

function GetEntryE(Path: string): TWZIMGEntry;
begin
  Result := EquipData[Path];
end;

function HasEntryE(Path: string): Boolean;
begin
  Result := EquipData.ContainsKey(Path);
end;

function GetEntryPath(Entry: TWZIMGEntry): string;
var
  Path: string;
  E: TWZEntry;
begin
  Path := Entry.Name;
  E := Entry.Parent;
  while E <> nil do
  begin
    Path := E.Name + '/' + Path;
    E := E.Parent;
  end;
  Result := Path;
end;

procedure Scan1(IE: TWZIMGEntry; ToData: TObjectDictionary<string, TWZIMGEntry>; ToImageLib:
  TObjectDictionary<TWZIMGEntry, TDX9LockableTexture>; ColorEffect: TColorEffect; Value: Integer);
var
  C, Child, Entry: TWZIMGEntry;
  NodeInfo: TNodeInfo;
begin
  case IE.DataType of
    mdtUOL:
      begin
        Entry := TWZIMGEntry(IE.Parent);
        Child := Entry.Get(IE.Data);
        if Child = nil then
          Exit;
        if Child.DataType = mdtUOL then
          Child := TWZIMGEntry(Child.Parent).Get(Child.Data);
        if Child = nil then
          Exit;
        NodeInfo.OriNode := IE.GetPath;
        NodeInfo.UOLNode := Child.GetPath;
        NodeInfo.UOLEntry := Child;
        NodeList1.Add(NodeInfo);
      end;
    mdtInt, mdtVector, mdtShort, mdtString, mdtFloat, mdtDouble, mdtInt64, mdtProperty:
      begin
        ToData.AddOrSetValue(IE.GetPath, IE);
      end;

    mdtCanvas:
      begin
        ToData.AddOrSetValue(IE.GetPath, IE);
        ToImageLib.AddOrSetValue(IE, GetImgEntry(IE.GetPath, True).Canvas.Dump(ColorEffect, Value));
      end;
  end;

  for C in IE.Children do
    Scan1(C, ToData, ToImageLib, ColorEffect, Value);
end;

procedure Scan2(OriNode, UOLNode: string; IE: TWZIMGEntry; ToData: TObjectDictionary<string,
  TWZIMGEntry>; ToImageLib: TObjectDictionary<TWZIMGEntry, TDX9LockableTexture>; ColorEffect:
  TColorEffect; Value: Integer);
var
  C: TWZIMGEntry;
  Child, Entry: TWZIMGEntry;
  Str: string;
  NodeInfo: TNodeInfo;
begin
  if IE.DataType = mdtUOL then
  begin
    Entry := TWZIMGEntry(IE.Parent);
    Child := Entry.Get(IE.Data);
    if Child = nil then
      Exit;
    if Child.DataType = mdtUOL then
      Child := TWZIMGEntry(Child.Parent).Get(Child.Data);
  //  if Child = nil then
    //  Exit;
  end
  else
    Child := IE;

  case Child.DataType of
    mdtInt, mdtVector, mdtShort, mdtString, mdtFloat, mdtDouble, mdtInt64, mdtCanvas, mdtProperty:
      begin
        Str := StringReplace(IE.GetPath, UOLNode, '', [rfReplaceAll]);
        ToData.AddOrSetValue(OriNode + Str, Child);
        NodeInfo.OriNode := OriNode + Str;
        NodeInfo.UOLNode := Child.GetPath;
        NodeInfo.UOLEntry := Child;
        NodeList2.Add(NodeInfo);
      end;
  end;

  if Child.DataType = mdtCanvas then
  begin
    if not ToData.ContainsKey(IE.GetPath) then
    begin
      ToData.AddOrSetValue(IE.GetPath, IE);
      ToImageLib.AddOrSetValue(IE, GetImgEntry(IE.GetPath, True).Canvas.Dump(ColorEffect, Value));
    end;
  end;

  for C in IE.Children do
    Scan2(OriNode, UOLNode, C, ToData, ToImageLib, ColorEffect, Value);
end;

procedure Scan3(OriNode, UOLNode: string; IE: TWZIMGEntry; ToData: TObjectDictionary<string, TWZIMGEntry>);
var
  C: TWZIMGEntry;
  Str: string;
begin
  Str := StringReplace(GetEntryPath(IE), UOLNode, '', [rfReplaceAll]);
  ToData.AddOrSetValue(OriNode + Str, IE);
  for C in IE.Children do
    Scan3(OriNode, UOLNode, C, ToData);
end;

procedure DumpData(Entry: TWZIMGEntry; ToData: TObjectDictionary<string, TWZIMGEntry>; ToImageLib:
  TObjectDictionary<TWZIMGEntry, TDX9LockableTexture>; ColorEffect: TColorEffect = ceNone; Value: Integer = 0);
var
  P: TNodeInfo;
begin
  NodeList1.Clear;
  NodeList2.Clear;
  Scan1(Entry, ToData, ToImageLib, ColorEffect, Value);
  for P in NodeList1 do
    Scan2(P.OriNode, P.UOLNode, P.UOLEntry, ToData, ToImageLib, ColorEffect, Value);
  for P in NodeList2 do
    Scan3(P.OriNode, P.UOLNode, P.UOLEntry, ToData);
end;

initialization
  NodeList1 := TList<TNodeInfo>.Create;
  NodeList2 := TList<TNodeInfo>.Create;

finalization
  NodeList1.Free;
  NodeList2.Free;

end.

