unit WzUtils;

interface

uses
  Windows, SysUtils, StrUtils, Generics.Collections, WZIMGFile, Global, Tools,
  WZArchive, WZDirectory, ColorUtils, PXT.Types, PXT.Graphics;

function NoIMG(const Name: string): string; inline;

function GetImgEntry(Path: string; UseGet2: Boolean = False): TWZIMGEntry;

function HasImgEntry(Path: string): Boolean;

function GetEntryPath(Entry: TWZIMGEntry): string;

function GetImgFile(Path: string): TWZIMGFile;
function GetImgList(PathName: string):  TList<TWZFile>;

function HasImgFile(Path: string): Boolean;

function GetEntryE(Path: string): TWZIMGEntry;

function HasEntryE(Path: string): Boolean;

function GetUOL(Entry: TWZIMGEntry): TWZIMGEntry;

procedure DumpData(Entry: TWZIMGEntry; ToData: TDictionary<string, TWZIMGEntry>; ToImageLib: TDictionary<TWZIMGEntry, TTexture>; ColorEffect: TColorEffect = ceNone; Value: Integer = 0);

implementation

type
  TNodeInfo = record
    OriNode: string;
    UOLNode: string;
    UOLEntry: TWZIMGEntry;
  end;

var
  NodeList1, NodeList2: TList<TNodeInfo>;

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
begin
  S := Explode('.img/', Path);
  if UseGet2 then
    Result := GetImgFile(S[0] + '.img').Root.Get2_64(S[1])
  else
    Result := GetImgFile(S[0] + '.img').Root.Get(S[1]);
end;

function GetImgFile(Path: string): TWZIMGFile;
begin
  var Split := Explode('/', Path);
  if Is64Bit then
  begin
    var imgName := Split[High(Split)];
    var Str := '';
    for var I := 0 to High(Split) - 1 do
      Str := Str + Split[i] + '/';
    Delete(Str, Length(Str), 1);
    var Len := Length(Str);

    for var WZ in WzList do
    begin
      if Leftstr(WZ.path, Len) = LeftStr(Path, Len) then
      begin
        if WZ.GetImgFile(imgName) <> nil then
          Exit(WZ.GetImgFile(imgName));

      end;
    end;
  end
  else
  begin
    var Str := '';
    for var I := 1 to High(Split) do
      Str := Str + Split[i] + '/';
    Delete(Str, Length(Str), 1);
    for var WZ in WzList do
    begin
      if Leftstr(WZ.Name, 2) = LeftStr(Path, 2) then
      begin
        if WZ.GetImgFile(Str) <> nil then
          Exit(WZ.GetImgFile(Str));
      end;
    end;
  end;
end;

function GetImgList(PathName: string): TList<TWZFile>;
begin
  Result := TList<TWZFile>.Create;
  var Str := PathName.Split(['/']);
  for var Wz in WzList do
  begin
    if Is64Bit then
    begin
      if Wz.PathName = PathName then
      begin
        for var Iter in Wz.Root.Files do
        begin
          Result.Add(Iter);
        end;
      end;
    end
    else
    begin
      if Wz.PathName = Str[0] then
      begin
       if High(Str)>0 then
       begin
        if TWZDirectory(Wz.Root.Entry[Str[1]]) <> nil then
          for var Iter in TWZDirectory(Wz.Root.Entry[Str[1]]).Files do
            Result.Add(Iter);
       end
       else
         for var Iter in Wz.Root.Files do
             Result.Add(Iter);
      end;
    end;
  end;

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

procedure Scan1(IE: TWZIMGEntry; ToData: TDictionary<string, TWZIMGEntry>; ToImageLib: TDictionary<TWZIMGEntry, TTexture>; ColorEffect: TColorEffect; Value: Integer);
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

procedure Scan2(OriNode, UOLNode: string; IE: TWZIMGEntry; ToData: TDictionary<string, TWZIMGEntry>; ToImageLib: TDictionary<TWZIMGEntry, TTexture>; ColorEffect: TColorEffect; Value: Integer);
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

procedure Scan3(OriNode, UOLNode: string; IE: TWZIMGEntry; ToData: TDictionary<string, TWZIMGEntry>);
var
  C: TWZIMGEntry;
  Str: string;
begin
  Str := StringReplace(GetEntryPath(IE), UOLNode, '', [rfReplaceAll]);
  ToData.AddOrSetValue(OriNode + Str, IE);
  for C in IE.Children do
    Scan3(OriNode, UOLNode, C, ToData);
end;

procedure DumpData(Entry: TWZIMGEntry; ToData: TDictionary<string, TWZIMGEntry>; ToImageLib: TDictionary<TWZIMGEntry, TTexture>; ColorEffect: TColorEffect = ceNone; Value: Integer = 0);
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

