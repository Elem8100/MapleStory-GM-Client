unit WZIMGFile;

interface

uses
  Windows, Classes, SysUtils, Variants, Generics.Collections, Tools, Dialogs, WZReader, WZDirectory,
  PNGMapleCanvasEx, MP3MapleSound, StrUtils, System.Types;

type
  tagVARENUM = (VT_EMPTY, VT_NULL, VT_I2, VT_I4, VT_R4, VT_R8, VT_CY, VT_DATE, VT_BSTR, VT_DISPATCH,
    VT_ERROR, VT_BOOL, VT_VARIANT, VT_UNKNOWN, VT_DECIMAL, VT_UNDEFINED0xF, VT_I1, VT_UI1, VT_UI2,
    VT_UI4, VT_I8, VT_UI8);

  TMapleDataType = (mdtNone, mdtIMG_0x00, mdtShort, mdtInt, mdtFloat, mdtDouble, mdtString,
    mdtExtended, mdtProperty, mdtCanvas, mdtVector, mdtConvex, mdtSound, mdtUOL, mdtInt64);

  TWZIMGEntry = class(TWZEntry)
  private
    FType: TMapleDataType;
    FData: Variant;
    FCanvas: TPNGMapleCanvas;
    FVector: TPoint;
    FSound: TMP3MapleSound;
    FChildren: TObjectList<TWZIMGEntry>;
    function GetChild(const Name: string): TWZIMGEntry;
  public
    constructor Create(Parent: TWZEntry);
    destructor Destroy; override;
    procedure AddChild(Entry: TWZIMGEntry);
    procedure Finish;
    function Get(const Path: string): TWZIMGEntry; overload;
    function Get(const Path: string; Default: Variant): Variant; overload;
    function Get2(const Path: string): TWZIMGEntry;
    function GetPath: string;
    property DataType: TMapleDataType read FType write FType;
    property Data: Variant read FData write FData;
    property Canvas: TPNGMapleCanvas read FCanvas write FCanvas;
    property Vector: TPoint read FVector write FVector;
    property Sound: TMP3MapleSound read FSound write FSound;
    property Child[const Name: string]: TWZIMGEntry read GetChild;
    property Children: TObjectList<TWZIMGEntry> read FChildren; // read-only
  end;

  TWZIMGFile = class
  private
    WZ: TWZReader;
    FFileEntry: TWZFile;
    FRoot: TWZIMGEntry;
    procedure Parse(var Entry: TWZIMGEntry);
    procedure ParseExtended(var Entry: TWZIMGEntry; EndOfExtendedBlock: Cardinal);
  public
    constructor Create(WZReader: TWZReader; WZFileEntry: TWZFile);
    destructor Destroy; override;
    property Root: TWZIMGEntry read FRoot write FRoot;
  end;

function LoadStandalone(const FileName: string): TWZIMGFile;

implementation

uses
  WzUtils, Global;

function MakePChar(const S: string): NativeUInt;
var
  P: PChar;
begin
  P := StrAlloc(Length(S) + 1);
  StrPCopy(P, S);
  Result := NativeUInt(P);
end;
{ TWZIMGEntry }

constructor TWZIMGEntry.Create(Parent: TWZEntry);
begin
  Self.Parent := Parent;
  FChildren := TObjectList<TWZIMGEntry>.Create;
end;

destructor TWZIMGEntry.Destroy;
begin

  FChildren.Free;

  if FSound <> nil then
    FSound.Free;

  if FCanvas <> nil then
    FCanvas.Free;

  inherited;
end;

procedure TWZIMGEntry.AddChild(Entry: TWZIMGEntry);
begin
  FChildren.Add(Entry);
end;

procedure TWZIMGEntry.Finish;
begin
  FChildren.TrimExcess;
end;

function TWZIMGEntry.Get(const Path: string): TWZIMGEntry;
var
  Split: TStringArray;
  i: Integer;
begin
  Split := Explode('/', Path);

  Result := Self;
  for i := 0 to High(Split) do
  begin
    if Split[i] = '..' then
      Result := TWZIMGEntry(Result.Parent)
    else
      Result := Result.Child[Split[i]];

    if not Assigned(Result) then
      Exit;
  end;

  Split := nil;
end;

function GetTopEntry(Entry: TWZIMGEntry): TWZIMGEntry;
var
  E: TWZEntry;
begin
  E := Entry.Parent;
  while E <> nil do
  begin
    E := E.Parent;
    if RightStr(E.Name, 4) = '.img' then
      Exit(TWZIMGEntry(E));
  end;
end;

function TWZIMGEntry.GetPath: string;
var
  Path: string;
  E: TWZEntry;
begin
  Path := Self.Name;
  E := Self.Parent;
  while E <> nil do
  begin
    Path := E.Name + '/' + Path;
    E := E.Parent;
  end;
  Result := Path;

end;

function TWZIMGEntry.Get2(const Path: string): TWZIMGEntry;
var
  Split, split2: TStringArray;
  i: Integer;
  Child, Entry: TWZIMGEntry;
  s1, s2, Err, OutLink: string;
begin
  Split := Explode('/', Path);

  Result := Self;

  for i := 0 to High(Split) do
  begin

    if Split[i] = '..' then
      Result := TWZIMGEntry(Result.Parent)
    else
      Result := Result.Child[Split[i]];

    if not Assigned(Result) then
      Exit(GetImgEntry('Character/00002000.img/alert/0/arm'));

  end;

  case Result.DataType of
    mdtUOL:
      begin
        Entry := TWZIMGEntry(Result.Parent);
        Child := Entry.Get(Result.Data);

        if (Child = nil) then
        begin
          Err := GetEntryPath(Result);
          if LeftStr(Err, 6) <> 'Npc.wz' then
          begin
            s1 := StringReplace(GetEntryPath(Result), '.wz', '', [rfReplaceAll]);
            s2 := StringReplace(Result.Data, '../', '', [rfReplaceAll]);
            split2 := Explode('/', s2);
            s2 := '';
            for i := 1 to High(split2) do
              s2 := s2 + split2[i] + '/';
            s2 := split2[0] + '.img/' + s2;
            Delete(s2, Length(s2), 1);
            Child := GetImgEntry(StringReplace(s1, RightStr(s1, Length(s2)), s2, [rfReplaceAll]));
            if Child = nil then
              Exit(GetImgEntry('Character/00002000.img/alert/0/arm'));

          end
          else
            Child := GetImgEntry('Character/00002000.img/alert/0/arm');
        end;

        if Child <> nil then
          case Child.DataType of
            // UOL link to UOL
            mdtUOL:
              begin
                Child := TWZIMGEntry(Child.Parent).Get(Child.Data);
              end;
            // UOL link to Canvas
            mdtCanvas:
              begin
                if Child.Child['_inlink'] <> nil then
                  Child := GetTopEntry(Child).Get(Child.Child['_inlink'].Data)
                else if Child.Child['_outlink'] <> nil then
                  Child := GetImgEntry(Child.Child['_outlink'].Data, True);
              end;
          end;

        Result := Child;
      end;

    mdtCanvas:
      begin
        if Result.Child['_outlink'] <> nil then
        begin

          OutLink := Result.Child['_outlink'].Data;
          var S: TArray<string> := OutLink.Split(['/']);
          if LeftStr(Result.GetPath, 4) = 'Map2' then
          begin
            OutLink := StringReplace(OutLink, 'Map', 'Map2', [rfReplaceAll]);
            Result := GetImgEntry(OutLink, True);
          end
          else if LeftStr(GetEntryPath(Result), 4) = 'Map0' then
          begin
            if S[1] = 'Back' then
            begin
              OutLink := StringReplace(OutLink, 'Map', 'Map0', [rfReplaceAll]);
              Result := GetImgEntry(OutLink, True);
            end;
          end
          else if LeftStr(GetEntryPath(Result), 4) = 'Mob2' then
          begin
            OutLink := StringReplace(OutLink, 'Mob', 'Mob2', [rfReplaceAll]);
            Result := GetImgEntry(OutLink, True);
          end
          else if (LeftStr(GetEntryPath(Result), 8) = 'Skill001') and (not HasImgFile(S[0] + '/' + S[1])) then
          begin
            OutLink := StringReplace(OutLink, 'Skill', 'Skill001', [rfReplaceAll]);
            Result := GetImgEntry(OutLink, True);
          end
          else
            Result := GetImgEntry(Result.Child['_outlink'].Data, True);

        end
        else if Result.Child['_inlink'] <> nil then
        begin
          Result := GetTopEntry(Result).Get(Result.Child['_inlink'].Data);
           if Result = nil then
              Exit(GetImgEntry('Character/00002000.img/alert/0/arm'));
        end
        else if Result.Child['source'] <> nil then
          Result := GetImgEntry(Result.Child['source'].Data, True);
      end;
  end;

  Split := nil;
end;

function TWZIMGEntry.Get(const Path: string; Default: Variant): Variant;
var
  i: Integer;
  E: TWZIMGEntry;
begin
  E := Get(Path);
  if E = nil then
    Exit(Default);

  if (VarType(Default) in [varSmallint..varUInt64]) and (not (VarType(E.Data) in [varSmallint..varUInt64])) then
  begin
    if VarType(E.Data) = varUString then
      if TryStrToInt(E.Data, i) then
        Exit(i)
      else
        Exit(Default)
    else
      ShowMessage(VarTypeAsText(VarType(E.Data)) + '  --->  ' + IntToStr(Byte(E.DataType)) +
        '  ||  ' + VarTypeAsText(VarType(Default)));

    Exit(Default);
  end;

  Result := E.Data;
end;

function TWZIMGEntry.GetChild(const Name: string): TWZIMGEntry;
begin
  for Result in FChildren do
    if SameText(Result.Name, Name) then
      Exit;

  Result := nil;
end;

{ TWZIMGFile }

constructor TWZIMGFile.Create(WZReader: TWZReader; WZFileEntry: TWZFile);
begin
  WZ := WZReader;
  FFileEntry := WZFileEntry;

  WZ.Seek(FFileEntry.Offset, soBeginning);

  FRoot := TWZIMGEntry.Create(FFileEntry.Parent);
  FRoot.Name := FFileEntry.Name;
  FRoot.DataType := mdtExtended;

  ParseExtended(FRoot, 0);
end;

destructor TWZIMGFile.Destroy;
begin
  FFileEntry.IMGFile := nil;
  FRoot.Free;

  inherited;
end;

procedure TWZIMGFile.Parse(var Entry: TWZIMGEntry);
var
  VT: tagVARENUM;
  Marker, iMarker: Byte;
  EndOfExtBlock: Cardinal;
begin
  Marker := WZ.ReadByte;

  case Marker of
    0:
      Entry.Name := WZ.ReadDecodedString;
    1:
      Entry.Name := WZ.ReadDecodedStringAtOffsetAndReset(FFileEntry.Offset + WZ.ReadInt);
  end;

  VT := tagVARENUM(WZ.ReadByte);
  case VT of
    VT_EMPTY:
      Entry.DataType := mdtIMG_0x00;
    VT_I2, VT_BOOL, VT_UI2:
      begin
        Entry.DataType := mdtShort;
        Entry.Data := WZ.ReadShort;
      end;
    VT_I4, VT_UI4:
      begin
        Entry.DataType := mdtInt;
        Entry.Data := WZ.ReadValue;
      end;
    VT_R4:
      begin
        Entry.DataType := mdtFloat;
        Entry.Data := WZ.ReadFloatValue;
      end;
    VT_R8:
      begin
        Entry.DataType := mdtDouble;
        Entry.Data := WZ.ReadDouble;
      end;
    VT_I8, VT_UI8:
      begin
        Entry.DataType := mdtInt64;
        Entry.Data := WZ.ReadValue64;
      end;
    VT_BSTR:
      begin
        Entry.DataType := mdtString;
        iMarker := WZ.ReadByte;
        case iMarker of
          0:
            Entry.Data := WZ.ReadDecodedString;
          1:
            Entry.Data := WZ.ReadDecodedStringAtOffsetAndReset(WZ.ReadInt + FFileEntry.Offset);
        else
          raise Exception.CreateFmt('Unknown iMarker: %d', [iMarker]);
        end;
      end;
    VT_DISPATCH:
      begin
        Entry.DataType := mdtExtended;
        EndOfExtBlock := WZ.ReadInt;
        EndOfExtBlock := EndOfExtBlock + WZ.Position;
        ParseExtended(Entry, EndOfExtBlock);
      end;
  else
    raise Exception.CreateFmt('Unknown VARENUM: %d at offset %d', [Byte(VT), WZ.Position]);
  end;
end;

procedure TWZIMGFile.ParseExtended(var Entry: TWZIMGEntry; EndOfExtendedBlock: Cardinal);
var
  Marker: Byte;
  dType: string;
  Children, i: Integer;
  ChildEntry: TWZIMGEntry;
  Width, Height, Format, Format2, DataLength, X, Y: Integer;
begin
  Marker := WZ.ReadByte;

  case Marker of
    $73:
      dType := WZ.ReadDecodedString;
    $1B:
      dType := WZ.ReadDecodedStringAtOffsetAndReset(FFileEntry.Offset + WZ.ReadInt);
  end;

  if dType = 'Property' then
  begin
    Entry.DataType := mdtProperty;

    WZ.ReadByte;
    WZ.ReadByte;
    Children := WZ.ReadValue;

    for i := 0 to Children - 1 do
    begin
      ChildEntry := TWZIMGEntry.Create(Entry);
      Parse(ChildEntry);
      ChildEntry.Finish;
      Entry.AddChild(ChildEntry);
    end;
  end
  else if dType = 'Canvas' then
  begin
    Entry.DataType := mdtCanvas;
    WZ.ReadByte; // always 0 (?)

    Marker := WZ.ReadByte;
    if Marker = 1 then
    begin
      WZ.ReadByte;
      WZ.ReadByte;

      Children := WZ.ReadValue;

      for i := 0 to Children - 1 do
      begin
        ChildEntry := TWZIMGEntry.Create(Entry);
        Parse(ChildEntry);
        ChildEntry.Finish;
        Entry.AddChild(ChildEntry);
      end;
    end;

    Width := WZ.ReadValue;
    Height := WZ.ReadValue;
    Format := WZ.ReadValue;
    Format2 := WZ.ReadByte;
    WZ.ReadInt; // always 0 (?)
    DataLength := WZ.ReadInt - 1;
    WZ.ReadByte; // always 0 (?)

    Entry.Canvas := TPNGMapleCanvas.Create(Width, Height, DataLength, WZ.Position, Format + Format2, WZ);
    WZ.Seek(DataLength, soCurrent); // Skip the image
  end
  else if dType = 'Shape2D#Vector2D' then
  begin
    Entry.DataType := mdtVector;
    X := WZ.ReadValue;
    Y := WZ.ReadValue;
    Entry.Vector := Point(X, Y);
  end
  else if dType = 'Shape2D#Convex2D' then
  begin
    Children := WZ.ReadValue;

    for i := 0 to Children - 1 do
    begin
      ChildEntry := TWZIMGEntry.Create(Entry);
      ParseExtended(ChildEntry, 0);
      ChildEntry.Finish;
      Entry.AddChild(ChildEntry);
    end;
  end
  else if dType = 'Sound_DX8' then
  begin
    Entry.DataType := mdtSound;
    WZ.ReadByte;
    DataLength := WZ.ReadValue;
    WZ.ReadValue; // no clue what this is

    Entry.Sound := TMP3MapleSound.Create(DataLength, WZ.Position, WZ);
    WZ.Seek(EndOfExtendedBlock, soBeginning);
  end
  else if dType = 'UOL' then
  begin
    Entry.DataType := mdtUOL;
    WZ.ReadByte;

    Marker := WZ.ReadByte;
    case Marker of
      0:
        Entry.Data := WZ.ReadDecodedString;
      1:
        Entry.Data := WZ.ReadDecodedStringAtOffsetAndReset(FFileEntry.Offset + WZ.ReadInt);
    end;
  end;
end;

function LoadStandalone(const FileName: string): TWZIMGFile;
var
  Reader: TWZReader;
  F: TWZFile;
begin
  Reader := TWZReader.Create(FileName);
  // F := TWZFile.Create(Reader.FileName, 0, 0, nil);
  F.Offset := 0;
  Result := TWZIMGFile.Create(Reader, F);
  F.IMGFile := Result;
end;

end.

