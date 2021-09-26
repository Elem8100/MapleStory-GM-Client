unit WZArchive;

interface

uses
  Classes, SysUtils, WZReader, WZDirectory, WZIMGFile, Tools, StrUtils;

type
  TWZArchive = class
  private
    FReader: TWZReader;
    FRoot: TWZDirectory;
    FFileSize, FHeaderSize, FVersion: Integer;
    FName, FPKG, FCopyright, FPath, FPathName: string;
    procedure Load;
    procedure GetOffsets(Dir: TWZDirectory; var StartOffset: Int64);
    procedure ParseDirectory(Dir: TWZDirectory);
    function DecodeVersion(Encoded: Smallint): Integer;
  public
    constructor Create(Filename: string; LoadToMem: Boolean = False);
    destructor Destroy; override;
    function GetImgFile(const Path: string): TWZIMGFile;
    function ResolveFullPath(P: string): TWZIMGEntry;
    function ParseFile(F: TWZFile): TWZIMGFile;
    property Reader: TWZReader read FReader;
    property Root: TWZDirectory read FRoot;
    property Copyright: string read FCopyright;
    property FileSize: Integer read FFileSize;
    property HeaderSize: Integer read FHeaderSize;
    property Name: string read FName;
    property Path: string read FPath write FPath;
    property PathName: string read FPathName write FPathName;
    property PKG: string read FPKG;
    property Version: Integer read FVersion;
  end;

var
  VerStart: Integer = 100;  // Their fucking hash algorithm has collisions every 10 versions...

implementation

function TWZArchive.ResolveFullPath(P: string): TWZIMGEntry;
var
  Split: TArray<string>;
begin
  while P[1] <> '/' do
    Delete(P, 1, 1);
  Delete(P, 1, 1);

  Split := Explode('.img/', P);
  Result := GetImgFile(Split[0] + '.img').Root.Get(Split[1]);
end;

constructor TWZArchive.Create(FileName: string; LoadToMem: Boolean = False);
begin
  FReader := TWZReader.Create(FileName, LoadToMem);

  FName := ExtractFileName(FileName);
  var Split := FileName.Split(['Data']);

  if High(Split) = 0 then
  begin
    case FName[2] of
      'a':
        PathName := 'Map';

      'o':
        begin
          if LeftStr(FName, 3) = 'Mob' then
            PathName := 'Mob';
          if FName[1] = 'S' then
            PathName := 'Sound'
          else if FName[3] = 'r' then
            PathName := 'Morph';

        end;
      'p':
        PathName := 'Npc';
      'h':
        PathName := 'Character';
      'k':
        PathName := 'Skill';
      'I':
        PathName := 'UI';
      'e':
        PathName := 'Reactor';
      'f':
        PathName := 'Effect';
      't':
        if FName[1] = 'I' then
          PathName := 'Item'
        else if FName[3] = 'c' then
          PathName := 'Etc'
        else
          PathName := 'String';
    end;
  end
  else
  begin
    var Split1 := FileName.Split(['Data\']);
    Split1[1] := Split1[1].Replace('\', '/');
    var Split2 := Split1[1].Split(FName);
    Delete(Split2[0], Length(Split2[0]), 1);
    PathName := Split2[0];
  end;

  FRoot := TWZDirectory.Create(ExtractFileName(PathName), 0, 0, 0, nil);
  FRoot.Archive := Self;

  Load;
end;

destructor TWZArchive.Destroy;
begin
  FRoot.Free;
  FReader.Free;

  inherited;
end;

procedure TWZArchive.Load;
var
  Off: Int64;
begin
  FPKG := FReader.ReadString(4);
  FFileSize := FReader.ReadUInt64;
  FHeaderSize := FReader.ReadInt;
  FCopyright := FReader.ReadNullTerminatedString;
  FVersion := DecodeVersion(FReader.ReadShort);

  ParseDirectory(FRoot);
  Off := FReader.Position;
  GetOffsets(FRoot, Off);
end;

function FastHash(a, b, c: Integer): Integer; register;
asm   // Borland FastCall: eax -> a, edx -> b, ecx -> c
{$IFDEF CPUX86}
        push    eax  // so that it becomes available as [esp]
        SHR     ecx, 24
        SHR     edx, 16
        SHR     eax, 8
        XOR     cl, dl
        XOR     cl, al
        XOR     cl, [esp]
        NOT     cl
        movzx   eax, cl
        pop     ecx  // don't overwrite eax
{$ELSE}  // ECX: A; EDX: B; R8: C
{
  push rcx  // so that it becomes available as [rsp]
  mov rax, rcx
  mov rcx, r8
  shr ecx, 24
  shr edx, 16
  shr eax, 8
  xor cl, dl
  xor cl, al
  xor cl, [rsp]
  not cl
  movzx eax,cl
  add rsp,8
  }
{$ENDIF}
end;

function TWZArchive.DecodeVersion(Encoded: Smallint): Integer;
var
  Sum, i, j: Integer;
  Version: string;
begin
  for i := VerStart to 500 do
  begin
    Sum := 0;

    Version := IntToStr(i);

    for j := 1 to Length(Version) do
      Sum := (Sum * 32) + Ord(Version[j]) + 1;

    if Encoded = FastHash(Sum, Sum, Sum) then
      Exit(i);
  end;

  Result := -1;
end;

procedure TWZArchive.ParseDirectory(Dir: TWZDirectory);
var
  EntryCount, i, Size, Checksum: Integer;
  Marker: Byte;
  Name: string;
  E: TWZEntry;
begin
  EntryCount := FReader.ReadValue;

  for i := 0 to EntryCount - 1 do
  begin
    Marker := FReader.ReadByte;

    case Marker of
      $01, $02:
        begin
          Name := FReader.ReadDecodedStringAtOffsetAndReset(FReader.ReadInt + FHeaderSize + 1);
          Size := FReader.ReadValue;
          Checksum := FReader.ReadValue;
          FReader.ReadInt;       // Dummy

          if Marker = 1 then
          begin
            E := TWZDirectory.Create(Name, Size, Checksum, 0, Dir);
            Dir.AddDirectory(TWZDirectory(E));
          end
          else
          begin
            E := TWZFile.Create(Name, Size, Checksum, 0, Dir);
            Dir.AddFile(TWZFile(E));
          end;
        end;

      $03, $04:
        begin
          Name := FReader.ReadDecodedString;
          Size := FReader.ReadValue;
          Checksum := FReader.ReadValue;
          FReader.ReadInt;     // Dummy

          if Marker = 3 then
          begin
            E := TWZDirectory.Create(Name, Size, Checksum, 0, Dir);
            Dir.AddDirectory(TWZDirectory(E));
          end
          else
          begin
            E := TWZFile.Create(Name, Size, Checksum, 0, Dir);
            Dir.AddFile(TWZFile(E));
          end;
        end;

    else
      raise Exception.CreateFmt('Unknown Marker at ParseDirectory(%s): ' + sLineBreak + 'i = %d; Marker %d', [Dir.Name, i, Marker]);
    end;
  end;

  for i := 0 to Dir.SubDirs.Count - 1 do
    ParseDirectory(Dir.SubDirs[i]);
end;

function TWZArchive.GetImgFile(const Path: string): TWZIMGFile;
var
  Segments: TStringArray;
  i: Integer;
  Dir: TWZDirectory;
  Entry: TWZFile;
begin
  Segments := Explode('/', Path);

  Dir := FRoot;
  for i := 0 to High(Segments) - 1 do
  begin
    Dir := TWZDirectory(Dir.Entry[Segments[i]]);

    if Dir = nil then
   //  Exit(mainunit1.CharacterWZ.GetImgFile('Cap/01000000.img'));
      Exit(nil);

  end;

  Entry := TWZFile(Dir.Entry[Segments[High(Segments)]]);
  if Entry = nil then
    Exit(nil);
   // Exit(mainunit1.CharacterWZ.GetImgFile('Cap/01000000.img'));
  Segments := nil;

  Result := ParseFile(Entry);
end;

procedure TWZArchive.GetOffsets(Dir: TWZDirectory; var StartOffset: Int64);
var
  Entry: TWZEntry;
begin
  for Entry in Dir.Files do
  begin
    Entry.Offset := StartOffset;
    Inc(StartOffset, Entry.Size);
  end;

  for Entry in Dir.SubDirs do
    GetOffsets(TWZDirectory(Entry), StartOffset);
end;

function TWZArchive.ParseFile(F: TWZFile): TWZIMGFile;
begin
  if Assigned(F.IMGFile) then
    Exit(TWZIMGFile(F.IMGFile));

  FReader.LoadCache(F.Offset, F.Size);
  try
    Result := TWZIMGFile.Create(FReader, F);
    F.IMGFile := Result;
  finally
    FReader.ClearCache;
  end;
end;

end.

