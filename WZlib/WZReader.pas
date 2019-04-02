unit WZReader;

interface

uses SysUtils, Classes, KeyHandler;

type
  TWZReader = class
  private
    FFileName: string;
    FWZ: TStream;
    FKey: TBytes;
    FHeaderSize, FHash: Cardinal;

    function GetPosition: Int64;
  public
    constructor Create(const AFileName: string; LoadFully: Boolean = False);
    destructor Destroy; override;

    procedure LoadCache(Offset, Size: NativeInt);
    procedure ClearCache;

    function ReadByte: Byte;
    function ReadInt8: ShortInt;
    function ReadShort: SmallInt;
    function ReadInt: Integer;
    function ReadValue: Integer;
    function ReadValue64: Int64;
    function ReadUInt64: UInt64;
    function ReadFloat: Single;
    function ReadFloatValue: Single;
    function ReadDouble: Double;
    function ReadChar: AnsiChar;
    function ReadString(Length: Integer): string;
    function ReadNullTerminatedString: string;
    function ReadDecodedString: string;
    function ReadDecodedStringAtOffsetAndReset(Offset: Int64): string;
    function ReadOffset: Cardinal;

    procedure Seek(Offset: Int64; Origin: TSeekOrigin);

    property FileName: string read FFileName;
    property Key: TBytes read FKey;
    property Position: Int64 read GetPosition;
    property Stream: TStream read FWZ;
    property HeaderSize: Cardinal read FHeaderSize write FHeaderSize;
    property Hash: Cardinal read FHash write FHash;

    class var EncryptionIV: Cardinal;
  end;

  // Used to cache .img files (leading to much better performance because it bypasses OS API calls)
  TCacheFileStream = class(TFileStream)
  private
    FCache: PByte;
    FCacheStart, FCacheSize, FQPos: NativeInt;
  public
    procedure AfterConstruction; override;
    procedure LoadCache(Offset, Size: NativeInt);
    procedure ClearCache;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

implementation

{ TWZReader }

constructor TWZReader.Create(const AFileName: string; LoadFully: Boolean = False);
begin
  inherited Create;

  FFileName := AFileName;
  FKey := nil;
  if not LoadFully then
    FWZ := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone)
  else
  begin
    FWZ := TMemoryStream.Create;
    TMemoryStream(FWZ).LoadFromFile(AFileName);
  end;
end;

destructor TWZReader.Destroy;
begin
  FKey := nil;
  FWZ.Free;

  inherited;
end;

procedure TWZReader.LoadCache(Offset, Size: NativeInt);
var
  H: THandle;
begin
  // Caching only makes sense if it isn't a MemoryStream
  if FWZ is THandleStream then
  begin
    if not (FWZ is TCacheFileStream) then
    begin
      H := THandleStream(FWZ).Handle;
      FWZ.FreeInstance; // Don't call the destructor (that would close the handle)
      FWZ := TCacheFileStream.Create(H);
    end;

    TCacheFileStream(FWZ).LoadCache(Offset, Size);
  end;
end;

procedure TWZReader.ClearCache;
begin
  if FWZ is TCacheFileStream then
    TCacheFileStream(FWZ).ClearCache;
end;

function TWZReader.GetPosition: Int64;
begin
  Result := FWZ.Position;
end;

procedure TWZReader.Seek(Offset: Int64; Origin: TSeekOrigin);
begin
  FWZ.Seek(Offset, Origin);
end;

// ==================== Stream Reading functions ===============================

function TWZReader.ReadByte: Byte;
begin
  FWZ.Read(Result, 1);
end;

function TWZReader.ReadInt8: ShortInt;
begin
  FWZ.Read(Result, 1);
end;

function TWZReader.ReadShort: SmallInt;
begin
  FWZ.Read(Result, 2);
end;

function TWZReader.ReadInt: Integer;
begin
  FWZ.Read(Result, 4);
end;

function TWZReader.ReadFloat: Single;
begin
  FWZ.Read(Result, 4);
end;

function TWZReader.ReadUInt64: UInt64;
begin
  FWZ.Read(Result, 8);
end;

function TWZReader.ReadDouble: Double;
begin
  FWZ.Read(Result, 8);
end;

function TWZReader.ReadChar: AnsiChar;
begin
  FWZ.Read(Result, 1);
end;

function TWZReader.ReadString(Length: Integer): string;
var
  i, Limit: Integer;
  b: Byte;
begin
  Result := '';

  Limit := Length;
  if Limit = 0 then
    Limit := FWZ.Size - FWZ.Position;

  for i := 1 to Limit do
  begin
    FWZ.Read(b, 1);
    if b <> 0 then
      Result := Result + Chr(b);
  end;
end;

function TWZReader.ReadNullTerminatedString: string;
var
  b: Byte;
  i: Integer;
begin
  b := 1;
  i := 0;

  while b <> 0 do
  begin
    FWZ.Read(b, 1);
    Inc(i);
    SetLength(Result, i);
    Result[i] := Chr(b);
  end;
end;

function TWZReader.ReadDecodedString: string;
var
  StrLength, i: Integer;
  B: Byte;
  Mask, UChr: Word;
  AStr: AnsiString;
begin
  Result := '';
  b := ReadByte;

  if b = 0 then
    Exit;

  if b <= $7F then   // Unicode
  begin
    Mask := $AAAA;

    if b = $7F then
      StrLength := ReadInt
    else
      StrLength := b;

    if StrLength < 0 then
      raise Exception.Create('StrLength < 0');

    SetLength(Result, StrLength);
    FWZ.Read(Result[1], StrLength * 2);
    for i := 1 to StrLength do
    begin
      UChr := Word(Result[i]) xor Mask;

      if FKey <> nil then
        UChr := UChr xor PWord(@FKey[i * 2 - 2])^;

      Result[i] := Char(UChr);
      Inc(Mask);
    end;
  end
  else  // Ansi
  begin
    if (FKey = nil) and (TWZReader.EncryptionIV <> 0) then
      FKey := KeyCreator.GetKey(TWZReader.EncryptionIV);

    Mask := $AA;

    if b = $80 then
      StrLength := ReadInt
    else
      StrLength := 256 - b;

    SetLength(AStr, StrLength);
    FWZ.Read(AStr[1], StrLength);
    for i := 1 to StrLength do
    begin
      B := Ord(AStr[i]) xor Mask;

      if (FKey <> nil) and (i < Length(FKey)) then
        B := B xor FKey[i - 1];

      AStr[i] := AnsiChar(B);
      Inc(Mask);
    end;

    Result := string(AStr);
  end;
end;

function TWZReader.ReadDecodedStringAtOffsetAndReset(Offset: Int64): string;
var
  Pos: Int64;
begin
  Pos := FWZ.Position;             // save old position
  FWZ.Seek(Offset, soBeginning);   // seek to wanted position
  Result := ReadDecodedString;     // read
  FWZ.Seek(Pos, soBeginning);      // reset to old position
end;

function TWZReader.ReadFloatValue: Single;
var
  B: UInt8;
begin
  FWZ.Read(B, 1);
  if B = $80 then
    FWZ.Read(Result, 4)
  else
    Result := 0;
end;

function TWZReader.ReadValue: Integer;
var
  B: UInt8;
begin
  FWZ.Read(B, 1);
  if B = $80 then
    FWZ.Read(Result, 4)
  else
    Result := Int8(B); // Sign-Extend 8-bit to 32-bit value
end;

function TWZReader.ReadValue64: Int64;
var
  B: UInt8;
begin
  FWZ.Read(B, 1);
  if B = $80 then
    FWZ.Read(Result, 8)
  else
    Result := Int8(B); // Sign-Extend 8-bit to 64-bit value
end;

function _ROL(V: Cardinal; Count: Byte): Cardinal;
asm
  mov cl, dl
  rol eax, cl
end;

function TWZReader.ReadOffset: Cardinal;
begin
  Result := FWZ.Position;
  Result := (Result - FHeaderSize) xor High(Cardinal);
  Result := Result * FHash;
  Dec(Result, $581C3F6D);
  Result := _ROL(Result, Result and 31);
  Result := Result xor Cardinal(ReadInt);
  Inc(Result, FHeaderSize * 2);
end;

{ TCacheFileStream }

procedure TCacheFileStream.AfterConstruction;
begin
  inherited;

  FCache := nil;
  FCacheStart := -1;
  FCacheSize := 0;
end;

procedure TCacheFileStream.ClearCache;
begin
  if FCache <> nil then
    FreeMem(FCache);
  FCache := nil;
  FCacheStart := -1;
  FCacheSize := 0;
end;

procedure TCacheFileStream.LoadCache(Offset, Size: NativeInt);
begin
  FCacheStart := Offset;
  FCacheSize := Size;
  FQPos := Offset;
  GetMem(FCache, Size);
  Position := Offset;
  if inherited Read(FCache^, Size) <> Size then
    raise EUnderflow.Create('LoadCache failed');
end;

function TCacheFileStream.Read(var Buffer; Count: Integer): Longint;
begin
  if FCache <> nil then  // skip the other checks if we don't even have a cache
  begin
    if (FQPos >= FCacheStart) and (FQPos + Count <= FCacheStart + FCacheSize) then
    begin
      Move(Pointer(FCache + FQPos - FCacheStart)^, Buffer, Count);
      Inc(FQPos, Count);
      Exit(Count);
    end
    else
      FileSeek(FHandle, FQPos, Ord(soBeginning));
  end;

  Result := inherited Read(Buffer, Count);
  Inc(FQPos, Result);
end;

function TCacheFileStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if (FCache <> nil) and (Offset = 0) and (Origin = soCurrent) then
    Exit(FQPos);

  if Origin = soCurrent then
    FileSeek(FHandle, FQPos, Ord(soBeginning));
  Result := FileSeek(FHandle, Offset, Ord(Origin));
  FQPos := Result;
end;

end.

