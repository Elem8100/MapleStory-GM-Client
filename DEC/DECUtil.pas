{*****************************************************************************

  Delphi Encryption Compendium (DEC Part I)
  Version 5.2, Part I, for Delphi 7 - 2009

  Remarks:          Freeware, Copyright must be included

  Original Author:  (c) 2006 Hagen Reddmann, HaReddmann [at] T-Online [dot] de
  Modifications:    (c) 2008 Arvid Winkelsdorf, info [at] digivendo [dot] de

  Last change:      02. November 2008

  Description:      Utilities for the DEC

 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ''AS IS'' AND ANY EXPRESS
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*****************************************************************************}

unit DECUtil;

{$I VER.INC}

interface

uses Windows, SysUtils, Classes, CRC;

type
{$IFNDEF UNICODE}
  RawByteString  = AnsiString;
{$ENDIF}
  Binary         = RawByteString; // LongString with Binary Content
{$IFNDEF VER_D4H}
  LongWord       = type Integer;
{$ENDIF}
  PLongWord      = ^LongWord;
  PByte          = ^Byte;
  PInteger       = ^Integer;
  PWord          = ^Word;
  PLongArray     = ^TLongArray;
  TLongArray     = array[0..1023] of LongWord;

  // DEC exception class which all DEC-Classes/methods should use
  EDECException  = class(Exception);

  // basic class for all DEC classes that needed a RefCounter and have
  // registration support
  TDECClass = class of TDECObject;

  TDECObject = class(TPersistent)
  public
    constructor Create; virtual;
    class function Identity: LongWord;
    class procedure Register;
    procedure FreeInstance; override;
  end;

  IDECProgress = interface
    ['{64366E77-82FE-4B86-951E-79389729A493}']
    procedure Process(const Min,Max,Pos: Int64); stdcall;
  end;

// DEC classes Registration
type
  TDECEnumClassesCallback = function(UserData: Pointer; ClassType: TClass): Boolean; register;

// Register DEC classes to make it streamable
procedure RegisterDECClasses(const Classes: array of TClass);

// Unregister DEC classes
procedure UnregisterDECClasses(const Classes: array of TClass);

// fill a StringList with registered DEC classes
procedure DECClasses(List: TStrings; Include: TClass = nil; Exclude: TClass = nil);

// find a registered DEC class by Identity
function DECClassByIdentity(Identity: LongWord; ClassType: TClass): TDECClass;

// find DEC class by Name, can be as Example: TCipher_Blowfish, Blowfish or registered Name override
function DECClassByName(const Name: String; ClassType: TClass): TDECClass;

// returns corrected short Classname of any registered DEC Class
function DECClassName(ClassType: TClass): String;

// enumerate by callback over registered DEC classes
function DECEnumClasses(Callback: TDECEnumClassesCallback; UserData: Pointer; Include: TClass = nil; Exclude: TClass = nil): TDECClass;

procedure ProtectBuffer(var Buffer; Size: Integer);
procedure ProtectBinary(var Value: Binary);
procedure ProtectStream(Stream: TStream; Size: Integer = 0);

// test if Buffer contains BufferSize values
function IsFilledWith(var Buffer; Size: Integer; Value: Char): Boolean;
procedure FoldBuf(var Dest; DestSize: Integer; const Source; SourceSize: Integer);
procedure FoldStr(var Dest; DestSize: Integer; const Source: String);

// Random Buffer/Binary, ATTENTION! standard Random Function aren't
// crytographicaly secure, please include DECRandom to install secure PRNG
function RandomBinary(Size: Integer): Binary;
procedure RandomBuffer(var Buffer; Size: Integer);
function RandomLong: LongWord;
procedure RandomSeed(const Buffer; Size: Integer); overload;
procedure RandomSeed; overload;
function RandomSystemTime: Cardinal;

// reverse Byte order from Buffer
procedure SwapBytes(var Buffer; BufferSize: Integer);
function SwapLong(Value: LongWord): LongWord;
procedure SwapLongBuffer(const Source; var Dest; Count: Integer);
function SwapInt64(const Value: Int64): Int64;
procedure SwapInt64Buffer(const Source; var Dest; Count: Integer);
function SwapBits(Value, Bits: LongWord): LongWord;
procedure XORBuffers(const Source1, Source2; Size: Integer; var Dest);

// saver test if AObject is valid
function IsObject(AObject: Pointer; AClass: TClass): Boolean;

var
  IdentityBase : LongWord = $25844852; // used as base in class method Identity

  DoRandomBuffer: procedure(var Buffer; Size: Integer); register = nil;
  DoRandomSeed: procedure(const Buffer; Size: Integer); register = nil;

implementation

resourcestring
  sClassNotRegistered = 'Class %s is not registered';
  sWrongIdentity      = 'Another class "%s" with the same identity as "%s" has already been registered.';
  
var
  FClasses: TList = nil;

function GetShortClassName(const Value: String): String;
var
  I: Integer;
begin
  Result := Value;
  I := Pos('_', Result);
  if I > 0 then Delete(Result, 1, I);
end;

procedure RegisterDECClasses(const Classes: array of TClass);
var
  I: Integer;
begin
  for I := Low(Classes) to High(Classes) do
    if (Classes[I] <> nil) and Classes[I].InheritsFrom(TDECObject) then
      TDECClass(Classes[I]).Register;
end;

procedure UnregisterDECClasses(const Classes: array of TClass);
var
  I,J: Integer;
begin
  if IsObject(FClasses, TList) then
    for I := Low(Classes) to High(Classes) do
    begin
      J := FClasses.IndexOf(Classes[I]);
      if J >= 0 then FClasses.Delete(J);
    end;
end;

procedure DECClasses(List: TStrings; Include: TClass = nil; Exclude: TClass = nil);

  function DoAdd(List: TStrings; ClassType: TClass): Boolean;
  begin
    Result := False;
    List.AddObject(ClassType.ClassName, Pointer(ClassType));
  end;

begin
  if IsObject(List, TStrings) then
  try
    List.BeginUpdate;
    List.Clear;
    DECEnumClasses(@DoAdd, List, Include, Exclude);
  finally
    List.EndUpdate;
  end;
end;

function DECClassByIdentity(Identity: LongWord; ClassType: TClass): TDECClass;

  function DoFind(Identity: LongWord; ClassType: TDECClass): Boolean;
  begin
    Result := ClassType.Identity = Identity;
  end;

begin
  Result := DECEnumClasses(@DoFind, Pointer(Identity), ClassType);
  if Result = nil then
    raise EDECException.CreateFmt(sClassNotRegistered, [IntToHEX(Identity, 8)]);
end;

function DECClassByName(const Name: String; ClassType: TClass): TDECClass;

  function DoFindShort(const Name: String; ClassType: TClass): Boolean;
  begin
    Result := CompareText(DECClassName(ClassType), Name) = 0;
  end;

  function DoFindLong(const Name: String; ClassType: TClass): Boolean;
  begin
    Result := CompareText(ClassType.ClassName, Name) = 0;
  end;

begin
  Result := nil;
  if Name <> '' then
    if GetShortClassName(Name) = Name then
      Result := DECEnumClasses(@DoFindShort, Pointer(Name), ClassType)
    else
      Result := DECEnumClasses(@DoFindLong, Pointer(Name), ClassType);
  if Result = nil then
    raise EDECException.CreateFmt(sClassNotRegistered, [Name]);
end;

function DECClassName(ClassType: TClass): String;
begin
  if ClassType = nil then Result := ''
    else Result := GetShortClassName(ClassType.ClassName);
end;

function DECEnumClasses(Callback: TDECEnumClassesCallback; UserData: Pointer;
            Include: TClass = nil; Exclude: TClass = nil): TDECClass;
var
  I: Integer;
begin
  Result := nil;
  if Assigned(Callback) and IsObject(FClasses, TList) then
    for I := 0 to FClasses.Count -1 do
      if ((Include = nil) or     TClass(FClasses[I]).InheritsFrom(Include)) and
         ((Exclude = nil) or not TClass(FClasses[I]).InheritsFrom(Exclude)) and
          Callback(UserData, FClasses[I]) then
      begin
        Result := FClasses[I];
        Break;
      end;
end;

constructor TDECObject.Create;
begin
  inherited Create;
end;

class function TDECObject.Identity: LongWord;
var
  Signature: String;
begin
  Signature := StringOfChar(#$5A, 256 - Length(Classname)) + UpperCase(ClassName);
  Result := CRC32(IdentityBase, Signature[1], Length(Signature) * SizeOf(Signature[1]));
end;

class procedure TDECObject.Register;
var
  I: Integer;
  Found: Boolean;
  ID: LongWord;
begin
  if IsObject(FClasses, TList) then
  begin
    Found := False;
    ID := Identity;
    for I := 0 to FClasses.Count-1 do
      if FClasses[I] = Self then Found := True else
        if ID = TDECClass(FClasses[I]).Identity then 
          raise EDECException.CreateFmt(sWrongIdentity, [TDECClass(FClasses[I]).ClassName, ClassName]);
    if not Found then FClasses.Add(Self);
  end;
end;

// override FreeInstance to fillout allocated Object with Zeros
// that is safer for any access to invalid Pointers of any released Object
// WE WANT SECURITY !!!
procedure TDECObject.FreeInstance;
asm
      PUSH    EBX
      PUSH    EDI
      MOV     EBX,EAX
      CALL    TObject.CleanupInstance
      MOV     EAX,[EBX]
      CALL    TObject.InstanceSize
      MOV     ECX,EAX
      MOV     EDI,EBX
      XOR     EAX,EAX
      REP     STOSB
      MOV     EAX,EBX
      CALL    System.@FreeMem
      POP     EDI
      POP     EBX
end;

function IsObject(AObject: Pointer; AClass: TClass): Boolean;
// Relacement of "is" Operator for safer access/check if AObject is AClass

  function IsClass(AObject: Pointer; AClass: TClass): Boolean;
  asm  // safer replacement for Delphi's "is" operator
  @@1:    TEST    EAX,EAX
          JE      @@3
          MOV     EAX,[EAX]
          TEST    EAX,EAX
          JE      @@3
          CMP     EAX,EDX
          JE      @@2
          MOV     EAX,[EAX].vmtParent
          JMP     @@1
  @@2:    MOV     EAX,1
  @@3:
  end;
  
begin
  Result := False;
  if AObject <> nil then
  try
    Result := IsClass(AObject, AClass);
  except
  end;
end;

function MemCompare(P1, P2: Pointer; Size: Integer): Integer;
asm //equal to StrLComp(P1, P2, Size), but always Size Bytes are checked
       PUSH    ESI
       PUSH    EDI
       MOV     ESI,P1
       MOV     EDI,P2
       XOR     EAX,EAX
       REPE    CMPSB
       JE      @@1
       MOVZX   EAX,BYTE PTR [ESI-1]
       MOVZX   EDX,BYTE PTR [EDI-1]
       SUB     EAX,EDX
@@1:   POP     EDI
       POP     ESI
end;

procedure XORBuffers(const Source1, Source2; Size: Integer; var Dest);
asm // Dest^ =  Source1^ xor Source2^ , Size bytes
       AND   ECX,ECX
       JZ    @@5
       PUSH  ESI
       PUSH  EDI
       MOV   ESI,EAX
       MOV   EDI,Dest
@@1:   TEST  ECX,3
       JNZ   @@3
@@2:   SUB   ECX,4
       JL    @@4
       MOV   EAX,[ESI + ECX]
       XOR   EAX,[EDX + ECX]
       MOV   [EDI + ECX],EAX
       JMP   @@2
@@3:   DEC   ECX
       MOV   AL,[ESI + ECX]
       XOR   AL,[EDX + ECX]
       MOV   [EDI + ECX],AL
       JMP   @@1
@@4:   POP   EDI
       POP   ESI
@@5:                           
end;

// wipe
const
  WipeCount = 4;
  WipeBytes : array[0..WipeCount -1] of Byte = ($55, $AA, $FF, $00);

procedure ProtectBuffer(var Buffer; Size: Integer);
var
  Count: Integer;
begin
  if Size > 0 then
    for Count := 0 to WipeCount -1 do
      FillChar(Buffer, Size, WipeBytes[Count]);
end;

procedure ProtectString(var Value: String);
begin
  UniqueString(Value);
  ProtectBuffer(Pointer(Value)^, Length(Value));
  Value := '';
end;

procedure ProtectBinary(var Value: Binary);
begin
  UniqueString(AnsiString(Value));
  ProtectBuffer(Pointer(Value)^, Length(Value));
  Value := '';
end;

procedure ProtectStream(Stream: TStream; Size: Integer = 0);
const
  BufferSize = 512;
var
  Buffer: String;
  Count,Bytes,DataSize: Integer;
  Position: Integer;
begin
  if IsObject(Stream, TStream) then
  begin
    Position := Stream.Position;
    DataSize := Stream.Size;
    if Size <= 0 then
    begin
      Size := DataSize;
      Position := 0;
    end else
    begin
      Dec(DataSize, Position);
      if Size > DataSize then Size := DataSize;
    end;
    SetLength(Buffer, BufferSize);
    for Count := 0 to WipeCount -1 do
    begin
      Stream.Position := Position;
      DataSize := Size;
      FillChar(Buffer[1], BufferSize, WipeBytes[Count]);
      while DataSize > 0 do
      begin
        Bytes := DataSize;
        if Bytes > BufferSize then Bytes := BufferSize;
        Stream.Write(Buffer[1], Bytes);
        Dec(DataSize, Bytes);
      end;
    end;
  end;
end;

function IsFilledWith(var Buffer; Size: Integer; Value: Char): Boolean;
asm // check if Buffer is filled with Size of bytes with Value
       TEST   EAX,EAX
       JZ     @@1
       PUSH   EDI
       MOV    EDI,EAX
       MOV    EAX,ECX
       MOV    ECX,EDX
       REPE   SCASB
       SETE   AL
       POP    EDI
@@1:
end;

procedure FoldBuf(var Dest; DestSize: Integer; const Source; SourceSize: Integer);
var
  I: Integer;
  S,D: PByteArray;
begin
  if (DestSize <= 0) or (SourceSize <= 0) then Exit;
  S := PByteArray(@Source);
  D := PByteArray(@Dest);
  if SourceSize > DestSize then
  begin
    FillChar(D^, DestSize, 0);
    for I := 0 to SourceSize-1 do
      D[I mod DestSize] := D[I mod DestSize] + S[I];
  end else
  begin
    while DestSize > SourceSize do
    begin
      Move(S^, D^, SourceSize);
      Dec(DestSize, SourceSize);
      Inc(PChar(D), SourceSize);
    end;
    Move(S^, D^, DestSize);
  end;
end;

procedure FoldStr(var Dest; DestSize: Integer; const Source: String);
begin
  FoldBuf(Dest, DestSize, PChar(Source)^, Length(Source));
end;

// random
var
  FRndSeed: Cardinal = 0;

function DoRndBuffer(Seed: Cardinal; var Buffer; Size: Integer): Cardinal;
// same as Borlands Random
asm
      AND     EDX,EDX
      JZ      @@2
      AND     ECX,ECX
      JLE     @@2
      PUSH    EBX
@@1:  IMUL    EAX,EAX,134775813
      INC     EAX
      MOV     EBX,EAX
      SHR     EBX,24
      MOV     [EDX],BL
      INC     EDX
      DEC     ECX
      JNZ     @@1
      POP     EBX
@@2:
end;

function RandomSystemTime: Cardinal;
// create Seed from Systemtime and PerformanceCounter
var
  SysTime: record
             Year: Word;
             Month: Word;
             DayOfWeek: Word;
             Day: Word;
             Hour: Word;
             Minute: Word;
             Second: Word;
             MilliSeconds: Word;
             Reserved: array [0..7] of Byte;
           end;
  Counter: record
             Lo,Hi: Integer;
           end;
asm
         LEA    EAX,SysTime
         PUSH   EAX
         CALL   GetSystemTime
         MOVZX  EAX,Word Ptr SysTime.Hour
         IMUL   EAX,60
         ADD    AX,SysTime.Minute
         IMUL   EAX,60
         MOVZX  ECX,Word Ptr SysTime.Second
         ADD    EAX,ECX
         IMUL   EAX,1000
         MOV    CX,SysTime.MilliSeconds
         ADD    EAX,ECX
         PUSH   EAX
         LEA    EAX,Counter
         PUSH   EAX
         CALL   QueryPerformanceCounter
         POP    EAX
         ADD    EAX,Counter.Hi
         ADC    EAX,Counter.Lo
end;

function RandomBinary(Size: Integer): Binary;
begin
  SetLength(Result, Size);
  RandomBuffer(Result[1], Size);
end;

procedure RandomBuffer(var Buffer; Size: Integer);
begin
  if Assigned(DoRandomBuffer) then DoRandomBuffer(Buffer, Size)
    else FRndSeed := DoRndBuffer(FRndSeed, Buffer, Size);
end;

function RandomLong: LongWord;
begin
  RandomBuffer(Result, SizeOf(Result));
end;

procedure RandomSeed(const Buffer; Size: Integer);
begin
  if Assigned(DoRandomSeed) then DoRandomSeed(Buffer, Size) else
    if Size >= 0 then
    begin
      FRndSeed := 0;
      while Size > 0 do
      begin
        Dec(Size);
        FRndSeed := (FRndSeed shl 8 + FRndSeed shr 24) xor TByteArray(Buffer)[Size]
      end;
    end else FRndSeed := RandomSystemTime;
end;

procedure RandomSeed;
begin
  RandomSeed('', -1);
end;

procedure SwapBytes(var Buffer; BufferSize: Integer);
asm
       CMP    EDX,1
       JLE    @@3
       AND    EAX,EAX
       JZ     @@3
       PUSH   EBX
       MOV    ECX,EDX
       LEA    EDX,[EAX + ECX -1]
       SHR    ECX,1
@@1:   MOV    BL,[EAX]
       XCHG   BL,[EDX]
       DEC    EDX
       MOV    [EAX],BL
       INC    EAX
       DEC    ECX
       JNZ    @@1
@@2:   POP    EBX
@@3:
end;

function SwapLong(Value: LongWord): LongWord;
{$IFDEF UseASM}
  {$IFDEF 486GE}
    {$DEFINE SwapLong_asm}
  {$ENDIF}
{$ENDIF}
{$IFDEF SwapLong_asm}
asm
       BSWAP  EAX
end;
{$ELSE}
begin
  Result := Value shl 24 or Value shr 24 or Value shl 8 and $00FF0000 or Value shr 8 and $0000FF00;
end;
{$ENDIF}

procedure SwapLongBuffer(const Source; var Dest; Count: Integer);
{$IFDEF UseASM}
  {$IFDEF 486GE}
    {$DEFINE SwapLongBuffer_asm}
  {$ENDIF}
{$ENDIF}
{$IFDEF SwapLongBuffer_asm}
asm
       TEST   ECX,ECX
       JLE    @Exit
       PUSH   EDI
       SUB    EAX,4
       SUB    EDX,4
@@1:   MOV    EDI,[EAX + ECX * 4]
       BSWAP  EDI
       MOV    [EDX + ECX * 4],EDI
       DEC    ECX
       JNZ    @@1
       POP    EDI
@Exit:
end;
{$ELSE}
var
  I: Integer;
  T: LongWord;
begin
  for I := 0 to Count -1 do
  begin
    T := TLongArray(Source)[I];
    TLongArray(Dest)[I] := (T shl 24) or (T shr 24) or ((T shl 8) and $00FF0000) or ((T shr 8) and $0000FF00);
  end;
end;
{$ENDIF}

function SwapInt64(const Value: Int64): Int64;
{$IFDEF UseASM}
  {$IFDEF 486GE}
    {$DEFINE SwapInt64_asm}
  {$ENDIF}
{$ENDIF}
{$IFDEF SwapInt64_asm}
asm
       MOV    EDX,Value.DWord[0]
       MOV    EAX,Value.DWord[4]
       BSWAP  EDX
       BSWAP  EAX
end;
{$ELSE}
var
  L,H: LongWord;
begin
  L := Int64Rec(Value).Lo;
  H := Int64Rec(Value).Hi;
  L := L shl 24 or L shr 24 or L shl 8 and $00FF0000 or L shr 8 and $0000FF00;
  H := H shl 24 or H shr 24 or H shl 8 and $00FF0000 or H shr 8 and $0000FF00;
  Int64Rec(Result).Hi := L;
  Int64Rec(Result).Lo := H;
end;
{$ENDIF}

procedure SwapInt64Buffer(const Source; var Dest; Count: Integer);
{$IFDEF UseASM}
  {$IFDEF 486GE}
    {$DEFINE SwapInt64Buffer_asm}
  {$ENDIF}
{$ENDIF}
{$IFDEF SwapInt64Buffer_asm}
asm
       TEST   ECX,ECX
       JLE    @Exit
       PUSH   ESI
       PUSH   EDI
       LEA    ESI,[EAX + ECX * 8]
       LEA    EDI,[EDX + ECX * 8]
       NEG    ECX
@@1:   MOV    EAX,[ESI + ECX * 8]
       MOV    EDX,[ESI + ECX * 8 + 4]
       BSWAP  EAX
       BSWAP  EDX
       MOV    [EDI + ECX * 8 + 4],EAX
       MOV    [EDI + ECX * 8],EDX
       INC    ECX
       JNZ    @@1
       POP    EDI
       POP    ESI
@Exit:
end;
{$ELSE}
var
  I: Integer;
  H,L: LongWord;
begin
  for I := 0 to Count -1 do
  begin
   H := TLongArray(Source)[I * 2    ];
   L := TLongArray(Source)[I * 2 + 1];
   TLongArray(Dest)[I * 2    ] := L shl 24 or L shr 24 or L shl 8 and $00FF0000 or L shr 8 and $0000FF00;
   TLongArray(Dest)[I * 2 + 1] := H shl 24 or H shr 24 or H shl 8 and $00FF0000 or H shr 8 and $0000FF00;
  end;
end;
{$ENDIF}

{reverse the bit order from a integer}
function SwapBits(Value, Bits: LongWord): LongWord;
{$IFDEF UseASM}
  {$IFDEF 486GE}
    {$DEFINE SwapBits_asm}
  {$ENDIF}
{$ENDIF}
{$IFDEF SwapBits_asm}
asm
       BSWAP  EAX
       MOV    ECX,EAX
       AND    EAX,0AAAAAAAAh
       SHR    EAX,1
       AND    ECX,055555555h
       SHL    ECX,1
       OR     EAX,ECX
       MOV    ECX,EAX
       AND    EAX,0CCCCCCCCh
       SHR    EAX,2
       AND    ECX,033333333h
       SHL    ECX,2
       OR     EAX,ECX
       MOV    ECX,EAX
       AND    EAX,0F0F0F0F0h
       SHR    EAX,4
       AND    ECX,00F0F0F0Fh
       SHL    ECX,4
       OR     EAX,ECX
       AND    EDX,01Fh
       JZ     @@1
       MOV    ECX,32
       SUB    ECX,EDX
       SHR    EAX,CL
@@1:
end;
{$ELSE}
{$ENDIF}

{$IFDEF VER_D3H}
procedure ModuleUnload(Instance: Integer);
var // automaticaly deregistration/releasing
  I: Integer;
begin
  if IsObject(FClasses, TList) then
    for I := FClasses.Count -1 downto 0 do
      if Integer(FindClassHInstance(TClass(FClasses[I]))) = Instance then
        FClasses.Delete(I);
end;

initialization
 // AddModuleUnloadProc(ModuleUnload);
{$ELSE}
initialization
{$ENDIF}
  FClasses := TList.Create;
finalization
{$IFDEF VER_D3H}
 // RemoveModuleUnloadProc(ModuleUnload);
{$ENDIF}
  FreeAndNil(FClasses);
end.
