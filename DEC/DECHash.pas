{*****************************************************************************

  Delphi Encryption Compendium (DEC Part I)
  Version 5.2, Part I, for Delphi 7 - 2009

  Remarks:          Freeware, Copyright must be included

  Original Author:  (c) 2006 Hagen Reddmann, HaReddmann [at] T-Online [dot] de
  Modifications:    (c) 2008 Arvid Winkelsdorf, info [at] digivendo [dot] de

  Last change:      02. November 2008

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

unit DECHash;

{$WARNINGS OFF}
{$RANGECHECKS OFF}

interface

uses SysUtils, Classes, DECUtil, DECFmt;

{$I VER.INC}
                                          
type
{all Hash Classes}
  THash_MD2             = class;  {$DEFINE THash_MD2_asm}
  THash_MD4             = class;  {$DEFINE THash_MD4_asm}
  THash_MD5             = class;  {$DEFINE THash_MD5_asm}
  THash_RipeMD128       = class;  {$DEFINE THash_RipeMD128_asm}
  THash_RipeMD160       = class;  {$DEFINE THash_RipeMD160_asm}
  THash_RipeMD256       = class;  {$DEFINE THash_RipeMD256_asm}
  THash_RipeMD320       = class;  {$DEFINE THash_RipeMD320_asm}
  THash_SHA             = class;  {$DEFINE THash_SHA_asm}
  THash_SHA1            = class;
  THash_SHA256          = class;  {$DEFINE THash_SHA256_asm}
  THash_SHA384          = class;  {$DEFINE THash_SHA384_asm}
  THash_SHA512          = class;
  THash_Haval128        = class;  {$DEFINE THashBaseHaval_asm}
  THash_Haval160        = class;  // Haval 160, 3 Rounds
  THash_Haval192        = class;  // Haval 192, 4 Rounds
  THash_Haval224        = class;  // Haval 224, 4 Rounds
  THash_Haval256        = class;  // Haval 256, 5 Rounds
  THash_Tiger           = class;  {$DEFINE THash_Tiger_asm}
  THash_Panama          = class;  {$DEFINE THash_Panama_asm}
  THash_Whirlpool       = class;  {$DEFINE THashBaseWhirlpool_asm}
  THash_Whirlpool1      = class;
  THash_Square          = class;  {$DEFINE THash_Square_asm}
  THash_Snefru128       = class;  {$DEFINE THash_Snefru128_asm}
  THash_Snefru256       = class;  {$DEFINE THash_Snefru256_asm}
  THash_Sapphire        = class;  {$DEFINE THash_Sapphire_asm}

  TDECHashClass = class of TDECHash;

  TDECHash = class(TDECObject)
  protected
    FCount: array[0..7] of LongWord;
    FBuffer: PByteArray;
    FBufferSize: Integer;
    FBufferIndex: Integer;
    FPaddingByte: Byte;
    procedure DoTransform(Buffer: PLongArray); virtual; abstract;
    procedure DoInit; virtual; abstract;
    procedure DoDone; virtual; abstract;
  public
    destructor Destroy; override;

    procedure Init;
    procedure Calc(const Data; DataSize: Integer); virtual;
    procedure Done;

    function Digest: PByteArray; virtual; abstract;
    function DigestStr(Format: TDECFormatClass = nil): Binary; virtual;

    class function DigestSize: Integer; virtual; abstract;
    class function BlockSize: Integer; virtual; abstract;

    class function CalcBuffer(const Buffer; BufferSize: Integer; Format: TDECFormatClass = nil): Binary;
    class function CalcStream(const Stream: TStream; Size: Int64; Format: TDECFormatClass = nil; const Progress: IDECProgress = nil): Binary;
    class function CalcBinary(const Data: Binary; Format: TDECFormatClass = nil): Binary;
    class function CalcFile(const FileName: String; Format: TDECFormatClass = nil; const Progress: IDECProgress = nil): Binary;

    class function MGF1(const Data: Binary; MaskSize: Integer; Format: TDECFormatClass = nil): Binary; overload;
    class function MGF1(const Data; DataSize, MaskSize: Integer; Format: TDECFormatClass = nil): Binary; overload;
    class function KDF2(const Data,Seed: Binary; MaskSize: Integer; Format: TDECFormatClass = nil): Binary; overload;
    class function KDF2(const Data; DataSize: Integer; const Seed; SeedSize, MaskSize: Integer; Format: TDECFormatClass = nil): Binary; overload;
   // DEC's own KDF+MGF
    class function MGFx(const Data; DataSize, MaskSize: Integer; Format: TDECFormatClass = nil; Index: LongWord = 1): Binary; overload;
    class function MGFx(const Data: Binary; MaskSize: Integer; Format: TDECFormatClass = nil; Index: LongWord = 1): Binary; overload;
    class function KDFx(const Data,Seed: Binary; MaskSize: Integer; Format: TDECFormatClass = nil; Index: LongWord = 1): Binary; overload;
    class function KDFx(const Data; DataSize: Integer; const Seed; SeedSize, MaskSize: Integer; Format: TDECFormatClass = nil; Index: LongWord = 1): Binary; overload;
  published
    property PaddingByte: Byte read FPaddingByte write FPaddingByte;
  end;

  THash_MD2 = class(TDECHash)
  private
    FDigest: array[0..63] of Byte;
  protected
    procedure DoTransform(Buffer: PLongArray); override;
    procedure DoInit; override;
    procedure DoDone; override;
  public
    class function DigestSize: Integer; override;
    class function BlockSize: Integer; override;
    function Digest: PByteArray; override;
  end;

  THashBaseMD4 = class(TDECHash)
  private
    FDigest: array[0..9] of LongWord;
  protected
    procedure DoInit; override;
    procedure DoDone; override;
  public
    class function DigestSize: Integer; override;
    class function BlockSize: Integer; override;
    function Digest: PByteArray; override;
  end;

  THash_MD4 = class(THashBaseMD4)
  protected
    procedure DoTransform(Buffer: PLongArray); override;
  end;

  THash_MD5 = class(THashBaseMD4)
  protected
    procedure DoTransform(Buffer: PLongArray); override;
  end;

  THash_RipeMD128 = class(THashBaseMD4)
  protected
    procedure DoTransform(Buffer: PLongArray); override;
  end;

  THash_RipeMD160 = class(THashBaseMD4)
  protected
    procedure DoTransform(Buffer: PLongArray); override;
  public
    class function DigestSize: Integer; override;
  end;

  THash_RipeMD256 = class(THashBaseMD4)
  protected
    procedure DoTransform(Buffer: PLongArray); override;
    procedure DoInit; override;
  public
    class function DigestSize: Integer; override;
  end;

  THash_RipeMD320 = class(THashBaseMD4)
  protected
    procedure DoTransform(Buffer: PLongArray); override;
  public
    class function DigestSize: Integer; override;
  end;

  THash_SHA = class(THashBaseMD4)
  protected
    procedure DoTransform(Buffer: PLongArray); override;
    procedure DoDone; override;
  public
    class function DigestSize: Integer; override;
  end;

  THash_SHA1 = class(THash_SHA);

  THash_SHA256 = class(THash_SHA)
  protected
    procedure DoTransform(Buffer: PLongArray); override;
    procedure DoInit; override;
  public
    class function DigestSize: Integer; override;
  end;

  THash_SHA384 = class(TDECHash)
  private
    FDigest: array[0..7] of Int64;
  protected
    procedure DoTransform(Buffer: PLongArray); override;
    procedure DoInit; override;
    procedure DoDone; override;
  public
    class function DigestSize: Integer; override;
    class function BlockSize: Integer; override; // 128
    function Digest: PByteArray; override;
  end;

  THash_SHA512 = class(THash_SHA384)
  protected
    procedure DoInit; override;
  public
    class function DigestSize: Integer; override;
  end;

  THashBaseHaval = class(TDECHash)
  private
    FDigest: array[0..7] of LongWord;
    FRounds: Integer; {3 - 5}
    FTransform: procedure(Buffer: PLongArray) of object;
    procedure SetRounds(Value: Integer);
  protected
    procedure DoTransform (Buffer: PLongArray); override;
    procedure DoTransform3(Buffer: PLongArray);
    procedure DoTransform4(Buffer: PLongArray);
    procedure DoTransform5(Buffer: PLongArray);
    procedure DoInit; override;
    procedure DoDone; override;
  public
    class function BlockSize: Integer; override;
    function Digest: PByteArray; override;
  published
    property Rounds: Integer read FRounds write SetRounds;
  end;

  THash_Haval128 = class(THashBaseHaval)
  public
    class function DigestSize: Integer; override;
  end;

  THash_Haval160 = class(THashBaseHaval)
  public
    class function DigestSize: Integer; override;
  end;

  THash_Haval192 = class(THashBaseHaval)
  public
    class function DigestSize: Integer; override;
  end;

  THash_Haval224 = class(THashBaseHaval)
  public
    class function DigestSize: Integer; override;
  end;

  THash_Haval256 = class(THashBaseHaval)
  public
    class function DigestSize: Integer; override;
  end;

  THash_Tiger = class(THashBaseMD4)
  private
    FRounds: Integer;
    procedure SetRounds(Value: Integer);
  protected
    procedure DoTransform(Buffer: PLongArray); override;
    procedure DoInit; override;
  public
    class function DigestSize: Integer; override;
  published
    property Rounds: Integer read FRounds write SetRounds;
  end;

  THash_Panama = class(TDECHash)
  private
    FLFSRBuffer: array[0..31, 0..7] of LongWord;
    FDigest: array[0..16] of LongWord;
    FTap: LongWord;
  protected
    procedure DoInit; override;
    procedure DoDone; override;
    procedure DoPull;
    procedure DoTransform(Buffer: PLongArray); override;
  public
    class function DigestSize: Integer; override;
    class function BlockSize: Integer; override; // 32
    function Digest: PByteArray; override;
  end;

  THashBaseWhirlpool = class(TDECHash)
  private
    FDigest: array[0..15] of LongWord;
    FTableC: Pointer;
    FTableR: Pointer;
  protected
    procedure DoTransform(Buffer: PLongArray); override;
    procedure DoDone; override;
  public
    class function DigestSize: Integer; override;
    class function BlockSize: Integer; override;
    function Digest: PByteArray; override;
  end;

  THash_Whirlpool = class(THashBaseWhirlpool)
  protected
    procedure DoInit; override;
  end;

  THash_Whirlpool1 = class(THashBaseWhirlpool)
  protected
    procedure DoInit; override;
  end;

  THash_Square = class(TDECHash)
  private
    FDigest: array[0..3] of LongWord;
  protected
    procedure DoInit; override;
    procedure DoDone; override;
    procedure DoTransform(Buffer: PLongArray); override;
  public
    class function DigestSize: Integer; override;
    class function BlockSize: Integer; override; 
    function Digest: PByteArray; override;
  end;

  THashBaseSnefru = class(TDECHash)  {"derived from the Xerox Secure Hash Function"}
  private
    FDigest: array[0..23] of LongWord;
    FSecurity_Level: Integer;
    procedure SetSecurity_Level(Value: Integer);
  protected
    procedure DoInit; override;
    procedure DoDone; override;
  public
    function Digest: PByteArray; override;
  published
    property Security_Level: Integer read FSecurity_Level write SetSecurity_Level; // can set from 2 to 8, default is 8
  end;

  THash_Snefru128 = class(THashBaseSnefru)
  protected
    procedure DoTransform(Buffer: PLongArray); override;
  public
    class function DigestSize: Integer; override;
    class function BlockSize: Integer; override; // 48
  end;

  THash_Snefru256 = class(THashBaseSnefru)
  protected
    procedure DoTransform(Buffer: PLongArray); override;
  public
    class function DigestSize: Integer; override;
    class function BlockSize: Integer; override;  // 32
  end;

  THash_Sapphire = class(TDECHash)
  private
    FCards: array[0..255] of LongWord;
    FDigest: array[0..15] of LongWord;
    FRotor: LongWord;                 // don't change order
    FRatchet: LongWord;
    FAvalanche: LongWord;
    FPlain: LongWord;
    FCipher: LongWord;
    FDigestSize: Integer;
  protected
    procedure DoInit; override;
    procedure DoDone; override;
  public
    class function BlockSize: Integer; override;
    class function DigestSize: Integer; override;
    function Digest: PByteArray; override;
    function DigestStr(Format: TDECFormatClass = nil): Binary; override;
    procedure Calc(const Data; DataSize: Integer); override;
  published
    property RequestedDigestSize: Integer read FDigestSize write FDigestSize;
  end;

function  ValidHash(HashClass: TDECHashClass = nil): TDECHashClass;
function  HashByName(const Name: String): TDECHashClass;
function  HashByIdentity(Identity: LongWord): TDECHashClass;
procedure SetDefaultHashClass(HashClass: TDECHashClass);

var
  StreamBufferSize: Integer = 8192;

implementation

uses DECData;

{$I *.inc}

{                                        assembler                             pascal
THash_SHA512        :       85.1 cycles/byte      17.62 Mb/sec      220.9 cycles/byte       6.79 Mb/sec  159%
THash_SHA384        :       85.2 cycles/byte      17.61 Mb/sec      220.0 cycles/byte       6.82 Mb/sec  158%
THash_Tiger         :       24.6 cycles/byte      60.98 Mb/sec       60.7 cycles/byte      24.69 Mb/sec  147%
THash_Haval128      :       13.3 cycles/byte     112.55 Mb/sec       26.0 cycles/byte      57.77 Mb/sec   95%
THash_SHA1          :       20.1 cycles/byte      74.80 Mb/sec       36.1 cycles/byte      41.51 Mb/sec   80%
THash_SHA           :       20.0 cycles/byte      75.03 Mb/sec       35.5 cycles/byte      42.21 Mb/sec   78%
THash_Haval160      :       13.2 cycles/byte     113.30 Mb/sec       22.7 cycles/byte      66.12 Mb/sec   71%
THash_Haval256      :       25.9 cycles/byte      57.84 Mb/sec       40.5 cycles/byte      37.07 Mb/sec   56%
THash_Snefru128     :      159.7 cycles/byte       9.39 Mb/sec      248.2 cycles/byte       6.04 Mb/sec   55%
THash_Snefru256     :      239.3 cycles/byte       6.27 Mb/sec      367.9 cycles/byte       4.08 Mb/sec   54%
THash_RipeMD256     :       14.5 cycles/byte     103.16 Mb/sec       21.4 cycles/byte      70.08 Mb/sec   47%
THash_MD4           :        5.8 cycles/byte     256.73 Mb/sec        8.5 cycles/byte     176.92 Mb/sec   45%

THash_MD2           :      251.6 cycles/byte       5.96 Mb/sec      366.1 cycles/byte       4.10 Mb/sec   45%
THash_RipeMD128     :       15.2 cycles/byte      98.89 Mb/sec       21.2 cycles/byte      70.61 Mb/sec   40%
THash_RipeMD320     :       25.5 cycles/byte      58.73 Mb/sec       35.8 cycles/byte      41.87 Mb/sec   40%
THash_MD5           :        8.9 cycles/byte     169.43 Mb/sec       11.4 cycles/byte     131.01 Mb/sec   29%
THash_RipeMD160     :       26.5 cycles/byte      56.66 Mb/sec       31.4 cycles/byte      47.79 Mb/sec   19%
THash_Square        :       44.7 cycles/byte      33.58 Mb/sec       53.1 cycles/byte      28.23 Mb/sec   19%
THash_Haval192      :       32.5 cycles/byte      46.17 Mb/sec       37.6 cycles/byte      39.87 Mb/sec   18%
THash_Whirlpool1    :      104.9 cycles/byte      14.30 Mb/sec      122.8 cycles/byte      12.22 Mb/sec   17%
THash_Whirlpool     :      104.7 cycles/byte      14.33 Mb/sec      119.9 cycles/byte      12.51 Mb/sec   15%
THash_Sapphire      :       52.9 cycles/byte      28.35 Mb/sec       53.8 cycles/byte      27.86 Mb/sec    2%
THash_Haval224      :       32.0 cycles/byte      46.82 Mb/sec       32.3 cycles/byte      46.46 Mb/sec    1%
THash_SHA256        :       47.8 cycles/byte      31.35 Mb/sec       47.8 cycles/byte      31.39 Mb/sec    0%
THash_Panama        :        8.9 cycles/byte     169.01 Mb/sec        7.3 cycles/byte     206.55 Mb/sec  -18%
}

resourcestring
  sHashingOverflowError = 'Hash function have to many bits processed';
  sHashNotInitialized   = 'Hash must be initialized';
  sHashNoDefault        = 'No default hash are registered';

var
  FDefaultHashClass: TDECHashClass = nil;


function ValidHash(HashClass: TDECHashClass): TDECHashClass;
begin
  if HashClass <> nil then Result := HashClass
    else Result := FDefaultHashClass;
  if Result = nil then raise EDECException.Create(sHashNoDefault);
end;

function HashByName(const Name: String): TDECHashClass;
begin
  Result := TDECHashClass(DECClassByName(Name, TDECHash));
end;

function HashByIdentity(Identity: LongWord): TDECHashClass;
begin
  Result := TDECHashClass(DECClassByIdentity(Identity, TDECHash));
end;

procedure SetDefaultHashClass(HashClass: TDECHashClass);
begin
  if HashClass <> nil then HashClass.Register;
  FDefaultHashClass := HashClass;
end;

// .TDECHash
destructor TDECHash.Destroy;
begin
  ProtectBuffer(Digest^, DigestSize);
  ProtectBuffer(FBuffer^, FBufferSize);
  ReallocMem(FBuffer, 0);
  inherited Destroy;
end;

procedure TDECHash.Init;
begin
  FBufferIndex := 0;
  FBufferSize := BlockSize;
  ReallocMem(FBuffer, FBufferSize);
  FillChar(FBuffer^, FBufferSize, 0);
  FillChar(FCount, SizeOf(FCount), 0);
  DoInit;
end;

procedure TDECHash.Done;
begin
  DoDone;
  ProtectBuffer(FBuffer^, FBufferSize);
  FBufferSize := 0;
  ReallocMem(FBuffer, 0);
end;

procedure HashingOverflowError;
begin
  raise EDECException.Create(sHashingOverflowError);
end;

procedure HashNotInitialized;
begin
  raise EDECException.Create(sHashNotInitialized);
end;

procedure Increment8(var Value; Add: LongWord);
// Value := Value + 8 * Add
// Value: array[0..7] of LongWord
asm
    MOV  ECX,EDX
    LEA  EDX,[EDX * 8]
    SHR  ECX,25
    ADD  [EAX].DWord[ 0],EDX
    ADC  [EAX].DWord[ 4],ECX
    ADC  [EAX].DWord[ 8],0
    ADC  [EAX].DWord[12],0
    ADC  [EAX].DWord[16],0
    ADC  [EAX].DWord[20],0
    ADC  [EAX].DWord[24],0
    ADC  [EAX].DWord[28],0
    JC   HashingOverflowError
end;

procedure TDECHash.Calc(const Data; DataSize: Integer);
var
  Remain: Integer;
  Source: PByte;
begin
  if DataSize <= 0 then Exit;
  if FBuffer = nil then HashNotInitialized;
  Increment8(FCount, DataSize);
  Source := @TByteArray(Data)[0];
  if FBufferIndex > 0 then
  begin
    Remain := FBufferSize - FBufferIndex;
    if DataSize < Remain then
    begin
      Move(Source^, FBuffer[FBufferIndex], DataSize);
      Inc(FBufferIndex, DataSize);
      Exit;
    end;
    Move(Source^, FBuffer[FBufferIndex], Remain);
    DoTransform(Pointer(FBuffer));
    Dec(DataSize, Remain);
    Inc(Source, Remain);
  end;
  while DataSize >= FBufferSize do
  begin
    DoTransform(Pointer(Source));
    Inc(Source, FBufferSize);
    Dec(DataSize, FBufferSize);
  end;
  Move(Source^, FBuffer^, DataSize);
  FBufferIndex := DataSize;
end;

function TDECHash.DigestStr(Format: TDECFormatClass): Binary;
begin
  Result := ValidFormat(Format).Encode(Digest[0], DigestSize);
end;

class function TDECHash.CalcStream(const Stream: TStream; Size: Int64; Format: TDECFormatClass; const Progress: IDECProgress): Binary;
var
  Buffer: Binary;
  Bytes: Integer;
  Min,Max,Pos: Int64;
begin
  Min := 0;
  Max := 0;
  with Create do
  try
    Init;
    if StreamBufferSize <= 0 then StreamBufferSize := 8192;
    if Size < 0 then
    begin
      Stream.Position := 0;
      Size := Stream.Size;
      Pos := 0;
    end else Pos := Stream.Position;
    Bytes := StreamBufferSize mod FBufferSize;
    if Bytes = 0 then Bytes := StreamBufferSize
      else Bytes := StreamBufferSize + FBufferSize - Bytes;
    if Bytes > Size then SetLength(Buffer, Size)
      else SetLength(Buffer, Bytes);
    Min := Pos;
    Max := Pos + Size;
    while Size > 0 do
    begin
      if Assigned(Progress) then Progress.Process(Min, Max, Pos);
      Bytes := Length(Buffer);
      if Bytes > Size then Bytes := Size;
      Stream.ReadBuffer(Buffer[1], Bytes);
      Calc(Buffer[1], Bytes);
      Dec(Size, Bytes);
      Inc(Pos, Bytes);
    end;
    Done;
    Result := DigestStr(Format);
  finally
    Free;
    ProtectBinary(Buffer);
    if Assigned(Progress) then Progress.Process(Min, Max, Max);
  end;
end;

class function TDECHash.CalcBinary(const Data: Binary; Format: TDECFormatClass): Binary;
begin
  Result := CalcBuffer(Data[1], Length(Data), Format);
end;

class function TDECHash.CalcFile(const FileName: String; Format: TDECFormatClass; const Progress: IDECProgress): Binary;
var
  S: TFileStream;
begin
  S := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Result := CalcStream(S, S.Size, Format, Progress);
  finally
    S.Free;
  end;
end;

class function TDECHash.CalcBuffer(const Buffer; BufferSize: Integer; Format: TDECFormatClass): Binary;
begin
  with Create do
  try
    Init;
    Calc(Buffer, BufferSize);
    Done;
    Result := DigestStr(Format);
  finally
    Free;
  end;
end;

class function TDECHash.MGF1(const Data; DataSize, MaskSize: Integer; Format: TDECFormatClass = nil): Binary;
// indexed Mask generation function, IEEE P1363 Working Group
// equal to KDF2 except without Seed
begin
  Result := KDF2(Data, DataSize, EmptyStr[1], 0, MaskSize, Format);
end;

class function TDECHash.MGF1(const Data: Binary; MaskSize: Integer; Format: TDECFormatClass = nil): Binary;
begin
  Result := KDF2(Data, Length(Data), EmptyStr[1], 0, MaskSize, Format);
end;

class function TDECHash.KDF2(const Data; DataSize: Integer; const Seed; SeedSize, MaskSize: Integer; Format: TDECFormatClass = nil): Binary;
// Key Generation Function 2, IEEE P1363 Working Group
var
  I,Rounds,DigestBytes: Integer;
  Dest: PByteArray;
  Count: LongWord;
begin
  DigestBytes := DigestSize;
  Assert(MaskSize >= 0);
  Assert(DataSize >= 0);
  Assert(SeedSize >= 0);
  Assert(DigestBytes >= 0);
  with Create do
  try
    Rounds := (MaskSize + DigestBytes -1) div DigestBytes;
    SetLength(Result, Rounds * DigestBytes);
    Dest := @Result[1];
    for I := 0 to Rounds -1 do
    begin
      Count := SwapLong(I);
      Init;
      Calc(Data, DataSize);
      Calc(Count, SizeOf(Count));
      Calc(Seed, SeedSize);
      Done;
      Move(Digest[0], Dest[I * DigestBytes], DigestBytes);
    end;
  finally
    Free;
  end;
  SetLength(Result, MaskSize);
  Result := ValidFormat(Format).Encode(Result[1], MaskSize);
end;

class function TDECHash.KDF2(const Data, Seed: Binary; MaskSize: Integer; Format: TDECFormatClass = nil): Binary;
begin
  Result := KDF2(Data[1], Length(Data), Seed[1], Length(Seed), MaskSize, Format);
end;

class function TDECHash.KDFx(const Data; DataSize: Integer; const Seed; SeedSize, MaskSize: Integer; Format: TDECFormatClass = nil; Index: LongWord = 1): Binary;
// DEC's own KDF, even stronger
var
  I,J: Integer;
  Count: LongWord;
  R: Byte;
begin
  Assert(MaskSize >= 0);
  Assert(DataSize >= 0);
  Assert(SeedSize >= 0);
  Assert(DigestSize >= 0);

  SetLength(Result, MaskSize);
  Index := SwapLong(Index);
  with Create do
  try
    for I := 0 to MaskSize -1 do
    begin
      Init;

      Count := SwapLong(I);
      Calc(Count, SizeOf(Count));
      Calc(Result[1], I);

      Calc(Index, SizeOf(Index));

      Count := SwapLong(SeedSize);
      Calc(Count, SizeOf(Count));
      Calc(Seed, SeedSize);

      Count := SwapLong(DataSize);
      Calc(Count, SizeOf(Count));
      Calc(Data, DataSize);

      Done;

      R := 0;
      for J := 0 to DigestSize -1 do
        R := R xor Digest[J];

      Result[I +1] := AnsiChar(R);
    end;
  finally
    Free;
  end;
  Result := ValidFormat(Format).Encode(Result[1], MaskSize);
end;

class function TDECHash.KDFx(const Data, Seed: Binary; MaskSize: Integer; Format: TDECFormatClass = nil; Index: LongWord = 1): Binary;
begin
  Result := KDFx(Data[1], Length(Data), Seed[1], Length(Seed), MaskSize, Format, Index);
end;

class function TDECHash.MGFx(const Data; DataSize, MaskSize: Integer; Format: TDECFormatClass = nil; Index: LongWord = 1): Binary;
begin
  Result := KDFx(Data, DataSize, EmptyStr[1], 0, MaskSize, Format, Index);
end;

class function TDECHash.MGFx(const Data: Binary; MaskSize: Integer; Format: TDECFormatClass = nil; Index: LongWord = 1): Binary;
begin
  Result := KDFx(Data[1], Length(Data), EmptyStr[1], 0, MaskSize, Format, Index);
end;

// .THash_MD2
class function THash_MD2.DigestSize: Integer;
begin
  Result := 16;
end;

class function THash_MD2.BlockSize: Integer;
begin
  Result := 16;
end;

function THash_MD2.Digest: PByteArray;
begin
  Result := @FDigest;
end;

{$IFNDEF THash_MD2_asm}
procedure THash_MD2.DoTransform(Buffer: PLongArray);
var
  I,J,T: LongWord;
begin
  for I := 0 to 3 do
  begin
    PLongArray(@FDigest[16])[I] := Buffer[I];
    PLongArray(@FDigest[32])[I] := PLongArray(@FDigest[0])[I] xor PLongArray(@FDigest[16])[I];
  end;
  T := FDigest[63];
  for I := 0 to 15 do
  begin
    T := FDigest[I + 48] xor MD2_PISubst[FDigest[I + 16] xor Byte(T)];
    FDigest[I + 48] := Byte(T);
  end;
  T := 0;
  for I := 0 to 17 do
  begin
    for J := 0 to 47 do
    begin
      T := FDigest[J] xor MD2_PISubst[T];
      FDigest[J] := Byte(T);
    end;
    T := (T + I) and $FF;
  end;
end;
{$ENDIF}

procedure THash_MD2.DoInit;
begin
  FillChar(FDigest, SizeOf(FDigest), 0);
end;

procedure THash_MD2.DoDone;
var
  Remain: Integer;
begin
  Remain := FBufferSize - FBufferIndex;
  FillChar(FBuffer[FBufferIndex], Remain, Remain);
  DoTransform(Pointer(FBuffer));
  Move(FDigest[48], FBuffer^, FBufferSize);
  DoTransform(Pointer(FBuffer));
end;

// .THashBaseMD4
class function THashBaseMD4.DigestSize: Integer;
begin
  Result := 16;
end;

class function THashBaseMD4.BlockSize: Integer;
begin
  Result := 64;
end;

function THashBaseMD4.Digest: PByteArray;
begin
  Result := @FDigest;
end;

procedure THashBaseMD4.DoInit;
begin
  FDigest[0] := $67452301;
  FDigest[1] := $EFCDAB89;
  FDigest[2] := $98BADCFE;
  FDigest[3] := $10325476;
  FDigest[4] := $C3D2E1F0;
  FDigest[5] := $76543210;
  FDigest[6] := $FEDCBA98;
  FDigest[7] := $89ABCDEF;
  FDigest[8] := $01234567;
  FDigest[9] := $3C2D1E0F;
end;

procedure THashBaseMD4.DoDone;
begin
  if FCount[2] or FCount[3] <> 0 then HashingOverflowError;
  if FPaddingByte = 0 then FPaddingByte := $80;
  FBuffer[FBufferIndex] := FPaddingByte;
  Inc(FBufferIndex);
  if FBufferIndex > FBufferSize - 8 then
  begin
    FillChar(FBuffer[FBufferIndex], FBufferSize - FBufferIndex, 0);
    DoTransform(Pointer(FBuffer));
    FBufferIndex := 0;
  end;
  FillChar(FBuffer[FBufferIndex], FBufferSize - FBufferIndex, 0);
  Move(FCount, FBuffer[FBufferSize - 8], 8);
  DoTransform(Pointer(FBuffer));
end;

// .THash_MD4
{$IFNDEF THash_MD4_asm}
procedure THash_MD4.DoTransform(Buffer: PLongArray);
const
  S1 = $5A827999;
  S2 = $6ED9EBA1;
var
  A,B,C,D: LongWord;
begin
  A := FDigest[0];
  B := FDigest[1];
  C := FDigest[2];
  D := FDigest[3];

  Inc(A, B and C or not B and D + Buffer[ 0]); A := A shl  3 or A shr 29;
  Inc(D, A and B or not A and C + Buffer[ 1]); D := D shl  7 or D shr 25;
  Inc(C, D and A or not D and B + Buffer[ 2]); C := C shl 11 or C shr 21;
  Inc(B, C and D or not C and A + Buffer[ 3]); B := B shl 19 or B shr 13;
  Inc(A, B and C or not B and D + Buffer[ 4]); A := A shl  3 or A shr 29;
  Inc(D, A and B or not A and C + Buffer[ 5]); D := D shl  7 or D shr 25;
  Inc(C, D and A or not D and B + Buffer[ 6]); C := C shl 11 or C shr 21;
  Inc(B, C and D or not C and A + Buffer[ 7]); B := B shl 19 or B shr 13;
  Inc(A, B and C or not B and D + Buffer[ 8]); A := A shl  3 or A shr 29;
  Inc(D, A and B or not A and C + Buffer[ 9]); D := D shl  7 or D shr 25;
  Inc(C, D and A or not D and B + Buffer[10]); C := C shl 11 or C shr 21;
  Inc(B, C and D or not C and A + Buffer[11]); B := B shl 19 or B shr 13;
  Inc(A, B and C or not B and D + Buffer[12]); A := A shl  3 or A shr 29;
  Inc(D, A and B or not A and C + Buffer[13]); D := D shl  7 or D shr 25;
  Inc(C, D and A or not D and B + Buffer[14]); C := C shl 11 or C shr 21;
  Inc(B, C and D or not C and A + Buffer[15]); B := B shl 19 or B shr 13;

  Inc(A, B and C or B and D or C and D + Buffer[ 0] + S1); A := A shl  3 or A shr 29;
  Inc(D, A and B or A and C or B and C + Buffer[ 4] + S1); D := D shl  5 or D shr 27;
  Inc(C, D and A or D and B or A and B + Buffer[ 8] + S1); C := C shl  9 or C shr 23;
  Inc(B, C and D or C and A or D and A + Buffer[12] + S1); B := B shl 13 or B shr 19;
  Inc(A, B and C or B and D or C and D + Buffer[ 1] + S1); A := A shl  3 or A shr 29;
  Inc(D, A and B or A and C or B and C + Buffer[ 5] + S1); D := D shl  5 or D shr 27;
  Inc(C, D and A or D and B or A and B + Buffer[ 9] + S1); C := C shl  9 or C shr 23;
  Inc(B, C and D or C and A or D and A + Buffer[13] + S1); B := B shl 13 or B shr 19;
  Inc(A, B and C or B and D or C and D + Buffer[ 2] + S1); A := A shl  3 or A shr 29;
  Inc(D, A and B or A and C or B and C + Buffer[ 6] + S1); D := D shl  5 or D shr 27;
  Inc(C, D and A or D and B or A and B + Buffer[10] + S1); C := C shl  9 or C shr 23;
  Inc(B, C and D or C and A or D and A + Buffer[14] + S1); B := B shl 13 or B shr 19;
  Inc(A, B and C or B and D or C and D + Buffer[ 3] + S1); A := A shl  3 or A shr 29;
  Inc(D, A and B or A and C or B and C + Buffer[ 7] + S1); D := D shl  5 or D shr 27;
  Inc(C, D and A or D and B or A and B + Buffer[11] + S1); C := C shl  9 or C shr 23;
  Inc(B, C and D or C and A or D and A + Buffer[15] + S1); B := B shl 13 or B shr 19;

  Inc(A, B xor C xor D + Buffer[ 0] + S2); A := A shl  3 or A shr 29;
  Inc(D, A xor B xor C + Buffer[ 8] + S2); D := D shl  9 or D shr 23;
  Inc(C, D xor A xor B + Buffer[ 4] + S2); C := C shl 11 or C shr 21;
  Inc(B, C xor D xor A + Buffer[12] + S2); B := B shl 15 or B shr 17;
  Inc(A, B xor C xor D + Buffer[ 2] + S2); A := A shl  3 or A shr 29;
  Inc(D, A xor B xor C + Buffer[10] + S2); D := D shl  9 or D shr 23;
  Inc(C, D xor A xor B + Buffer[ 6] + S2); C := C shl 11 or C shr 21;
  Inc(B, C xor D xor A + Buffer[14] + S2); B := B shl 15 or B shr 17;
  Inc(A, B xor C xor D + Buffer[ 1] + S2); A := A shl  3 or A shr 29;
  Inc(D, A xor B xor C + Buffer[ 9] + S2); D := D shl  9 or D shr 23;
  Inc(C, D xor A xor B + Buffer[ 5] + S2); C := C shl 11 or C shr 21;
  Inc(B, C xor D xor A + Buffer[13] + S2); B := B shl 15 or B shr 17;
  Inc(A, B xor C xor D + Buffer[ 3] + S2); A := A shl  3 or A shr 29;
  Inc(D, A xor B xor C + Buffer[11] + S2); D := D shl  9 or D shr 23;
  Inc(C, D xor A xor B + Buffer[ 7] + S2); C := C shl 11 or C shr 21;
  Inc(B, C xor D xor A + Buffer[15] + S2); B := B shl 15 or B shr 17;

  Inc(FDigest[0], A);
  Inc(FDigest[1], B);
  Inc(FDigest[2], C);
  Inc(FDigest[3], D);
end;
{$ENDIF}

// .THash_MD5
{$IFNDEF THash_MD5_asm}
procedure THash_MD5.DoTransform(Buffer: PLongArray);
var
  A,B,C,D: LongWord;
begin
  A := FDigest[0];
  B := FDigest[1];
  C := FDigest[2];
  D := FDigest[3];

  Inc(A, Buffer[ 0] + $D76AA478 + (D xor (B and (C xor D)))); A := A shl  7 or A shr 25 + B;
  Inc(D, Buffer[ 1] + $E8C7B756 + (C xor (A and (B xor C)))); D := D shl 12 or D shr 20 + A;
  Inc(C, Buffer[ 2] + $242070DB + (B xor (D and (A xor B)))); C := C shl 17 or C shr 15 + D;
  Inc(B, Buffer[ 3] + $C1BDCEEE + (A xor (C and (D xor A)))); B := B shl 22 or B shr 10 + C;
  Inc(A, Buffer[ 4] + $F57C0FAF + (D xor (B and (C xor D)))); A := A shl  7 or A shr 25 + B;
  Inc(D, Buffer[ 5] + $4787C62A + (C xor (A and (B xor C)))); D := D shl 12 or D shr 20 + A;
  Inc(C, Buffer[ 6] + $A8304613 + (B xor (D and (A xor B)))); C := C shl 17 or C shr 15 + D;
  Inc(B, Buffer[ 7] + $FD469501 + (A xor (C and (D xor A)))); B := B shl 22 or B shr 10 + C;
  Inc(A, Buffer[ 8] + $698098D8 + (D xor (B and (C xor D)))); A := A shl  7 or A shr 25 + B;
  Inc(D, Buffer[ 9] + $8B44F7AF + (C xor (A and (B xor C)))); D := D shl 12 or D shr 20 + A;
  Inc(C, Buffer[10] + $FFFF5BB1 + (B xor (D and (A xor B)))); C := C shl 17 or C shr 15 + D;
  Inc(B, Buffer[11] + $895CD7BE + (A xor (C and (D xor A)))); B := B shl 22 or B shr 10 + C;
  Inc(A, Buffer[12] + $6B901122 + (D xor (B and (C xor D)))); A := A shl  7 or A shr 25 + B;
  Inc(D, Buffer[13] + $FD987193 + (C xor (A and (B xor C)))); D := D shl 12 or D shr 20 + A;
  Inc(C, Buffer[14] + $A679438E + (B xor (D and (A xor B)))); C := C shl 17 or C shr 15 + D;
  Inc(B, Buffer[15] + $49B40821 + (A xor (C and (D xor A)))); B := B shl 22 or B shr 10 + C;

  Inc(A, Buffer[ 1] + $F61E2562 + (C xor (D and (B xor C)))); A := A shl  5 or A shr 27 + B;
  Inc(D, Buffer[ 6] + $C040B340 + (B xor (C and (A xor B)))); D := D shl  9 or D shr 23 + A;
  Inc(C, Buffer[11] + $265E5A51 + (A xor (B and (D xor A)))); C := C shl 14 or C shr 18 + D;
  Inc(B, Buffer[ 0] + $E9B6C7AA + (D xor (A and (C xor D)))); B := B shl 20 or B shr 12 + C;
  Inc(A, Buffer[ 5] + $D62F105D + (C xor (D and (B xor C)))); A := A shl  5 or A shr 27 + B;
  Inc(D, Buffer[10] + $02441453 + (B xor (C and (A xor B)))); D := D shl  9 or D shr 23 + A;
  Inc(C, Buffer[15] + $D8A1E681 + (A xor (B and (D xor A)))); C := C shl 14 or C shr 18 + D;
  Inc(B, Buffer[ 4] + $E7D3FBC8 + (D xor (A and (C xor D)))); B := B shl 20 or B shr 12 + C;
  Inc(A, Buffer[ 9] + $21E1CDE6 + (C xor (D and (B xor C)))); A := A shl  5 or A shr 27 + B;
  Inc(D, Buffer[14] + $C33707D6 + (B xor (C and (A xor B)))); D := D shl  9 or D shr 23 + A;
  Inc(C, Buffer[ 3] + $F4D50D87 + (A xor (B and (D xor A)))); C := C shl 14 or C shr 18 + D;
  Inc(B, Buffer[ 8] + $455A14ED + (D xor (A and (C xor D)))); B := B shl 20 or B shr 12 + C;
  Inc(A, Buffer[13] + $A9E3E905 + (C xor (D and (B xor C)))); A := A shl  5 or A shr 27 + B;
  Inc(D, Buffer[ 2] + $FCEFA3F8 + (B xor (C and (A xor B)))); D := D shl  9 or D shr 23 + A;
  Inc(C, Buffer[ 7] + $676F02D9 + (A xor (B and (D xor A)))); C := C shl 14 or C shr 18 + D;
  Inc(B, Buffer[12] + $8D2A4C8A + (D xor (A and (C xor D)))); B := B shl 20 or B shr 12 + C;

  Inc(A, Buffer[ 5] + $FFFA3942 + (B xor C xor D)); A := A shl  4 or A shr 28 + B;
  Inc(D, Buffer[ 8] + $8771F681 + (A xor B xor C)); D := D shl 11 or D shr 21 + A;
  Inc(C, Buffer[11] + $6D9D6122 + (D xor A xor B)); C := C shl 16 or C shr 16 + D;
  Inc(B, Buffer[14] + $FDE5380C + (C xor D xor A)); B := B shl 23 or B shr  9 + C;
  Inc(A, Buffer[ 1] + $A4BEEA44 + (B xor C xor D)); A := A shl  4 or A shr 28 + B;
  Inc(D, Buffer[ 4] + $4BDECFA9 + (A xor B xor C)); D := D shl 11 or D shr 21 + A;
  Inc(C, Buffer[ 7] + $F6BB4B60 + (D xor A xor B)); C := C shl 16 or C shr 16 + D;
  Inc(B, Buffer[10] + $BEBFBC70 + (C xor D xor A)); B := B shl 23 or B shr  9 + C;
  Inc(A, Buffer[13] + $289B7EC6 + (B xor C xor D)); A := A shl  4 or A shr 28 + B;
  Inc(D, Buffer[ 0] + $EAA127FA + (A xor B xor C)); D := D shl 11 or D shr 21 + A;
  Inc(C, Buffer[ 3] + $D4EF3085 + (D xor A xor B)); C := C shl 16 or C shr 16 + D;
  Inc(B, Buffer[ 6] + $04881D05 + (C xor D xor A)); B := B shl 23 or B shr  9 + C;
  Inc(A, Buffer[ 9] + $D9D4D039 + (B xor C xor D)); A := A shl  4 or A shr 28 + B;
  Inc(D, Buffer[12] + $E6DB99E5 + (A xor B xor C)); D := D shl 11 or D shr 21 + A;
  Inc(C, Buffer[15] + $1FA27CF8 + (D xor A xor B)); C := C shl 16 or C shr 16 + D;
  Inc(B, Buffer[ 2] + $C4AC5665 + (C xor D xor A)); B := B shl 23 or B shr  9 + C;

  Inc(A, Buffer[ 0] + $F4292244 + (C xor (B or not D))); A := A shl  6 or A shr 26 + B;
  Inc(D, Buffer[ 7] + $432AFF97 + (B xor (A or not C))); D := D shl 10 or D shr 22 + A;
  Inc(C, Buffer[14] + $AB9423A7 + (A xor (D or not B))); C := C shl 15 or C shr 17 + D;
  Inc(B, Buffer[ 5] + $FC93A039 + (D xor (C or not A))); B := B shl 21 or B shr 11 + C;
  Inc(A, Buffer[12] + $655B59C3 + (C xor (B or not D))); A := A shl  6 or A shr 26 + B;
  Inc(D, Buffer[ 3] + $8F0CCC92 + (B xor (A or not C))); D := D shl 10 or D shr 22 + A;
  Inc(C, Buffer[10] + $FFEFF47D + (A xor (D or not B))); C := C shl 15 or C shr 17 + D;
  Inc(B, Buffer[ 1] + $85845DD1 + (D xor (C or not A))); B := B shl 21 or B shr 11 + C;
  Inc(A, Buffer[ 8] + $6FA87E4F + (C xor (B or not D))); A := A shl  6 or A shr 26 + B;
  Inc(D, Buffer[15] + $FE2CE6E0 + (B xor (A or not C))); D := D shl 10 or D shr 22 + A;
  Inc(C, Buffer[ 6] + $A3014314 + (A xor (D or not B))); C := C shl 15 or C shr 17 + D;
  Inc(B, Buffer[13] + $4E0811A1 + (D xor (C or not A))); B := B shl 21 or B shr 11 + C;
  Inc(A, Buffer[ 4] + $F7537E82 + (C xor (B or not D))); A := A shl  6 or A shr 26 + B;
  Inc(D, Buffer[11] + $BD3AF235 + (B xor (A or not C))); D := D shl 10 or D shr 22 + A;
  Inc(C, Buffer[ 2] + $2AD7D2BB + (A xor (D or not B))); C := C shl 15 or C shr 17 + D;
  Inc(B, Buffer[ 9] + $EB86D391 + (D xor (C or not A))); B := B shl 21 or B shr 11 + C;

  Inc(FDigest[0], A);
  Inc(FDigest[1], B);
  Inc(FDigest[2], C);
  Inc(FDigest[3], D);
end;
{$ENDIF}

// .THash_RipeMD128
const
  RipeS1 = $5A827999;
  RipeS2 = $6ED9EBA1;
  RipeS3 = $8F1BBCDC;
  RipeS4 = $A953FD4E;
  RipeS5 = $50A28BE6;
  RipeS6 = $5C4DD124;
  RipeS7 = $6D703EF3;
  RipeS8 = $7A6D76E9;

{$IFNDEF THash_RipeMD128_asm}
procedure THash_RipeMD128.DoTransform(Buffer: PLongArray);
var
  A1,B1,C1,D1: LongWord;
  A2,B2,C2,D2: LongWord;
  T: LongWord;
begin
  A1 := FDigest[0];
  B1 := FDigest[1];
  C1 := FDigest[2];
  D1 := FDigest[3];
  A2 := FDigest[0];
  B2 := FDigest[1];
  C2 := FDigest[2];
  D2 := FDigest[3];

  Inc(A1, B1 xor C1 xor D1 + Buffer[ 0]); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 1]); D1 := D1 shl 14 or D1 shr 18;
  Inc(C1, D1 xor A1 xor B1 + Buffer[ 2]); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 xor D1 xor A1 + Buffer[ 3]); B1 := B1 shl 12 or B1 shr 20;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 4]); A1 := A1 shl  5 or A1 shr 27;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 5]); D1 := D1 shl  8 or D1 shr 24;
  Inc(C1, D1 xor A1 xor B1 + Buffer[ 6]); C1 := C1 shl  7 or C1 shr 25;
  Inc(B1, C1 xor D1 xor A1 + Buffer[ 7]); B1 := B1 shl  9 or B1 shr 23;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 8]); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 9]); D1 := D1 shl 13 or D1 shr 19;
  Inc(C1, D1 xor A1 xor B1 + Buffer[10]); C1 := C1 shl 14 or C1 shr 18;
  Inc(B1, C1 xor D1 xor A1 + Buffer[11]); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, B1 xor C1 xor D1 + Buffer[12]); A1 := A1 shl  6 or A1 shr 26;
  Inc(D1, A1 xor B1 xor C1 + Buffer[13]); D1 := D1 shl  7 or D1 shr 25;
  Inc(C1, D1 xor A1 xor B1 + Buffer[14]); C1 := C1 shl  9 or C1 shr 23;
  Inc(B1, C1 xor D1 xor A1 + Buffer[15]); B1 := B1 shl  8 or B1 shr 24;

  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 7] + RipeS1); A1 := A1 shl  7 or A1 shr 25;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 4] + RipeS1); D1 := D1 shl  6 or D1 shr 26;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[13] + RipeS1); C1 := C1 shl  8 or C1 shr 24;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 1] + RipeS1); B1 := B1 shl 13 or B1 shr 19;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[10] + RipeS1); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 6] + RipeS1); D1 := D1 shl  9 or D1 shr 23;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[15] + RipeS1); C1 := C1 shl  7 or C1 shr 25;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 3] + RipeS1); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[12] + RipeS1); A1 := A1 shl  7 or A1 shr 25;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 0] + RipeS1); D1 := D1 shl 12 or D1 shr 20;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[ 9] + RipeS1); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 5] + RipeS1); B1 := B1 shl  9 or B1 shr 23;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 2] + RipeS1); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[14] + RipeS1); D1 := D1 shl  7 or D1 shr 25;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[11] + RipeS1); C1 := C1 shl 13 or C1 shr 19;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 8] + RipeS1); B1 := B1 shl 12 or B1 shr 20;

  Inc(A1, B1 or not C1 xor D1 + Buffer[ 3] + RipeS2); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 or not B1 xor C1 + Buffer[10] + RipeS2); D1 := D1 shl 13 or D1 shr 19;
  Inc(C1, D1 or not A1 xor B1 + Buffer[14] + RipeS2); C1 := C1 shl  6 or C1 shr 26;
  Inc(B1, C1 or not D1 xor A1 + Buffer[ 4] + RipeS2); B1 := B1 shl  7 or B1 shr 25;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 9] + RipeS2); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, A1 or not B1 xor C1 + Buffer[15] + RipeS2); D1 := D1 shl  9 or D1 shr 23;
  Inc(C1, D1 or not A1 xor B1 + Buffer[ 8] + RipeS2); C1 := C1 shl 13 or C1 shr 19;
  Inc(B1, C1 or not D1 xor A1 + Buffer[ 1] + RipeS2); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 2] + RipeS2); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, A1 or not B1 xor C1 + Buffer[ 7] + RipeS2); D1 := D1 shl  8 or D1 shr 24;
  Inc(C1, D1 or not A1 xor B1 + Buffer[ 0] + RipeS2); C1 := C1 shl 13 or C1 shr 19;
  Inc(B1, C1 or not D1 xor A1 + Buffer[ 6] + RipeS2); B1 := B1 shl  6 or B1 shr 26;
  Inc(A1, B1 or not C1 xor D1 + Buffer[13] + RipeS2); A1 := A1 shl  5 or A1 shr 27;
  Inc(D1, A1 or not B1 xor C1 + Buffer[11] + RipeS2); D1 := D1 shl 12 or D1 shr 20;
  Inc(C1, D1 or not A1 xor B1 + Buffer[ 5] + RipeS2); C1 := C1 shl  7 or C1 shr 25;
  Inc(B1, C1 or not D1 xor A1 + Buffer[12] + RipeS2); B1 := B1 shl  5 or B1 shr 27;

  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 1] + RipeS3); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 9] + RipeS3); D1 := D1 shl 12 or D1 shr 20;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[11] + RipeS3); C1 := C1 shl 14 or C1 shr 18;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[10] + RipeS3); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 0] + RipeS3); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 8] + RipeS3); D1 := D1 shl 15 or D1 shr 17;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[12] + RipeS3); C1 := C1 shl  9 or C1 shr 23;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[ 4] + RipeS3); B1 := B1 shl  8 or B1 shr 24;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[13] + RipeS3); A1 := A1 shl  9 or A1 shr 23;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 3] + RipeS3); D1 := D1 shl 14 or D1 shr 18;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[ 7] + RipeS3); C1 := C1 shl  5 or C1 shr 27;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[15] + RipeS3); B1 := B1 shl  6 or B1 shr 26;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[14] + RipeS3); A1 := A1 shl  8 or A1 shr 24;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 5] + RipeS3); D1 := D1 shl  6 or D1 shr 26;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[ 6] + RipeS3); C1 := C1 shl  5 or C1 shr 27;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[ 2] + RipeS3); B1 := B1 shl 12 or B1 shr 20;

  T := A1; A1 := A2; A2 := T;
  T := B1; B1 := B2; B2 := T;
  T := C1; C1 := C2; C2 := T;
  T := D1; D1 := D2; D2 := T;

  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 5] + RipeS5); A1 := A1 shl  8 or A1 shr 24;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[14] + RipeS5); D1 := D1 shl  9 or D1 shr 23;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[ 7] + RipeS5); C1 := C1 shl  9 or C1 shr 23;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[ 0] + RipeS5); B1 := B1 shl 11 or B1 shr 21;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 9] + RipeS5); A1 := A1 shl 13 or A1 shr 19;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 2] + RipeS5); D1 := D1 shl 15 or D1 shr 17;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[11] + RipeS5); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[ 4] + RipeS5); B1 := B1 shl  5 or B1 shr 27;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[13] + RipeS5); A1 := A1 shl  7 or A1 shr 25;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 6] + RipeS5); D1 := D1 shl  7 or D1 shr 25;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[15] + RipeS5); C1 := C1 shl  8 or C1 shr 24;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[ 8] + RipeS5); B1 := B1 shl 11 or B1 shr 21;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 1] + RipeS5); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[10] + RipeS5); D1 := D1 shl 14 or D1 shr 18;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[ 3] + RipeS5); C1 := C1 shl 12 or C1 shr 20;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[12] + RipeS5); B1 := B1 shl  6 or B1 shr 26;

  Inc(A1, B1 or not C1 xor D1 + Buffer[ 6] + RipeS6); A1 := A1 shl  9 or A1 shr 23;
  Inc(D1, A1 or not B1 xor C1 + Buffer[11] + RipeS6); D1 := D1 shl 13 or D1 shr 19;
  Inc(C1, D1 or not A1 xor B1 + Buffer[ 3] + RipeS6); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 or not D1 xor A1 + Buffer[ 7] + RipeS6); B1 := B1 shl  7 or B1 shr 25;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 0] + RipeS6); A1 := A1 shl 12 or A1 shr 20;
  Inc(D1, A1 or not B1 xor C1 + Buffer[13] + RipeS6); D1 := D1 shl  8 or D1 shr 24;
  Inc(C1, D1 or not A1 xor B1 + Buffer[ 5] + RipeS6); C1 := C1 shl  9 or C1 shr 23;
  Inc(B1, C1 or not D1 xor A1 + Buffer[10] + RipeS6); B1 := B1 shl 11 or B1 shr 21;
  Inc(A1, B1 or not C1 xor D1 + Buffer[14] + RipeS6); A1 := A1 shl  7 or A1 shr 25;
  Inc(D1, A1 or not B1 xor C1 + Buffer[15] + RipeS6); D1 := D1 shl  7 or D1 shr 25;
  Inc(C1, D1 or not A1 xor B1 + Buffer[ 8] + RipeS6); C1 := C1 shl 12 or C1 shr 20;
  Inc(B1, C1 or not D1 xor A1 + Buffer[12] + RipeS6); B1 := B1 shl  7 or B1 shr 25;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 4] + RipeS6); A1 := A1 shl  6 or A1 shr 26;
  Inc(D1, A1 or not B1 xor C1 + Buffer[ 9] + RipeS6); D1 := D1 shl 15 or D1 shr 17;
  Inc(C1, D1 or not A1 xor B1 + Buffer[ 1] + RipeS6); C1 := C1 shl 13 or C1 shr 19;
  Inc(B1, C1 or not D1 xor A1 + Buffer[ 2] + RipeS6); B1 := B1 shl 11 or B1 shr 21;

  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[15] + RipeS7); A1 := A1 shl  9 or A1 shr 23;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 5] + RipeS7); D1 := D1 shl  7 or D1 shr 25;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[ 1] + RipeS7); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 3] + RipeS7); B1 := B1 shl 11 or B1 shr 21;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 7] + RipeS7); A1 := A1 shl  8 or A1 shr 24;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[14] + RipeS7); D1 := D1 shl  6 or D1 shr 26;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[ 6] + RipeS7); C1 := C1 shl  6 or C1 shr 26;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 9] + RipeS7); B1 := B1 shl 14 or B1 shr 18;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[11] + RipeS7); A1 := A1 shl 12 or A1 shr 20;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 8] + RipeS7); D1 := D1 shl 13 or D1 shr 19;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[12] + RipeS7); C1 := C1 shl  5 or C1 shr 27;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 2] + RipeS7); B1 := B1 shl 14 or B1 shr 18;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[10] + RipeS7); A1 := A1 shl 13 or A1 shr 19;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 0] + RipeS7); D1 := D1 shl 13 or D1 shr 19;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[ 4] + RipeS7); C1 := C1 shl  7 or C1 shr 25;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[13] + RipeS7); B1 := B1 shl  5 or B1 shr 27;

  Inc(A1, B1 xor C1 xor D1 + Buffer[ 8]); A1 := A1 shl 15 or A1 shr 17;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 6]); D1 := D1 shl  5 or D1 shr 27;
  Inc(C1, D1 xor A1 xor B1 + Buffer[ 4]); C1 := C1 shl  8 or C1 shr 24;
  Inc(B1, C1 xor D1 xor A1 + Buffer[ 1]); B1 := B1 shl 11 or B1 shr 21;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 3]); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, A1 xor B1 xor C1 + Buffer[11]); D1 := D1 shl 14 or D1 shr 18;
  Inc(C1, D1 xor A1 xor B1 + Buffer[15]); C1 := C1 shl  6 or C1 shr 26;
  Inc(B1, C1 xor D1 xor A1 + Buffer[ 0]); B1 := B1 shl 14 or B1 shr 18;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 5]); A1 := A1 shl  6 or A1 shr 26;
  Inc(D1, A1 xor B1 xor C1 + Buffer[12]); D1 := D1 shl  9 or D1 shr 23;
  Inc(C1, D1 xor A1 xor B1 + Buffer[ 2]); C1 := C1 shl 12 or C1 shr 20;
  Inc(B1, C1 xor D1 xor A1 + Buffer[13]); B1 := B1 shl  9 or B1 shr 23;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 9]); A1 := A1 shl 12 or A1 shr 20;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 7]); D1 := D1 shl  5 or D1 shr 27;
  Inc(C1, D1 xor A1 xor B1 + Buffer[10]); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 xor D1 xor A1 + Buffer[14]); B1 := B1 shl  8 or B1 shr 24;

  Inc(D1, C2 + FDigest[1]);
  FDigest[1] := FDigest[2] + D2 + A1;
  FDigest[2] := FDigest[3] + A2 + B1;
  FDigest[3] := FDIgest[0] + B2 + C1;
  FDigest[0] := D1;
end;
{$ENDIF}

// .THash_RipeMD160
{$IFNDEF THash_RipeMD160_asm}
procedure THash_RipeMD160.DoTransform(Buffer: PLongArray);
var
  A1,B1,C1,D1,E1: LongWord;
  A2,B2,C2,D2,E2: LongWord;
  T: LongWord;
begin
  A1 := FDigest[0];
  B1 := FDigest[1];
  C1 := FDigest[2];
  D1 := FDigest[3];
  E1 := FDigest[4];

  A2 := FDigest[0];
  B2 := FDigest[1];
  C2 := FDigest[2];
  D2 := FDigest[3];
  E2 := FDigest[4];

  Inc(A1, B1 xor C1 xor D1 + Buffer[ 0]); A1 := A1 shl 11 or A1 shr 21 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 xor B1 xor C1 + Buffer[ 1]); E1 := E1 shl 14 or E1 shr 18 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 xor A1 xor B1 + Buffer[ 2]); D1 := D1 shl 15 or D1 shr 17 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 xor E1 xor A1 + Buffer[ 3]); C1 := C1 shl 12 or C1 shr 20 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 xor D1 xor E1 + Buffer[ 4]); B1 := B1 shl  5 or B1 shr 27 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 5]); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 xor B1 xor C1 + Buffer[ 6]); E1 := E1 shl  7 or E1 shr 25 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 xor A1 xor B1 + Buffer[ 7]); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 xor E1 xor A1 + Buffer[ 8]); C1 := C1 shl 11 or C1 shr 21 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 xor D1 xor E1 + Buffer[ 9]); B1 := B1 shl 13 or B1 shr 19 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 xor C1 xor D1 + Buffer[10]); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 xor B1 xor C1 + Buffer[11]); E1 := E1 shl 15 or E1 shr 17 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 xor A1 xor B1 + Buffer[12]); D1 := D1 shl  6 or D1 shr 26 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 xor E1 xor A1 + Buffer[13]); C1 := C1 shl  7 or C1 shr 25 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 xor D1 xor E1 + Buffer[14]); B1 := B1 shl  9 or B1 shr 23 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 xor C1 xor D1 + Buffer[15]); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;

  Inc(E1, A1 and B1 or not A1 and C1 + Buffer[ 7] + RipeS1); E1 := E1 shl  7 or E1 shr 25 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and A1 or not E1 and B1 + Buffer[ 4] + RipeS1); D1 := D1 shl  6 or D1 shr 26 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and E1 or not D1 and A1 + Buffer[13] + RipeS1); C1 := C1 shl  8 or C1 shr 24 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and D1 or not C1 and E1 + Buffer[ 1] + RipeS1); B1 := B1 shl 13 or B1 shr 19 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[10] + RipeS1); A1 := A1 shl 11 or A1 shr 21 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and B1 or not A1 and C1 + Buffer[ 6] + RipeS1); E1 := E1 shl  9 or E1 shr 23 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and A1 or not E1 and B1 + Buffer[15] + RipeS1); D1 := D1 shl  7 or D1 shr 25 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and E1 or not D1 and A1 + Buffer[ 3] + RipeS1); C1 := C1 shl 15 or C1 shr 17 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and D1 or not C1 and E1 + Buffer[12] + RipeS1); B1 := B1 shl  7 or B1 shr 25 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 0] + RipeS1); A1 := A1 shl 12 or A1 shr 20 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and B1 or not A1 and C1 + Buffer[ 9] + RipeS1); E1 := E1 shl 15 or E1 shr 17 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and A1 or not E1 and B1 + Buffer[ 5] + RipeS1); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and E1 or not D1 and A1 + Buffer[ 2] + RipeS1); C1 := C1 shl 11 or C1 shr 21 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and D1 or not C1 and E1 + Buffer[14] + RipeS1); B1 := B1 shl  7 or B1 shr 25 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[11] + RipeS1); A1 := A1 shl 13 or A1 shr 19 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and B1 or not A1 and C1 + Buffer[ 8] + RipeS1); E1 := E1 shl 12 or E1 shr 20 + D1; B1 := B1 shl 10 or B1 shr 22;

  Inc(D1, E1 or not A1 xor B1 + Buffer[ 3] + RipeS2); D1 := D1 shl 11 or D1 shr 21 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 or not E1 xor A1 + Buffer[10] + RipeS2); C1 := C1 shl 13 or C1 shr 19 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 or not D1 xor E1 + Buffer[14] + RipeS2); B1 := B1 shl  6 or B1 shr 26 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 4] + RipeS2); A1 := A1 shl  7 or A1 shr 25 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 or not B1 xor C1 + Buffer[ 9] + RipeS2); E1 := E1 shl 14 or E1 shr 18 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 or not A1 xor B1 + Buffer[15] + RipeS2); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 or not E1 xor A1 + Buffer[ 8] + RipeS2); C1 := C1 shl 13 or C1 shr 19 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 or not D1 xor E1 + Buffer[ 1] + RipeS2); B1 := B1 shl 15 or B1 shr 17 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 2] + RipeS2); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 or not B1 xor C1 + Buffer[ 7] + RipeS2); E1 := E1 shl  8 or E1 shr 24 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 or not A1 xor B1 + Buffer[ 0] + RipeS2); D1 := D1 shl 13 or D1 shr 19 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 or not E1 xor A1 + Buffer[ 6] + RipeS2); C1 := C1 shl  6 or C1 shr 26 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 or not D1 xor E1 + Buffer[13] + RipeS2); B1 := B1 shl  5 or B1 shr 27 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 or not C1 xor D1 + Buffer[11] + RipeS2); A1 := A1 shl 12 or A1 shr 20 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 or not B1 xor C1 + Buffer[ 5] + RipeS2); E1 := E1 shl  7 or E1 shr 25 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 or not A1 xor B1 + Buffer[12] + RipeS2); D1 := D1 shl  5 or D1 shr 27 + C1; A1 := A1 shl 10 or A1 shr 22;

  Inc(C1, D1 and A1 or E1 and not A1 + Buffer[ 1] + RipeS3); C1 := C1 shl 11 or C1 shr 21 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and E1 or D1 and not E1 + Buffer[ 9] + RipeS3); B1 := B1 shl 12 or B1 shr 20 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[11] + RipeS3); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and C1 or B1 and not C1 + Buffer[10] + RipeS3); E1 := E1 shl 15 or E1 shr 17 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and B1 or A1 and not B1 + Buffer[ 0] + RipeS3); D1 := D1 shl 14 or D1 shr 18 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and A1 or E1 and not A1 + Buffer[ 8] + RipeS3); C1 := C1 shl 15 or C1 shr 17 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and E1 or D1 and not E1 + Buffer[12] + RipeS3); B1 := B1 shl  9 or B1 shr 23 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 4] + RipeS3); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and C1 or B1 and not C1 + Buffer[13] + RipeS3); E1 := E1 shl  9 or E1 shr 23 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and B1 or A1 and not B1 + Buffer[ 3] + RipeS3); D1 := D1 shl 14 or D1 shr 18 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and A1 or E1 and not A1 + Buffer[ 7] + RipeS3); C1 := C1 shl  5 or C1 shr 27 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and E1 or D1 and not E1 + Buffer[15] + RipeS3); B1 := B1 shl  6 or B1 shr 26 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[14] + RipeS3); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and C1 or B1 and not C1 + Buffer[ 5] + RipeS3); E1 := E1 shl  6 or E1 shr 26 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and B1 or A1 and not B1 + Buffer[ 6] + RipeS3); D1 := D1 shl  5 or D1 shr 27 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and A1 or E1 and not A1 + Buffer[ 2] + RipeS3); C1 := C1 shl 12 or C1 shr 20 + B1; E1 := E1 shl 10 or E1 shr 22;

  Inc(B1, D1 or not E1 xor C1 + Buffer[ 4] + RipeS4); B1 := B1 shl  9 or B1 shr 23 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, C1 or not D1 xor B1 + Buffer[ 0] + RipeS4); A1 := A1 shl 15 or A1 shr 17 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, B1 or not C1 xor A1 + Buffer[ 5] + RipeS4); E1 := E1 shl  5 or E1 shr 27 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, A1 or not B1 xor E1 + Buffer[ 9] + RipeS4); D1 := D1 shl 11 or D1 shr 21 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, E1 or not A1 xor D1 + Buffer[ 7] + RipeS4); C1 := C1 shl  6 or C1 shr 26 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, D1 or not E1 xor C1 + Buffer[12] + RipeS4); B1 := B1 shl  8 or B1 shr 24 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, C1 or not D1 xor B1 + Buffer[ 2] + RipeS4); A1 := A1 shl 13 or A1 shr 19 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, B1 or not C1 xor A1 + Buffer[10] + RipeS4); E1 := E1 shl 12 or E1 shr 20 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, A1 or not B1 xor E1 + Buffer[14] + RipeS4); D1 := D1 shl  5 or D1 shr 27 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, E1 or not A1 xor D1 + Buffer[ 1] + RipeS4); C1 := C1 shl 12 or C1 shr 20 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, D1 or not E1 xor C1 + Buffer[ 3] + RipeS4); B1 := B1 shl 13 or B1 shr 19 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, C1 or not D1 xor B1 + Buffer[ 8] + RipeS4); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, B1 or not C1 xor A1 + Buffer[11] + RipeS4); E1 := E1 shl 11 or E1 shr 21 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, A1 or not B1 xor E1 + Buffer[ 6] + RipeS4); D1 := D1 shl  8 or D1 shr 24 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, E1 or not A1 xor D1 + Buffer[15] + RipeS4); C1 := C1 shl  5 or C1 shr 27 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, D1 or not E1 xor C1 + Buffer[13] + RipeS4); B1 := B1 shl  6 or B1 shr 26 + A1; D1 := D1 shl 10 or D1 shr 22;

  T := A1; A1 := A2; A2 := T;
  T := B1; B1 := B2; B2 := T;
  T := C1; C1 := C2; C2 := T;
  T := D1; D1 := D2; D2 := T;
  T := E1; E1 := E2; E2 := T;

  Inc(A1, C1 or not D1 xor B1 + Buffer[ 5] + RipeS5); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, B1 or not C1 xor A1 + Buffer[14] + RipeS5); E1 := E1 shl  9 or E1 shr 23 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, A1 or not B1 xor E1 + Buffer[ 7] + RipeS5); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, E1 or not A1 xor D1 + Buffer[ 0] + RipeS5); C1 := C1 shl 11 or C1 shr 21 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, D1 or not E1 xor C1 + Buffer[ 9] + RipeS5); B1 := B1 shl 13 or B1 shr 19 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, C1 or not D1 xor B1 + Buffer[ 2] + RipeS5); A1 := A1 shl 15 or A1 shr 17 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, B1 or not C1 xor A1 + Buffer[11] + RipeS5); E1 := E1 shl 15 or E1 shr 17 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, A1 or not B1 xor E1 + Buffer[ 4] + RipeS5); D1 := D1 shl  5 or D1 shr 27 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, E1 or not A1 xor D1 + Buffer[13] + RipeS5); C1 := C1 shl  7 or C1 shr 25 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, D1 or not E1 xor C1 + Buffer[ 6] + RipeS5); B1 := B1 shl  7 or B1 shr 25 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, C1 or not D1 xor B1 + Buffer[15] + RipeS5); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, B1 or not C1 xor A1 + Buffer[ 8] + RipeS5); E1 := E1 shl 11 or E1 shr 21 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, A1 or not B1 xor E1 + Buffer[ 1] + RipeS5); D1 := D1 shl 14 or D1 shr 18 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, E1 or not A1 xor D1 + Buffer[10] + RipeS5); C1 := C1 shl 14 or C1 shr 18 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, D1 or not E1 xor C1 + Buffer[ 3] + RipeS5); B1 := B1 shl 12 or B1 shr 20 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, C1 or not D1 xor B1 + Buffer[12] + RipeS5); A1 := A1 shl  6 or A1 shr 26 + E1; C1 := C1 shl 10 or C1 shr 22;

  Inc(E1, A1 and C1 or B1 and not C1 + Buffer[ 6] + RipeS6); E1 := E1 shl  9 or E1 shr 23 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and B1 or A1 and not B1 + Buffer[11] + RipeS6); D1 := D1 shl 13 or D1 shr 19 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and A1 or E1 and not A1 + Buffer[ 3] + RipeS6); C1 := C1 shl 15 or C1 shr 17 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and E1 or D1 and not E1 + Buffer[ 7] + RipeS6); B1 := B1 shl  7 or B1 shr 25 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 0] + RipeS6); A1 := A1 shl 12 or A1 shr 20 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and C1 or B1 and not C1 + Buffer[13] + RipeS6); E1 := E1 shl  8 or E1 shr 24 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and B1 or A1 and not B1 + Buffer[ 5] + RipeS6); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and A1 or E1 and not A1 + Buffer[10] + RipeS6); C1 := C1 shl 11 or C1 shr 21 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and E1 or D1 and not E1 + Buffer[14] + RipeS6); B1 := B1 shl  7 or B1 shr 25 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[15] + RipeS6); A1 := A1 shl  7 or A1 shr 25 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and C1 or B1 and not C1 + Buffer[ 8] + RipeS6); E1 := E1 shl 12 or E1 shr 20 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and B1 or A1 and not B1 + Buffer[12] + RipeS6); D1 := D1 shl  7 or D1 shr 25 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and A1 or E1 and not A1 + Buffer[ 4] + RipeS6); C1 := C1 shl  6 or C1 shr 26 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and E1 or D1 and not E1 + Buffer[ 9] + RipeS6); B1 := B1 shl 15 or B1 shr 17 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 1] + RipeS6); A1 := A1 shl 13 or A1 shr 19 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and C1 or B1 and not C1 + Buffer[ 2] + RipeS6); E1 := E1 shl 11 or E1 shr 21 + D1; B1 := B1 shl 10 or B1 shr 22;

  Inc(D1, E1 or not A1 xor B1 + Buffer[15] + RipeS7); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 or not E1 xor A1 + Buffer[ 5] + RipeS7); C1 := C1 shl  7 or C1 shr 25 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 or not D1 xor E1 + Buffer[ 1] + RipeS7); B1 := B1 shl 15 or B1 shr 17 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 3] + RipeS7); A1 := A1 shl 11 or A1 shr 21 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 or not B1 xor C1 + Buffer[ 7] + RipeS7); E1 := E1 shl  8 or E1 shr 24 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 or not A1 xor B1 + Buffer[14] + RipeS7); D1 := D1 shl  6 or D1 shr 26 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 or not E1 xor A1 + Buffer[ 6] + RipeS7); C1 := C1 shl  6 or C1 shr 26 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 or not D1 xor E1 + Buffer[ 9] + RipeS7); B1 := B1 shl 14 or B1 shr 18 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 or not C1 xor D1 + Buffer[11] + RipeS7); A1 := A1 shl 12 or A1 shr 20 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 or not B1 xor C1 + Buffer[ 8] + RipeS7); E1 := E1 shl 13 or E1 shr 19 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 or not A1 xor B1 + Buffer[12] + RipeS7); D1 := D1 shl  5 or D1 shr 27 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 or not E1 xor A1 + Buffer[ 2] + RipeS7); C1 := C1 shl 14 or C1 shr 18 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 or not D1 xor E1 + Buffer[10] + RipeS7); B1 := B1 shl 13 or B1 shr 19 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 0] + RipeS7); A1 := A1 shl 13 or A1 shr 19 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 or not B1 xor C1 + Buffer[ 4] + RipeS7); E1 := E1 shl  7 or E1 shr 25 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 or not A1 xor B1 + Buffer[13] + RipeS7); D1 := D1 shl  5 or D1 shr 27 + C1; A1 := A1 shl 10 or A1 shr 22;

  Inc(C1, D1 and E1 or not D1 and A1 + Buffer[ 8] + RipeS8); C1 := C1 shl 15 or C1 shr 17 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and D1 or not C1 and E1 + Buffer[ 6] + RipeS8); B1 := B1 shl  5 or B1 shr 27 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 4] + RipeS8); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and B1 or not A1 and C1 + Buffer[ 1] + RipeS8); E1 := E1 shl 11 or E1 shr 21 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and A1 or not E1 and B1 + Buffer[ 3] + RipeS8); D1 := D1 shl 14 or D1 shr 18 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and E1 or not D1 and A1 + Buffer[11] + RipeS8); C1 := C1 shl 14 or C1 shr 18 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and D1 or not C1 and E1 + Buffer[15] + RipeS8); B1 := B1 shl  6 or B1 shr 26 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 0] + RipeS8); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and B1 or not A1 and C1 + Buffer[ 5] + RipeS8); E1 := E1 shl  6 or E1 shr 26 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and A1 or not E1 and B1 + Buffer[12] + RipeS8); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and E1 or not D1 and A1 + Buffer[ 2] + RipeS8); C1 := C1 shl 12 or C1 shr 20 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and D1 or not C1 and E1 + Buffer[13] + RipeS8); B1 := B1 shl  9 or B1 shr 23 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 9] + RipeS8); A1 := A1 shl 12 or A1 shr 20 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and B1 or not A1 and C1 + Buffer[ 7] + RipeS8); E1 := E1 shl  5 or E1 shr 27 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and A1 or not E1 and B1 + Buffer[10] + RipeS8); D1 := D1 shl 15 or D1 shr 17 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and E1 or not D1 and A1 + Buffer[14] + RipeS8); C1 := C1 shl  8 or C1 shr 24 + B1; E1 := E1 shl 10 or E1 shr 22;

  Inc(B1, C1 xor D1 xor E1 + Buffer[12]); B1 := B1 shl  8 or B1 shr 24 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 xor C1 xor D1 + Buffer[15]); A1 := A1 shl  5 or A1 shr 27 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 xor B1 xor C1 + Buffer[10]); E1 := E1 shl 12 or E1 shr 20 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 xor A1 xor B1 + Buffer[ 4]); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 xor E1 xor A1 + Buffer[ 1]); C1 := C1 shl 12 or C1 shr 20 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 xor D1 xor E1 + Buffer[ 5]); B1 := B1 shl  5 or B1 shr 27 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 8]); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 xor B1 xor C1 + Buffer[ 7]); E1 := E1 shl  6 or E1 shr 26 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 xor A1 xor B1 + Buffer[ 6]); D1 := D1 shl  8 or D1 shr 24 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 xor E1 xor A1 + Buffer[ 2]); C1 := C1 shl 13 or C1 shr 19 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 xor D1 xor E1 + Buffer[13]); B1 := B1 shl  6 or B1 shr 26 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 xor C1 xor D1 + Buffer[14]); A1 := A1 shl  5 or A1 shr 27 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 xor B1 xor C1 + Buffer[ 0]); E1 := E1 shl 15 or E1 shr 17 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 xor A1 xor B1 + Buffer[ 3]); D1 := D1 shl 13 or D1 shr 19 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 xor E1 xor A1 + Buffer[ 9]); C1 := C1 shl 11 or C1 shr 21 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 xor D1 xor E1 + Buffer[11]); B1 := B1 shl 11 or B1 shr 21 + A1; D1 := D1 shl 10 or D1 shr 22;

  Inc(D1, C2 + FDigest[1]);
  FDigest[1] := FDigest[2] + D2 + E1;
  FDigest[2] := FDigest[3] + E2 + A1;
  FDigest[3] := FDigest[4] + A2 + B1;
  FDigest[4] := FDigest[0] + B2 + C1;
  FDigest[0] := D1;
end;
{$ENDIF}

class function THash_RipeMD160.DigestSize: Integer;
begin
  Result := 20;
end;

// .THash_RipeMD256
{$IFNDEF THash_RipeMD256_asm}
procedure THash_RipeMD256.DoTransform(Buffer: PLongArray);
var
  A1,B1,C1,D1: LongWord;
  A2,B2,C2,D2: LongWord;
  T: LongWord;
begin
  A1 := FDigest[0];
  B1 := FDigest[1];
  C1 := FDigest[2];
  D1 := FDigest[3];

  A2 := FDigest[4];
  B2 := FDigest[5];
  C2 := FDigest[6];
  D2 := FDigest[7];

  Inc(A1, B1 xor C1 xor D1 + Buffer[ 0]); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 1]); D1 := D1 shl 14 or D1 shr 18;
  Inc(C1, D1 xor A1 xor B1 + Buffer[ 2]); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 xor D1 xor A1 + Buffer[ 3]); B1 := B1 shl 12 or B1 shr 20;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 4]); A1 := A1 shl  5 or A1 shr 27;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 5]); D1 := D1 shl  8 or D1 shr 24;
  Inc(C1, D1 xor A1 xor B1 + Buffer[ 6]); C1 := C1 shl  7 or C1 shr 25;
  Inc(B1, C1 xor D1 xor A1 + Buffer[ 7]); B1 := B1 shl  9 or B1 shr 23;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 8]); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 9]); D1 := D1 shl 13 or D1 shr 19;
  Inc(C1, D1 xor A1 xor B1 + Buffer[10]); C1 := C1 shl 14 or C1 shr 18;
  Inc(B1, C1 xor D1 xor A1 + Buffer[11]); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, B1 xor C1 xor D1 + Buffer[12]); A1 := A1 shl  6 or A1 shr 26;
  Inc(D1, A1 xor B1 xor C1 + Buffer[13]); D1 := D1 shl  7 or D1 shr 25;
  Inc(C1, D1 xor A1 xor B1 + Buffer[14]); C1 := C1 shl  9 or C1 shr 23;
  Inc(B1, C1 xor D1 xor A1 + Buffer[15]); B1 := B1 shl  8 or B1 shr 24;

  T := A1; A1 := A2; A2 := T;
  T := B1; B1 := B2; B2 := T;
  T := C1; C1 := C2; C2 := T;
  T := D1; D1 := D2; D2 := T;

  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 5] + RipeS5); A1 := A1 shl  8 or A1 shr 24;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[14] + RipeS5); D1 := D1 shl  9 or D1 shr 23;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[ 7] + RipeS5); C1 := C1 shl  9 or C1 shr 23;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[ 0] + RipeS5); B1 := B1 shl 11 or B1 shr 21;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 9] + RipeS5); A1 := A1 shl 13 or A1 shr 19;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 2] + RipeS5); D1 := D1 shl 15 or D1 shr 17;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[11] + RipeS5); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[ 4] + RipeS5); B1 := B1 shl  5 or B1 shr 27;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[13] + RipeS5); A1 := A1 shl  7 or A1 shr 25;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 6] + RipeS5); D1 := D1 shl  7 or D1 shr 25;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[15] + RipeS5); C1 := C1 shl  8 or C1 shr 24;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[ 8] + RipeS5); B1 := B1 shl 11 or B1 shr 21;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 1] + RipeS5); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[10] + RipeS5); D1 := D1 shl 14 or D1 shr 18;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[ 3] + RipeS5); C1 := C1 shl 12 or C1 shr 20;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[12] + RipeS5); B1 := B1 shl  6 or B1 shr 26;

  T := B1; B1 := B2; B2 := T;
  T := C1; C1 := C2; C2 := T;
  T := D1; D1 := D2; D2 := T;

  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 7] + RipeS1); A1 := A1 shl  7 or A1 shr 25;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 4] + RipeS1); D1 := D1 shl  6 or D1 shr 26;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[13] + RipeS1); C1 := C1 shl  8 or C1 shr 24;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 1] + RipeS1); B1 := B1 shl 13 or B1 shr 19;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[10] + RipeS1); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 6] + RipeS1); D1 := D1 shl  9 or D1 shr 23;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[15] + RipeS1); C1 := C1 shl  7 or C1 shr 25;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 3] + RipeS1); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[12] + RipeS1); A1 := A1 shl  7 or A1 shr 25;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 0] + RipeS1); D1 := D1 shl 12 or D1 shr 20;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[ 9] + RipeS1); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 5] + RipeS1); B1 := B1 shl  9 or B1 shr 23;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 2] + RipeS1); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[14] + RipeS1); D1 := D1 shl  7 or D1 shr 25;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[11] + RipeS1); C1 := C1 shl 13 or C1 shr 19;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 8] + RipeS1); B1 := B1 shl 12 or B1 shr 20;

  T := A1; A1 := A2; A2 := T;
  T := B1; B1 := B2; B2 := T;
  T := C1; C1 := C2; C2 := T;
  T := D1; D1 := D2; D2 := T;

  Inc(A1, B1 or not C1 xor D1 + Buffer[ 6] + RipeS6); A1 := A1 shl  9 or A1 shr 23;
  Inc(D1, A1 or not B1 xor C1 + Buffer[11] + RipeS6); D1 := D1 shl 13 or D1 shr 19;
  Inc(C1, D1 or not A1 xor B1 + Buffer[ 3] + RipeS6); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 or not D1 xor A1 + Buffer[ 7] + RipeS6); B1 := B1 shl  7 or B1 shr 25;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 0] + RipeS6); A1 := A1 shl 12 or A1 shr 20;
  Inc(D1, A1 or not B1 xor C1 + Buffer[13] + RipeS6); D1 := D1 shl  8 or D1 shr 24;
  Inc(C1, D1 or not A1 xor B1 + Buffer[ 5] + RipeS6); C1 := C1 shl  9 or C1 shr 23;
  Inc(B1, C1 or not D1 xor A1 + Buffer[10] + RipeS6); B1 := B1 shl 11 or B1 shr 21;
  Inc(A1, B1 or not C1 xor D1 + Buffer[14] + RipeS6); A1 := A1 shl  7 or A1 shr 25;
  Inc(D1, A1 or not B1 xor C1 + Buffer[15] + RipeS6); D1 := D1 shl  7 or D1 shr 25;
  Inc(C1, D1 or not A1 xor B1 + Buffer[ 8] + RipeS6); C1 := C1 shl 12 or C1 shr 20;
  Inc(B1, C1 or not D1 xor A1 + Buffer[12] + RipeS6); B1 := B1 shl  7 or B1 shr 25;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 4] + RipeS6); A1 := A1 shl  6 or A1 shr 26;
  Inc(D1, A1 or not B1 xor C1 + Buffer[ 9] + RipeS6); D1 := D1 shl 15 or D1 shr 17;
  Inc(C1, D1 or not A1 xor B1 + Buffer[ 1] + RipeS6); C1 := C1 shl 13 or C1 shr 19;
  Inc(B1, C1 or not D1 xor A1 + Buffer[ 2] + RipeS6); B1 := B1 shl 11 or B1 shr 21;

  T := A1; A1 := A2; A2 := T;
  T := C1; C1 := C2; C2 := T;
  T := D1; D1 := D2; D2 := T;

  Inc(A1, B1 or not C1 xor D1 + Buffer[ 3] + RipeS2); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 or not B1 xor C1 + Buffer[10] + RipeS2); D1 := D1 shl 13 or D1 shr 19;
  Inc(C1, D1 or not A1 xor B1 + Buffer[14] + RipeS2); C1 := C1 shl  6 or C1 shr 26;
  Inc(B1, C1 or not D1 xor A1 + Buffer[ 4] + RipeS2); B1 := B1 shl  7 or B1 shr 25;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 9] + RipeS2); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, A1 or not B1 xor C1 + Buffer[15] + RipeS2); D1 := D1 shl  9 or D1 shr 23;
  Inc(C1, D1 or not A1 xor B1 + Buffer[ 8] + RipeS2); C1 := C1 shl 13 or C1 shr 19;
  Inc(B1, C1 or not D1 xor A1 + Buffer[ 1] + RipeS2); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 2] + RipeS2); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, A1 or not B1 xor C1 + Buffer[ 7] + RipeS2); D1 := D1 shl  8 or D1 shr 24;
  Inc(C1, D1 or not A1 xor B1 + Buffer[ 0] + RipeS2); C1 := C1 shl 13 or C1 shr 19;
  Inc(B1, C1 or not D1 xor A1 + Buffer[ 6] + RipeS2); B1 := B1 shl  6 or B1 shr 26;
  Inc(A1, B1 or not C1 xor D1 + Buffer[13] + RipeS2); A1 := A1 shl  5 or A1 shr 27;
  Inc(D1, A1 or not B1 xor C1 + Buffer[11] + RipeS2); D1 := D1 shl 12 or D1 shr 20;
  Inc(C1, D1 or not A1 xor B1 + Buffer[ 5] + RipeS2); C1 := C1 shl  7 or C1 shr 25;
  Inc(B1, C1 or not D1 xor A1 + Buffer[12] + RipeS2); B1 := B1 shl  5 or B1 shr 27;

  T := A1; A1 := A2; A2 := T;
  T := B1; B1 := B2; B2 := T;
  T := C1; C1 := C2; C2 := T;
  T := D1; D1 := D2; D2 := T;

  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[15] + RipeS7); A1 := A1 shl  9 or A1 shr 23;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 5] + RipeS7); D1 := D1 shl  7 or D1 shr 25;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[ 1] + RipeS7); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 3] + RipeS7); B1 := B1 shl 11 or B1 shr 21;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 7] + RipeS7); A1 := A1 shl  8 or A1 shr 24;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[14] + RipeS7); D1 := D1 shl  6 or D1 shr 26;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[ 6] + RipeS7); C1 := C1 shl  6 or C1 shr 26;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 9] + RipeS7); B1 := B1 shl 14 or B1 shr 18;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[11] + RipeS7); A1 := A1 shl 12 or A1 shr 20;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 8] + RipeS7); D1 := D1 shl 13 or D1 shr 19;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[12] + RipeS7); C1 := C1 shl  5 or C1 shr 27;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[ 2] + RipeS7); B1 := B1 shl 14 or B1 shr 18;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[10] + RipeS7); A1 := A1 shl 13 or A1 shr 19;
  Inc(D1, A1 and B1 or not A1 and C1 + Buffer[ 0] + RipeS7); D1 := D1 shl 13 or D1 shr 19;
  Inc(C1, D1 and A1 or not D1 and B1 + Buffer[ 4] + RipeS7); C1 := C1 shl  7 or C1 shr 25;
  Inc(B1, C1 and D1 or not C1 and A1 + Buffer[13] + RipeS7); B1 := B1 shl  5 or B1 shr 27;

  T := A1; A1 := A2; A2 := T;
  T := B1; B1 := B2; B2 := T;
  T := D1; D1 := D2; D2 := T;

  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 1] + RipeS3); A1 := A1 shl 11 or A1 shr 21;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 9] + RipeS3); D1 := D1 shl 12 or D1 shr 20;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[11] + RipeS3); C1 := C1 shl 14 or C1 shr 18;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[10] + RipeS3); B1 := B1 shl 15 or B1 shr 17;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 0] + RipeS3); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 8] + RipeS3); D1 := D1 shl 15 or D1 shr 17;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[12] + RipeS3); C1 := C1 shl  9 or C1 shr 23;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[ 4] + RipeS3); B1 := B1 shl  8 or B1 shr 24;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[13] + RipeS3); A1 := A1 shl  9 or A1 shr 23;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 3] + RipeS3); D1 := D1 shl 14 or D1 shr 18;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[ 7] + RipeS3); C1 := C1 shl  5 or C1 shr 27;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[15] + RipeS3); B1 := B1 shl  6 or B1 shr 26;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[14] + RipeS3); A1 := A1 shl  8 or A1 shr 24;
  Inc(D1, A1 and C1 or B1 and not C1 + Buffer[ 5] + RipeS3); D1 := D1 shl  6 or D1 shr 26;
  Inc(C1, D1 and B1 or A1 and not B1 + Buffer[ 6] + RipeS3); C1 := C1 shl  5 or C1 shr 27;
  Inc(B1, C1 and A1 or D1 and not A1 + Buffer[ 2] + RipeS3); B1 := B1 shl 12 or B1 shr 20;

  T := A1; A1 := A2; A2 := T;
  T := B1; B1 := B2; B2 := T;
  T := C1; C1 := C2; C2 := T;
  T := D1; D1 := D2; D2 := T;

  Inc(A1, B1 xor C1 xor D1 + Buffer[ 8]); A1 := A1 shl 15 or A1 shr 17;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 6]); D1 := D1 shl  5 or D1 shr 27;
  Inc(C1, D1 xor A1 xor B1 + Buffer[ 4]); C1 := C1 shl  8 or C1 shr 24;
  Inc(B1, C1 xor D1 xor A1 + Buffer[ 1]); B1 := B1 shl 11 or B1 shr 21;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 3]); A1 := A1 shl 14 or A1 shr 18;
  Inc(D1, A1 xor B1 xor C1 + Buffer[11]); D1 := D1 shl 14 or D1 shr 18;
  Inc(C1, D1 xor A1 xor B1 + Buffer[15]); C1 := C1 shl  6 or C1 shr 26;
  Inc(B1, C1 xor D1 xor A1 + Buffer[ 0]); B1 := B1 shl 14 or B1 shr 18;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 5]); A1 := A1 shl  6 or A1 shr 26;
  Inc(D1, A1 xor B1 xor C1 + Buffer[12]); D1 := D1 shl  9 or D1 shr 23;
  Inc(C1, D1 xor A1 xor B1 + Buffer[ 2]); C1 := C1 shl 12 or C1 shr 20;
  Inc(B1, C1 xor D1 xor A1 + Buffer[13]); B1 := B1 shl  9 or B1 shr 23;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 9]); A1 := A1 shl 12 or A1 shr 20;
  Inc(D1, A1 xor B1 xor C1 + Buffer[ 7]); D1 := D1 shl  5 or D1 shr 27;
  Inc(C1, D1 xor A1 xor B1 + Buffer[10]); C1 := C1 shl 15 or C1 shr 17;
  Inc(B1, C1 xor D1 xor A1 + Buffer[14]); B1 := B1 shl  8 or B1 shr 24;

  Inc(FDigest[0], A2);
  Inc(FDigest[1], B2);
  Inc(FDigest[2], C2);
  Inc(FDigest[3], D1);

  Inc(FDigest[4], A1);
  Inc(FDigest[5], B1);
  Inc(FDigest[6], C1);
  Inc(FDigest[7], D2);
end;
{$ENDIF}

procedure THash_RipeMD256.DoInit;
begin
  FDigest[0] := $67452301;
  FDigest[1] := $EFCDAB89;
  FDigest[2] := $98BADCFE;
  FDigest[3] := $10325476;
  FDigest[4] := $76543210;
  FDigest[5] := $FEDCBA98;
  FDigest[6] := $89ABCDEF;
  FDigest[7] := $01234567;
  FDigest[8] := $01234567;
  FDigest[9] := $3C2D1E0F;
end;

class function THash_RipeMD256.DigestSize: Integer;
begin
  Result := 32;
end;

// .THash_RipeMD320
{$IFNDEF THash_RipeMD320_asm}
procedure THash_RipeMD320.DoTransform(Buffer: PLongArray);
var
  A1,B1,C1,D1,E1: LongWord;
  A2,B2,C2,D2,E2: LongWord;
  T: LongWord;
begin
  A1 := FDigest[0];
  B1 := FDigest[1];
  C1 := FDigest[2];
  D1 := FDigest[3];
  E1 := FDigest[4];
  A2 := FDigest[5];
  B2 := FDigest[6];
  C2 := FDigest[7];
  D2 := FDigest[8];
  E2 := FDigest[9];

  Inc(A1, B1 xor C1 xor D1 + Buffer[ 0]); A1 := A1 shl 11 or A1 shr 21 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 xor B1 xor C1 + Buffer[ 1]); E1 := E1 shl 14 or E1 shr 18 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 xor A1 xor B1 + Buffer[ 2]); D1 := D1 shl 15 or D1 shr 17 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 xor E1 xor A1 + Buffer[ 3]); C1 := C1 shl 12 or C1 shr 20 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 xor D1 xor E1 + Buffer[ 4]); B1 := B1 shl  5 or B1 shr 27 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 5]); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 xor B1 xor C1 + Buffer[ 6]); E1 := E1 shl  7 or E1 shr 25 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 xor A1 xor B1 + Buffer[ 7]); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 xor E1 xor A1 + Buffer[ 8]); C1 := C1 shl 11 or C1 shr 21 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 xor D1 xor E1 + Buffer[ 9]); B1 := B1 shl 13 or B1 shr 19 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 xor C1 xor D1 + Buffer[10]); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 xor B1 xor C1 + Buffer[11]); E1 := E1 shl 15 or E1 shr 17 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 xor A1 xor B1 + Buffer[12]); D1 := D1 shl  6 or D1 shr 26 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 xor E1 xor A1 + Buffer[13]); C1 := C1 shl  7 or C1 shr 25 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 xor D1 xor E1 + Buffer[14]); B1 := B1 shl  9 or B1 shr 23 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 xor C1 xor D1 + Buffer[15]); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;

  T := A1; A1 := A2; A2 := T;
  T := B1; B1 := B2; B2 := T;
  T := C1; C1 := C2; C2 := T;
  T := D1; D1 := D2; D2 := T;
  T := E1; E1 := E2; E2 := T;

  Inc(A1, C1 or not D1 xor B1 + Buffer[ 5] + RipeS5); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, B1 or not C1 xor A1 + Buffer[14] + RipeS5); E1 := E1 shl  9 or E1 shr 23 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, A1 or not B1 xor E1 + Buffer[ 7] + RipeS5); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, E1 or not A1 xor D1 + Buffer[ 0] + RipeS5); C1 := C1 shl 11 or C1 shr 21 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, D1 or not E1 xor C1 + Buffer[ 9] + RipeS5); B1 := B1 shl 13 or B1 shr 19 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, C1 or not D1 xor B1 + Buffer[ 2] + RipeS5); A1 := A1 shl 15 or A1 shr 17 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, B1 or not C1 xor A1 + Buffer[11] + RipeS5); E1 := E1 shl 15 or E1 shr 17 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, A1 or not B1 xor E1 + Buffer[ 4] + RipeS5); D1 := D1 shl  5 or D1 shr 27 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, E1 or not A1 xor D1 + Buffer[13] + RipeS5); C1 := C1 shl  7 or C1 shr 25 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, D1 or not E1 xor C1 + Buffer[ 6] + RipeS5); B1 := B1 shl  7 or B1 shr 25 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, C1 or not D1 xor B1 + Buffer[15] + RipeS5); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, B1 or not C1 xor A1 + Buffer[ 8] + RipeS5); E1 := E1 shl 11 or E1 shr 21 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, A1 or not B1 xor E1 + Buffer[ 1] + RipeS5); D1 := D1 shl 14 or D1 shr 18 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, E1 or not A1 xor D1 + Buffer[10] + RipeS5); C1 := C1 shl 14 or C1 shr 18 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, D1 or not E1 xor C1 + Buffer[ 3] + RipeS5); B1 := B1 shl 12 or B1 shr 20 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, C1 or not D1 xor B1 + Buffer[12] + RipeS5); A1 := A1 shl  6 or A1 shr 26 + E1; C1 := C1 shl 10 or C1 shr 22;

  T := B1; B1 := B2; B2 := T;
  T := C1; C1 := C2; C2 := T;
  T := D1; D1 := D2; D2 := T;
  T := E1; E1 := E2; E2 := T;

  Inc(E1, A1 and B1 or not A1 and C1 + Buffer[ 7] + RipeS1); E1 := E1 shl  7 or E1 shr 25 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and A1 or not E1 and B1 + Buffer[ 4] + RipeS1); D1 := D1 shl  6 or D1 shr 26 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and E1 or not D1 and A1 + Buffer[13] + RipeS1); C1 := C1 shl  8 or C1 shr 24 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and D1 or not C1 and E1 + Buffer[ 1] + RipeS1); B1 := B1 shl 13 or B1 shr 19 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[10] + RipeS1); A1 := A1 shl 11 or A1 shr 21 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and B1 or not A1 and C1 + Buffer[ 6] + RipeS1); E1 := E1 shl  9 or E1 shr 23 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and A1 or not E1 and B1 + Buffer[15] + RipeS1); D1 := D1 shl  7 or D1 shr 25 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and E1 or not D1 and A1 + Buffer[ 3] + RipeS1); C1 := C1 shl 15 or C1 shr 17 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and D1 or not C1 and E1 + Buffer[12] + RipeS1); B1 := B1 shl  7 or B1 shr 25 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 0] + RipeS1); A1 := A1 shl 12 or A1 shr 20 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and B1 or not A1 and C1 + Buffer[ 9] + RipeS1); E1 := E1 shl 15 or E1 shr 17 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and A1 or not E1 and B1 + Buffer[ 5] + RipeS1); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and E1 or not D1 and A1 + Buffer[ 2] + RipeS1); C1 := C1 shl 11 or C1 shr 21 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and D1 or not C1 and E1 + Buffer[14] + RipeS1); B1 := B1 shl  7 or B1 shr 25 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[11] + RipeS1); A1 := A1 shl 13 or A1 shr 19 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and B1 or not A1 and C1 + Buffer[ 8] + RipeS1); E1 := E1 shl 12 or E1 shr 20 + D1; B1 := B1 shl 10 or B1 shr 22;

  T := A1; A1 := A2; A2 := T;
  T := B1; B1 := B2; B2 := T;
  T := C1; C1 := C2; C2 := T;
  T := D1; D1 := D2; D2 := T;
  T := E1; E1 := E2; E2 := T;

  Inc(E1, A1 and C1 or B1 and not C1 + Buffer[ 6] + RipeS6); E1 := E1 shl  9 or E1 shr 23 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and B1 or A1 and not B1 + Buffer[11] + RipeS6); D1 := D1 shl 13 or D1 shr 19 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and A1 or E1 and not A1 + Buffer[ 3] + RipeS6); C1 := C1 shl 15 or C1 shr 17 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and E1 or D1 and not E1 + Buffer[ 7] + RipeS6); B1 := B1 shl  7 or B1 shr 25 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 0] + RipeS6); A1 := A1 shl 12 or A1 shr 20 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and C1 or B1 and not C1 + Buffer[13] + RipeS6); E1 := E1 shl  8 or E1 shr 24 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and B1 or A1 and not B1 + Buffer[ 5] + RipeS6); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and A1 or E1 and not A1 + Buffer[10] + RipeS6); C1 := C1 shl 11 or C1 shr 21 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and E1 or D1 and not E1 + Buffer[14] + RipeS6); B1 := B1 shl  7 or B1 shr 25 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[15] + RipeS6); A1 := A1 shl  7 or A1 shr 25 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and C1 or B1 and not C1 + Buffer[ 8] + RipeS6); E1 := E1 shl 12 or E1 shr 20 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and B1 or A1 and not B1 + Buffer[12] + RipeS6); D1 := D1 shl  7 or D1 shr 25 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and A1 or E1 and not A1 + Buffer[ 4] + RipeS6); C1 := C1 shl  6 or C1 shr 26 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and E1 or D1 and not E1 + Buffer[ 9] + RipeS6); B1 := B1 shl 15 or B1 shr 17 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 1] + RipeS6); A1 := A1 shl 13 or A1 shr 19 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and C1 or B1 and not C1 + Buffer[ 2] + RipeS6); E1 := E1 shl 11 or E1 shr 21 + D1; B1 := B1 shl 10 or B1 shr 22;

  T := A1; A1 := A2; A2 := T;
  T := C1; C1 := C2; C2 := T;
  T := D1; D1 := D2; D2 := T;
  T := E1; E1 := E2; E2 := T;

  Inc(D1, E1 or not A1 xor B1 + Buffer[ 3] + RipeS2); D1 := D1 shl 11 or D1 shr 21 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 or not E1 xor A1 + Buffer[10] + RipeS2); C1 := C1 shl 13 or C1 shr 19 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 or not D1 xor E1 + Buffer[14] + RipeS2); B1 := B1 shl  6 or B1 shr 26 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 4] + RipeS2); A1 := A1 shl  7 or A1 shr 25 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 or not B1 xor C1 + Buffer[ 9] + RipeS2); E1 := E1 shl 14 or E1 shr 18 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 or not A1 xor B1 + Buffer[15] + RipeS2); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 or not E1 xor A1 + Buffer[ 8] + RipeS2); C1 := C1 shl 13 or C1 shr 19 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 or not D1 xor E1 + Buffer[ 1] + RipeS2); B1 := B1 shl 15 or B1 shr 17 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 2] + RipeS2); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 or not B1 xor C1 + Buffer[ 7] + RipeS2); E1 := E1 shl  8 or E1 shr 24 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 or not A1 xor B1 + Buffer[ 0] + RipeS2); D1 := D1 shl 13 or D1 shr 19 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 or not E1 xor A1 + Buffer[ 6] + RipeS2); C1 := C1 shl  6 or C1 shr 26 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 or not D1 xor E1 + Buffer[13] + RipeS2); B1 := B1 shl  5 or B1 shr 27 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 or not C1 xor D1 + Buffer[11] + RipeS2); A1 := A1 shl 12 or A1 shr 20 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 or not B1 xor C1 + Buffer[ 5] + RipeS2); E1 := E1 shl  7 or E1 shr 25 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 or not A1 xor B1 + Buffer[12] + RipeS2); D1 := D1 shl  5 or D1 shr 27 + C1; A1 := A1 shl 10 or A1 shr 22;

  T := A1; A1 := A2; A2 := T;
  T := B1; B1 := B2; B2 := T;
  T := C1; C1 := C2; C2 := T;
  T := D1; D1 := D2; D2 := T;
  T := E1; E1 := E2; E2 := T;

  Inc(D1, E1 or not A1 xor B1 + Buffer[15] + RipeS7); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 or not E1 xor A1 + Buffer[ 5] + RipeS7); C1 := C1 shl  7 or C1 shr 25 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 or not D1 xor E1 + Buffer[ 1] + RipeS7); B1 := B1 shl 15 or B1 shr 17 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 3] + RipeS7); A1 := A1 shl 11 or A1 shr 21 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 or not B1 xor C1 + Buffer[ 7] + RipeS7); E1 := E1 shl  8 or E1 shr 24 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 or not A1 xor B1 + Buffer[14] + RipeS7); D1 := D1 shl  6 or D1 shr 26 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 or not E1 xor A1 + Buffer[ 6] + RipeS7); C1 := C1 shl  6 or C1 shr 26 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 or not D1 xor E1 + Buffer[ 9] + RipeS7); B1 := B1 shl 14 or B1 shr 18 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 or not C1 xor D1 + Buffer[11] + RipeS7); A1 := A1 shl 12 or A1 shr 20 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 or not B1 xor C1 + Buffer[ 8] + RipeS7); E1 := E1 shl 13 or E1 shr 19 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 or not A1 xor B1 + Buffer[12] + RipeS7); D1 := D1 shl  5 or D1 shr 27 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 or not E1 xor A1 + Buffer[ 2] + RipeS7); C1 := C1 shl 14 or C1 shr 18 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 or not D1 xor E1 + Buffer[10] + RipeS7); B1 := B1 shl 13 or B1 shr 19 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 or not C1 xor D1 + Buffer[ 0] + RipeS7); A1 := A1 shl 13 or A1 shr 19 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 or not B1 xor C1 + Buffer[ 4] + RipeS7); E1 := E1 shl  7 or E1 shr 25 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 or not A1 xor B1 + Buffer[13] + RipeS7); D1 := D1 shl  5 or D1 shr 27 + C1; A1 := A1 shl 10 or A1 shr 22;

  T := A1; A1 := A2; A2 := T;
  T := B1; B1 := B2; B2 := T;
  T := D1; D1 := D2; D2 := T;
  T := E1; E1 := E2; E2 := T;

  Inc(C1, D1 and A1 or E1 and not A1 + Buffer[ 1] + RipeS3); C1 := C1 shl 11 or C1 shr 21 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and E1 or D1 and not E1 + Buffer[ 9] + RipeS3); B1 := B1 shl 12 or B1 shr 20 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[11] + RipeS3); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and C1 or B1 and not C1 + Buffer[10] + RipeS3); E1 := E1 shl 15 or E1 shr 17 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and B1 or A1 and not B1 + Buffer[ 0] + RipeS3); D1 := D1 shl 14 or D1 shr 18 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and A1 or E1 and not A1 + Buffer[ 8] + RipeS3); C1 := C1 shl 15 or C1 shr 17 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and E1 or D1 and not E1 + Buffer[12] + RipeS3); B1 := B1 shl  9 or B1 shr 23 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[ 4] + RipeS3); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and C1 or B1 and not C1 + Buffer[13] + RipeS3); E1 := E1 shl  9 or E1 shr 23 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and B1 or A1 and not B1 + Buffer[ 3] + RipeS3); D1 := D1 shl 14 or D1 shr 18 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and A1 or E1 and not A1 + Buffer[ 7] + RipeS3); C1 := C1 shl  5 or C1 shr 27 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and E1 or D1 and not E1 + Buffer[15] + RipeS3); B1 := B1 shl  6 or B1 shr 26 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and D1 or C1 and not D1 + Buffer[14] + RipeS3); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and C1 or B1 and not C1 + Buffer[ 5] + RipeS3); E1 := E1 shl  6 or E1 shr 26 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and B1 or A1 and not B1 + Buffer[ 6] + RipeS3); D1 := D1 shl  5 or D1 shr 27 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and A1 or E1 and not A1 + Buffer[ 2] + RipeS3); C1 := C1 shl 12 or C1 shr 20 + B1; E1 := E1 shl 10 or E1 shr 22;

  T := A1; A1 := A2; A2 := T;
  T := B1; B1 := B2; B2 := T;
  T := C1; C1 := C2; C2 := T;
  T := D1; D1 := D2; D2 := T;
  T := E1; E1 := E2; E2 := T;

  Inc(C1, D1 and E1 or not D1 and A1 + Buffer[ 8] + RipeS8); C1 := C1 shl 15 or C1 shr 17 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and D1 or not C1 and E1 + Buffer[ 6] + RipeS8); B1 := B1 shl  5 or B1 shr 27 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 4] + RipeS8); A1 := A1 shl  8 or A1 shr 24 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and B1 or not A1 and C1 + Buffer[ 1] + RipeS8); E1 := E1 shl 11 or E1 shr 21 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and A1 or not E1 and B1 + Buffer[ 3] + RipeS8); D1 := D1 shl 14 or D1 shr 18 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and E1 or not D1 and A1 + Buffer[11] + RipeS8); C1 := C1 shl 14 or C1 shr 18 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and D1 or not C1 and E1 + Buffer[15] + RipeS8); B1 := B1 shl  6 or B1 shr 26 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 0] + RipeS8); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and B1 or not A1 and C1 + Buffer[ 5] + RipeS8); E1 := E1 shl  6 or E1 shr 26 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and A1 or not E1 and B1 + Buffer[12] + RipeS8); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and E1 or not D1 and A1 + Buffer[ 2] + RipeS8); C1 := C1 shl 12 or C1 shr 20 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 and D1 or not C1 and E1 + Buffer[13] + RipeS8); B1 := B1 shl  9 or B1 shr 23 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 and C1 or not B1 and D1 + Buffer[ 9] + RipeS8); A1 := A1 shl 12 or A1 shr 20 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 and B1 or not A1 and C1 + Buffer[ 7] + RipeS8); E1 := E1 shl  5 or E1 shr 27 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 and A1 or not E1 and B1 + Buffer[10] + RipeS8); D1 := D1 shl 15 or D1 shr 17 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 and E1 or not D1 and A1 + Buffer[14] + RipeS8); C1 := C1 shl  8 or C1 shr 24 + B1; E1 := E1 shl 10 or E1 shr 22;

  T := A1; A1 := A2; A2 := T;
  T := B1; B1 := B2; B2 := T;
  T := C1; C1 := C2; C2 := T;
  T := E1; E1 := E2; E2 := T;

  Inc(B1, D1 or not E1 xor C1 + Buffer[ 4] + RipeS4); B1 := B1 shl  9 or B1 shr 23 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, C1 or not D1 xor B1 + Buffer[ 0] + RipeS4); A1 := A1 shl 15 or A1 shr 17 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, B1 or not C1 xor A1 + Buffer[ 5] + RipeS4); E1 := E1 shl  5 or E1 shr 27 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, A1 or not B1 xor E1 + Buffer[ 9] + RipeS4); D1 := D1 shl 11 or D1 shr 21 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, E1 or not A1 xor D1 + Buffer[ 7] + RipeS4); C1 := C1 shl  6 or C1 shr 26 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, D1 or not E1 xor C1 + Buffer[12] + RipeS4); B1 := B1 shl  8 or B1 shr 24 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, C1 or not D1 xor B1 + Buffer[ 2] + RipeS4); A1 := A1 shl 13 or A1 shr 19 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, B1 or not C1 xor A1 + Buffer[10] + RipeS4); E1 := E1 shl 12 or E1 shr 20 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, A1 or not B1 xor E1 + Buffer[14] + RipeS4); D1 := D1 shl  5 or D1 shr 27 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, E1 or not A1 xor D1 + Buffer[ 1] + RipeS4); C1 := C1 shl 12 or C1 shr 20 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, D1 or not E1 xor C1 + Buffer[ 3] + RipeS4); B1 := B1 shl 13 or B1 shr 19 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, C1 or not D1 xor B1 + Buffer[ 8] + RipeS4); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, B1 or not C1 xor A1 + Buffer[11] + RipeS4); E1 := E1 shl 11 or E1 shr 21 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, A1 or not B1 xor E1 + Buffer[ 6] + RipeS4); D1 := D1 shl  8 or D1 shr 24 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, E1 or not A1 xor D1 + Buffer[15] + RipeS4); C1 := C1 shl  5 or C1 shr 27 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, D1 or not E1 xor C1 + Buffer[13] + RipeS4); B1 := B1 shl  6 or B1 shr 26 + A1; D1 := D1 shl 10 or D1 shr 22;

  T := A1; A1 := A2; A2 := T;
  T := B1; B1 := B2; B2 := T;
  T := C1; C1 := C2; C2 := T;
  T := D1; D1 := D2; D2 := T;
  T := E1; E1 := E2; E2 := T;

  Inc(B1, C1 xor D1 xor E1 + Buffer[12]); B1 := B1 shl  8 or B1 shr 24 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 xor C1 xor D1 + Buffer[15]); A1 := A1 shl  5 or A1 shr 27 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 xor B1 xor C1 + Buffer[10]); E1 := E1 shl 12 or E1 shr 20 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 xor A1 xor B1 + Buffer[ 4]); D1 := D1 shl  9 or D1 shr 23 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 xor E1 xor A1 + Buffer[ 1]); C1 := C1 shl 12 or C1 shr 20 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 xor D1 xor E1 + Buffer[ 5]); B1 := B1 shl  5 or B1 shr 27 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 xor C1 xor D1 + Buffer[ 8]); A1 := A1 shl 14 or A1 shr 18 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 xor B1 xor C1 + Buffer[ 7]); E1 := E1 shl  6 or E1 shr 26 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 xor A1 xor B1 + Buffer[ 6]); D1 := D1 shl  8 or D1 shr 24 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 xor E1 xor A1 + Buffer[ 2]); C1 := C1 shl 13 or C1 shr 19 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 xor D1 xor E1 + Buffer[13]); B1 := B1 shl  6 or B1 shr 26 + A1; D1 := D1 shl 10 or D1 shr 22;
  Inc(A1, B1 xor C1 xor D1 + Buffer[14]); A1 := A1 shl  5 or A1 shr 27 + E1; C1 := C1 shl 10 or C1 shr 22;
  Inc(E1, A1 xor B1 xor C1 + Buffer[ 0]); E1 := E1 shl 15 or E1 shr 17 + D1; B1 := B1 shl 10 or B1 shr 22;
  Inc(D1, E1 xor A1 xor B1 + Buffer[ 3]); D1 := D1 shl 13 or D1 shr 19 + C1; A1 := A1 shl 10 or A1 shr 22;
  Inc(C1, D1 xor E1 xor A1 + Buffer[ 9]); C1 := C1 shl 11 or C1 shr 21 + B1; E1 := E1 shl 10 or E1 shr 22;
  Inc(B1, C1 xor D1 xor E1 + Buffer[11]); B1 := B1 shl 11 or B1 shr 21 + A1; D1 := D1 shl 10 or D1 shr 22;

  Inc(FDigest[0], A2);
  Inc(FDigest[1], B2);
  Inc(FDigest[2], C2);
  Inc(FDigest[3], D2);
  Inc(FDigest[4], E1);
  Inc(FDigest[5], A1);
  Inc(FDigest[6], B1);
  Inc(FDigest[7], C1);
  Inc(FDigest[8], D1);
  Inc(FDigest[9], E2);
end;
{$ENDIF}

class function THash_RipeMD320.DigestSize: Integer;
begin
  Result := 40;
end;

// .THash_SHA
class function THash_SHA.DigestSize: Integer;
begin
  Result := 20;
end;
{$IFNDEF THash_SHA_asm}
procedure THash_SHA.DoTransform(Buffer: PLongArray);
var
  A,B,C,D,E,T: LongWord;
  W: array[0..79] of LongWord;
  I: Integer;
begin
  SwapLongBuffer(Buffer[0], W, 16);
  if ClassType = THash_SHA then
    for I := 16 to 79 do
    begin
      T := W[I - 3] xor W[I - 8] xor W[I - 14] xor W[I - 16];
      W[I] := T;
    end
  else
    for I := 16 to 79 do
    begin
      T := W[I - 3] xor W[I - 8] xor W[I - 14] xor W[I - 16];
      W[I] := T shl 1 or T shr 31;
    end;

  A := FDigest[0];
  B := FDigest[1];
  C := FDigest[2];
  D := FDigest[3];
  E := FDigest[4];

  Inc(E, (A shl 5 or A shr 27) + (D xor (B and (C xor D))) + W[ 0] + $5A827999); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor (A and (B xor C))) + W[ 1] + $5A827999); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor (E and (A xor B))) + W[ 2] + $5A827999); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor (D and (E xor A))) + W[ 3] + $5A827999); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor (C and (D xor E))) + W[ 4] + $5A827999); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor (B and (C xor D))) + W[ 5] + $5A827999); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor (A and (B xor C))) + W[ 6] + $5A827999); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor (E and (A xor B))) + W[ 7] + $5A827999); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor (D and (E xor A))) + W[ 8] + $5A827999); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor (C and (D xor E))) + W[ 9] + $5A827999); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor (B and (C xor D))) + W[10] + $5A827999); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor (A and (B xor C))) + W[11] + $5A827999); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor (E and (A xor B))) + W[12] + $5A827999); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor (D and (E xor A))) + W[13] + $5A827999); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor (C and (D xor E))) + W[14] + $5A827999); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor (B and (C xor D))) + W[15] + $5A827999); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor (A and (B xor C))) + W[16] + $5A827999); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor (E and (A xor B))) + W[17] + $5A827999); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor (D and (E xor A))) + W[18] + $5A827999); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor (C and (D xor E))) + W[19] + $5A827999); C := C shr 2 or C shl 30;

  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[20] + $6ED9EBA1); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[21] + $6ED9EBA1); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[22] + $6ED9EBA1); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[23] + $6ED9EBA1); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[24] + $6ED9EBA1); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[25] + $6ED9EBA1); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[26] + $6ED9EBA1); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[27] + $6ED9EBA1); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[28] + $6ED9EBA1); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[29] + $6ED9EBA1); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[30] + $6ED9EBA1); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[31] + $6ED9EBA1); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[32] + $6ED9EBA1); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[33] + $6ED9EBA1); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[34] + $6ED9EBA1); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[35] + $6ED9EBA1); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[36] + $6ED9EBA1); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[37] + $6ED9EBA1); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[38] + $6ED9EBA1); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[39] + $6ED9EBA1); C := C shr 2 or C shl 30;

  Inc(E, (A shl 5 or A shr 27) + ((B and C) or (D and (B or C))) + W[40] + $8F1BBCDC); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + ((A and B) or (C and (A or B))) + W[41] + $8F1BBCDC); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + ((E and A) or (B and (E or A))) + W[42] + $8F1BBCDC); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + ((D and E) or (A and (D or E))) + W[43] + $8F1BBCDC); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + ((C and D) or (E and (C or D))) + W[44] + $8F1BBCDC); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + ((B and C) or (D and (B or C))) + W[45] + $8F1BBCDC); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + ((A and B) or (C and (A or B))) + W[46] + $8F1BBCDC); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + ((E and A) or (B and (E or A))) + W[47] + $8F1BBCDC); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + ((D and E) or (A and (D or E))) + W[48] + $8F1BBCDC); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + ((C and D) or (E and (C or D))) + W[49] + $8F1BBCDC); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + ((B and C) or (D and (B or C))) + W[50] + $8F1BBCDC); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + ((A and B) or (C and (A or B))) + W[51] + $8F1BBCDC); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + ((E and A) or (B and (E or A))) + W[52] + $8F1BBCDC); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + ((D and E) or (A and (D or E))) + W[53] + $8F1BBCDC); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + ((C and D) or (E and (C or D))) + W[54] + $8F1BBCDC); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + ((B and C) or (D and (B or C))) + W[55] + $8F1BBCDC); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + ((A and B) or (C and (A or B))) + W[56] + $8F1BBCDC); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + ((E and A) or (B and (E or A))) + W[57] + $8F1BBCDC); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + ((D and E) or (A and (D or E))) + W[58] + $8F1BBCDC); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + ((C and D) or (E and (C or D))) + W[59] + $8F1BBCDC); C := C shr 2 or C shl 30;

  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[60] + $CA62C1D6); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[61] + $CA62C1D6); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[62] + $CA62C1D6); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[63] + $CA62C1D6); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[64] + $CA62C1D6); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[65] + $CA62C1D6); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[66] + $CA62C1D6); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[67] + $CA62C1D6); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[68] + $CA62C1D6); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[69] + $CA62C1D6); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[70] + $CA62C1D6); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[71] + $CA62C1D6); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[72] + $CA62C1D6); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[73] + $CA62C1D6); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[74] + $CA62C1D6); C := C shr 2 or C shl 30;
  Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + W[75] + $CA62C1D6); B := B shr 2 or B shl 30;
  Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + W[76] + $CA62C1D6); A := A shr 2 or A shl 30;
  Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + W[77] + $CA62C1D6); E := E shr 2 or E shl 30;
  Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + W[78] + $CA62C1D6); D := D shr 2 or D shl 30;
  Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + W[79] + $CA62C1D6); C := C shr 2 or C shl 30;

  Inc(FDigest[0], A);
  Inc(FDigest[1], B);
  Inc(FDigest[2], C);
  Inc(FDigest[3], D);
  Inc(FDigest[4], E);
end;
{$ENDIF}

procedure THash_SHA.DoDone;
begin
  if FCount[2] or FCount[3] <> 0 then HashingOverflowError;
  if FPaddingByte = 0 then FPaddingByte := $80;
  FBuffer[FBufferIndex] := FPaddingByte;
  Inc(FBufferIndex);
  if FBufferIndex > FBufferSize - 8 then
  begin
    FillChar(FBuffer[FBufferIndex], FBufferSize - FBufferIndex, 0);
    DoTransform(Pointer(FBuffer));
    FBufferIndex := 0;
  end;
  FillChar(FBuffer[FBufferIndex], FBufferSize - FBufferIndex, 0);
  PLongWord(@FBuffer[FBufferSize - 8])^ := SwapLong(FCount[1]);
  PLongWord(@FBuffer[FBufferSize - 4])^ := SwapLong(FCount[0]);
  DoTransform(Pointer(FBuffer));
  SwapLongBuffer(FDigest, FDigest, SizeOf(FDigest) div 4);
end;

// .THash_SHA256
class function THash_SHA256.DigestSize: Integer;
begin
  Result := 32;
end;

procedure THash_SHA256.DoInit;
begin
  FDigest[0]:= $6A09E667;
  FDigest[1]:= $BB67AE85;
  FDigest[2]:= $3C6EF372;
  FDigest[3]:= $A54FF53A;
  FDigest[4]:= $510E527F;
  FDigest[5]:= $9B05688C;
  FDigest[6]:= $1F83D9AB;
  FDigest[7]:= $5BE0CD19;
end;

{$IFNDEF THash_SHA256_asm}
procedure THash_SHA256.DoTransform(Buffer: PLongArray);
var
  I: Integer;
  A,B,C,D,E,F,G,H: LongWord;
  T1,T2: LongWord;
  W: array[0..63] of LongWord;
begin
  SwapLongBuffer(Buffer[0], W, 16);

  for I := 16 to 63 do
  begin
    T1 := W[I - 15];
    T2 := W[I - 2];
    W[I] := W[I - 16] + W[I - 7] +
           ((T1 shr  7 or T1 shl 25) xor (T1 shr 18 or T1 shl 14) xor (T1 shr  3)) +
           ((T2 shr 17 or T2 shl 15) xor (T2 shr 19 or T2 shl 13) xor (T2 shr 10));
  end;

  // calculate new hash values
  A := FDigest[0];
  B := FDigest[1];
  C := FDigest[2];
  D := FDigest[3];
  E := FDigest[4];
  F := FDigest[5];
  G := FDigest[6];
  H := FDigest[7];
  for I := 0 to 63 do
  begin
    T1 := ((E shr 6 or E shl 26) xor (E shr 11 or E shl 21) xor
           (E shr 25 or E shl 7)) + H + (((F xor G) and E) xor G) + SHA_256K[I] + W[I];
    T2 := ((A shr 2 or A shl 30) xor (A shr 13 or A shl 19) xor
           (A shr 22 or A shl 10)) + (((B or C) and A) or (B and C));
    H := G; G := F; F := E; E := D + T1; D := C; C := B; B := A; A := T1 + T2;
  end;
  Inc(FDigest[0], A);
  Inc(FDigest[1], B);
  Inc(FDigest[2], C);
  Inc(FDigest[3], D);
  Inc(FDigest[4], E);
  Inc(FDigest[5], F);
  Inc(FDigest[6], G);
  Inc(FDigest[7], H);
end;
{$ENDIF}

// .THash_SHA384
class function THash_SHA384.DigestSize: Integer;
begin
  Result := 48;
end;

class function THash_SHA384.BlockSize: Integer;
begin
  Result := 128;
end;

function THash_SHA384.Digest: PByteArray;
begin
  Result := @FDigest;
end;

{$IFNDEF THash_SHA384_asm}
procedure THash_SHA384.DoTransform(Buffer: PLongArray);
var
  A,B,C,D,E,F,G,H: Int64;
  T1,T2: Int64;
  I: Integer;
  W: array [0..79] of Int64;
begin
  SwapInt64Buffer(Buffer[0], W, 16);

  // calculate other 64 uint64
  for I := 16 to 79 do
  begin
    T1 := W[I - 15];
    T2 := W[I - 2];
    W[I] := W[I - 16] + W[I - 7]  +
             ((T1 shr  1 or T1 shl 63) xor (T1 shr  8 or T1 shl 56) xor (T1 shr  7)) +
             ((T2 shr 19 or T2 shl 45) xor (T2 shr 61 or T2 shl  3) xor (T2 shr  6));
  end;

  // calculate new hash values
  A := FDigest[0];
  B := FDigest[1];
  C := FDigest[2];
  D := FDigest[3];
  E := FDigest[4];
  F := FDigest[5];
  G := FDigest[6];
  H := FDigest[7];
  for I := 0 to 79 do
  begin
    T1 := ((E shr 14 or E shl 50) xor (E shr 18 or E shl 46) xor
           (E shr 41 or E shl 23)) + H + (((F xor G) and E) xor G) + SHA_512K[I] + W[I];
    T2 := ((A shr 28 or A shl 36) xor (A shr 34 or A shl 30) xor
           (A shr 39 or A shl 25)) + (((B or C) and A) or (B and C));
    H := G;
    G := F;
    F := E;
    E := D + T1;
    D := C;
    C := B;
    B := A;
    A := T1 + T2;
  end;
  Inc(FDigest[0], A);
  Inc(FDigest[1], B);
  Inc(FDigest[2], C);
  Inc(FDigest[3], D);
  Inc(FDigest[4], E);
  Inc(FDigest[5], F);
  Inc(FDigest[6], G);
  Inc(FDigest[7], H);
end;
{$ENDIF}

procedure THash_SHA384.DoInit;
begin
  FDigest[0] := $CBBB9D5DC1059ED8;
  FDigest[1] := $629A292A367CD507;
  FDigest[2] := $9159015A3070DD17;
  FDigest[3] := $152FECD8F70E5939;
  FDigest[4] := $67332667FFC00B31;
  FDigest[5] := $8EB44A8768581511;
  FDigest[6] := $DB0C2E0D64F98FA7;
  FDigest[7] := $47B5481DBEFA4FA4;
end;

procedure THash_SHA384.DoDone;
begin
  if FPaddingByte = 0 then FPaddingByte := $80;
  FBuffer[FBufferIndex] := FPaddingByte;
  Inc(FBufferIndex);
  if FBufferIndex > FBufferSize - 16 then
  begin
    FillChar(FBuffer[FBufferIndex], FBufferSize - FBufferIndex, 0);
    DoTransform(Pointer(FBuffer));
    FBufferIndex := 0;
  end;
  FillChar(FBuffer[FBufferIndex], FBufferSize - FBufferIndex, 0);
  SwapLongBuffer(FCount, FCount, 4);
  PLongWord(@FBuffer[FBufferSize - 16])^ := FCount[3];
  PLongWord(@FBuffer[FBufferSize - 12])^ := FCount[2];
  PLongWord(@FBuffer[FBufferSize -  8])^ := FCount[1];
  PLongWord(@FBuffer[FBufferSize -  4])^ := FCount[0];
  DoTransform(Pointer(FBuffer));
  SwapInt64Buffer(FDigest, FDigest, SizeOf(FDigest) div 8);
end;

// .THash_SHA512
class function THash_SHA512.DigestSize: Integer;
begin
  Result := 64;
end;

procedure THash_SHA512.DoInit;
begin
  FDigest[0] := $6A09E667F3BCC908;
  FDigest[1] := $BB67AE8584CAA73B;
  FDigest[2] := $3C6EF372FE94F82B;
  FDigest[3] := $A54FF53A5F1D36F1;
  FDigest[4] := $510E527FADE682D1;
  FDigest[5] := $9B05688C2B3E6C1F;
  FDigest[6] := $1F83D9ABFB41BD6B;
  FDigest[7] := $5BE0CD19137E2179;
end;

// .THashBaseHaval
procedure THashBaseHaval.SetRounds(Value: Integer);
begin
  if (Value < 3) or (Value > 5) then
    if DigestSize <= 20 then Value := 3 else
      if DigestSize <= 28 then Value := 4
        else Value := 5;
  FRounds := Value;
  case FRounds of
    3: FTransform := DoTransform3;
    4: FTransform := DoTransform4;
    5: FTransform := DoTransform5;
  end;
end;

procedure THashBaseHaval.DoTransform(Buffer: PLongArray);
begin
  FTRansform(Buffer);
end;

{$IFNDEF THashBaseHaval_asm}
procedure THashBaseHaval.DoTransform3(Buffer: PLongArray);
var
  A,B,C,D,E,F,G,H,I,T: LongWord;
  Data: PLongWord;
  Offset: PByte;
begin
  Offset := @Haval_Offset;
  Data   := @Haval_Data;

  A := FDigest[0];
  B := FDigest[1];
  C := FDigest[2];
  D := FDigest[3];
  E := FDigest[4];
  F := FDigest[5];
  G := FDigest[6];
  H := FDigest[7];

  for I := 0 to 31 do
  begin
    T := C and (E xor D) xor G and A xor F and B xor E;
    T := (T shr 7 or T shl 25) + (H shr 11 or H shl 21) + Buffer[I];
    H := G; G := F; F := E; E := D; D := C; C := B; B := A; A := T;
  end;
  for I := 0 to 31 do
  begin
    T := F and (D and not A xor B and C xor E xor G) xor B and (D xor C) xor A and C xor G;
    T := (T shr 7 or T shl 25) + (H shr 11 or H shl 21) + Buffer[Offset^] + Data^;
    Inc(Offset); Inc(Data);
    H := G; G := F; F := E; E := D; D := C; C := B; B := A; A := T;
  end;
  for I := 0 to 31 do
  begin
    T := D and (F and E xor G xor A) xor F and C xor E and B xor A;
    T := (T shr 7 or T shl 25) + (H shr 11 or H shl 21) + Buffer[Offset^] + Data^;
    Inc(Offset); Inc(Data);
    H := G; G := F; F := E; E := D; D := C; C := B; B := A; A := T;
  end;

  Inc(FDigest[0], A);
  Inc(FDigest[1], B);
  Inc(FDigest[2], C);
  Inc(FDigest[3], D);
  Inc(FDigest[4], E);
  Inc(FDigest[5], F);
  Inc(FDigest[6], G);
  Inc(FDigest[7], H);
end;

procedure THashBaseHaval.DoTransform4(Buffer: PLongArray);
var
  A,B,C,D,E,F,G,H,I,T: LongWord;
  Data: PLongWord;
  Offset: PByte;
begin
  Offset := @Haval_Offset;
  Data   := @Haval_Data;

  A := FDigest[0];
  B := FDigest[1];
  C := FDigest[2];
  D := FDigest[3];
  E := FDigest[4];
  F := FDigest[5];
  G := FDigest[6];
  H := FDigest[7];

  for I := 0 to 31 do
  begin
    T := D and (A xor B) xor F and G xor E and C xor A;
    T := (T shr 7 or T shl 25) + (H shr 11 or H shl 21) + Buffer[I];
    H := G; G := F; F := E; E := D; D := C; C := B; B := A; A := T;
  end;
  for I := 0 to 31 do
  begin
    T := B and (G and not A xor C and F xor D xor E) xor C and (G xor F) xor A and F xor E;
    T := (T shr 7 or T shl 25) + (H shr 11 or H shl 21) + Buffer[Offset^] + Data^;
    Inc(Offset); Inc(Data);
    H := G; G := F; F := E; E := D; D := C; C := B; B := A; A := T;
  end;
  for I := 0 to 31 do
  begin
    T := G and (C and A xor B xor F) xor C and D xor A and E xor F;
    T := (T shr 7 or T shl 25) + (H shr 11 or H shl 21) + Buffer[Offset^] + Data^;
    Inc(Offset); Inc(Data);
    H := G; G := F; F := E; E := D; D := C; C := B; B := A; A := T;
  end;
  for I := 0 to 31 do
  begin
    T := A and (E and not C xor F and not G xor B xor G xor D) xor F and
        (B and C xor E xor G) xor C and G xor D;
    T := (T shr 7 or T shl 25) + (H shr 11 or H shl 21) + Buffer[Offset^] + Data^;
    Inc(Offset); Inc(Data);
    H := G; G := F; F := E; E := D; D := C; C := B; B := A; A := T;
  end;

  Inc(FDigest[0], A);
  Inc(FDigest[1], B);
  Inc(FDigest[2], C);
  Inc(FDigest[3], D);
  Inc(FDigest[4], E);
  Inc(FDigest[5], F);
  Inc(FDigest[6], G);
  Inc(FDigest[7], H);
end;


procedure THashBaseHaval.DoTransform5(Buffer: PLongArray);
var
  A,B,C,D,E,F,G,H,I,T: LongWord;
  Data: PLongWord;
  Offset: PByte;
begin
  Offset := @Haval_Offset;
  Data   := @Haval_Data;

  A := FDigest[0];
  B := FDigest[1];
  C := FDigest[2];
  D := FDigest[3];
  E := FDigest[4];
  F := FDigest[5];
  G := FDigest[6];
  H := FDigest[7];

  for I := 0 to 31 do
  begin
    T := C and (G xor B) xor F and E xor A and D xor G;
    T := (T shr 7 or T shl 25) + (H shr 11 or H shl 21) + Buffer[I];
    H := G; G := F; F := E; E := D; D := C; C := B; B := A; A := T;
  end;
  for I := 0 to 31 do
  begin
    T := D and (E and not A xor B and C xor G xor F) xor B and (E xor C) xor A and C xor F;
    T := (T shr 7 or T shl 25) + (H shr 11 or H shl 21) + Buffer[Offset^] + Data^;
    Inc(Offset); Inc(Data);
    H := G; G := F; F := E; E := D; D := C; C := B; B := A; A := T;
  end;
  for I := 0 to 31 do
  begin
    T := E and (B and D xor C xor F) xor B and A xor D and G xor F;
    T := (T shr 7 or T shl 25) + (H shr 11 or H shl 21) + Buffer[Offset^] + Data^;
    Inc(Offset); Inc(Data);
    H := G; G := F; F := E; E := D; D := C; C := B; B := A; A := T;
  end;
  for I := 0 to 31 do
  begin
    T := D and (F and not A xor C and not B xor E xor B xor G) xor C and
        (E and A xor F xor B) xor A and B xor G;
    T := (T shr 7 or T shl 25) + (H shr 11 or H shl 21) + Buffer[Offset^] + Data^;
    Inc(Offset); Inc(Data);
    H := G; G := F; F := E; E := D; D := C; C := B; B := A; A := T;
  end;
  for I := 0 to 31 do
  begin
    T := B and (D and E and G xor not F) xor D and A xor E and F xor G and C;
    T := (T shr 7 or T shl 25) + (H shr 11 or H shl 21) + Buffer[Offset^] + Data^;
    Inc(Offset); Inc(Data);
    H := G; G := F; F := E; E := D; D := C; C := B; B := A; A := T;
  end;

  Inc(FDigest[0], A);
  Inc(FDigest[1], B);
  Inc(FDigest[2], C);
  Inc(FDigest[3], D);
  Inc(FDigest[4], E);
  Inc(FDigest[5], F);
  Inc(FDigest[6], G);
  Inc(FDigest[7], H);
end;
{$ENDIF}

procedure THashBaseHaval.DoInit;
begin
  SetRounds(FRounds);
  FDigest[0] := $243F6A88;
  FDigest[1] := $85A308D3;
  FDigest[2] := $13198A2E;
  FDigest[3] := $03707344;
  FDigest[4] := $A4093822;
  FDigest[5] := $299F31D0;
  FDigest[6] := $082EFA98;
  FDigest[7] := $EC4E6C89;
end;

procedure THashBaseHaval.DoDone;

  function ROR(Value,Count: LongWord): LongWord;
  asm
     MOV  ECX,EDX
     ROR  EAX,CL
  end;

var
  T: Word;
begin
  if FCount[2] or FCount[3] <> 0 then HashingOverflowError;
  if FPaddingByte = 0 then FPaddingByte := $01;
  FBuffer[FBufferIndex] := FPaddingByte;
  Inc(FBufferIndex);
  if FBufferIndex > FBufferSize -10 then
  begin
    FillChar(FBuffer[FBufferIndex], FBufferSize - FBufferIndex - 10, 0);
    DoTransform(Pointer(FBuffer));
    FBufferIndex := 0;
  end;
  FillChar(FBuffer[FBufferIndex], FBufferSize - FBufferIndex -10, 0);
  T := DigestSize shl 9 or FRounds shl 3 or 1;
  Move(T, FBuffer[FBufferSize - 10], SizeOf(T));
  Move(FCount, FBuffer[FBufferSize - 8], 8);
  DoTransform(Pointer(FBuffer));

  case DigestSize of
    16: begin
          Inc(FDigest[0], ROR(FDigest[7] and $000000FF or
                              FDigest[6] and $FF000000 or
                              FDigest[5] and $00FF0000 or
                              FDigest[4] and $0000FF00, 8));
          Inc(FDigest[1], ROR(FDigest[7] and $0000FF00 or
                              FDigest[6] and $000000FF or
                              FDigest[5] and $FF000000 or
                              FDigest[4] and $00FF0000, 16));
          Inc(FDigest[2], ROR(FDigest[7] and $00FF0000 or
                              FDigest[6] and $0000FF00 or
                              FDigest[5] and $000000FF or
                              FDigest[4] and $FF000000, 24));
          Inc(FDigest[3],     FDigest[7] and $FF000000 or
                              FDigest[6] and $00FF0000 or
                              FDigest[5] and $0000FF00 or
                              FDigest[4] and $000000FF);
        end;
    20: begin
          Inc(FDigest[0], ROR(FDigest[7] and ($3F) or
                              FDigest[6] and ($7F shl 25) or
                              FDigest[5] and ($3F shl 19), 19));
          Inc(FDigest[1], ROR(FDigest[7] and ($3F shl 6) or
                              FDigest[6] and ($3F) or
                              FDigest[5] and ($7F shl 25), 25));
          Inc(FDigest[2],     FDigest[7] and ($7F shl 12) or
                              FDigest[6] and ($3F shl  6) or
                              FDigest[5] and ($3F));
          Inc(FDigest[3],    (FDigest[7] and ($3F shl 19) or
                              FDigest[6] and ($7F shl 12) or
                              FDigest[5] and ($3F shl  6)) shr 6);
          Inc(FDigest[4],    (FDigest[7] and ($7F shl 25) or
                              FDigest[6] and ($3F shl 19) or
                              FDigest[5] and ($7F shl 12)) shr 12);
        end;
    24: begin
          Inc(FDigest[0], ROR(FDigest[7] and ($1F) or
                              FDigest[6] and ($3F shl 26), 26));
          Inc(FDigest[1],     FDigest[7] and ($1F shl 5) or
                              FDigest[6] and ($1F));
          Inc(FDigest[2],    (FDigest[7] and ($3F shl 10) or
                              FDigest[6] and ($1F shl  5)) shr 5);
          Inc(FDigest[3],    (FDigest[7] and ($1F shl 16) or
                              FDigest[6] and ($3F shl 10)) shr 10);
          Inc(FDigest[4],    (FDigest[7] and ($1F shl 21) or
                              FDigest[6] and ($1F shl 16)) shr 16);
          Inc(FDigest[5],    (FDigest[7] and ($3F shl 26) or
                              FDigest[6] and ($1F shl 21)) shr 21);
        end;
    28: begin
          Inc(FDigest[0], FDigest[7] shr 27 and $1F);
          Inc(FDigest[1], FDigest[7] shr 22 and $1F);
          Inc(FDigest[2], FDigest[7] shr 18 and $0F);
          Inc(FDigest[3], FDigest[7] shr 13 and $1F);
          Inc(FDigest[4], FDigest[7] shr  9 and $0F);
          Inc(FDigest[5], FDigest[7] shr  4 and $1F);
          Inc(FDigest[6], FDigest[7]        and $0F);
        end;
  end;
end;

class function THashBaseHaval.BlockSize: Integer;
begin
  Result := 128;
end;

function THashBaseHaval.Digest: PByteArray;
begin
  Result := @FDigest;
end;

class function THash_Haval128.DigestSize: Integer;
begin
  Result := 16;
end;

class function THash_Haval160.DigestSize: Integer;
begin
  Result := 20;
end;

class function THash_Haval192.DigestSize: Integer;
begin
  Result := 24;
end;

class function THash_Haval224.DigestSize: Integer;
begin
  Result := 28;
end;

class function THash_Haval256.DigestSize: Integer;
begin
  Result := 32;
end;

// .THash_Tiger
class function THash_Tiger.DigestSize: Integer;
begin
  Result := 24;
end;

procedure THash_Tiger.SetRounds(Value: Integer);
begin
  if (Value < 3) or (Value > 32) then Value := 3;
  FRounds := Value;
end;

{$IFNDEF THash_Tiger_asm}
procedure THash_Tiger.DoTransform(Buffer: PLongArray);
type
  PTiger_Data = ^TTiger_Data;
  TTiger_Data = array[0..3, 0..255] of Int64;

  PInt64Array = ^TInt64Array;
  TInt64Array = array[0..7] of Int64;

var
  A,B,C,T: Int64;
  x0,x1,x2,x3,x4,x5,x6,x7: Int64;
  I: Integer;
begin
  A  := PInt64Array(@FDigest)[0];
  B  := PInt64Array(@FDigest)[1];
  C  := PInt64Array(@FDigest)[2];
  x0 := PInt64Array(Buffer)[0];
  x1 := PInt64Array(Buffer)[1];
  x2 := PInt64Array(Buffer)[2];
  x3 := PInt64Array(Buffer)[3];
  x4 := PInt64Array(Buffer)[4];
  x5 := PInt64Array(Buffer)[5];
  x6 := PInt64Array(Buffer)[6];
  x7 := PInt64Array(Buffer)[7];

  for I := 1 to FRounds do {a Loop is faster for PC's with small Cache}
  begin
    if I > 1 then {key schedule}
    begin
      Dec(x0, x7 xor $A5A5A5A5A5A5A5A5);
      x1 := x1 xor x0;
      Inc(x2, x1);
      Dec(x3, x2 xor (not x1 shl 19));
      x4 := x4 xor x3;
      Inc(x5, x4);
      Dec(x6, x5 xor (not x4 shr 23));
      x7 := x7 xor x6;
      Inc(x0, x7);
      Dec(x1, x0 xor (not x7 shl 19));
      x2 := x2 xor x1;
      Inc(x3, x2);
      Dec(x4, x3 xor (not x2 shr 23));
      x5 := x5 xor x4;
      Inc(x6, x5);
      Dec(x7, x6 xor $0123456789ABCDEF);
    end;

    C := C xor x0;
    Dec(A, TTiger_Data(Tiger_Data)[0, LongWord(C)        and $FF] xor
           TTiger_Data(Tiger_Data)[1, LongWord(C) shr 16 and $FF] xor
           TTiger_Data(Tiger_Data)[2,          C  shr 32 and $FF] xor
           TTiger_Data(Tiger_Data)[3, LongWord(C shr 32) shr 16 and $FF]);
    Inc(B, TTiger_Data(Tiger_Data)[3, LongWord(C) shr  8 and $FF] xor
           TTiger_Data(Tiger_Data)[2, LongWord(C) shr 24] xor
           TTiger_Data(Tiger_Data)[1, LongWord(C shr 32) shr 8 and $FF] xor
           TTiger_Data(Tiger_Data)[0, LongWord(C shr 32) shr 24]);
    if I = 1 then B := B shl 2 + B else
      if I = 2 then B := B shl 3 - B
        else B := B shl 3 + B;

    A := A xor x1;
    Dec(B, TTiger_Data(Tiger_Data)[0, LongWord(A)        and $FF] xor
           TTiger_Data(Tiger_Data)[1, LongWord(A) shr 16 and $FF] xor
           TTiger_Data(Tiger_Data)[2,          A  shr 32 and $FF] xor
           TTiger_Data(Tiger_Data)[3, LongWord(A shr 32) shr 16 and $FF]);
    Inc(C, TTiger_Data(Tiger_Data)[3, LongWord(A) shr  8 and $FF] xor
           TTiger_Data(Tiger_Data)[2, LongWord(A) shr 24] xor
           TTiger_Data(Tiger_Data)[1, LongWord(A shr 32) shr 8 and $FF] xor
           TTiger_Data(Tiger_Data)[0, LongWord(A shr 32) shr 24]);
    if I = 1 then C := C shl 2 + C else
      if I = 2 then C := C shl 3 - C
        else C := C shl 3 + C;

    B := B xor x2;
    Dec(C, TTiger_Data(Tiger_Data)[0, LongWord(B)        and $FF] xor
           TTiger_Data(Tiger_Data)[1, LongWord(B) shr 16 and $FF] xor
           TTiger_Data(Tiger_Data)[2,          B  shr 32 and $FF] xor
           TTiger_Data(Tiger_Data)[3, LongWord(B shr 32) shr 16 and $FF]);
    Inc(A, TTiger_Data(Tiger_Data)[3, LongWord(B) shr  8 and $FF] xor
           TTiger_Data(Tiger_Data)[2, LongWord(B) shr 24] xor
           TTiger_Data(Tiger_Data)[1, LongWord(B shr 32) shr 8 and $FF] xor
           TTiger_Data(Tiger_Data)[0, LongWord(B shr 32) shr 24]);
    if I = 1 then A := A shl 2 + A else
      if I = 2 then A := A shl 3 - A
        else A := A shl 3 + A;

    C := C xor x3;
    Dec(A, TTiger_Data(Tiger_Data)[0, LongWord(C)        and $FF] xor
           TTiger_Data(Tiger_Data)[1, LongWord(C) shr 16 and $FF] xor
           TTiger_Data(Tiger_Data)[2,          C  shr 32 and $FF] xor
           TTiger_Data(Tiger_Data)[3, LongWord(C shr 32) shr 16 and $FF]);
    Inc(B, TTiger_Data(Tiger_Data)[3, LongWord(C) shr  8 and $FF] xor
           TTiger_Data(Tiger_Data)[2, LongWord(C) shr 24] xor
           TTiger_Data(Tiger_Data)[1, LongWord(C shr 32) shr 8 and $FF] xor
           TTiger_Data(Tiger_Data)[0, LongWord(C shr 32) shr 24]);
    if I = 1 then B := B shl 2 + B else
      if I = 2 then B := B shl 3 - B
        else B := B shl 3 + B;

    A := A xor x4;
    Dec(B, TTiger_Data(Tiger_Data)[0, LongWord(A)        and $FF] xor
           TTiger_Data(Tiger_Data)[1, LongWord(A) shr 16 and $FF] xor
           TTiger_Data(Tiger_Data)[2,          A  shr 32 and $FF] xor
           TTiger_Data(Tiger_Data)[3, LongWord(A shr 32) shr 16 and $FF]);
    Inc(C, TTiger_Data(Tiger_Data)[3, LongWord(A) shr  8 and $FF] xor
           TTiger_Data(Tiger_Data)[2, LongWord(A) shr 24] xor
           TTiger_Data(Tiger_Data)[1, LongWord(A shr 32) shr 8 and $FF] xor
           TTiger_Data(Tiger_Data)[0, LongWord(A shr 32) shr 24]);
    if I = 1 then C := C shl 2 + C else
      if I = 2 then C := C shl 3 - C
        else C := C shl 3 + C;

    B := B xor x5;
    Dec(C, TTiger_Data(Tiger_Data)[0, LongWord(B)        and $FF] xor
           TTiger_Data(Tiger_Data)[1, LongWord(B) shr 16 and $FF] xor
           TTiger_Data(Tiger_Data)[2,          B  shr 32 and $FF] xor
           TTiger_Data(Tiger_Data)[3, LongWord(B shr 32) shr 16 and $FF]);
    Inc(A, TTiger_Data(Tiger_Data)[3, LongWord(B) shr  8 and $FF] xor
           TTiger_Data(Tiger_Data)[2, LongWord(B) shr 24] xor
           TTiger_Data(Tiger_Data)[1, LongWord(B shr 32) shr 8 and $FF] xor
           TTiger_Data(Tiger_Data)[0, LongWord(B shr 32) shr 24]);
    if I = 1 then A := A shl 2 + A else
      if I = 2 then A := A shl 3 - A
        else A := A shl 3 + A;

    C := C xor x6;
    Dec(A, TTiger_Data(Tiger_Data)[0, LongWord(C)        and $FF] xor
           TTiger_Data(Tiger_Data)[1, LongWord(C) shr 16 and $FF] xor
           TTiger_Data(Tiger_Data)[2,          C  shr 32 and $FF] xor
           TTiger_Data(Tiger_Data)[3, LongWord(C shr 32) shr 16 and $FF]);
    Inc(B, TTiger_Data(Tiger_Data)[3, LongWord(C) shr  8 and $FF] xor
           TTiger_Data(Tiger_Data)[2, LongWord(C) shr 24] xor
           TTiger_Data(Tiger_Data)[1, LongWord(C shr 32) shr 8 and $FF] xor
           TTiger_Data(Tiger_Data)[0, LongWord(C shr 32) shr 24]);
    if I = 1 then B := B shl 2 + B else
      if I = 2 then B := B shl 3 - B
        else B := B shl 3 + B;

    A := A xor x7;
    Dec(B, TTiger_Data(Tiger_Data)[0, LongWord(A)        and $FF] xor
           TTiger_Data(Tiger_Data)[1, LongWord(A) shr 16 and $FF] xor
           TTiger_Data(Tiger_Data)[2,          A  shr 32 and $FF] xor
           TTiger_Data(Tiger_Data)[3, LongWord(A shr 32) shr 16 and $FF]);
    Inc(C, TTiger_Data(Tiger_Data)[3, LongWord(A) shr  8 and $FF] xor
           TTiger_Data(Tiger_Data)[2, LongWord(A) shr 24] xor
           TTiger_Data(Tiger_Data)[1, LongWord(A shr 32) shr 8 and $FF] xor
           TTiger_Data(Tiger_Data)[0, LongWord(A shr 32) shr 24]);
    if I = 1 then C := C shl 2 + C else
      if I = 2 then C := C shl 3 - C
        else C := C shl 3 + C;

    T := A; A := C; C := B; B := T;
  end;

  PInt64Array(@FDigest)[0] := A xor PInt64Array(@FDigest)[0];
  PInt64Array(@FDigest)[1] := B  -  PInt64Array(@FDigest)[1];
  PInt64Array(@FDigest)[2] := C  +  PInt64Array(@FDigest)[2];
end;
{$ENDIF}

procedure THash_Tiger.DoInit;
begin
  SetRounds(FRounds);
  if FPaddingByte = 0 then FPaddingByte := $01;
  FDigest[0] := $89ABCDEF;
  FDigest[1] := $01234567;
  FDigest[2] := $76543210;
  FDigest[3] := $FEDCBA98;
  FDigest[4] := $C3B2E187;
  FDigest[5] := $F096A5B4;
end;

// .THash_Panama
class function THash_Panama.DigestSize: Integer;
begin
  Result := 32;
end;

class function THash_Panama.BlockSize: Integer;
begin
  Result := 32
end;

function THash_Panama.Digest: PByteArray;
begin
  Result := @FDigest;
end;

procedure THash_Panama.DoInit;
begin
  FillChar(FLFSRBuffer, SizeOf(FLFSRBuffer), 0);
  FillChar(FDigest, SizeOf(FDigest), 0);
  FTap := 0;
end;

procedure THash_Panama.DoDone;
begin
  if FPaddingByte = 0 then FPaddingByte := $01;
  FBuffer[FBufferIndex] := FPaddingByte;
  Inc(FBufferIndex);
  FillChar(FBuffer[FBufferIndex], FBufferSize - FBufferIndex, 0);
  DoTransform(Pointer(FBuffer));
  DoPull;
  FillChar(FLFSRBuffer, SizeOf(FLFSRBuffer), 0);
  FTap := 0;
end;

{$IFNDEF THash_Panama_asm}
procedure THash_Panama.DoPull;
var
  PBufL,PBufB,PTap0,PTap25: PLongArray;
  T0,T1,T2,T3: LongWord;
  I: Integer;
begin
  for I := 0 to 31 do
  begin
    // LFSR emulation
    PBufL := @FLFSRBuffer[(FTap +  4) and 31];
    PBufB := @FLFSRBuffer[(FTap + 16) and 31];
    FTap := (FTap - 1) and 31;
    PTap0  := @FLFSRBuffer[FTap];
    PTap25 := @FLFSRBuffer[(FTap + 25) and 31];
    // update the LFSR buffer (LAMBDA_PULL)
    PTap25[ 0] := PTap25[ 0] xor PTap0[ 2];
    PTap25[ 1] := PTap25[ 1] xor PTap0[ 3];
    PTap25[ 2] := PTap25[ 2] xor PTap0[ 4];
    PTap25[ 3] := PTap25[ 3] xor PTap0[ 5];
    PTap25[ 4] := PTap25[ 4] xor PTap0[ 6];
    PTap25[ 5] := PTap25[ 5] xor PTap0[ 7];
    PTap25[ 6] := PTap25[ 6] xor PTap0[ 0];
    PTap25[ 7] := PTap25[ 7] xor PTap0[ 1];
    PTap0[ 0] := PTap0[ 0] xor FDigest[ 1];
    PTap0[ 1] := PTap0[ 1] xor FDigest[ 2];
    PTap0[ 2] := PTap0[ 2] xor FDigest[ 3];
    PTap0[ 3] := PTap0[ 3] xor FDigest[ 4];
    PTap0[ 4] := PTap0[ 4] xor FDigest[ 5];
    PTap0[ 5] := PTap0[ 5] xor FDigest[ 6];
    PTap0[ 6] := PTap0[ 6] xor FDigest[ 7];
    PTap0[ 7] := PTap0[ 7] xor FDigest[ 8];
    // perform non-linearity stage (GAMMA)
    T0 := FDigest[ 0];
    T1 := FDigest[ 1];
    FDigest[ 0] := FDigest[ 0] xor (FDigest[ 1] or not FDigest[ 2]);
    FDigest[ 1] := FDigest[ 1] xor (FDigest[ 2] or not FDigest[ 3]);
    FDigest[ 2] := FDigest[ 2] xor (FDigest[ 3] or not FDigest[ 4]);
    FDigest[ 3] := FDigest[ 3] xor (FDigest[ 4] or not FDigest[ 5]);
    FDigest[ 4] := FDigest[ 4] xor (FDigest[ 5] or not FDigest[ 6]);
    FDigest[ 5] := FDigest[ 5] xor (FDigest[ 6] or not FDigest[ 7]);
    FDigest[ 6] := FDigest[ 6] xor (FDigest[ 7] or not FDigest[ 8]);
    FDigest[ 7] := FDigest[ 7] xor (FDigest[ 8] or not FDigest[ 9]);
    FDigest[ 8] := FDigest[ 8] xor (FDigest[ 9] or not FDigest[10]);
    FDigest[ 9] := FDigest[ 9] xor (FDigest[10] or not FDigest[11]);
    FDigest[10] := FDigest[10] xor (FDigest[11] or not FDigest[12]);
    FDigest[11] := FDigest[11] xor (FDigest[12] or not FDigest[13]);
    FDigest[12] := FDigest[12] xor (FDigest[13] or not FDigest[14]);
    FDigest[13] := FDigest[13] xor (FDigest[14] or not FDigest[15]);
    FDigest[14] := FDigest[14] xor (FDigest[15] or not FDigest[16]);
    FDigest[15] := FDigest[15] xor (FDigest[16] or not T0);
    FDigest[16] := FDigest[16] xor (T0 or not T1);

    // perform bit-dispersion stage (PI)
    T0 := FDigest[ 1];
    T1 := FDigest[ 7]; FDigest[ 1] := (T1 shl  1) or (T1 shr 31);
    T1 := FDigest[ 5]; FDigest[ 5] := (T0 shl 15) or (T0 shr 17);
    T0 := FDigest[ 8]; FDigest[ 8] := (T1 shl  4) or (T1 shr 28);
    T1 := FDigest[ 6]; FDigest[ 6] := (T0 shl 21) or (T0 shr 11);
    T0 := FDigest[13]; FDigest[13] := (T1 shl 27) or (T1 shr  5);
    T1 := FDigest[14]; FDigest[14] := (T0 shl  9) or (T0 shr 23);
    T0 := FDigest[ 2]; FDigest[ 2] := (T1 shl  3) or (T1 shr 29);
    T1 := FDigest[10]; FDigest[10] := (T0 shl 23) or (T0 shr  9);
    T0 := FDigest[16]; FDigest[16] := (T1 shl  8) or (T1 shr 24);
    T1 := FDigest[12]; FDigest[12] := (T0 shl 14) or (T0 shr 18);
    T0 := FDigest[ 9]; FDigest[ 9] := (T1 shl 13) or (T1 shr 19);
    T1 := FDigest[11]; FDigest[11] := (T0 shl  2) or (T0 shr 30);
    T0 := FDigest[ 4]; FDigest[ 4] := (T1 shl 10) or (T1 shr 22);
    T1 := FDigest[ 3]; FDigest[ 3] := (T0 shl  6) or (T0 shr 26);
    T0 := FDigest[15]; FDigest[15] := (T1 shl 24) or (T1 shr  8);
    FDigest[ 7] := (T0 shl 28) or (T0 shr  4);

    // perform diffusion stage (THETA) + buffer injection stage (SIGMA)
    T0 := FDigest[ 0];
    T1 := FDigest[ 1];
    T2 := FDigest[ 2];
    T3 := FDigest[ 3];
    FDigest[ 0] := FDigest[ 0] xor FDigest[ 1] xor FDigest[ 4] xor 1;
    FDigest[ 1] := FDigest[ 1] xor FDigest[ 2] xor FDigest[ 5] xor PBufL[ 0];
    FDigest[ 2] := FDigest[ 2] xor FDigest[ 3] xor FDigest[ 6] xor PBufL[ 1];
    FDigest[ 3] := FDigest[ 3] xor FDigest[ 4] xor FDigest[ 7] xor PBufL[ 2];
    FDigest[ 4] := FDigest[ 4] xor FDigest[ 5] xor FDigest[ 8] xor PBufL[ 3];
    FDigest[ 5] := FDigest[ 5] xor FDigest[ 6] xor FDigest[ 9] xor PBufL[ 4];
    FDigest[ 6] := FDigest[ 6] xor FDigest[ 7] xor FDigest[10] xor PBufL[ 5];
    FDigest[ 7] := FDigest[ 7] xor FDigest[ 8] xor FDigest[11] xor PBufL[ 6];
    FDigest[ 8] := FDigest[ 8] xor FDigest[ 9] xor FDigest[12] xor PBufL[ 7];
    FDigest[ 9] := FDigest[ 9] xor FDigest[10] xor FDigest[13] xor PBufB[ 0];
    FDigest[10] := FDigest[10] xor FDigest[11] xor FDigest[14] xor PBufB[ 1];
    FDigest[11] := FDigest[11] xor FDigest[12] xor FDigest[15] xor PBufB[ 2];
    FDigest[12] := FDigest[12] xor FDigest[13] xor FDigest[16] xor PBufB[ 3];
    FDigest[13] := FDigest[13] xor FDigest[14] xor T0          xor PBufB[ 4];
    FDigest[14] := FDigest[14] xor FDigest[15] xor T1          xor PBufB[ 5];
    FDigest[15] := FDigest[15] xor FDigest[16] xor T2          xor PBufB[ 6];
    FDigest[16] := FDigest[16] xor T0 xor T3                   xor PBufB[ 7];
  end;
  // move state to Digest buffer
  FDigest[0] := FDigest[ 9];
  FDigest[1] := FDigest[10];
  FDigest[2] := FDigest[11];
  FDigest[3] := FDigest[12];
  FDigest[4] := FDigest[13];
  FDigest[5] := FDigest[14];
  FDigest[6] := FDigest[15];
  FDigest[7] := FDigest[16];
end;

procedure THash_Panama.DoTransform(Buffer: PLongArray);
var
  T0,T1,T2,T3 : LongWord;
  PBufB,PTap0,PTap25: PLongArray;
begin
  // perform non-linearity stage (GAMMA)
  T0 := FDigest[ 0];
  T1 := FDigest[ 1];
  FDigest[ 0] := FDigest[ 0] xor (FDigest[ 1] or not FDigest[ 2]);
  FDigest[ 1] := FDigest[ 1] xor (FDigest[ 2] or not FDigest[ 3]);
  FDigest[ 2] := FDigest[ 2] xor (FDigest[ 3] or not FDigest[ 4]);
  FDigest[ 3] := FDigest[ 3] xor (FDigest[ 4] or not FDigest[ 5]);
  FDigest[ 4] := FDigest[ 4] xor (FDigest[ 5] or not FDigest[ 6]);
  FDigest[ 5] := FDigest[ 5] xor (FDigest[ 6] or not FDigest[ 7]);
  FDigest[ 6] := FDigest[ 6] xor (FDigest[ 7] or not FDigest[ 8]);
  FDigest[ 7] := FDigest[ 7] xor (FDigest[ 8] or not FDigest[ 9]);
  FDigest[ 8] := FDigest[ 8] xor (FDigest[ 9] or not FDigest[10]);
  FDigest[ 9] := FDigest[ 9] xor (FDigest[10] or not FDigest[11]);
  FDigest[10] := FDigest[10] xor (FDigest[11] or not FDigest[12]);
  FDigest[11] := FDigest[11] xor (FDigest[12] or not FDigest[13]);
  FDigest[12] := FDigest[12] xor (FDigest[13] or not FDigest[14]);
  FDigest[13] := FDigest[13] xor (FDigest[14] or not FDigest[15]);
  FDigest[14] := FDigest[14] xor (FDigest[15] or not FDigest[16]);
  FDigest[15] := FDigest[15] xor (FDigest[16] or not T0);
  FDigest[16] := FDigest[16] xor (T0 or not T1);

  // perform bit-dispersion stage (PI)
  T0 := FDigest[ 1];
  T1 := FDigest[ 7]; FDigest[ 1] := (T1 shl  1) or (T1 shr 31);
  T1 := FDigest[ 5]; FDigest[ 5] := (T0 shl 15) or (T0 shr 17);
  T0 := FDigest[ 8]; FDigest[ 8] := (T1 shl  4) or (T1 shr 28);
  T1 := FDigest[ 6]; FDigest[ 6] := (T0 shl 21) or (T0 shr 11);
  T0 := FDigest[13]; FDigest[13] := (T1 shl 27) or (T1 shr  5);
  T1 := FDigest[14]; FDigest[14] := (T0 shl  9) or (T0 shr 23);
  T0 := FDigest[ 2]; FDigest[ 2] := (T1 shl  3) or (T1 shr 29);
  T1 := FDigest[10]; FDigest[10] := (T0 shl 23) or (T0 shr  9);
  T0 := FDigest[16]; FDigest[16] := (T1 shl  8) or (T1 shr 24);
  T1 := FDigest[12]; FDigest[12] := (T0 shl 14) or (T0 shr 18);
  T0 := FDigest[ 9]; FDigest[ 9] := (T1 shl 13) or (T1 shr 19);
  T1 := FDigest[11]; FDigest[11] := (T0 shl  2) or (T0 shr 30);
  T0 := FDigest[ 4]; FDigest[ 4] := (T1 shl 10) or (T1 shr 22);
  T1 := FDigest[ 3]; FDigest[ 3] := (T0 shl  6) or (T0 shr 26);
  T0 := FDigest[15]; FDigest[15] := (T1 shl 24) or (T1 shr  8);
  FDigest[ 7] := (T0 shl 28) or (T0 shr  4);

  // LFSR emulation
  PBufB  := @FLFSRBuffer[(FTap + 16) and 31];
  FTap   := (FTap - 1) and 31;
  PTap0  := @FLFSRBuffer[FTap];
  PTap25 := @FLFSRBuffer[(FTap + 25) and 31];

  // update the LFSR buffer (LAMBDA_PUSH)
  PTap25[ 0] := PTap25[ 0] xor PTap0[ 2];
  PTap25[ 1] := PTap25[ 1] xor PTap0[ 3];
  PTap25[ 2] := PTap25[ 2] xor PTap0[ 4];
  PTap25[ 3] := PTap25[ 3] xor PTap0[ 5];
  PTap25[ 4] := PTap25[ 4] xor PTap0[ 6];
  PTap25[ 5] := PTap25[ 5] xor PTap0[ 7];
  PTap25[ 6] := PTap25[ 6] xor PTap0[ 0];
  PTap25[ 7] := PTap25[ 7] xor PTap0[ 1];
  PTap0[ 0] := PTap0[ 0] xor Buffer[ 0];
  PTap0[ 1] := PTap0[ 1] xor Buffer[ 1];
  PTap0[ 2] := PTap0[ 2] xor Buffer[ 2];
  PTap0[ 3] := PTap0[ 3] xor Buffer[ 3];
  PTap0[ 4] := PTap0[ 4] xor Buffer[ 4];
  PTap0[ 5] := PTap0[ 5] xor Buffer[ 5];
  PTap0[ 6] := PTap0[ 6] xor Buffer[ 6];
  PTap0[ 7] := PTap0[ 7] xor Buffer[ 7];

  // perform diffusion stage (THETA) + buffer injection stage (SIGMA)
  T0 := FDigest[ 0];
  T1 := FDigest[ 1];
  T2 := FDigest[ 2];
  T3 := FDigest[ 3];
  FDigest[ 0] := FDigest[ 0] xor FDigest[ 1] xor FDigest[ 4] xor 1;
  FDigest[ 1] := FDigest[ 1] xor FDigest[ 2] xor FDigest[ 5] xor Buffer[ 0];
  FDigest[ 2] := FDigest[ 2] xor FDigest[ 3] xor FDigest[ 6] xor Buffer[ 1];
  FDigest[ 3] := FDigest[ 3] xor FDigest[ 4] xor FDigest[ 7] xor Buffer[ 2];
  FDigest[ 4] := FDigest[ 4] xor FDigest[ 5] xor FDigest[ 8] xor Buffer[ 3];
  FDigest[ 5] := FDigest[ 5] xor FDigest[ 6] xor FDigest[ 9] xor Buffer[ 4];
  FDigest[ 6] := FDigest[ 6] xor FDigest[ 7] xor FDigest[10] xor Buffer[ 5];
  FDigest[ 7] := FDigest[ 7] xor FDigest[ 8] xor FDigest[11] xor Buffer[ 6];
  FDigest[ 8] := FDigest[ 8] xor FDigest[ 9] xor FDigest[12] xor Buffer[ 7];

  FDigest[ 9] := FDigest[ 9] xor FDigest[10] xor FDigest[13] xor PBufB[ 0];
  FDigest[10] := FDigest[10] xor FDigest[11] xor FDigest[14] xor PBufB[ 1];
  FDigest[11] := FDigest[11] xor FDigest[12] xor FDigest[15] xor PBufB[ 2];
  FDigest[12] := FDigest[12] xor FDigest[13] xor FDigest[16] xor PBufB[ 3];
  FDigest[13] := FDigest[13] xor FDigest[14] xor T0          xor PBufB[ 4];
  FDigest[14] := FDigest[14] xor FDigest[15] xor T1          xor PBufB[ 5];
  FDigest[15] := FDigest[15] xor FDigest[16] xor T2          xor PBufB[ 6];
  FDigest[16] := FDigest[16] xor T0          xor T3          xor PBufB[ 7];
end;
{$ENDIF}

// .THashBaseWhirlpool
class function THashBaseWhirlpool.DigestSize: Integer;
begin
  Result := 64;
end;

class function THashBaseWhirlpool.BlockSize: Integer;
begin
  Result := 64;
end;

function THashBaseWhirlpool.Digest: PByteArray;
begin
  Result := @FDigest;
end;

procedure THashBaseWhirlpool.DoDone;
var
  I: Integer;
begin
  if FPaddingByte = 0 then FPaddingByte := $80;
  FBuffer[FBufferIndex] := FPaddingByte;
  Inc(FBufferIndex);
  if FBufferIndex > FBufferSize - 32 then
  begin
    FillChar(FBuffer[FBufferIndex], FBufferSize - FBufferIndex, 0);
    DoTransform(Pointer(FBuffer));
    FBufferIndex := 0;
  end;
  FillChar(FBuffer[FBufferIndex], FBufferSize - FBufferIndex, 0);
  for I := 31 downto 0 do
    FBuffer[63 - I] := PByteArray(@FCount)[I];
  DoTransform(Pointer(FBuffer));
end;

{$IFNDEF THashBaseWhirlpool_asm}
procedure THashBaseWhirlpool.DoTransform(Buffer: PLongArray);
type
  PWhirlData = ^TWhirlData;
  TWhirlData = array[0..15] of LongWord;
  PWhirlTable = ^TWhirlTable;
  TWhirlTable = array[0..7, 0..511] of LongWord;

  procedure Whirl(var L: TWhirlData; const K: TWhirlData; const T: PWhirlTable);
  begin
    L[0*2+0] := T[0, ((K[ 0] shl  1) and $1fe)] xor
                T[1, ((K[14] shr  7) and $1fe)] xor
                T[2, ((K[12] shr 15) and $1fe)] xor
                T[3, ((K[10] shr 23) and $1fe)] xor
                T[4, ((K[ 9] shl  1) and $1fe)] xor
                T[5, ((K[ 7] shr  7) and $1fe)] xor
                T[6, ((K[ 5] shr 15) and $1fe)] xor
                T[7, ((K[ 3] shr 23) and $1fe)];
    L[0*2+1] := T[0, ((K[ 0] shl  1) and $1fe)+1] xor
                T[1, ((K[14] shr  7) and $1fe)+1] xor
                T[2, ((K[12] shr 15) and $1fe)+1] xor
                T[3, ((K[10] shr 23) and $1fe)+1] xor
                T[4, ((K[ 9] shl  1) and $1fe)+1] xor
                T[5, ((K[ 7] shr  7) and $1fe)+1] xor
                T[6, ((K[ 5] shr 15) and $1fe)+1] xor
                T[7, ((K[ 3] shr 23) and $1fe)+1];
    L[1*2+0] := T[0, ((K[ 2] shl  1) and $1fe)] xor
                T[1, ((K[ 0] shr  7) and $1fe)] xor
                T[2, ((K[14] shr 15) and $1fe)] xor
                T[3, ((K[12] shr 23) and $1fe)] xor
                T[4, ((K[11] shl  1) and $1fe)] xor
                T[5, ((K[ 9] shr  7) and $1fe)] xor
                T[6, ((K[ 7] shr 15) and $1fe)] xor
                T[7, ((K[ 5] shr 23) and $1fe)];
    L[1*2+1] := T[0, ((K[ 2] shl  1) and $1fe)+1] xor
                T[1, ((K[ 0] shr  7) and $1fe)+1] xor
                T[2, ((K[14] shr 15) and $1fe)+1] xor
                T[3, ((K[12] shr 23) and $1fe)+1] xor
                T[4, ((K[11] shl  1) and $1fe)+1] xor
                T[5, ((K[ 9] shr  7) and $1fe)+1] xor
                T[6, ((K[ 7] shr 15) and $1fe)+1] xor
                T[7, ((K[ 5] shr 23) and $1fe)+1];
    L[2*2+0] := T[0, ((K[ 4] shl  1) and $1fe)] xor
                T[1, ((K[ 2] shr  7) and $1fe)] xor
                T[2, ((K[ 0] shr 15) and $1fe)] xor
                T[3, ((K[14] shr 23) and $1fe)] xor
                T[4, ((K[13] shl  1) and $1fe)] xor
                T[5, ((K[11] shr  7) and $1fe)] xor
                T[6, ((K[ 9] shr 15) and $1fe)] xor
                T[7, ((K[ 7] shr 23) and $1fe)];
    L[2*2+1] := T[0, ((K[ 4] shl  1) and $1fe)+1] xor
                T[1, ((K[ 2] shr  7) and $1fe)+1] xor
                T[2, ((K[ 0] shr 15) and $1fe)+1] xor
                T[3, ((K[14] shr 23) and $1fe)+1] xor
                T[4, ((K[13] shl  1) and $1fe)+1] xor
                T[5, ((K[11] shr  7) and $1fe)+1] xor
                T[6, ((K[ 9] shr 15) and $1fe)+1] xor
                T[7, ((K[ 7] shr 23) and $1fe)+1];
    L[3*2+0] := T[0, ((K[ 6] shl  1) and $1fe)] xor
                T[1, ((K[ 4] shr  7) and $1fe)] xor
                T[2, ((K[ 2] shr 15) and $1fe)] xor
                T[3, ((K[ 0] shr 23) and $1fe)] xor
                T[4, ((K[15] shl  1) and $1fe)] xor
                T[5, ((K[13] shr  7) and $1fe)] xor
                T[6, ((K[11] shr 15) and $1fe)] xor
                T[7, ((K[ 9] shr 23) and $1fe)];
    L[3*2+1] := T[0, ((K[ 6] shl  1) and $1fe)+1] xor
                T[1, ((K[ 4] shr  7) and $1fe)+1] xor
                T[2, ((K[ 2] shr 15) and $1fe)+1] xor
                T[3, ((K[ 0] shr 23) and $1fe)+1] xor
                T[4, ((K[15] shl  1) and $1fe)+1] xor
                T[5, ((K[13] shr  7) and $1fe)+1] xor
                T[6, ((K[11] shr 15) and $1fe)+1] xor
                T[7, ((K[ 9] shr 23) and $1fe)+1];
    L[4*2+0] := T[0, ((K[ 8] shl  1) and $1fe)] xor
                T[1, ((K[ 6] shr  7) and $1fe)] xor
                T[2, ((K[ 4] shr 15) and $1fe)] xor
                T[3, ((K[ 2] shr 23) and $1fe)] xor
                T[4, ((K[ 1] shl  1) and $1fe)] xor
                T[5, ((K[15] shr  7) and $1fe)] xor
                T[6, ((K[13] shr 15) and $1fe)] xor
                T[7, ((K[11] shr 23) and $1fe)];
    L[4*2+1] := T[0, ((K[ 8] shl  1) and $1fe)+1] xor
                T[1, ((K[ 6] shr  7) and $1fe)+1] xor
                T[2, ((K[ 4] shr 15) and $1fe)+1] xor
                T[3, ((K[ 2] shr 23) and $1fe)+1] xor
                T[4, ((K[ 1] shl  1) and $1fe)+1] xor
                T[5, ((K[15] shr  7) and $1fe)+1] xor
                T[6, ((K[13] shr 15) and $1fe)+1] xor
                T[7, ((K[11] shr 23) and $1fe)+1];
    L[5*2+0] := T[0, ((K[10] shl  1) and $1fe)] xor
                T[1, ((K[ 8] shr  7) and $1fe)] xor
                T[2, ((K[ 6] shr 15) and $1fe)] xor
                T[3, ((K[ 4] shr 23) and $1fe)] xor
                T[4, ((K[ 3] shl  1) and $1fe)] xor
                T[5, ((K[ 1] shr  7) and $1fe)] xor
                T[6, ((K[15] shr 15) and $1fe)] xor
                T[7, ((K[13] shr 23) and $1fe)];
    L[5*2+1] := T[0, ((K[10] shl  1) and $1fe)+1] xor
                T[1, ((K[ 8] shr  7) and $1fe)+1] xor
                T[2, ((K[ 6] shr 15) and $1fe)+1] xor
                T[3, ((K[ 4] shr 23) and $1fe)+1] xor
                T[4, ((K[ 3] shl  1) and $1fe)+1] xor
                T[5, ((K[ 1] shr  7) and $1fe)+1] xor
                T[6, ((K[15] shr 15) and $1fe)+1] xor
                T[7, ((K[13] shr 23) and $1fe)+1];
    L[6*2+0] := T[0, ((K[12] shl  1) and $1fe)] xor
                T[1, ((K[10] shr  7) and $1fe)] xor
                T[2, ((K[ 8] shr 15) and $1fe)] xor
                T[3, ((K[ 6] shr 23) and $1fe)] xor
                T[4, ((K[ 5] shl  1) and $1fe)] xor
                T[5, ((K[ 3] shr  7) and $1fe)] xor
                T[6, ((K[ 1] shr 15) and $1fe)] xor
                T[7, ((K[15] shr 23) and $1fe)];
    L[6*2+1] := T[0, ((K[12] shl  1) and $1fe)+1] xor
                T[1, ((K[10] shr  7) and $1fe)+1] xor
                T[2, ((K[ 8] shr 15) and $1fe)+1] xor
                T[3, ((K[ 6] shr 23) and $1fe)+1] xor
                T[4, ((K[ 5] shl  1) and $1fe)+1] xor
                T[5, ((K[ 3] shr  7) and $1fe)+1] xor
                T[6, ((K[ 1] shr 15) and $1fe)+1] xor
                T[7, ((K[15] shr 23) and $1fe)+1];
    L[7*2+0] := T[0, ((K[14] shl  1) and $1fe)] xor
                T[1, ((K[12] shr  7) and $1fe)] xor
                T[2, ((K[10] shr 15) and $1fe)] xor
                T[3, ((K[ 8] shr 23) and $1fe)] xor
                T[4, ((K[ 7] shl  1) and $1fe)] xor
                T[5, ((K[ 5] shr  7) and $1fe)] xor
                T[6, ((K[ 3] shr 15) and $1fe)] xor
                T[7, ((K[ 1] shr 23) and $1fe)];
    L[7*2+1] := T[0, ((K[14] shl  1) and $1fe)+1] xor
                T[1, ((K[12] shr  7) and $1fe)+1] xor
                T[2, ((K[10] shr 15) and $1fe)+1] xor
                T[3, ((K[ 8] shr 23) and $1fe)+1] xor
                T[4, ((K[ 7] shl  1) and $1fe)+1] xor
                T[5, ((K[ 5] shr  7) and $1fe)+1] xor
                T[6, ((K[ 3] shr 15) and $1fe)+1] xor
                T[7, ((K[ 1] shr 23) and $1fe)+1];
  end;

var
  S,L,K: TWhirlData;
  I: Integer;
begin
  Assert(not Odd(Whirlpool_Rounds));
  
  Move(FDigest, K, SizeOf(FDigest));
  XORBuffers(FDigest, Buffer[0], SizeOf(FDigest), S);
  // iterate over all rounds
  for I := 0 to Whirlpool_Rounds div 2 - 1 do
  begin
    Whirl(L, K, FTableC);
    L[0] := L[0] xor PLongArray(FTableR)[I*4+0];
    L[1] := L[1] xor PLongArray(FTableR)[I*4+1];
    Whirl(K, S, FTableC);
    XORBuffers(L, K, SizeOf(S), S);

    Whirl(K, L, FTableC);
    K[0] := K[0] xor PLongArray(FTableR)[I*4+2];
    K[1] := K[1] xor PLongArray(FTableR)[I*4+3];
    Whirl(L, S, FTableC);
    XORBuffers(K, L, SizeOf(S), S);
  end;
  XORBuffers(S, Buffer[0], SizeOf(FDigest), S);
  XORBuffers(S, FDigest, SizeOf(FDigest), FDigest);
end;
{$ENDIF}

// .THash_Whirlpool
procedure THash_Whirlpool.DoInit;
begin
  FillChar(FDigest, SizeOf(FDigest), 0);
  FTableC := @Whirlpool_C_U;
  FTableR := @Whirlpool_RC_U
end;

// .THash_Whirlpool1
procedure THash_Whirlpool1.DoInit;
begin
  FillChar(FDigest, SizeOf(FDigest), 0);
  FTableC := @Whirlpool_C_T;
  FTableR := @Whirlpool_RC_T;
end;

// .THash_Square
class function THash_Square.DigestSize: Integer;
begin
  Result := 16;
end;

class function THash_Square.BlockSize: Integer;
begin
  Result := 16;
end;

function THash_Square.Digest: PByteArray;
begin
  Result := @FDigest;
end;

procedure THash_Square.DoInit;
begin
  FillChar(FDigest, SizeOf(FDigest), 0);
end;

procedure THash_Square.DoDone;
var
  I: Integer;
begin
  if FPaddingByte = 0 then FPaddingByte := $80;
  FBuffer[FBufferIndex] := FPaddingByte;
  Inc(FBufferIndex);
  if FBufferIndex > FBufferSize - 8 then
  begin
    FillChar(FBuffer[FBufferIndex], FBufferSize - FBufferIndex, 0);
    DoTransform(Pointer(FBuffer));
    FBufferIndex := 0;
  end;
  FillChar(FBuffer[FBufferIndex], FBufferSize - FBufferIndex, 0);
  for I := 7 downto 0 do
    FBuffer[15 - I] := PByteArray(@FCount[0])[I];
  DoTransform(Pointer(FBuffer));
end;

{$IFNDEF THash_Square_asm}
procedure THash_Square.DoTransform(Buffer: PLongArray);
var
  Key: array[0..8, 0..3] of LongWord;
  A,B,C,D: LongWord;
  AA,BB,CC,DD: LongWord;
  I: Integer;
begin
{Build and expand the Key, Digest include the Key}
  Key[0, 0] := FDigest[0];
  Key[0, 1] := FDigest[1];
  Key[0, 2] := FDigest[2];
  Key[0, 3] := FDigest[3];
  for I := 1 to 8 do
  begin
    Key[I, 0] := Key[I -1, 0] xor Key[I -1, 3] shr 8 xor Key[I -1, 3] shl 24 xor 1 shl (I - 1);
    Key[I, 1] := Key[I -1, 1] xor Key[I, 0];
    Key[I, 2] := Key[I -1, 2] xor Key[I, 1];
    Key[I, 3] := Key[I -1, 3] xor Key[I, 2];

    Key[I -1, 0] := Square_PHIr[0, Key[I -1, 0]        and $FF] xor
                    Square_PHIr[1, Key[I -1, 0] shr  8 and $FF] xor
                    Square_PHIr[2, Key[I -1, 0] shr 16 and $FF] xor
                    Square_PHIr[3, Key[I -1, 0] shr 24        ];
    Key[I -1, 1] := Square_PHIr[0, Key[I -1, 1]        and $FF] xor
                    Square_PHIr[1, Key[I -1, 1] shr  8 and $FF] xor
                    Square_PHIr[2, Key[I -1, 1] shr 16 and $FF] xor
                    Square_PHIr[3, Key[I -1, 1] shr 24        ];
    Key[I -1, 2] := Square_PHIr[0, Key[I -1, 2]        and $FF] xor
                    Square_PHIr[1, Key[I -1, 2] shr  8 and $FF] xor
                    Square_PHIr[2, Key[I -1, 2] shr 16 and $FF] xor
                    Square_PHIr[3, Key[I -1, 2] shr 24        ];
    Key[I -1, 3] := Square_PHIr[0, Key[I -1, 3]        and $FF] xor
                    Square_PHIr[1, Key[I -1, 3] shr  8 and $FF] xor
                    Square_PHIr[2, Key[I -1, 3] shr 16 and $FF] xor
                    Square_PHIr[3, Key[I -1, 3] shr 24        ];
  end;
{Encrypt begin here, same TCipher_Square.Encode}
  A := Buffer[0] xor Key[0, 0];
  B := Buffer[1] xor Key[0, 1];
  C := Buffer[2] xor Key[0, 2];
  D := Buffer[3] xor Key[0, 3];

  for I := 0 to 6 do
  begin
    AA := Square_TE[0, A        and $FF] xor
          Square_TE[1, B        and $FF] xor
          Square_TE[2, C        and $FF] xor
          Square_TE[3, D        and $FF] xor Key[I + 1, 0];
    BB := Square_TE[0, A shr  8 and $FF] xor
          Square_TE[1, B shr  8 and $FF] xor
          Square_TE[2, C shr  8 and $FF] xor
          Square_TE[3, D shr  8 and $FF] xor Key[I + 1, 1];
    CC := Square_TE[0, A shr 16 and $FF] xor
          Square_TE[1, B shr 16 and $FF] xor
          Square_TE[2, C shr 16 and $FF] xor
          Square_TE[3, D shr 16 and $FF] xor Key[I + 1, 2];
    DD := Square_TE[0, A shr 24        ] xor
          Square_TE[1, B shr 24        ] xor
          Square_TE[2, C shr 24        ] xor
          Square_TE[3, D shr 24        ] xor Key[I + 1, 3];

    A := AA; B := BB; C := CC; D := DD;
  end;

  FDigest[0] := Buffer[0] xor
                Square_SEint[A        and $FF]        xor
                Square_SEint[B        and $FF] shl  8 xor
                Square_SEint[C        and $FF] shl 16 xor
                Square_SEint[D        and $FF] shl 24 xor Key[8, 0];
  FDigest[1] := Buffer[1] xor
                Square_SEint[A shr  8 and $FF]        xor
                Square_SEint[B shr  8 and $FF] shl  8 xor
                Square_SEint[C shr  8 and $FF] shl 16 xor
                Square_SEint[D shr  8 and $FF] shl 24 xor Key[8, 1];
  FDigest[2] := Buffer[2] xor
                Square_SEint[A shr 16 and $FF]        xor
                Square_SEint[B shr 16 and $FF] shl  8 xor
                Square_SEint[C shr 16 and $FF] shl 16 xor
                Square_SEint[D shr 16 and $FF] shl 24 xor Key[8, 2];
  FDigest[3] := Buffer[3] xor
                Square_SEint[A shr 24        ]        xor
                Square_SEint[B shr 24        ] shl  8 xor
                Square_SEint[C shr 24        ] shl 16 xor
                Square_SEint[D shr 24        ] shl 24 xor Key[8, 3];
end;
{$ENDIF}

// .THashBaseSnefru
procedure THashBaseSnefru.SetSecurity_Level(Value: Integer);
begin
  if (Value < 2) or (Value > 8) then Value := 8;
  FSecurity_Level := Value;
end;

function THashBaseSnefru.Digest: PByteArray;
begin
  Result := @FDigest;
end;

procedure THashBaseSnefru.DoInit;
begin
  FillChar(FDigest, SizeOf(FDigest), 0);
  SetSecurity_Level(FSecurity_Level);
end;

procedure THashBaseSnefru.DoDone;
begin
  if FBufferIndex > 0 then
  begin
    FillChar(FBuffer[FBufferIndex], FBufferSize - FBufferIndex, 0);
    DoTransform(Pointer(FBuffer));
    FBufferIndex := 0;
  end;
  FillChar(FBuffer[FBufferIndex], FBufferSize - FBufferIndex, 0);
  PLongWord(@FBuffer[FBufferSize - 8])^ := SwapLong(FCount[1]);
  PLongWord(@FBuffer[FBufferSize - 4])^ := SwapLong(FCount[0]);
  DoTransform(Pointer(FBuffer));
  SwapLongBuffer(FDigest, FDigest, 8);
end;

// .THash_Snefru128
class function THash_Snefru128.DigestSize: Integer;
begin
  Result := 16;
end;

class function THash_Snefru128.BlockSize: Integer;
begin
  Result := 48
end;

{$IFNDEF THash_Snefru128_asm}
procedure THash_Snefru128.DoTransform(Buffer: PLongArray);
const
  ShiftTable: array[0..3] of Integer = (16, 8, 16, 24);
var
  I,Index,ByteInWord,T,N,S,S0,S1: LongWord;
  D,Box0,Box1: PLongArray;
begin
  D := @FDigest;
  SwapLongBuffer(Buffer[0], D[4], 12);
  Move(D[0], D[16], 16);
  Box0 := @Snefru_Data[0];
  Box1 := @Snefru_Data[1];
  for Index := 0 to FSecurity_Level-1 do
  begin
    for ByteInWord := 0 to 3 do
    begin
      I := 0;
      N := D[0];
      while I < 16 do
      begin
        S0 := Box0[N and $FF];
        T := (I +  1) and 15;    N := D[T] xor S0; D[T] := N;
        T := (I + 15) and 15; D[T] := D[T] xor S0;
        S1 := Box0[N and $FF];
        T := (I +  2) and 15;    N := D[T] xor S1; D[T] := N;
        T := (I + 16) and 15; D[T] := D[T] xor S1;
        S0 := Box1[N and $FF];
        T := (I +  3) and 15;    N := D[T] xor S0; D[T] := N;
        T := (I + 17) and 15; D[T] := D[T] xor S0;
        S1 := Box1[N and $FF];
        T := (I +  4) and 15;    N := D[T] xor S1; D[T] := N;
        T := (I + 18) and 15; D[T] := D[T] xor S1;
        Inc(I, 4);
      end;
      T := ShiftTable[ByteInWord];
      S := 32 - T;
      for I := 0 to 15 do D[I] := D[I] shr T or D[I] shl S;
    end;
    Box0 := @Box0[512];
    Box1 := @Box1[512];
  end;
  for I := 0 to 3 do D[I] := D[I + 16] xor D[15 - I];
end;
{$ENDIF}

// .THash_Snefru256
class function THash_Snefru256.DigestSize: Integer;
begin
  Result := 32;
end;

class function THash_Snefru256.BlockSize: Integer;
begin
  Result := 32
end;

{$IFNDEF THash_Snefru256_asm}
procedure THash_Snefru256.DoTransform(Buffer: PLongArray);
const
  ShiftTable: array[0..3] of Integer = (16, 8, 16, 24);
var
  I,Index,ByteInWord,T,N,S,S0,S1: LongWord;
  D,Box0,Box1: PLongArray;
begin
  D := @FDigest;
  SwapLongBuffer(Buffer[0], D[8], 8);
  Move(D[0], D[16], 32);
  Box0 := @Snefru_Data[0];
  Box1 := @Snefru_Data[1];
  for Index := 0 to FSecurity_Level-1 do
  begin
    for ByteInWord := 0 to 3 do
    begin
      I := 0;
      N := D[0];
      while I < 16 do
      begin
        S0 := Box0[N and $FF];
        T := (I +  1) and 15;    N := D[T] xor S0; D[T] := N;
        T := (I + 15) and 15; D[T] := D[T] xor S0;
        S1 := Box0[N and $FF];
        T := (I +  2) and 15;    N := D[T] xor S1; D[T] := N;
        T := (I + 16) and 15; D[T] := D[T] xor S1;
        S0 := Box1[N and $FF];
        T := (I +  3) and 15;    N := D[T] xor S0; D[T] := N;
        T := (I + 17) and 15; D[T] := D[T] xor S0;
        S1 := Box1[N and $FF];
        T := (I +  4) and 15;    N := D[T] xor S1; D[T] := N;
        T := (I + 18) and 15; D[T] := D[T] xor S1;
        Inc(I, 4);
      end;
      T := ShiftTable[ByteInWord];
      S := 32 - T;
      for I := 0 to 15 do D[I] := D[I] shr T or D[I] shl S;
    end;
    Box0 := @Box0[512];
    Box1 := @Box1[512];
  end;
  for I := 0 to 7 do D[I] := D[I + 16] xor D[15 - I];
end;
{$ENDIF}

// .THashBaseSapphire
class function THash_Sapphire.BlockSize: Integer;
begin
  Result := 1;
end;

class function THash_Sapphire.DigestSize: Integer;
begin
  Result := 64;
end;

function THash_Sapphire.Digest: PByteArray;
begin
  Result := @FDigest;
end;

function THash_Sapphire.DigestStr(Format: TDECFormatClass = nil): Binary;
var
  Size: Integer;
begin
  if FDigestSize > 0 then Size := FDigestSize else Size := DigestSize;
  Result := ValidFormat(Format).Encode(FDigest, Size);
end;

procedure THash_Sapphire.DoInit;
var
  I: Integer;
begin
  FillChar(FDigest, SizeOf(FDigest), 0);
  FRotor := 1;
  FRatchet := 3;
  FAvalanche := 5;
  FPlain := 7;
  FCipher := 11;
  for I := 0 to 255 do FCards[I] := 255 - I;
end;

procedure THash_Sapphire.DoDone;
var
  I: Integer;
begin
  for I := 255 downto 0 do Calc(I, 1);
  for I := 0 to DigestSize -1 do
  begin
    Calc(#0#0, 1);
    PByteArray(@FDigest)[I] := FCipher;
  end;
end;

{$IFNDEF THash_Sapphire_asm}
procedure THash_Sapphire.Calc(const Data; DataSize: Integer);
var
  Cipher,Ratchet,Rotor,Plain,Avalanche,T: LongWord;
  D: PByte;
begin
  D         := @Data;
  Cipher    := FCipher;
  Ratchet   := FRatchet;
  Rotor     := FRotor;
  Plain     := FPlain;
  Avalanche := FAvalanche;
  while DataSize > 0 do
  begin
    Dec(DataSize);
    Ratchet := (Ratchet + FCards[Rotor]) and $FF;
    Rotor := (Rotor + 1) and $FF;
    T := FCards[Cipher];
    FCards[Cipher] := FCards[Ratchet];
    FCards[Ratchet] := FCards[Plain];
    FCards[Plain] := FCards[Rotor];
    FCards[Rotor] := T;
    Avalanche := (Avalanche + FCards[T]) and $FF;
    T := (FCards[Plain] + FCards[Cipher] + FCards[Avalanche]) and $FF;
    Plain := D^; Inc(D);
    Cipher := Plain xor FCards[FCards[T]] xor FCards[(FCards[Ratchet] + FCards[Rotor]) and $FF];
  end;
  FCipher    := Cipher;
  FRatchet   := Ratchet;
  FRotor     := Rotor;
  FPlain     := Plain;
  FAvalanche := Avalanche;
end;
{$ENDIF}

end.
