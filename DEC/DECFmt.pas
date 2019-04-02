{*****************************************************************************

  Delphi Encryption Compendium (DEC Part I)
  Version 5.2, Part I, for Delphi 7 - 2009

  Remarks:          Freeware, Copyright must be included

  Original Author:  (c) 2006 Hagen Reddmann, HaReddmann [at] T-Online [dot] de
  Modifications:    (c) 2008 Arvid Winkelsdorf, info [at] digivendo [dot] de

  Last change:      02. November 2008

  Description:      Format conversion utilities for DEC

  Remarks:          adds about 10Kb code if all TFormats are used,
                    designed to made universal code (not for speed),

  Unicode Remarks:  format conversions support ANSI input due to the given
                    RFC Specs, to preserve unicode use Delphi's UTF8Encode and
                    UTF8Decode before conversion

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

unit DECFmt;

interface

uses Windows, SysUtils, Classes, DECUtil;

{$I VER.INC}

type
  TDECFormat      = class;

  TFormat_Copy    = class;    // copy input to output, it's the default format, e.g. FormatClass = nil
  TFormat_HEX     = class;    // HEXadecimal in UpperCase
  TFormat_HEXL    = class;    // HEXadecimal in Lowercase
  TFormat_MIME32  = class;    // MIME like format for Base 32
  TFormat_MIME64  = class;    // MIME Base 64 format
  TFormat_PGP     = class;    // PGP's MIME Base 64 with PGP's Checksums
  TFormat_UU      = class;    // Unix UU Base 64
  TFormat_XX      = class;    // Unix XX base 64
  TFormat_ESCAPE  = class;    // Escaped Strings

  TDECFormatClass = class of TDECFormat;

  TDECFormat = class(TDECObject) // for binary one to one convert = fmtCOPY
  protected
    class function DoEncode(const Value; Size: Integer): Binary; virtual; abstract;
    class function DoDecode(const Value; Size: Integer): Binary; virtual; abstract;
    class function DoIsValid(const Value; Size: Integer): Boolean; virtual; abstract;
  public
    class function Encode(const Value: Binary): Binary; overload;
    class function Encode(const Value; Size: Integer): Binary; overload;
    class function Decode(const Value: Binary): Binary; overload;
    class function Decode(const Value; Size: Integer): Binary; overload;
    class function IsValid(const Value: Binary): Boolean; overload;
    class function IsValid(const Value; Size: Integer): Boolean; overload;
  end;

  TFormat_Copy = class(TDECFormat)
  protected
    class function DoEncode(const Value; Size: Integer): Binary; override;
    class function DoDecode(const Value; Size: Integer): Binary; override;
    class function DoIsValid(const Value; Size: Integer): Boolean; override;
  end;

  TFormat_HEX = class(TDECFormat) // Hexadecimal = fmtHEX
  protected
    class function DoEncode(const Value; Size: Integer): Binary; override;
    class function DoDecode(const Value; Size: Integer): Binary; override;
    class function DoIsValid(const Value; Size: Integer): Boolean; override;
  public
    class function CharTable: PAnsiChar; virtual;
  end;

  TFormat_HEXL = class(TFormat_HEX) // Hexadecimal lowercase = fmtHEXL
  public
    class function CharTable: PAnsiChar; override;
  end;

  TFormat_MIME32 = class(TFormat_HEX)  // MIME Base 32 = fmtMIME32
  protected
    class function DoEncode(const Value; Size: Integer): Binary; override;
    class function DoDecode(const Value; Size: Integer): Binary; override;
  public
    class function CharTable: PAnsiChar; override;
  end;

  TFormat_MIME64 = class(TFormat_HEX)  // MIME Base 64 = fmtMIME64
  protected
    class function DoEncode(const Value; Size: Integer): Binary; override;
    class function DoDecode(const Value; Size: Integer): Binary; override;
  public
    class function CharTable: PAnsiChar; override;
  end;

  TFormat_PGP = class(TFormat_MIME64)
  protected
    class function DoExtractCRC(const Value; var Size: Integer): LongWord;
    class function DoEncode(const Value; Size: Integer): Binary; override;
    class function DoDecode(const Value; Size: Integer): Binary; override;
  end;

  TFormat_UU = class(TDECFormat) // UU Encode = fmtUU
  protected
    class function DoEncode(const Value; Size: Integer): Binary; override;
    class function DoDecode(const Value; Size: Integer): Binary; override;
    class function DoIsValid(const Value; Size: Integer): Boolean; override;
  public
    class function CharTable: PAnsiChar; virtual;
  end;

  TFormat_XX = class(TFormat_UU) // XX Encode = fmtXX
  public
    class function CharTable: PAnsiChar; override;
  end;

  TFormat_ESCAPE = class(TDECFormat)
  protected
    class function DoEncode(const Value; Size: Integer): Binary; override;
    class function DoDecode(const Value; Size: Integer): Binary; override;
  end;

function ValidFormat(FormatClass: TDECFormatClass = nil): TDECFormatClass;
function FormatByName(const Name: String): TDECFormatClass;
function FormatByIdentity(Identity: LongWord): TDECFormatClass;
// insert #13#10 Chars in Blocks from BlockSize
function InsertCR(const Value: AnsiString; BlockSize: Integer): AnsiString;
// delete all #13 and #10 Chars
function DeleteCR(const Value: AnsiString): AnsiString;
// format any String to a Block
function InsertBlocks(const Value, BlockStart, BlockEnd: AnsiString; BlockSize: Integer): AnsiString;
// remove any Block format
function RemoveBlocks(const Value, BlockStart, BlockEnd: AnsiString): AnsiString;

var
  PGPCharsPerLine: Integer = 80;

implementation

uses CRC;

resourcestring
  sStringFormatExists  = 'String format "%d" does not exist.';
  sInvalidStringFormat = 'Input is not an valid %s format.';
  sInvalidFormatString = 'Input can not be converted to %s format.';
  sFormatNotRegistered = 'String format is not registered.';

function ValidFormat(FormatClass: TDECFormatClass = nil): TDECFormatClass;
begin
  if FormatClass <> nil then Result := FormatClass
    else Result := TFormat_Copy;
end;

function FormatByName(const Name: String): TDECFormatClass;
begin
  Result := TDECFormatClass(DECClassByName(Name, TDECFormat));
end;

function FormatByIdentity(Identity: LongWord): TDECFormatClass;
begin
  Result := TDECFormatClass(DECClassByIdentity(Identity, TDECFormat));
end;

class function TDECFormat.Encode(const Value: Binary): Binary;
begin
  Result := DoEncode(Value[1], Length(Value));
end;

class function TDECFormat.Encode(const Value; Size: Integer): Binary;
begin
  Result := DoEncode(Value, Size);
end;

class function TDECFormat.Decode(const Value: Binary): Binary;
begin
  Result := DoDecode(Value[1], Length(Value));
end;

class function TDECFormat.Decode(const Value; Size: Integer): Binary;
begin
  Result := DoDecode(Value, Size);
end;

class function TDECFormat.IsValid(const Value: Binary): Boolean;
begin
  Result := DoIsValid(Value[1], Length(Value));
end;

class function TDECFormat.IsValid(const Value; Size: Integer): Boolean;
begin
  Result := DoIsValid(Value, Size);
end;

// TFormat_Copy
class function TFormat_Copy.DoEncode(const Value; Size: Integer): Binary;
begin
  SetLength(Result, Size);
  Move(Value, Result[1], Size);
end;

class function TFormat_Copy.DoDecode(const Value; Size: Integer): Binary;
begin
  SetLength(Result, Size);
  Move(Value, Result[1], Size);
end;

class function TFormat_Copy.DoIsValid(const Value; Size: Integer): Boolean;
begin
  Result := Size >= 0;
end;

function TableFind(Value: AnsiChar; Table: PAnsiChar; Len: Integer): Integer; assembler;
asm // Utility for TStringFormat_XXXXX
      PUSH  EDI
      MOV   EDI,EDX
      REPNE SCASB
      MOV   EAX,0
      JNE   @@1
      MOV   EAX,EDI
      SUB   EAX,EDX
@@1:  DEC   EAX
      POP   EDI
end;

class function TFormat_HEX.DoEncode(const Value; Size: Integer): Binary;
var
  S: PByte;
  D,T: PAnsiChar;
begin
  Result := '';
  if Size <= 0 then Exit;
  SetLength(Result, Size * 2);
  T := CharTable;
  D := PAnsiChar(Result);
  S := PByte(@Value);
  while Size > 0 do
  begin
    D[0] := T[S^ shr  4];
    D[1] := T[S^ and $F];
    Inc(D, 2);
    Inc(S);
    Dec(Size);
  end;
end;

class function TFormat_HEX.DoDecode(const Value; Size: Integer): Binary;
var
  S: PAnsiChar;
  D: PByte;
  T: PAnsiChar;
  I,P: Integer;
  HasIdent: Boolean;
begin
  Result := '';
  if Size <= 0 then Exit;
  SetLength(Result, Size div 2 +1);
  T := CharTable;
  D := PByte(Result);
  S := PAnsiChar(@Value);
  I := 0;
  HasIdent := False;
  while Size > 0 do
  begin
    P := TableFind(S^, T, 18);
    if P < 0 then P := TableFind(UpCase(S^), T, MaxInt);
    if P < 0 then
      raise EDECException.CreateFmt(sInvalidStringFormat, [DECClassname(Self)]);
    Inc(S);
    if P >= 0 then
      if P > 16 then
      begin
        if not HasIdent and (P < 18) then
        begin
          HasIdent := True;
          I := 0;
          D := PByte(Result);
        end;
      end else
      begin
        if Odd(I) then
        begin
          D^ := D^ or P;
          Inc(D);
        end else D^ := P shl 4;
        Inc(I);
      end;
    Dec(Size);
  end;
  SetLength(Result, PAnsiChar(D) - PAnsiChar(Result));
end;

class function TFormat_HEX.DoIsValid(const Value; Size: Integer): Boolean;
var
  S,T: PAnsiChar;
  L: Integer;
begin
  Result := True;
  T := CharTable;
  L := StrLen(T);
  S := PAnsiChar(@Value);
  while Result and (Size > 0) do
    if TableFind(S^, T, L) >= 0 then
    begin
      Dec(Size);
      Inc(S);
    end else Result := False;
end;

class function TFormat_HEX.CharTable: PAnsiChar; assembler;
asm
      MOV  EAX,OFFSET @@1
      RET
@@1:  DB   '0123456789ABCDEF'     // Table must be >= 18 Chars
      DB   'X$ abcdefhHx()[]{},;:-_/\*+"''',9,10,13,0
end;

class function TFormat_HEXL.CharTable: PAnsiChar;
asm
      MOV  EAX,OFFSET @@1
      RET
@@1:  DB   '0123456789abcdef'     // Table must be >= 18 Chars
      DB   'X$ ABCDEFhHx()[]{},;:-_/\*+"''',9,10,13,0
end;

class function TFormat_MIME32.DoEncode(const Value; Size: Integer): Binary;
var
  S: PByteArray;
  D,T: PAnsiChar;
  I: Integer;
begin
  Result := '';
  if Size <= 0 then Exit;
  Size := Size * 8;
  SetLength(Result, Size div 5 + 5);
  D := PAnsiChar(Result);
  T := CharTable;
  S := PByteArray(@Value);
  I := 0;
  while I < Size do
  begin
    D^ := T[PWord(@S[I shr 3])^ shr (I and $7) and $1F];
    Inc(D);
    Inc(I, 5);
  end;
  SetLength(Result, D - PAnsiChar(Result));
end;

class function TFormat_MIME32.DoDecode(const Value; Size: Integer): Binary;
var
  S,T,D: PAnsiChar;
  I,V: Integer;
begin
  Result := '';
  if Size <= 0 then Exit;
  T := CharTable;
  SetLength(Result, Size * 5 div 8);
  D := PAnsiChar(Result);
  FillChar(D^, Length(Result), 0);
  S := PAnsiChar(@Value);
  Size := Size * 5;
  I := 0;
  while I < Size do
  begin
    V := TableFind(S^, T, 32);
    if V < 0 then V := TableFind(UpCase(S^), T, 32);
    if V >= 0 then
    begin
      PWord(@D[I shr 3])^ := PWord(@D[I shr 3])^ or (V shl (I and $7));
      Inc(I, 5);
    end else Dec(Size, 5);
    Inc(S);
  end;
  SetLength(Result, Size div 8);
end;

class function TFormat_MIME32.CharTable: PAnsiChar;
asm
      MOV  EAX,OFFSET @@1
      RET  // must be >= 32 Chars
@@1:  DB  'abcdefghijklnpqrstuwxyz123456789'
      DB  ' =$()[]{},;:-_\*"''',9,10,13,0  // special and skipped chars
end;

class function TFormat_MIME64.DoEncode(const Value; Size: Integer): Binary;
var
  B: Cardinal;
  I: Integer;
  D,T: PAnsiChar;
  S: PByteArray;
begin
  Result := '';
  if Size <= 0 then Exit;
  SetLength(Result, Size * 4 div 3 + 4);
  D := PAnsiChar(Result);
  T := CharTable;
  S := PByteArray(@Value);
  while Size >= 3 do
  begin
    Dec(Size, 3);
    B := S[0] shl 16 or S[1] shl 8 or S[2];
    D[0] := T[B shr 18 and $3F];
    D[1] := T[B shr 12 and $3F];
    D[2] := T[B shr  6 and $3F];
    D[3] := T[B        and $3F];
    Inc(D, 4);
    S := @S[3];
  end;
  while Size > 0 do
  begin
    B := 0;
    for I := 0 to 2 do
    begin
      B := B shl 8;
      if Size > 0 then
      begin
        B := B or S[0];
        S := @S[1];
      end;
      Dec(Size);
    end;
    for I := 3 downto 0 do
    begin
      if Size < 0 then
      begin
        D[I] := T[64];
        Inc(Size);
      end else D[I] := T[B and $3F];
      B := B shr 6;
    end;
    Inc(D, 4);
  end;
  SetLength(Result, D - PAnsiChar(Result));
end;

class function TFormat_MIME64.DoDecode(const Value; Size: Integer): Binary;
var
  B: Cardinal;
  J,I: Integer;
  S,D,L,T: PAnsiChar;
begin
  Result := '';
  if Size <= 0 then Exit;
  SetLength(Result, Size);
  Move(Value, PAnsiChar(Result)^, Size);
  T := CharTable;
  D := PAnsiChar(Result);
  S := D;
  L := S + Size;
  J := 0;
  while S < L do
  begin
    B := 0;
    J := 4;
    while (J > 0) and (S < L) do
    begin
      I := TableFind(S^, T, 65);
      Inc(S);
      if I >= 0 then
        if I < 64 then
        begin
          B := B shl 6 or Byte(I);
          Dec(J);
        end else L := S;
    end;
    if J > 0 then
      if J >= 4 then
      begin
        J := 0;
        Break;
      end else B := B shl (6 * J);
    I := 2;
    while I >= 0 do
    begin
      D[I] := AnsiChar(B);
      B := B shr 8;
      Dec(I);
    end;
    Inc(D, 3);
  end;
  SetLength(Result, D - PAnsiChar(Result) - J);
end;

class function TFormat_MIME64.CharTable: PAnsiChar; assembler;
asm
      MOV  EAX,OFFSET @@1
      RET  // must be >= 65 Chars
@@1:  DB  'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/='
      DB  ' $()[]{},;:-_\*"''',9,10,13,0  // special and skipped chars
end;

class function TFormat_PGP.DoExtractCRC(const Value; var Size: Integer): LongWord;
var
  L: PAnsiChar;
  C: ANsiChar;
  R: AnsiString;
begin
  Result := $FFFFFFFF;
  C := CharTable[64];                       // get padding char, per default '='
  L := PAnsiChar(@Value) + Size;
  while L <> PAnsiChar(@Value) do
    if L^ = C then Break else Dec(L);       // scan reverse for padding char
  if L - PAnsiChar(@Value) >= Size - 5 then // remaining chars must be > 4 ,i.e. '=XQRT'
  try
    Inc(L);
    R := inherited DoDecode(L^, Size - (L - PAnsiChar(@Value)));
    if Length(R) >= 3 then
    begin
      Result := 0;
      Move(PAnsiChar(R)^, Result, 3);
      Size := L - PAnsiChar(@Value);
    end;
  except
  end;
end;

class function TFormat_PGP.DoEncode(const Value; Size: Integer): Binary;
var
  CRC: LongWord;
begin
  Result := '';
  if Size <= 0 then Exit;
  Result := InsertCR(inherited DoEncode(Value, Size), PGPCharsPerLine); // 80 chars per line
  CRC := CRCCalc(CRC_24, Value, Size);                               // calculate 24Bit Checksum
  SwapBytes(CRC, 3);                                                 // PGP use Big Endian
  if Result[Length(Result)] <> #10 then Result := Result + #13#10;   // insert CR iff needed, CRC must be in next line
  Result := Result + '=' + inherited DoEncode(CRC, 3);                 // append CRC
end;

class function TFormat_PGP.DoDecode(const Value; Size: Integer): Binary;
var
  CRC: LongWord;
begin
  Result := '';
  if Size <= 0 then Exit;
  CRC := DoExtractCRC(Value, Size);
  Result := inherited DoDecode(Value, Size);
  if CRC <> $FFFFFFFF then // iff CRC found check it
  begin
    SwapBytes(CRC, 3);
    if CRC <> CRCCalc(CRC_24, PAnsiChar(Result)^, Length(Result)) then
      raise EDECException.CreateFmt(sInvalidStringFormat, [DECClassname(Self)]);
  end;
end;

class function TFormat_UU.DoEncode(const Value; Size: Integer): Binary;
var
  S,T,D: PAnsiChar;
  L,I: Integer;
  B: Cardinal;
begin
  Result := '';
  if Size <= 0 then Exit;
  SetLength(Result, Size * 4 div 3 + Size div 45 + 10);
  D := PAnsiChar(Result);
  T := CharTable;
  S := PAnsiChar(@Value);
  while Size > 0 do
  begin
    L := Size;
    if L > 45 then L := 45;
    Dec(Size, L);
    D^ := T[L];
    while L > 0 do
    begin
      B := 0;
      for I := 0 to 2 do
      begin
        B := B shl 8;
        if L > 0 then
        begin
          B := B or Byte(S^);
          Inc(S);
        end;
        Dec(L);
      end;
      for I := 4 downto 1 do
      begin
        D[I] := T[B and $3F];
        B := B shr 6;
      end;
      Inc(D, 4);
    end;
    Inc(D);
  end;
  SetLength(Result, D - PAnsiChar(Result));
end;

class function TFormat_UU.DoDecode(const Value; Size: Integer): Binary;
var
  T,D,L,S: PAnsiChar;
  I,E: Integer;
  B: Cardinal;
begin
  Result := '';
  if Size <= 0 then Exit;
  SetLength(Result, Size);
  S := PAnsiChar(@Value);
  L := S + Size;
  D := PAnsiChar(Result);
  T := CharTable;
  repeat
    Size := TableFind(S^, T, 64);
    if (Size < 0) or (Size > 45) then
      raise EDECException.CreateFmt(sInvalidStringFormat, [DECClassName(Self)]);
    Inc(S);
    while Size > 0 do
    begin
      B := 0;
      I := 4;
      while (I > 0) and (S <= L) do
      begin
        E := TableFind(S^, T, 64);
        if E >= 0 then
        begin
          B := B shl 6 or Byte(E);
          Dec(I);
        end;
        Inc(S);
      end;
      I := 2;
      repeat
        D[I] := AnsiChar(B);
        B    := B shr 8;
        Dec(I);
      until I < 0;
      if Size > 3 then Inc(D, 3) else Inc(D, Size);
      Dec(Size, 3);
    end;
  until S >= L;
  SetLength(Result, D - PAnsiChar(Result));
end;

class function TFormat_UU.DoIsValid(const Value; Size: Integer): Boolean;
var
  S,T: PAnsiChar;
  L,I,P: Integer;
begin
  Result := False;
  T := CharTable;
  L := StrLen(T);
  S := PAnsiChar(@Value);
  P := 0;
  while Size > 0 do
  begin
    I := TableFind(S^, T, L);
    if I >= 0 then
    begin
      Dec(Size);
      Inc(S);
      if P = 0 then
      begin
        if I > 45 then Exit;
        P := (I * 4 + 2) div 3;
      end else
        if I < 64 then Dec(P);
    end else Exit;
  end;
  if P <> 0 then Exit;
  Result := True;
end;

class function TFormat_UU.CharTable: PAnsiChar;
asm
      MOV  EAX,OFFSET @@1
      RET  // must be >= 64 Chars
@@1:  DB   '`!"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_'
      DB   ' ',9,10,13,0
end;

class function TFormat_XX.CharTable: PAnsiChar;
asm
      MOV  EAX,OFFSET @@1
      RET
@@1:  DB   '+-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
      DB   ' "()[]''',9,10,13,0
end;

const
  ESCAPE_CodesL: PAnsiChar = 'abtnvfr';
  ESCAPE_CodesU: PAnsiChar = 'ABTNVFR';

class function TFormat_ESCAPE.DoDecode(const Value; Size: Integer): Binary;
var
  D,S,T: PAnsiChar;
  I: Integer;
begin
  Result := '';
  if Size <= 0 then Exit;
  SetLength(Result, Size);
  D := PAnsiChar(Result);
  S := PAnsiChar(@Value);
  T := S + Size;
  while S < T do
  begin
    if S^ = '\' then
    begin
      Inc(S);
      if S > T then Break;
      if UpCase(S^) = 'X' then
      begin
        if S + 2 > T then
          raise EDECException.CreateFmt(sInvalidStringFormat, [DECClassName(Self)]);
        I := TableFind(UpCase(S[1]), TFormat_HEX.CharTable, 16);
        if I < 0 then
          raise EDECException.CreateFmt(sInvalidStringFormat, [DECClassName(Self)]);
        D^ := AnsiChar(I shl 4);
        I := TableFind(UpCase(S[2]), TFormat_HEX.CharTable, 16);
        if I < 0 then
          raise EDECException.CreateFmt(sInvalidStringFormat, [DECClassName(Self)]);
        D^ := AnsiChar(Byte(D^) or I);
        Inc(S, 2);
      end else
      begin
        I := TableFind(UpCase(S^), ESCAPE_CodesU, 7);
        if I >= 0 then D^ := AnsiChar(I + 7)
          else D^ := S^;
      end;
    end else D^ := S^;
    Inc(D);
    Inc(S);
  end;
  SetLength(Result, D - PAnsiChar(Result));
end;

class function TFormat_ESCAPE.DoEncode(const Value; Size: Integer): Binary;
var
  S: PByte;
  D,T: PAnsiChar;
  I: Integer;
begin
  Result := '';
  if Size = 0 then Exit;
  SetLength(Result, Size + 8);
  I := Size;
  D := PAnsiChar(Result);
  S := PByte(@Value);
  T := TFormat_HEX.CharTable;
  while Size > 0 do
  begin
    if I <= 0 then
    begin
      I := D - PAnsiChar(Result);
      SetLength(Result, I + Size + 8);
      D := PAnsiChar(Result) + I;
      I := Size;
    end;
    if (S^ < 32) {or (S^ > $7F)} then
      if (S^ >= 7) and (S^ <= 13) then
      begin
        D[0] := '\';
        D[1] := ESCAPE_CodesL[S^ - 7];
        Inc(D, 2);
        Dec(I, 2);
      end else
      begin
        D[0] := '\';
        D[1] := 'x';
        D[2] := T[S^ shr 4];
        D[3] := T[S^ and $F];
        Inc(D, 4);
        Dec(I, 4);
      end
    else
      if S^ = Ord('\') then
      begin
        D[0] := '\';
        D[1] := '\';
        Inc(D, 2);
        Dec(I, 2);
      end else
        if S^ = Ord('"') then
        begin
          D[0] := '\';
          D[1] := '"';
          Inc(D, 2);
          Dec(I, 2);
        end else
        begin
          D^ := AnsiChar(S^);
          Inc(D);
          Dec(I);
        end;
    Dec(Size);
    Inc(S);
  end;
  SetLength(Result, D - PAnsiChar(Result));
end;

function InsertCR(const Value: AnsiString; BlockSize: Integer): AnsiString;
var
  I: Integer;
  S,D: PAnsiChar;
begin
  if (BlockSize <= 0) or (Length(Value) <= BlockSize) then
  begin
    Result := Value;
    Exit;
  end;
  I := Length(Value);
  SetLength(Result, I + I * 2 div BlockSize + 2);
  S := PAnsiChar(Value);
  D := PAnsiChar(Result);
  repeat
    Move(S^, D^, BlockSize);
    Inc(S, BlockSize);
    Inc(D, BlockSize);
    D^ := #13; Inc(D);
    D^ := #10; Inc(D);
    Dec(I, BlockSize);
  until I < BlockSize;
  Move(S^, D^, I);
  Inc(D, I);
  SetLength(Result, D - PAnsiChar(Result));
end;

function DeleteCR(const Value: AnsiString): AnsiString;
var
  S,D: PAnsiChar;
  I: Integer;
begin
  I := Length(Value);
  SetLength(Result, I);
  D := PAnsiChar(Result);
  S := PAnsiChar(Value);
  while I > 0 do
  begin
    if (S^ <> #10) and (S^ <> #13) then
    begin
      D^ := S^;
      Inc(D);
    end;
    Inc(S);
    Dec(I);
  end;
  SetLength(Result, D - PAnsiChar(Result));
end;

function InsertBlocks(const Value, BlockStart, BlockEnd: AnsiString; BlockSize: Integer): AnsiString;
var
  I,LS,LE: Integer;
  D,S: PAnsiChar;
begin
  if (BlockSize <= 0) or (Length(Value) <= BlockSize) then
  begin
    Result := Value;
    Exit;
  end;
  I := Length(Value);
  LS := Length(BlockStart);
  LE := Length(BlockEnd);
  SetLength(Result, I + (I div BlockSize + 1) * (LS + LE));
  S := PAnsiChar(Value);
  D := PAnsiChar(Result);
  repeat
    Move(PAnsiChar(BlockStart)^, D^, LS); Inc(D, LS);
    Move(S^, D^, BlockSize);          Inc(D, BlockSize);
    Move(PAnsiChar(BlockEnd)^, D^, LE);   Inc(D, LE);
    Dec(I, BlockSize);
    Inc(S, BlockSize);
  until I < BlockSize;
  if I > 0 then
  begin
    Move(PAnsiChar(BlockStart)^, D^, LS); Inc(D, LS);
    Move(S^, D^, I);                  Inc(D, I);
    Move(PAnsiChar(BlockEnd)^, D^, LE);   Inc(D, LE);
  end;
  SetLength(Result, D - PAnsiChar(Result));
end;

function RemoveBlocks(const Value, BlockStart, BlockEnd: AnsiString): AnsiString;
var
  LS,LE: Integer;
  S,D,L,K: PAnsiChar;
begin
  SetLength(Result, Length(Value));
  LS := Length(BlockStart);
  LE := Length(BlockEnd);
  D := PAnsiChar(Result);
  S := PAnsiChar(Value);
  L := S + Length(Value);
  repeat
    if S > L then Break;
    if LS > 0 then
    begin
      S := StrPos(S, PAnsiChar(BlockStart));
      if S = nil then Break;
      Inc(S, LS);
      if S > L then Break;
    end;
    K := StrPos(S, PAnsiChar(BlockEnd));
    if K = nil then K := L;
    Move(S^, D^, K - S);
    Inc(D, K - S);
    S := K + LE;
  until S >= L;
  SetLength(Result, D - PAnsiChar(Result));
end;

end.
