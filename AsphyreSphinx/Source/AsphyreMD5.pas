unit AsphyreMD5;
//---------------------------------------------------------------------------
// AsphyreMD5.pas                                       Modified: 17-Oct-2005
// MD5 Message-Digest Algorithm                                   Version 1.0
//---------------------------------------------------------------------------
// This unit was originally written by Matthias Fichtner, 1999 and has been
// adapted by Afterwarp Interactive to be compatible with the rest of the
// library.
//
// NOTICE: The license statement mentioned below applies *ONLY* to this file
// and is not related in any way to Afterwarp Interactive or Asphyre library!
//---------------------------------------------------------------------------
// RSA Data Security, Inc., MD5 message-digest algorithm
//
// Copyright (C) 1991-2, RSA Data Security, Inc. Created 1991. All
// rights reserved.
//
// License to copy and use this software is granted provided that it
// is identified as the "RSA Data Security, Inc. MD5 Message-Digest
// Algorithm" in all material mentioning or referencing this software
// or this function.
//
// License is also granted to make and use derivative works provided
// that such works are identified as "derived from the RSA Data
// Security, Inc. MD5 Message-Digest Algorithm" in all material
// mentioning or referencing the derived work.
//
// RSA Data Security, Inc. makes no representations concerning either
// the merchantability of this software or the suitability of this
// software for any particular purpose. It is provided "as is"
// without express or implied warranty of any kind.
//
// These notices must be retained in any copies of any part of this
// documentation and/or software.
//---------------------------------------------------------------------------

{$ifdef fpc}{$mode delphi}{$packrecords 1}{$packenum 1}{$endif}

interface

//---------------------------------------------------------------------------
uses
 SysUtils;

//---------------------------------------------------------------------------
type
 MD5Block    = array[0..15] of Longword;
 MD5CBits    = array[0..7] of byte;
 TMD5State   = array[0..3] of Longword;
 TMD5Digest  = array[0..15] of Byte;
 TMD5Buffer  = array[0..63] of Byte;
 TMD5Context = record
  State : TMD5State;
  Count : array[0..1] of Longword;
  Buffer: TMD5Buffer;
 end;

//---------------------------------------------------------------------------
procedure MD5Init(out Context: TMD5Context);
procedure MD5Update(var Context: TMD5Context; Source: Pointer; Size: Longword);
procedure MD5Final(var Context: TMD5Context; out Digest: TMD5Digest);

//---------------------------------------------------------------------------
// MD5Checksum()
//
// Returns MD5 128-bit checksum based on supplied memory block. The checksum
// occupies 16 bytes of memory.
//---------------------------------------------------------------------------
procedure MD5Checksum(Source: Pointer; Size: Integer; Checksum: Pointer);

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
{$R-}
{$Q-}

//---------------------------------------------------------------------------
var
 Padding: TMD5Buffer = ($80, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
  $00, $00, $00, $00, $00, $00, $00, $00);

//---------------------------------------------------------------------------
function F(x, y, z: Longword): Longword;
begin
 Result:= (x and y) or ((not x) and z);
end;

//---------------------------------------------------------------------------
function G(x, y, z: Longword): Longword;
begin
 Result:= (x and z) or (y and (not z));
end;

//---------------------------------------------------------------------------
function H(x, y, z: Longword): Longword;
begin
 Result:= x xor y xor z;
end;

//---------------------------------------------------------------------------
function I(x, y, z: Longword): Longword;
begin
 Result:= y xor (x or (not z));
end;

//---------------------------------------------------------------------------
procedure Rot(var x: Longword; n: Byte);
begin
 x:= (x shl n) or (x shr (32 - n));
end;

//---------------------------------------------------------------------------
procedure FF(var a: Longword; b, c, d, x: Longword; s: Byte; ac: Longword);
begin
 Inc(a, F(b, c, d) + x + ac);
 Rot(a, s);
 Inc(a, b);
end;

//---------------------------------------------------------------------------
procedure GG(var a: Longword; b, c, d, x: Longword; s: Byte; ac: Longword);
begin
 Inc(a, G(b, c, d) + x + ac);
 Rot(a, s);
 Inc(a, b);
end;

//---------------------------------------------------------------------------
procedure HH(var a: Longword; b, c, d, x: Longword; s: Byte; ac: Longword);
begin
 Inc(a, H(b, c, d) + x + ac);
 Rot(a, s);
 Inc(a, b);
end;

//---------------------------------------------------------------------------
procedure II(var a: Longword; b, c, d, x: Longword; s: Byte; ac: Longword);
begin
 Inc(a, I(b, c, d) + x + ac);
 Rot(a, s);
 Inc(a, b);
end;

//---------------------------------------------------------------------------
procedure Encode(Source, Target: Pointer; Count: Longword);
var
 S: PByte;
 T: PLongword;
 i: Longword;
begin
 S:= Source;
 T:= Target;

 for i:= 0 to (Count div 4) - 1 do
  begin
   T^:= S^;
   Inc(S);
   T^:= T^ or (S^ shl 8);
   Inc(S);
   T^:= T^ or (S^ shl 16);
   Inc(S);
   T^:= T^ or (S^ shl 24);

   Inc(S);
   Inc(T);
  end;
end;

//---------------------------------------------------------------------------
procedure Decode(Source, Target: Pointer; Count: Longword);
var
 S: PLongword;
 T: PByte;
 i: Longword;
begin
 S:= Source;
 T:= Target;

 for i:= 0 to Count - 1 do
  begin
   T^:= S^ and $FF;
   Inc(T);
   T^:= (S^ shr 8) and $FF;
   Inc(T);
   T^:= (S^ shr 16) and $FF;
   Inc(T);
   T^:= (S^ shr 24) and $FF;

   Inc(T);
   Inc(S);
  end;
end;

//---------------------------------------------------------------------------
procedure Transform(Buffer: Pointer; var State: TMD5State);
var
 a, b, c, d: Longword;
 Block: MD5Block;
begin
 Encode(Buffer, @Block, 64);

 a:= State[0];
 b:= State[1];
 c:= State[2];
 d:= State[3];

 FF(a, b, c, d, Block[ 0],  7, $d76aa478);
 FF(d, a, b, c, Block[ 1], 12, $e8c7b756);
 FF(c, d, a, b, Block[ 2], 17, $242070db);
 FF(b, c, d, a, Block[ 3], 22, $c1bdceee);
 FF(a, b, c, d, Block[ 4],  7, $f57c0faf);
 FF(d, a, b, c, Block[ 5], 12, $4787c62a);
 FF(c, d, a, b, Block[ 6], 17, $a8304613);
 FF(b, c, d, a, Block[ 7], 22, $fd469501);
 FF(a, b, c, d, Block[ 8],  7, $698098d8);
 FF(d, a, b, c, Block[ 9], 12, $8b44f7af);
 FF(c, d, a, b, Block[10], 17, $ffff5bb1);
 FF(b, c, d, a, Block[11], 22, $895cd7be);
 FF(a, b, c, d, Block[12],  7, $6b901122);
 FF(d, a, b, c, Block[13], 12, $fd987193);
 FF(c, d, a, b, Block[14], 17, $a679438e);
 FF(b, c, d, a, Block[15], 22, $49b40821);
 GG(a, b, c, d, Block[ 1],  5, $f61e2562);
 GG(d, a, b, c, Block[ 6],  9, $c040b340);
 GG(c, d, a, b, Block[11], 14, $265e5a51);
 GG(b, c, d, a, Block[ 0], 20, $e9b6c7aa);
 GG(a, b, c, d, Block[ 5],  5, $d62f105d);
 GG(d, a, b, c, Block[10],  9,  $2441453);
 GG(c, d, a, b, Block[15], 14, $d8a1e681);
 GG(b, c, d, a, Block[ 4], 20, $e7d3fbc8);
 GG(a, b, c, d, Block[ 9],  5, $21e1cde6);
 GG(d, a, b, c, Block[14],  9, $c33707d6);
 GG(c, d, a, b, Block[ 3], 14, $f4d50d87);
 GG(b, c, d, a, Block[ 8], 20, $455a14ed);
 GG(a, b, c, d, Block[13],  5, $a9e3e905);
 GG(d, a, b, c, Block[ 2],  9, $fcefa3f8);
 GG(c, d, a, b, Block[ 7], 14, $676f02d9);
 GG(b, c, d, a, Block[12], 20, $8d2a4c8a);
 HH(a, b, c, d, Block[ 5],  4, $fffa3942);
 HH(d, a, b, c, Block[ 8], 11, $8771f681);
 HH(c, d, a, b, Block[11], 16, $6d9d6122);
 HH(b, c, d, a, Block[14], 23, $fde5380c);
 HH(a, b, c, d, Block[ 1],  4, $a4beea44);
 HH(d, a, b, c, Block[ 4], 11, $4bdecfa9);
 HH(c, d, a, b, Block[ 7], 16, $f6bb4b60);
 HH(b, c, d, a, Block[10], 23, $bebfbc70);
 HH(a, b, c, d, Block[13],  4, $289b7ec6);
 HH(d, a, b, c, Block[ 0], 11, $eaa127fa);
 HH(c, d, a, b, Block[ 3], 16, $d4ef3085);
 HH(b, c, d, a, Block[ 6], 23,  $4881d05);
 HH(a, b, c, d, Block[ 9],  4, $d9d4d039);
 HH(d, a, b, c, Block[12], 11, $e6db99e5);
 HH(c, d, a, b, Block[15], 16, $1fa27cf8);
 HH(b, c, d, a, Block[ 2], 23, $c4ac5665);
 II(a, b, c, d, Block[ 0],  6, $f4292244);
 II(d, a, b, c, Block[ 7], 10, $432aff97);
 II(c, d, a, b, Block[14], 15, $ab9423a7);
 II(b, c, d, a, Block[ 5], 21, $fc93a039);
 II(a, b, c, d, Block[12],  6, $655b59c3);
 II(d, a, b, c, Block[ 3], 10, $8f0ccc92);
 II(c, d, a, b, Block[10], 15, $ffeff47d);
 II(b, c, d, a, Block[ 1], 21, $85845dd1);
 II(a, b, c, d, Block[ 8],  6, $6fa87e4f);
 II(d, a, b, c, Block[15], 10, $fe2ce6e0);
 II(c, d, a, b, Block[ 6], 15, $a3014314);
 II(b, c, d, a, Block[13], 21, $4e0811a1);
 II(a, b, c, d, Block[ 4],  6, $f7537e82);
 II(d, a, b, c, Block[11], 10, $bd3af235);
 II(c, d, a, b, Block[ 2], 15, $2ad7d2bb);
 II(b, c, d, a, Block[ 9], 21, $eb86d391);

 Inc(State[0], a);
 Inc(State[1], b);
 Inc(State[2], c);
 Inc(State[3], d);
end;

//---------------------------------------------------------------------------
procedure MD5Init(out Context: TMD5Context);
begin
 with Context do
  begin
   State[0]:= $67452301;
   State[1]:= $efcdab89;
   State[2]:= $98badcfe;
   State[3]:= $10325476;

   Count[0]:= 0;
   Count[1]:= 0;

   FillChar(Buffer, SizeOf(TMD5Buffer), 0);
  end;
end;

//---------------------------------------------------------------------------
procedure MD5Update(var Context: TMD5Context; Source: Pointer; Size: Longword);
var
 Index: Longword;
 PartLen: Longword;
 i: Longword;
begin
 with Context do
  begin
   Index:= (Count[0] shr 3) and $3F;
   Inc(Count[0], Size shl 3);
   if (Count[0] < (Size shl 3)) then Inc(Count[1]);
   Inc(Count[1], Size shr 29);
  end;

 PartLen:= 64 - Index;
 if (Size >= PartLen) then
  begin
   Move(Source^, Context.Buffer[Index], PartLen);
   Transform(@Context.Buffer, Context.State);
   i:= PartLen;
   while (i + 63 < Size) do
    begin
     Transform(@PByteArray(Source)[i], Context.State);
     Inc(i, 64);
		end;

   Index:= 0;
	end else i:= 0;

 Move(PByteArray(Source)[i], Context.Buffer[Index], Size - i);
end;

//---------------------------------------------------------------------------
procedure MD5Final(var Context: TMD5Context; out Digest: TMD5Digest);
var
 Bits  : MD5CBits;
 Index : Longword;
 PadLen: Longword;
begin
 Decode(@Context.Count, @Bits, 2);
 Index:= (Context.Count[0] shr 3) and $3F;
 if (Index < 56) then PadLen:= 56 - Index else PadLen:= 120 - Index;

 MD5Update(Context, @Padding, PadLen);
 MD5Update(Context, @Bits, 8);

 Decode(@Context.State, @Digest, 4);
 FillChar(Context, SizeOf(TMD5Context), 0);
end;

//---------------------------------------------------------------------------
procedure MD5Checksum(Source: Pointer; Size: Integer; Checksum: Pointer);
var
 Context: TMD5Context;
 DigestM: TMD5Digest;
begin
 MD5Init(Context);
 MD5Update(Context, Source, Size);
 MD5Final(Context, DigestM);

 Move(DigestM, Checksum^, SizeOf(TMD5Digest));
end;

//---------------------------------------------------------------------------
end.

