unit AsphyreBase64;
//---------------------------------------------------------------------------
// AsphyreBase64.pas                                    Modified: 15-Dec-2008
// Base-64 conversion                                            Version 1.01
//---------------------------------------------------------------------------
// This file was part of cryptographic components written by David Barton.
// It has been modified by Yuriy Kotsarenko to be compatible with the rest
// of Asphyre library.
//
// NOTICE: The license statement mentioned below applies *ONLY* to this file
// and is not related in any way to other Asphyre components!
//---------------------------------------------------------------------------
// Copyright (c) 1999 - 2002 David Barton (crypto@cityinthesky.co.uk)
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
//---------------------------------------------------------------------------

{$ifdef fpc}{$mode delphi}{$endif}

//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 SysUtils;

//---------------------------------------------------------------------------
// EncodeBase64()
//
// Encodes the binary source into base-64 readable text. This effectively
// increases the size by 4/3 (rounded up). If the source is not divisible
// by 3, it will be padded with 0.
//---------------------------------------------------------------------------
function EncodeBase64(Source, Dest: Pointer; Size: Integer): Integer;

//---------------------------------------------------------------------------
// DecodeBase64()
//
// Decodes the base-64 readable text back into the binary format.
//---------------------------------------------------------------------------
function DecodeBase64(Source, Dest: Pointer; Size: Integer): Integer;

//---------------------------------------------------------------------------
// Base64String()
//
// Encodes the binary source and returns base-64 AnsiString.
//---------------------------------------------------------------------------
function Base64String(Source: Pointer; Size: Integer): AnsiString;

//---------------------------------------------------------------------------
// Base64Binary()
//
// Decodes base-64 AnsiString back into binary format.
//---------------------------------------------------------------------------
function Base64Binary(const Source: AnsiString; Dest: Pointer): Integer;

//---------------------------------------------------------------------------
implementation
{$Q-}{$R-}

//---------------------------------------------------------------------------
const
 Base64: array[0..63] of Byte = (65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,
  76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 97, 98, 99, 100,
  101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115,
  116, 117, 118, 119, 120, 121, 122, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57,
  43, 47);

//---------------------------------------------------------------------------
function EncodeBase64(Source, Dest: Pointer; Size: Integer): Integer;
var
 i     : Integer;
 iPtr  : Integer;
 oPtr  : Integer;
 Input : PByteArray;
 Output: PByteArray;
begin
 Input := PByteArray(Source);
 Output:= PByteArray(Dest);
 iPtr  := 0;
 oPtr  := 0;

 for i:= 1 to (Size div 3) do
  begin
   Output[oPtr + 0]:= Base64[Input[iPtr] shr 2];
   Output[oPtr + 1]:= Base64[((Input[iPtr] and 3) shl 4) +
    (Input[iPtr + 1] shr 4)];
   Output[oPtr + 2]:= Base64[((Input[iPtr + 1] and 15) shl 2) +
    (Input[iPtr + 2] shr 6)];
   Output[oPtr + 3]:= Base64[Input[iPtr + 2] and 63];

   Inc(iPtr, 3);
   Inc(oPtr, 4);
  end;

 case (Size mod 3) of
  1: begin
      Output[oPtr + 0]:= Base64[Input[iPtr] shr 2];
      Output[oPtr + 1]:= Base64[(Input[iPtr] and 3) shl 4];
      Output[oPtr + 2]:= Byte('=');
      Output[oPtr + 3]:= Byte('=');
     end;
  2: begin
      Output^[oPtr + 0]:= Base64[Input[iPtr] shr 2];
      Output^[oPtr + 1]:= Base64[((Input[iPtr] and 3) shl 4) +
       (Input[iPtr + 1] shr 4)];
      Output^[oPtr + 2]:= Base64[(Input^[iPtr + 1] and 15) shl 2];
      Output^[oPtr + 3]:= Byte('=');
     end;
 end;

 Result:= ((Size + 2) div 3) * 4;
end;

//---------------------------------------------------------------------------
function DecodeBase64(Source, Dest: Pointer; Size: Integer): Integer;
var
 i, j: Integer;
 iPtr: Integer;
 oPtr: Integer;
 Temp: array[0..3] of Byte;
 Input :  PByteArray;
 Output: PByteArray;
begin
 Input := PByteArray(Source);
 Output:= PByteArray(Dest);
 iPtr  := 0;
 oPtr  := 0;
 Result:= 0;

 for i:= 1 to (Size div 4) do
  begin
   for j:= 0 to 3 do
    begin
     case Input[iPtr] of
      65..90 : Temp[j]:= Input[iPtr] - Ord('A');
      97..122: Temp[j]:= Input[iPtr] - Ord('a') + 26;
      48..57 : Temp[j]:= Input[iPtr] - Ord('0') + 52;
      43     : Temp[j]:= 62;
      47     : Temp[j]:= 63;
      61     : Temp[j]:= $FF;
     end;

     Inc(iPtr);
    end;

   Output[oPtr]:= (Temp[0] shl 2) or (Temp[1] shr 4);
   Result:= oPtr + 1;
   if (Temp[2] <> $FF)and(Temp[3] = $FF) then
    begin
     Output[oPtr + 1]:= (Temp[1] shl 4) or (Temp[2] shr 2);
     Result:= oPtr + 2;
     Inc(oPtr);
    end else
   if (Temp[2] <> $FF) then
    begin
     Output[oPtr + 1]:= (Temp[1] shl 4) or (Temp[2] shr 2);
     Output[oPtr + 2]:= (Temp[2] shl 6) or  Temp[3];
     Result:= oPtr + 3;
     Inc(oPtr, 2);
    end;
   Inc(oPtr);
  end;
end;

//---------------------------------------------------------------------------
function Base64String(Source: Pointer; Size: Integer): AnsiString;
begin
 SetLength(Result, ((Size + 2) div 3) * 4);
 EncodeBase64(Source, @Result[1], Size);
end;

//---------------------------------------------------------------------------
function Base64Binary(const Source: AnsiString; Dest: Pointer): Integer;
begin
 Result:= DecodeBase64(@Source[1], Dest, Length(Source));
end;

//---------------------------------------------------------------------------
end.
