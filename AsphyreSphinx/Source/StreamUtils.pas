unit StreamUtils;
//---------------------------------------------------------------------------
// StreamUtils.pas                                      Modified: 30-Dec-2010
// Utility routines for handling simple data types in streams.   Version 1.01
//---------------------------------------------------------------------------
// Important Notice:
//
// If you modify/use this code or one of its parts either in original or
// modified form, you must comply with Mozilla Public License v1.1,
// specifically section 3, "Distribution Obligations". Failure to do so will
// result in the license breach, which will be resolved in the court.
// Remember that violating author's rights is considered a serious crime in
// many countries. Thank you!
//
// !! Please *read* Mozilla Public License 1.1 document located at:
//  http://www.mozilla.org/MPL/
//
// If you require any clarifications about the license, feel free to contact
// us or post your question on our forums at: http://www.afterwarp.net
//---------------------------------------------------------------------------
// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
// License for the specific language governing rights and limitations
// under the License.
//
// The Original Code is StreamUtils.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2007 - 2011,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Classes, AsphyreUtils, Vectors2px;

//---------------------------------------------------------------------------
// StreamPutWideString()
//
// Stores a wide string inside the stream. If FixedLength is specified,
// a fixed number of chars is always written. In this case, the string
// cannot be larger than 65535 characters.
//---------------------------------------------------------------------------
procedure StreamPutWideString(Stream: TStream; const Text: WideString;
 FixedLength: Integer = -1);
function StreamGetWideString(Stream: TStream;
 FixedLength: Integer = -1): WideString;

//---------------------------------------------------------------------------
// StreamPutAnsiString()
//
// Stores an ansi string inside the stream. If FixedLength is specified,
// a fixed number of chars is always written. In this case, the string
// cannot be larger than 65535 characters.
//---------------------------------------------------------------------------
procedure StreamPutAnsiString(Stream: TStream; const Text: AnsiString;
 FixedLength: Integer = -1);
function StreamGetAnsiString(Stream: TStream;
 FixedLength: Integer = -1): AnsiString;

//---------------------------------------------------------------------------
// StreamPutShortString()
//
// Stores a short string inside the stream. If FixedLength is specified,
// a fixed number of chars is always written. In this case, the string
// cannot be larger than 255 characters.
//---------------------------------------------------------------------------
procedure StreamPutShortString(Stream: TStream; const Text: ShortString;
 FixedLength: Integer = -1);
function StreamGetShortString(Stream: TStream;
 FixedLength: Integer = -1): ShortString;

//---------------------------------------------------------------------------
// StreamPutByte()
//
// Stores a byte inside the stream. If the value is outside of 0..255 range,
// it will be clamped.
//---------------------------------------------------------------------------
procedure StreamPutByte(Stream: TStream; Value: Cardinal);
function StreamGetByte(Stream: TStream): Cardinal;

//---------------------------------------------------------------------------
// StreamPutWord()
//
// Stores a word inside the stream. If the value is outside of 0..65535 range,
// it will be clamped.
//---------------------------------------------------------------------------
procedure StreamPutWord(Stream: TStream; Value: Cardinal);
function StreamGetWord(Stream: TStream): Cardinal;

//---------------------------------------------------------------------------
// StreamPutLongword()
//
// Stores a longword inside the stream.
//---------------------------------------------------------------------------
procedure StreamPutLongword(Stream: TStream; Value: Cardinal);
function StreamGetLongword(Stream: TStream): Cardinal;

//---------------------------------------------------------------------------
// StreamPutShortInt()
//
// Stores a shortint inside the stream. If the value is outside
// of -128..127 range, it will be clamped.
//---------------------------------------------------------------------------
procedure StreamPutShortInt(Stream: TStream; Value: Integer);
function StreamGetShortInt(Stream: TStream): Integer;

//---------------------------------------------------------------------------
// StreamPutSmallInt()
//
// Stores a smallint inside the stream. If the value is outside
// of -32768..32767 range, it will be clamped.
//---------------------------------------------------------------------------
procedure StreamPutSmallInt(Stream: TStream; Value: Integer);
function StreamGetSmallInt(Stream: TStream): Integer;

//---------------------------------------------------------------------------
// StreamPutLongint()
//
// Stores a longint inside the stream.
//---------------------------------------------------------------------------
procedure StreamPutLongint(Stream: TStream; Value: Integer);
function StreamGetLongint(Stream: TStream): Integer;

//---------------------------------------------------------------------------
// StreamPutBool()
//
// Stores a boolean value as byte. A value of 255 is equivalent of False,
// while 0 corresponds True.
//---------------------------------------------------------------------------
procedure StreamPutBool(Stream: TStream; Value: Boolean);
function StreamGetBool(Stream: TStream): Boolean;

//---------------------------------------------------------------------------
// StreamPutByteIndex()
//
// Stores an index as byte. A value of -1 (or other negatives) is stored
// as 255. All other values are clamped between 0 and 254.
//---------------------------------------------------------------------------
procedure StreamPutByteIndex(Stream: TStream; Value: Integer);
function StreamGetByteIndex(Stream: TStream): Integer;

//---------------------------------------------------------------------------
// StreamPutWordIndex()
//
// Stores an index as word. A value of -1 (or other negatives) is stored
// as 65535. All other values are clamped between 0 and 65534.
//---------------------------------------------------------------------------
procedure StreamPutWordIndex(Stream: TStream; Value: Integer);
function StreamGetWordIndex(Stream: TStream): Integer;

//---------------------------------------------------------------------------
// StreamPutLongPoint2px()
//
// Stores a 2-dimensional vector as a combination of two longints.
//---------------------------------------------------------------------------
procedure StreamPutLongPoint2px(Stream: TStream; const Vec: TPoint2px);
function StreamGetLongPoint2px(Stream: TStream): TPoint2px;

//---------------------------------------------------------------------------
// StreamPutWordPoint2px()
//
// Stores a 2-dimensional vector as a combination of two words. The maximum
// value for both coordinates is 65534. Higher values will be clamped.
//---------------------------------------------------------------------------
procedure StreamPutWordPoint2px(Stream: TStream; const Vec: TPoint2px);
function StreamGetWordPoint2px(Stream: TStream): TPoint2px;

//---------------------------------------------------------------------------
// StreamPutBytePoint2px()
//
// Stores a 2-dimensional vector as a combination of two bytes. The maximum
// value for both coordinates is 254. Higher values will be clamped.
//---------------------------------------------------------------------------
procedure StreamPutBytePoint2px(Stream: TStream; const Vec: TPoint2px);
function StreamGetBytePoint2px(Stream: TStream): TPoint2px;

//---------------------------------------------------------------------------
// StreamPutStrings()
//
// Stores the list of strings with a possibly fixed length.
//---------------------------------------------------------------------------
procedure StreamPutStrings(Stream: TStream; Strings: TStrings;
 FixedLength: Integer = -1);
procedure StreamGetStrings(Stream: TStream; Strings: TStrings;
 FixedLength: Integer = -1);

//---------------------------------------------------------------------------
// StreamPutSingle()
//
// Stores a single floating point value inside the stream.
//---------------------------------------------------------------------------
procedure StreamPutSingle(Stream: TStream; Value: Single);
function StreamGetSingle(Stream: TStream): Single;

//---------------------------------------------------------------------------
// StreamPutDouble()
//
// Stores a double floating point value inside the stream.
//---------------------------------------------------------------------------
procedure StreamPutDouble(Stream: TStream; Value: Double);
function StreamGetDouble(Stream: TStream): Double;

//---------------------------------------------------------------------------
// StreamPutFloat34()
//
// Stores a floating-point type in 1:3:4 fixed-point format.
//---------------------------------------------------------------------------
procedure StreamPutFloat34(Stream: TStream; Value: Single);
function StreamGetFloat34(Stream: TStream): Single;

//---------------------------------------------------------------------------
// StreamPutFloat43()
//
// Stores a floating-point type in 1:4:3 fixed-point format.
//---------------------------------------------------------------------------
procedure StreamPutFloat43(Stream: TStream; Value: Single);
function StreamGetFloat43(Stream: TStream): Single;

//---------------------------------------------------------------------------
// StreamPutFloats44()
//
// Stores two floating-point types in 4:0 fixed-point format.
//---------------------------------------------------------------------------
procedure StreamPutFloats44(Stream: TStream; Value1, Value2: Single);
procedure StreamGetFloats44(Stream: TStream; out Value1, Value2: Single);

//---------------------------------------------------------------------------
// StreamPutFloats3311()
//
// Stores two floating-point types in 3:1 fixed-point format.
//---------------------------------------------------------------------------
procedure StreamPutFloats3311(Stream: TStream; Value1, Value2: Single);
procedure StreamGetFloats3311(Stream: TStream; out Value1, Value2: Single);

//---------------------------------------------------------------------------
// StreamPutSimpleShortString()
//
// Stores a short string inside the stream using simple format.
//---------------------------------------------------------------------------
procedure StreamPutSimpleShortString(Stream: TStream; const Text: ShortString);
function StreamGetSimpleShortString(Stream: TStream): ShortString;

 //---------------------------------------------------------------------------
// StreamPutSimpleAnsiString()
//
// Stores the string as an ansi string using simple format.
//---------------------------------------------------------------------------
function StreamGetSimpleAnsiString(Stream: TStream): AnsiString;
procedure StreamPutSimpleAnsiString(Stream: TStream; const Value: AnsiString);

//----------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
procedure StreamPutWideString(Stream: TStream; const Text: WideString;
 FixedLength: Integer = -1);
var
 i, LongChars: LongInt;
 FixedPlaces, FixedChars, ZeroWord: Word;
begin
 FixedPlaces:= 0;
 if (FixedLength > 0)and(FixedLength < 65536) then FixedPlaces:= FixedLength;

 // -> Fixed Size (2 bytes)
 Stream.WriteBuffer(FixedPlaces, SizeOf(Word));

 if (FixedPlaces = 0) then
  begin // Unlimited size
   LongChars:= Length(Text);

   // -> Text Length (4 bytes)
   Stream.WriteBuffer(LongChars, SizeOf(LongInt));

   // -> Characters (Length x 2 bytes)
   for i:= 0 to LongChars - 1 do
    Stream.WriteBuffer(Text[i + 1], SizeOf(Word));
  end else
  begin // Fixed size
   LongChars:= Min2(Length(Text), FixedPlaces);

   ZeroWord  := 0;
   FixedChars:= LongChars;

   // -> Text Length (2 bytes)
   Stream.WriteBuffer(FixedChars, SizeOf(Word));

   // Characters (FixedLength x 2 bytes)
   for i:= 0 to FixedPlaces - 1 do
    if (i < FixedChars) then
     Stream.WriteBuffer(Text[i + 1], SizeOf(Word))
      else Stream.WriteBuffer(ZeroWord, SizeOf(Word));
  end;
end;

//---------------------------------------------------------------------------
function StreamGetWideString(Stream: TStream;
 FixedLength: Integer = -1): WideString;
var
 i, LongChars: LongInt;
 FixedPlaces, FixedChars, NullWord: Word;
begin
 // -> Fixed Size (2 bytes)
 Stream.ReadBuffer(FixedPlaces, SizeOf(Word));

 if (FixedLength <> -1) then FixedPlaces:= Min2(FixedPlaces, FixedLength);

 if (FixedPlaces = 0) then
  begin // Unlimited size
   // -> Text Length (4 bytes)
   Stream.ReadBuffer(LongChars, SizeOf(LongInt));

   // -> Characters (Length x 2 bytes)
   SetLength(Result, LongChars);
   for i:= 0 to LongChars - 1 do
    Stream.ReadBuffer(Result[i + 1], SizeOf(Word));
  end else
  begin // Fixed size
   // -> Text Length (2 bytes)
   Stream.ReadBuffer(FixedChars, SizeOf(Word));

   // Characters (FixedLength x 2 bytes)
   SetLength(Result, Min2(FixedChars, FixedPlaces));
   for i:= 0 to FixedPlaces - 1 do
    if (i < FixedChars) then
     Stream.ReadBuffer(Result[i + 1], SizeOf(Word))
      else Stream.ReadBuffer(NullWord, SizeOf(Word));
  end;
end;

//---------------------------------------------------------------------------
procedure StreamPutAnsiString(Stream: TStream; const Text: AnsiString;
 FixedLength: Integer = -1);
var
 i, LongChars: LongInt;
 FixedPlaces, FixedChars: Word;
 ZeroByte: Byte;
begin
 FixedPlaces:= 0;
 if (FixedLength > 0)and(FixedLength < 65536) then FixedPlaces:= FixedLength;

 // -> Fixed Size (2 bytes)
 Stream.WriteBuffer(FixedPlaces, SizeOf(Word));

 if (FixedPlaces = 0) then
  begin // Unlimited size
   LongChars:= Length(Text);

   // -> Text Length (4 bytes)
   Stream.WriteBuffer(LongChars, SizeOf(LongInt));

   // -> Characters (Length bytes)
   for i:= 0 to LongChars - 1 do
    Stream.WriteBuffer(Text[i + 1], SizeOf(Byte));
  end else
  begin // Fixed size
   LongChars:= Min2(Length(Text), FixedPlaces);

   ZeroByte  := 0;
   FixedChars:= LongChars;

   // -> Text Length (2 bytes)
   Stream.WriteBuffer(FixedChars, SizeOf(Word));

   // Characters (FixedLength bytes)
   for i:= 0 to FixedPlaces - 1 do
    if (i < FixedChars) then
     Stream.WriteBuffer(Text[i + 1], SizeOf(Byte))
      else Stream.WriteBuffer(ZeroByte, SizeOf(Byte));
  end;
end;

//---------------------------------------------------------------------------
function StreamGetAnsiString(Stream: TStream;
 FixedLength: Integer = -1): AnsiString;
var
 i, LongChars: LongInt;
 FixedPlaces, FixedChars: Word;
 NullByte: Byte;
begin
 // -> Fixed Size (2 bytes)
 Stream.ReadBuffer(FixedPlaces, SizeOf(Word));

 if (FixedLength <> -1) then FixedPlaces:= Min2(FixedPlaces, FixedLength);

 if (FixedPlaces = 0) then
  begin // Unlimited size
   // -> Text Length (4 bytes)
   Stream.ReadBuffer(LongChars, SizeOf(LongInt));

   // -> Characters (Length bytes)
   SetLength(Result, LongChars);
   for i:= 0 to LongChars - 1 do
    Stream.ReadBuffer(Result[i + 1], SizeOf(Byte));
  end else
  begin // Fixed size
   // -> Text Length (2 bytes)
   Stream.ReadBuffer(FixedChars, SizeOf(Word));

   // Characters (FixedLength bytes)
   SetLength(Result, Min2(FixedChars, FixedPlaces));
   for i:= 0 to FixedPlaces - 1 do
    if (i < FixedChars) then
     Stream.ReadBuffer(Result[i + 1], SizeOf(Byte))
      else Stream.ReadBuffer(NullByte, SizeOf(Byte));
  end;
end;

//---------------------------------------------------------------------------
procedure StreamPutShortString(Stream: TStream; const Text: ShortString;
 FixedLength: Integer = -1);
var
 i, FixedPlaces: Integer;
begin
 FixedPlaces:= 0;

 // Check, if user wants a fixed number of characters.
 if (FixedLength > 0) then FixedPlaces:= Min2(FixedLength, 255);

 // If fixed length is not specified, then change number of places depending
 // on the length of text (dynamic size).
 if (FixedPlaces = 0) then FixedPlaces:= Length(Text);

 // -> Text Length (1 byte)
 StreamPutByte(Stream, Length(Text));

 // -> Text Contents (Length x 1 byte)
 for i:= 0 to FixedPlaces - 1 do
  if (i < Length(Text)) then StreamPutByte(Stream, Byte(Text[i + 1]))
   else StreamPutByte(Stream, 0);
end;

//---------------------------------------------------------------------------
function StreamGetShortString(Stream: TStream;
 FixedLength: Integer = -1): ShortString;
var
 i, FixedPlaces, TextLen: Integer;
begin
 FixedPlaces:= 0;

 // Check, if user wants a fixed number of characters.
 if (FixedLength > 0) then FixedPlaces:= Min2(FixedLength, 255);

 // -> Text Length (1 byte)
 TextLen:= StreamGetByte(Stream);

 // If fixed length is not specified, then the number of places corresponds to
 // the length of the input text (dynamic size).
 if (FixedPlaces = 0) then FixedPlaces:= TextLen;

 // Specify new text size.
 SetLength(Result, TextLen);

 // Read the text contents.
 for i:= 0 to FixedPlaces - 1 do
  if (i < TextLen) then
   Result[i + 1]:= AnsiChar(StreamGetByte(Stream))
    else StreamGetByte(Stream);
end;

//---------------------------------------------------------------------------
procedure StreamPutByte(Stream: TStream; Value: Cardinal);
var
 ByteValue: Byte;
begin
 ByteValue:= Min2(Value, 255);
 Stream.WriteBuffer(ByteValue, SizeOf(Byte));
end;

//---------------------------------------------------------------------------
function StreamGetByte(Stream: TStream): Cardinal;
var
 ByteValue: Byte;
begin
 Stream.ReadBuffer(ByteValue, SizeOf(Byte));
 Result:= ByteValue;
end;

//---------------------------------------------------------------------------
procedure StreamPutWord(Stream: TStream; Value: Cardinal);
var
 WordValue: Word;
begin
 WordValue:= Min2(Value, 65535);
 Stream.WriteBuffer(WordValue, SizeOf(Word));
end;

//---------------------------------------------------------------------------
function StreamGetWord(Stream: TStream): Cardinal;
var
 WordValue: Word;
begin
 Stream.ReadBuffer(WordValue, SizeOf(Word));
 Result:= WordValue;
end;

//---------------------------------------------------------------------------
procedure StreamPutLongword(Stream: TStream; Value: Cardinal);
begin
 Stream.WriteBuffer(Value, SizeOf(Longword));
end;

//---------------------------------------------------------------------------
function StreamGetLongword(Stream: TStream): Cardinal;
begin
 Stream.ReadBuffer(Result, SizeOf(Longword));
end;

//---------------------------------------------------------------------------
procedure StreamPutShortInt(Stream: TStream; Value: Integer);
var
 IntValue: ShortInt;
begin
 IntValue:= MinMax2(Value, -128, 127);
 Stream.WriteBuffer(IntValue, SizeOf(ShortInt));
end;

//---------------------------------------------------------------------------
function StreamGetShortInt(Stream: TStream): Integer;
var
 IntValue: ShortInt;
begin
 Stream.ReadBuffer(IntValue, SizeOf(ShortInt));
 Result:= IntValue;
end;

//---------------------------------------------------------------------------
procedure StreamPutSmallInt(Stream: TStream; Value: Integer);
var
 IntValue: SmallInt;
begin
 IntValue:= MinMax2(Value, -32768, 32767);
 Stream.WriteBuffer(IntValue, SizeOf(SmallInt));
end;

//---------------------------------------------------------------------------
function StreamGetSmallInt(Stream: TStream): Integer;
var
 IntValue: SmallInt;
begin
 Stream.ReadBuffer(IntValue, SizeOf(SmallInt));
 Result:= IntValue;
end;

//---------------------------------------------------------------------------
procedure StreamPutLongint(Stream: TStream; Value: Integer);
begin
 Stream.WriteBuffer(Value, SizeOf(Longint));
end;

//---------------------------------------------------------------------------
function StreamGetLongint(Stream: TStream): Integer;
begin
 Stream.ReadBuffer(Result, SizeOf(Longint));
end;

//---------------------------------------------------------------------------
procedure StreamPutBool(Stream: TStream; Value: Boolean);
var
 ByteValue: Byte;
begin
 ByteValue:= 255;
 if (Value) then ByteValue:= 0;

 Stream.WriteBuffer(ByteValue, SizeOf(Byte));
end;

//---------------------------------------------------------------------------
function StreamGetBool(Stream: TStream): Boolean;
var
 ByteValue: Byte;
begin
 Stream.ReadBuffer(ByteValue, SizeOf(Byte));
 Result:= ByteValue < 128;
end;

//---------------------------------------------------------------------------
procedure StreamPutByteIndex(Stream: TStream; Value: Integer);
var
 ByteValue: Byte;
begin
 if (Value >= 0) then ByteValue:= Min2(Value, 254)
  else ByteValue:= 255;

 Stream.WriteBuffer(ByteValue, SizeOf(Byte));
end;

//---------------------------------------------------------------------------
function StreamGetByteIndex(Stream: TStream): Integer;
var
 ByteValue: Byte;
begin
 Stream.ReadBuffer(ByteValue, SizeOf(Byte));

 if (ByteValue <> 255) then Result:= ByteValue
  else Result:= -1;
end;

//---------------------------------------------------------------------------
procedure StreamPutWordIndex(Stream: TStream; Value: Integer);
var
 WordValue: Word;
begin
 if (Value >= 0) then WordValue:= Min2(Value, 65534)
  else WordValue:= 65535;

 Stream.WriteBuffer(WordValue, SizeOf(Word));
end;

//---------------------------------------------------------------------------
function StreamGetWordIndex(Stream: TStream): Integer;
var
 WordValue: Word;
begin
 Stream.ReadBuffer(WordValue, SizeOf(Word));

 if (WordValue <> 65535) then Result:= WordValue
  else Result:= -1;
end;

//---------------------------------------------------------------------------
procedure StreamPutLongPoint2px(Stream: TStream; const Vec: TPoint2px);
begin
 Stream.WriteBuffer(Vec.x, SizeOf(Longint));
 Stream.WriteBuffer(Vec.y, SizeOf(Longint));
end;

//---------------------------------------------------------------------------
function StreamGetLongPoint2px(Stream: TStream): TPoint2px;
begin
 Stream.ReadBuffer(Result.x, SizeOf(Longint));
 Stream.ReadBuffer(Result.y, SizeOf(Longint));
end;

//---------------------------------------------------------------------------
procedure StreamPutWordPoint2px(Stream: TStream; const Vec: TPoint2px);
var
 WordValue: Word;
begin
 if (Vec.x <> Low(Integer)) then WordValue:= MinMax2(Vec.x, 0, 65534)
  else WordValue:= 65535;

 Stream.WriteBuffer(WordValue, SizeOf(Word));

 if (Vec.y <> Low(Integer)) then WordValue:= MinMax2(Vec.y, 0, 65534)
  else WordValue:= 65535;

 Stream.WriteBuffer(WordValue, SizeOf(Word));
end;

//---------------------------------------------------------------------------
function StreamGetWordPoint2px(Stream: TStream): TPoint2px;
var
 WordValue: Word;
begin
 Stream.ReadBuffer(WordValue, SizeOf(Word));

 if (WordValue <> 65535) then Result.x:= WordValue
  else Result.x:= Low(Integer);

 Stream.ReadBuffer(WordValue, SizeOf(Word));

 if (WordValue <> 65535) then Result.y:= WordValue
  else Result.y:= Low(Integer);
end;

//---------------------------------------------------------------------------
procedure StreamPutBytePoint2px(Stream: TStream; const Vec: TPoint2px);
var
 ByteValue: Byte;
begin
 if (Vec.x <> Low(Integer)) then ByteValue:= MinMax2(Vec.x, 0, 254)
  else ByteValue:= 255;

 Stream.WriteBuffer(ByteValue, SizeOf(Byte));

 if (Vec.y <> Low(Integer)) then ByteValue:= MinMax2(Vec.y, 0, 254)
  else ByteValue:= 255;

 Stream.WriteBuffer(ByteValue, SizeOf(Byte));
end;

//---------------------------------------------------------------------------
function StreamGetBytePoint2px(Stream: TStream): TPoint2px;
var
 ByteValue: Byte;
begin
 Stream.ReadBuffer(ByteValue, SizeOf(Byte));

 if (ByteValue <> 255) then Result.x:= ByteValue
  else Result.x:= Low(Integer);

 Stream.ReadBuffer(ByteValue, SizeOf(Byte));

 if (ByteValue <> 255) then Result.y:= ByteValue
  else Result.y:= Low(Integer);
end;

//---------------------------------------------------------------------------
procedure StreamPutStrings(Stream: TStream; Strings: TStrings;
 FixedLength: Integer = -1);
var
 i, Count: Integer;
begin
 // -> Number of strings
 Count:= Min2(Strings.Count, 65535);
 StreamPutWord(Stream, Strings.Count);

 // -> Strings x Count
 for i:= 0 to Count - 1 do
  StreamPutAnsiString(Stream, Strings[i], FixedLength);
end;

//---------------------------------------------------------------------------
procedure StreamGetStrings(Stream: TStream; Strings: TStrings;
 FixedLength: Integer = -1);
var
 i, Count: Integer;
begin
 // -> Number of strings
 Count:= StreamGetWord(Stream);

 Strings.Clear();
 Strings.Capacity:= Count;

 // -> Strings x Count
 for i:= 0 to Count - 1 do
  Strings.Add(StreamGetAnsiString(Stream, FixedLength));
end;

//---------------------------------------------------------------------------
procedure StreamPutSingle(Stream: TStream; Value: Single);
begin
 Stream.WriteBuffer(Value, SizeOf(Single));
end;

//---------------------------------------------------------------------------
function StreamGetSingle(Stream: TStream): Single;
begin
 Stream.ReadBuffer(Result, SizeOf(Single));
end;

//---------------------------------------------------------------------------
procedure StreamPutDouble(Stream: TStream; Value: Double);
begin
 Stream.WriteBuffer(Value, SizeOf(Double));
end;

//---------------------------------------------------------------------------
function StreamGetDouble(Stream: TStream): Double;
begin
 Stream.ReadBuffer(Result, SizeOf(Double));
end;

//----------------------------------------------------------------------------
procedure StreamPutFloat34(Stream: TStream; Value: Single);
var
 Aux: Integer;
begin
 Aux:= MinMax2(Round(Value * 16.0), -128, 127);
 StreamPutShortInt(Stream, Aux);
end;

//----------------------------------------------------------------------------
function StreamGetFloat34(Stream: TStream): Single;
begin
 Result:= StreamGetShortInt(Stream) / 16.0;
end;

//----------------------------------------------------------------------------
procedure StreamPutFloat43(Stream: TStream; Value: Single);
var
 Aux: Integer;
begin
 Aux:= MinMax2(Round(Value * 8.0), -128, 127);
 StreamPutShortInt(Stream, Aux);
end;

//----------------------------------------------------------------------------
function StreamGetFloat43(Stream: TStream): Single;
begin
 Result:= StreamGetShortInt(Stream) / 8.0;
end;

//---------------------------------------------------------------------------
procedure StreamPutFloats44(Stream: TStream; Value1, Value2: Single);
var
 Aux1, Aux2: Integer;
begin
 Aux1:= MinMax2(Round(Value1), -8, 7) + 8;
 Aux2:= MinMax2(Round(Value2), -8, 7) + 8;

 StreamPutByte(Stream, Aux1 or (Aux2 shl 4));
end;

//---------------------------------------------------------------------------
procedure StreamGetFloats44(Stream: TStream; out Value1, Value2: Single);
var
 Aux: Integer;
begin
 Aux:= StreamGetByte(Stream);

 Value1:= ((Aux and $0F) - 8);
 Value2:= ((Aux shr 4) - 8);
end;

//---------------------------------------------------------------------------
procedure StreamPutFloats3311(Stream: TStream; Value1, Value2: Single);
var
 Aux1, Aux2: Integer;
begin
 Aux1:= MinMax2(Round(Value1 * 2.0), -8, 7) + 8;
 Aux2:= MinMax2(Round(Value2 * 2.0), -8, 7) + 8;

 StreamPutByte(Stream, Aux1 or (Aux2 shl 4));
end;

//---------------------------------------------------------------------------
procedure StreamGetFloats3311(Stream: TStream; out Value1, Value2: Single);
var
 Aux: Integer;
begin
 Aux:= StreamGetByte(Stream);

 Value1:= ((Aux and $0F) - 8) / 2.0;
 Value2:= ((Aux shr 4) - 8) / 2.0;
end;

//---------------------------------------------------------------------------
procedure StreamPutSimpleShortString(Stream: TStream; const Text: ShortString);
var
 i: Integer;
begin
 StreamPutByte(Stream, Length(Text));

 for i:= 0 to Length(Text) - 1 do
  StreamPutByte(Stream, Byte(Text[i + 1]));
end;

//---------------------------------------------------------------------------
function StreamGetSimpleShortString(Stream: TStream): ShortString;
var
 Count, i: Integer;
begin
 Count:= StreamGetByte(Stream);
 SetLength(Result, Count);

 for i:= 0 to Count - 1 do
  Result[i + 1]:= AnsiChar(StreamGetByte(Stream));
end;

//---------------------------------------------------------------------------
procedure StreamPutSimpleAnsiString(Stream: TStream; const Value: AnsiString);
var
 i: Integer;
begin
 StreamPutLongint(Stream, Length(Value));

 for i:= 0 to Length(Value) - 1 do
  StreamPutByte(Stream, Byte(Value[i + 1]));
end;

//---------------------------------------------------------------------------
function StreamGetSimpleAnsiString(Stream: TStream): AnsiString;
var
 Count, i: Integer;
begin
 Count:= StreamGetLongint(Stream);
 SetLength(Result, Count);

 for i:= 0 to Count - 1 do
  Result[i + 1]:= AnsiChar(StreamGetByte(Stream));
end;

//----------------------------------------------------------------------------
end.
