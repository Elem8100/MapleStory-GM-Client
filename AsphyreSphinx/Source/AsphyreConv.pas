unit AsphyreConv;
//---------------------------------------------------------------------------
// AsphyreConv.pas                                      Modified: 14-Dec-2008
// Asphyre Pixel Format conversion                                Version 1.4
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
// The Original Code is AsphyreConv.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by M. Sc. Yuriy Kotsarenko are Copyright (C) 2007 - 2008,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 AsphyreTypes;

//---------------------------------------------------------------------------
// PixelXto32()
//
// Converts a pixel from an arbitrary format to A8R8G8B8 (32-bit).
//---------------------------------------------------------------------------
function PixelXto32(Source: Pointer;
 SourceFormat: TAsphyrePixelFormat): Longword;

//---------------------------------------------------------------------------
// Pixel32toX()
//
// Converts a pixel from A8R8G8B8 (32-bit) format to an arbitrary format.
//---------------------------------------------------------------------------
procedure Pixel32toX(Source: Longword; Dest: Pointer;
 DestFormat: TAsphyrePixelFormat);

//---------------------------------------------------------------------------
// PixelXto32Array()
//
// Converts an array of pixels from A8R8G8B8 (32-bit) format to an arbitrary
// format.
//---------------------------------------------------------------------------
procedure PixelXto32Array(Source, Dest: Pointer;
 SourceFormat: TAsphyrePixelFormat; Elements: Integer);

//---------------------------------------------------------------------------
// Pixel32toX()
//
// Converts an array of pixels from A8R8G8B8 (32-bit) format to an
// arbitrary format.
//---------------------------------------------------------------------------
procedure Pixel32toXArray(Source, Dest: Pointer;
 DestFormat: TAsphyrePixelFormat; Elements: Integer);

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 AsphyreFormatInfo;

//---------------------------------------------------------------------------
function PixelXto32(Source: Pointer;
 SourceFormat: TAsphyrePixelFormat): Longword;
var
 Bits : Integer;
 Value: Longword;
 Info : PFormatBitInfo;
 Mask : Longword;
begin
 Result:= 0;

 if (SourceFormat = apf_A16B16G16R16) then
  begin
   Value:= PLongword(Source)^;

   Result:=
    (((Value shl 16) shr 24) shl 16) or
    ((Value shr 24) shl 8);

   Value:= PLongword(Cardinal(Source) + 4)^;

   Result:= Result or
    ((Value shl 16) shr 24) or
    ((Value shr 24) shl 24);

   Exit;
  end;

 Bits:= AsphyrePixelFormatBits[SourceFormat];
 if (Bits < 8)or(Bits > 32) then Exit;

 Value:= 0;
 Move(Source^, Value, Bits div 8);

 case SourceFormat of
  apf_R8G8B8, apf_X8R8G8B8:
   Result:= Value or $FF000000;

  apf_A8R8G8B8:
   Result:= Value;

  apf_A8:
   Result:= Value shl 24;

  apf_L8:
   Result:= Value or (Value shl 8) or (Value shl 16) or $FF000000;

  apf_A8L8:
   begin
    Result:= Value and $FF;
    Result:= Result or (Result shl 8) or (Result shl 16) or
     ((Value shr 8) shl 24)
   end;

  apf_A4L4:
   begin
    Result:= ((Value and $0F) * 255) div 15;
    Result:= Result or (Result shl 8) or (Result shl 16) or
     ((((Value shr 4) * 255) div 15) shl 24);
   end;

  apf_L16:
   begin
    Result:= Value shr 8;
    Result:= Result or (Result shl 8) or (Result shl 16) or $FF000000;
   end;

  apf_A8B8G8R8:
   Result:= (Value and $FF00FF00) or ((Value shr 16) and $FF) or
    ((Value and $FF) shl 16);

  apf_X8B8G8R8:
   Result:= (Value and $0000FF00) or ((Value shr 16) and $FF) or
    ((Value and $FF) shl 16) or ($FF000000);

  apf_A6L2:
   begin
    Result:= ((Value and 3) * 255) div 3;
    Result:= Result or (Result shl 8) or (Result shl 16) or
     ((((Value shr 2) * 255) div 63) shl 24);
   end;

  else
   begin
    Info:= @FormatInfo[SourceFormat];

    // -> Blue Component
    if (Info^.bNo > 0) then
     begin
      Mask:= (1 shl Info^.bNo) - 1;
      Result:= (((Value shr Info^.bAt) and Mask) * 255) div Mask;
     end else Result:= 255;

    // -> Green Component
    if (Info^.gNo > 0) then
     begin
      Mask:= (1 shl Info^.gNo) - 1;
      Result:= Result or
       (((((Value shr Info^.gAt) and Mask) * 255) div Mask) shl 8);
     end else Result:= Result or $FF00;

    // -> Red Component
    if (Info^.rNo > 0) then
     begin
      Mask:= (1 shl Info^.rNo) - 1;
      Result:= Result or
       (((((Value shr Info^.rAt) and Mask) * 255) div Mask) shl 16);
     end else Result:= Result or $FF0000;

    // -> Alpha Component
    if (Info^.aNo > 0) then
     begin
      Mask:= (1 shl Info^.aNo) - 1;
      Result:= Result or
       (((((Value shr Info^.aAt) and Mask) * 255) div Mask) shl 24);
     end else Result:= Result or $FF000000;
   end;
 end;
end;

//---------------------------------------------------------------------------
function ComputeLuminance(Value: Longword): Single;
begin
 Result:= ((Value and $FF) * 0.11 + ((Value shr 8) and $FF) * 0.59 +
  ((Value shr 16) and $FF) * 0.3) / 255.0;
end;

//---------------------------------------------------------------------------
procedure Pixel32toX(Source: Longword; Dest: Pointer;
 DestFormat: TAsphyrePixelFormat);
var
 Bits : Integer;
 Value: Longword;
 Info : PFormatBitInfo;
 Mask : Longword;
begin
 if (DestFormat = apf_A16B16G16R16) then
  begin
   PLongword(Dest)^:=
    (((((Source shl 16) shr 24) * $FFFF) div $FF) shl 16) or
    ((((Source shl 8) shr 24) * $FFFF) div $FF);

   PLongword(Cardinal(Dest) + 4)^:=
    ((((Source shl 24) shr 24) * $FFFF) div $FF) or
    ((((Source shr 24) * $FFFF) div $FF) shl 16);

   Exit;
  end;

 Bits:= AsphyrePixelFormatBits[DestFormat];
 if (Bits < 8)or(Bits > 32) then Exit;

 Value:= 0;

 case DestFormat of
  apf_R8G8B8, apf_X8R8G8B8, apf_A8R8G8B8:
   Value:= Source;

  apf_A8:
   Value:= Source shr 24;

  apf_A8B8G8R8:
   Value:= (Source and $FF00FF00) or ((Source shr 16) and $FF) or
    ((Source and $FF) shl 16);

  apf_X8B8G8R8:
   Value:= (Source and $0000FF00) or ((Source shr 16) and $FF) or
    ((Source and $FF) shl 16);

  apf_L8:
   Value:= Round(ComputeLuminance(Source) * 255.0);

  apf_A8L8:
   Value:= ((Source shr 24) shl 8) or Round(ComputeLuminance(Source) * 255.0);

  apf_A4L4:
   Value:= ((Source shr 28) shl 4) or Round(ComputeLuminance(Source) * 15.0);

  apf_L16:
   Value:= Round(ComputeLuminance(Source) * 65535.0);

  apf_A6L2:
   Value:= ((Source shr 26) shl 2) or Round(ComputeLuminance(Source) * 3.0);

  else
   begin
    Info:= @FormatInfo[DestFormat];

    // -> Blue Component
    if (Info^.bNo > 0) then
     begin
      Mask:= (1 shl Info^.bNo) - 1;
      Value:= (((Source and $FF) * Mask) div 255) shl Info^.bAt;
     end;

    // -> Green Component
    if (Info^.gNo > 0) then
     begin
      Mask:= (1 shl Info^.gNo) - 1;
      Value:= Value or
       ((((Source shr 8) and $FF) * Mask) div 255) shl Info^.gAt;
     end;

    // -> Red Component
    if (Info^.rNo > 0) then
     begin
      Mask:= (1 shl Info^.rNo) - 1;
      Value:= Value or
       ((((Source shr 16) and $FF) * Mask) div 255) shl Info^.rAt;
     end;

    // -> Alpha Component
    if (Info^.aNo > 0) then
     begin
      Mask:= (1 shl Info^.aNo) - 1;
      Value:= Value or
       ((((Source shr 24) and $FF) * Mask) div 255) shl Info^.aAt;
     end;
   end;
 end;

 Move(Value, Dest^, Bits div 8);
end;

//---------------------------------------------------------------------------
procedure PixelXto32Array(Source, Dest: Pointer;
 SourceFormat: TAsphyrePixelFormat; Elements: Integer);
var
 Bits: Integer;
 SourcePx: Pointer;
 DestPx  : PLongword;
 i, BytesPerPixel: Integer;
begin
 Bits:= AsphyrePixelFormatBits[SourceFormat];
 if (Bits < 8) then Exit;

 BytesPerPixel:= Bits div 8;

 SourcePx:= Source;
 DestPx  := Dest;
 for i:= 0 to Elements - 1 do
  begin
   DestPx^:= PixelXto32(SourcePx, SourceFormat);

   Inc(Cardinal(SourcePx), BytesPerPixel);
   Inc(DestPx);
  end;
end;

//---------------------------------------------------------------------------
procedure Pixel32toXArray(Source, Dest: Pointer;
 DestFormat: TAsphyrePixelFormat; Elements: Integer);
var
 Bits: Integer;
 SourcePx: PLongword;
 DestPx  : Pointer;
 i, BytesPerPixel: Integer;
begin
 Bits:= AsphyrePixelFormatBits[DestFormat];
 if (Bits < 8) then Exit;

 BytesPerPixel:= Bits div 8;

 SourcePx:= Source;
 DestPx  := Dest;
 for i:= 0 to Elements - 1 do
  begin
   Pixel32toX(SourcePx^, DestPx, DestFormat);

   Inc(SourcePx);
   Inc(Cardinal(DestPx), BytesPerPixel);
  end;
end;

//---------------------------------------------------------------------------
end.
