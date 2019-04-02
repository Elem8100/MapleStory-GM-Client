unit CommonDef;
//---------------------------------------------------------------------------
// CommonDef.pas                                        Modified: 04-Jan-2007
// Asphyre eXtreme II Common Framework definitions                Version 1.0
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
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
{$ifdef fpc}
{$asmmode intel}
{$endif}

//---------------------------------------------------------------------------
function DisplaceRB(Color: Cardinal): Cardinal;
procedure LineConvMasked(Source, Dest: Pointer; Count, Tolerance: Integer;
 ColorMask: Cardinal);

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
type
 PRealColor = ^TRealColor;
 TRealColor = record
  r, g, b, a: Real;
 end;

//---------------------------------------------------------------------------
function DisplaceRB(Color: Cardinal): Cardinal;
{$ifdef fpc} assembler;{$endif}
asm { params: eax, edx, ecx }
 mov ecx, eax
 mov edx, eax
 and eax, 0FF00FF00h
 and edx, 0000000FFh
 shl edx, 16
 or eax, edx
 mov edx, ecx
 shr edx, 16
 and edx, 0000000FFh
 or eax, edx
end;

//---------------------------------------------------------------------------
function Color2Real(Color: Longword): TRealColor;
begin
 Result.r:= (Color and $FF) / 255.0;
 Result.g:= ((Color shr 8) and $FF) / 255.0;
 Result.b:= ((Color shr 16) and $FF) / 255.0;
 Result.a:= ((Color shr 24) and $FF) / 255.0;
end;

//---------------------------------------------------------------------------
function Real2Color(Pix: TRealColor): Longword;
begin
 Result:= Round(Pix.r * 255.0) + (Round(Pix.g * 255.0) shl 8) +
  (Round(Pix.b * 255.0) shl 16) + (Round(Pix.a * 255.0) shl 24);
end;

//---------------------------------------------------------------------------
function Linear2Sine(Alpha: Real): Real;
const
 PiHalf = Pi / 2.0;
begin
 Result:= (Sin((Alpha * Pi) - PiHalf) + 1.0) / 2.0;
end;

//---------------------------------------------------------------------------
procedure LineConvMasked(Source, Dest: Pointer; Count, Tolerance: Integer;
 ColorMask: Cardinal);
const
 Delta2Dist = 57.73502692;
 DeltaMin = 0.025;
var
 InPx, OutPx: PLongword;
 Color, cMask: TRealColor;
 i: Integer;
 Delta, DeltaMax: Real;
begin
 InPx:= Source;
 OutPx:= Dest;
 cMask:= Color2Real(DisplaceRB(ColorMask));

 DeltaMax:= (Abs(Tolerance) / Delta2Dist) + DeltaMin;

 for i:= 0 to Count - 1 do
  begin
   // retreive real color
   Color:= Color2Real(DisplaceRB(InPx^));

   // calculate the difference (in %)
   Delta:= Sqrt(Sqr(Color.r - cMask.r) + Sqr(Color.g - cMask.g) + Sqr(Color.b - cMask.b));

   // based on distance, find the specified alpha-channel
   Color.a:= 1.0;
   if (Delta <= DeltaMax) then
    Color.a:= Linear2Sine(Delta / DeltaMax);

   // write final pixel
   OutPx^:= DisplaceRB(Real2Color(Color));

   // advance in pixel list
   Inc(InPx);
   Inc(OutPx);
  end;
end;

//---------------------------------------------------------------------------
end.
