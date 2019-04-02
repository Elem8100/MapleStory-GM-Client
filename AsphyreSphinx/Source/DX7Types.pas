unit DX7Types;
//---------------------------------------------------------------------------
// DX7Types.pas                                         Modified: 11-Jun-2009
// Shared DirectX 7.0 types and variables                        Version 1.02
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
// The Original Code is DX7Types.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2007 - 2008,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 DirectDraw7, Direct3D7, DirectInput, SysUtils, AsphyreTypes;

//---------------------------------------------------------------------------
var
 DirectDraw: IDirectDraw7     = nil;
 Direct3D  : IDirect3D7       = nil;
 Device7   : IDirect3DDevice7 = nil;
 DInput7   : IDirectInput7    = nil;

//---------------------------------------------------------------------------
function ApproximateTextureFormat(Format: TAsphyrePixelFormat): TAsphyrePixelFormat;
procedure PixelFormatToDesc(Format: TAsphyrePixelFormat; Desc: PDDPixelFormat);

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 AsphyreFormats, AsphyreFormatInfo;

//---------------------------------------------------------------------------
procedure LearnBitMask(BitMask: Cardinal; out BitPos, BitSize: Integer);
var
 Pos: Integer;
begin
 Pos:= 0;

 while (Pos < 32)and(BitMask and (1 shl Pos) = 0) do Inc(Pos);

 if (Pos >= 32) then
  begin
   BitPos := -1;
   BitSize:= 0;
   Exit;
  end;

 BitPos := Pos;
 BitSize:= 0;

 while (Pos < 32)and(BitMask and (1 shl Pos) > 0) do
  begin
   Inc(Pos);
   Inc(BitSize);
  end;
end;

//---------------------------------------------------------------------------
function CreateBitMask(BitPos, BitSize: Integer): Cardinal;
var
 i: Integer;
begin
 Result:= 0;

 for i:= 0 to BitSize - 1 do
  Result:= Result or (1 shl (BitPos + i));
end;

//---------------------------------------------------------------------------
function EnumCallback(var PixelFmt: TDDPixelFormat;
 Context: Pointer): HResult; stdcall;
var
 Format: TAsphyrePixelFormat;
 Info: TPixelFormatInfo;
 BitPos, BitSize, BitCount, TotalBitCount: Integer;
begin
 Format:= apf_Unknown;
 ResetFormatInfo(Info);

 // -> Check AxRxGxBx formats
 if (PixelFmt.dwFlags and DDPF_RGB > 0) then
  begin // RGB format
   BitCount:= 0;

   // (1) Red Channel
   LearnBitMask(PixelFmt.dwRBitMask, BitPos, BitSize);
   if (BitPos <> -1)and(BitSize > 0) then
    begin
     AddChannel(Info, ctiR, csUnsigned, BitSize, BitPos);
     Inc(BitCount, BitSize);
    end;

   // (2) Green Channel
   LearnBitMask(PixelFmt.dwGBitMask, BitPos, BitSize);
   if (BitPos <> -1)and(BitSize > 0) then
    begin
     AddChannel(Info, ctiG, csUnsigned, BitSize, BitPos);
     Inc(BitCount, BitSize);
    end;

   // (3) Blue Channel
   LearnBitMask(PixelFmt.dwBBitMask, BitPos, BitSize);
   if (BitPos <> -1)and(BitSize > 0) then
    begin
     AddChannel(Info, ctiB, csUnsigned, BitSize, BitPos);
     Inc(BitCount, BitSize);
    end;

   // (4) Alpha Channel
   LearnBitMask(PixelFmt.dwRGBAlphaBitMask, BitPos, BitSize);

   if (BitPos <> -1)and(BitSize > 0) then
    begin
     AddChannel(Info, ctiA, csUnsigned, BitSize, BitPos);
     Inc(BitCount, BitSize);
    end;

   // (5) X Channel (unused bits)
   TotalBitCount:= PixelFmt.dwRGBBitCount;

   if (TotalBitCount > BitCount) then
    AddChannel(Info, ctiX, csUnsigned, TotalBitCount - BitCount, BitCount);
  end;

 // -> Check LxAx formats
 if (PixelFmt.dwFlags and DDPF_LUMINANCE > 0) then
  begin // Luminance format
   BitCount:= 0;

   // (1) Luminance Channel
   LearnBitMask(PixelFmt.dwLuminanceBitMask, BitPos, BitSize);
   if (BitPos <> -1)and(BitSize > 0) then
    begin
     AddChannel(Info, ctiL, csUnsigned, BitSize, BitPos);
     Inc(BitCount, BitSize);
    end;

   // (2) Alpha Channel
   LearnBitMask(PixelFmt.dwLuminanceAlphaBitMask, BitPos, BitSize);
   if (BitPos <> -1)and(BitSize > 0) then
    begin
     AddChannel(Info, ctiA, csUnsigned, BitSize, BitPos);
     Inc(BitCount, BitSize);
    end;

   // (3) X Channel (unused bits)
   TotalBitCount:= PixelFmt.dwLuminanceBitCount;

   if (TotalBitCount > BitCount) then
    AddChannel(Info, ctiX, csUnsigned, TotalBitCount - BitCount, BitCount);
  end;

 // -> Check Bump-Mapping formats
 if (PixelFmt.dwFlags and DDPF_BUMPDUDV > 0) then
  begin // Bump-mapping format
   BitCount:= 0;

   // (1) Luminance Channel
   LearnBitMask(PixelFmt.dwBumpLuminanceBitMask, BitPos, BitSize);
   if (BitPos <> -1)and(BitSize > 0) then
    begin
     AddChannel(Info, ctiL, csUnsigned, BitSize, BitPos);
     Inc(BitCount, BitSize);
    end;

   // (2) U-Channel
   LearnBitMask(PixelFmt.dwBumpDuBitMask, BitPos, BitSize);
   if (BitPos <> -1)and(BitSize > 0) then
    begin
     AddChannel(Info, ctiU, csSigned, BitSize, BitPos);
     Inc(BitCount, BitSize);
    end;

   // (3) V-Channel
   LearnBitMask(PixelFmt.dwBumpDvBitMask, BitPos, BitSize);
   if (BitPos <> -1)and(BitSize > 0) then
    begin
     AddChannel(Info, ctiV, csSigned, BitSize, BitPos);
     Inc(BitCount, BitSize);
    end;

   // (4) X Channel (unused bits)
   TotalBitCount:= PixelFmt.dwBumpBitCount;

   if (TotalBitCount > BitCount) then
    AddChannel(Info, ctiX, csUnsigned, TotalBitCount - BitCount, BitCount);
  end;

 // -> Alpha-only format
 if (PixelFmt.dwFlags and DDPF_ALPHA > 0) then
  AddChannel(Info, ctiA, csUnsigned, PixelFmt.dwAlphaBitDepth, 0);

 // -> FOURCC Formats
 if (PixelFmt.dwFlags and DDPF_FOURCC > 0) then
  begin
   if (PixelFmt.dwFourCC = MAKEFOURCC('D', 'X', 'T', '1')) then
    Format:= apf_DXT1;

   if (PixelFmt.dwFourCC = MAKEFOURCC('D', 'X', 'T', '2')) then
    Format:= apf_DXT2;

   if (PixelFmt.dwFourCC = MAKEFOURCC('D', 'X', 'T', '3')) then
    Format:= apf_DXT3;

   if (PixelFmt.dwFourCC = MAKEFOURCC('D', 'X', 'T', '4')) then
    Format:= apf_DXT4;

   if (PixelFmt.dwFourCC = MAKEFOURCC('D', 'X', 'T', '5')) then
    Format:= apf_DXT5;
  end;

 // Find an existing pixel format matching the current description.
 if (Format = apf_Unknown) then Format:= InfoToPixelFormat(Info);
 if (Format <> apf_Unknown) then TAsphyreFormatList(Context).Insert(Format);

 Result:= DDENUMRET_OK;
end;

//---------------------------------------------------------------------------
function ApproximateTextureFormat(Format: TAsphyrePixelFormat): TAsphyrePixelFormat;
var
 Supported: TAsphyreFormatList;
begin
 Result:= apf_Unknown;
 if (Device7 = nil) then Exit;

 Supported:= TAsphyreFormatList.Create();
 
 {$ifdef fpc}
 Device7.EnumTextureFormats(@EnumCallback, Supported);
 {$else}
 Device7.EnumTextureFormats(EnumCallback, Supported);
 {$endif}

 Result:= FindClosestFormat(Format, Supported);

 FreeAndNil(Supported);
end;

//---------------------------------------------------------------------------
procedure PixelFormatToDesc(Format: TAsphyrePixelFormat; Desc: PDDPixelFormat);
var
 Info: TPixelFormatInfo;
 Category: TPixelFormatCategory;
 RedAt, GreenAt, BlueAt, AlphaAt, VoidAt, LumAt, uBumpAt, vBumpAt: Integer;
begin
 FillChar(Desc^, SizeOf(TDDPixelFormat), 0);

 Desc^.dwSize:= SizeOf(TDDPixelFormat);

 if (Format = apf_A8) then
  begin
   Desc^.dwFlags:= DDPF_ALPHA;
   Desc^.dwAlphaBitDepth:= 8;
   Exit;
  end;

 Info:= GetPixelFormatInfo(Format);
 Category:= PixelFormatCategory[Format];

 case Category of
  pfc_RGB:
   begin
    RedAt  := FindChannelAt(ctiR, Info);
    GreenAt:= FindChannelAt(ctiG, Info);
    BlueAt := FindChannelAt(ctiB, Info);
    AlphaAt:= FindChannelAt(ctiA, Info);
    VoidAt := FindChannelAt(ctiX, Info);

    Desc^.dwFlags:= DDPF_RGB;

    if (AlphaAt <> -1)and(Info.Channels[AlphaAt].Bits > 0) then
     Desc^.dwFlags:= Desc^.dwFlags or DDPF_ALPHAPIXELS;

    if (RedAt <> -1) then
     with Info.Channels[RedAt] do
      begin
       Inc(Desc^.dwRGBBitCount, Bits);
       Desc^.dwRBitMask:= CreateBitMask(Pos, Bits);
      end;

    if (GreenAt <> -1) then
     with Info.Channels[GreenAt] do
      begin
       Inc(Desc^.dwRGBBitCount, Bits);
       Desc^.dwGBitMask:= CreateBitMask(Pos, Bits);
      end;

    if (BlueAt <> -1) then
     with Info.Channels[BlueAt] do
      begin
       Inc(Desc^.dwRGBBitCount, Bits);
       Desc^.dwBBitMask:= CreateBitMask(Pos, Bits);
      end;

    if (AlphaAt <> -1) then
     with Info.Channels[AlphaAt] do
      begin
       Inc(Desc^.dwRGBBitCount, Bits);
       Desc^.dwRGBAlphaBitMask:= CreateBitMask(Pos, Bits);
      end;

    if (VoidAt <> -1) then
     with Info.Channels[VoidAt] do
      Inc(Desc^.dwRGBBitCount, Bits);
   end;

  pfc_Luminance:
   begin
    LumAt  := FindChannelAt(ctiL, Info);
    AlphaAt:= FindChannelAt(ctiA, Info);

    Desc^.dwFlags:= DDPF_LUMINANCE;

    if (AlphaAt <> -1)and(Info.Channels[AlphaAt].Bits > 0) then
     Desc^.dwFlags:= Desc^.dwFlags or DDPF_ALPHAPIXELS;

    if (LumAt <> -1) then
     with Info.Channels[LumAt] do
      begin
       Inc(Desc^.dwLuminanceBitCount, Bits);
       Desc^.dwLuminanceBitMask:= CreateBitMask(Pos, Bits);
      end;

    if (AlphaAt <> -1) then
     with Info.Channels[AlphaAt] do
      begin
       Inc(Desc^.dwLuminanceBitCount, Bits);
       Desc^.dwLuminanceAlphaBitMask:= CreateBitMask(Pos, Bits);
      end;
   end;

  pfc_BumpMap:
   begin
    if (FindChannelAt(ctiQ, Info) <> -1)or
     (FindChannelAt(ctiW, Info) <> -1)or
     (FindChannelAt(ctiA, Info) <> -1) then Exit;

    vBumpAt:= FindChannelAt(ctiU, Info);
    uBumpAt:= FindChannelAt(ctiV, Info);
    LumAt  := FindChannelAt(ctiL, Info);

    Desc^.dwFlags:= DDPF_BUMPDUDV;

    if (LumAt <> -1)and(Info.Channels[LumAt].Bits > 0) then
     Desc^.dwFlags:= Desc^.dwFlags or DDPF_BUMPLUMINANCE;

    if (vBumpAt <> -1) then
     with Info.Channels[vBumpAt] do
      begin
       Inc(Desc^.dwBumpBitCount, Bits);
       Desc^.dwBumpDvBitMask:= CreateBitMask(Pos, Bits);
      end;

    if (uBumpAt <> -1) then
     with Info.Channels[uBumpAt] do
      begin
       Inc(Desc^.dwBumpBitCount, Bits);
       Desc^.dwBumpDuBitMask:= CreateBitMask(Pos, Bits);
      end;

    if (LumAt <> -1) then
     with Info.Channels[LumAt] do
      begin
       Inc(Desc^.dwBumpBitCount, Bits);
       Desc^.dwBumpLuminanceBitMask:= CreateBitMask(Pos, Bits);
      end;
   end;
 end;
end;

//---------------------------------------------------------------------------
initialization

//---------------------------------------------------------------------------
finalization
 if (DInput7 <> nil) then DInput7:= nil;

//---------------------------------------------------------------------------
end.
