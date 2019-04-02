unit AsphyreLegacyFormats;
//---------------------------------------------------------------------------
// AsphyreLegacyFormats.pas                             Modified: 13-Dec-2008
// Texture formats used in older versions of Asphyre              Version 1.0
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
// The Original Code is AsphyreLegacyFormats.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2007 - 2008,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------

{$ifdef fpc}{$packenum 1}{$endif}

//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 AsphyreTypes;

//---------------------------------------------------------------------------
type
 PColorFormat = ^TColorFormat;
 TColorFormat = (COLOR_R3G3B2, COLOR_R5G6B5, COLOR_X8R8G8B8, COLOR_X1R5G5B5,
  COLOR_X4R4G4B4, COLOR_A8R8G8B8, COLOR_A1R5G5B5, COLOR_A4R4G4B4,
  COLOR_A8R3G3B2, COLOR_A2R2G2B2, COLOR_UNKNOWN);

//---------------------------------------------------------------------------
function LegacyToPixelFormat(ColorFormat: TColorFormat): TAsphyrePixelFormat;
function PixelFormatToLegacy(Format: TAsphyrePixelFormat): TColorFormat;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
function LegacyToPixelFormat(ColorFormat: TColorFormat): TAsphyrePixelFormat;
begin
 case ColorFormat of
  COLOR_R3G3B2:
   Result:= apf_R3G3B2;

  COLOR_R5G6B5:
   Result:= apf_R5G6B5;

  COLOR_X8R8G8B8:
   Result:= apf_X8R8G8B8;

  COLOR_X1R5G5B5:
   Result:= apf_X1R5G5B5;

  COLOR_X4R4G4B4:
   Result:= apf_X4R4G4B4;

  COLOR_A8R8G8B8:
   Result:= apf_A8R8G8B8;

  COLOR_A1R5G5B5:
   Result:= apf_A1R5G5B5;

  COLOR_A4R4G4B4:
   Result:= apf_A4R4G4B4;

  COLOR_A8R3G3B2:
   Result:= apf_A8R3G3B2;

  else Result:= apf_Unknown;
 end;
end;

//---------------------------------------------------------------------------
function PixelFormatToLegacy(Format: TAsphyrePixelFormat): TColorFormat;
begin
 case Format of
  apf_R3G3B2:
   Result:= COLOR_R3G3B2;

  apf_R5G6B5:
   Result:= COLOR_R5G6B5;

  apf_X8R8G8B8:
   Result:= COLOR_X8R8G8B8;

  apf_X1R5G5B5:
   Result:= COLOR_X1R5G5B5;

  apf_X4R4G4B4:
   Result:= COLOR_X4R4G4B4;

  apf_A8R8G8B8:
   Result:= COLOR_A8R8G8B8;

  apf_A1R5G5B5:
   Result:= COLOR_A1R5G5B5;

  apf_A4R4G4B4:
   Result:= COLOR_A4R4G4B4;

  apf_A8R3G3B2:
   Result:= COLOR_A8R3G3B2;

  else Result:= COLOR_UNKNOWN;
 end;
end;

//---------------------------------------------------------------------------
end.
