unit AsphyreVampyre;
//---------------------------------------------------------------------------
// AsphyreVampyre.pas                                   Modified: 23-Dec-2008
// Vampyre Imaging Library adapter for Asphyre                    Version 1.0
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
// The Original Code is AsphyreVampyre.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2008,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Classes, SysUtils, SystemSurfaces, AsphyreBitmaps;

//---------------------------------------------------------------------------
type
 TAsphyreVampyreBitmap = class(TAsphyreCustomBitmap)
 private
 public
  function LoadFromStream(const Extension: ShortString; Stream: TStream;
   Dest: TSystemSurface): Boolean; override;

  function SaveToStream(const Extension: ShortString; Stream: TStream;
   Source: TSystemSurface): Boolean; override;

  constructor Create();
 end;

//---------------------------------------------------------------------------
var
 VampyreBitmap: TAsphyreVampyreBitmap = nil;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 AsphyreTypes, ImagingTypes, ImagingClasses, ImagingUtility, AsphyreErrors,
 AsphyreConv;

//---------------------------------------------------------------------------
function VampyreToFormat(Format: TImageFormat): TAsphyrePixelFormat;
begin
 Result:= apf_Unknown;

 case Format of
  ifGray8:
   Result:= apf_L8;

  ifA8Gray8:
   Result:= apf_A8L8;

  ifGray16:
   Result:= apf_L16;

  ifR3G3B2:
   Result:= apf_R3G3B2;

  ifR5G6B5:
   Result:= apf_R5G6B5;

  ifA1R5G5B5:
   Result:= apf_A1R5G5B5;

  ifA4R4G4B4:
   Result:= apf_A4R4G4B4;

  ifX1R5G5B5:
   Result:= apf_X1R5G5B5;

  ifX4R4G4B4:
   Result:= apf_X4R4G4B4;

  ifR8G8B8:
   Result:= apf_R8G8B8;

  ifA8R8G8B8:
   Result:= apf_A8R8G8B8;

  ifX8R8G8B8:
   Result:= apf_X8R8G8B8;

  ifA16B16G16R16:
   Result:= apf_A16B16G16R16;

  ifR32F:
   Result:= apf_R32F;

  ifA32B32G32R32F:
   Result:= apf_A32B32G32R32F;

  ifR16F:
   Result:= apf_R16F;

  ifA16B16G16R16F:
   Result:= apf_A16B16G16R16F;

  ifDXT1:
   Result:= apf_DXT1;

  ifDXT3:
   Result:= apf_DXT3;

  ifDXT5:
   Result:= apf_DXT5;
 end;
end;

//---------------------------------------------------------------------------
function ExtNoDot(const Extension: ShortString): ShortString;
var
 DotAt: Integer;
begin
 Result:= Extension;

 DotAt:= Pos('.', Result);
 if (DotAt <> 0) then Delete(Result, 1, DotAt);
end;

//---------------------------------------------------------------------------
constructor TAsphyreVampyreBitmap.Create();
begin
 inherited;

 FDesc:= 'Vammpyre Imaging Library';
end;

//---------------------------------------------------------------------------
function TAsphyreVampyreBitmap.LoadFromStream(const Extension: ShortString;
 Stream: TStream; Dest: TSystemSurface): Boolean;
var
 Image: TSingleImage;
 InFmt: TAsphyrePixelFormat;
 i: Integer;
begin
 try
  Image:= TSingleImage.CreateFromStream(Stream);
 except
  Result:= False;
  Exit;
 end;

 InFmt:= VampyreToFormat(Image.Format);
 if (InFmt = apf_Unknown) then
  begin
   Errors.Insert(errUnsupportedFormat, Self, ClassName, 'LoadFromStream');
   Result:= False;
   Exit;
  end;

 Dest.SetSize(Image.Width, Image.Height);

 for i:= 0 to Image.Height - 1 do
  PixelXto32Array(Image.ScanLine[i], Dest.Scanline[i], InFmt, Image.Width);

 FreeAndNil(Image);
 Result:= True;
end;

//---------------------------------------------------------------------------
function TAsphyreVampyreBitmap.SaveToStream(const Extension: ShortString;
 Stream: TStream; Source: TSystemSurface): Boolean;
var
 Image: TSingleImage;
 i: Integer;
begin
 Result:= True;

 try
  Image:= TSingleImage.CreateFromParams(Source.Width, Source.Height, ifA8R8G8B8);
 except
  Result:= False;
  Exit;
 end;

 for i:= 0 to Source.Height - 1 do
  Move(Source.Scanline[i]^, Image.Scanline[i]^, Source.Width * 4);

 try
  Image.SaveToStream(ExtNoDot(Extension), Stream);
 except
  Errors.Insert(errSaveFile, Self, ClassName, 'SaveToStream');
  Result:= False;
 end;

 FreeAndNil(Image);
end;

//---------------------------------------------------------------------------
initialization
 VampyreBitmap:= TAsphyreVampyreBitmap.Create();

 BitmapManager.RegisterExt('.png',  VampyreBitmap);
 BitmapManager.RegisterExt('.tga',  VampyreBitmap);
 BitmapManager.RegisterExt('.jpg',  VampyreBitmap);
 BitmapManager.RegisterExt('.jpeg', VampyreBitmap);
 BitmapManager.RegisterExt('.bmp',  VampyreBitmap);
 BitmapManager.RegisterExt('.gif',  VampyreBitmap);
 BitmapManager.RegisterExt('.dds',  VampyreBitmap);
 BitmapManager.RegisterExt('.psd',  VampyreBitmap);

//---------------------------------------------------------------------------
finalization
 BitmapManager.UnregisterExt('.psd');
 BitmapManager.UnregisterExt('.dds');
 BitmapManager.UnregisterExt('.gif');
 BitmapManager.UnregisterExt('.bmp');
 BitmapManager.UnregisterExt('.jpeg');
 BitmapManager.UnregisterExt('.jpg');
 BitmapManager.UnregisterExt('.tga');
 BitmapManager.UnregisterExt('.png');

 FreeAndNil(VampyreBitmap);

//---------------------------------------------------------------------------
end.
