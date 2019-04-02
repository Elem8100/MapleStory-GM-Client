unit GLESTextures;
//---------------------------------------------------------------------------
// GLESTextures.pas                                     Modified: 12-Nov-2010
// OpenGL ES textures                                             Version 1.0
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
// The Original Code is GLESTextures.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2007 - 2010,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Types, SysUtils, Vectors2px, AsphyreTypes, AbstractTextures, SystemSurfaces;

//---------------------------------------------------------------------------
type
 TGLESLockableTexture = class(TAsphyreLockableTexture)
 private
  FTexture: Cardinal;
  FSurface: TSystemSurface;
  LockSurf: TSystemSurface;
  LockPos, LockSize: TPoint2px;

  FInternalFormat: TAsphyrePixelFormat;

  function CreateTextureInstance(): Boolean;
  procedure DestroyTextureInstance();
 protected
  procedure UpdateSize(); override;

  function CreateTexture(): Boolean; override;
  procedure DestroyTexture(); override;
 public
  property Texture: Cardinal read FTexture;
  property InternalFormat: TAsphyrePixelFormat read FInternalFormat;

  property Surface: TSystemSurface read FSurface;

  procedure Lock(const Rect: TRect; out Bits: Pointer;
   out Pitch: Integer); override;
  procedure Unlock(); override;

  procedure Bind(Stage: Integer); override;
  procedure RefreshSurface();

  constructor Create(); override;
  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 AsphyreGLES, AsphyreErrors, AsphyreFormats;

//---------------------------------------------------------------------------
function FindApproxFormat(Suggest: TAsphyrePixelFormat): TAsphyrePixelFormat;
var
 Formats: TAsphyreFormatList;
begin
 Formats:= TAsphyreFormatList.Create();

 Formats.Insert(apf_A8);
 Formats.Insert(apf_X8R8G8B8);
 Formats.Insert(apf_A8R8G8B8);
 Formats.Insert(apf_L8);
 Formats.Insert(apf_A8L8);

 Result:= FindClosestFormat(Suggest, Formats);

 FreeAndNil(Formats);
end;

//---------------------------------------------------------------------------
function FormatToOpenGL(AFormat: TAsphyrePixelFormat): Integer;
begin
 Result:= GL_RGBA;

 case AFormat of
  apf_X8R8G8B8:
   Result:= GL_RGB;

  apf_A8:
   Result:= GL_ALPHA;

  apf_L8:
   Result:= GL_LUMINANCE;

  apf_A8L8:
   Result:= GL_LUMINANCE_ALPHA;
 end;
end;

//---------------------------------------------------------------------------
constructor TGLESLockableTexture.Create();
begin
 inherited;

 FTexture:= 0;

 FSurface := TSystemSurface.Create();
 LockSurf:= TSystemSurface.Create();
 LockPos := InfPoint2px;
 LockSize:= ZeroPoint2px;

 FInternalFormat:= apf_Unknown;
end;

//---------------------------------------------------------------------------
destructor TGLESLockableTexture.Destroy();
begin
 if (LockSurf <> nil) then FreeAndNil(LockSurf);
 if (FSurface <> nil) then FreeAndNil(FSurface);

 inherited;
end;

//---------------------------------------------------------------------------
function TGLESLockableTexture.CreateTextureInstance(): Boolean;
begin
 glEnable(GL_TEXTURE_2D);
 glGenTextures(1, @FTexture);
 glBindTexture(GL_TEXTURE_2D, FTexture);

 glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
 glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

 if (Mipmapping) then
  begin
   glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
   glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,
    GL_LINEAR_MIPMAP_LINEAR);
   glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE)
  end else
  begin
   glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
   glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
   glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_FALSE);
  end;

 FSurface.SetSize(Width, Height);
 FSurface.Clear(0);

 glTexImage2D(GL_TEXTURE_2D, 0, FormatToOpenGL(FInternalFormat), Width, Height,
  0, GL_RGBA, GL_UNSIGNED_BYTE, FSurface.Bits);

 Result:= (glGetError() = GL_NO_ERROR)and(FTexture <> 0);

 if (not Result) then
  Errors.Insert(errCannotCreateTexture, Self, ClassName, 'CreateTexture');

 glBindTexture(GL_TEXTURE_2D, 0);
 glDisable(GL_TEXTURE_2D);
end;

//---------------------------------------------------------------------------
procedure TGLESLockableTexture.DestroyTextureInstance();
begin
 glBindTexture(GL_TEXTURE_2D, 0);
 glDisable(GL_TEXTURE_2D);

 glDeleteTextures(1, @FTexture);
end;

//---------------------------------------------------------------------------
function TGLESLockableTexture.CreateTexture(): Boolean;
begin
 FInternalFormat:= FindApproxFormat(FFormat);
 FFormat:= apf_A8R8G8B8;

 Result:= CreateTextureInstance();
end;

//---------------------------------------------------------------------------
procedure TGLESLockableTexture.DestroyTexture();
begin
 DestroyTextureInstance();
end;

//---------------------------------------------------------------------------
procedure TGLESLockableTexture.UpdateSize();
begin
 DestroyTextureInstance();

 if (not CreateTextureInstance()) then
  Errors.Insert(errChangeTextureSize, Self, ClassName, 'UpdateSize');
end;

//---------------------------------------------------------------------------
procedure TGLESLockableTexture.Bind(Stage: Integer);
begin
 glBindTexture(GL_TEXTURE_2D, FTexture);
end;

//---------------------------------------------------------------------------
procedure TGLESLockableTexture.Lock(const Rect: TRect; out Bits: Pointer;
 out Pitch: Integer);
begin
 LockPos.x := Rect.Left;
 LockPos.y := Rect.Top;
 LockSize.x:= Rect.Right - Rect.Left;
 LockSize.y:= Rect.Bottom - Rect.Top;

 if (LockPos.x < 0)or(LockPos.y < 0)or(LockPos.x + LockSize.x > Width)or
  (LockPos.y + LockSize.y > Height)or(FTexture = 0) then
  begin
   Bits := nil;
   Pitch:= 0;
   Exit;
  end;

 LockSurf.SetSize(LockSize.x, LockSize.y);

 LockSurf.CopyRect(Point2px(0, 0), FSurface,
  Bounds(LockPos.x, LockPos.y, LockSize.x, LockSize.y));

 Bits := LockSurf.Bits;
 Pitch:= LockSurf.Pitch;
end;

//---------------------------------------------------------------------------
procedure TGLESLockableTexture.Unlock();
begin
 if (FTexture = 0)or(LockPos = InfPoint2px)or(LockSize = ZeroPoint2px) then Exit;

 FSurface.CopyRect(LockPos, LockSurf, Bounds(0, 0, LockSurf.Width,
  LockSurf.Height));

 glBindTexture(GL_TEXTURE_2D, FTexture);
 glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

 glTexSubImage2D(GL_TEXTURE_2D, 0, LockPos.x, LockPos.y, LockSize.x,
  LockSize.y, GL_RGBA, GL_UNSIGNED_BYTE, LockSurf.Bits);

 if (glGetError() <> GL_NO_ERROR) then
   Errors.Insert(errUploadTexturePixels, Self, ClassName, 'Unlock');

 glBindTexture(GL_TEXTURE_2D, 0);
end;

//---------------------------------------------------------------------------
procedure TGLESLockableTexture.RefreshSurface();
begin
 if (FTexture = 0) then Exit;

 glBindTexture(GL_TEXTURE_2D, FTexture);
 glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

 glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, Surface.Width, Surface.Height,
  GL_RGBA, GL_UNSIGNED_BYTE, Surface.Bits);

 if (glGetError() <> GL_NO_ERROR) then
   Errors.Insert(errUploadTexturePixels, Self, ClassName, 'RefreshSurface');

 glBindTexture(GL_TEXTURE_2D, 0);
end;

//---------------------------------------------------------------------------
end.
