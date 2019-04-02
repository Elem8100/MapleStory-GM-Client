unit OGLTextures;
//---------------------------------------------------------------------------
// OGLTextures.pas                                      Modified: 20-Feb-2009
// OpenGL textures                                               Version 1.02
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
// The Original Code is OGLTextures.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2007 - 2008,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 AsphyreGL, Types, SysUtils, Vectors2px, AsphyreTypes, AbstractTextures,
 SystemSurfaces;

//---------------------------------------------------------------------------
type
 TOGLLockableTexture = class(TAsphyreLockableTexture)
 private
  FTexture: Cardinal;
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

  procedure Lock(const Rect: TRect; out Bits: Pointer;
   out Pitch: Integer); override;
  procedure Unlock(); override;

  procedure Bind(Stage: Integer); override;

  constructor Create(); override;
  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
 TOGLRenderTargetTexture = class(TAsphyreRenderTargetTexture)
 private
  FTexture: Cardinal;
  FFrameBuffer: Cardinal;
  FDepthBuffer: Cardinal;

  FInternalFormat: TAsphyrePixelFormat;

  function CreateInternalTexture(): Boolean;
  function CreateFrameObjects(): Boolean;
  function CreateTextureInstance(): Boolean;
  procedure DestroyTextureInstance();
 protected
  procedure UpdateSize(); override;

  function CreateTexture(): Boolean; override;
  procedure DestroyTexture(); override;
 public
  property Texture: Cardinal read FTexture;
  property InternalFormat: TAsphyrePixelFormat read FInternalFormat;

  property FrameBuffer: Cardinal read FFrameBuffer;
  property DepthBuffer: Cardinal read FDepthBuffer;

  procedure Bind(Stage: Integer); override;

  function BeginDrawTo(): Boolean; override;
  procedure EndDrawTo(); override;

  constructor Create(); override;
 end;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 AsphyreErrors, AsphyreFormats;

//---------------------------------------------------------------------------
function FindApproxFormat(Suggest: TAsphyrePixelFormat): TAsphyrePixelFormat;
var
 Formats: TAsphyreFormatList;
begin
 Formats:= TAsphyreFormatList.Create();

 Formats.Insert(apf_A8);
 Formats.Insert(apf_A8R8G8B8);
 Formats.Insert(apf_X8R8G8B8);
 Formats.Insert(apf_X1R5G5B5);
 Formats.Insert(apf_A1R5G5B5);
 Formats.Insert(apf_A4R4G4B4);
 Formats.Insert(apf_X4R4G4B4);
 Formats.Insert(apf_R3G3B2);
 Formats.Insert(apf_A2R10G10B10);
 Formats.Insert(apf_A16B16G16R16);
 Formats.Insert(apf_L8);
 Formats.Insert(apf_A8L8);
 Formats.Insert(apf_A4L4);
 Formats.Insert(apf_L16);

 Result:= FindClosestFormat(Suggest, Formats);

 FreeAndNil(Formats);
end;

//---------------------------------------------------------------------------
function FormatToOpenGL(AFormat: TAsphyrePixelFormat): Integer;
begin
 Result:= GL_RGBA;

 case AFormat of
  apf_R8G8B8  : Result:= GL_RGB8;
  apf_A8R8G8B8: Result:= GL_RGBA8;
  apf_X8R8G8B8: Result:= GL_RGB8;
  apf_X1R5G5B5: Result:= GL_RGB5;
  apf_A1R5G5B5: Result:= GL_RGB5_A1;
  apf_A4R4G4B4: Result:= GL_RGBA4;
  apf_A8      : Result:= GL_ALPHA8;
  apf_X4R4G4B4: Result:= GL_RGB4;
  apf_A2R10G10B10 : Result:= GL_RGB10_A2;
  apf_A16B16G16R16: Result:= GL_RGBA16;
  apf_L8      : Result:= GL_LUMINANCE8;
  apf_A8L8    : Result:= GL_LUMINANCE8_ALPHA8;
  apf_A4L4    : Result:= GL_LUMINANCE4_ALPHA4;
  apf_L16     : Result:= GL_LUMINANCE16;
 end;
end;

//---------------------------------------------------------------------------
constructor TOGLLockableTexture.Create();
begin
 inherited;

 FTexture:= 0;
 LockSurf:= TSystemSurface.Create();
 LockPos := InfPoint2px;
 LockSize:= ZeroPoint2px;

 FInternalFormat:= apf_Unknown;
end;

//---------------------------------------------------------------------------
destructor TOGLLockableTexture.Destroy();
begin
 if (LockSurf <> nil) then FreeAndNil(LockSurf);

 inherited;
end;

//---------------------------------------------------------------------------
function TOGLLockableTexture.CreateTextureInstance(): Boolean;
begin
 glEnable(GL_TEXTURE_2D);
 glGenTextures(1, @FTexture);
 glBindTexture(GL_TEXTURE_2D, FTexture);

 glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
 glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

 if (GL_VERSION_1_4) then
  begin
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
  end else
  begin
   glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
   glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  end;

 glTexImage2D(GL_TEXTURE_2D, 0, FormatToOpenGL(FInternalFormat), Width, Height,
  0, GL_BGRA, GL_UNSIGNED_BYTE, nil);

 Result:= (glGetError() = GL_NO_ERROR)and(FTexture <> 0);

 if (not Result) then
  Errors.Insert(errCannotCreateTexture, Self, ClassName, 'CreateTexture');

 glBindTexture(GL_TEXTURE_2D, 0);
 glDisable(GL_TEXTURE_2D);
end;

//---------------------------------------------------------------------------
procedure TOGLLockableTexture.DestroyTextureInstance();
begin
 glBindTexture(GL_TEXTURE_2D, 0);
 glDisable(GL_TEXTURE_2D);

 glDeleteTextures(1, @FTexture);
end;

//---------------------------------------------------------------------------
function TOGLLockableTexture.CreateTexture(): Boolean;
begin
 FInternalFormat:= FindApproxFormat(FFormat);
 FFormat:= apf_A8R8G8B8;

 Result:= CreateTextureInstance();
end;

//---------------------------------------------------------------------------
procedure TOGLLockableTexture.DestroyTexture();
begin
 DestroyTextureInstance();
end;

//---------------------------------------------------------------------------
procedure TOGLLockableTexture.UpdateSize();
begin
 DestroyTextureInstance();

 if (not CreateTextureInstance()) then
  Errors.Insert(errChangeTextureSize, Self, ClassName, 'UpdateSize');
end;

//---------------------------------------------------------------------------
procedure TOGLLockableTexture.Bind(Stage: Integer);
begin
 glBindTexture(GL_TEXTURE_2D, FTexture);
end;

//---------------------------------------------------------------------------
procedure TOGLLockableTexture.Lock(const Rect: TRect; out Bits: Pointer;
 out Pitch: Integer);
var
 TexWidth, TexHeight: GLint;
 TempSurf: TSystemSurface;
begin
 // (1) Determine locking area.
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

 // (2) Bind the texture.
 glBindTexture(GL_TEXTURE_2D, FTexture);
 glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

 // (3) Retreive texture pixels.
 glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, @TexWidth);
 glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, @TexHeight);

 TempSurf:= TSystemSurface.Create();
 TempSurf.SetSize(TexWidth, TexHeight);

 glGetTexImage(GL_TEXTURE_2D, 0, GL_BGRA, GL_UNSIGNED_BYTE, TempSurf.Bits);

 if (glGetError() <> GL_NO_ERROR) then
  begin
   Errors.Insert(errDownloadTexturePixels, Self, ClassName, 'Lock');
   FreeAndNil(TempSurf);
   Exit;
  end;

 // (4) Copy the locked area to working surface.
 LockSurf.SetSize(LockSize.x, LockSize.y);
 LockSurf.Clear(0);

 LockSurf.CopyRect(Point2px(0, 0), TempSurf,
  Bounds(LockPos.x, LockPos.y, LockSize.x, LockSize.y));

 // (5) Release temporary memory.
 FreeAndNil(TempSurf);

 Bits := LockSurf.Bits;
 Pitch:= LockSurf.Pitch;

 glBindTexture(GL_TEXTURE_2D, 0);
end;

//---------------------------------------------------------------------------
procedure TOGLLockableTexture.Unlock();
begin
 if (FTexture = 0)or(LockPos = InfPoint2px)or(LockSize = ZeroPoint2px) then Exit;

 glBindTexture(GL_TEXTURE_2D, FTexture);
 glTexSubImage2D(GL_TEXTURE_2D, 0, LockPos.x, LockPos.y, LockSize.x,
  LockSize.y, GL_BGRA, GL_UNSIGNED_BYTE, LockSurf.Bits);

 if (glGetError() <> GL_NO_ERROR) then
   Errors.Insert(errUploadTexturePixels, Self, ClassName, 'Unlock');

 glBindTexture(GL_TEXTURE_2D, 0);
end;

//---------------------------------------------------------------------------
constructor TOGLRenderTargetTexture.Create();
begin
 inherited;

 FTexture:= 0;
 FFrameBuffer:= 0;
 FDepthBuffer:= 0;
end;

//---------------------------------------------------------------------------
function TOGLRenderTargetTexture.CreateInternalTexture(): Boolean;
begin
 glEnable(GL_TEXTURE_2D);
 glGenTextures(1, @FTexture);
 glBindTexture(GL_TEXTURE_2D, FTexture);

 glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
 glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);

 if (GL_VERSION_1_4) then
  begin
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
  end else
  begin
   glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
   glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  end;

 glTexImage2D(GL_TEXTURE_2D, 0, FormatToOpenGL(FInternalFormat), Width, Height,
  0, GL_BGRA, GL_UNSIGNED_BYTE, nil);

 Result:= (glGetError() = GL_NO_ERROR)and(FTexture <> 0);

 if (not Result) then
  Errors.Insert(errCannotCreateTexture, Self, ClassName, 'CreateTexture');

 if (Result) then glBindTexture(GL_TEXTURE_2D, 0);
end;

//---------------------------------------------------------------------------
function TOGLRenderTargetTexture.CreateFrameObjects(): Boolean;
begin
 // (1) Create Frame Buffer object.
 glGenFramebuffersEXT(1, @FFrameBuffer);
 glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, FFrameBuffer);

 Result:= (glGetError() = GL_NO_ERROR)and(FFrameBuffer <> 0);

 if (not Result) then
  Errors.Insert(errCreateFrameBuffer, Self, ClassName, 'CreateFrameBuffer');

 // (2) Optionally create and attach Depth-Stencil buffer.
 if (DepthStencil) then
  begin
   // -> Create Render Buffer object.
   glGenRenderbuffersEXT(1, @FDepthBuffer);
   glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, FDepthBuffer);
   glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT, GL_DEPTH_COMPONENT,
    Width, Height);
   glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, 0);

   Result:= (glGetError() = GL_NO_ERROR)and(FDepthBuffer <> 0);
   if (not Result) then
    Errors.Insert(errCreateRenderBuffer, Self, ClassName, 'CreateFrameBuffer');

   // -> Bind Render Buffer object to the Frame Buffer.
   glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT,
    GL_RENDERBUFFER_EXT, FDepthBuffer);

   Result:= (glGetError() = GL_NO_ERROR);
   if (not Result) then
    Errors.Insert(errBindRenderBuffer, Self, ClassName, 'CreateFrameBuffer');
  end;

 // -> Bind previously created texture to Frame Buffer object.
 glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT,
  GL_TEXTURE_2D, FTexture, 0);

 Result:= (glGetError() = GL_NO_ERROR);

 if (not Result) then
  Errors.Insert(errCannotSelectTexture, Self, ClassName, 'CreateFrameBuffer');

  // -> Check the completeness of the operation.
 if (Result) then
  begin
   Result:=
    glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT) =
    GL_FRAMEBUFFER_COMPLETE_EXT;

   if (not Result) then
    Errors.Insert(errFrameBufferIncomplete, Self, ClassName, 'CreateFrameBuffer');
  end;

 glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
end;

//---------------------------------------------------------------------------
function TOGLRenderTargetTexture.CreateTextureInstance(): Boolean;
begin
 Result:= CreateInternalTexture();
 if (not Result) then Exit;

 Result:= CreateFrameObjects();
end;

//---------------------------------------------------------------------------
procedure TOGLRenderTargetTexture.DestroyTextureInstance();
begin
 glBindTexture(GL_TEXTURE_2D, 0);
 glDisable(GL_TEXTURE_2D);

 if (FDepthBuffer <> 0) then glDeleteRenderbuffersEXT(1, @FDepthBuffer);
 if (FFrameBuffer <> 0) then glDeleteFramebuffersEXT(1, @FFrameBuffer);
 if (FTexture <> 0) then glDeleteTextures(1, @FTexture);
end;

//---------------------------------------------------------------------------
function TOGLRenderTargetTexture.CreateTexture(): Boolean;
begin
 Result:= GL_EXT_framebuffer_object;
 if (not Result) then Exit;

 FInternalFormat:= FindApproxFormat(FFormat);
 FFormat:= apf_A8R8G8B8;

 Result:= CreateTextureInstance();
end;

//---------------------------------------------------------------------------
procedure TOGLRenderTargetTexture.DestroyTexture();
begin
 DestroyTextureInstance();
end;

//---------------------------------------------------------------------------
procedure TOGLRenderTargetTexture.Bind(Stage: Integer);
begin
 glBindTexture(GL_TEXTURE_2D, FTexture);
end;

//---------------------------------------------------------------------------
function TOGLRenderTargetTexture.BeginDrawTo(): Boolean;
begin
 glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, FFrameBuffer);
 glPushAttrib(GL_VIEWPORT_BIT);
 glViewport(0, 0, Width, Height);

 Result:= (glGetError() = GL_NO_ERROR);

 if (not Result) then
  Errors.Insert(errSetRenderTarget, Self, ClassName, 'BeginDrawTo');
end;

//---------------------------------------------------------------------------
procedure TOGLRenderTargetTexture.EndDrawTo();
begin
 glPopAttrib();
 glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);

 if (Mipmapping)and(GL_VERSION_1_4) then
  begin
   glBindTexture(GL_TEXTURE_2D, FTexture);
   glGenerateMipmapEXT(GL_TEXTURE_2D);
   glBindTexture(GL_TEXTURE_2D, 0);
  end;
end;

//---------------------------------------------------------------------------
procedure TOGLRenderTargetTexture.UpdateSize();
begin
 DestroyTextureInstance();

 if (not CreateTextureInstance()) then
  Errors.Insert(errChangeTextureSize, Self, ClassName, 'UpdateSize');
end;

//---------------------------------------------------------------------------
end.
