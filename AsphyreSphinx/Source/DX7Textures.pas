unit DX7Textures;
//---------------------------------------------------------------------------
// DX7Textures.pas                                      Modified: 13-Dec-2008
// Texture implementation using DirectX 7.0                      Version 1.02
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
// The Original Code is DX7Textures.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2007 - 2008,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Windows, DirectDraw7, Direct3D7, Types, Vectors2px, AsphyreTypes,
 AbstractTextures, SystemSurfaces;

//---------------------------------------------------------------------------
type
 TDX7LockableTexture = class(TAsphyreLockableTexture)
 private
  FSurface: IDirectDrawSurface7;
  FSurfaceDesc: TDDSurfaceDesc2;

  LockedRect: TRect;

  procedure UnlockRect(Rect: PRect; Level: Integer);
  procedure InitSurfaceDesc();
  function CreateTextureInstance(): Boolean;
  procedure DestroyTextureInstance();
  function GetSurfaceLevel(Level: Integer): IDirectDrawSurface7;
  function GetSizeOfLevel(Level: Integer): TPoint2px;
  procedure LockRect(Rect: PRect; Level: Integer; out Bits: Pointer;
   out Pitch: Integer);
  function GetLockRectPtr(Rect: PRect): PRect;
  function GetPixelData(Level: Integer; Buffer: TSystemSurface): Boolean;
  function SetPixelData(Level: Integer; Buffer: TSystemSurface): Boolean;
  function MakeMipmap(DestNo, SrcNo: Integer): Boolean;
 protected
  procedure UpdateSize(); override;

  function CreateTexture(): Boolean; override;
  procedure DestroyTexture(); override;
 public
  property Surface: IDirectDrawSurface7 read FSurface;
  property SurfaceDesc: TDDSurfaceDesc2 read FSurfaceDesc;

  procedure Bind(Stage: Integer); override;

  procedure UpdateMipmaps(); override;

  procedure Lock(const Rect: TRect; out Bits: Pointer;
   out Pitch: Integer); override;
  procedure Unlock(); override;

  constructor Create(); override;
 end;

//---------------------------------------------------------------------------
 TDX7RenderTargetTexture = class(TAsphyreRenderTargetTexture)
 private
  FSurface    : IDirectDrawSurface7;
  FSurfaceDesc: TDDSurfaceDesc2;
  PrevTarget  : IDirectDrawSurface7;
  PrevViewport: TD3DViewport7;

  procedure InitSurfaceDesc();
  function CreateTextureInstance(): Boolean;
  procedure DestroyTextureInstance();
 protected
  procedure UpdateSize(); override;

  function CreateTexture(): Boolean; override;
  procedure DestroyTexture(); override;
 public
  property Surface: IDirectDrawSurface7 read FSurface;
  property SurfaceDesc: TDDSurfaceDesc2 read FSurfaceDesc;

  procedure Bind(Stage: Integer); override;

  procedure HandleDeviceReset(); override;
  procedure HandleDeviceLost(); override;

  function BeginDrawTo(): Boolean; override;
  procedure EndDrawTo(); override;

  constructor Create(); override;
 end;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 AsphyreUtils, AsphyreErrors, AsphyreConv, DX7Types;

//---------------------------------------------------------------------------
function ComputeMipLevels(Width, Height: Integer): Integer;
begin
 Result:= 1;

 while (Width > 1)and(Height > 1)and(Width and 1 = 0)and(Height and 1 = 0) do
  begin
   Width := Width div 2;
   Height:= Height div 2;
   Inc(Result);
  end;
end;

//---------------------------------------------------------------------------
constructor TDX7LockableTexture.Create();
begin
 inherited;

 FSurface:= nil;
 FillChar(FSurfaceDesc, SizeOf(TDDSurfaceDesc), 0);
end;

//---------------------------------------------------------------------------
procedure TDX7LockableTexture.InitSurfaceDesc();
begin
 FillChar(FSurfaceDesc, SizeOf(TDDSurfaceDesc2), 0);

 FSurfaceDesc.dwSize:= SizeOf(TDDSurfaceDesc2);

 FSurfaceDesc.dwFlags:= DDSD_CAPS or DDSD_HEIGHT or DDSD_WIDTH or
   DDSD_PIXELFORMAT or DDSD_TEXTURESTAGE;

 FSurfaceDesc.ddsCaps.dwCaps := DDSCAPS_TEXTURE;
 FSurfaceDesc.ddsCaps.dwCaps2:= DDSCAPS2_TEXTUREMANAGE;

 FSurfaceDesc.dwWidth := Width;
 FSurfaceDesc.dwHeight:= Height;

 if (MipMapping) then
  begin
   FSurfaceDesc.dwFlags:= FSurfaceDesc.dwFlags or DDSD_MIPMAPCOUNT;

   FSurfaceDesc.ddsCaps.dwCaps:= FSurfaceDesc.ddsCaps.dwCaps or
    DDSCAPS_MIPMAP or DDSCAPS_COMPLEX;

   FSurfaceDesc.dwMipMapCount:= ComputeMipLevels(Width, Height);
  end;

 PixelFormatToDesc(FFormat, @FSurfaceDesc.ddpfPixelFormat);
end;

//---------------------------------------------------------------------------
function TDX7LockableTexture.CreateTextureInstance(): Boolean;
begin
 Result:= (Device7 <> nil);
 if (not Result) then
  begin
   Errors.Insert(errInvalidCall, Self, ClassName, 'CreateTextureInstance');
   Exit;
  end;

 FFormat:= ApproximateTextureFormat(FFormat);

 Result:= FFormat <> apf_Unknown;
 if (not Result) then
  begin
   Errors.Insert(errUnsupportedFormat, Self, ClassName, 'CreateTextureInstance');
   Exit;
  end;

 InitSurfaceDesc();

 Result:= Succeeded(DirectDraw.CreateSurface(FSurfaceDesc, FSurface, nil));
 if (not Result) then
  Errors.Insert(errCannotCreateTexture, Self, ClassName, 'CreateTextureInstance');

 if (Result) then
  begin
   Result:= Succeeded(FSurface.GetSurfaceDesc(FSurfaceDesc));
   if (not Result) then
    begin
     FSurface:= nil;
     Errors.Insert(errCannotCreateTexture, Self, ClassName, 'CreateTextureInstance');
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TDX7LockableTexture.DestroyTextureInstance();
begin
 if (FSurface <> nil) then
  begin
   FSurface:= nil;
   FillChar(FSurfaceDesc, SizeOf(TDDSurfaceDesc), 0);
  end;
end;

//---------------------------------------------------------------------------
function TDX7LockableTexture.CreateTexture(): Boolean;
begin
 Result:= CreateTextureInstance();
end;

//---------------------------------------------------------------------------
procedure TDX7LockableTexture.DestroyTexture();
begin
 DestroyTextureInstance();
end;

//---------------------------------------------------------------------------
procedure TDX7LockableTexture.Bind(Stage: Integer);
begin
 if (Device7 = nil)or(FSurface = nil) then
  Errors.Insert(errInvalidCall, Self, ClassName, 'Bind');

 if (Device7 = nil) then Exit;

 if (FSurface <> nil) then
  Device7.SetTexture(Stage, FSurface)
   else Device7.SetTexture(Stage, nil);
end;

//---------------------------------------------------------------------------
function TDX7LockableTexture.GetSizeOfLevel(Level: Integer): TPoint2px;
var
 MipMap : IDirectDrawSurface7;
 MipDesc: TDDSurfaceDesc2;
begin
 Result:= ZeroPoint2px;

 MipMap:= GetSurfaceLevel(Level);
 if (MipMap = nil) then Exit;

 FillChar(MipDesc, SizeOf(TDDSurfaceDesc2), 0);
 MipDesc.dwSize:= SizeOf(TDDSurfaceDesc2);

 if (Succeeded(MipMap.GetSurfaceDesc(MipDesc))) then
  begin
   Result.x:= MipDesc.dwWidth;
   Result.y:= MipDesc.dwHeight;
  end;
end;

//---------------------------------------------------------------------------
function TDX7LockableTexture.GetSurfaceLevel(Level: Integer): IDirectDrawSurface7;
var
 Surface1, Surface2: IDirectDrawSurface7;
 Caps: TDDSCaps2;
begin
 if (FSurface = nil) then
  begin
   Result:= nil;
   Exit;
  end;

 if (Level = 0) then
  begin
   Result:= FSurface;
   Exit;
  end;

 Surface1:= FSurface;
 Surface2:= nil;

 repeat
  FillChar(Caps, SizeOf(TDDSCaps2), 0);
  Caps.dwCaps:= DDSCAPS_MIPMAP;

  if (Failed(Surface1.GetAttachedSurface(Caps, Surface2))) then
   begin
    Surface1:= nil;
    Surface2:= nil;
    Exit;
   end;

  Surface1:= Surface2;
  Surface2:= nil;

  Dec(Level);
 until (Level <= 0);

 Result:= Surface1;

 Surface1:= nil;
 Surface2:= nil;
end;

//---------------------------------------------------------------------------
procedure TDX7LockableTexture.LockRect(Rect: PRect; Level: Integer;
 out Bits: Pointer; out Pitch: Integer);
var
 MipMap : IDirectDrawSurface7;
 MipDesc: TDDSurfaceDesc2;
begin
 Bits := nil;
 Pitch:= 0;

 MipMap:= GetSurfaceLevel(Level);
 if (MipMap = nil) then
  begin
   Errors.Insert(errInvalidCall, Self, ClassName, 'LockRect');
   Exit;
  end;

 FillChar(MipDesc, SizeOf(TDDSurfaceDesc2), 0);
 MipDesc.dwSize:= SizeOf(TDDSurfaceDesc2);

 if (Succeeded(MipMap.Lock(Rect, MipDesc, DDLOCK_SURFACEMEMORYPTR or
  DDLOCK_WAIT, 0))) then
  begin
   Bits := MipDesc.lpSurface;
   Pitch:= MipDesc.lPitch;
  end else Errors.Insert(errAccessTexture, Self, ClassName, 'LockRect');

 MipMap:= nil;
end;

//---------------------------------------------------------------------------
procedure TDX7LockableTexture.UnlockRect(Rect: PRect; Level: Integer);
var
 MipMap: IDirectDrawSurface7;
begin
 MipMap:= GetSurfaceLevel(Level);

 if (MipMap <> nil) then MipMap.Unlock(Rect);
end;

//---------------------------------------------------------------------------
function TDX7LockableTexture.GetLockRectPtr(Rect: PRect): PRect;
begin
 Result:= Rect;

 if (Rect^.Left = 0)and(Rect^.Top = 0)and(Rect^.Right = Width)and
  (Rect^.Bottom = Height) then Result:= nil;
end;

//---------------------------------------------------------------------------
procedure TDX7LockableTexture.Lock(const Rect: TRect; out Bits: Pointer;
 out Pitch: Integer);
begin
 LockedRect:= Rect;
 LockRect(GetLockRectPtr(@LockedRect), 0, Bits, Pitch);
end;

//---------------------------------------------------------------------------
procedure TDX7LockableTexture.Unlock();
begin
 UnlockRect(GetLockRectPtr(@LockedRect), 0);
end;

//---------------------------------------------------------------------------
procedure TDX7LockableTexture.UpdateSize();
begin
 DestroyTextureInstance();
 CreateTextureInstance();
end;

//---------------------------------------------------------------------------
function TDX7LockableTexture.GetPixelData(Level: Integer;
 Buffer: TSystemSurface): Boolean;
var
 Size : TPoint2px;
 Bits : Pointer;
 Pitch: Integer;
 Index: Integer;
 LinePtr: Pointer;
begin
 Result:= False;

 Size:= GetSizeOfLevel(Level);
 if (Size = ZeroPoint2px) then
  begin
   Errors.Insert(errInvalidCall, Self, ClassName, 'GetPixelData');
   Exit;
  end;

 LockRect(nil, Level, Bits, Pitch);
 if (Bits = nil)or(Pitch < 1) then Exit;

 Buffer.SetSize(Size.x, Size.y);

 for Index:= 0 to Size.y - 1 do
  begin
   LinePtr:= Pointer(PtrInt(Bits) + (Pitch * Index));

   if (FFormat = apf_A8R8G8B8) then
    begin
     Move(LinePtr^, Buffer.Scanline[Index]^, Buffer.Width * 4);
    end else
    begin
     PixelXto32Array(LinePtr, Buffer.Scanline[Index], FFormat, Buffer.Width);
    end;
  end;

 UnlockRect(nil, Level);
 Result:= True;
end;

//---------------------------------------------------------------------------
function TDX7LockableTexture.SetPixelData(Level: Integer;
 Buffer: TSystemSurface): Boolean;
var
 Size : TPoint2px;
 Bits : Pointer;
 Pitch: Integer;
 Index: Integer;
 SegWidth: Integer;
 LinePtr : Pointer;
begin
 Result:= False;

 Size:= GetSizeOfLevel(Level);
 if (Size = ZeroPoint2px) then
  begin
   Errors.Insert(errInvalidCall, Self, ClassName, 'SetPixelData');
   Exit;
  end;

 LockRect(nil, Level, Bits, Pitch);
 if (Bits = nil)or(Pitch < 1) then Exit;

 SegWidth:= Min2(Size.x, Buffer.Width);

 for Index:= 0 to Min2(Size.y, Buffer.Height) - 1 do
  begin
   LinePtr:= Pointer(PtrInt(Bits) + (Pitch * Index));

   if (FFormat = apf_A8R8G8B8) then
    begin
     Move(Buffer.Scanline[Index]^, LinePtr^, SegWidth * 4);
    end else
    begin
     Pixel32toXArray(Buffer.Scanline[Index], LinePtr, FFormat, SegWidth);
    end;
  end;

 UnlockRect(nil, Level);
 Result:= True;
end;

//---------------------------------------------------------------------------
function TDX7LockableTexture.MakeMipmap(DestNo, SrcNo: Integer): Boolean;
var
 InBuf, OutBuf: TSystemSurface;
begin
 Result:= False;

 InBuf:= TSystemSurface.Create();

 if (not GetPixelData(SrcNo, InBuf)) then
  begin
   InBuf.Free();
   Exit;
  end;

 OutBuf:= TSystemSurface.Create();
 OutBuf.Shrink2x(InBuf);

 InBuf.Free();

 Result:= SetPixelData(DestNo, OutBuf);

 OutBuf.Free();
end;

//---------------------------------------------------------------------------
procedure TDX7LockableTexture.UpdateMipmaps();
var
 MipNo : Integer;
begin
 for MipNo:= 0 to FSurfaceDesc.dwMipMapCount - 2 do
  if (not MakeMipmap(MipNo + 1, MipNo)) then Break;
end;

//---------------------------------------------------------------------------
constructor TDX7RenderTargetTexture.Create();
begin
 inherited;

 FSurface  := nil;
 PrevTarget:= nil;
 FillChar(FSurfaceDesc, SizeOf(TDDSurfaceDesc), 0);
 FillChar(PrevViewport, SizeOf(TD3DViewport7), 0);
end;

//---------------------------------------------------------------------------
procedure TDX7RenderTargetTexture.InitSurfaceDesc();
begin
 FillChar(FSurfaceDesc, SizeOf(TDDSurfaceDesc2), 0);

 FSurfaceDesc.dwSize:= SizeOf(TDDSurfaceDesc2);

 FSurfaceDesc.dwFlags:= DDSD_CAPS or DDSD_HEIGHT or DDSD_WIDTH or
   DDSD_PIXELFORMAT or DDSD_TEXTURESTAGE;

 FSurfaceDesc.ddsCaps.dwCaps := DDSCAPS_TEXTURE or DDSCAPS_3DDEVICE or
  DDSCAPS_VIDEOMEMORY;

 FSurfaceDesc.dwWidth := Width;
 FSurfaceDesc.dwHeight:= Height;

 PixelFormatToDesc(FFormat, @FSurfaceDesc.ddpfPixelFormat);
end;

//---------------------------------------------------------------------------
function TDX7RenderTargetTexture.CreateTextureInstance(): Boolean;
begin
 Result:= (Device7 <> nil);
 if (not Result) then
  begin
   Errors.Insert(errInvalidCall, Self, ClassName, 'CreateTextureInstance');
   Exit;
  end;

 FFormat:= ApproximateTextureFormat(FFormat);

 Result:= FFormat <> apf_Unknown;
 if (not Result) then
  begin
   Errors.Insert(errUnsupportedFormat, Self, ClassName, 'CreateTextureInstance');
   Exit;
  end;

 InitSurfaceDesc();

 Result:= Succeeded(DirectDraw.CreateSurface(FSurfaceDesc, FSurface, nil));
 if (not Result) then
  Errors.Insert(errCannotCreateTexture, Self, ClassName, 'CreateTextureInstance');

 if (Result) then
  begin
   Result:= Succeeded(FSurface.GetSurfaceDesc(FSurfaceDesc));
   if (not Result) then
    begin
     FSurface:= nil;
     Errors.Insert(errGetSurfaceDesc, Self, ClassName, 'CreateTextureInstance');
    end;
  end;

 PrevTarget:= nil;
end;

//---------------------------------------------------------------------------
procedure TDX7RenderTargetTexture.DestroyTextureInstance();
begin
 if (PrevTarget <> nil) then PrevTarget:= nil;

 if (FSurface <> nil) then
  begin
   FSurface:= nil;
   FillChar(FSurfaceDesc, SizeOf(TDDSurfaceDesc), 0);
  end;
end;

//---------------------------------------------------------------------------
function TDX7RenderTargetTexture.CreateTexture(): Boolean;
begin
 Result:= CreateTextureInstance();
end;

//---------------------------------------------------------------------------
procedure TDX7RenderTargetTexture.DestroyTexture();
begin
 DestroyTextureInstance();
end;

//---------------------------------------------------------------------------
procedure TDX7RenderTargetTexture.Bind(Stage: Integer);
begin
 if (Device7 = nil)or(FSurface = nil) then
  Errors.Insert(errInvalidCall, Self, ClassName, 'Bind');

 if (Device7 = nil) then Exit;

 if (FSurface <> nil) then
  Device7.SetTexture(Stage, FSurface)
   else Device7.SetTexture(Stage, nil);
end;

//---------------------------------------------------------------------------
procedure TDX7RenderTargetTexture.HandleDeviceReset();
begin
 if (FSurface <> nil)or(DirectDraw = nil) then Exit;

 if (Failed(DirectDraw.CreateSurface(FSurfaceDesc, FSurface, nil))) then
  begin
   Errors.Insert(errCannotCreateTexture, Self, ClassName, 'HandleDeviceReset');
   Exit;
  end;

 if (Failed(FSurface.GetSurfaceDesc(FSurfaceDesc))) then
  Errors.Insert(errGetSurfaceDesc, Self, ClassName, 'HandleDeviceReset');
end;

//---------------------------------------------------------------------------
procedure TDX7RenderTargetTexture.HandleDeviceLost();
begin
 if (FSurface <> nil) then FSurface:= nil;
end;

//---------------------------------------------------------------------------
procedure TDX7RenderTargetTexture.UpdateSize();
begin
 DestroyTextureInstance();
 CreateTextureInstance();
end;

//---------------------------------------------------------------------------
function TDX7RenderTargetTexture.BeginDrawTo(): Boolean;
var
 Viewport: TD3DViewport7;
begin
 Result:= (FSurface <> nil)and(Device7 <> nil);
 if (not Result) then
  begin
   Errors.Insert(errInvalidCall, Self, ClassName, 'BeginDrawTo');
   Exit;
  end;

 Result:= Succeeded(Device7.GetRenderTarget(PrevTarget));
 if (not Result) then
  begin
   Errors.Insert(errGetRenderTarget, Self, ClassName, 'BeginDrawTo');
   Exit;
  end;

 Result:= Succeeded(Device7.SetRenderTarget(FSurface, 0));
 if (not Result) then
  begin
   Errors.Insert(errSetRenderTarget, Self, ClassName, 'BeginDrawTo');
   Exit;
  end;

 Device7.GetViewport(PrevViewport);

 Viewport.dwX:= 0;
 Viewport.dwY:= 0;
 Viewport.dwWidth := Width;
 Viewport.dwHeight:= Height;
 Viewport.dvMinZ:= 1.0;
 Viewport.dvMaxZ:= 1000.0;

 Device7.SetViewport(Viewport);
end;

//---------------------------------------------------------------------------
procedure TDX7RenderTargetTexture.EndDrawTo();
begin
 if (PrevTarget <> nil)and(Device7 <> nil) then
  begin
   Device7.SetRenderTarget(PrevTarget, 0);
   Device7.SetViewport(PrevViewport);
   PrevTarget:= nil;
  end;
end;

//---------------------------------------------------------------------------
end.
