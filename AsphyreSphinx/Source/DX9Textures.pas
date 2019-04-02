unit DX9Textures;
//---------------------------------------------------------------------------
// DX9Textures.pas                                      Modified: 05-May-2008
// Texture implementation using DirectX 9.0                       Version 1.0
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
// The Original Code is DX9Textures.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by M. Sc. Yuriy Kotsarenko are Copyright (C) 2007 - 2008,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// Use Delphi Compatibility mode in FreePascal
//---------------------------------------------------------------------------
{$ifdef fpc}{$mode delphi}{$endif}

//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Windows, Direct3D9, System.Types, SysUtils, AsphyreTypes, AbstractTextures;

//---------------------------------------------------------------------------
type
 TDX9LockableTexture = class(TAsphyreLockableTexture)
 private
  FTexture: IDirect3DTexture9;
  
  TextureUsage: Cardinal;
  TexturePool : TD3DPool;
  FSystemPool: Boolean;
  procedure ComputeParams();
  function CreateTextureInstance(): Boolean;
  procedure DestroyTextureInstance();
 protected
  procedure UpdateSize(); override;

  function CreateTexture(): Boolean; override;
  procedure DestroyTexture(); override;
 public
  property Texture: IDirect3DTexture9 read FTexture;
  property SystemPool: Boolean read FSystemPool write FSystemPool;
  procedure Bind(Stage: Integer); override;

  procedure HandleDeviceReset(); override;
  procedure HandleDeviceLost(); override;

  procedure UpdateMipmaps(); override;

  procedure Lock(const Rect: TRect; out Bits: Pointer;
   out Pitch: Integer); override;
  procedure Unlock(); override;

  constructor Create(); override;
 end;

//---------------------------------------------------------------------------
 TDX9RenderTargetTexture = class(TAsphyreRenderTargetTexture)
 private
  FTexture     : IDirect3DTexture9;
  FDepthBuffer : IDirect3DSurface9;
  SavedSurface : IDirect3DSurface9;
  SavedDepthBuf: IDirect3DSurface9;

  function CreateTextureInstance(): Boolean;
  procedure DestroyTextureInstance();
 protected
  procedure UpdateSize(); override;

  function CreateTexture(): Boolean; override;
  procedure DestroyTexture(); override;
 public
  property Texture: IDirect3DTexture9 read FTexture;
  property DepthBuffer: IDirect3DSurface9 read FDepthBuffer;

  procedure Bind(Stage: Integer); override;

  procedure HandleDeviceReset(); override;
  procedure HandleDeviceLost(); override;

  procedure UpdateMipmaps(); override;

  function BeginDrawTo(): Boolean; override;
  procedure EndDrawTo(); override;

  constructor Create(); override;
 end;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 AsphyreErrors, DX9Types;

//---------------------------------------------------------------------------
constructor TDX9LockableTexture.Create();
begin
 inherited;

 FTexture    := nil;
 TextureUsage:= 0;
 TexturePool := D3DPOOL_SCRATCH;
 if SystemPool then
  TexturePool:=D3DPOOL_SYSTEMMEM;

end;

//---------------------------------------------------------------------------
procedure TDX9LockableTexture.ComputeParams();
begin
 TextureUsage:= 0;
 TexturePool := D3DPOOL_MANAGED;

 if (MipMapping) then TextureUsage:= TextureUsage or D3DUSAGE_AUTOGENMIPMAP;

 if (DynamicTexture) then
  begin
   TextureUsage:= TextureUsage or D3DUSAGE_DYNAMIC;
   TexturePool := D3DPOOL_DEFAULT;
  end;
 if SystemPool then
  TexturePool:=D3DPOOL_SYSTEMMEM;
 FFormat:= ApproximateTextureFormat(FFormat, TextureUsage);

 if (FFormat = apf_Unknown) then
  Errors.Insert(errUnsupportedFormat, Self, ClassName, 'ComputeParams');
end;

//---------------------------------------------------------------------------
function TDX9LockableTexture.CreateTextureInstance(): Boolean;
var
 Levels : Integer;
 DFormat: TD3DFormat;
begin
 Levels:= 1;
 if (MipMapping) then Levels:= 0;

 DFormat:= PixelFormatToD3DFormat(FFormat);

 Result:= Succeeded(Device9.CreateTexture(Width, Height, Levels, TextureUsage,
  DFormat, TexturePool, FTexture, nil));

 if (not Result) then
  Errors.Insert(errCannotCreateTexture, Self, ClassName,
   'CreateTextureInstance');
end;

//---------------------------------------------------------------------------
procedure TDX9LockableTexture.DestroyTextureInstance();
begin
 if (FTexture <> nil) then FTexture:= nil;
end;

//---------------------------------------------------------------------------
function TDX9LockableTexture.CreateTexture(): Boolean;
begin
 ComputeParams();

 Result:= (Device9 <> nil);
 if (not Result) then
  begin
   Errors.Insert(errInvalidCall, Self, ClassName, 'CreateTexture');
   Exit;
  end;

 Result:= CreateTextureInstance();
end;

//---------------------------------------------------------------------------
procedure TDX9LockableTexture.DestroyTexture();
begin
 DestroyTextureInstance();
end;

//---------------------------------------------------------------------------
procedure TDX9LockableTexture.HandleDeviceReset();
begin
 if (FTexture = nil)and(FFormat <> apf_Unknown)and
  (TexturePool = D3DPOOL_DEFAULT) then CreateTextureInstance();
end;

//---------------------------------------------------------------------------
procedure TDX9LockableTexture.HandleDeviceLost();
begin
 if (TexturePool = D3DPOOL_DEFAULT) then DestroyTextureInstance();
end;

//---------------------------------------------------------------------------
procedure TDX9LockableTexture.Bind(Stage: Integer);
begin
 if (Device9 = nil)or(FTexture = nil) then
  Errors.Insert(errInvalidCall, Self, ClassName, 'Bind');

 if (Device9 = nil) then Exit;

 if (FTexture <> nil) then Device9.SetTexture(Stage, FTexture)
  else Device9.SetTexture(Stage, nil);
end;

//---------------------------------------------------------------------------
procedure TDX9LockableTexture.Lock(const Rect: TRect; out Bits: Pointer;
 out Pitch: Integer);
var
 LockedRect: TD3DLockedRect;
 Usage     : Cardinal;
 RectPtr   : Pointer;
begin
 Bits := nil;
 Pitch:= 0;

 if (FTexture = nil) then
  begin
   Errors.Insert(errInvalidCall, Self, ClassName, 'Lock');
   Exit;
  end;

 // If the rectangle specified in Rect is the entire texture, then provide
 // null pointer instead.
 RectPtr:= @Rect;
 if (Rect.Left = 0)and(Rect.Top = 0)and(Rect.Right = Width)and
  (Rect.Bottom = Height) then RectPtr:= nil;

 Usage:= 0;
 if (DynamicTexture) then
  begin
   Usage:= D3DLOCK_DISCARD;

   // Only the entire texture can be locked at a time when dealing with
   // dynamic textures.
   if (RectPtr <> nil) then
    begin
     Errors.Insert(errInvalidCall, Self, ClassName, 'Lock');
     Exit;
    end;
  end;

 if (Succeeded(FTexture.LockRect(0, LockedRect, RectPtr, Usage))) then
  begin
   Bits := LockedRect.pBits;
   Pitch:= LockedRect.Pitch;
  end else Errors.Insert(errAccessTexture, Self, ClassName, 'Lock');
end;

//---------------------------------------------------------------------------
procedure TDX9LockableTexture.Unlock();
begin
 if (FTexture = nil) then
  begin
   Errors.Insert(errInvalidCall, Self, ClassName, 'Unlock');
   Exit;
  end;

 FTexture.UnlockRect(0);
end;

//---------------------------------------------------------------------------
procedure TDX9LockableTexture.UpdateMipmaps();
begin
 if (FTexture = nil) then
  begin
   Errors.Insert(errInvalidCall, Self, ClassName, 'UpdateMipmaps');
   Exit;
  end;

 FTexture.GenerateMipSubLevels();
end;

//---------------------------------------------------------------------------
procedure TDX9LockableTexture.UpdateSize();
begin
 DestroyTextureInstance();
 CreateTextureInstance();
end;

//---------------------------------------------------------------------------
constructor TDX9RenderTargetTexture.Create();
begin
 inherited;

 FTexture     := nil;
 FDepthBuffer := nil;
 SavedSurface := nil;
 SavedDepthBuf:= nil;
end;

//---------------------------------------------------------------------------
function TDX9RenderTargetTexture.CreateTextureInstance(): Boolean;
var
 Levels : Integer;
 Usage  : Cardinal;
 DFormat: TD3DFormat;
begin
 Result:= (Device9 <> nil);
 if (not Result) then
  begin
   Errors.Insert(errInvalidCall, Self, ClassName, 'CreateTextureInstance');
   Exit;
  end;

 Levels:= 1;
 if (MipMapping) then Levels:= 0;

 Usage:= D3DUSAGE_RENDERTARGET;
 if (MipMapping) then Usage:= Usage or D3DUSAGE_AUTOGENMIPMAP;

 FFormat:= ApproximateTextureFormat(FFormat, Usage);
 if (FFormat = apf_Unknown) then
  begin
   Errors.Insert(errUnsupportedFormat, Self, ClassName, 'ComputeParams');
   Result:= False;
   Exit;
  end;

 DFormat:= PixelFormatToD3DFormat(FFormat);

 Result:= Succeeded(Device9.CreateTexture(Width, Height, Levels, Usage,
  DFormat, D3DPOOL_DEFAULT, FTexture, nil));

 if (not Result) then
  begin
   Errors.Insert(errCannotCreateTexture, Self, ClassName,
    'CreateTextureInstance');
   Exit;
  end;

 if (DepthStencil) then
  begin
   Result:= Succeeded(Device9.CreateDepthStencilSurface(Width, Height,
    Params9.AutoDepthStencilFormat, D3DMULTISAMPLE_NONE, 0, True,
    FDepthBuffer, nil));
   if (not Result) then
    begin
     Errors.Insert(errCreateDepthStencil, Self, ClassName,
      'CreateTextureInstance');

     FTexture:= nil;
     Exit;
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TDX9RenderTargetTexture.DestroyTextureInstance();
begin
 if (FDepthBuffer <> nil) then FDepthBuffer:= nil;
 if (FTexture <> nil) then FTexture:= nil;
end;

//---------------------------------------------------------------------------
function TDX9RenderTargetTexture.CreateTexture(): Boolean;
begin
 Result:= CreateTextureInstance();
end;

//---------------------------------------------------------------------------
procedure TDX9RenderTargetTexture.DestroyTexture();
begin
 DestroyTextureInstance();
end;

//---------------------------------------------------------------------------
procedure TDX9RenderTargetTexture.Bind(Stage: Integer);
begin
 if (Device9 = nil)or(FTexture = nil) then
  Errors.Insert(errInvalidCall, Self, ClassName, 'Bind');

 if (Device9 = nil) then Exit;

 if (FTexture <> nil) then Device9.SetTexture(Stage, FTexture)
   else Device9.SetTexture(Stage, nil);
end;

//---------------------------------------------------------------------------
procedure TDX9RenderTargetTexture.HandleDeviceReset();
begin
 CreateTextureInstance();
end;

//---------------------------------------------------------------------------
procedure TDX9RenderTargetTexture.HandleDeviceLost();
begin
 DestroyTextureInstance();
end;

//---------------------------------------------------------------------------
procedure TDX9RenderTargetTexture.UpdateMipmaps();
begin
 if (FTexture = nil) then
  begin
   Errors.Insert(errInvalidCall, Self, ClassName, 'UpdateMipmaps');
   Exit;
  end;

 FTexture.GenerateMipSubLevels();
end;

//---------------------------------------------------------------------------
procedure TDX9RenderTargetTexture.UpdateSize();
begin
 DestroyTextureInstance();
 CreateTextureInstance();
end;

//---------------------------------------------------------------------------
function TDX9RenderTargetTexture.BeginDrawTo(): Boolean;
const
 MethodName = 'BeginDrawTo';
var
 Surface: IDirect3DSurface9;
begin
 // (1) Make sure the device is initialized and texture is created properly.
 Result:= (Device9 <> nil)and(FTexture <> nil);
 if (not Result) then
  begin
   Errors.Insert(errInvalidCall, Self, ClassName, MethodName);
   Exit;
  end;

 // (2) Retreive texture's drawing surface to use as render target.
 Result:= Succeeded(FTexture.GetSurfaceLevel(0, Surface));
 if (not Result) then
  begin
   Errors.Insert(errGetTextureSurface, Self, ClassName, MethodName);
   Exit;
  end;

 // (3) Save previously used render target's surface.
 Result:= Succeeded(Device9.GetRenderTarget(0, SavedSurface));
 if (not Result) then
  begin
   Surface:= nil;
   Errors.Insert(errGetRenderTarget, Self, ClassName, MethodName);
   Exit;
  end;

 // (4) Save previously used depth-stencil buffer.
 if (FDepthBuffer <> nil) then
  begin
   Result:= Succeeded(Device9.GetDepthStencilSurface(SavedDepthBuf));
   if (not Result) then
    begin
     SavedSurface:= nil;
     Surface:= nil;
     Errors.Insert(errGetDepthStencilBuffer, Self, ClassName, MethodName);
     Exit;
    end;
  end;

 // (5) Set new render target.
 Result := Succeeded(Device9.SetRenderTarget(0, Surface));
 Surface:= nil;

 if (not Result) then
  begin
   SavedDepthBuf:= nil;
   SavedSurface:= nil;
   Errors.Insert(errSetRenderTarget, Self, ClassName, MethodName);
   Exit;
  end;

 // (6) Set new depth-stencil buffer.
 if (FDepthBuffer <> nil) then
  begin
   Result:= Succeeded(Device9.SetDepthStencilSurface(FDepthBuffer));
   if (not Result) then
    begin
     SavedDepthBuf:= nil;
     SavedSurface:= nil;
     Errors.Insert(errSetDepthStencilBuffer, Self, ClassName, MethodName);
     Exit;
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TDX9RenderTargetTexture.EndDrawTo();
begin
 if (Device9 = nil) then
  begin
   Errors.Insert(errInvalidCall, Self, ClassName, 'EndDrawTo');
   Exit;
  end;

 if (SavedDepthBuf <> nil) then
  begin
   Device9.SetDepthStencilSurface(SavedDepthBuf);
   SavedDepthBuf:= nil;
  end;

 if (SavedSurface <> nil) then
  begin
   Device9.SetRenderTarget(0, SavedSurface);
   SavedSurface:= nil;
  end;
end;

//---------------------------------------------------------------------------
end.
