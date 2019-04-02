unit DX7Providers;
//---------------------------------------------------------------------------
// DX7Providers.pas                                     Modified: 25-Jan-2009
// DirectX 7.0 support provider                                  Version 1.01
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
// The Original Code is DX7Providers.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2007 - 2009,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 SysUtils, AsphyreFactory, AbstractDevices, AbstractCanvas, AbstractTextures,
 AbstractRasterizer;

//---------------------------------------------------------------------------
const
 idDirectX7 = $10000700;

//---------------------------------------------------------------------------
type
 TDX7Provider = class(TAsphyreProvider)
 private
 public
  function CreateDevice(): TAsphyreDevice; override;
  function CreateCanvas(): TAsphyreCanvas; override;
  function CreateRasterizer(): TAsphyreRasterizer; override;
  function CreateLockableTexture(): TAsphyreLockableTexture; override;
  function CreateRenderTargetTexture(): TAsphyreRenderTargetTexture; override;

  constructor Create();
  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
var
 DX7Provider: TDX7Provider = nil;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 DX7Devices, DX7Canvas, DX7Textures, DX7Rasterizer;

//---------------------------------------------------------------------------
constructor TDX7Provider.Create();
begin
 inherited;

 FProviderID:= idDirectX7;

 Factory.Subscribe(Self);
end;

//---------------------------------------------------------------------------
destructor TDX7Provider.Destroy();
begin
 Factory.Unsubscribe(Self, True);

 inherited;
end;

//---------------------------------------------------------------------------
function TDX7Provider.CreateDevice(): TAsphyreDevice;
begin
 Result:= TDX7Device.Create();
end;

//---------------------------------------------------------------------------
function TDX7Provider.CreateCanvas(): TAsphyreCanvas;
begin
 Result:= TDX7Canvas.Create();
end;

//---------------------------------------------------------------------------
function TDX7Provider.CreateRasterizer(): TAsphyreRasterizer;
begin
 Result:= TDX7Rasterizer.Create();
end;

//---------------------------------------------------------------------------
function TDX7Provider.CreateLockableTexture(): TAsphyreLockableTexture;
begin
 Result:= TDX7LockableTexture.Create();
end;

//---------------------------------------------------------------------------
function TDX7Provider.CreateRenderTargetTexture(): TAsphyreRenderTargetTexture;
begin
 Result:= TDX7RenderTargetTexture.Create();
end;

//---------------------------------------------------------------------------
initialization
 DX7Provider:= TDX7Provider.Create();

//---------------------------------------------------------------------------
finalization
 FreeAndNil(DX7Provider);

//---------------------------------------------------------------------------
end.
