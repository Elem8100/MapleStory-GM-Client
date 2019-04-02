unit OGLProviders;
//---------------------------------------------------------------------------
// OGLProviders.pas                                     Modified: 18-Feb-2009
// OpenGL support provider                                       Version 1.02
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
// The Original Code is OGLProviders.pas.
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
 idOpenGL = $20000000;

//---------------------------------------------------------------------------
type
 TOGLProvider = class(TAsphyreProvider)
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
 OGLProvider: TOGLProvider = nil;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 OGLDevices, OGLCanvas, OGLTextures, OGLRasterizer;

//---------------------------------------------------------------------------
constructor TOGLProvider.Create();
begin
 inherited;

 FProviderID:= idOpenGL;

 Factory.Subscribe(Self);
end;

//---------------------------------------------------------------------------
destructor TOGLProvider.Destroy();
begin
 Factory.Unsubscribe(Self, True);

 inherited;
end;

//---------------------------------------------------------------------------
function TOGLProvider.CreateDevice(): TAsphyreDevice;
begin
 Result:= TOGLDevice.Create();
end;

//---------------------------------------------------------------------------
function TOGLProvider.CreateCanvas(): TAsphyreCanvas;
begin
 Result:= TOGLCanvas.Create();
end;

//---------------------------------------------------------------------------
function TOGLProvider.CreateRasterizer(): TAsphyreRasterizer;
begin
 Result:= TOGLRasterizer.Create();
end;

//---------------------------------------------------------------------------
function TOGLProvider.CreateLockableTexture(): TAsphyreLockableTexture;
begin
 Result:= TOGLLockableTexture.Create();
end;

//---------------------------------------------------------------------------
function TOGLProvider.CreateRenderTargetTexture(): TAsphyreRenderTargetTexture;
begin
 Result:= TOGLRenderTargetTexture.Create();
end;

//---------------------------------------------------------------------------
initialization
 OGLProvider:= TOGLProvider.Create();

//---------------------------------------------------------------------------
finalization
 FreeAndNil(OGLProvider);

//---------------------------------------------------------------------------
end.
