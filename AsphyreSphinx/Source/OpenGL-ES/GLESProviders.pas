unit GLESProviders;
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
 idOpenGLES = $2F000000;

//---------------------------------------------------------------------------
type
 TGLESProvider = class(TAsphyreProvider)
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
 GLESProvider: TGLESProvider = nil;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 GLESDevices, GLESCanvas, GLESTextures, GLESRasterizer;

//---------------------------------------------------------------------------
constructor TGLESProvider.Create();
begin
 inherited;

 FProviderID:= idOpenGLES;

 Factory.Subscribe(Self);
end;

//---------------------------------------------------------------------------
destructor TGLESProvider.Destroy();
begin
 Factory.Unsubscribe(Self, True);

 inherited;
end;

//---------------------------------------------------------------------------
function TGLESProvider.CreateDevice(): TAsphyreDevice;
begin
 Result:= TGLESDevice.Create();
end;

//---------------------------------------------------------------------------
function TGLESProvider.CreateCanvas(): TAsphyreCanvas;
begin
 Result:= TGLESCanvas.Create();
end;

//---------------------------------------------------------------------------
function TGLESProvider.CreateRasterizer(): TAsphyreRasterizer;
begin
 Result:= TGLESRasterizer.Create();
end;

//---------------------------------------------------------------------------
function TGLESProvider.CreateLockableTexture(): TAsphyreLockableTexture;
begin
 Result:= TGLESLockableTexture.Create();
end;

//---------------------------------------------------------------------------
function TGLESProvider.CreateRenderTargetTexture(): TAsphyreRenderTargetTexture;
begin
 Result:= nil;
end;

//---------------------------------------------------------------------------
initialization
 GLESProvider:= TGLESProvider.Create();

//---------------------------------------------------------------------------
finalization
 FreeAndNil(GLESProvider);

//---------------------------------------------------------------------------
end.
