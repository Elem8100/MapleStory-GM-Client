unit AbstractRasterizer;
//---------------------------------------------------------------------------
// AbstractRasterizer.pas                               Modified: 02-Feb-2009
// Asphyre 3D general rasterizer                                  Version 1.0
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
// The Original Code is AbstractRasterizer.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2007 - 2009,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Types, Vectors2, Vectors4, AbstractTextures, AsphyreImages;

//---------------------------------------------------------------------------
type
 TRasterEffect = (reUnknown, reNormal, reShadow, reAdd, reMultiply,
  reSrcAlphaAdd, reSrcColor, reSrcColorAdd);

//---------------------------------------------------------------------------
 TAsphyreRasterizer = class
 private
  FDrawCalls: Integer;

  CreateHandle    : Cardinal;
  DestroyHandle   : Cardinal;
  ResetHandle     : Cardinal;
  LostHandle      : Cardinal;
  BeginSceneHandle: Cardinal;
  EndSceneHandle  : Cardinal;

  procedure OnDeviceCreate(Sender: TObject; Param: Pointer;
   var Handled: Boolean);
  procedure OnDeviceDestroy(Sender: TObject; Param: Pointer;
   var Handled: Boolean);
  procedure OnDeviceReset(Sender: TObject; Param: Pointer;
   var Handled: Boolean);
  procedure OnDeviceLost(Sender: TObject; Param: Pointer;
   var Handled: Boolean);

  procedure OnBeginScene(Sender: TObject; Param: Pointer;
   var Handled: Boolean);
  procedure OnEndScene(Sender: TObject; Param: Pointer;
   var Handled: Boolean);

  function GetClipRect(): TRect;
  procedure SetClipRect(const Value: TRect);
 protected
  function HandleDeviceCreate(): Boolean; virtual;
  procedure HandleDeviceDestroy(); virtual;
  function HandleDeviceReset(): Boolean; virtual;
  procedure HandleDeviceLost(); virtual;

  procedure HandleBeginScene(); virtual; abstract;
  procedure HandleEndScene(); virtual; abstract;

  procedure GetViewport(out x, y, Width, Height: Integer); virtual; abstract;
  procedure SetViewport(x, y, Width, Height: Integer); virtual; abstract;

  procedure NextDrawCall();
 public
  property DrawCalls: Integer read FDrawCalls;

  property ClipRect: TRect read GetClipRect write SetClipRect;

  procedure FillTri(const Vtx0, Vtx1, Vtx2: TVector4; Diffuse0, Diffuse1,
   Diffuse2, Specular0, Specular1, Specular2: Cardinal;
   Effect: TRasterEffect = reNormal); virtual; abstract;

  procedure UseTexture(Texture: TAsphyreCustomTexture); virtual; abstract;
  procedure UseImage(Image: TAsphyreImage; TextureNo: Integer = 0);

  procedure TexMap(const Vtx0, Vtx1, Vtx2: TVector4; Tex0, Tex1, Tex2: TPoint2;
   Diffuse0, Diffuse1, Diffuse2, Specular0, Specular1, Specular2: Cardinal;
   Effect: TRasterEffect = reNormal); virtual; abstract;

  procedure Flush(); virtual; abstract;
  procedure ResetStates(); virtual;

  constructor Create(); virtual;
  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 AbstractDevices;

//---------------------------------------------------------------------------
constructor TAsphyreRasterizer.Create();
begin
 inherited;

 FDrawCalls:= 0;

 {$ifdef fpc}
 CreateHandle := EventDeviceCreate.Subscribe(@OnDeviceCreate, -2);
 DestroyHandle:= EventDeviceDestroy.Subscribe(@OnDeviceDestroy, -2);

 ResetHandle:= EventDeviceReset.Subscribe(@OnDeviceReset, -2);
 LostHandle := EventDeviceLost.Subscribe(@OnDeviceLost, -2);

 BeginSceneHandle:= EventBeginScene.Subscribe(@OnBeginScene, -2);
 EndSceneHandle  := EventEndScene.Subscribe(@OnEndScene, -2);
 {$else}
 CreateHandle := EventDeviceCreate.Subscribe(OnDeviceCreate, -2);
 DestroyHandle:= EventDeviceDestroy.Subscribe(OnDeviceDestroy, -2);

 ResetHandle:= EventDeviceReset.Subscribe(OnDeviceReset, -2);
 LostHandle := EventDeviceLost.Subscribe(OnDeviceLost, -2);

 BeginSceneHandle:= EventBeginScene.Subscribe(OnBeginScene, -2);
 EndSceneHandle  := EventEndScene.Subscribe(OnEndScene, -2);
 {$endif}
end;

//---------------------------------------------------------------------------
destructor TAsphyreRasterizer.Destroy();
begin
 EventEndScene.Unsubscribe(EndSceneHandle);
 EventDeviceLost.Unsubscribe(LostHandle);
 EventDeviceReset.Unsubscribe(ResetHandle);
 EventDeviceDestroy.Unsubscribe(DestroyHandle);
 EventDeviceCreate.Unsubscribe(CreateHandle);

 inherited;
end;

//---------------------------------------------------------------------------
procedure TAsphyreRasterizer.OnDeviceCreate(Sender: TObject; Param: Pointer;
 var Handled: Boolean);
var
 Success: Boolean;
begin
 Success:= HandleDeviceCreate();

 if (Param <> nil) then PBoolean(Param)^:= Success;
end;

//---------------------------------------------------------------------------
procedure TAsphyreRasterizer.OnDeviceDestroy(Sender: TObject; Param: Pointer;
 var Handled: Boolean);
begin
 HandleDeviceDestroy();
end;

//---------------------------------------------------------------------------
procedure TAsphyreRasterizer.OnDeviceReset(Sender: TObject; Param: Pointer;
 var Handled: Boolean);
var
 Success: Boolean;
begin
 Success:= HandleDeviceReset();

 if (Param <> nil) then PBoolean(Param)^:= Success;
end;

//---------------------------------------------------------------------------
procedure TAsphyreRasterizer.OnDeviceLost(Sender: TObject; Param: Pointer;
 var Handled: Boolean);
begin
 HandleDeviceLost();
end;

//---------------------------------------------------------------------------
procedure TAsphyreRasterizer.OnBeginScene(Sender: TObject; Param: Pointer;
 var Handled: Boolean);
begin
 FDrawCalls:= 0;

 HandleBeginScene();
end;

//---------------------------------------------------------------------------
procedure TAsphyreRasterizer.OnEndScene(Sender: TObject; Param: Pointer;
 var Handled: Boolean);
begin
 HandleEndScene();
end;

//---------------------------------------------------------------------------
procedure TAsphyreRasterizer.ResetStates();
begin
 // no code
end;

//---------------------------------------------------------------------------
function TAsphyreRasterizer.GetClipRect(): TRect;
var
 x, y, Width, Height: Integer;
begin
 GetViewport(x, y, Width, Height);

 Result:= Bounds(x, y, Width, Height);
end;

//---------------------------------------------------------------------------
procedure TAsphyreRasterizer.SetClipRect(const Value: TRect);
begin
 SetViewport(Value.Left, Value.Top, Value.Right - Value.Left,
  Value.Bottom - Value.Top);
end;

//---------------------------------------------------------------------------
procedure TAsphyreRasterizer.UseImage(Image: TAsphyreImage;
 TextureNo: Integer = 0);
var
 Texture: TAsphyreCustomTexture;
begin
 if (Image <> nil) then Texture:= Image.Texture[TextureNo]
  else Texture:= nil;

 UseTexture(Texture);
end;

//---------------------------------------------------------------------------
procedure TAsphyreRasterizer.NextDrawCall();
begin
 Inc(FDrawCalls);
end;

//---------------------------------------------------------------------------
function TAsphyreRasterizer.HandleDeviceCreate(): Boolean;
begin
 Result:= True;
end;

//---------------------------------------------------------------------------
procedure TAsphyreRasterizer.HandleDeviceDestroy();
begin
 // no code
end;

//---------------------------------------------------------------------------
function TAsphyreRasterizer.HandleDeviceReset(): Boolean;
begin
 Result:= True;
end;

//---------------------------------------------------------------------------
procedure TAsphyreRasterizer.HandleDeviceLost();
begin
 // no code
end;

//---------------------------------------------------------------------------
end.

