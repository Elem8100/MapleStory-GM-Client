unit DX9Rasterizer;
//---------------------------------------------------------------------------
// DX9Rasterizer.pas                                    Modified: 02-Feb-2009
// 3D Software Rasterizer using DirectX 9.0 for Asphyre           Version 1.0
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
// The Original Code is DX9Rasterizer.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2000 - 2009,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Windows, Direct3D9, AbstractRasterizer, Vectors2, Vectors4, AsphyreTypes,
 AsphyreUtils, AbstractTextures;

//---------------------------------------------------------------------------
const
 // The following parameters roughly affect the rendering performance. The
 // higher values means that more primitives will fit in cache, but it will
 // also occupy more bandwidth, even when few primitives are rendered.
 //
 // These parameters can be fine-tuned in a finished product to improve the
 // overall performance.
 MaxCachedVertices   = 4096 * 3;
 MaxCachedPrimitives = 4096;

//---------------------------------------------------------------------------
type
 TDX9Rasterizer = class(TAsphyreRasterizer)
 private
  VertexBuffer: IDirect3DVertexBuffer9;
  VertexArray : Pointer;
  FVertexCache: Integer;
  FVertexCount: Integer;

  FPrimitives   : Integer;
  FMaxPrimitives: Integer;

  ActiveTex   : TAsphyreCustomTexture;
  CachedTex   : TAsphyreCustomTexture;
  CachedEffect: TRasterEffect;

  procedure InitCacheSpec();

  procedure CreateStaticObjects();
  procedure DestroyStaticObjects();

  function CreateDynamicBuffers(): Boolean;
  procedure DestroyDynamicBuffers();

  function UploadVertexBuffer(): Boolean;
  procedure DrawBuffers();

  function NextVertexEntry(): Pointer;
  function RequestCache(Vertices: Integer; Effect: TRasterEffect;
   Texture: TAsphyreCustomTexture): Boolean;

  procedure SetEffectStates(Effect: TRasterEffect);
 protected
  function HandleDeviceCreate(): Boolean; override;
  procedure HandleDeviceDestroy(); override;
  function HandleDeviceReset(): Boolean; override;
  procedure HandleDeviceLost(); override;

  procedure HandleBeginScene(); override;
  procedure HandleEndScene(); override;

  procedure GetViewport(out x, y, Width, Height: Integer); override;
  procedure SetViewport(x, y, Width, Height: Integer); override;
 public
  procedure FillTri(const Vtx0, Vtx1, Vtx2: TVector4; Diffuse0, Diffuse1,
   Diffuse2, Specular0, Specular1, Specular2: Cardinal;
   Effect: TRasterEffect = reNormal); override;

  procedure UseTexture(Texture: TAsphyreCustomTexture); override;

  procedure TexMap(const Vtx0, Vtx1, Vtx2: TVector4; Tex0, Tex1, Tex2: TPoint2;
   Diffuse0, Diffuse1, Diffuse2, Specular0, Specular1, Specular2: Cardinal;
   Effect: TRasterEffect = reNormal); override;

  procedure Flush(); override;
  procedure ResetStates(); override;

  constructor Create(); override;
  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
implementation

//--------------------------------------------------------------------------
uses
 DX9Types, AsphyreErrors;

//--------------------------------------------------------------------------
const
 VertexFVFType = D3DFVF_XYZRHW or D3DFVF_DIFFUSE or D3DFVF_SPECULAR or
  D3DFVF_TEX1;

//--------------------------------------------------------------------------
type
 PVertexRecord = ^TVertexRecord;
 TVertexRecord = record
  Vertex  : TD3DVector;
  rhw     : Single;
  Diffuse : Longword;
  Specular: Longword;
  u, v    : Single;
 end;

//--------------------------------------------------------------------------
constructor TDX9Rasterizer.Create();
begin
 inherited;

 VertexArray := nil;
 VertexBuffer:= nil;
end;

//---------------------------------------------------------------------------
destructor TDX9Rasterizer.Destroy();
begin
 DestroyDynamicBuffers();
 DestroyStaticObjects();

 inherited;
end;

//---------------------------------------------------------------------------
procedure TDX9Rasterizer.InitCacheSpec();
begin
 with Caps9 do
  begin
   FMaxPrimitives:= Min2(MaxPrimitiveCount, MaxCachedPrimitives);
   FVertexCache  := Min2(MaxVertexIndex, MaxCachedVertices);
  end;
end;

//---------------------------------------------------------------------------
procedure TDX9Rasterizer.CreateStaticObjects();
begin
 ReallocMem(VertexArray, FVertexCache * SizeOf(TVertexRecord));
 FillChar(VertexArray^, FVertexCache * SizeOf(TVertexRecord), 0);
end;

//---------------------------------------------------------------------------
procedure TDX9Rasterizer.DestroyStaticObjects();
begin
 if (VertexArray <> nil) then
  begin
   FreeMem(VertexArray);
   VertexArray:= nil;
  end;
end;

//--------------------------------------------------------------------------
function TDX9Rasterizer.CreateDynamicBuffers(): Boolean;
begin
 Result:= Succeeded(Device9.CreateVertexBuffer(FVertexCache *
  SizeOf(TVertexRecord), D3DUSAGE_WRITEONLY or D3DUSAGE_DYNAMIC,
  VertexFVFType, D3DPOOL_DEFAULT, VertexBuffer, nil));
 if (not Result) then Exit;
end;

//---------------------------------------------------------------------------
procedure TDX9Rasterizer.DestroyDynamicBuffers();
begin
 if (VertexBuffer <> nil) then VertexBuffer:= nil;
end;

//---------------------------------------------------------------------------
procedure TDX9Rasterizer.ResetStates();
begin
 FVertexCount:= 0;
 FPrimitives := 0;
 CachedEffect:= reUnknown;
 CachedTex   := nil;
 ActiveTex   := nil;

 with Device9 do
  begin
   // Disable 3D fancy stuff.
   SetRenderState(D3DRS_LIGHTING,  iFalse);
   SetRenderState(D3DRS_CULLMODE,  D3DCULL_NONE);
   SetRenderState(D3DRS_ZENABLE,   D3DZB_FALSE);
   SetRenderState(D3DRS_FOGENABLE, iFalse);

   // Enable Alpha-testing.
   SetRenderState(D3DRS_ALPHATESTENABLE, iTrue);
   SetRenderState(D3DRS_ALPHAFUNC, D3DCMP_GREATEREQUAL);
   SetRenderState(D3DRS_ALPHAREF,  $00000001);

   // Default alpha-blending behavior
   SetRenderState(D3DRS_ALPHABLENDENABLE, iTrue);

   // Alpha-blending stages for COLOR component.
   SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
   SetTextureStageState(0, D3DTSS_COLORARG1, D3DTA_TEXTURE);
   SetTextureStageState(0, D3DTSS_COLORARG2, D3DTA_DIFFUSE);

   SetTextureStageState(1, D3DTSS_COLOROP, D3DTOP_ADD);
   SetTextureStageState(1, D3DTSS_COLORARG1, D3DTA_SPECULAR);
   SetTextureStageState(1, D3DTSS_COLORARG2, D3DTA_CURRENT);

   SetTextureStageState(2, D3DTSS_COLOROP, D3DTOP_DISABLE);

   // Alpha-blending stages for ALPHA component.
   SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
   SetTextureStageState(0, D3DTSS_ALPHAARG1, D3DTA_TEXTURE);
   SetTextureStageState(0, D3DTSS_ALPHAARG2, D3DTA_DIFFUSE);

   SetTextureStageState(1, D3DTSS_ALPHAOP, D3DTOP_ADD);
   SetTextureStageState(1, D3DTSS_ALPHAARG1, D3DTA_SPECULAR);
   SetTextureStageState(1, D3DTSS_ALPHAARG2, D3DTA_CURRENT);

   SetTextureStageState(2, D3DTSS_ALPHAOP, D3DTOP_DISABLE);

   // Texture filtering flags.
   SetSamplerState(0, D3DSAMP_MAGFILTER, D3DTEXF_LINEAR);
   SetSamplerState(0, D3DSAMP_MINFILTER, D3DTEXF_LINEAR);
   SetSamplerState(0, D3DSAMP_MIPFILTER, D3DTEXF_LINEAR);

   // Triangle fill mode.
   SetRenderState(D3DRS_FILLMODE, D3DFILL_SOLID);
  end;
end;

//--------------------------------------------------------------------------
function TDX9Rasterizer.HandleDeviceCreate(): Boolean;
begin
 InitCacheSpec();
 CreateStaticObjects();

 Result:= True;
end;

//--------------------------------------------------------------------------
procedure TDX9Rasterizer.HandleDeviceDestroy();
begin
 DestroyStaticObjects();
end;

//--------------------------------------------------------------------------
function TDX9Rasterizer.HandleDeviceReset(): Boolean;
begin
 Result:= CreateDynamicBuffers();
end;

//--------------------------------------------------------------------------
procedure TDX9Rasterizer.HandleDeviceLost();
begin
 DestroyDynamicBuffers();
end;

//--------------------------------------------------------------------------
procedure TDX9Rasterizer.HandleBeginScene();
begin
 ResetStates();
end;

//--------------------------------------------------------------------------
procedure TDX9Rasterizer.HandleEndScene();
begin
 Flush();
end;

//---------------------------------------------------------------------------
procedure TDX9Rasterizer.GetViewport(out x, y, Width, Height: Integer);
var
 vp: TD3DViewport9;
begin
 if (Device9 = nil) then
  begin
   x:= 0; y:= 0; Width:= 0; Height:= 0;
   Exit;
  end;

 FillChar(vp, SizeOf(vp), 0);
 Device9.GetViewport(vp);

 x:= vp.X;
 y:= vp.Y;

 Width := vp.Width;
 Height:= vp.Height;
end;

//---------------------------------------------------------------------------
procedure TDX9Rasterizer.SetViewport(x, y, Width, Height: Integer);
var
 vp: TD3DViewport9;
begin
 if (Device9 = nil) then Exit;

 Flush();

 vp.X:= x;
 vp.Y:= y;
 vp.Width := Width;
 vp.Height:= Height;
 vp.MinZ:= 0.0;
 vp.MaxZ:= 1.0;

 Device9.SetViewport(vp);
end;

//---------------------------------------------------------------------------
function TDX9Rasterizer.UploadVertexBuffer(): Boolean;
var
 MemAddr: Pointer;
 BufSize: Integer;
begin
 BufSize:= FVertexCount * SizeOf(TVertexRecord);
 Result:= Succeeded(VertexBuffer.Lock(0, BufSize, MemAddr, D3DLOCK_DISCARD));

 if (Result) then
  begin
   Move(VertexArray^, MemAddr^, BufSize);
   Result:= Succeeded(VertexBuffer.Unlock());
  end;
end;

//---------------------------------------------------------------------------
procedure TDX9Rasterizer.DrawBuffers();
begin
 with Device9 do
  begin
   SetStreamSource(0, VertexBuffer, 0, SizeOf(TVertexRecord));
   SetVertexShader(nil);
   SetFVF(VertexFVFType);
   DrawPrimitive(D3DPT_TRIANGLELIST, 0, FPrimitives);
  end;

 NextDrawCall();
end;

//---------------------------------------------------------------------------
procedure TDX9Rasterizer.Flush();
begin
 if (FVertexCount > 0)and(FPrimitives > 0)and(UploadVertexBuffer()) then
  DrawBuffers();

 FVertexCount:= 0;
 FPrimitives := 0;
 CachedEffect:= reUnknown;

 Device9.SetTexture(0, nil);

 CachedTex:= nil;
 ActiveTex:= nil;
end;

//---------------------------------------------------------------------------
procedure TDX9Rasterizer.SetEffectStates(Effect: TRasterEffect);
begin
 case Effect of
  reNormal:
   with Device9 do
    begin
     SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_SRCALPHA);
     SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  reShadow:
   with Device9 do
    begin
     SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_ZERO);
     SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  reAdd:
   with Device9 do
    begin
     SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_SRCALPHA);
     SetRenderState(D3DRS_DESTBLEND, D3DBLEND_ONE);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  reMultiply:
   with Device9 do
    begin
     SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_ZERO);
     SetRenderState(D3DRS_DESTBLEND, D3DBLEND_SRCCOLOR);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  reSrcAlphaAdd:
   with Device9 do
    begin
     SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_SRCALPHA);
     SetRenderState(D3DRS_DESTBLEND, D3DBLEND_ONE);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  reSrcColor:
   with Device9 do
    begin
     SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_SRCCOLOR);
     SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCCOLOR);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  reSrcColorAdd:
   with Device9 do
    begin
     SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_SRCCOLOR);
     SetRenderState(D3DRS_DESTBLEND, D3DBLEND_ONE);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;
 end;
end;

//---------------------------------------------------------------------------
function TDX9Rasterizer.RequestCache(Vertices: Integer; Effect: TRasterEffect;
 Texture: TAsphyreCustomTexture): Boolean;
var
 NeedReset: Boolean;
begin
 Result:= (Vertices <= MaxCachedVertices);
 if (not Result) then
  begin
   Errors.Insert(errGeometryTooComplex, Self, ClassName, 'RequestCache');
   Exit;
  end;

 NeedReset:= (FVertexCount + Vertices > FVertexCache);
 NeedReset:= (NeedReset)or(CachedEffect = reUnknown)or(CachedEffect <> Effect);
 NeedReset:= (NeedReset)or(CachedTex <> Texture);

 if (NeedReset) then
  begin
   Flush();

   if (CachedEffect = reUnknown)or(CachedEffect <> Effect) then
    SetEffectStates(Effect);

   if (CachedEffect = reUnknown)or(CachedTex <> Texture) then
    begin
     if (Texture <> nil) then Texture.Bind(0)
      else Device9.SetTexture(0, nil);
    end;

   CachedEffect:= Effect;
   CachedTex   := Texture;
  end;
end;

//---------------------------------------------------------------------------
function TDX9Rasterizer.NextVertexEntry(): Pointer;
begin
 Result:= Pointer(PtrInt(VertexArray) + (FVertexCount * SizeOf(TVertexRecord)));
end;

//---------------------------------------------------------------------------
procedure TDX9Rasterizer.FillTri(const Vtx0, Vtx1, Vtx2: TVector4; Diffuse0,
 Diffuse1, Diffuse2, Specular0, Specular1, Specular2: Cardinal;
 Effect: TRasterEffect);
var
 Entry: PVertexRecord;
begin
 if (not RequestCache(3, Effect, nil)) then Exit;

 Entry:= NextVertexEntry();
 Entry^.Vertex.x:= Vtx0.x - 0.5;
 Entry^.Vertex.y:= Vtx0.y - 0.5;
 Entry^.Vertex.z:= Vtx0.z;
 Entry^.rhw     := Vtx0.w;
 Entry^.Diffuse := Diffuse0;
 Entry^.Specular:= Specular0;
 Entry^.u       := 0.0;
 Entry^.v       := 0.0;
 Inc(FVertexCount);

 Entry:= NextVertexEntry();
 Entry^.Vertex.x:= Vtx1.x - 0.5;
 Entry^.Vertex.y:= Vtx1.y - 0.5;
 Entry^.Vertex.z:= Vtx1.z;
 Entry^.rhw     := Vtx1.w;
 Entry^.Diffuse := Diffuse1;
 Entry^.Specular:= Specular1;
 Entry^.u       := 0.0;
 Entry^.v       := 0.0;
 Inc(FVertexCount);

 Entry:= NextVertexEntry();
 Entry^.Vertex.x:= Vtx2.x - 0.5;
 Entry^.Vertex.y:= Vtx2.y - 0.5;
 Entry^.Vertex.z:= Vtx2.z;
 Entry^.rhw     := Vtx2.w;
 Entry^.Diffuse := Diffuse2;
 Entry^.Specular:= Specular2;
 Entry^.u       := 0.0;
 Entry^.v       := 0.0;
 Inc(FVertexCount);

 Inc(FPrimitives);
end;

//---------------------------------------------------------------------------
procedure TDX9Rasterizer.UseTexture(Texture: TAsphyreCustomTexture);
begin
 ActiveTex:= Texture;
end;

//---------------------------------------------------------------------------
procedure TDX9Rasterizer.TexMap(const Vtx0, Vtx1, Vtx2: TVector4; Tex0, Tex1,
 Tex2: TPoint2; Diffuse0, Diffuse1, Diffuse2, Specular0, Specular1,
 Specular2: Cardinal; Effect: TRasterEffect);
var
 Entry: PVertexRecord;
begin
 if (not RequestCache(3, Effect, ActiveTex)) then Exit;

 Entry:= NextVertexEntry();
 Entry^.Vertex.x:= Vtx0.x - 0.5;
 Entry^.Vertex.y:= Vtx0.y - 0.5;
 Entry^.Vertex.z:= Vtx0.z;
 Entry^.rhw     := Vtx0.w;
 Entry^.Diffuse := Diffuse0;
 Entry^.Specular:= Specular0;
 Entry^.u       := Tex0.x;
 Entry^.v       := Tex0.y;
 Inc(FVertexCount);

 Entry:= NextVertexEntry();
 Entry^.Vertex.x:= Vtx1.x - 0.5;
 Entry^.Vertex.y:= Vtx1.y - 0.5;
 Entry^.Vertex.z:= Vtx1.z;
 Entry^.rhw     := Vtx1.w;
 Entry^.Diffuse := Diffuse1;
 Entry^.Specular:= Specular1;
 Entry^.u       := Tex1.x;
 Entry^.v       := Tex1.y;
 Inc(FVertexCount);

 Entry:= NextVertexEntry();
 Entry^.Vertex.x:= Vtx2.x - 0.5;
 Entry^.Vertex.y:= Vtx2.y - 0.5;
 Entry^.Vertex.z:= Vtx2.z;
 Entry^.rhw     := Vtx2.w;
 Entry^.Diffuse := Diffuse2;
 Entry^.Specular:= Specular2;
 Entry^.u       := Tex2.x;
 Entry^.v       := Tex2.y;
 Inc(FVertexCount);

 Inc(FPrimitives);
end;

//---------------------------------------------------------------------------
end.
