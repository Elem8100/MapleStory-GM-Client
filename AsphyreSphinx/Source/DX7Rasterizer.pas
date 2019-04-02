unit DX7Rasterizer;
//---------------------------------------------------------------------------
// DX7Rasterizer.pas                                    Modified: 02-Feb-2009
// 3D Software Rasterizer using DirectX 7.0 for Asphyre           Version 1.0
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
// The Original Code is DX7Rasterizer.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2000 - 2009,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Windows, DirectDraw7, Direct3D7, AbstractRasterizer, Vectors2, Vectors4,
 AsphyreTypes, AbstractTextures;

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
 TDX7Rasterizer = class(TAsphyreRasterizer)
 private
  VertexBuffer: IDirect3DVertexBuffer7;
  VertexArray : Pointer;
  FVertexCount: Integer;
  FPrimitives : Integer;

  ActiveTex   : TAsphyreCustomTexture;
  CachedTex   : TAsphyreCustomTexture;
  CachedEffect: TRasterEffect;

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
 DX7Types, AsphyreErrors;

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
constructor TDX7Rasterizer.Create();
begin
 inherited;

 VertexArray := nil;
 VertexBuffer:= nil;
end;

//---------------------------------------------------------------------------
destructor TDX7Rasterizer.Destroy();
begin
 DestroyDynamicBuffers();
 DestroyStaticObjects();

 inherited;
end;

//---------------------------------------------------------------------------
procedure TDX7Rasterizer.CreateStaticObjects();
begin
 ReallocMem(VertexArray, MaxCachedVertices * SizeOf(TVertexRecord));
 FillChar(VertexArray^, MaxCachedVertices * SizeOf(TVertexRecord), 0);
end;

//---------------------------------------------------------------------------
procedure TDX7Rasterizer.DestroyStaticObjects();
begin
 if (VertexArray <> nil) then
  begin
   FreeMem(VertexArray);
   VertexArray:= nil;
  end;
end;

//--------------------------------------------------------------------------
function TDX7Rasterizer.CreateDynamicBuffers(): Boolean;
var
 Desc: TD3DVertexBufferDesc;
begin
 Result:= Direct3D <> nil;
 if (not Result) then Exit;

 FillChar(Desc, SizeOf(TD3DVertexBufferDesc), 0);

 Desc.dwSize:= SizeOf(TD3DVertexBufferDesc);
 Desc.dwCaps:= D3DVBCAPS_WRITEONLY or D3DVBCAPS_SYSTEMMEMORY;
 Desc.dwFVF := VertexFVFType;
 Desc.dwNumVertices:= MaxCachedVertices;

 Result:= Succeeded(Direct3D.CreateVertexBuffer(Desc, VertexBuffer, 0));
end;

//---------------------------------------------------------------------------
procedure TDX7Rasterizer.DestroyDynamicBuffers();
begin
 if (VertexBuffer <> nil) then VertexBuffer:= nil;
end;

//---------------------------------------------------------------------------
procedure TDX7Rasterizer.ResetStates();
begin
 FVertexCount:= 0;
 FPrimitives := 0;
 CachedEffect:= reUnknown;
 CachedTex   := nil;
 ActiveTex   := nil;

 with Device7 do
  begin
   // Disable 3D fancy stuff.
   SetRenderState(D3DRENDERSTATE_LIGHTING,  iFalse);
   SetRenderState(D3DRENDERSTATE_CULLMODE,  D3DCULL_NONE);
   SetRenderState(D3DRENDERSTATE_ZENABLE,   D3DZB_FALSE);
   SetRenderState(D3DRENDERSTATE_FOGENABLE, iFalse);

   // Enable Alpha-testing.
   SetRenderState(D3DRENDERSTATE_ALPHATESTENABLE, iTrue);
   SetRenderState(D3DRENDERSTATE_ALPHAFUNC, D3DCMP_GREATEREQUAL);
   SetRenderState(D3DRENDERSTATE_ALPHAREF,  $00000001);

   // Default alpha-blending behavior
   SetRenderState(D3DRENDERSTATE_ALPHABLENDENABLE, iTrue);

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
   SetTextureStageState(0, D3DTSS_MAGFILTER, D3DTFG_LINEAR);
   SetTextureStageState(0, D3DTSS_MINFILTER, D3DTFN_LINEAR);
   SetTextureStageState(0, D3DTSS_MIPFILTER, D3DTFP_LINEAR);

   // Triangle fill mode.
   SetRenderState(D3DRENDERSTATE_FILLMODE, Longword(D3DFILL_SOLID));
  end;
end;

//--------------------------------------------------------------------------
function TDX7Rasterizer.HandleDeviceCreate(): Boolean;
begin
 CreateStaticObjects();

 Result:= True;
end;

//--------------------------------------------------------------------------
procedure TDX7Rasterizer.HandleDeviceDestroy();
begin
 DestroyStaticObjects();
end;

//--------------------------------------------------------------------------
function TDX7Rasterizer.HandleDeviceReset(): Boolean;
begin
 Result:= CreateDynamicBuffers();
end;

//--------------------------------------------------------------------------
procedure TDX7Rasterizer.HandleDeviceLost();
begin
 DestroyDynamicBuffers();
end;

//--------------------------------------------------------------------------
procedure TDX7Rasterizer.HandleBeginScene();
begin
 ResetStates();
end;

//--------------------------------------------------------------------------
procedure TDX7Rasterizer.HandleEndScene();
begin
 Flush();
end;

//---------------------------------------------------------------------------
procedure TDX7Rasterizer.GetViewport(out x, y, Width, Height: Integer);
var
 vp: TD3DViewport7;
begin
 if (Device7 = nil) then
  begin
   x:= 0; y:= 0; Width:= 0; Height:= 0;
   Exit;
  end;

 FillChar(vp, SizeOf(vp), 0);
 Device7.GetViewport(vp);

 x:= vp.dwX;
 y:= vp.dwY;

 Width := vp.dwWidth;
 Height:= vp.dwHeight;
end;

//---------------------------------------------------------------------------
procedure TDX7Rasterizer.SetViewport(x, y, Width, Height: Integer);
var
 vp: TD3DViewport7;
begin
 if (Device7 = nil) then Exit;

 Flush();

 vp.dwX:= x;
 vp.dwY:= y;
 vp.dwWidth := Width;
 vp.dwHeight:= Height;
 vp.dvMinZ:= 0.0;
 vp.dvMaxZ:= 1.0;

 Device7.SetViewport(vp);
end;

//---------------------------------------------------------------------------
function TDX7Rasterizer.UploadVertexBuffer(): Boolean;
var
 MemAddr: Pointer;
 BufSize: Cardinal;
begin
 BufSize:= FVertexCount * SizeOf(TVertexRecord);
 Result:= Succeeded(VertexBuffer.Lock(DDLOCK_DISCARDCONTENTS or
  DDLOCK_SURFACEMEMORYPTR or DDLOCK_WRITEONLY, MemAddr, BufSize));

 if (Result) then
  begin
   Move(VertexArray^, MemAddr^, BufSize);
   Result:= Succeeded(VertexBuffer.Unlock());
  end;
end;

//---------------------------------------------------------------------------
procedure TDX7Rasterizer.DrawBuffers();
begin
 Device7.DrawPrimitiveVB(D3DPT_TRIANGLELIST, VertexBuffer, 0, FVertexCount, 0);

 NextDrawCall();
end;

//---------------------------------------------------------------------------
procedure TDX7Rasterizer.Flush();
begin
 if (FVertexCount > 0)and(FPrimitives > 0)and(UploadVertexBuffer()) then
  DrawBuffers();

 FVertexCount:= 0;
 FPrimitives := 0;
 CachedEffect:= reUnknown;

 Device7.SetTexture(0, nil);

 CachedTex:= nil;
 ActiveTex:= nil;
end;

//---------------------------------------------------------------------------
procedure TDX7Rasterizer.SetEffectStates(Effect: TRasterEffect);
begin
 case Effect of
  reNormal:
   with Device7 do
    begin
     SetRenderState(D3DRENDERSTATE_SRCBLEND,  D3DBLEND_SRCALPHA);
     SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  reShadow:
   with Device7 do
    begin
     SetRenderState(D3DRENDERSTATE_SRCBLEND,  D3DBLEND_ZERO);
     SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  reAdd:
   with Device7 do
    begin
     SetRenderState(D3DRENDERSTATE_SRCBLEND,  D3DBLEND_SRCALPHA);
     SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_ONE);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  reMultiply:
   with Device7 do
    begin
     SetRenderState(D3DRENDERSTATE_SRCBLEND,  D3DBLEND_ZERO);
     SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_SRCCOLOR);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  reSrcAlphaAdd:
   with Device7 do
    begin
     SetRenderState(D3DRENDERSTATE_SRCBLEND,  D3DBLEND_SRCALPHA);
     SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_ONE);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  reSrcColor:
   with Device7 do
    begin
     SetRenderState(D3DRENDERSTATE_SRCBLEND,  D3DBLEND_SRCCOLOR);
     SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCCOLOR);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  reSrcColorAdd:
   with Device7 do
    begin
     SetRenderState(D3DRENDERSTATE_SRCBLEND,  D3DBLEND_SRCCOLOR);
     SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_ONE);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;
 end;
end;

//---------------------------------------------------------------------------
function TDX7Rasterizer.RequestCache(Vertices: Integer; Effect: TRasterEffect;
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

 NeedReset:= (FVertexCount + Vertices > MaxCachedVertices);
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
      else Device7.SetTexture(0, nil);
    end;

   CachedEffect:= Effect;
   CachedTex   := Texture;
  end;
end;

//---------------------------------------------------------------------------
function TDX7Rasterizer.NextVertexEntry(): Pointer;
begin
 Result:= Pointer(PtrInt(VertexArray) + (FVertexCount * SizeOf(TVertexRecord)));
end;

//---------------------------------------------------------------------------
procedure TDX7Rasterizer.FillTri(const Vtx0, Vtx1, Vtx2: TVector4; Diffuse0,
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
procedure TDX7Rasterizer.UseTexture(Texture: TAsphyreCustomTexture);
begin
 ActiveTex:= Texture;
end;

//---------------------------------------------------------------------------
procedure TDX7Rasterizer.TexMap(const Vtx0, Vtx1, Vtx2: TVector4; Tex0, Tex1,
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
