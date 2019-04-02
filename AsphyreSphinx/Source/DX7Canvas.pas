unit DX7Canvas;
//---------------------------------------------------------------------------
// DX7Canvas.pas                                        Modified: 14-Dec-2008
// 2D Canvas using DirectX 7.0                                   Version 1.02
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
// The Original Code is DX7Canvas.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2007 - 2008,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------

{$ifdef fpc}{$mode delphi}{$endif}

interface

//---------------------------------------------------------------------------
uses
 Windows, DirectDraw7, Direct3D7, AbstractCanvas, Vectors2, AsphyreColors,
 AsphyreTypes, AbstractTextures;

//---------------------------------------------------------------------------
const
 // The following parameters roughly affect the rendering performance. The
 // higher values means that more primitives will fit in cache, but it will
 // also occupy more bandwidth, even when few primitives are rendered.
 //
 // These parameters can be fine-tuned in a finished product to improve the
 // overall performance.
 MaxCachedPrimitives = 3072;
 MaxCachedIndices    = 4096;
 MaxCachedVertices   = 4096;

//---------------------------------------------------------------------------
type
 TDrawingMode = (dmUnknown, dmPoints, dmLines, dmTriangles);

//---------------------------------------------------------------------------
 TDX7Canvas = class(TAsphyreCanvas)
 private
  VertexBuffer: IDirect3DVertexBuffer7;

  VertexArray : Pointer;
  IndexArray  : packed array[0..MaxCachedIndices - 1] of Word;

  DrawingMode: TDrawingMode;

  VertexCount: Integer;
  IndexCount : Integer;
  Primitives : Integer;
  ActiveTex  : TAsphyreCustomTexture;

  CachedEffect: TDrawingEffect;
  CachedTex   : TAsphyreCustomTexture;
  QuadMapping : TPoint4;

  procedure CreateStaticObjects();
  procedure DestroyStaticObjects();
  procedure PrepareVertexArray();

  function CreateDynamicBuffers(): Boolean;
  procedure DestroyDynamicBuffers();
  procedure ResetDeviceStates();

  function UploadVertexBuffer(): Boolean;
  procedure DrawBuffers();

  function NextVertexEntry(): Pointer;
  procedure AddIndexEntry(Index: Integer);
  function RequestCache(Mode: TDrawingMode; Vertices, Indices: Integer;
   Effect: TDrawingEffect; Texture: TAsphyreCustomTexture): Boolean;

  procedure SetEffectStates(Effect: TDrawingEffect);
 protected
  function HandleDeviceCreate(): Boolean; override;
  procedure HandleDeviceDestroy(); override;
  function HandleDeviceReset(): Boolean; override;
  procedure HandleDeviceLost(); override;

  procedure HandleBeginScene(); override;
  procedure HandleEndScene(); override;

  procedure GetViewport(out x, y, Width, Height: Integer); override;
  procedure SetViewport(x, y, Width, Height: Integer); override;

  function GetAntialias(): Boolean; override;
  procedure SetAntialias(const Value: Boolean); override;
  function GetMipMapping(): Boolean; override;
  procedure SetMipMapping(const Value: Boolean); override;
 public
  procedure PutPixel(const Point: TPoint2; Color: Cardinal); override;
  procedure Line(const Src, Dest: TPoint2; Color0, Color1: Cardinal); override;

  procedure DrawIndexedTriangles(Vertices: PPoint2; Colors: PCardinal;
   Indices: PInteger; NoVertices, NoTriangles: Integer;
   Effect: TDrawingEffect = deNormal); override;

  procedure UseTexture(Texture: TAsphyreCustomTexture;
   const Mapping: TPoint4); override;

  procedure TexMap(const Points: TPoint4; const Colors: TColor4;
   Effect: TDrawingEffect = deNormal); override;

  procedure Flush(); override;

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
 VertexFVFType = D3DFVF_XYZRHW or D3DFVF_DIFFUSE or D3DFVF_TEX1;

//--------------------------------------------------------------------------
type
 PVertexRecord = ^TVertexRecord;
 TVertexRecord = record
  Vertex: TD3DVector;
  rhw   : Single;
  Color : Longword;
  u, v  : Single;
 end;

//--------------------------------------------------------------------------
constructor TDX7Canvas.Create();
begin
 inherited;

 VertexArray := nil;
 VertexBuffer:= nil;
end;

//---------------------------------------------------------------------------
destructor TDX7Canvas.Destroy();
begin
 DestroyDynamicBuffers();
 DestroyStaticObjects();

 inherited;
end;

//---------------------------------------------------------------------------
procedure TDX7Canvas.PrepareVertexArray();
var
 Entry: PVertexRecord;
 Index: Integer;
begin
 Entry:= VertexArray;
 for Index:= 0 to MaxCachedVertices - 1 do
  begin
   FillChar(Entry^, SizeOf(TVertexRecord), 0);

   Entry.Vertex.z:= 0.0;
   Entry.rhw     := 1.0;

   Inc(Entry);
  end;
end;

//---------------------------------------------------------------------------
procedure TDX7Canvas.CreateStaticObjects();
begin
 ReallocMem(VertexArray, MaxCachedVertices * SizeOf(TVertexRecord));
 FillChar(VertexArray^, MaxCachedVertices * SizeOf(TVertexRecord), 0);

 PrepareVertexArray();
end;

//---------------------------------------------------------------------------
procedure TDX7Canvas.DestroyStaticObjects();
begin
 if (VertexArray <> nil) then
  begin
   FreeMem(VertexArray);
   VertexArray:= nil;
  end;
end;

//--------------------------------------------------------------------------
function TDX7Canvas.CreateDynamicBuffers(): Boolean;
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
procedure TDX7Canvas.DestroyDynamicBuffers();
begin
 if (VertexBuffer <> nil) then VertexBuffer:= nil;
end;

//---------------------------------------------------------------------------
procedure TDX7Canvas.ResetDeviceStates();
begin
 VertexCount := 0;
 IndexCount  := 0;
 Primitives  := 0;
 DrawingMode := dmUnknown;
 CachedEffect:= deUnknown;
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

   SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
   SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);

   SetTextureStageState(0, D3DTSS_MAGFILTER, D3DTFG_LINEAR);
   SetTextureStageState(0, D3DTSS_MINFILTER, D3DTFG_LINEAR);
   SetTextureStageState(0, D3DTSS_MIPFILTER, D3DTFP_NONE);
  end;
end;

//--------------------------------------------------------------------------
function TDX7Canvas.HandleDeviceCreate(): Boolean;
begin
 CreateStaticObjects();

 Result:= True;
end;

//--------------------------------------------------------------------------
procedure TDX7Canvas.HandleDeviceDestroy();
begin
 DestroyStaticObjects();
end;

//--------------------------------------------------------------------------
function TDX7Canvas.HandleDeviceReset(): Boolean;
begin
 Result:= CreateDynamicBuffers();
end;

//--------------------------------------------------------------------------
procedure TDX7Canvas.HandleDeviceLost();
begin
 DestroyDynamicBuffers();
end;

//--------------------------------------------------------------------------
procedure TDX7Canvas.HandleBeginScene();
begin
 ResetDeviceStates();
end;

//--------------------------------------------------------------------------
procedure TDX7Canvas.HandleEndScene();
begin
 Flush();
end;

//---------------------------------------------------------------------------
procedure TDX7Canvas.GetViewport(out x, y, Width, Height: Integer);
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
procedure TDX7Canvas.SetViewport(x, y, Width, Height: Integer);
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
function TDX7Canvas.GetAntialias(): Boolean;
var
 MagFlt, MinFlt: Cardinal;
begin
 if (Device7 = nil) then
  begin
   Result:= False;
   Exit;
  end;

 Device7.GetTextureStageState(0, D3DTSS_MAGFILTER, MagFlt);
 Device7.GetTextureStageState(0, D3DTSS_MINFILTER, MinFlt);

 Result:= True;

 if (MagFlt = D3DTFG_POINT)or(MinFlt = D3DTFN_POINT) then Result:= False;
end;

//---------------------------------------------------------------------------
procedure TDX7Canvas.SetAntialias(const Value: Boolean);
begin
 if (Device7 = nil) then Exit;

 Flush();

 case Value of
  False:
   begin
    Device7.SetTextureStageState(0, D3DTSS_MAGFILTER, D3DTFG_POINT);
    Device7.SetTextureStageState(0, D3DTSS_MINFILTER, D3DTFN_POINT);
   end;

  True:
   begin
    Device7.SetTextureStageState(0, D3DTSS_MAGFILTER, D3DTFG_LINEAR);
    Device7.SetTextureStageState(0, D3DTSS_MINFILTER, D3DTFN_LINEAR);
   end;
 end;
end;

//---------------------------------------------------------------------------
function TDX7Canvas.GetMipMapping(): Boolean;
var
 MipFlt: Cardinal;
begin
 if (Device7 = nil) then
  begin
   Result:= False;
   Exit;
  end;

 Device7.GetTextureStageState(0, D3DTSS_MIPFILTER, MipFlt);

 Result:= True;

 if (MipFlt = D3DTFP_NONE)or(MipFlt = D3DTFP_POINT) then Result:= False;
end;

//---------------------------------------------------------------------------
procedure TDX7Canvas.SetMipMapping(const Value: Boolean);
begin
 if (Device7 = nil) then Exit;

 Flush();

 case Value of
  False:
   Device7.SetTextureStageState(0, D3DTSS_MIPFILTER, D3DTFP_NONE);

  True:
   Device7.SetTextureStageState(0, D3DTSS_MIPFILTER, D3DTFP_LINEAR);
 end;
end;

//---------------------------------------------------------------------------
function TDX7Canvas.UploadVertexBuffer(): Boolean;
var
 MemAddr: Pointer;
 BufSize: Cardinal;
begin
 BufSize:= VertexCount * SizeOf(TVertexRecord);
 Result:= Succeeded(VertexBuffer.Lock(DDLOCK_DISCARDCONTENTS or
  DDLOCK_SURFACEMEMORYPTR or DDLOCK_WRITEONLY, MemAddr, BufSize));

 if (Result) then
  begin
   Move(VertexArray^, MemAddr^, BufSize);
   Result:= Succeeded(VertexBuffer.Unlock());
  end;
end;

//---------------------------------------------------------------------------
procedure TDX7Canvas.DrawBuffers();
begin
 with Device7 do
  begin
   case DrawingMode of
    dmPoints:
     DrawPrimitiveVB(D3DPT_POINTLIST, VertexBuffer, 0, VertexCount, 0);

    dmLines:
     DrawPrimitiveVB(D3DPT_LINELIST, VertexBuffer, 0, VertexCount, 0);

    dmTriangles:
     DrawIndexedPrimitiveVB(D3DPT_TRIANGLELIST, VertexBuffer, 0, VertexCount,
      IndexArray[0], IndexCount, 0);
   end;
  end;

 NextDrawCall();
end;

//---------------------------------------------------------------------------
procedure TDX7Canvas.Flush();
begin
 if (VertexCount > 0)and(Primitives > 0)and(UploadVertexBuffer()) then
  DrawBuffers();

 VertexCount:= 0;
 IndexCount := 0;
 Primitives := 0;
 DrawingMode := dmUnknown;
 CachedEffect:= deUnknown;

 Device7.SetTexture(0, nil);

 CachedTex:= nil;
 ActiveTex:= nil;
end;

//---------------------------------------------------------------------------
procedure TDX7Canvas.SetEffectStates(Effect: TDrawingEffect);
begin
 case Effect of
  deNormal:
   with Device7 do
    begin
     SetRenderState(D3DRENDERSTATE_SRCBLEND,  D3DBLEND_SRCALPHA);
     SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deShadow:
   with Device7 do
    begin
     SetRenderState(D3DRENDERSTATE_SRCBLEND,  D3DBLEND_ZERO);
     SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deAdd:
   with Device7 do
    begin
     SetRenderState(D3DRENDERSTATE_SRCBLEND,  D3DBLEND_SRCALPHA);
     SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_ONE);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deMultiply:
   with Device7 do
    begin
     SetRenderState(D3DRENDERSTATE_SRCBLEND,  D3DBLEND_ZERO);
     SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_SRCCOLOR);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deSrcAlphaAdd:
   with Device7 do
    begin
     SetRenderState(D3DRENDERSTATE_SRCBLEND,  D3DBLEND_SRCALPHA);
     SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_ONE);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deSrcColor:
   with Device7 do
    begin
     SetRenderState(D3DRENDERSTATE_SRCBLEND,  D3DBLEND_SRCCOLOR);
     SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCCOLOR);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deSrcColorAdd:
   with Device7 do
    begin
     SetRenderState(D3DRENDERSTATE_SRCBLEND,  D3DBLEND_SRCCOLOR);
     SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_ONE);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deInvert:
   with Device7 do
    begin
     SetRenderState(D3DRENDERSTATE_SRCBLEND,  D3DBLEND_INVDESTCOLOR);
     SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_ZERO);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deSrcBright:
   with Device7 do
    begin
     SetRenderState(D3DRENDERSTATE_SRCBLEND,  D3DBLEND_SRCCOLOR);
     SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_SRCCOLOR);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deInvMultiply:
   with Device7 do
    begin
     SetRenderState(D3DRENDERSTATE_SRCBLEND,  D3DBLEND_ZERO);
     SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCCOLOR);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deMultiplyAlpha:
   with Device7 do
    begin
     SetRenderState(D3DRENDERSTATE_SRCBLEND,  D3DBLEND_ZERO);
     SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_SRCALPHA);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deInvMultiplyAlpha:
   with Device7 do
    begin
     SetRenderState(D3DRENDERSTATE_SRCBLEND,  D3DBLEND_ZERO);
     SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deDestBright:
   with Device7 do
    begin
     SetRenderState(D3DRENDERSTATE_SRCBLEND,  D3DBLEND_DESTCOLOR);
     SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_DESTCOLOR);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deInvSrcBright:
   with Device7 do
    begin
     SetRenderState(D3DRENDERSTATE_SRCBLEND,  D3DBLEND_INVSRCCOLOR);
     SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCCOLOR);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deInvDestBright:
   with Device7 do
    begin
     SetRenderState(D3DRENDERSTATE_SRCBLEND,  D3DBLEND_INVDESTCOLOR);
     SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVDESTCOLOR);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deBright:
   with Device7 do
    begin
     SetRenderState(D3DRENDERSTATE_SRCBLEND,  D3DBLEND_SRCALPHA);
     SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA);
     SetTextureStageState(0, D3DTSS_COLOROP,  D3DTOP_MODULATE2X);
     SetTextureStageState(0, D3DTSS_ALPHAOP,  D3DTOP_MODULATE);
    end;

  deBrightAdd:
   with Device7 do
    begin
     SetRenderState(D3DRENDERSTATE_SRCBLEND,  D3DBLEND_SRCALPHA);
     SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA);
     SetTextureStageState(0, D3DTSS_COLOROP,  D3DTOP_MODULATE4X);
     SetTextureStageState(0, D3DTSS_ALPHAOP,  D3DTOP_MODULATE);
    end;

  deGrayscale:
   with Device7 do
    begin
     SetRenderState(D3DRENDERSTATE_TextureFactor, RGBA_MAKE(129, 255, 48, 255));
     SetTextureStageState(0, D3DTSS_COLOROP,   D3DTOP_DOTPRODUCT3);
     SetTextureStageState(0, D3DTSS_COLORARG2, D3DTA_TFACTOR);
     // Note: D3DTSS_COLORARG2 is changed after using this effect and will
     // need to be reset before rendering anything else.
    end;

  deLight:
   with Device7 do
    begin
     SetRenderState(D3DRENDERSTATE_SRCBLEND,  Cardinal(D3DBLEND_DESTCOLOR));
     SetRenderState(D3DRENDERSTATE_DESTBLEND, Cardinal(D3DBLEND_ONE));
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE2X);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deLightAdd:
   with Device7 do
    begin
     SetRenderState(D3DRENDERSTATE_SRCBLEND,  D3DBLEND_DESTCOLOR);
     SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_ONE);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE4X);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deAdd2X:
   with Device7 do
    begin
     SetRenderState(D3DRENDERSTATE_SRCBLEND,  D3DBLEND_SRCALPHA);
     SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_ONE);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE2X);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deOneColor:
   with Device7 do
    begin
     SetRenderState(D3DRENDERSTATE_SRCBLEND,  D3DBLEND_SRCALPHA);
     SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCALPHA);
     SetTextureStageState(0, D3DTSS_COLOROP, 25);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
     // Note: '25' is used by DraculaLin here, which is most likely incorrect
     // usage of D3DTSS_COLOROP.
    end;

  deXOR:
   with Device7 do
    begin
     SetRenderState(D3DRENDERSTATE_SRCBLEND,  D3DBLEND_INVDESTCOLOR);
     SetRenderState(D3DRENDERSTATE_DESTBLEND, D3DBLEND_INVSRCCOLOR);
    end;
 end;
end;

//---------------------------------------------------------------------------
function TDX7Canvas.RequestCache(Mode: TDrawingMode; Vertices, Indices: Integer;
 Effect: TDrawingEffect; Texture: TAsphyreCustomTexture): Boolean;
var
 NeedReset: Boolean;
begin
 Result:= (Vertices <= MaxCachedVertices)and(Indices <= MaxCachedIndices);
 if (not Result) then
  begin
   Errors.Insert(errGeometryTooComplex, Self, ClassName, 'RequestCache');
   Exit;
  end;

 NeedReset:= (VertexCount + Vertices > MaxCachedVertices);
 NeedReset:= (NeedReset)or(IndexCount + Indices > MaxCachedIndices);
 NeedReset:= (NeedReset)or(DrawingMode = dmUnknown)or(DrawingMode <> Mode);
 NeedReset:= (NeedReset)or(CachedEffect = deUnknown)or(CachedEffect <> Effect);
 NeedReset:= (NeedReset)or(CachedTex <> Texture);

 if (NeedReset) then
  begin
   Flush();

   if (CachedEffect = deUnknown)or(CachedEffect <> Effect) then
    SetEffectStates(Effect);

   if (CachedEffect = deUnknown)or(CachedTex <> Texture) then
    begin
     if (Texture <> nil) then Texture.Bind(0)
      else Device7.SetTexture(0, nil);
    end;

   DrawingMode := Mode;
   CachedEffect:= Effect;
   CachedTex   := Texture;
  end;
end;

//---------------------------------------------------------------------------
function TDX7Canvas.NextVertexEntry(): Pointer;
begin
 Result:= Pointer(PtrInt(VertexArray) + (VertexCount * SizeOf(TVertexRecord)));
end;

//---------------------------------------------------------------------------
procedure TDX7Canvas.AddIndexEntry(Index: Integer);
begin
 IndexArray[IndexCount]:= Index;
 Inc(IndexCount);
end;

//---------------------------------------------------------------------------
procedure TDX7Canvas.PutPixel(const Point: TPoint2; Color: Cardinal);
var
 Entry: PVertexRecord;
begin
 if (not RequestCache(dmPoints, 1, 0, deNormal, nil)) then Exit;

 Entry:= NextVertexEntry();
 Entry.Vertex.x:= Point.x;
 Entry.Vertex.y:= Point.y;
 Entry.Color   := Color;

 Inc(VertexCount);
 Inc(Primitives);
end;

//---------------------------------------------------------------------------
procedure TDX7Canvas.Line(const Src, Dest: TPoint2; Color0, Color1: Cardinal);
var
 Entry: PVertexRecord;
begin
 if (not RequestCache(dmLines, 2, 0, deNormal, nil)) then Exit;

 Entry:= NextVertexEntry();
 Entry.Vertex.x:= Src.x;
 Entry.Vertex.y:= Src.y;
 Entry.Color   := Color0;
 Inc(VertexCount);

 Entry:= NextVertexEntry();
 Entry.Vertex.x:= Dest.x;
 Entry.Vertex.y:= Dest.y;
 Entry.Color   := Color1;
 Inc(VertexCount);

 Inc(Primitives);
end;

//---------------------------------------------------------------------------
procedure TDX7Canvas.DrawIndexedTriangles(Vertices: PPoint2;
 Colors: PCardinal; Indices: PInteger; NoVertices, NoTriangles: Integer;
 Effect: TDrawingEffect = deNormal);
var
 Entry : PVertexRecord;
 Index : PInteger;
 Vertex: PPoint2;
 Color : PCardinal;
 i     : Integer;
begin
 if (not RequestCache(dmTriangles, NoVertices, NoTriangles * 3, Effect,
  nil)) then Exit;

 Index:= Indices;

 for i:= 0 to (NoTriangles * 3) - 1 do
  begin
   AddIndexEntry(VertexCount + Index^);

   Inc(Index);
  end;

 Vertex:= Vertices;
 Color := Colors;

 for i:= 0 to NoVertices - 1 do
  begin
   Entry:= NextVertexEntry();
   Entry.Vertex.x:= Vertex.x - 0.5;
   Entry.Vertex.y:= Vertex.y - 0.5;
   Entry.Color   := Color^;
   Inc(VertexCount);

   Inc(Vertex);
   Inc(Color);
  end;

 Inc(Primitives, NoTriangles);
end;

//---------------------------------------------------------------------------
procedure TDX7Canvas.UseTexture(Texture: TAsphyreCustomTexture;
 const Mapping: TPoint4);
begin
 ActiveTex  := Texture;
 QuadMapping:= Mapping;
end;

//---------------------------------------------------------------------------
procedure TDX7Canvas.TexMap(const Points: TPoint4; const Colors: TColor4;
 Effect: TDrawingEffect);
var
 Entry: PVertexRecord;
begin
 RequestCache(dmTriangles, 4, 6, Effect, ActiveTex);

 AddIndexEntry(VertexCount + 2);
 AddIndexEntry(VertexCount);
 AddIndexEntry(VertexCount + 1);

 AddIndexEntry(VertexCount + 3);
 AddIndexEntry(VertexCount + 2);
 AddIndexEntry(VertexCount + 1);

 Entry:= NextVertexEntry();
 Entry.Vertex.x:= Points[0].x - 0.5;
 Entry.Vertex.y:= Points[0].y - 0.5;
 Entry.Color   := Colors[0];
 Entry.u:= QuadMapping[0].x;
 Entry.v:= QuadMapping[0].y;
 Inc(VertexCount);

 Entry:= NextVertexEntry();
 Entry.Vertex.x:= Points[1].x - 0.5;
 Entry.Vertex.y:= Points[1].y - 0.5;
 Entry.Color   := Colors[1];
 Entry.u:= QuadMapping[1].x;
 Entry.v:= QuadMapping[1].y;
 Inc(VertexCount);

 Entry:= NextVertexEntry();
 Entry.Vertex.x:= Points[3].x - 0.5;
 Entry.Vertex.y:= Points[3].y - 0.5;
 Entry.Color   := Colors[3];
 Entry.u:= QuadMapping[3].x;
 Entry.v:= QuadMapping[3].y;
 Inc(VertexCount);

 Entry:= NextVertexEntry();
 Entry.Vertex.x:= Points[2].x - 0.5;
 Entry.Vertex.y:= Points[2].y - 0.5;
 Entry.Color   := Colors[2];
 Entry.u:= QuadMapping[2].x;
 Entry.v:= QuadMapping[2].y;
 Inc(VertexCount);

 Inc(Primitives, 2);
end;

//---------------------------------------------------------------------------
end.
