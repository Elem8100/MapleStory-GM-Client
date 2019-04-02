unit GLESRasterizer;
//---------------------------------------------------------------------------
// GLESCanvas.pas                                       Modified: 12-Nov-2010
// OpenGL ES canvas implementation                                Version 1.0
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
// The Original Code is GLESCanvas.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2000 - 2010,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Types, Vectors2, Vectors4, AsphyreTypes, AbstractRasterizer,
 AbstractTextures;

//---------------------------------------------------------------------------
const
 // The following parameters roughly affect the rendering performance. The
 // higher values means that more primitives will fit in cache, but it will
 // also occupy more bandwidth, even when few primitives are rendered.
 //
 // These parameters can be fine-tuned in a finished product to improve the
 // overall performance.
 MaxCachedIndices    = 2048;
 MaxCachedVertices   = 2048;

//---------------------------------------------------------------------------
type
 TCanvasCacheMode = (ccmNone, ccmPoint, ccmLine, ccmTriangle);

//---------------------------------------------------------------------------
 TGLESRasterizer = class(TAsphyreRasterizer)
 private
  FCacheMode  : TCanvasCacheMode;
  NormSize    : TPoint2;
  CachedEffect: TRasterEffect;
  CachedTex   : TAsphyreCustomTexture;
  ActiveTex   : TAsphyreCustomTexture;

  ClippingRect: TRect;
  ViewportRect: TRect;

  IndexBuffer  : Pointer;
  VertexBuffer : Pointer;
  ColorBuffer  : Pointer;
  TexCordBuffer: Pointer;

  VertexCount: Integer;
  IndexCount : Integer;

  procedure CreateStaticObjects();
  procedure DestroyStaticObjects();

  procedure DrawBuffers();
  procedure ResetScene();
  procedure RequestCache(Mode: TCanvasCacheMode; Vertices,
   Indices: Integer);

  procedure RequestEffect(Effect: TRasterEffect);
  procedure RequestTexture(Texture: TAsphyreCustomTexture);
  procedure InsertRawVertex(const Pos: TVector4);
  procedure InsertVertex(const Pos: TVector4; const TexCoord: TPoint2;
   Color: Cardinal);
  procedure InsertIndex(Value: Integer);
 protected
  function HandleDeviceCreate(): Boolean; override;
  procedure HandleDeviceDestroy(); override;

  procedure HandleBeginScene(); override;
  procedure HandleEndScene(); override;

  procedure GetViewport(out x, y, Width, Height: Integer); override;
  procedure SetViewport(x, y, Width, Height: Integer); override;
 public
  property CacheMode: TCanvasCacheMode read FCacheMode;

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

//---------------------------------------------------------------------------
uses
 AsphyreGLES;

//---------------------------------------------------------------------------
constructor TGLESRasterizer.Create();
begin
 inherited;

end;

//---------------------------------------------------------------------------
destructor TGLESRasterizer.Destroy();
begin
 DestroyStaticObjects();

 inherited;
end;

//---------------------------------------------------------------------------
procedure TGLESRasterizer.CreateStaticObjects();
begin
 ReallocMem(VertexBuffer, MaxCachedVertices * SizeOf(TPoint4));
 FillChar(VertexBuffer^, MaxCachedVertices * SizeOf(TPoint4), 0);

 ReallocMem(TexCordBuffer, MaxCachedVertices * SizeOf(TPoint2));
 FillChar(TexCordBuffer^, MaxCachedVertices * SizeOf(TPoint2), 0);

 ReallocMem(ColorBuffer, MaxCachedVertices * SizeOf(Longword));
 FillChar(ColorBuffer^, MaxCachedVertices * SizeOf(Longword), 0);

 ReallocMem(IndexBuffer, MaxCachedIndices * SizeOf(Word));
 FillChar(IndexBuffer^, MaxCachedIndices * SizeOf(Word), 0);
end;

//---------------------------------------------------------------------------
procedure TGLESRasterizer.DestroyStaticObjects();
begin
 if (IndexBuffer <> nil) then
  begin
   FreeMem(IndexBuffer);
   IndexBuffer:= nil;
  end;

 if (ColorBuffer <> nil) then
  begin
   FreeMem(ColorBuffer);
   ColorBuffer:= nil;
  end;

 if (TexCordBuffer <> nil) then
  begin
   FreeMem(TexCordBuffer);
   TexCordBuffer:= nil;
  end;

 if (VertexBuffer <> nil) then
  begin
   FreeMem(VertexBuffer);
   VertexBuffer:= nil;
  end;
end;

//--------------------------------------------------------------------------
function TGLESRasterizer.HandleDeviceCreate(): Boolean;
begin
 CreateStaticObjects();

 Result:= True;
end;

//--------------------------------------------------------------------------
procedure TGLESRasterizer.HandleDeviceDestroy();
begin
 DestroyStaticObjects();
end;

//---------------------------------------------------------------------------
procedure TGLESRasterizer.ResetStates();
var
 Viewport: array[0..3] of GLint;
begin
 FCacheMode  := ccmNone;
 CachedEffect:= reUnknown;
 CachedTex   := nil;
 ActiveTex   := nil;

 VertexCount:= 0;
 IndexCount := 0;

 glGetIntegerv(GL_VIEWPORT, @Viewport[0]);

 NormSize.x:= Viewport[2] * 0.5;
 NormSize.y:= Viewport[3] * 0.5;

 glMatrixMode(GL_MODELVIEW);
 glLoadIdentity();

 glMatrixMode(GL_PROJECTION);
 glLoadIdentity();

 glDisable(GL_DEPTH_TEST);
 glDisable(GL_TEXTURE_2D);
 glEnable(GL_LINE_SMOOTH);
 glDisable(GL_LIGHTING);

 glEnable(GL_ALPHA_TEST);
 glAlphaFunc(GL_GREATER, 0.001);

 glScissor(Viewport[0], Viewport[1], Viewport[2], Viewport[3]);
 glEnable(GL_SCISSOR_TEST);

 ClippingRect:= Bounds(Viewport[0], Viewport[1], Viewport[2], Viewport[3]);
 ViewportRect:= Bounds(Viewport[0], Viewport[1], Viewport[2], Viewport[3]);

 glEnableClientState(GL_VERTEX_ARRAY);
 glEnableClientState(GL_TEXTURE_COORD_ARRAY);
 glEnableClientState(GL_COLOR_ARRAY);
 glDisableClientState(GL_NORMAL_ARRAY);

	glBindBuffer(GL_ARRAY_BUFFER, 0);
end;

//---------------------------------------------------------------------------
procedure TGLESRasterizer.HandleBeginScene();
begin
 ResetStates();
end;

//---------------------------------------------------------------------------
procedure TGLESRasterizer.HandleEndScene();
begin
 Flush();
end;

//---------------------------------------------------------------------------
procedure TGLESRasterizer.GetViewport(out x, y, Width, Height: Integer);
begin
 x     := ClippingRect.Left;
 y     := ClippingRect.Top;
 Width := ClippingRect.Right - ClippingRect.Left;
 Height:= ClippingRect.Bottom - ClippingRect.Top;
end;

//---------------------------------------------------------------------------
procedure TGLESRasterizer.SetViewport(x, y, Width, Height: Integer);
begin
 ResetScene();

 glScissor(x, y, Width, Height);
 ClippingRect:= Bounds(x, y, Width, Height);
end;

//---------------------------------------------------------------------------
procedure TGLESRasterizer.DrawBuffers();
begin
	glColorPointer(4, GL_UNSIGNED_BYTE, 0, ColorBuffer);
 glTexCoordPointer(2, GL_FLOAT, 0, TexCordBuffer);
 glVertexPointer(4, GL_FLOAT, 0, VertexBuffer);

 case FCacheMode of
  ccmPoint:
   glDrawElements(GL_POINTS, IndexCount, GL_UNSIGNED_SHORT, IndexBuffer);

  ccmLine:
   glDrawElements(GL_LINES, IndexCount, GL_UNSIGNED_SHORT,
    IndexBuffer);

  ccmTriangle:
   glDrawElements(GL_TRIANGLES, IndexCount, GL_UNSIGNED_SHORT,
    IndexBuffer);
 end;
end;

//---------------------------------------------------------------------------
procedure TGLESRasterizer.ResetScene();
begin
 if (VertexCount > 0) then DrawBuffers();

 VertexCount:= 0;
 IndexCount := 0;

 FCacheMode:= ccmNone;
end;

//---------------------------------------------------------------------------
procedure TGLESRasterizer.Flush();
begin
 ResetScene();
 RequestEffect(reUnknown);
 RequestTexture(nil);
end;

//---------------------------------------------------------------------------
procedure TGLESRasterizer.RequestCache(Mode: TCanvasCacheMode; Vertices,
 Indices: Integer);
begin
 if (VertexCount + Vertices > MaxCachedVertices)or
  (IndexCount + Indices > MaxCachedIndices)or
  (FCacheMode = ccmNone)or(FCacheMode <> Mode) then ResetScene();

 FCacheMode:= Mode;
end;

//---------------------------------------------------------------------------
procedure TGLESRasterizer.RequestEffect(Effect: TRasterEffect);
begin
 if (CachedEffect = Effect) then Exit;

 ResetScene();

 if (Effect <> reUnknown) then glEnable(GL_BLEND)
  else glDisable(GL_BLEND);

 case Effect of
  reNormal:
   glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  reShadow:
   glBlendFunc(GL_ZERO, GL_ONE_MINUS_SRC_ALPHA);

  reAdd:
   glBlendFunc(GL_SRC_ALPHA, GL_ONE);

  reMultiply:
   glBlendFunc(GL_ZERO, GL_SRC_COLOR);

  reSrcAlphaAdd:
   glBlendFunc(GL_SRC_ALPHA, GL_ONE);

  reSrcColor:
   glBlendFunc(GL_SRC_COLOR, GL_ONE_MINUS_SRC_COLOR);

  reSrcColorAdd:
   glBlendFunc(GL_SRC_COLOR, GL_ONE);
 end;

 CachedEffect:= Effect;
end;

//---------------------------------------------------------------------------
procedure TGLESRasterizer.RequestTexture(Texture: TAsphyreCustomTexture);
begin
 if (CachedTex = Texture) then Exit;

 ResetScene();

 if (Texture <> nil) then
  begin
   Texture.Bind(0);

   glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,
    GL_LINEAR_MIPMAP_LINEAR);

   glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

   glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
   glEnable(GL_TEXTURE_2D);
  end else
  begin
   glBindTexture(GL_TEXTURE_2D, 0);
   glDisable(GL_TEXTURE_2D);
  end;

 CachedTex:= Texture;
end;

//---------------------------------------------------------------------------
procedure TGLESRasterizer.InsertRawVertex(const Pos: TVector4);
var
 NewPos: TPoint2;
 DestPt: PVector4;
begin
 NewPos.x:= (Pos.x - NormSize.x) / NormSize.x;
 NewPos.y:= (Pos.y - NormSize.y) / NormSize.y;

 DestPt:= Pointer(Integer(VertexBuffer) + (VertexCount * SizeOf(TVector4)));
 DestPt^.x:= NewPos.x / Pos.w;
 DestPt^.y:= NewPos.y / Pos.w;
 DestPt^.z:= 1.0 / Pos.w;
 DestPt^.w:= 1.0 / Pos.w;
end;

//---------------------------------------------------------------------------
procedure TGLESRasterizer.InsertVertex(const Pos: TVector4;
 const TexCoord: TPoint2; Color: Cardinal);
var
 DestCol: PCardinal;
 DestPt : PPoint2;
begin
 InsertRawVertex(Pos);

 DestCol:= Pointer(Integer(ColorBuffer) + (VertexCount * SizeOf(Cardinal)));
 DestCol^:= Color;

 DestPt:= Pointer(Integer(TexCordBuffer) + (VertexCount * SizeOf(TPoint2)));
 DestPt^:= TexCoord;

 Inc(VertexCount);
end;

//---------------------------------------------------------------------------
procedure TGLESRasterizer.InsertIndex(Value: Integer);
var
 DestIndx: PWord;
begin
 DestIndx:= Pointer(Integer(IndexBuffer) + (IndexCount * SizeOf(Word)));
 DestIndx^:= Value;

 Inc(IndexCount);
end;

//---------------------------------------------------------------------------
procedure TGLESRasterizer.FillTri(const Vtx0, Vtx1, Vtx2: TVector4; Diffuse0,
 Diffuse1, Diffuse2, Specular0, Specular1, Specular2: Cardinal;
 Effect: TRasterEffect);
var
 Index: Integer;
begin
 RequestEffect(Effect);
 RequestTexture(nil);
 RequestCache(ccmTriangle, 3, 3);

 Index:= VertexCount;

 InsertVertex(Vtx0, ZeroVec2, Diffuse0);
 InsertVertex(Vtx1, ZeroVec2, Diffuse1);
 InsertVertex(Vtx2, ZeroVec2, Diffuse2);

 InsertIndex(Index + 0);
 InsertIndex(Index + 1);
 InsertIndex(Index + 2);
end;

//---------------------------------------------------------------------------
procedure TGLESRasterizer.UseTexture(Texture: TAsphyreCustomTexture);
begin
 ActiveTex:= Texture;
end;

//---------------------------------------------------------------------------
procedure TGLESRasterizer.TexMap(const Vtx0, Vtx1, Vtx2: TVector4; Tex0, Tex1,
 Tex2: TPoint2; Diffuse0, Diffuse1, Diffuse2, Specular0, Specular1,
 Specular2: Cardinal; Effect: TRasterEffect);
var
 Index: Integer;
begin
 RequestEffect(Effect);
 RequestTexture(ActiveTex);
 RequestCache(ccmTriangle, 3, 3);

 Index:= VertexCount;

 InsertVertex(Vtx0, Tex0, Diffuse0);
 InsertVertex(Vtx1, Tex1, Diffuse1);
 InsertVertex(Vtx2, Tex2, Diffuse2);

 InsertIndex(Index + 0);
 InsertIndex(Index + 1);
 InsertIndex(Index + 2);
end;

//---------------------------------------------------------------------------
end.
