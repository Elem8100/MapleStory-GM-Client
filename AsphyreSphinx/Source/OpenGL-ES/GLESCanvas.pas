unit GLESCanvas;
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
 Types, AbstractCanvas, Vectors2, AsphyreTypes, AbstractTextures;

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
 TGLESCanvas = class(TAsphyreCanvas)
 private
  FCacheMode  : TCanvasCacheMode;
  NormSize    : TPoint2;
  CachedEffect: TDrawingEffect;
  CachedTex   : TAsphyreCustomTexture;
  ActiveTex   : TAsphyreCustomTexture;
  QuadMapping : TPoint4;
  FAntialias  : Boolean;
  FMipmapping : Boolean;

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

  procedure RequestEffect(Effect: TDrawingEffect);
  procedure RequestTexture(Texture: TAsphyreCustomTexture);
  procedure InsertRawVertex(const Pos: TPoint2);
  procedure InsertVertex(const Pos, TexCoord: TPoint2; Color: Cardinal);
  procedure InsertIndex(Value: Integer);
 protected
  function HandleDeviceCreate(): Boolean; override;
  procedure HandleDeviceDestroy(); override;

  procedure HandleBeginScene(); override;
  procedure HandleEndScene(); override;

  procedure GetViewport(out x, y, Width, Height: Integer); override;
  procedure SetViewport(x, y, Width, Height: Integer); override;

  function GetAntialias(): Boolean; override;
  procedure SetAntialias(const Value: Boolean); override;
  function GetMipMapping(): Boolean; override;
  procedure SetMipMapping(const Value: Boolean); override;
 public
  property CacheMode: TCanvasCacheMode read FCacheMode;

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
constructor TGLESCanvas.Create();
begin
 inherited;

 FAntialias := True;
 FMipmapping:= False;
end;

//---------------------------------------------------------------------------
destructor TGLESCanvas.Destroy();
begin
 DestroyStaticObjects();

 inherited;
end;

//---------------------------------------------------------------------------
procedure TGLESCanvas.CreateStaticObjects();
begin
 ReallocMem(VertexBuffer, MaxCachedVertices * SizeOf(TPoint2));
 FillChar(VertexBuffer^, MaxCachedVertices * SizeOf(TPoint2), 0);

 ReallocMem(TexCordBuffer, MaxCachedVertices * SizeOf(TPoint2));
 FillChar(TexCordBuffer^, MaxCachedVertices * SizeOf(TPoint2), 0);

 ReallocMem(ColorBuffer, MaxCachedVertices * SizeOf(Longword));
 FillChar(ColorBuffer^, MaxCachedVertices * SizeOf(Longword), 0);

 ReallocMem(IndexBuffer, MaxCachedIndices * SizeOf(Word));
 FillChar(IndexBuffer^, MaxCachedIndices * SizeOf(Word), 0);
end;

//---------------------------------------------------------------------------
procedure TGLESCanvas.DestroyStaticObjects();
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
function TGLESCanvas.HandleDeviceCreate(): Boolean;
begin
 CreateStaticObjects();

 Result:= True;
end;

//--------------------------------------------------------------------------
procedure TGLESCanvas.HandleDeviceDestroy();
begin
 DestroyStaticObjects();
end;

//---------------------------------------------------------------------------
procedure TGLESCanvas.ResetStates();
var
 Viewport: array[0..3] of GLint;
begin
 FCacheMode  := ccmNone;
 CachedEffect:= deUnknown;
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
procedure TGLESCanvas.HandleBeginScene();
begin
 ResetStates();
end;

//---------------------------------------------------------------------------
procedure TGLESCanvas.HandleEndScene();
begin
 Flush();
end;

//---------------------------------------------------------------------------
function TGLESCanvas.GetAntialias(): Boolean;
begin
 Result:= FAntialias;
end;

//---------------------------------------------------------------------------
procedure TGLESCanvas.SetAntialias(const Value: Boolean);
begin
 FAntialias:= Value;
end;

//---------------------------------------------------------------------------
function TGLESCanvas.GetMipMapping(): Boolean;
begin
 Result:= FMipmapping;
end;

//---------------------------------------------------------------------------
procedure TGLESCanvas.SetMipMapping(const Value: Boolean);
begin
 FMipmapping:= Value;
end;

//---------------------------------------------------------------------------
procedure TGLESCanvas.GetViewport(out x, y, Width, Height: Integer);
begin
 x     := ClippingRect.Left;
 y     := ClippingRect.Top;
 Width := ClippingRect.Right - ClippingRect.Left;
 Height:= ClippingRect.Bottom - ClippingRect.Top;
end;

//---------------------------------------------------------------------------
procedure TGLESCanvas.SetViewport(x, y, Width, Height: Integer);
begin
 ResetScene();

 glScissor(x, y, Width, Height);
 ClippingRect:= Bounds(x, y, Width, Height);
end;

//---------------------------------------------------------------------------
procedure TGLESCanvas.DrawBuffers();
begin
	glColorPointer(4, GL_UNSIGNED_BYTE, 0, ColorBuffer);
 glTexCoordPointer(2, GL_FLOAT, 0, TexCordBuffer);
 glVertexPointer(2, GL_FLOAT, 0, VertexBuffer);

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
procedure TGLESCanvas.ResetScene();
begin
 if (VertexCount > 0) then DrawBuffers();

 VertexCount:= 0;
 IndexCount := 0;

 FCacheMode:= ccmNone;
end;

//---------------------------------------------------------------------------
procedure TGLESCanvas.Flush();
begin
 ResetScene();
 RequestEffect(deUnknown);
 RequestTexture(nil);
end;

//---------------------------------------------------------------------------
procedure TGLESCanvas.RequestCache(Mode: TCanvasCacheMode; Vertices,
 Indices: Integer);
begin
 if (VertexCount + Vertices > MaxCachedVertices)or
  (IndexCount + Indices > MaxCachedIndices)or
  (FCacheMode = ccmNone)or(FCacheMode <> Mode) then ResetScene();

 FCacheMode:= Mode;
end;

//---------------------------------------------------------------------------
procedure TGLESCanvas.RequestEffect(Effect: TDrawingEffect);
begin
 if (CachedEffect = Effect) then Exit;

 ResetScene();

 if (Effect <> deUnknown) then glEnable(GL_BLEND)
  else glDisable(GL_BLEND);

 case Effect of
  deNormal:
   glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  deShadow:
   glBlendFunc(GL_ZERO, GL_ONE_MINUS_SRC_ALPHA);

  deAdd:
   glBlendFunc(GL_SRC_ALPHA, GL_ONE);

  deMultiply:
   glBlendFunc(GL_ZERO, GL_SRC_COLOR);

  deSrcAlphaAdd:
   glBlendFunc(GL_SRC_ALPHA, GL_ONE);

  deSrcColor:
   glBlendFunc(GL_SRC_COLOR, GL_ONE_MINUS_SRC_COLOR);

  deSrcColorAdd:
   glBlendFunc(GL_SRC_COLOR, GL_ONE);

  deInvert:
   glBlendFunc(GL_ONE_MINUS_DST_COLOR, GL_ZERO);

  deSrcBright:
   glBlendFunc(GL_SRC_COLOR, GL_SRC_COLOR);

  deInvMultiply:
   glBlendFunc(GL_ZERO, GL_ONE_MINUS_SRC_COLOR);

  deMultiplyAlpha:
   glBlendFunc(GL_ZERO, GL_SRC_ALPHA);

  deInvMultiplyAlpha:
   glBlendFunc(GL_ZERO, GL_ONE_MINUS_SRC_ALPHA);

  deDestBright:
   glBlendFunc(GL_DST_COLOR, GL_DST_COLOR);

  deInvSrcBright:
   glBlendFunc(GL_ONE_MINUS_SRC_COLOR, GL_ONE_MINUS_SRC_COLOR);

  deInvDestBright:
   glBlendFunc(GL_ONE_MINUS_DST_COLOR, GL_ONE_MINUS_DST_COLOR);

  deXOR:
   glBlendFunc(GL_ONE_MINUS_SRC_COLOR, GL_ONE_MINUS_DST_COLOR);
 end;

 CachedEffect:= Effect;
end;

//---------------------------------------------------------------------------
procedure TGLESCanvas.RequestTexture(Texture: TAsphyreCustomTexture);
begin
 if (CachedTex = Texture) then Exit;

 ResetScene();

 if (Texture <> nil) then
  begin
   Texture.Bind(0);

   if (FAntialias) then
    begin
     if (FMipmapping)and(Texture.Mipmapping) then
      begin
       glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,
        GL_LINEAR_MIPMAP_LINEAR);
      end else
      begin
       glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      end;

     glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    end else
    begin
     if (FMipmapping)and(Texture.Mipmapping) then
      begin
       glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,
        GL_LINEAR_MIPMAP_NEAREST);
      end else
      begin
       glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      end;

     glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    end;

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
procedure TGLESCanvas.InsertRawVertex(const Pos: TPoint2);
var
 NewPos: TPoint2;
 DestPt: PPoint2;
begin
 NewPos.x:= (Pos.x - NormSize.x) / NormSize.x;
 NewPos.y:= (Pos.y - NormSize.y) / NormSize.y;

 DestPt:= Pointer(Integer(VertexBuffer) + (VertexCount * SizeOf(TPoint2)));
 DestPt^:= NewPos;
end;

//---------------------------------------------------------------------------
procedure TGLESCanvas.InsertVertex(const Pos, TexCoord: TPoint2;
 Color: Cardinal);
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
procedure TGLESCanvas.InsertIndex(Value: Integer);
var
 DestIndx: PWord;
begin
 DestIndx:= Pointer(Integer(IndexBuffer) + (IndexCount * SizeOf(Word)));
 DestIndx^:= Value;

 Inc(IndexCount);
end;

//---------------------------------------------------------------------------
procedure TGLESCanvas.PutPixel(const Point: TPoint2; Color: Cardinal);
var
 Index: Integer;
begin
 RequestEffect(deNormal);
 RequestTexture(nil);
 RequestCache(ccmPoint, 1, 1);

 Index:= VertexCount;

 InsertVertex(Point + Point2(0.5, 0.5), ZeroVec2, Color);
 InsertIndex(Index);
end;

//---------------------------------------------------------------------------
procedure TGLESCanvas.Line(const Src, Dest: TPoint2; Color0, Color1: Cardinal);
var
 Index: Integer;
begin
 RequestEffect(deNormal);
 RequestTexture(nil);
 RequestCache(ccmLine, 2, 2);

 Index:= VertexCount;

 InsertVertex(Src + Point2(0.5, 0.5), ZeroVec2, Color0);
 InsertVertex(Dest + Point2(0.5, 0.5), ZeroVec2, Color1);

 InsertIndex(Index);
 InsertIndex(Index + 1);
end;

//---------------------------------------------------------------------------
procedure TGLESCanvas.DrawIndexedTriangles(Vertices: PPoint2; Colors: PCardinal;
 Indices: PInteger; NoVertices, NoTriangles: Integer; Effect: TDrawingEffect);
var
 Index : PInteger;
 Vertex: PPoint2;
 Color : PCardinal;
 i     : Integer;
begin
 RequestEffect(Effect);
 RequestTexture(nil);
 RequestCache(ccmTriangle, NoVertices, NoTriangles * 3);

 Index:= Indices;

 for i:= 0 to (NoTriangles * 3) - 1 do
  begin
   InsertIndex(VertexCount + Index^);
   Inc(Index);
  end;

 Vertex:= Vertices;
 Color := Colors;

 for i:= 0 to NoVertices - 1 do
  begin
   InsertVertex(Vertex^, ZeroVec2, Color^);

   Inc(Vertex);
   Inc(Color);
  end;
end;

//---------------------------------------------------------------------------
procedure TGLESCanvas.UseTexture(Texture: TAsphyreCustomTexture;
 const Mapping: TPoint4);
begin
 ActiveTex  := Texture;
 QuadMapping:= Mapping;
end;

//---------------------------------------------------------------------------
procedure TGLESCanvas.TexMap(const Points: TPoint4; const Colors: TColor4;
 Effect: TDrawingEffect);
var
 Index: Integer;
begin
 RequestEffect(Effect);
 RequestTexture(ActiveTex);
 RequestCache(ccmTriangle, 4, 6);

 Index:= VertexCount;

 InsertVertex(Points[0], QuadMapping[0], Colors[0]);
 InsertVertex(Points[1], QuadMapping[1], Colors[1]);
 InsertVertex(Points[3], QuadMapping[3], Colors[3]);
 InsertVertex(Points[2], QuadMapping[2], Colors[2]);

 InsertIndex(Index + 2);
 InsertIndex(Index + 0);
 InsertIndex(Index + 1);

 InsertIndex(Index + 3);
 InsertIndex(Index + 2);
 InsertIndex(Index + 1);
end;

//---------------------------------------------------------------------------
end.
