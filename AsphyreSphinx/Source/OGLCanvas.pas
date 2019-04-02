unit OGLCanvas;
//---------------------------------------------------------------------------
// OGLCanvas.pas                                        Modified: 24-Feb-2009
// OpenGL canvas implementation                                  Version 1.03
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
// The Original Code is OGLCanvas.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2000 - 2009,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Types, AbstractCanvas, Vectors2px, Vectors2, AsphyreTypes, AbstractTextures;

//---------------------------------------------------------------------------
type
 TCanvasCacheMode = (ccmNone, ccmPoints, ccmLines, ccmTris, ccmQuads);

//---------------------------------------------------------------------------
 TOGLCanvas = class(TAsphyreCanvas)
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

  procedure ResetScene();
  procedure RequestScene(Mode: TCanvasCacheMode);
  procedure RequestEffect(Effect: TDrawingEffect);
  procedure RequestTexture(Texture: TAsphyreCustomTexture);
  procedure AddVertexGL(x, y: Single);
  procedure AddPointGL(const Point: TPoint2; Color: Longword);
 protected
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
  procedure Draw(Texture: TAsphyreCustomTexture; X, Y, Scale: Real; MirrorX: Boolean;
    Red, Green, Blue, Alpha: Byte; Effect: TDrawingEffect = deNormal); override;
  procedure Flush(); override;
  procedure ResetStates(); override;

  constructor Create(); override;
 end;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 AsphyreGL, OGLDevices;

//---------------------------------------------------------------------------
constructor TOGLCanvas.Create();
begin
 inherited;

 FAntialias := True;
 FMipmapping:= False;
end;

//---------------------------------------------------------------------------
procedure TOGLCanvas.ResetStates();
var
 Viewport: array[0..3] of GLint;
begin
 FCacheMode  := ccmNone;
 CachedEffect:= deUnknown;
 CachedTex   := nil;
 ActiveTex   := nil;

 glGetIntegerv(GL_VIEWPORT, @Viewport[0]);

 NormSize.x:= Viewport[2] * 0.5;
 NormSize.y:= Viewport[3] * 0.5;

 glMatrixMode(GL_MODELVIEW);
 glLoadIdentity();

 glMatrixMode(GL_PROJECTION);
 glLoadIdentity();

 glDisable(GL_DEPTH_TEST);

 glDisable(GL_TEXTURE_1D);
 glDisable(GL_TEXTURE_2D);
 glEnable(GL_LINE_SMOOTH);

 if (GL_EXT_separate_specular_color)or(GL_VERSION_1_2) then
  glDisable(GL_COLOR_SUM_EXT);

 glEnable(GL_ALPHA_TEST);
 glAlphaFunc(GL_GREATER, 0.001);

 glScissor(Viewport[0], Viewport[1], Viewport[2], Viewport[3]);
 glEnable(GL_SCISSOR_TEST);

 ClippingRect:= Bounds(Viewport[0], Viewport[1], Viewport[2], Viewport[3]);
 ViewportRect:= Bounds(Viewport[0], Viewport[1], Viewport[2], Viewport[3]);
end;

//---------------------------------------------------------------------------
procedure TOGLCanvas.HandleBeginScene();
begin
 ResetStates();
end;

//---------------------------------------------------------------------------
procedure TOGLCanvas.HandleEndScene();
begin
 Flush();
end;

//---------------------------------------------------------------------------
function TOGLCanvas.GetAntialias(): Boolean;
begin
 Result:= FAntialias;
end;

//---------------------------------------------------------------------------
procedure TOGLCanvas.SetAntialias(const Value: Boolean);
begin
 FAntialias:= Value;
end;

//---------------------------------------------------------------------------
function TOGLCanvas.GetMipMapping(): Boolean;
begin
 Result:= FMipmapping;
end;

//---------------------------------------------------------------------------
procedure TOGLCanvas.SetMipMapping(const Value: Boolean);
begin
 FMipmapping:= Value;
end;

//---------------------------------------------------------------------------
procedure TOGLCanvas.GetViewport(out x, y, Width, Height: Integer);
begin
 x     := ClippingRect.Left;
 y     := ClippingRect.Top;
 Width := ClippingRect.Right - ClippingRect.Left;
 Height:= ClippingRect.Bottom - ClippingRect.Top;
end;

//---------------------------------------------------------------------------
procedure TOGLCanvas.SetViewport(x, y, Width, Height: Integer);
var
 ViewPos: Integer;
begin
 ResetScene();

 ViewPos:= (ViewportRect.Bottom - ViewportRect.Top) - (y + Height);

 glScissor(x, Viewpos, Width, Height);
 ClippingRect:= Bounds(x, y, Width, Height);
end;

//---------------------------------------------------------------------------
procedure TOGLCanvas.ResetScene();
begin
 if (FCacheMode <> ccmNone) then glEnd();
 FCacheMode:= ccmNone;
end;

//---------------------------------------------------------------------------
procedure TOGLCanvas.RequestScene(Mode: TCanvasCacheMode);
begin
 if (FCacheMode <> Mode) then
  begin
   ResetScene();

   case Mode of
    ccmPoints: glBegin(GL_POINTS);
    ccmLines : glBegin(GL_LINES);
    ccmTris  : glBegin(GL_TRIANGLES);
    ccmQuads : glBegin(GL_QUADS);
   end;

   FCacheMode:= Mode;
  end;
end;

//---------------------------------------------------------------------------
procedure TOGLCanvas.RequestEffect(Effect: TDrawingEffect);
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
procedure TOGLCanvas.RequestTexture(Texture: TAsphyreCustomTexture);
begin
 if (CachedTex = Texture) then Exit;

 ResetScene();

 if (Texture <> nil) then
  begin
   Texture.Bind(0);

   if (FAntialias) then
    begin
     if (FMipmapping)and(Texture.Mipmapping)and(GL_VERSION_1_4) then
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
     if (FMipmapping)and(Texture.Mipmapping)and(GL_VERSION_1_4) then
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
procedure TOGLCanvas.AddVertexGL(x, y: Single);
var
 xNorm, yNorm: Single;
begin
 xNorm:= (x - NormSize.x) / NormSize.x;
 yNorm:= (y - NormSize.y) / NormSize.y;

 if (UsingFrameBuffer) then glVertex2f(xNorm, yNorm)
  else glVertex2f(xNorm, -yNorm);
end;

//---------------------------------------------------------------------------
procedure TOGLCanvas.AddPointGL(const Point: TPoint2; Color: Longword);
var
 Colors: array[0..3] of Single;
begin
 Colors[0]:= ((Color shr 16) and $FF) / 255.0;
 Colors[1]:= ((Color shr 8) and $FF) / 255.0;
 Colors[2]:= (Color and $FF) / 255.0;
 Colors[3]:= ((Color shr 24) and $FF) / 255.0;

 glColor4fv(@Colors[0]);
 AddVertexGL(Point.x, Point.y);
end;

//---------------------------------------------------------------------------
procedure TOGLCanvas.PutPixel(const Point: TPoint2; Color: Cardinal);
begin
 RequestEffect(deNormal);
 RequestTexture(nil);
 RequestScene(ccmPoints);

 AddPointGL(Point, Color);
end;

//---------------------------------------------------------------------------
procedure TOGLCanvas.Line(const Src, Dest: TPoint2; Color0, Color1: Cardinal);
begin
 RequestEffect(deNormal);
 RequestTexture(nil);
 RequestScene(ccmLines);

 AddPointGL(Src + Point2(0.5, 0.5), Color0);
 AddPointGL(Dest + Point2(0.5, 0.5), Color1);
end;

//---------------------------------------------------------------------------
procedure TOGLCanvas.DrawIndexedTriangles(Vertices: PPoint2; Colors: PCardinal;
 Indices: PInteger; NoVertices, NoTriangles: Integer; Effect: TDrawingEffect);
var
 i, i0, i1, i2: Integer;
 Vertex: PPoint2;
 Color : PCardinal;
begin
 RequestEffect(Effect);
 RequestTexture(nil);
 RequestScene(ccmTris);

 for i:= 0 to NoTriangles - 1 do
  begin
   i0:= Indices^; Inc(Indices);

   i1:= Indices^; Inc(Indices);

   i2:= Indices^; Inc(Indices);

   // Vertex 0
   Vertex:= Vertices;
   Inc(Vertex, i0);

   Color:= Colors;
   Inc(Color, i0);

   AddPointGL(Vertex^, Color^);

   // Vertex 1
   Vertex:= Vertices;
   Inc(Vertex, i1);

   Color:= Colors;
   Inc(Color, i1);

   AddPointGL(Vertex^, Color^);

   // Vertex 2
   Vertex:= Vertices;
   Inc(Vertex, i2);

   Color:= Colors;
   Inc(Color, i2);

   AddPointGL(Vertex^, Color^);
  end;
end;

//---------------------------------------------------------------------------
procedure TOGLCanvas.UseTexture(Texture: TAsphyreCustomTexture;
 const Mapping: TPoint4);
begin
 ActiveTex  := Texture;
 QuadMapping:= Mapping;
end;

//---------------------------------------------------------------------------
procedure TOGLCanvas.TexMap(const Points: TPoint4; const Colors: TColor4;
 Effect: TDrawingEffect);
begin
 RequestEffect(Effect);
 RequestTexture(ActiveTex);
 RequestScene(ccmQuads);

 glTexCoord2f(QuadMapping[3].x, QuadMapping[3].y);
 AddPointGL(Points[3], Colors[3]);

 glTexCoord2f(QuadMapping[0].x, QuadMapping[0].y);
 AddPointGL(Points[0], Colors[0]);

 glTexCoord2f(QuadMapping[1].x, QuadMapping[1].y);
 AddPointGL(Points[1], Colors[1]);

 glTexCoord2f(QuadMapping[2].x, QuadMapping[2].y);
 AddPointGL(Points[2], Colors[2]);

 ResetScene();
end;

//---------------------------------------------------------------------------
procedure TOGLCanvas.Flush();
begin
 ResetScene();
 RequestEffect(deUnknown);
 RequestTexture(nil);
end;
procedure TOGLCanvas.Draw(Texture: TAsphyreCustomTexture; X, Y, Scale: Real; MirrorX: Boolean;
   Red, Green, Blue, Alpha: Byte; Effect: TDrawingEffect = deNormal);
var
  P: TPoint4;
begin
  if MirrorX then
  begin
    P[0] := Point2(X, Y);
    P[1] := Point2(X + Texture.Width, Y);
    P[2] := Point2(X + Texture.Width, Y + Texture.Height);
    P[3] := Point2(X, Y + Texture.Height);
    UseTexturePx(Texture, pxBounds4(0, 0, Texture.Width, Texture.Height));
    TexMap(pMirror4(P), cRGB4(Red, Green, Blue, Alpha), Effect);

  end
  else
  begin
    UseTexturePx(Texture, pxBounds4(0, 0, Texture.Width, Texture.Height));
    TexMap(pBounds4s(X, Y, Texture.Width, Texture.Height, Scale),
     cRGB4(Red, Green, Blue, Alpha), Effect);
  end;
end;
//---------------------------------------------------------------------------
end.
