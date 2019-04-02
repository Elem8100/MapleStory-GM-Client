unit OGLRasterizer;
//---------------------------------------------------------------------------
// OGLRasterizer.pas                                    Modified: 02-Feb-2009
// 3D Software Rasterizer using OpenGL for Asphyre                Version 1.0
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
// The Original Code is OGLRasterizer.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2000 - 2009,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 AbstractRasterizer, Vectors2, Vectors4, AbstractTextures;

//---------------------------------------------------------------------------
type
 TOGLRasterizer = class(TAsphyreRasterizer)
 private
  NormSize    : TPoint2;
  SceneBegan  : Boolean;

  ActiveTex   : TAsphyreCustomTexture;
  CachedTex   : TAsphyreCustomTexture;
  CachedEffect: TRasterEffect;

  procedure ResetScene();
  procedure RequestScene();
  procedure RequestEffect(Effect: TRasterEffect);
  procedure RequestTexture(Texture: TAsphyreCustomTexture);
  procedure AddVertexGL(const Vtx: TVector4);
  procedure AddPointGL(const Vtx: TVector4; Color, Specular: Longword);
 protected
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
 end;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 AsphyreGL;

//---------------------------------------------------------------------------
procedure TOGLRasterizer.ResetStates();
var
 Viewport: array[0..3] of GLint;
begin
 CachedEffect:= reUnknown;
 CachedTex   := nil;
 ActiveTex   := nil;
 SceneBegan  := False;

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
  begin
   // Enable specular highlights
   glEnable(GL_COLOR_SUM_EXT);
  end;
end;

//--------------------------------------------------------------------------
procedure TOGLRasterizer.HandleBeginScene();
begin
 ResetStates();
end;

//--------------------------------------------------------------------------
procedure TOGLRasterizer.HandleEndScene();
begin
 Flush();
end;

//---------------------------------------------------------------------------
procedure TOGLRasterizer.GetViewport(out x, y, Width, Height: Integer);
var
 Viewport: array[0..3] of GLint;
begin
 glGetIntegerv(GL_VIEWPORT, @Viewport[0]);

 x     := Viewport[0];
 y     := Viewport[1];
 Width := Viewport[2];
 Height:= Viewport[3];
end;

//---------------------------------------------------------------------------
procedure TOGLRasterizer.SetViewport(x, y, Width, Height: Integer);
begin
 ResetScene();
 glViewport(x, y, Width, Height);
end;

//---------------------------------------------------------------------------
procedure TOGLRasterizer.ResetScene();
begin
 if (SceneBegan) then
  begin
   glEnd();
   SceneBegan:= False;
  end;
end;

//---------------------------------------------------------------------------
procedure TOGLRasterizer.RequestScene();
begin
 if (not SceneBegan) then
  begin
   glBegin(GL_TRIANGLES);
   SceneBegan:= True;
  end;
end;

//---------------------------------------------------------------------------
procedure TOGLRasterizer.RequestEffect(Effect: TRasterEffect);
begin
 if (Effect = CachedEffect) then Exit;

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
procedure TOGLRasterizer.RequestTexture(Texture: TAsphyreCustomTexture);
begin
 if (CachedTex = Texture) then Exit;

 ResetScene();

 if (Texture <> nil) then
  begin
   Texture.Bind(0);

   if (Texture.Mipmapping)and(GL_VERSION_1_4) then
    begin
     glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,
      GL_LINEAR_MIPMAP_LINEAR);
    end else
    begin
     glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    end;

   glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

   glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
   glEnable(GL_TEXTURE_2D);
  end else glDisable(GL_TEXTURE_2D);

 CachedTex:= Texture;
end;

//---------------------------------------------------------------------------
procedure TOGLRasterizer.Flush();
begin
 ResetScene();
 RequestEffect(reUnknown);
 RequestTexture(nil);
end;

//---------------------------------------------------------------------------
procedure TOGLRasterizer.AddVertexGL(const Vtx: TVector4);
var
 xNorm, yNorm: Single;
begin
 xNorm:= (Vtx.x - NormSize.x) / NormSize.x;
 yNorm:= (Vtx.y - NormSize.y) / NormSize.y;
 glVertex4f(xNorm / Vtx.w, -yNorm / Vtx.w, 1.0 / Vtx.w, 1.0 / Vtx.w);
end;

//---------------------------------------------------------------------------
procedure TOGLRasterizer.AddPointGL(const Vtx: TVector4; Color,
 Specular: Longword);
var
 Colors: array[0..3] of Single;
begin
 Colors[0]:= ((Color shr 16) and $FF) / 255.0;
 Colors[1]:= ((Color shr 8) and $FF) / 255.0;
 Colors[2]:= (Color and $FF) / 255.0;
 Colors[3]:= ((Color shr 24) and $FF) / 255.0;
 glColor4fv(@Colors[0]);

 if (GL_EXT_separate_specular_color) then
  begin
   Colors[0]:= ((Specular shr 16) and $FF) / 255.0;
   Colors[1]:= ((Specular shr 8) and $FF) / 255.0;
   Colors[2]:= (Specular and $FF) / 255.0;
   Colors[3]:= ((Specular shr 24) and $FF) / 255.0;

   glSecondaryColor3fvEXT(@Colors[0]);
  end else
 if (GL_VERSION_1_4) then
  begin
   Colors[0]:= ((Specular shr 16) and $FF) / 255.0;
   Colors[1]:= ((Specular shr 8) and $FF) / 255.0;
   Colors[2]:= (Specular and $FF) / 255.0;
   Colors[3]:= ((Specular shr 24) and $FF) / 255.0;

   glSecondaryColor3fv(@Colors[0]);
  end;

 AddVertexGL(Vtx);
end;

//---------------------------------------------------------------------------
procedure TOGLRasterizer.FillTri(const Vtx0, Vtx1, Vtx2: TVector4; Diffuse0,
 Diffuse1, Diffuse2, Specular0, Specular1, Specular2: Cardinal;
 Effect: TRasterEffect);
begin
 RequestEffect(Effect);
 RequestTexture(nil);
 RequestScene();

 AddPointGL(Vtx0, Diffuse0, Specular0);
 AddPointGL(Vtx1, Diffuse1, Specular1);
 AddPointGL(Vtx2, Diffuse2, Specular2);
end;

//---------------------------------------------------------------------------
procedure TOGLRasterizer.UseTexture(Texture: TAsphyreCustomTexture);
begin
 ActiveTex:= Texture;
end;

//---------------------------------------------------------------------------
procedure TOGLRasterizer.TexMap(const Vtx0, Vtx1, Vtx2: TVector4; Tex0, Tex1,
 Tex2: TPoint2; Diffuse0, Diffuse1, Diffuse2, Specular0, Specular1,
 Specular2: Cardinal; Effect: TRasterEffect);
begin
 RequestEffect(Effect);
 RequestTexture(ActiveTex);
 RequestScene();

 glTexCoord2f(Tex0.x, Tex0.y);
 AddPointGL(Vtx0, Diffuse0, Specular0);

 glTexCoord2f(Tex1.x, Tex1.y);
 AddPointGL(Vtx1, Diffuse1, Specular1);

 glTexCoord2f(Tex2.x, Tex2.y);
 AddPointGL(Vtx2, Diffuse2, Specular2);
end;

//---------------------------------------------------------------------------
end.
