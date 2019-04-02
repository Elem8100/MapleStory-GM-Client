unit AsphyreBillboards;
//---------------------------------------------------------------------------
// AsphyreBillboards.pas                                Modified: 25-Jan-2009
// 3D billboard engine for Asphyre                                Version 1.0
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
// The Original Code is AsphyreBillboards.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2000 - 2009,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Types, SysUtils, Vectors2, Vectors3, Vectors4, Matrices4, AsphyreTypes,
 AbstractTextures, AsphyreImages, AsphyreBillboardTypes, AsphyreScenes;

//---------------------------------------------------------------------------
type
 TAsphyreBillboards = class
 private
  FEntries : TBillboardEntries;
  FVertices: TVectors4;

  ActiveTex: TAsphyreCustomTexture;
  TxMapping: TPoint4;
 public
  property Entries : TBillboardEntries read FEntries;
  property Vertices: TVectors4 read FVertices;

  procedure UseTexture(Texture: TAsphyreCustomTexture;
   const Mapping: TPoint4);
  procedure UseTexturePx(Texture: TAsphyreCustomTexture;
   const Mapping: TPoint4px); overload;
  procedure UseTexturePx(Texture: TAsphyreCustomTexture;
   const Mapping: TPoint4); overload;

  procedure UseImage(Image: TAsphyreImage; const Mapping: TPoint4;
   TextureNo: Integer = 0);

  procedure UseImagePt(Image: TAsphyreImage; Pattern: Integer); overload;
  procedure UseImagePt(Image: TAsphyreImage; Pattern: Integer;
   const SrcRect: TRect; Mirror: Boolean = False;
   Flip: Boolean = False); overload;

  procedure UseImagePx(Image: TAsphyreImage; const Mapping: TPoint4px;
   TextureNo: Integer = 0); overload;
  procedure UseImagePx(Image: TAsphyreImage; const Mapping: TPoint4;
   TextureNo: Integer = 0); overload;

  procedure TexMap(const Position: TVector3; const Size: TPoint2; Phi: Single;
   Color: Longword);

  procedure Render(Scene: TAsphyreScene; ViewMtx: PMatrix4);

  constructor Create();
  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
constructor TAsphyreBillboards.Create();
begin
 inherited;

 FEntries := TBillboardEntries.Create();
 FVertices:= TVectors4.Create();
end;

//---------------------------------------------------------------------------
destructor TAsphyreBillboards.Destroy();
begin
 FreeAndNil(FVertices);
 FreeAndNil(FEntries);

 inherited;
end;

//---------------------------------------------------------------------------
procedure TAsphyreBillboards.UseTexture(Texture: TAsphyreCustomTexture;
 const Mapping: TPoint4);
begin
 ActiveTex:= Texture;
 TxMapping:= Mapping;
end;

//---------------------------------------------------------------------------
procedure TAsphyreBillboards.UseTexturePx(Texture: TAsphyreCustomTexture;
 const Mapping: TPoint4px);
var
 Points: TPoint4;
begin
 if (Texture <> nil) then
  begin
   Points[0]:= Texture.PixelToLogical(Mapping[0]);
   Points[1]:= Texture.PixelToLogical(Mapping[1]);
   Points[2]:= Texture.PixelToLogical(Mapping[2]);
   Points[3]:= Texture.PixelToLogical(Mapping[3]);

   UseTexture(Texture, Points);
  end else UseTexture(Texture, TexFull4);
end;

//---------------------------------------------------------------------------
procedure TAsphyreBillboards.UseTexturePx(Texture: TAsphyreCustomTexture;
 const Mapping: TPoint4);
var
 Points: TPoint4;
begin
 if (Texture <> nil) then
  begin
   Points[0]:= Texture.PixelToLogical(Mapping[0]);
   Points[1]:= Texture.PixelToLogical(Mapping[1]);
   Points[2]:= Texture.PixelToLogical(Mapping[2]);
   Points[3]:= Texture.PixelToLogical(Mapping[3]);

   UseTexture(Texture, Points);
  end else UseTexture(Texture, TexFull4);
end;

//---------------------------------------------------------------------------
procedure TAsphyreBillboards.UseImage(Image: TAsphyreImage;
 const Mapping: TPoint4; TextureNo: Integer);
var
 Texture: TAsphyreCustomTexture;
begin
 if (Image <> nil) then Texture:= Image.Texture[TextureNo]
  else Texture:= nil;

 UseTexture(Texture, Mapping);
end;

//---------------------------------------------------------------------------
procedure TAsphyreBillboards.UseImagePx(Image: TAsphyreImage;
 const Mapping: TPoint4px; TextureNo: Integer);
var
 Texture: TAsphyreCustomTexture;
begin
 if (Image <> nil) then Texture:= Image.Texture[TextureNo]
  else Texture:= nil;

 UseTexturePx(Texture, Mapping);
end;

//---------------------------------------------------------------------------
procedure TAsphyreBillboards.UseImagePx(Image: TAsphyreImage;
 const Mapping: TPoint4; TextureNo: Integer);
var
 Texture: TAsphyreCustomTexture;
begin
 if (Image <> nil) then Texture:= Image.Texture[TextureNo]
  else Texture:= nil;

 UseTexturePx(Texture, Mapping);
end;

//---------------------------------------------------------------------------
procedure TAsphyreBillboards.UseImagePt(Image: TAsphyreImage; Pattern: Integer);
var
 Mapping  : TPoint4;
 TextureNo: Integer;
begin
 TextureNo:= -1;
 if (Image <> nil) then TextureNo:= Image.RetreiveTex(Pattern, Mapping);

 UseImage(Image, Mapping, TextureNo);
end;

//---------------------------------------------------------------------------
procedure TAsphyreBillboards.UseImagePt(Image: TAsphyreImage; Pattern: Integer;
 const SrcRect: TRect; Mirror, Flip: Boolean);
var
 Mapping  : TPoint4;
 TextureNo: Integer;
begin
 TextureNo:= -1;

 if (Image <> nil) then
  TextureNo:= Image.RetreiveTex(Pattern, SrcRect, Mirror, Flip, Mapping);

 UseImage(Image, Mapping, TextureNo);
end;

//---------------------------------------------------------------------------
procedure TAsphyreBillboards.TexMap(const Position: TVector3;
 const Size: TPoint2; Phi: Single; Color: Longword);
var
 Entry: TBillboardEntry;
begin
 Entry.Texture:= ActiveTex;
 Entry.Mapping:= TxMapping;
 Entry.Size   := Size;
 Entry.Phi    := Phi;
 Entry.Color  := Color;

 FEntries.Insert(Entry);
 FVertices.Add(Position);
end;

//---------------------------------------------------------------------------
procedure TAsphyreBillboards.Render(Scene: TAsphyreScene; ViewMtx: PMatrix4);
var
 i: Integer;
 Vtx   : TVector4;
 Entry : PBillboardEntry;
 Quad  : array[0..3] of TVector4;
 Points: TPoint4;
begin
 FVertices.CopyTransform(FVertices, ViewMtx);

 for i:= 0 to FEntries.Count - 1 do
  begin
   Vtx  := FVertices[i];
   Entry:= FEntries[i];

   Points:= pRotate4c(ZeroVec2, Entry^.Size, Entry^.Phi, 1.0);

   Quad[0]:= Vector4(Vtx.x + Points[0].x, Vtx.y + Points[0].y, Vtx.z);
   Quad[1]:= Vector4(Vtx.x + Points[1].x, Vtx.y + Points[1].y, Vtx.z);
   Quad[2]:= Vector4(Vtx.x + Points[2].x, Vtx.y + Points[2].y, Vtx.z);
   Quad[3]:= Vector4(Vtx.x + Points[3].x, Vtx.y + Points[3].y, Vtx.z);

   Scene.InsertPostTriangle(
    Quad[0], Quad[1], Quad[2],
    Entry^.Mapping[3], Entry^.Mapping[2], Entry^.Mapping[1],
    Entry^.Color, Entry^.Color, Entry^.Color,
    0, 0, 0, Entry^.Texture);

   Scene.InsertPostTriangle(
    Quad[2], Quad[3], Quad[0],
    Entry^.Mapping[1], Entry^.Mapping[0], Entry^.Mapping[3],
    Entry^.Color, Entry^.Color, Entry^.Color,
    0, 0, 0, Entry^.Texture);
  end;

 FEntries.RemoveAll();
 FVertices.RemoveAll(); 
end;

//---------------------------------------------------------------------------
end.

