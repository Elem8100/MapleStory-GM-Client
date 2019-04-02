unit AsphyreScenes;
//---------------------------------------------------------------------------
// AsphyreScenes.pas                                    Modified: 24-Jan-2009
// Software 3D pipeline for Asphyre                               Version 1.0
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
// The Original Code is AsphyreScenes.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2000 - 2009,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 SysUtils, Math, Vectors2px, Vectors2, Vectors3, Vectors4, Matrices4,
 HelperSets, AsphyreColors, AsphyreMatrices, AsphyreMeshes, AbstractTextures,
 AsphyreImages, AbstractRasterizer, AsphyreLights;

//---------------------------------------------------------------------------
// The following option indicates how the normals are transformed. If the
// option is enabled, then the normals are transformed by world inverse
// transposed matrix. This is particularly useful for non-uniform scale
// transformations, but at the expense of reduced performance.
{$define PreciseNormalTransforms}

//---------------------------------------------------------------------------
const
 // The minimal distance between the triangle and camera before it gets
 // clipped. This approach can conveniently discard all triangles that are
 // around and in the back of the camera.
 zNearPlane   = 1.0;

 // The maximum number of triangles that can be handled simultaneously by the
 // rendering pipeline. This parameter does not affect the performance but it
 // does affect the memory usage.
 MaxTriangles = 65536;

//---------------------------------------------------------------------------
type
 TSphinxCullingType = (sctNone, sctClockwise, sctCounterClockwise);

//---------------------------------------------------------------------------
 TDepthElement = record
  Index: Integer;
  Depth: Single;
 end;

//---------------------------------------------------------------------------
 PSphinxTriangle = ^TSphinxTriangle;
 TSphinxTriangle = record
  vIndex : array[0..2] of Integer;
  uvIndex: array[0..2] of Integer;
  Culling: TSphinxCullingType;
  Texture: TAsphyreCustomTexture;
  Visible: Boolean;
 end;

//---------------------------------------------------------------------------
 TAsphyreScene = class
 private
  PreVertices   : TVectors4;
  PostVertices  : TVectors4;
  Normals       : TVectors4;
  TextureCoords : TPoints2;
  DiffuseColors : TAsphyreColors;
  SpecularColors: TAsphyreColors;

  FaceOrigins  : TVectors4;
  FaceNormals  : TVectors4;
  DepthList    : array[0..MaxTriangles - 1] of TDepthElement;
  Triangles    : array[0..MaxTriangles - 1] of TSphinxTriangle;
  TriangleCount: Integer;
  SortedCount  : Integer;

  NormalMtx   : TAsphyreMatrix;
  FFieldOfView: Single;
  FAspectRatio: Single;

  FRaster: TAsphyreRasterizer;
  FLights: TAsphyreLights;

  FDisplaySize: TPoint2px;
  FDisplayPos : TPoint2;
  FCullingType: TSphinxCullingType;

  EyePos: TVector3;

  procedure TransferFaces(VertexIndices, TextureIndices: TIntegerList;
   FaceCount, vAdd, uvAdd: Integer; Texture: TAsphyreCustomTexture);
  procedure InsertVertexColors(NoVertices: Integer);

  procedure BackfaceCull();
  procedure ZPlaneCull();

  procedure MarkOccludedColors();

  procedure Illuminate();

  procedure InitDepthList();
  function DepthListSplit(Start, Stop: Integer): Integer;
  procedure DepthListSort(Start, Stop: Integer);
  procedure UpdateDepthList();

  procedure Project(const DrawPos, DrawSize: TPoint2);
  procedure RenderTriangles(Effect: TRasterEffect);
 public
  property Lights: TAsphyreLights read FLights;

  property Raster: TAsphyreRasterizer read FRaster write FRaster;

  property DisplaySize: TPoint2px read FDisplaySize write FDisplaySize;
  property DisplayPos : TPoint2 read FDisplayPos write FDisplayPos;

  property FieldOfView: Single read FFieldOfView write FFieldOfView;
  property AspectRatio: Single read FAspectRatio write FAspectRatio;
  property CullingType: TSphinxCullingType read FCullingType write FCullingType;

  property TotalTriangles: Integer read TriangleCount;

  procedure BeginScene();
  procedure Draw(Mesh: TAsphyreMesh; WorldMtx: PMatrix4;
   Texture: TAsphyreCustomTexture); overload;
  procedure Draw(Mesh: TAsphyreMesh; WorldMtx: PMatrix4;
   Image: TAsphyreImage; TextureNo: Integer = 0); overload;
  procedure EndScene(ViewMtx: PMatrix4);

  procedure InsertPostTriangle(const v0, v1, v2: TVector4;
   const tx0, tx1, tx2: TPoint2; c0, c1, c2, s0, s1, s2: Longword;
   ATexture: TAsphyreCustomTexture);

  procedure Present(const DrawPos, DrawSize: TPoint2;
   Effect: TRasterEffect = reNormal);

  constructor Create();
  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 AsphyreTypes, AsphyreErrors;

//---------------------------------------------------------------------------
constructor TAsphyreScene.Create();
begin
 inherited;

 PreVertices   := TVectors4.Create();
 PostVertices  := TVectors4.Create();
 Normals       := TVectors4.Create();
 FaceOrigins   := TVectors4.Create();
 FaceNormals   := TVectors4.Create();
 TextureCoords := TPoints2.Create();
 DiffuseColors := TAsphyreColors.Create();
 SpecularColors:= TAsphyreColors.Create();

 NormalMtx:= TAsphyreMatrix.Create();

 FLights:= TAsphyreLights.Create();

 FFieldOfView:= Pi * 0.5;
 FAspectRatio:= 1.0;
 FCullingType:= sctCounterClockwise;

 FDisplaySize:= Point2(640.0, 480.0);
 FDisplayPos := ZeroVec2;
end;

//---------------------------------------------------------------------------
destructor TAsphyreScene.Destroy();
begin
 FreeAndNil(FLights);
 FreeAndNil(NormalMtx);
 FreeAndNil(SpecularColors);
 FreeAndNil(DiffuseColors);
 FreeAndNil(TextureCoords);
 FreeAndNil(FaceNormals);
 FreeAndNil(FaceOrigins);
 FreeAndNil(Normals);
 FreeAndNil(PostVertices);
 FreeAndNil(PreVertices);

 inherited;
end;

//---------------------------------------------------------------------------
procedure TAsphyreScene.BeginScene();
begin
 TriangleCount:= 0;

 PreVertices.RemoveAll();
 PostVertices.RemoveAll();
 Normals.RemoveAll();
 FaceOrigins.RemoveAll();
 FaceNormals.RemoveAll();
 TextureCoords.RemoveAll();
 DiffuseColors.RemoveAll();
 SpecularColors.RemoveAll();

 FillChar(DepthList, SizeOf(DepthList), 0);
 FillChar(Triangles, SizeOf(Triangles), 0);
end;

//---------------------------------------------------------------------------
procedure TAsphyreScene.TransferFaces(VertexIndices,
 TextureIndices: TIntegerList; FaceCount, vAdd, uvAdd: Integer;
 Texture: TAsphyreCustomTexture);
var
 i, VertNo: Integer;
 Dest: PSphinxTriangle;
begin
 VertNo:= 0;

 Dest:= @Triangles[TriangleCount];

 for i:= 0 to FaceCount - 1 do
  begin
   Dest^.vIndex[0]:= VertexIndices[VertNo] + vAdd;
   Dest^.vIndex[1]:= VertexIndices[VertNo + 1] + vAdd;
   Dest^.vIndex[2]:= VertexIndices[VertNo + 2] + vAdd;

   Dest^.uvIndex[0]:= TextureIndices[VertNo] + uvAdd;
   Dest^.uvIndex[1]:= TextureIndices[VertNo + 1] + uvAdd;
   Dest^.uvIndex[2]:= TextureIndices[VertNo + 2] + uvAdd;

   Dest^.Texture:= Texture;
   Dest^.Culling:= FCullingType;

   Inc(VertNo, 3);
   Inc(Dest);
  end;
end;

//---------------------------------------------------------------------------
procedure TAsphyreScene.InsertVertexColors(NoVertices: Integer);
var
 i: Integer;
begin
 for i:= 0 to NoVertices - 1 do
  DiffuseColors.Add(cColor(255, 255, 255, 0));

 for i:= 0 to NoVertices - 1 do
  SpecularColors.Add(cColor(0, 0, 0, 0));
end;

//---------------------------------------------------------------------------
procedure TAsphyreScene.Draw(Mesh: TAsphyreMesh; WorldMtx: PMatrix4;
 Texture: TAsphyreCustomTexture);
var
 NoFaces, VertexNo, TexCoordNo: Integer;
begin
 // Make the validation of input mesh to prevent unnecessary access violations
 // when working with experimental meshes. The mesh that pass this validaiton
 // should most likely appear without any problems. Make sure the indices are
 // specified correctly though.
 if (Mesh.Vertices.Count < 1) then
  begin
   Errors.Insert(errMeshVertices, Self, ClassName, 'Draw');
   Exit;
  end;

 if (Mesh.VertexIndices.Count < 1)or(Mesh.VertexIndices.Count mod 3 > 0) then
  begin
   Errors.Insert(errMeshNoIndices, Self, ClassName, 'Draw');
   Exit;
  end;

 if (Mesh.TextureIndices.Count <> Mesh.VertexIndices.Count) then
  begin
   Errors.Insert(errMeshTextureIndices, Self, ClassName, 'Draw');
   Exit;
  end;

 if (Mesh.Vertices.Count <> Mesh.Normals.Count) then
  begin
   Errors.Insert(errMeshVertexNormals, Self, ClassName, 'Draw');
   Exit;
  end;

 NoFaces:= Mesh.VertexIndices.Count div 3;

 if (NoFaces <> Mesh.FaceNormals.Count) then
  begin
   Errors.Insert(errMeshFaceNormals, Self, ClassName, 'Draw');
   Exit;
  end;

 if (NoFaces <> Mesh.FaceOrigins.Count) then
  begin
   Errors.Insert(errMeshFaceOrigins, Self, ClassName, 'Draw');
   Exit;
  end;

 if (TriangleCount + NoFaces > MaxTriangles) then
  begin
   Errors.Insert(errSceneTooComplex, Self, ClassName, 'Draw');
   Exit;
  end;

 // Apply world transformation to mesh vertices and normals.
 VertexNo  := PreVertices.Count;
 TexCoordNo:= TextureCoords.Count;

 PreVertices.AddTransform(Mesh.Vertices, WorldMtx);
 FaceOrigins.AddTransform(Mesh.FaceOrigins, WorldMtx);

 // Transform the vertex normals according to the desired transformation scheme.
 {$ifdef PreciseNormalTransforms}
 NormalMtx.LoadMtx(WorldMtx);
 NormalMtx.InverseTranspose();
 {$else}
 NormalMtx.LoadRotation(WorldMtx);
 {$endif}

 Normals.AddTransform(Mesh.Normals, NormalMtx.RawMtx);
 FaceNormals.AddTransform(Mesh.FaceNormals, NormalMtx.RawMtx);

 // Copy the texture coordinates into the working buffer.
 TextureCoords.AddFrom(Mesh.TexCoords);

 // Insert the colors for the mesh vertices.
 InsertVertexColors(Mesh.Vertices.Count);

 // Transfer vertex and texture indices to triangle buffer, modifying the
 // indices on the fly.
 TransferFaces(Mesh.VertexIndices, Mesh.TextureIndices, NoFaces, VertexNo,
  TexCoordNo, Texture);

 // update pointers
 Inc(TriangleCount, NoFaces);
end;

//---------------------------------------------------------------------------
procedure TAsphyreScene.Draw(Mesh: TAsphyreMesh; WorldMtx: PMatrix4;
 Image: TAsphyreImage; TextureNo: Integer = 0);
var
 Texture: TAsphyreCustomTexture;
begin
 Texture:= nil;
 if (Image <> nil) then Texture:= Image.Texture[TextureNo];

 if (Mesh <> nil) then
  Draw(Mesh, WorldMtx, Texture);
end;

//---------------------------------------------------------------------------
procedure TAsphyreScene.BackfaceCull();
var
 i: Integer;
begin
 for i:= 0 to TriangleCount - 1 do
  case Triangles[i].Culling of
   sctNone:
    Triangles[i].Visible:= True;

   sctClockwise:
    Triangles[i].Visible:= Dot4(FaceNormals[i], FaceOrigins[i]) > 0;

   sctCounterClockwise:
    Triangles[i].Visible:= Dot4(FaceNormals[i], FaceOrigins[i]) < 0;
  end;
end;

//---------------------------------------------------------------------------
procedure TAsphyreScene.ZPlaneCull();
var
 i   : Integer;
 Tri : PSphinxTriangle;
 zMin: Single;
begin
 Tri:= @Triangles[0];

 for i:= 0 to TriangleCount - 1 do
  begin
   if (Tri^.Visible) then
    begin
     zMin:= Min(Min(
      PostVertices[Tri^.vIndex[0]].z,
      PostVertices[Tri^.vIndex[1]].z),
      PostVertices[Tri^.vIndex[2]].z);

     Tri^.Visible:= (zMin > zNearPlane);
    end;

   Inc(Tri);
  end;
end;

//---------------------------------------------------------------------------
procedure TAsphyreScene.MarkOccludedColors();
var
 i: Integer;
begin
 for i:= 0 to PostVertices.Count - 1 do
  DiffuseColors[i]^.a:= 0;

 for i:= 0 to TriangleCount - 1 do
  if (Triangles[i].Visible) then
   begin
    DiffuseColors[Triangles[i].vIndex[0]]^.a:= 1;
    DiffuseColors[Triangles[i].vIndex[1]]^.a:= 1;
    DiffuseColors[Triangles[i].vIndex[2]]^.a:= 1;
   end;
end;

//---------------------------------------------------------------------------
procedure TAsphyreScene.Illuminate();
var
 i: Integer;
 Diffuse, Specular: TAsphyreColor;
begin
 MarkOccludedColors();

 for i:= 0 to PostVertices.Count - 1 do
  if (DiffuseColors[i]^.a > 0) then
   begin
    FLights.Illuminate(PreVertices[i], Norm3(Normals[i]), EyePos,
     Diffuse, Specular);

    Diffuse.a:= 65535;

    DiffuseColors[i]^ := Diffuse;
    SpecularColors[i]^:= Specular;
   end;
end;

//---------------------------------------------------------------------------
procedure TAsphyreScene.InsertPostTriangle(const v0, v1, v2: TVector4;
 const tx0, tx1, tx2: TPoint2; c0, c1, c2, s0, s1, s2: Longword;
 ATexture: TAsphyreCustomTexture);
begin
 if (TriangleCount >= MaxTriangles) then
  begin
   Errors.Insert(errSceneTooComplex, Self, ClassName, 'InsertPostTriangle');
   Exit;
  end;

 PreVertices.Add(v0);
 PreVertices.Add(v1);
 PreVertices.Add(v2);

 PostVertices.Add(v0);
 PostVertices.Add(v1);
 PostVertices.Add(v2);

 Normals.Add(AxisZVec4);
 Normals.Add(AxisZVec4);
 Normals.Add(AxisZVec4);

 TextureCoords.Add(tx0);
 TextureCoords.Add(tx1);
 TextureCoords.Add(tx2);

 DiffuseColors.Add(c0);
 DiffuseColors.Add(c1);
 DiffuseColors.Add(c2);

 SpecularColors.Add(s0);
 SpecularColors.Add(s1);
 SpecularColors.Add(s2);

 FaceOrigins.Add(v0);
 FaceNormals.Add(AxisZVec4);

 with Triangles[TriangleCount] do
  begin
   vIndex[0]:= PostVertices.Count - 3;
   vIndex[1]:= PostVertices.Count - 2;
   vIndex[2]:= PostVertices.Count - 1;

   uvIndex[0]:= TextureCoords.Count - 3;
   uvIndex[1]:= TextureCoords.Count - 2;
   uvIndex[2]:= TextureCoords.Count - 1;

   Culling:= sctNone;
   Texture:= ATexture;
   Visible:= True;
  end;

 Inc(TriangleCount);
end;

//---------------------------------------------------------------------------
procedure TAsphyreScene.EndScene(ViewMtx: PMatrix4);
begin
 if (TriangleCount < 1) then Exit;

 PostVertices.CopyTransform(PreVertices, ViewMtx);
 FaceOrigins.CopyTransform(FaceOrigins, ViewMtx);

 // Transform the face normals according to the desired transformation scheme.
 {$ifdef PreciseNormalTransforms}
 NormalMtx.LoadMtx(ViewMtx);
 NormalMtx.InverseTranspose();
 {$else}
 NormalMtx.LoadRotation(ViewMtx);
 {$endif}

 FaceNormals.CopyTransform(FaceNormals, NormalMtx.RawMtx);

 BackfaceCull();
 ZPlaneCull();

 EyePos:= GetEyePos4(ViewMtx^);

 Illuminate();
end;

//---------------------------------------------------------------------------
procedure TAsphyreScene.InitDepthList();
var
 i: Integer;
begin
 SortedCount:= 0;

 for i:= 0 to TriangleCount - 1 do
  if (Triangles[i].Visible) then
   begin
    DepthList[SortedCount].Index:= i;
    DepthList[SortedCount].Depth:= FaceOrigins[i].z;
    Inc(SortedCount);
   end;
end;

//---------------------------------------------------------------------------
function TAsphyreScene.DepthListSplit(Start, Stop: Integer): Integer;
var
 Left, Right, Pivot: Integer;
 Aux: TDepthElement;
begin
 Left := Start + 1;
 Right:= Stop;
 Pivot:= Start;

 while (Left <= Right) do
  begin
   while (Left <= Stop)and(DepthList[Left].Depth <
    DepthList[Pivot].Depth) do Inc(Left);

   while (Right > Start)and(DepthList[Right].Depth >=
    DepthList[Pivot].Depth) do Dec(Right);

   if (Left < Right) then
    begin
     Aux:= DepthList[Left];
     DepthList[Left]:= DepthList[Right];
     DepthList[Right]:= Aux;
    end;
  end;

 Aux:= DepthList[Start];
 DepthList[Start]:= DepthList[Right];
 DepthList[Right]:= Aux;

 Result:= Right;
end;

//---------------------------------------------------------------------------
procedure TAsphyreScene.DepthListSort(Start, Stop: integer);
var
 SplitPt: Integer;
begin
 if (Start < Stop) then
  begin
   SplitPt:= DepthListSplit(Start, Stop);

   DepthListSort(Start, SplitPt - 1);
   DepthListSort(SplitPt + 1, Stop);
  end;
end;

//---------------------------------------------------------------------------
procedure TAsphyreScene.UpdateDepthList();
begin
 InitDepthList();
 DepthListSort(0, SortedCount - 1);
end;

//---------------------------------------------------------------------------
procedure TAsphyreScene.Project(const DrawPos, DrawSize: TPoint2);
var
 Delta, DeltaX, DeltaY: Single;
 Vertex: PVector4;
 i: Integer;
begin
 Delta := Cot(FFieldOfView * 0.5);
 DeltaX:= (Delta * FAspectRatio) * DrawSize.x * 0.5;
 DeltaY:= -Delta * DrawSize.y * 0.5;

 for i:= 0 to PostVertices.Count - 1 do
  begin
   Vertex:= PostVertices.Vector[i];

   if (Vertex^.z >= ZNearPlane) then
    begin
     Vertex^.x:= ((DeltaX * Vertex^.x) / Vertex^.z) + DrawPos.x;
     Vertex^.y:= ((DeltaY * Vertex^.y) / Vertex^.z) + DrawPos.y;
     Vertex^.w:= 1.0 / Vertex^.z;
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TAsphyreScene.RenderTriangles(Effect: TRasterEffect);
var
 i: Integer;
 Tri : PSphinxTriangle;
 Vertices: array[0..2] of TVector4;
begin
 for i:= SortedCount - 1 downto 0 do
// for i:= 0 to TriangleCount - 1 do
  begin
   Tri:= @Triangles[DepthList[i].Index];
   if (not Tri^.Visible) then Continue;

   Vertices[0]:= PostVertices[Tri^.vIndex[0]];
   Vertices[1]:= PostVertices[Tri^.vIndex[1]];
   Vertices[2]:= PostVertices[Tri^.vIndex[2]];

   if (Tri^.Texture <> nil) then
    begin
     FRaster.UseTexture(Tri^.Texture);
     FRaster.TexMap(
      // Transformed vertices
      Vertices[0],
      Vertices[1],
      Vertices[2],
      // Texture coordinates
      TextureCoords[Tri^.uvIndex[0]]^,
      TextureCoords[Tri^.uvIndex[1]]^,
      TextureCoords[Tri^.uvIndex[2]]^,
      // Diffuse colors
      DiffuseColors[Tri^.vIndex[0]]^,
      DiffuseColors[Tri^.vIndex[1]]^,
      DiffuseColors[Tri^.vIndex[2]]^,
      // Specular colors
      SpecularColors[Tri^.vIndex[0]]^,
      SpecularColors[Tri^.vIndex[1]]^,
      SpecularColors[Tri^.vIndex[2]]^,
      // Drawing effect
      Effect);
    end else
    begin
     FRaster.FillTri(
      // Transformed vertices
      Vertices[0],
      Vertices[1],
      Vertices[2],
      // Diffuse colors
      cClamp(DiffuseColors[Tri^.vIndex[0]]^ + SpecularColors[Tri^.vIndex[0]]^),
      cClamp(DiffuseColors[Tri^.vIndex[1]]^ + SpecularColors[Tri^.vIndex[1]]^),
      cClamp(DiffuseColors[Tri^.vIndex[2]]^ + SpecularColors[Tri^.vIndex[2]]^),
      // Specular colors
      0, 0, 0,
      // Drawing effect
      Effect);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TAsphyreScene.Present(const DrawPos, DrawSize: TPoint2;
 Effect: TRasterEffect = reNormal);
begin
 UpdateDepthList();

 Project(DrawPos, DrawSize);

 RenderTriangles(Effect);
end;

//---------------------------------------------------------------------------
end.
