unit AsphyreTorusKnot;
//---------------------------------------------------------------------------
// AsphyreTorusKnot.pas                                 Modified: 29-Jan-2009
// Torus PQ Knot implementation for Asphyre                      Version 1.01
//---------------------------------------------------------------------------
// This code is based on the description at:
//  http://www.blackpawn.com/texts/pqtorus/default.html
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
//
// If you require any clarifications about the license, feel free to contact
// us or post your question on our forums at: http://www.afterwarp.net
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
// The Original Code is AsphyreTorusKnot.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2000 - 2011,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Math, Vectors2, Vectors3, Vectors4, AsphyreMeshes;

//---------------------------------------------------------------------------
procedure CreateTorusKnot(Mesh: TAsphyreMesh; Radius, TubeRadius: Single;
 p, q: Integer; Rings, Sides: Integer; TexTilesX, TexTilesY: Single);

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
procedure CreateTorusKnot(Mesh: TAsphyreMesh; Radius, TubeRadius: Single;
 p, q: Integer; Rings, Sides: Integer; TexTilesX, TexTilesY: Single);
var
 SideBlock: Integer;
 i, j, ni, nj: Integer;
 Phi, ThetaInc: Single;
 Theta, PhiInc, Phi1, r: Single;
 Vertex, Pos, Next, t, n, b: TVector3;
 Point: TPoint2;
begin
 SideBlock:= Sides + 1;

 Mesh.Vertices.RemoveAll();
 Mesh.TexCoords.RemoveAll();
 Mesh.Normals.RemoveAll();
 Mesh.VertexIndices.Clear();
 Mesh.TextureIndices.Clear();

 Phi:= 0.0;
 PhiInc:= 2.0 * Pi / Rings;
 ThetaInc:= 2.0 * Pi / Sides;

 for j:= 0 to Rings{ - 1} do
  begin
   Theta:= 0.0;

   r:= 0.5 * (2.0 + Sin(q * Phi)) * Radius;
   Pos.x:= r * Cos(p * Phi);
   Pos.y:= r * Cos(q * Phi);
   Pos.z:= r * Sin(p * Phi);

   Phi1:= Phi + PhiInc;
   r:= 0.5 * (2.0 + Sin(q * Phi1)) * Radius;
   Next.x:= r * Cos(p * Phi1);
   Next.y:= r * Cos(q * Phi1);
   Next.z:= r * Sin(p * Phi1);

   t:= Next - Pos;
   n:= Next + Pos;
   b:= Norm3(Cross3(t, n));
   n:= Norm3(Cross3(b, t));

   for i:= 0 to Sides do
    begin
     Point.x:= Sin(Theta) * TubeRadius;
     Point.y:= Cos(Theta) * TubeRadius;

     Vertex:= Pos + (n * Point.x) + (b * Point.y);

     Mesh.Vertices.Add(Vertex);
     Mesh.Normals.Add(Norm3(Vertex - Pos));
     Mesh.TexCoords.Add(
      Point2(Phi * TexTilesX / (2.0 * Pi), Theta * TexTilesY / (2.0 * Pi)));

     ni:= i + 1;
     nj:= j + 1;

     if (i < Sides)and(j < Rings) then
      begin
       Mesh.VertexIndices.Insert(i + j * SideBlock);
       Mesh.VertexIndices.Insert(i + nj * SideBlock);
       Mesh.VertexIndices.Insert(ni + nj * SideBlock);

       Mesh.VertexIndices.Insert(i + j * SideBlock);
       Mesh.VertexIndices.Insert(ni + nj * SideBlock);
       Mesh.VertexIndices.Insert(ni + j * SideBlock);

       Mesh.TextureIndices.Insert(i + j * SideBlock);
       Mesh.TextureIndices.Insert(i + nj * SideBlock);
       Mesh.TextureIndices.Insert(ni + nj * SideBlock);

       Mesh.TextureIndices.Insert(i + j * SideBlock);
       Mesh.TextureIndices.Insert(ni + nj * SideBlock);
       Mesh.TextureIndices.Insert(ni + j * SideBlock);
      end;

     Theta:= Theta + ThetaInc;
    end;

   Phi:= Phi + PhiInc;
  end;

 Mesh.ComputeFaceOrigins();
 Mesh.ComputeFaceNormals();
end;

//---------------------------------------------------------------------------
end.
