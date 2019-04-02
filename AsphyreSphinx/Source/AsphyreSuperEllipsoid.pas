unit AsphyreSuperEllipsoid;
//---------------------------------------------------------------------------
// AsphyreSuperEllipsoid.pas                            Modified: 26-Dec-2008
// Superellipsoid implementation for Asphyre                      Version 1.1
//---------------------------------------------------------------------------
// This code generates superellipsoid documented by Paul Bourke at:
//  http://local.wasp.uwa.edu.au/~pbourke/surfaces_curves/superellipse/
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
// The Original Code is AsphyreSuperEllipsoid.pas.
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
procedure CreateSuperEllipsoid(Mesh: TAsphyreMesh; Divisions: Integer;
 n1, n2: Single);

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
function SampleSuperEllipsoid(Phi, Beta, n1, n2: Single): TVector3;
var
 CosPhi, SinPhi, CosBeta, SinBeta, Temp: Single;
begin
 CosPhi := Cos(Phi);
 SinPhi := Sin(Phi);
 CosBeta:= Cos(Beta);
 SinBeta:= Sin(Beta);

 Temp:= Sign(CosPhi) * Power(Abs(CosPhi), n1);

 Result.x:= Temp * Sign(CosBeta) * Power(Abs(CosBeta), n2);
 Result.y:= Sign(SinPhi) * Power(Abs(SinPhi), n1);
 Result.z:= Temp * Sign(SinBeta) * Power(Abs(SinBeta), n2);
end;

//---------------------------------------------------------------------------
function CalculateNormal(Phi, Beta, n1, n2: Single): TVector3;
var
 CosPhi, SinPhi, CosBeta, SinBeta: Single;
begin
 CosPhi := Cos(Phi);
 SinPhi := Sin(Phi);
 CosBeta:= Cos(Beta);
 SinBeta:= Sin(Beta);

 Result.x:= Sign(CosPhi) * Power(Abs(CosPhi), 2.0 - n1) * Sign(CosBeta) *
  Power(Abs(CosBeta), 2.0 - n2);
 Result.z:= Sign(CosPhi) * Power(Abs(CosPhi), 2.0 - n1) * Sign(SinBeta) *
  Power(Abs(SinBeta), 2.0 - n2);
 Result.y:= Sign(SinPhi) * Power(Abs(SinPhi), 2.0 - n1);
end;

//---------------------------------------------------------------------------
procedure CreateSuperEllipsoid(Mesh: TAsphyreMesh; Divisions: Integer;
 n1, n2: Single);
var
 Phi, Beta, PhiInc, BetaInc: Single;
 i, j, ni, nj: Integer;
 Rows, Cols: Integer;
begin
 Rows:= Divisions + 1;
 Cols:= (Divisions div 2) + 2;

 PhiInc := Pi / (Cols - 2);
 BetaInc:= 2.0 * Pi / (Rows - 1);

 Mesh.Vertices.RemoveAll();
 Mesh.TexCoords.RemoveAll();
 Mesh.Normals.RemoveAll();
 Mesh.VertexIndices.Clear();
 Mesh.TextureIndices.Clear();

 Phi:= -Pi * 0.5;
 for j:= 0 to Cols - 1 do
  begin
   Beta:= -Pi;

   for i:= 0 to Rows - 1 do
    begin
     Mesh.Vertices.Add(SampleSuperEllipsoid(Phi, Beta, n1, n2) * 0.5);
     Mesh.Normals.Add(CalculateNormal(Phi, Beta, n1, n2));
     Mesh.TexCoords.Add(Point2(i / Rows, j / Cols));

     ni:= (i + 1) mod Rows;
     nj:= j + 1;

     if (j < Cols - 1) then
      begin
       Mesh.VertexIndices.Insert(i + j * Rows);
       Mesh.VertexIndices.Insert(i + nj * Rows);
       Mesh.VertexIndices.Insert(ni + nj * Rows);
       Mesh.VertexIndices.Insert(i + j * Rows);
       Mesh.VertexIndices.Insert(ni + nj * Rows);
       Mesh.VertexIndices.Insert(ni + j * Rows);

       Mesh.TextureIndices.Insert(i + j * Rows);
       Mesh.TextureIndices.Insert(i + nj * Rows);
       Mesh.TextureIndices.Insert(ni + nj * Rows);
       Mesh.TextureIndices.Insert(i + j * Rows);
       Mesh.TextureIndices.Insert(ni + nj * Rows);
       Mesh.TextureIndices.Insert(ni + j * Rows);
      end;

     Beta:= Beta + BetaInc;
    end;

   Phi:= Phi + PhiInc;
  end;

 Mesh.ComputeFaceOrigins();
 Mesh.ComputeFaceNormals();
end;

//---------------------------------------------------------------------------
end.
