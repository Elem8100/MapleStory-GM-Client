unit AsphyreMeshes;
//---------------------------------------------------------------------------
// AsphyreMeshes.pas                                    Modified: 02-Feb-2009
// Asphyre 3D mesh implementation                                 Version 1.0
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
// The Original Code is AsphyreMeshes.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2000 - 2009,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Classes, SysUtils, Math, Vectors2, Vectors4, AsphyreDb, HelperSets;

//---------------------------------------------------------------------------
type
 TAsphyreMesh = class
 private
  FName: ShortString;

  FVertices   : TVectors4;
  FNormals    : TVectors4;
  FTexCoords  : TPoints2;
  FFaceNormals: TVectors4;
  FFaceOrigins: TVectors4;
  FVertexIndices : TIntegerList;
  FTextureIndices: TIntegerList;

  function FindUnifyMatch(VtxList: TVectors4; TexList: TPoints2;
   const VtxPoint: TVector4; const TexPoint: TPoint2): Integer;
 public
  property Name: ShortString read FName write FName;

  property Vertices : TVectors4 read FVertices;
  property Normals  : TVectors4 read FNormals;
  property TexCoords: TPoints2 read FTexCoords;

  property FaceNormals: TVectors4 read FFaceNormals;
  property FaceOrigins: TVectors4 read FFaceOrigins;

  property VertexIndices : TIntegerList read FVertexIndices;
  property TextureIndices: TIntegerList read FTextureIndices;

  procedure ComputeFaceOrigins();
  procedure ComputeFaceNormals();
  procedure ComputeVertexNormals();

  procedure Rescale(const Theta: TVector4);
  procedure Displace(const Theta: TVector4);
  procedure Normalize();
  procedure Centralize();
  procedure InvertNormals();

  procedure UnifyVertices();

  procedure SphericalTextureMappingNormal();
  procedure SphericalTextureMappingPosition();

  procedure Validate();

  procedure LoadFromStream(Stream: TStream);
  procedure SaveToStream(Stream: TStream);

  function LoadFromFile(const FileName: string): Boolean;
  function SaveToFile(const FileName: string): Boolean;

  function LoadFromASDb(const Key: ShortString; Archive: TASDb): Boolean;
  function SaveToASDb(const Key: ShortString; Archive: TASDb): Boolean;

  procedure Assign(Source: TAsphyreMesh);

  constructor Create(const AName: ShortString = '');
  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
 TAsphyreMeshes = class
 private
  Meshes: array of TAsphyreMesh;

  SearchObjects: array of Integer;
  SearchDirty  : Boolean;

  function GetCount(): Integer;
  function GetItem(Num: Integer): TAsphyreMesh;
  function Insert(Element: TAsphyreMesh): Integer;
  procedure InitSearchObjects();
  procedure SwapSearchObjects(Index1, Index2: Integer);
  function CompareSearchObjects(Obj1, Obj2: TAsphyreMesh): Integer;
  function SplitSearchObjects(Start, Stop: Integer): integer;
  procedure SortSearchObjects(Start, Stop: integer);
  procedure UpdateSearchObjects();
  function GetMesh(const Name: ShortString): TAsphyreMesh;
 public
  property Count: Integer read GetCount;
  property Items[Num: Integer]: TAsphyreMesh read GetItem; default;

  property Mesh[const Name: ShortString]: TAsphyreMesh read GetMesh;

  function IndexOf(Element: TAsphyreMesh): Integer; overload;
  function IndexOf(const Name: ShortString): Integer; overload;
  function Include(Element: TAsphyreMesh): Integer;

  function AddFromFile(const FileName: string; const Name: ShortString = ''): Integer;
  function AddFromASDb(const Key: ShortString; Archive: TASDb;
   const Name: ShortString = ''): Integer;

  procedure Remove(Num: Integer);
  procedure RemoveAll();

  procedure MarkSearchDirty();

  constructor Create();
  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
var
 Meshes: TAsphyreMeshes = nil;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 StreamUtils, AsphyreErrors, AsphyreAuth, MediaUtils;

//---------------------------------------------------------------------------
const
// Vertices under this distance are considered the same point in space,
// when calculating mesh normals.
 WeldEpsilon = 0.0001;

//---------------------------------------------------------------------------
constructor TAsphyreMesh.Create(const AName: ShortString = '');
begin
 inherited Create();

 FName:= AName;

 FVertices := TVectors4.Create();
 FNormals  := TVectors4.Create();
 FTexCoords:= TPoints2.Create();

 FFaceNormals:= TVectors4.Create();
 FFaceOrigins:= TVectors4.Create();

 FVertexIndices := TIntegerList.Create();
 FTextureIndices:= TIntegerList.Create();
end;

//---------------------------------------------------------------------------
destructor TAsphyreMesh.Destroy();
begin
 FreeAndNil(FTextureIndices);
 FreeAndNil(FVertexIndices);

 FreeAndNil(FFaceOrigins);
 FreeAndNil(FFaceNormals);

 FreeAndNil(FTexCoords);
 FreeAndNil(FNormals);
 FreeAndNil(FVertices);

 inherited;
end;

//---------------------------------------------------------------------------
procedure TAsphyreMesh.ComputeFaceOrigins();
var
 Num, v0, v1, v2: Integer;
begin
 FFaceOrigins.RemoveAll();

 Num:= 0;

 while (Num < FVertexIndices.Count - 2) do
  begin
   v0:= FVertexIndices[Num];
   v1:= FVertexIndices[Num + 1];
   v2:= FVertexIndices[Num + 2];

   FFaceOrigins.Add((FVertices[v0] + FVertices[v1] + FVertices[v2]) / 3.0);

   Inc(Num, 3);
  end;
end;

//---------------------------------------------------------------------------
procedure TAsphyreMesh.ComputeFaceNormals();
var
 Num, v0, v1, v2: Integer;
 a, b: TVector4;
begin
 FFaceNormals.RemoveAll();

 Num:= 0;

 while (Num < FVertexIndices.Count - 2) do
  begin
   v0:= FVertexIndices[Num];
   v1:= FVertexIndices[Num + 1];
   v2:= FVertexIndices[Num + 2];

   a:= FVertices[v2] - FVertices[v0];
   b:= FVertices[v2] - FVertices[v1];
   FFaceNormals.Add(Norm4(Cross4(a, b)));

   Inc(Num, 3);
  end;
end;

//---------------------------------------------------------------------------
procedure TAsphyreMesh.ComputeVertexNormals();
var
 i, FaceNo, VertNo, v0, v1, v2: Integer;
 Weights: array of Single;
 a, b, Normal, Sample: TVector4;
begin
 FNormals.RemoveAll();
 FFaceNormals.RemoveAll();

 SetLength(Weights, FVertexIndices.Count div 3);

 VertNo:= 0;
 FaceNo:= 0;
 while (VertNo < FVertexIndices.Count - 2) do
  begin
   v0:= FVertexIndices[VertNo];
   v1:= FVertexIndices[VertNo + 1];
   v2:= FVertexIndices[VertNo + 2];

   a:= FVertices[v2] - FVertices[v0];
   b:= FVertices[v2] - FVertices[v1];

   Normal:= Cross4(a, b);
   FFaceNormals.Add(Norm4(Normal));

   Weights[FaceNo]:= Length4(Normal);
   Inc(FaceNo);
   Inc(VertNo, 3);
  end;

 for i:= 0 to FVertices.Count - 1 do
  begin
   Normal:= ZeroVec4;
   Sample:= FVertices[i];

   FaceNo:= 0;
   VertNo:= 0;

   while (VertNo < FVertexIndices.Count - 2) do
    begin
     v0:= FVertexIndices[VertNo];
     v1:= FVertexIndices[VertNo + 1];
     v2:= FVertexIndices[VertNo + 2];

     if (Length4(FVertices[v0] - Sample) < WeldEpsilon) then
      Normal:= Normal + (FFaceNormals[FaceNo] * Weights[FaceNo]);

     if (Length4(FVertices[v1] - Sample) < WeldEpsilon) then
      Normal:= Normal + (FFaceNormals[FaceNo] * Weights[FaceNo]);

     if (Length4(FVertices[v2] - Sample) < WeldEpsilon) then
      Normal:= Normal + (FFaceNormals[FaceNo] * Weights[FaceNo]);

     Inc(FaceNo);
     Inc(VertNo, 3);
    end;

   FNormals.Add(Norm4(Normal));
  end;
end;

//---------------------------------------------------------------------------
procedure TAsphyreMesh.Rescale(const Theta: TVector4);
var
 i: Integer;
begin
 for i:= 0 to FVertices.Count - 1 do
  FVertices[i]:= FVertices[i] * Theta;

 for i:= 0 to FFaceOrigins.Count - 1 do
  FFaceOrigins[i]:= FFaceOrigins[i] * Theta;
end;

//---------------------------------------------------------------------------
procedure TAsphyreMesh.Displace(const Theta: TVector4);
var
 i: Integer;
begin
 for i:= 0 to FVertices.Count - 1 do
  FVertices[i]:= FVertices[i] + Theta;

 for i:= 0 to FFaceOrigins.Count - 1 do
  FFaceOrigins[i]:= FFaceOrigins[i] + Theta;
end;

//---------------------------------------------------------------------------
function TAsphyreMesh.FindUnifyMatch(VtxList: TVectors4; TexList: TPoints2;
 const VtxPoint: TVector4; const TexPoint: TPoint2): Integer;
var
 i: Integer;
begin
 Result:= -1;

 for i:= 0 to VtxList.Count - 1 do
  if (SameVec4(VtxList[i], VtxPoint, WeldEpsilon))and
   (SameVec2(TexList[i]^, TexPoint)) then
   begin
    Result:= i;
    Break;
   end;
end;

//---------------------------------------------------------------------------
procedure TAsphyreMesh.UnifyVertices();
var
 NewVertices : TVectors4;
 NewTexCoords: TPoints2;
 NewIndices  : TIntegerList;
 i, v0, v1, v2, uv0, uv1, uv2, Index: Integer;
begin
 if (FVertexIndices.Count <> FTextureIndices.Count) then
  begin
   Errors.Insert(errMeshTextureIndices, Self, ClassName, 'UnifyVertices');
   Exit;
  end;

 NewVertices := TVectors4.Create();
 NewTexCoords:= TPoints2.Create();
 NewIndices  := TIntegerList.Create();

 for i:= 0 to (FVertexIndices.Count div 3) - 1 do
  begin
   v0:= FVertexIndices[i * 3];
   v1:= FVertexIndices[(i * 3) + 1];
   v2:= FVertexIndices[(i * 3) + 2];

   uv0:= FTextureIndices[i * 3];
   uv1:= FTextureIndices[(i * 3) + 1];
   uv2:= FTextureIndices[(i * 3) + 2];

   // First Index (v0, uv0)
   Index:= FindUnifyMatch(NewVertices, NewTexCoords, FVertices[v0],
    FTexCoords[uv0]^);
   if (Index = -1) then
    begin
     Index:= NewVertices.Count;
     NewVertices.Add(FVertices[v0]);
     NewTexCoords.Add(FTexCoords[uv0]^);
    end;

   NewIndices.Insert(Index);

   // Second Index (v1, uv1)
   Index:= FindUnifyMatch(NewVertices, NewTexCoords, FVertices[v1],
    FTexCoords[uv1]^);
   if (Index = -1) then
    begin
     Index:= NewVertices.Count;
     NewVertices.Add(FVertices[v1]);
     NewTexCoords.Add(FTexCoords[uv1]^);
    end;

   NewIndices.Insert(Index);

   // Third Index (v2, uv2)
   Index:= FindUnifyMatch(NewVertices, NewTexCoords, FVertices[v2],
    FTexCoords[uv2]^);
   if (Index = -1) then
    begin
     Index:= NewVertices.Count;
     NewVertices.Add(FVertices[v2]);
     NewTexCoords.Add(FTexCoords[uv2]^);
    end;

   NewIndices.Insert(Index);
  end;

 FVertices.CopyFrom(NewVertices);
 FTexCoords.CopyFrom(NewTexCoords);
 FVertexIndices.CopyFrom(NewIndices);
 FTextureIndices.CopyFrom(NewIndices);

 FreeAndNil(NewIndices);
 FreeAndNil(NewTexCoords);
 FreeAndNil(NewVertices);
end;

//---------------------------------------------------------------------------
procedure TAsphyreMesh.SaveToStream(Stream: TStream);
var
 i: Integer;
begin
 // -> Mesh Name
 StreamPutAnsiString(Stream, FName, 255);

 // -> Vertex Count
 StreamPutWord(Stream, FVertices.Count);
 // -> Normals Count
 StreamPutWord(Stream, FNormals.Count);
 // -> Texture Coordinate Count
 StreamPutWord(Stream, FTexCoords.Count);
 // -> Vertex Indices Count
 StreamPutWord(Stream, FVertexIndices.Count);
 // -> Texture Indices Count
 StreamPutWord(Stream, FTextureIndices.Count);
 // -> Face Normal Count
 StreamPutWord(Stream, FFaceNormals.Count);

 // -> Vertices
 for i:= 0 to FVertices.Count - 1 do
  begin
   StreamPutSingle(Stream, FVertices[i].x);
   StreamPutSingle(Stream, FVertices[i].y);
   StreamPutSingle(Stream, FVertices[i].z);
  end;

 // -> Normals
 for i:= 0 to FNormals.Count - 1 do
  begin
   StreamPutSingle(Stream, FNormals[i].x);
   StreamPutSingle(Stream, FNormals[i].y);
   StreamPutSingle(Stream, FNormals[i].z);
  end;

 // -> Texture Coordinates
 for i:= 0 to FTexCoords.Count - 1 do
  begin
   StreamPutSingle(Stream, FTexCoords[i]^.x);
   StreamPutSingle(Stream, FTexCoords[i]^.y);
  end;

 // -> Vertex Indices
 for i:= 0 to FVertexIndices.Count - 1 do
  StreamPutWord(Stream, Cardinal(FVertexIndices[i]));

 // -> Texture Indices
 for i:= 0 to FTextureIndices.Count - 1 do
  StreamPutWord(Stream, Cardinal(FTextureIndices[i]));

 // -> Face Normals
 for i:= 0 to FFaceNormals.Count - 1 do
  begin
   StreamPutSingle(Stream, FFaceNormals[i].x);
   StreamPutSingle(Stream, FFaceNormals[i].y);
   StreamPutSingle(Stream, FFaceNormals[i].z);
  end;

 // Face Origins are not saved and are calculated upon loading instead.
end;

//---------------------------------------------------------------------------
procedure TAsphyreMesh.LoadFromStream(Stream: TStream);
var
 VertexCount, NormalCount, TexCoordCount: Integer;
 i, VertIndexCount, TexIndexCount, FaceNormCount: Integer;
 Coord: TVector4;
begin
 Coord.w:= 1.0;

 // -> Mesh Name
 FName:= StreamGetAnsiString(Stream, 255);

 // -> Vertex Count
 VertexCount:= StreamGetWord(Stream);
 // -> Normals Count
 NormalCount:= StreamGetWord(Stream);
 // -> Texture Coordinate Count
 TexCoordCount:= StreamGetWord(Stream);
 // -> Vertex Indices Count
 VertIndexCount:= StreamGetWord(Stream);
 // -> Texture Indices Count
 TexIndexCount:= StreamGetWord(Stream);
 // -> Face Normal Count
 FaceNormCount:= StreamGetWord(Stream);

 // -> Vertices
 FVertices.RemoveAll();
 for i:= 0 to VertexCount - 1 do
  begin
   Coord.x:= StreamGetSingle(Stream);
   Coord.y:= StreamGetSingle(Stream);
   Coord.z:= StreamGetSingle(Stream);
   FVertices.Add(Coord);
  end;

 // -> Normals
 FNormals.RemoveAll();
 for i:= 0 to NormalCount - 1 do
  begin
   Coord.x:= StreamGetSingle(Stream);
   Coord.y:= StreamGetSingle(Stream);
   Coord.z:= StreamGetSingle(Stream);
   FNormals.Add(Coord);
  end;

 // -> Texture Coordinates
 FTexCoords.RemoveAll();
 for i:= 0 to TexCoordCount - 1 do
  begin
   Coord.x:= StreamGetSingle(Stream);
   Coord.y:= StreamGetSingle(Stream);
   FTexCoords.Add(Coord.x, Coord.y);
  end;

 // -> Vertex Indices
 FVertexIndices.Clear();
 for i:= 0 to VertIndexCount - 1 do
  FVertexIndices.Insert(Integer(StreamGetWord(Stream)));

 // -> Texture Indices
 FTextureIndices.Clear();
 for i:= 0 to TexIndexCount - 1 do
  FTextureIndices.Insert(Integer(StreamGetWord(Stream)));

 // -> Face Normals
 FFaceNormals.RemoveAll();
 for i:= 0 to FaceNormCount - 1 do
  begin
   Coord.x:= StreamGetSingle(Stream);
   Coord.y:= StreamGetSingle(Stream);
   Coord.z:= StreamGetSingle(Stream);
   FFaceNormals.Add(Coord);
  end;

 // Face Origins are calculated automatically.
 ComputeFaceOrigins();
end;

//---------------------------------------------------------------------------
procedure TAsphyreMesh.Normalize();
var
 MinSize, MaxSize, Theta: Single;
 i: Integer;
begin
 if (FVertices.Count < 1) then Exit;

 MinSize:= High(Integer);
 MaxSize:= Low(Integer);

 for i:= 0 to FVertices.Count - 1 do
  begin
   MinSize:= MinValue([MinSize, FVertices[i].x, FVertices[i].y, FVertices[i].z]);
   MaxSize:= MaxValue([MaxSize, FVertices[i].x, FVertices[i].y, FVertices[i].z]);
  end;

 Theta:= 1.0 / (MaxSize - MinSize);
 Rescale(Vector4(Theta, Theta, Theta));
end;

//---------------------------------------------------------------------------
procedure TAsphyreMesh.InvertNormals();
var
 i: Integer;
begin
 for i:= 0 to Normals.Count - 1 do
  Normals[i]:= -Normals[i];
end;

//---------------------------------------------------------------------------
procedure TAsphyreMesh.Centralize();
var
 MinAxis, MaxAxis, Shift: TVector4;
 i: Integer;
begin
 if (FVertices.Count < 1) then Exit;

 MinAxis:= Vector4(High(Integer), High(Integer), High(Integer));
 MaxAxis:= Vector4(Low(Integer), Low(Integer), Low(Integer));

 for i:= 0 to FVertices.Count - 1 do
  begin
   MinAxis.x:= Min(MinAxis.x, FVertices[i].x);
   MinAxis.y:= Min(MinAxis.y, FVertices[i].y);
   MinAxis.z:= Min(MinAxis.z, FVertices[i].z);

   MaxAxis.x:= Max(MaxAxis.x, FVertices[i].x);
   MaxAxis.y:= Max(MaxAxis.y, FVertices[i].y);
   MaxAxis.z:= Max(MaxAxis.z, FVertices[i].z);
  end;

 Shift.x:= -(MinAxis.x + MaxAxis.x) * 0.5;
 Shift.y:= -(MinAxis.y + MaxAxis.y) * 0.5;
 Shift.z:= -(MinAxis.z + MaxAxis.z) * 0.5;

 Displace(Shift);
end;

//---------------------------------------------------------------------------
procedure TAsphyreMesh.SphericalTextureMappingNormal();
var
 i: Integer;
 Normal: TVector4;
 TexPos: TPoint2;
begin
 if (FVertices.Count <> FNormals.Count) then Exit;

 FTexCoords.RemoveAll();

 for i:= 0 to FVertices.Count - 1 do
  begin
   Normal:= FNormals[i];

   TexPos.x:= 0.5 + ArcSin(Normal.x) / Pi;
   TexPos.y:= 0.5 + ArcSin(Normal.y) / Pi;

   FTexCoords.Add(TexPos);
  end;

 FTextureIndices.CopyFrom(FVertexIndices);
end;

//---------------------------------------------------------------------------
procedure TAsphyreMesh.SphericalTextureMappingPosition();
var
 i: Integer;
 Middle, VecTo: TVector4;
 TexPos: TPoint2;
begin
 if (FVertices.Count < 1) then Exit;

 Middle:= ZeroVec4;

 for i:= 0 to FVertices.Count - 1 do
  Middle:= Middle + FVertices[i];

 Middle:= Middle * (1.0 / FVertices.Count);

 FTexCoords.RemoveAll();

 for i:= 0 to FVertices.Count - 1 do
  begin
   VecTo:= Norm4(FVertices[i] - Middle);

   TexPos.x:= 0.5 + ArcSin(VecTo.x) / Pi;
   TexPos.y:= 0.5 + ArcSin(VecTo.y) / Pi;

   FTexCoords.Add(TexPos);
  end;

 FTextureIndices.CopyFrom(FVertexIndices);
end;

//---------------------------------------------------------------------------
procedure TAsphyreMesh.Validate();
var
 Triangles: Integer;
begin
 if (FVertices.Count < 1)or(FVertexIndices.Count mod 3 > 0) then Exit;

 Triangles:= FVertexIndices.Count div 3;
 if (Triangles < 1) then Exit;

 if (FFaceOrigins.Count <> Triangles) then ComputeFaceOrigins();
 if (FFaceNormals.Count <> Triangles) then ComputeFaceNormals();

 if (TextureIndices.Count < 1) then
  begin
   if (FNormals.Count = FVertices.Count) then SphericalTextureMappingNormal()
    else SphericalTextureMappingPosition();
  end;

 if (FNormals.Count <> FVertices.Count) then ComputeVertexNormals();
end;

//---------------------------------------------------------------------------
procedure TAsphyreMesh.Assign(Source: TAsphyreMesh);
begin
 FName:= Source.Name;

 FVertices.CopyFrom(Source.Vertices);
 FNormals.CopyFrom(Source.Normals);
 FTexCoords.CopyFrom(Source.TexCoords);
 FFaceNormals.CopyFrom(Source.FaceNormals);
 FFaceOrigins.CopyFrom(Source.FaceOrigins);
 FVertexIndices.CopyFrom(Source.VertexIndices);
 FTextureIndices.CopyFrom(Source.TextureIndices);
end;

//---------------------------------------------------------------------------
function TAsphyreMesh.LoadFromFile(const FileName: string): Boolean;
var
 Stream: TFileStream;
begin
 try
  Stream:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
 except
  Errors.Insert(errOpenFile, Self, ClassName, 'LoadFromFile');
  Result:= False;
  Exit;
 end;

 Result:= True;

 try
  LoadFromStream(Stream);
 except
  Errors.Insert(errLoadFile, Self, ClassName, 'LoadFromFile');
  Result:= False;
 end;

 FreeAndNil(Stream);
end;

//---------------------------------------------------------------------------
function TAsphyreMesh.SaveToFile(const FileName: string): Boolean;
var
 Stream: TFileStream;
begin
 try
  Stream:= TFileStream.Create(FileName, fmCreate or fmShareExclusive);
 except
  Errors.Insert(errCreateFile, Self, ClassName, 'SaveToFile');
  Result:= False;
  Exit;
 end;

 Result:= True;

 try
  SaveToStream(Stream);
 except
  Errors.Insert(errSaveFile, Self, ClassName, 'SaveToFile');
  Result:= False;
 end;

 FreeAndNil(Stream);
end;

//---------------------------------------------------------------------------
function TAsphyreMesh.LoadFromASDb(const Key: ShortString;
 Archive: TASDb): Boolean;
var
 Stream: TMemoryStream;
begin
 Result:= Archive.UpdateOnce();
 if (not Result) then
  begin
   Errors.Insert(errOpenFile, Self, ClassName, 'LoadFromASDb');
   Exit;
  end;

 Auth.Authorize(Self, Archive);

 Stream:= TMemoryStream.Create();

 Result:= Archive.ReadStream(Key, Stream);
 if (not Result) then
  begin
   Errors.Insert(errUnpackArchive, Self, ClassName, 'LoadFromASDb');
   Auth.Unauthorize();
   FreeAndNil(Stream);
   Exit;
  end;

 Auth.Unauthorize();

 Stream.Seek(0, soFromBeginning);

 try
  LoadFromStream(Stream);
 except
  Errors.Insert(errLoadFile, Self, ClassName, 'LoadFromASDb');
  Result:= False;
 end;

 FreeAndNil(Stream);
end;

//---------------------------------------------------------------------------
function TAsphyreMesh.SaveToASDb(const Key: ShortString;
 Archive: TASDb): Boolean;
var
 Stream: TMemoryStream;
begin
 Result:= Archive.UpdateOnce();
 if (not Result) then
  begin
   Errors.Insert(errOpenFile, Self, ClassName, 'SaveToASDb');
   Exit;
  end;

 Stream:= TMemoryStream.Create();

 try
  SaveToStream(Stream);
 except
  Errors.Insert(errSaveFile, Self, ClassName, 'SaveToASDb');
  FreeAndNil(Stream);
  Result:= False;
  Exit;
 end;

 Auth.Authorize(Self, Archive);

 Stream.Seek(0, soFromBeginning);
 Result:= Archive.WriteStream(Key, Stream, recFile);

 Auth.Unauthorize();
 FreeAndNil(Stream);
end;

//---------------------------------------------------------------------------
constructor TAsphyreMeshes.Create();
begin
 inherited;

 SearchDirty:= False;
end;

//---------------------------------------------------------------------------
destructor TAsphyreMeshes.Destroy();
begin
 RemoveAll();

 inherited;
end;

//---------------------------------------------------------------------------
function TAsphyreMeshes.GetCount(): Integer;
begin
 Result:= Length(Meshes);
end;

//---------------------------------------------------------------------------
function TAsphyreMeshes.GetItem(Num: Integer): TAsphyreMesh;
begin
 if (Num >= 0)and(Num < Length(Meshes)) then Result:= Meshes[Num]
  else Result:= nil;
end;

//---------------------------------------------------------------------------
procedure TAsphyreMeshes.Remove(Num: Integer);
var
 i: Integer;
begin
 if (Num < 0)or(Num >= Length(Meshes)) then Exit;

 Meshes[Num].Free();

 for i:= Num to Length(Meshes) - 2 do
  Meshes[i]:= Meshes[i + 1];

 SetLength(Meshes, Length(Meshes) - 1);
 SearchDirty:= True;
end;

//---------------------------------------------------------------------------
procedure TAsphyreMeshes.RemoveAll();
var
 i: Integer;
begin
 for i:= 0 to Length(Meshes) - 1 do
  if (Meshes[i] <> nil) then
   begin
    Meshes[i].Free();
    Meshes[i]:= nil;
   end;

 SetLength(Meshes, 0);
 SearchDirty:= True;
end;

//---------------------------------------------------------------------------
function TAsphyreMeshes.Insert(Element: TAsphyreMesh): Integer;
var
 Index: Integer;
begin
 Index:= Length(Meshes);
 SetLength(Meshes, Index + 1);

 Meshes[Index]:= Element;
 Result:= Index;

 SearchDirty:= True;
end;

//---------------------------------------------------------------------------
function TAsphyreMeshes.IndexOf(Element: TAsphyreMesh): Integer;
var
 i: Integer;
begin
 Result:= -1;

 for i:= 0 to Length(Meshes) - 1 do
  if (Meshes[i] = Element) then
   begin
    Result:= i;
    Break;
   end;
end;

//---------------------------------------------------------------------------
function TAsphyreMeshes.Include(Element: TAsphyreMesh): Integer;
begin
 Result:= IndexOf(Element);
 if (Result = -1) then Result:= Insert(Element);
end;

//---------------------------------------------------------------------------
procedure TAsphyreMeshes.InitSearchObjects();
var
 i: Integer;
begin
 if (Length(Meshes) <> Length(SearchObjects)) then
  SetLength(SearchObjects, Length(Meshes));

 for i:= 0 to Length(Meshes) - 1 do
  SearchObjects[i]:= i;
end;

//---------------------------------------------------------------------------
procedure TAsphyreMeshes.SwapSearchObjects(Index1, Index2: Integer);
var
 Aux: Integer;
begin
 Aux:= SearchObjects[Index1];

 SearchObjects[Index1]:= SearchObjects[Index2];
 SearchObjects[Index2]:= Aux;
end;

//---------------------------------------------------------------------------
function TAsphyreMeshes.CompareSearchObjects(Obj1, Obj2: TAsphyreMesh): Integer;
begin
 Result:= CompareText(Obj1.Name, Obj2.Name);
end;

//---------------------------------------------------------------------------
function TAsphyreMeshes.SplitSearchObjects(Start, Stop: Integer): integer;
var
 Left, Right: Integer;
 Pivot: TAsphyreMesh;
begin
 Left := Start + 1;
 Right:= Stop;
 Pivot:= Meshes[SearchObjects[Start]];

 while (Left <= Right) do
  begin
   while (Left <= Stop)and(CompareSearchObjects(Meshes[SearchObjects[Left]],
    Pivot) < 0) do Inc(Left);

   while (Right > Start)and(CompareSearchObjects(Meshes[SearchObjects[Right]],
    Pivot) >= 0) do Dec(Right);

   if (Left < Right) then SwapSearchObjects(Left, Right);
  end;

 SwapSearchObjects(Start, Right);

 Result:= Right;
end;

//---------------------------------------------------------------------------
procedure TAsphyreMeshes.SortSearchObjects(Start, Stop: integer);
var
 SplitPt: integer;
begin
 if (Start < Stop) then
  begin
   SplitPt:= SplitSearchObjects(Start, Stop);

   SortSearchObjects(Start, SplitPt - 1);
   SortSearchObjects(SplitPt + 1, Stop);
  end;
end;

//---------------------------------------------------------------------------
procedure TAsphyreMeshes.UpdateSearchObjects();
begin
 InitSearchObjects();
 SortSearchObjects(0, Length(SearchObjects) - 1);

 SearchDirty:= False;
end;

//---------------------------------------------------------------------------
function TAsphyreMeshes.IndexOf(const Name: ShortString): Integer;
var
 Lo, Hi, Mid: Integer;
begin
 if (SearchDirty) then UpdateSearchObjects();

 Result:= -1;

 Lo:= 0;
 Hi:= Length(SearchObjects) - 1;

 while (Lo <= Hi) do
  begin
   Mid:= (Lo + Hi) div 2;

   if (CompareText(Meshes[SearchObjects[Mid]].Name, Name) = 0) then
    begin
     Result:= SearchObjects[Mid];
     Break;
    end;

   if (CompareText(Meshes[SearchObjects[Mid]].Name, Name) > 0) then
    Hi:= Mid - 1 else Lo:= Mid + 1;
 end;
end;

//---------------------------------------------------------------------------
procedure TAsphyreMeshes.MarkSearchDirty();
begin
 SearchDirty:= True;
end;

//---------------------------------------------------------------------------
function TAsphyreMeshes.GetMesh(const Name: ShortString): TAsphyreMesh;
var
 Index: Integer;
begin
 Index:= IndexOf(Name);
 if (Index <> -1) then Result:= Meshes[Index] else Result:= nil;
end;

//---------------------------------------------------------------------------
function TAsphyreMeshes.AddFromASDb(const Key: ShortString; Archive: TASDb;
 const Name: ShortString): Integer;
var
 MeshItem: TAsphyreMesh;
begin
 if (Name <> '') then MeshItem:= TAsphyreMesh.Create(Name)
  else MeshItem:= TAsphyreMesh.Create(ExtractPureKey(Key));

 if (not MeshItem.LoadFromASDb(Key, Archive)) then
  begin
   MeshItem.Free();
   Result:= -1;
   Exit;
  end;

 Result:= Insert(MeshItem);
end;

//---------------------------------------------------------------------------
function TAsphyreMeshes.AddFromFile(const FileName: string;
 const Name: ShortString = ''): Integer;
var
 MeshItem: TAsphyreMesh;
begin
 if (Name <> '') then MeshItem:= TAsphyreMesh.Create(Name)
  else MeshItem:= TAsphyreMesh.Create(ChangeFileExt(ExtractFileName(FileName), ''));

 if (not MeshItem.LoadFromFile(FileName)) then
  begin
   MeshItem.Free();
   Result:= -1;
   Exit;
  end;

 Result:= Insert(MeshItem);
end;

//---------------------------------------------------------------------------
initialization
 Meshes:= TAsphyreMeshes.Create();

//---------------------------------------------------------------------------
finalization
 FreeAndNil(Meshes);

//---------------------------------------------------------------------------
end.
