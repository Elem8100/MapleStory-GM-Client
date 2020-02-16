unit PXT.Headers;
(*
 * Copyright (c) 2015 - 2020 Yuriy Kotsarenko. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is
 * distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and limitations under the License.
 *)
interface

// NOTE: this file is auto-generated, please do not edit.

{$INCLUDE PXT.Config.inc}

uses
  PXT.Types;

const
  // Name of external dynamic library.
  AfterwarpLib = 'afterwarp';

type
  PApplicationRec = ^TApplicationRec;
  TApplicationRec = record
  end;

  PBufferRec = ^TBufferRec;
  TBufferRec = record
  end;

  PCanvasRec = ^TCanvasRec;
  TCanvasRec = record
  end;

  PComputeProgramRec = ^TComputeProgramRec;
  TComputeProgramRec = record
  end;

  PDeviceRec = ^TDeviceRec;
  TDeviceRec = record
  end;

  PGaussianBlurRec = ^TGaussianBlurRec;
  TGaussianBlurRec = record
  end;

  PGaussianHighlightRec = ^TGaussianHighlightRec;
  TGaussianHighlightRec = record
  end;

  PGrapherRec = ^TGrapherRec;
  TGrapherRec = record
  end;

  PImageAtlasRec = ^TImageAtlasRec;
  TImageAtlasRec = record
  end;

  PMeshBufferRec = ^TMeshBufferRec;
  TMeshBufferRec = record
  end;

  PMeshMetaTagRec = ^TMeshMetaTagRec;
  TMeshMetaTagRec = record
  end;

  PPMeshMetaTagsRec = ^PMeshMetaTagsRec;
  PMeshMetaTagsRec = ^TMeshMetaTagsRec;
  TMeshMetaTagsRec = record
  end;

  PPMeshModelRec = ^PMeshModelRec;
  PMeshModelRec = ^TMeshModelRec;
  TMeshModelRec = record
  end;

  PPMeshVoxelRec = ^PMeshVoxelRec;
  PMeshVoxelRec = ^TMeshVoxelRec;
  TMeshVoxelRec = record
  end;

  PObjectCameraRec = ^TObjectCameraRec;
  TObjectCameraRec = record
  end;

  PObjectModelRec = ^TObjectModelRec;
  TObjectModelRec = record
  end;

  PObjectModelsRec = ^TObjectModelsRec;
  TObjectModelsRec = record
  end;

  PProgramRec = ^TProgramRec;
  TProgramRec = record
  end;

  PRandomContextRec = ^TRandomContextRec;
  TRandomContextRec = record
  end;

  PRasterSurfaceRec = ^TRasterSurfaceRec;
  TRasterSurfaceRec = record
  end;

  PSamplerRec = ^TSamplerRec;
  TSamplerRec = record
  end;

  PSceneRec = ^TSceneRec;
  TSceneRec = record
  end;

  PSceneMeshRec = ^TSceneMeshRec;
  TSceneMeshRec = record
  end;

  PSceneMeshesRec = ^TSceneMeshesRec;
  TSceneMeshesRec = record
  end;

  PSceneTextureRec = ^TSceneTextureRec;
  TSceneTextureRec = record
  end;

  PTextRendererRec = ^TTextRendererRec;
  TTextRendererRec = record
  end;

  PTextureRec = ^TTextureRec;
  TTextureRec = record
  end;

  PTimerRec = ^TTimerRec;
  TTimerRec = record
  end;

function DeviceCreate(Configuration: PDeviceConfiguration): PDeviceRec; cdecl; external AfterwarpLib;

function DeviceCreateShared(Device: PDeviceRec; Configuration: PDeviceConfiguration): PDeviceRec; cdecl;
  external AfterwarpLib;

procedure DeviceDestroy(Device: PDeviceRec); cdecl; external AfterwarpLib;

function DeviceResize(Device: PDeviceRec; Width, Height: Integer): TLibraryBool; cdecl; external AfterwarpLib;

function DeviceBegin(Device: PDeviceRec): TLibraryBool; cdecl; external AfterwarpLib;

procedure DeviceEnd(Device: PDeviceRec); cdecl; external AfterwarpLib;

procedure DeviceResetCache(Device: PDeviceRec); cdecl; external AfterwarpLib;

function DeviceClear(Device: PDeviceRec; ClearLayers: Byte; ClearColor: PFloatColor; ClearDepth: Single; 
  ClearStencil: Cardinal): TLibraryBool; cdecl; external AfterwarpLib;

function DeviceSetRenderingState(Device: PDeviceRec; State: PRenderingState): TLibraryBool; cdecl;
  external AfterwarpLib;

function DeviceGetRenderingState(Device: PDeviceRec; State: PRenderingState): TLibraryBool; cdecl;
  external AfterwarpLib;

function DeviceSetViewport(Device: PDeviceRec; Viewport: PIntRect): TLibraryBool; cdecl;
  external AfterwarpLib;

function DeviceGetViewport(Device: PDeviceRec; Viewport: PIntRect): TLibraryBool; cdecl;
  external AfterwarpLib;

function DeviceSetScissor(Device: PDeviceRec; Scissor: PIntRect): TLibraryBool; cdecl; external AfterwarpLib;

function DeviceGetScissor(Device: PDeviceRec; Scissor: PIntRect): TLibraryBool; cdecl; external AfterwarpLib;

procedure DeviceGetConfiguration(Device: PDeviceRec; Configuration: PDeviceConfiguration); cdecl;
  external AfterwarpLib;

procedure DeviceGetCapabilities(Device: PDeviceRec; Capabilities: PDeviceCapabilities); cdecl;
  external AfterwarpLib;

function BufferCreate(Device: PDeviceRec; Parameters: PBufferParameters; InitialData: Pointer): PBufferRec;
  cdecl; external AfterwarpLib;

procedure BufferDestroy(Buffer: PBufferRec); cdecl; external AfterwarpLib;

procedure BufferGetParameters(Buffer: PBufferRec; Parameters: PBufferParameters); cdecl;
  external AfterwarpLib;

function BufferUpdate(Buffer: PBufferRec; Data: Pointer; Offset, Size: Cardinal): TLibraryBool; cdecl;
  external AfterwarpLib;

function ProgramCreate(Device: PDeviceRec; Parameters: PProgramParameters): PProgramRec; cdecl;
  external AfterwarpLib;

procedure ProgramDestroy(_Program: PProgramRec); cdecl; external AfterwarpLib;

function ProgramUpdateByIndex(_Program: PProgramRec; VariableIndex: Integer; VariableData: Pointer; 
  Size: Cardinal): TLibraryBool; cdecl; external AfterwarpLib;

function ProgramUpdateByName(_Program: PProgramRec; VariableName: PAnsiChar; VariableData: Pointer; 
  Size: Cardinal): TLibraryBool; cdecl; external AfterwarpLib;

function ProgramBind(_Program: PProgramRec; Buffer: PBufferRec; Channel, Offset: Cardinal): TLibraryBool;
  cdecl; external AfterwarpLib;

procedure ProgramUnbind(_Program: PProgramRec; Buffer: PBufferRec; Channel: Cardinal); cdecl;
  external AfterwarpLib;

procedure ProgramResetBindings(_Program: PProgramRec); cdecl; external AfterwarpLib;

procedure ProgramResetCache(_Program: PProgramRec); cdecl; external AfterwarpLib;

procedure ProgramPurgeCache(_Program: PProgramRec; Buffer: PBufferRec); cdecl; external AfterwarpLib;

function ProgramCommit(_Program: PProgramRec): TLibraryBool; cdecl; external AfterwarpLib;

function ProgramBegin(_Program: PProgramRec): TLibraryBool; cdecl; external AfterwarpLib;

procedure ProgramEnd(_Program: PProgramRec); cdecl; external AfterwarpLib;

function ProgramDraw(_Program: PProgramRec; Topology: TPrimitiveTopology; VertexCount: Cardinal; 
  BaseVertex: Integer): TLibraryBool; cdecl; external AfterwarpLib;

function ProgramDrawIndexed(_Program: PProgramRec; Topology: TPrimitiveTopology; IndexCount, 
  FirstIndex: Cardinal; BaseVertex: Integer): TLibraryBool; cdecl; external AfterwarpLib;

function ProgramDrawInstances(_Program: PProgramRec; Topology: TPrimitiveTopology; VertexCount, 
  InstanceCount: Cardinal; BaseVertex: Integer): TLibraryBool; cdecl; external AfterwarpLib;

function ProgramDrawInstancesIndexed(_Program: PProgramRec; Topology: TPrimitiveTopology; IndexCount, 
  InstanceCount, FirstIndex: Cardinal; BaseVertex: Integer): TLibraryBool; cdecl; external AfterwarpLib;

function ComputeProgramCreate(Device: PDeviceRec; ProgramElements: PProgramElementEntry; 
  ProgramElementCount: Cardinal; Shader: PAnsiChar; ShaderLength: Cardinal): PComputeProgramRec; cdecl;
  external AfterwarpLib;

procedure ComputeProgramDestroy(_Program: PComputeProgramRec); cdecl; external AfterwarpLib;

function ComputeProgramBindBuffer(_Program: PComputeProgramRec; Buffer: PBufferRec; Channel, Offset: Cardinal)
  : TLibraryBool; cdecl; external AfterwarpLib;

procedure ComputeProgramUnbindBuffer(_Program: PComputeProgramRec; Buffer: PBufferRec; Channel: Cardinal);
  cdecl; external AfterwarpLib;

function ComputeProgramBindTexture(_Program: PComputeProgramRec; Texture: PTextureRec; 
  BindFormat: PComputeBindTextureFormat): TLibraryBool; cdecl; external AfterwarpLib;

procedure ComputeProgramUnbindTexture(_Program: PComputeProgramRec; Texture: PTextureRec; 
  BindFormat: PComputeBindTextureFormat); cdecl; external AfterwarpLib;

procedure ComputeProgramResetBindings(_Program: PComputeProgramRec); cdecl; external AfterwarpLib;

function ComputeProgramCommit(_Program: PComputeProgramRec): TLibraryBool; cdecl; external AfterwarpLib;

function ComputeProgramBegin(_Program: PComputeProgramRec): TLibraryBool; cdecl; external AfterwarpLib;

procedure ComputeProgramEnd(_Program: PComputeProgramRec); cdecl; external AfterwarpLib;

function ComputeProgramDispatch(_Program: PComputeProgramRec; GroupsX, GroupsY, GroupsZ: Cardinal)
  : TLibraryBool; cdecl; external AfterwarpLib;

function SamplerCreate(Device: PDeviceRec; _SamplerState: PSamplerState): PSamplerRec; cdecl;
  external AfterwarpLib;

procedure SamplerDestroy(Sampler: PSamplerRec); cdecl; external AfterwarpLib;

procedure SamplerGetState(Sampler: PSamplerRec; _SamplerState: PSamplerState); cdecl; external AfterwarpLib;

function SamplerUpdate(Sampler: PSamplerRec; _SamplerState: PSamplerState): TLibraryBool; cdecl;
  external AfterwarpLib;

function SamplerBind(Sampler: PSamplerRec; Channel: Cardinal): TLibraryBool; cdecl; external AfterwarpLib;

procedure SamplerUnbind(Sampler: PSamplerRec; Channel: Cardinal); cdecl; external AfterwarpLib;

function TextureCreate(Device: PDeviceRec; Parameters: PTextureParameters): PTextureRec; cdecl;
  external AfterwarpLib;

function TextureCreateFromFile(Device: PDeviceRec; FileName: PAnsiChar; Format: TPixelFormat; 
  Attributes: TTextureAttributes): PTextureRec; cdecl; external AfterwarpLib;

function TextureCreateFromFileInMemory(Device: PDeviceRec; FileContent: Pointer; ContentSize: Cardinal; 
  Extension: PAnsiChar; Format: TPixelFormat; Attributes: TTextureAttributes): PTextureRec; cdecl;
  external AfterwarpLib;

procedure TextureDestroy(Texture: PTextureRec); cdecl; external AfterwarpLib;

procedure TextureGetParameters(Texture: PTextureRec; Parameters: PTextureParameters); cdecl;
  external AfterwarpLib;

function TextureUpdate(Texture: PTextureRec; Content: Pointer; Pitch: Cardinal; Layer: Integer; 
  Rect: PIntRect; MipLevel: Integer): TLibraryBool; cdecl; external AfterwarpLib;

function TextureRetrieve(Texture: PTextureRec; Content: Pointer; Pitch: Cardinal; Layer: Integer; 
  Rect: PIntRect; MipLevel: Integer): TLibraryBool; cdecl; external AfterwarpLib;

function TextureCopy(Texture, Source: PTextureRec; DestLayer: Integer; DestPos: PPoint2i; SrcLayer: Integer; 
  SourceRect: PIntRect; DestMipLevel, SrcMipLevel: Integer): TLibraryBool; cdecl; external AfterwarpLib;

function TextureCopyFromSurface(Texture: PTextureRec; Surface: PRasterSurfaceRec; Layer: Integer; 
  DestPos: PPoint2i; SourceRect: PIntRect; MipLevel: Integer): TLibraryBool; cdecl; external AfterwarpLib;

function TextureCopyToSurface(Texture: PTextureRec; Surface: PRasterSurfaceRec; Layer: Integer; 
  DestPos: PPoint2i; SourceRect: PIntRect; MipLevel: Integer): TLibraryBool; cdecl; external AfterwarpLib;

function TextureLoadFromFile(Texture: PTextureRec; FileName: PAnsiChar; Layer: Integer; DestPos: PPoint2i; 
  SourceRect: PIntRect; MipLevel: Integer): TLibraryBool; cdecl; external AfterwarpLib;

function TextureSaveToFile(Texture: PTextureRec; FileName: PAnsiChar; Quality: Pointer; Layer: Integer; 
  SourceRect: PIntRect; MipLevel: Integer): TLibraryBool; cdecl; external AfterwarpLib;

function TextureClear(Texture: PTextureRec): TLibraryBool; cdecl; external AfterwarpLib;

function TextureClearWith(Texture: PTextureRec; Color: PFloatColor; Layer: Integer): TLibraryBool; cdecl;
  external AfterwarpLib;

function TextureBind(Texture: PTextureRec; Channel: Cardinal): TLibraryBool; cdecl; external AfterwarpLib;

function TextureUnbind(Texture: PTextureRec; Channel: Cardinal): TLibraryBool; cdecl; external AfterwarpLib;

function TextureAttach(Texture, Attachment: PTextureRec; Layer: Integer): TLibraryBool; cdecl;
  external AfterwarpLib;

procedure TextureDetach(Texture: PTextureRec); cdecl; external AfterwarpLib;

function TextureBegin(Texture: PTextureRec; Layer: Cardinal): TLibraryBool; cdecl; external AfterwarpLib;

function TextureEnd(Texture: PTextureRec): TLibraryBool; cdecl; external AfterwarpLib;

function TextureGenerateMipMaps(Texture: PTextureRec): TLibraryBool; cdecl; external AfterwarpLib;

procedure TextureResetCache(Texture: PTextureRec); cdecl; external AfterwarpLib;

function MeshModelCreate(Device: PDeviceRec; Parameters: PMeshModelParameters): PMeshModelRec; cdecl;
  external AfterwarpLib;

procedure MeshModelDestroy(MeshModel: PMeshModelRec); cdecl; external AfterwarpLib;

procedure MeshModelGetInformation(MeshModel: PMeshModelRec; Information: PMeshModelInformation); cdecl;
  external AfterwarpLib;

function MeshModelGetVertexBuffer(MeshModel: PMeshModelRec): PBufferRec; cdecl; external AfterwarpLib;

function MeshModelGetIndexBuffer(MeshModel: PMeshModelRec): PBufferRec; cdecl; external AfterwarpLib;

function MeshModelDraw(MeshModel: PMeshModelRec; _Program: PProgramRec; Topology: TPrimitiveTopology; 
  ElementCount, FirstIndex: Cardinal; BaseVertex: Integer; PostUnbind: TLibraryBool): TLibraryBool; cdecl;
  external AfterwarpLib;

function MeshModelDrawInstances(MeshModel: PMeshModelRec; _Program: PProgramRec; InstanceCount: Cardinal; 
  Topology: TPrimitiveTopology; ElementCount, FirstIndex: Cardinal; BaseVertex: Integer; 
  PostUnbind: TLibraryBool): TLibraryBool; cdecl; external AfterwarpLib;

procedure MeshModelPurgeProgramCache(MeshModel: PMeshModelRec; _Program: PProgramRec); cdecl;
  external AfterwarpLib;

function MeshMetaTagGetOwner(Tag: PMeshMetaTagRec): PMeshMetaTagsRec; cdecl; external AfterwarpLib;

procedure MeshMetaTagGetName(Tag: PMeshMetaTagRec; Name: PAnsiChar; NameLength: PInteger); cdecl;
  external AfterwarpLib;

procedure MeshMetaTagGetBounds(Tag: PMeshMetaTagRec; MinBounds, MaxBounds: PVector3f); cdecl;
  external AfterwarpLib;

procedure MeshMetaTagGetLimits(Tag: PMeshMetaTagRec; FirstVertex, VertexCount, FirstIndex, 
  IndexCount: PInteger); cdecl; external AfterwarpLib;

function MeshMetaTagsCreate: PMeshMetaTagsRec; cdecl; external AfterwarpLib;

procedure MeshMetaTagsDestroy(Tags: PMeshMetaTagsRec); cdecl; external AfterwarpLib;

function MeshMetaTagsCount(Tags: PMeshMetaTagsRec): Integer; cdecl; external AfterwarpLib;

function MeshMetaTagsGetByIndex(Tags: PMeshMetaTagsRec; Index: Integer): PMeshMetaTagRec; cdecl;
  external AfterwarpLib;

function MeshMetaTagsGetByName(Tags: PMeshMetaTagsRec; Name: PAnsiChar): PMeshMetaTagRec; cdecl;
  external AfterwarpLib;

function MeshMetaTagsSpawn(Tags: PMeshMetaTagsRec; Name: PAnsiChar; MinBounds, MaxBounds: PVector3f; 
  FirstVertex, VertexCount, FirstIndex, IndexCount: Integer): PMeshMetaTagRec; cdecl; external AfterwarpLib;

procedure MeshMetaTagsErase(Tags: PMeshMetaTagsRec; Index: Integer); cdecl; external AfterwarpLib;

procedure MeshMetaTagsClear(Tags: PMeshMetaTagsRec); cdecl; external AfterwarpLib;

function MeshBufferCreate: PMeshBufferRec; cdecl; external AfterwarpLib;

procedure MeshBufferDestroy(MeshBuffer: PMeshBufferRec); cdecl; external AfterwarpLib;

procedure MeshBufferGetInformation(MeshBuffer: PMeshBufferRec; Information: PMeshBufferInformation); cdecl;
  external AfterwarpLib;

procedure MeshBufferSetInformation(MeshBuffer: PMeshBufferRec; Information: PMeshBufferInformation); cdecl;
  external AfterwarpLib;

function MeshBufferCreateModel(MeshBuffer: PMeshBufferRec; Device: PDeviceRec; 
  VertexElementEntries: PVertexElementEntry; VertexElementCount, FirstVertex, VertexCount, FirstIndex, 
  IndexCount, Channel: Cardinal): PMeshModelRec; cdecl; external AfterwarpLib;

procedure MeshBufferSuperEllipse(MeshBuffer: PMeshBufferRec; LongitudeSections, LatitudeSections: Cardinal; 
  Origin, Radius: PVector3f; InitLongitudeAngle, EndLongitudeAngle, LongitudeShape, InitLatitudeAngle, 
  EndLatitudeAngle, LatitudeShape: Single; InitTexCoord, EndTexCoord: PPoint2f; Color: PFloatColor; 
  IndicesClockwise: TLibraryBool); cdecl; external AfterwarpLib;

procedure MeshBufferCylinder(MeshBuffer: PMeshBufferRec; RadialSections, HeightSections: Cardinal; Origin, 
  HorizAxis, VertAxis, HeightAxis: PVector3f; InitAngle, EndAngle, Shape: Single; InitTexCoord, 
  EndTexCoord: PPoint2f; BendAngle, LateralAngle: Single; Color: PFloatColor; IndicesClockwise: TLibraryBool);
  cdecl; external AfterwarpLib;

procedure MeshBufferConeSimple(MeshBuffer: PMeshBufferRec; Sections: Cardinal; Origin: PVector3f; Radius, 
  Height: Single; Color: PFloatColor; IndicesClockwise: TLibraryBool); cdecl; external AfterwarpLib;

procedure MeshBufferDisc(MeshBuffer: PMeshBufferRec; RadialSections, InnerSections: Cardinal; Origin, 
  HorizAxis, VertAxis, Normal: PVector3f; InitAngle, EndAngle, Shape, InitRadius: Single; InitTexCoord, 
  EndTexCoord: PPoint2f; LateralAngle: Single; Color: PFloatColor; IndicesClockwise: TLibraryBool); cdecl;
  external AfterwarpLib;

procedure MeshBufferPlane(MeshBuffer: PMeshBufferRec; HorizSections, VertSections: Cardinal; Origin, 
  HorizAxis, VertAxis, Normal: PVector3f; InitTexCoord, EndTexCoord: PPoint2f; Color: PFloatColor; 
  IndicesClockwise: TLibraryBool); cdecl; external AfterwarpLib;

procedure MeshBufferCube(MeshBuffer: PMeshBufferRec; HorizSections, VertSections, DepthSections: Cardinal; 
  Origin, HorizAxis, VertAxis, DepthAxis: PVector3f; InitTexCoord, EndTexCoord: PPoint2f; Color: PFloatColor; 
  IndicesClockwise: TLibraryBool); cdecl; external AfterwarpLib;

procedure MeshBufferCubeMinimal(MeshBuffer: PMeshBufferRec; Origin, Size: PVector3f; Color: PFloatColor; 
  IndicesClockwise: TLibraryBool); cdecl; external AfterwarpLib;

procedure MeshBufferCubeRound(MeshBuffer: PMeshBufferRec; Origin, Size: PVector3f; Roundness: Single; 
  Color: PFloatColor; IndicesClockwise: TLibraryBool); cdecl; external AfterwarpLib;

procedure MeshBufferTorus(MeshBuffer: PMeshBufferRec; OuterSections, InnerSections: Cardinal; Origin, 
  HorizAxis, VertAxis: PVector3f; InitOuterAngle, EndOuterAngle, OuterShape, InitInnerAngle, EndInnerAngle, 
  InnerShape: Single; InnerRadius, InitTexCoord, EndTexCoord: PPoint2f; LateralAngle: Single; 
  Color: PFloatColor; IndicesClockwise: TLibraryBool); cdecl; external AfterwarpLib;

procedure MeshBufferTorusKnot(MeshBuffer: PMeshBufferRec; OuterSections, InnerSections: Cardinal; 
  Origin: PVector3f; P, Q: Integer; InitOuterAngle, EndOuterAngle, OuterRadius, InitInnerAngle, 
  EndInnerAngle, InnerShape: Single; InnerRadius, InitTexCoord, EndTexCoord: PPoint2f; LateralAngle: Single; 
  Color: PFloatColor; IndicesClockwise: TLibraryBool); cdecl; external AfterwarpLib;

procedure MeshBufferSupertoroid(MeshBuffer: PMeshBufferRec; OuterSections, InnerSections: Cardinal; 
  Origin: PVector3f; InitOuterAngle, EndOuterAngle, OuterRadius, OuterShape, InitInnerAngle, EndInnerAngle, 
  InnerRadius, InnerShape: Single; InitTexCoord, EndTexCoord: PPoint2f; Color: PFloatColor; 
  IndicesClockwise: TLibraryBool); cdecl; external AfterwarpLib;

procedure MeshBufferTransformVertices(MeshBuffer: PMeshBufferRec; FirstVertex, VertexCount: Cardinal); cdecl;
  external AfterwarpLib;

function MeshBufferCalculateFlatNormals(MeshBuffer: PMeshBufferRec; Epsilon: Single): TLibraryBool; cdecl;
  external AfterwarpLib;

procedure MeshBufferCalculateNormals(MeshBuffer: PMeshBufferRec; FirstVertex, VertexCount, FirstIndex, 
  IndexCount: Cardinal); cdecl; external AfterwarpLib;

procedure MeshBufferCalculateNormalsWeld(MeshBuffer: PMeshBufferRec; FirstVertex, VertexCount, FirstIndex, 
  IndexCount: Cardinal; WeldEpsilon: Single); cdecl; external AfterwarpLib;

procedure MeshBufferInvertNormals(MeshBuffer: PMeshBufferRec; FirstVertex, VertexCount: Cardinal); cdecl;
  external AfterwarpLib;

procedure MeshBufferInvertIndexOrder(MeshBuffer: PMeshBufferRec; FirstIndex, IndexCount: Cardinal); cdecl;
  external AfterwarpLib;

procedure MeshBufferCalculateBounds(MeshBuffer: PMeshBufferRec; VertexMin, VertexMax: PVector3f; FirstVertex, 
  VertexCount: Cardinal); cdecl; external AfterwarpLib;

procedure MeshBufferCentralize(MeshBuffer: PMeshBufferRec; FirstVertex, VertexCount: Cardinal); cdecl;
  external AfterwarpLib;

procedure MeshBufferEliminateUnusedVertices(MeshBuffer: PMeshBufferRec; FirstVertex, VertexCount: Cardinal);
  cdecl; external AfterwarpLib;

function MeshBufferJoinDuplicateVertices(MeshBuffer: PMeshBufferRec; FirstVertex, VertexCount: Cardinal; 
  Treshold: Single): TLibraryBool; cdecl; external AfterwarpLib;

function MeshBufferCombine(MeshBuffer, MeshBufferSource: PMeshBufferRec; FirstVertex, VertexCount, 
  FirstIndex, IndexCount: Cardinal): TLibraryBool; cdecl; external AfterwarpLib;

procedure MeshBufferClear(MeshBuffer: PMeshBufferRec); cdecl; external AfterwarpLib;

procedure MeshBufferSetTransform(MeshBuffer: PMeshBufferRec; Transform: PMatrix4f); cdecl;
  external AfterwarpLib;

procedure MeshBufferGetTransform(MeshBuffer: PMeshBufferRec; Transform: PMatrix4f); cdecl;
  external AfterwarpLib;

function MeshBufferTransferVertexElements(MeshBuffer: PMeshBufferRec; Buffer: Pointer; 
  VertexElementEntries: PVertexElementEntry; VertexElementCount, FirstVertex, VertexCount, Channel, 
  SemanticIndex: Cardinal): Cardinal; cdecl; external AfterwarpLib;

function MeshBufferTransferIndexElements(MeshBuffer: PMeshBufferRec; Buffer: Pointer; Pitch, FirstVertex, 
  FirstIndex, IndexCount: Cardinal): Cardinal; cdecl; external AfterwarpLib;

function MeshBufferTransferVertices(MeshBuffer: PMeshBufferRec; Buffer: PBufferRec; 
  VertexElementEntries: PVertexElementEntry; VertexElementCount, FirstVertex, VertexCount, Channel, Offset, 
  SemanticIndex: Cardinal): TLibraryBool; cdecl; external AfterwarpLib;

function MeshBufferTransferIndices(MeshBuffer: PMeshBufferRec; Buffer: PBufferRec; FirstVertex, FirstIndex, 
  IndexCount, Offset: Cardinal): TLibraryBool; cdecl; external AfterwarpLib;

function MeshBufferLoadMeshFromFile(MeshBuffer: PMeshBufferRec; Tags: PMeshMetaTagsRec; FileName: PAnsiChar; 
  MinBounds, MaxBounds: PVector3f; Debug: PAnsiChar; DebugMaxLength: Cardinal; DebugLength: PCardinal)
  : TLibraryBool; cdecl; external AfterwarpLib;

function MeshBufferSaveMeshToFile(MeshBuffer: PMeshBufferRec; FileName: PAnsiChar; ExportFlags: Cardinal; 
  Debug: PAnsiChar; DebugMaxLength: Cardinal; DebugLength: PCardinal): TLibraryBool; cdecl;
  external AfterwarpLib;

function CanvasCreate(Device: PDeviceRec): PCanvasRec; cdecl; external AfterwarpLib;

procedure CanvasDestroy(Canvas: PCanvasRec); cdecl; external AfterwarpLib;

function CanvasBegin(Canvas: PCanvasRec): TLibraryBool; cdecl; external AfterwarpLib;

procedure CanvasEnd(Canvas: PCanvasRec); cdecl; external AfterwarpLib;

procedure CanvasPixels(Canvas: PCanvasRec; Positions: PPoint2f; Colors: PIntColor; ElementCount: Integer; 
  Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasLines(Canvas: PCanvasRec; Vertices: PPoint2f; Colors: PIntColor; Indices: PCardinal; 
  VertexCount, PrimitiveCount: Integer; Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasTriangles(Canvas: PCanvasRec; Vertices: PPoint2f; Colors: PIntColor; Indices: PCardinal; 
  VertexCount, PrimitiveCount: Integer; Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasTexturedTriangles(Canvas: PCanvasRec; Texture: PTextureRec; Vertices, TexCoords: PPoint2f; 
  Colors: PIntColor; Indices: PCardinal; VertexCount, PrimitiveCount: Integer; Effect: TBlendingEffect);
  cdecl; external AfterwarpLib;

procedure CanvasPixel(Canvas: PCanvasRec; Position: PPoint2f; Color: TIntColor; Effect: TBlendingEffect);
  cdecl; external AfterwarpLib;

procedure CanvasLine(Canvas: PCanvasRec; SrcPos, DestPos: PPoint2f; Colors: PColorPair; 
  Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasThickLine(Canvas: PCanvasRec; SrcPos, DestPos: PPoint2f; Colors: PColorPair; 
  Thickness: Single; Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasLineEllipse(Canvas: PCanvasRec; Origin, Radius: PPoint2f; Steps: Integer; Color: TIntColor; 
  Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasLineCircle(Canvas: PCanvasRec; Origin: PPoint2f; Radius: Single; Steps: Integer; 
  Color: TIntColor; Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasThickLineEllipse(Canvas: PCanvasRec; Origin, Radius: PPoint2f; Steps: Integer; 
  Color: TIntColor; Thickness: Single; Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasThickLineCircle(Canvas: PCanvasRec; Origin: PPoint2f; Radius: Single; Steps: Integer; 
  Color: TIntColor; Thickness: Single; Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasLineTriangle(Canvas: PCanvasRec; Vertex1, Vertex2, Vertex3: PPoint2f; Color1, Color2, 
  Color3: TIntColor; Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasThickLineTriangle(Canvas: PCanvasRec; Vertex1, Vertex2, Vertex3: PPoint2f; Color1, Color2, 
  Color3: TIntColor; Thickness: Single; Joint: TPathJoint; SmoothStep: Single; Effect: TBlendingEffect);
  cdecl; external AfterwarpLib;

procedure CanvasLineQuad(Canvas: PCanvasRec; Vertices: PQuad; Colors: PColorRect; Effect: TBlendingEffect);
  cdecl; external AfterwarpLib;

procedure CanvasThickLineQuad(Canvas: PCanvasRec; Vertices: PQuad; Colors: PColorRect; Thickness: Single; 
  Joint: TPathJoint; SmoothStep: Single; Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasLineHexagon(Canvas: PCanvasRec; Matrix: PMatrix3f; Color1, Color2, Color3, Color4, Color5, 
  Color6: TIntColor; Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasLineHexagonGrad(Canvas: PCanvasRec; Matrix: PMatrix3f; Colors: PColorRect; 
  Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasThickLineHexagon(Canvas: PCanvasRec; Matrix: PMatrix3f; Color1, Color2, Color3, Color4, 
  Color5, Color6: TIntColor; Thickness: Single; Joint: TPathJoint; SmoothStep: Single; 
  Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasThickLineHexagonGrad(Canvas: PCanvasRec; Matrix: PMatrix3f; Colors: PColorRect; 
  Thickness: Single; Joint: TPathJoint; SmoothStep: Single; Effect: TBlendingEffect); cdecl;
  external AfterwarpLib;

procedure CanvasTriangle(Canvas: PCanvasRec; Vertex1, Vertex2, Vertex3: PPoint2f; Color1, Color2, 
  Color3: TIntColor; Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasQuad(Canvas: PCanvasRec; Vertices: PQuad; Colors: PColorRect; Effect: TBlendingEffect); cdecl;
  external AfterwarpLib;

procedure CanvasFillRect(Canvas: PCanvasRec; Rect: PFloatRect; Colors: PColorRect; Effect: TBlendingEffect);
  cdecl; external AfterwarpLib;

procedure CanvasFrameRect(Canvas: PCanvasRec; Rect: PFloatRect; Colors: PColorRect; Thickness: Single; 
  Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasFillRoundRect(Canvas: PCanvasRec; Rect: PFloatRect; Colors: PColorRect; Radius: Single; 
  Steps: Cardinal; Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasFrameRoundRect(Canvas: PCanvasRec; Rect: PFloatRect; Colors: PColorRect; Radius, 
  Thickness: Single; Steps: Cardinal; Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasFillRoundRectTop(Canvas: PCanvasRec; Rect: PFloatRect; Colors: PColorRect; Radius: Single; 
  Steps: Integer; Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasFillRoundRectBottom(Canvas: PCanvasRec; Rect: PFloatRect; Colors: PColorRect; Radius: Single; 
  Steps: Integer; Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasFillRoundRectTopInverse(Canvas: PCanvasRec; Rect: PFloatRect; Colors: PColorRect; 
  Radius: Single; Steps: Integer; Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasHighlight(Canvas: PCanvasRec; Rect: PFloatRect; CornerRadius, Luma, Distance: Single; 
  Steps: Integer); cdecl; external AfterwarpLib;

procedure CanvasHexagon(Canvas: PCanvasRec; Matrix: PMatrix3f; Color1, Color2, Color3, Color4, Color5, 
  Color6: TIntColor; Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasHexagonGrad(Canvas: PCanvasRec; Matrix: PMatrix3f; Colors: PColorRect; 
  Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasArc(Canvas: PCanvasRec; Origin, Radius: PPoint2f; InitAngle, EndAngle: Single; 
  Steps: Integer; Colors: PColorRect; Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasEllipse(Canvas: PCanvasRec; Origin, Radius: PPoint2f; Steps: Integer; Colors: PColorRect; 
  Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasRibbonGrad(Canvas: PCanvasRec; Origin, InsideRadius, OutsideRadius: PPoint2f; Steps: Integer; 
  Colors: PColorRect; Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasRibbonTri(Canvas: PCanvasRec; Origin, InsideRadius, OutsideRadius: PPoint2f; Steps: Integer; 
  Color1, Color2, Color3: PColorPair; Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasRibbon(Canvas: PCanvasRec; Origin, InsideRadius, OutsideRadius: PPoint2f; Steps: Integer; 
  Color: TIntColor; Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasTapeGrad(Canvas: PCanvasRec; Origin, InsideRadius, OutsideRadius: PPoint2f; InitAngle, 
  EndAngle: Single; Steps: Integer; Colors: PColorRect; Effect: TBlendingEffect); cdecl;
  external AfterwarpLib;

procedure CanvasTapeTri(Canvas: PCanvasRec; Origin, InsideRadius, OutsideRadius: PPoint2f; InitAngle, 
  EndAngle: Single; Steps: Integer; Color1, Color2, Color3: PColorPair; Effect: TBlendingEffect); cdecl;
  external AfterwarpLib;

procedure CanvasTape(Canvas: PCanvasRec; Origin, InsideRadius, OutsideRadius: PPoint2f; InitAngle, 
  EndAngle: Single; Steps: Integer; Color: TIntColor; Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasRectWithHole(Canvas: PCanvasRec; Rect: PFloatRect; HoleOrigin, HoleRadius: PPoint2f; 
  Colors: PColorPair; Steps: Integer; Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasThickLinePath(Canvas: PCanvasRec; Points: PPoint2f; Colors: PIntColor; Joints: PPathJoint; 
  Count: Cardinal; Thickness: Single; BaseJoint: TPathJoint; LineCaps: TLineCaps; MiterLimit, 
  SmoothStep: Single; ClosePath: TLibraryBool; Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasPolygon(Canvas: PCanvasRec; Contours: PPoint2f; ContourLengths: PCardinal; 
  ContourCount: Cardinal; Color: TIntColor; WindingRule: TTessellationWinding; Effect: TBlendingEffect);
  cdecl; external AfterwarpLib;

procedure CanvasTexturedTriangle(Canvas: PCanvasRec; Texture: PTextureRec; Vertex1, Vertex2, Vertex3, 
  TexCoord1, TexCoord2, TexCoord3: PPoint2f; Color1, Color2, Color3: TIntColor; Effect: TBlendingEffect);
  cdecl; external AfterwarpLib;

procedure CanvasTexturedTriangleRegion(Canvas: PCanvasRec; Texture: PTextureRec; Vertex1, Vertex2, Vertex3, 
  TexCoord1, TexCoord2, TexCoord3: PPoint2f; Color1, Color2, Color3: TIntColor; Effect: TBlendingEffect);
  cdecl; external AfterwarpLib;

procedure CanvasTexturedQuad(Canvas: PCanvasRec; Texture: PTextureRec; Vertices, TexCoords: PQuad; 
  Colors: PColorRect; Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasTexturedQuadRegion(Canvas: PCanvasRec; Texture: PTextureRec; Vertices, TexCoords: PQuad; 
  Colors: PColorRect; Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasTexturedRoundRect(Canvas: PCanvasRec; Texture: PTextureRec; Rect: PFloatRect; 
  TexCoords: PQuad; Colors: PColorRect; Radius: Single; Steps: Integer; Effect: TBlendingEffect); cdecl;
  external AfterwarpLib;

procedure CanvasTexturedRoundRectRegion(Canvas: PCanvasRec; Texture: PTextureRec; Rect: PFloatRect; 
  TexCoords: PQuad; Colors: PColorRect; Radius: Single; Steps: Integer; Effect: TBlendingEffect); cdecl;
  external AfterwarpLib;

procedure CanvasQuadImage(Canvas: PCanvasRec; ImageAtlas: PImageAtlasRec; Vertices: PQuad; 
  RegionIndex: Integer; Colors: PColorRect; SourceRect: PFloatRect; Modifiers: Cardinal; 
  Effect: TBlendingEffect); cdecl; external AfterwarpLib;

procedure CanvasSetTransform(Canvas: PCanvasRec; Transform: PMatrix4f); cdecl; external AfterwarpLib;

procedure CanvasGetTransform(Canvas: PCanvasRec; Transform: PMatrix4f); cdecl; external AfterwarpLib;

procedure CanvasSetSignedDistanceField(Canvas: PCanvasRec; _SignedDistanceField: PSignedDistanceField); cdecl;
  external AfterwarpLib;

procedure CanvasGetSignedDistanceField(Canvas: PCanvasRec; _SignedDistanceField: PSignedDistanceField); cdecl;
  external AfterwarpLib;

function CanvasGetContextState(Canvas: PCanvasRec): TCanvasContextState; cdecl; external AfterwarpLib;

function CanvasSetContextState(Canvas: PCanvasRec; ContextState: TCanvasContextState): TLibraryBool; cdecl;
  external AfterwarpLib;

function CanvasSetSamplerState(Canvas: PCanvasRec; SamplerState: PCanvasSamplerState): TLibraryBool; cdecl;
  external AfterwarpLib;

function CanvasGetSamplerState(Canvas: PCanvasRec; SamplerState: PCanvasSamplerState): TLibraryBool; cdecl;
  external AfterwarpLib;

procedure CanvasResetSamplerState(Canvas: PCanvasRec); cdecl; external AfterwarpLib;

procedure CanvasSetAttributes(Canvas: PCanvasRec; Attributes: Cardinal); cdecl; external AfterwarpLib;

function CanvasGetAttributes(Canvas: PCanvasRec): Cardinal; cdecl; external AfterwarpLib;

procedure CanvasFlush(Canvas: PCanvasRec); cdecl; external AfterwarpLib;

procedure CanvasReset(Canvas: PCanvasRec); cdecl; external AfterwarpLib;

procedure CanvasResetCache(Canvas: PCanvasRec); cdecl; external AfterwarpLib;

function CanvasGetBatchCount(Canvas: PCanvasRec): Integer; cdecl; external AfterwarpLib;

procedure CanvasGetClipRect(Canvas: PCanvasRec; ClipRect: PIntRect); cdecl; external AfterwarpLib;

function CanvasSetClipRect(Canvas: PCanvasRec; Rect: PIntRect): TLibraryBool; cdecl; external AfterwarpLib;

function ImageAtlasCreate(Device: PDeviceRec): PImageAtlasRec; cdecl; external AfterwarpLib;

procedure ImageAtlasDestroy(Atlas: PImageAtlasRec); cdecl; external AfterwarpLib;

function ImageAtlasTextureCount(Atlas: PImageAtlasRec): Integer; cdecl; external AfterwarpLib;

function ImageAtlasTexture(Atlas: PImageAtlasRec; TextureIndex: Integer): PTextureRec; cdecl;
  external AfterwarpLib;

function ImageAtlasRegionCount(Atlas: PImageAtlasRec): Integer; cdecl; external AfterwarpLib;

function ImageAtlasRegion(Atlas: PImageAtlasRec; RegionIndex: Integer; Region: PImageRegion): TLibraryBool;
  cdecl; external AfterwarpLib;

function ImageAtlasCreateRegion(Atlas: PImageAtlasRec; Rect: PIntRect; TextureIndex: Integer): Integer; cdecl;
  external AfterwarpLib;

procedure ImageAtlasRemoveRegion(Atlas: PImageAtlasRec; RegionIndex: Integer); cdecl; external AfterwarpLib;

procedure ImageAtlasClearRegions(Atlas: PImageAtlasRec); cdecl; external AfterwarpLib;

procedure ImageAtlasMakeRegions(Atlas: PImageAtlasRec; PatternSize, VisibleSize: PPoint2i; 
  PatternCount: Integer); cdecl; external AfterwarpLib;

procedure ImageAtlasClearTextures(Atlas: PImageAtlasRec); cdecl; external AfterwarpLib;

procedure ImageAtlasRemoveTexture(Atlas: PImageAtlasRec; TextureIndex: Integer); cdecl; external AfterwarpLib;

function ImageAtlasCreateTexture(Atlas: PImageAtlasRec; Size: PPoint2i; Format: TPixelFormat; 
  Attributes: TTextureAttributes): Integer; cdecl; external AfterwarpLib;

function ImageAtlasPackRegion(Atlas: PImageAtlasRec; Size: PPoint2i; Padding: Integer): Integer; cdecl;
  external AfterwarpLib;

function ImageAtlasPackSurface(Atlas: PImageAtlasRec; Surface: PRasterSurfaceRec; SourceRect: PIntRect; 
  Padding: Integer): Integer; cdecl; external AfterwarpLib;

function TextRendererCreate(Canvas: PCanvasRec; TextureSize: PPoint2i; PixelFormat: TPixelFormat; 
  MipMapping: TLibraryBool): PTextRendererRec; cdecl; external AfterwarpLib;

procedure TextRendererDestroy(TextRenderer: PTextRendererRec); cdecl; external AfterwarpLib;

function TextRendererGetCanvas(TextRenderer: PTextRendererRec): PCanvasRec; cdecl; external AfterwarpLib;

function TextRendererSetFontSettings(TextRenderer: PTextRendererRec; Settings: PFontSettings): TLibraryBool;
  cdecl; external AfterwarpLib;

function TextRendererGetFontSettings(TextRenderer: PTextRendererRec; Settings: PFontSettings): TLibraryBool;
  cdecl; external AfterwarpLib;

procedure TextRendererExtent(TextRenderer: PTextRendererRec; Text: PAnsiChar; Dimensions: PPoint2f; 
  Modifiers: PTextRenderModifiers); cdecl; external AfterwarpLib;

procedure TextRendererExtentByPixels(TextRenderer: PTextRendererRec; Text: PAnsiChar; Rect: PFloatRect; 
  Modifiers: PTextRenderModifiers); cdecl; external AfterwarpLib;

function TextRendererRects(TextRenderer: PTextRendererRec; Extent: PPoint2f; Rects: PTextEntryRect; 
  Text: PAnsiChar; Modifiers: PTextRenderModifiers): Integer; cdecl; external AfterwarpLib;

procedure TextRendererDraw(TextRenderer: PTextRendererRec; Position: PPoint2f; Text: PAnsiChar; 
  Colors: PColorPair; Alpha: Single; Modifiers: PTextRenderModifiers); cdecl; external AfterwarpLib;

procedure TextRendererDrawAligned(TextRenderer: PTextRendererRec; Position: PPoint2f; Text: PAnsiChar; 
  Colors: PColorPair; HorizAlign, VertAlign: TTextAlignment; Alpha: Single; AlignToPixels: TLibraryBool; 
  Modifiers: PTextRenderModifiers); cdecl; external AfterwarpLib;

procedure TextRendererDrawCentered(TextRenderer: PTextRendererRec; Position: PPoint2f; Text: PAnsiChar; 
  Colors: PColorPair; Alpha: Single; AlignToPixels: TLibraryBool; Modifiers: PTextRenderModifiers); cdecl;
  external AfterwarpLib;

procedure TextRendererDrawAlignedByPixels(TextRenderer: PTextRendererRec; Position: PPoint2f; 
  Text: PAnsiChar; Colors: PColorPair; HorizAlign, VertAlign: TTextAlignment; Alpha: Single; 
  AlignToPixels: TLibraryBool; Modifiers: PTextRenderModifiers); cdecl; external AfterwarpLib;

procedure TextRendererDrawCenteredByPixels(TextRenderer: PTextRendererRec; Position: PPoint2f; 
  Text: PAnsiChar; Colors: PColorPair; Alpha: Single; AlignToPixels: TLibraryBool; 
  Modifiers: PTextRenderModifiers); cdecl; external AfterwarpLib;

function GrapherCreate(Device: PDeviceRec): PGrapherRec; cdecl; external AfterwarpLib;

procedure GrapherDestroy(Grapher: PGrapherRec); cdecl; external AfterwarpLib;

function GrapherBegin(Grapher: PGrapherRec): TLibraryBool; cdecl; external AfterwarpLib;

procedure GrapherEnd(Grapher: PGrapherRec); cdecl; external AfterwarpLib;

procedure GrapherPoints(Grapher: PGrapherRec; Vertices: PVector4f; Colors: PIntColor; Angles: PSingle; 
  ElementCount: Integer; Shape: TPointShape); cdecl; external AfterwarpLib;

procedure GrapherLines(Grapher: PGrapherRec; Vertices: PVector4f; Colors: PIntColor; Indices: PCardinal; 
  VertexCount, IndexCount: Integer; Caps: TLineCaps); cdecl; external AfterwarpLib;

procedure GrapherPoint(Grapher: PGrapherRec; Position: PVector3f; Color: TIntColor; Size: Single; 
  Shape: TPointShape; Angle: Single); cdecl; external AfterwarpLib;

procedure GrapherLine(Grapher: PGrapherRec; Position1, Position2: PVector3f; Color1, Color2: TIntColor; 
  Thickness1, Thickness2: Single; Caps: TLineCaps); cdecl; external AfterwarpLib;

procedure GrapherArrow(Grapher: PGrapherRec; TargetSize: PPoint2f; Origin, Destination: PVector3f; 
  Color: TIntColor; Thickness, Size: Single; Caps: TLineCaps); cdecl; external AfterwarpLib;

procedure GrapherBoundingBox(Grapher: PGrapherRec; Volume: PMatrix4f; Color: TIntColor; Thickness, 
  Length: Single; Caps: TLineCaps); cdecl; external AfterwarpLib;

procedure GrapherDottedLine(Grapher: PGrapherRec; Position1, Position2: PVector3f; Color1, Color2: TIntColor; 
  Thickness1, Thickness2, Sparsity: Single; Shape: TPointShape; Angle: Single); cdecl; external AfterwarpLib;

procedure GrapherFlush(Grapher: PGrapherRec); cdecl; external AfterwarpLib;

procedure GrapherReset(Grapher: PGrapherRec); cdecl; external AfterwarpLib;

procedure GrapherGetTransform(Grapher: PGrapherRec; Transform: PMatrix4f); cdecl; external AfterwarpLib;

procedure GrapherSetTransform(Grapher: PGrapherRec; Transform: PMatrix4f); cdecl; external AfterwarpLib;

function GrapherGetBatchCount(Grapher: PGrapherRec): Integer; cdecl; external AfterwarpLib;

function GaussianBlurCreate(Device: PDeviceRec): PGaussianBlurRec; cdecl; external AfterwarpLib;

procedure GaussianBlurDestroy(GaussianBlur: PGaussianBlurRec); cdecl; external AfterwarpLib;

function GaussianBlurGetSamples(GaussianBlur: PGaussianBlurRec): Integer; cdecl; external AfterwarpLib;

procedure GaussianBlurSetSamples(GaussianBlur: PGaussianBlurRec; Samples: Integer); cdecl;
  external AfterwarpLib;

function GaussianBlurGetSigma(GaussianBlur: PGaussianBlurRec): Single; cdecl; external AfterwarpLib;

procedure GaussianBlurSetSigma(GaussianBlur: PGaussianBlurRec; Sigma: Single); cdecl; external AfterwarpLib;

function GaussianBlurGetLuma(GaussianBlur: PGaussianBlurRec): Single; cdecl; external AfterwarpLib;

procedure GaussianBlurSetLuma(GaussianBlur: PGaussianBlurRec; Luma: Single); cdecl; external AfterwarpLib;

function GaussianBlurGetChroma(GaussianBlur: PGaussianBlurRec): Single; cdecl; external AfterwarpLib;

procedure GaussianBlurSetChroma(GaussianBlur: PGaussianBlurRec; Chroma: Single); cdecl; external AfterwarpLib;

function GaussianBlurUpdate(GaussianBlur: PGaussianBlurRec; Destination, Intermediary, Source: PTextureRec)
  : TLibraryBool; cdecl; external AfterwarpLib;

function GaussianHighlightCreate(Device: PDeviceRec): PGaussianHighlightRec; cdecl; external AfterwarpLib;

procedure GaussianHighlightDestroy(GaussianHighlight: PGaussianHighlightRec); cdecl; external AfterwarpLib;

procedure GaussianHighlightGetParameters(GaussianHighlight: PGaussianHighlightRec; 
  Parameters: PGaussianHighlightParameters); cdecl; external AfterwarpLib;

procedure GaussianHighlightSetParameters(GaussianHighlight: PGaussianHighlightRec; 
  Parameters: PGaussianHighlightParameters); cdecl; external AfterwarpLib;

procedure GaussianHighlightGetRectangle(GaussianHighlight: PGaussianHighlightRec; Rectangle: PFloatRect);
  cdecl; external AfterwarpLib;

procedure GaussianHighlightSetRectangle(GaussianHighlight: PGaussianHighlightRec; Rectangle: PFloatRect);
  cdecl; external AfterwarpLib;

function GaussianHighlightUpdate(GaussianHighlight: PGaussianHighlightRec; Destination, Intermediary, 
  Source: PTextureRec): TLibraryBool; cdecl; external AfterwarpLib;

procedure SceneTextureDestroy(SceneTexture: PSceneTextureRec); cdecl; external AfterwarpLib;

procedure SceneTextureGetSize(SceneTexture: PSceneTextureRec; Size: PPoint2i); cdecl; external AfterwarpLib;

function SceneTextureGetSamples(SceneTexture: PSceneTextureRec): Integer; cdecl; external AfterwarpLib;

function SceneTextureRetrieve(SceneTexture: PSceneTextureRec; TextureType: TSceneTextureType): PTextureRec;
  cdecl; external AfterwarpLib;

function SceneTextureBegin(SceneTexture: PSceneTextureRec): TLibraryBool; cdecl; external AfterwarpLib;

procedure SceneTextureEnd(SceneTexture: PSceneTextureRec); cdecl; external AfterwarpLib;

function SceneTextureRendering(SceneTexture: PSceneTextureRec): TLibraryBool; cdecl; external AfterwarpLib;

function ShadowTextureCreate(Device: PDeviceRec; ShadowTechnique: TShadowTechnique; Size: PPoint2i; 
  Samples: Integer; DepthReversed: TLibraryBool): PSceneTextureRec; cdecl; external AfterwarpLib;

function ShadowTextureGetTechnique(SceneTexture: PSceneTextureRec): TShadowTechnique; cdecl;
  external AfterwarpLib;

procedure ShadowTextureGetParameters(SceneTexture: PSceneTextureRec; Parameters: PShadowParameters); cdecl;
  external AfterwarpLib;

procedure ShadowTextureSetParameters(SceneTexture: PSceneTextureRec; Parameters: PShadowParameters); cdecl;
  external AfterwarpLib;

function ShadowTextureFilter(SceneTexture: PSceneTextureRec): TLibraryBool; cdecl; external AfterwarpLib;

function ModelTextureCreate(Device: PDeviceRec; Size: PPoint2i; Format, DepthStencil: TPixelFormat; 
  Samples: Integer): PSceneTextureRec; cdecl; external AfterwarpLib;

function ModelTextureGetFormat(SceneTexture: PSceneTextureRec): TPixelFormat; cdecl; external AfterwarpLib;

function ModelTextureGetDepthStencil(SceneTexture: PSceneTextureRec): TPixelFormat; cdecl;
  external AfterwarpLib;

function ModelTextureSetSize(SceneTexture: PSceneTextureRec; Size: PPoint2i): TLibraryBool; cdecl;
  external AfterwarpLib;

function ModelTexturePresent(SceneTexture: PSceneTextureRec): TLibraryBool; cdecl; external AfterwarpLib;

function GlassTextureCreate(Device: PDeviceRec; Technique: TGlassTechnique; Size: PPoint2i; 
  Attributes: TSceneAttributes; Format: TPixelFormat; Samples: Integer): PSceneTextureRec; cdecl;
  external AfterwarpLib;

function GlassTextureGetTechnique(SceneTexture: PSceneTextureRec): TGlassTechnique; cdecl;
  external AfterwarpLib;

function GlassTextureGetAttributes(SceneTexture: PSceneTextureRec): TSceneAttributes; cdecl;
  external AfterwarpLib;

function GlassTextureGetFormat(SceneTexture: PSceneTextureRec): TPixelFormat; cdecl; external AfterwarpLib;

procedure GlassTextureGetBackground(SceneTexture: PSceneTextureRec; Background: PFloatColor); cdecl;
  external AfterwarpLib;

procedure GlassTextureSetBackground(SceneTexture: PSceneTextureRec; Background: PFloatColor); cdecl;
  external AfterwarpLib;

procedure GlassTextureSetInput(SceneTexture: PSceneTextureRec; TextureType: TSceneTextureType; 
  Texture: PTextureRec); cdecl; external AfterwarpLib;

function GlassTextureSetSize(SceneTexture: PSceneTextureRec; Size: PPoint2i): TLibraryBool; cdecl;
  external AfterwarpLib;

function GlassTexturePresent(SceneTexture: PSceneTextureRec): TLibraryBool; cdecl; external AfterwarpLib;

procedure SceneDestroy(Scene: PSceneRec); cdecl; external AfterwarpLib;

function SceneGetShadowTechnique(Scene: PSceneRec): TShadowTechnique; cdecl; external AfterwarpLib;

procedure SceneGetShadowParameters(Scene: PSceneRec; Parameters: PShadowParameters); cdecl;
  external AfterwarpLib;

procedure SceneSetShadowParameters(Scene: PSceneRec; Parameters: PShadowParameters); cdecl;
  external AfterwarpLib;

function SceneGetAttributes(Scene: PSceneRec): TSceneAttributes; cdecl; external AfterwarpLib;

procedure SceneGetWorld(Scene: PSceneRec; World: PMatrix4f); cdecl; external AfterwarpLib;

procedure SceneSetWorld(Scene: PSceneRec; World: PMatrix4f); cdecl; external AfterwarpLib;

procedure SceneGetView(Scene: PSceneRec; View: PMatrix4f); cdecl; external AfterwarpLib;

procedure SceneSetView(Scene: PSceneRec; View: PMatrix4f); cdecl; external AfterwarpLib;

procedure SceneGetProjection(Scene: PSceneRec; Projection: PMatrix4f); cdecl; external AfterwarpLib;

procedure SceneSetProjection(Scene: PSceneRec; Projection: PMatrix4f); cdecl; external AfterwarpLib;

procedure SceneGetLightView(Scene: PSceneRec; LightView: PMatrix4f); cdecl; external AfterwarpLib;

procedure SceneSetLightView(Scene: PSceneRec; LightView: PMatrix4f); cdecl; external AfterwarpLib;

procedure SceneGetLightProjection(Scene: PSceneRec; LightProjection: PMatrix4f); cdecl; external AfterwarpLib;

procedure SceneSetLightProjection(Scene: PSceneRec; LightProjection: PMatrix4f); cdecl; external AfterwarpLib;

function SceneGetProgram(Scene: PSceneRec): PProgramRec; cdecl; external AfterwarpLib;

function SceneInstances(Scene: PSceneRec; Transforms: PMatrix4f; Colors: PFloatColor; Count: Cardinal)
  : TLibraryBool; cdecl; external AfterwarpLib;

function SceneBegin(Scene: PSceneRec): TLibraryBool; cdecl; external AfterwarpLib;

procedure SceneEnd(Scene: PSceneRec); cdecl; external AfterwarpLib;

function SceneRendering(Scene: PSceneRec): TLibraryBool; cdecl; external AfterwarpLib;

function ShadowSceneCreate(Device: PDeviceRec; Technique: TShadowTechnique; Attributes: TSceneAttributes; 
  VertexElements: PVertexElementEntry; VertexElementCount: Cardinal): PSceneRec; cdecl; external AfterwarpLib;

function ModelSceneCreate(Device: PDeviceRec; ShadowTechnique: TShadowTechnique; 
  Attributes: TSceneAttributes; VertexElements: PVertexElementEntry; VertexElementCount: Cardinal): PSceneRec;
  cdecl; external AfterwarpLib;

function ModelSceneGetTexture(Scene: PSceneRec; TextureType: TSceneTextureType): PTextureRec; cdecl;
  external AfterwarpLib;

function ModelSceneSetTexture(Scene: PSceneRec; TextureType: TSceneTextureType; Texture: PTextureRec)
  : TLibraryBool; cdecl; external AfterwarpLib;

function ModelSceneGetLight(Scene: PSceneRec; Index: Cardinal; Light: PSceneLight): TLibraryBool; cdecl;
  external AfterwarpLib;

function ModelSceneSetLight(Scene: PSceneRec; Index: Cardinal; Light: PSceneLight): TLibraryBool; cdecl;
  external AfterwarpLib;

function GlassSceneCreate(Device: PDeviceRec; GlassTechnique: TGlassTechnique; 
  ShadowTechnique: TShadowTechnique; Attributes: TSceneAttributes; Samples: Integer; 
  VertexElements: PVertexElementEntry; VertexElementCount: Cardinal): PSceneRec; cdecl; external AfterwarpLib;

function GlassSceneGetTechnique(Scene: PSceneRec): TGlassTechnique; cdecl; external AfterwarpLib;

function GlassSceneGetSamples(Scene: PSceneRec): Integer; cdecl; external AfterwarpLib;

function GlassSceneGetAssociate(Scene: PSceneRec): PSceneTextureRec; cdecl; external AfterwarpLib;

procedure GlassSceneSetAssociate(Scene: PSceneRec; Texture: PSceneTextureRec); cdecl; external AfterwarpLib;

function GlassSceneIterate(Scene: PSceneRec): TLibraryBool; cdecl; external AfterwarpLib;

function VolumeSurfaceShadowSceneCreate(Device: PDeviceRec; Technique: TShadowTechnique; 
  Attributes: TSceneAttributes): PSceneRec; cdecl; external AfterwarpLib;

function VolumeSurfaceSceneCreate(Device: PDeviceRec; ShadowTechnique: TShadowTechnique; 
  Attributes: TSceneAttributes): PSceneRec; cdecl; external AfterwarpLib;

function VolumeSurfaceSceneSetParameters(Scene: PSceneRec; Parameters: PVolumeSurfaceParameters)
  : TLibraryBool; cdecl; external AfterwarpLib;

procedure VolumeSurfaceSceneGetParameters(Scene: PSceneRec; Parameters: PVolumeSurfaceParameters); cdecl;
  external AfterwarpLib;

procedure VolumeSurfaceSceneSetTexture(Scene: PSceneRec; Texture: PTextureRec); cdecl; external AfterwarpLib;

function VolumeSurfaceSceneExecute(Scene: PSceneRec): TLibraryBool; cdecl; external AfterwarpLib;

function RasterSurfaceCreate(Width, Height: Integer; Format: TPixelFormat): PRasterSurfaceRec; cdecl;
  external AfterwarpLib;

function RasterSurfaceCreateFromFile(FileName: PAnsiChar; FormatRequest: TAlphaFormatRequest)
  : PRasterSurfaceRec; cdecl; external AfterwarpLib;

function RasterSurfaceCreateFromFileInMemory(FileContent: Pointer; ContentSize: Cardinal; 
  Extension: PAnsiChar; FormatRequest: TAlphaFormatRequest): PRasterSurfaceRec; cdecl; external AfterwarpLib;

procedure RasterSurfaceDestroy(Surface: PRasterSurfaceRec); cdecl; external AfterwarpLib;

function RasterSurfaceReplicate(Surface: PRasterSurfaceRec): PRasterSurfaceRec; cdecl; external AfterwarpLib;

procedure RasterSurfaceGetParameters(Surface: PRasterSurfaceRec; Parameters: PRasterSurfaceParameters); cdecl;
  external AfterwarpLib;

function RasterSurfaceUpdate(Surface: PRasterSurfaceRec; Parameters: PRasterSurfaceParameters): TLibraryBool;
  cdecl; external AfterwarpLib;

function RasterSurfaceApproximateFormat(Surface: PRasterSurfaceRec; Format: TPixelFormat): TPixelFormat;
  cdecl; external AfterwarpLib;

function RasterSurfaceConvertFormat(Surface: PRasterSurfaceRec; PixelFormat: TPixelFormat): TLibraryBool;
  cdecl; external AfterwarpLib;

function RasterSurfaceGetPixel(Surface: PRasterSurfaceRec; X, Y: Integer): TIntColor; cdecl;
  external AfterwarpLib;

procedure RasterSurfaceSetPixel(Surface: PRasterSurfaceRec; X, Y: Integer; Color: TIntColor); cdecl;
  external AfterwarpLib;

function RasterSurfaceCopyRect(Surface, Source: PRasterSurfaceRec; SourceRect: PIntRect; DestPos: PPoint2i)
  : TLibraryBool; cdecl; external AfterwarpLib;

function RasterSurfaceCopyFrom(Surface, Source: PRasterSurfaceRec): TLibraryBool; cdecl;
  external AfterwarpLib;

procedure RasterSurfaceClear(Surface: PRasterSurfaceRec); cdecl; external AfterwarpLib;

function RasterSurfaceClearWith(Surface: PRasterSurfaceRec; Color: TIntColor): TLibraryBool; cdecl;
  external AfterwarpLib;

function RasterSurfaceResetAlpha(Surface: PRasterSurfaceRec; Opaque: TLibraryBool): TLibraryBool; cdecl;
  external AfterwarpLib;

function RasterSurfaceHasAlphaChannel(Surface: PRasterSurfaceRec): TLibraryBool; cdecl; external AfterwarpLib;

function RasterSurfacePremultiplyAlpha(Surface: PRasterSurfaceRec): TLibraryBool; cdecl;
  external AfterwarpLib;

function RasterSurfaceUnpremultiplyAlpha(Surface: PRasterSurfaceRec): TLibraryBool; cdecl;
  external AfterwarpLib;

function RasterSurfaceMirror(Surface: PRasterSurfaceRec): TLibraryBool; cdecl; external AfterwarpLib;

function RasterSurfaceFlip(Surface: PRasterSurfaceRec): TLibraryBool; cdecl; external AfterwarpLib;

procedure RasterSurfaceFillRect(Surface: PRasterSurfaceRec; DestRect: PIntRect; Colors: PColorRect); cdecl;
  external AfterwarpLib;

procedure RasterSurfaceDrawPixel(Surface: PRasterSurfaceRec; X, Y: Integer; Color: TIntColor); cdecl;
  external AfterwarpLib;

procedure RasterSurfaceHorizLine(Surface: PRasterSurfaceRec; X, Y, LineWidth: Integer; Color: TIntColor);
  cdecl; external AfterwarpLib;

procedure RasterSurfaceVertLine(Surface: PRasterSurfaceRec; X, Y, LineHeight: Integer; Color: TIntColor);
  cdecl; external AfterwarpLib;

procedure RasterSurfaceFrameRect(Surface: PRasterSurfaceRec; DestRect: PIntRect; Color: TIntColor); cdecl;
  external AfterwarpLib;

procedure RasterSurfaceDraw(Surface, Source: PRasterSurfaceRec; DestPos: PPoint2i; SourceRect: PIntRect; 
  Colors: PColorRect); cdecl; external AfterwarpLib;

function RasterSurfaceShrinkToHalfFrom(Surface, Source: PRasterSurfaceRec): TLibraryBool; cdecl;
  external AfterwarpLib;

function RasterSurfaceGetBilinearPixel(Surface: PRasterSurfaceRec; X, Y: Single): TIntColor; cdecl;
  external AfterwarpLib;

function RasterSurfaceGetPixelWithEdge(Surface: PRasterSurfaceRec; X, Y: Integer): TIntColor; cdecl;
  external AfterwarpLib;

function RasterSurfaceCalculateSignedDistance(Surface: PRasterSurfaceRec; X, Y: Integer; Spread: Single)
  : Single; cdecl; external AfterwarpLib;

procedure RasterSurfaceStretchFrom(Surface, Source: PRasterSurfaceRec; DestRect, SourceRect: PFloatRect);
  cdecl; external AfterwarpLib;

procedure RasterSurfaceStretchBilinearFrom(Surface, Source: PRasterSurfaceRec; DestRect, 
  SourceRect: PFloatRect); cdecl; external AfterwarpLib;

function RasterSurfaceMakeSignedDistanceField(Surface, Source: PRasterSurfaceRec; Spread: Single; 
  DestPos: PPoint2i; SourceRect: PIntRect): TLibraryBool; cdecl; external AfterwarpLib;

function RasterSurfaceSaveToFile(Surface: PRasterSurfaceRec; FileName: PAnsiChar; Quality: SizeUInt)
  : TLibraryBool; cdecl; external AfterwarpLib;

function RasterSurfaceSaveToFileInMemory(Surface: PRasterSurfaceRec; FileContent: Pointer; 
  FileContentSize: Cardinal; Extension: PAnsiChar; Quality: SizeUInt): Cardinal; cdecl; external AfterwarpLib;

function TimerCreate: PTimerRec; cdecl; external AfterwarpLib;

procedure TimerDestroy(Timer: PTimerRec); cdecl; external AfterwarpLib;

procedure TimerSetSpeed(Timer: PTimerRec; Speed: Double); cdecl; external AfterwarpLib;

function TimerGetSpeed(Timer: PTimerRec): Double; cdecl; external AfterwarpLib;

procedure TimerSetMaxFrameRate(Timer: PTimerRec; MaxFrameRate: Integer); cdecl; external AfterwarpLib;

function TimerGetMaxFrameRate(Timer: PTimerRec): Integer; cdecl; external AfterwarpLib;

function TimerGetFrameRate(Timer: PTimerRec): Integer; cdecl; external AfterwarpLib;

function TimerGetLatency(Timer: PTimerRec): UInt64; cdecl; external AfterwarpLib;

procedure TimerExecute(Timer: PTimerRec; Active, AllowSleep: TLibraryBool); cdecl; external AfterwarpLib;

procedure TimerProcess(Timer: PTimerRec; SingleCallOnly: TLibraryBool); cdecl; external AfterwarpLib;

procedure TimerReset(Timer: PTimerRec); cdecl; external AfterwarpLib;

procedure TimerSetEvents(Timer: PTimerRec; EventTimer, EventProcess: TTimerFunc; User: Pointer); cdecl;
  external AfterwarpLib;

function ObjectCameraCreate(Position, Rotation: PVector3f; Distance: Single): PObjectCameraRec; cdecl;
  external AfterwarpLib;

procedure ObjectCameraDestroy(Camera: PObjectCameraRec); cdecl; external AfterwarpLib;

procedure ObjectCameraGetPosition(Camera: PObjectCameraRec; Position: PVector3f); cdecl;
  external AfterwarpLib;

procedure ObjectCameraSetPosition(Camera: PObjectCameraRec; Position: PVector3f); cdecl;
  external AfterwarpLib;

procedure ObjectCameraGetRotation(Camera: PObjectCameraRec; Rotation: PVector3f); cdecl;
  external AfterwarpLib;

procedure ObjectCameraSetRotation(Camera: PObjectCameraRec; Rotation: PVector3f); cdecl;
  external AfterwarpLib;

function ObjectCameraGetDistance(Camera: PObjectCameraRec): Single; cdecl; external AfterwarpLib;

procedure ObjectCameraSetDistance(Camera: PObjectCameraRec; Distance: Single); cdecl; external AfterwarpLib;

function ObjectCameraGetRotationLateral(Camera: PObjectCameraRec): Single; cdecl; external AfterwarpLib;

procedure ObjectCameraSetRotationLateral(Camera: PObjectCameraRec; Lateral: Single); cdecl;
  external AfterwarpLib;

procedure ObjectCameraZoom(Camera: PObjectCameraRec; Position, Size: PPoint2f; Projection: PMatrix4f; 
  Delta: Single); cdecl; external AfterwarpLib;

procedure ObjectCameraZoomOrtho(Camera: PObjectCameraRec; Position, Size: PPoint2f; Distance, 
  AdjustedDistance: Single; Projection, AdjustedProjection: PMatrix4f); cdecl; external AfterwarpLib;

procedure ObjectCameraGetConstraints(Camera: PObjectCameraRec; Raints: PObjectCameraConstraints); cdecl;
  external AfterwarpLib;

procedure ObjectCameraSetConstraints(Camera: PObjectCameraRec; Constraints: PObjectCameraConstraints); cdecl;
  external AfterwarpLib;

procedure ObjectCameraGetSensitivity(Camera: PObjectCameraRec; Sensitivity: PObjectCameraSensitivity); cdecl;
  external AfterwarpLib;

procedure ObjectCameraSetSensitivity(Camera: PObjectCameraRec; Sensitivity: PObjectCameraSensitivity); cdecl;
  external AfterwarpLib;

procedure ObjectCameraStartRotation(Camera: PObjectCameraRec; Touch: PPoint2f); cdecl; external AfterwarpLib;

procedure ObjectCameraStartMovement(Camera: PObjectCameraRec; Touch: PPoint2f); cdecl; external AfterwarpLib;

function ObjectCameraUpdate(Camera: PObjectCameraRec; Touch: PPoint2f): TLibraryBool; cdecl;
  external AfterwarpLib;

function ObjectCameraStop(Camera: PObjectCameraRec): TLibraryBool; cdecl; external AfterwarpLib;

procedure ObjectCameraGetView(Camera: PObjectCameraRec; View: PMatrix4f); cdecl; external AfterwarpLib;

function ApplicationCreate(Configuration: PApplicationConfiguration): PApplicationRec; cdecl;
  external AfterwarpLib;

procedure ApplicationDestroy(Application: PApplicationRec); cdecl; external AfterwarpLib;

procedure ApplicationSetEvents(Application: PApplicationRec; Events: PApplicationEvents;
  User: Pointer); cdecl; external AfterwarpLib;

function ApplicationGetWindowHandle(Application: PApplicationRec): TUntypedHandle; cdecl;
  external AfterwarpLib;

function ApplicationSetTitle(Application: PApplicationRec; Title: PAnsiChar): TLibraryBool; cdecl;
  external AfterwarpLib;

procedure ApplicationGetTitle(Application: PApplicationRec; Title: PAnsiChar; TitleLength: PInteger); cdecl;
  external AfterwarpLib;

procedure ApplicationGetExecutablePath(Application: PApplicationRec; Path: PAnsiChar; PathLength: PInteger);
  cdecl; external AfterwarpLib;

procedure ApplicationGetWindowRect(Application: PApplicationRec; Rect: PIntRect); cdecl;
  external AfterwarpLib;

procedure ApplicationSetWindowRect(Application: PApplicationRec; Rect: PIntRect); cdecl;
  external AfterwarpLib;

procedure ApplicationGetClientRect(Application: PApplicationRec; Rect: PIntRect); cdecl;
  external AfterwarpLib;

procedure ApplicationSetClientSize(Application: PApplicationRec; Size: PPoint2i); cdecl;
  external AfterwarpLib;

function ApplicationGetWindowScale(Application: PApplicationRec): Single; cdecl; external AfterwarpLib;

procedure ApplicationInvalidate(Application: PApplicationRec); cdecl; external AfterwarpLib;

function ApplicationTranslateVirtualKey(Application: PApplicationRec; VirtualKey: Integer): TKey; cdecl;
  external AfterwarpLib;

function ApplicationConvertPortableKey(Application: PApplicationRec; Key: TKey): Integer; cdecl;
  external AfterwarpLib;

function ApplicationExecute(Application: PApplicationRec): TLibraryBool; cdecl; external AfterwarpLib;

function ObjectModelGetOwner(ObjectModel: PObjectModelRec): PObjectModelsRec; cdecl; external AfterwarpLib;

function ObjectModelGetPayload(ObjectModel: PObjectModelRec): TObjectPayload; cdecl; external AfterwarpLib;

function ObjectModelGetName(ObjectModel: PObjectModelRec): PAnsiChar; cdecl; external AfterwarpLib;

function ObjectModelSetName(ObjectModel: PObjectModelRec; Name: PAnsiChar): TLibraryBool; cdecl;
  external AfterwarpLib;

function ObjectModelGetVoxel(ObjectModel: PObjectModelRec): PMeshVoxelRec; cdecl; external AfterwarpLib;

procedure ObjectModelSetVoxel(ObjectModel: PObjectModelRec; MeshVoxel: PMeshVoxelRec); cdecl;
  external AfterwarpLib;

function ObjectModelGetMesh(ObjectModel: PObjectModelRec): PSceneMeshRec; cdecl; external AfterwarpLib;

function ObjectModelSetMesh(ObjectModel: PObjectModelRec; SceneMesh: PSceneMeshRec): TLibraryBool; cdecl;
  external AfterwarpLib;

function ObjectModelGetMeshName(ObjectModel: PObjectModelRec): PAnsiChar; cdecl; external AfterwarpLib;

function ObjectModelSetMeshName(ObjectModel: PObjectModelRec; MeshName: PAnsiChar): TLibraryBool; cdecl;
  external AfterwarpLib;

function ObjectModelGetParent(ObjectModel: PObjectModelRec): PObjectModelRec; cdecl; external AfterwarpLib;

function ObjectModelSetParent(ObjectModel, Parent: PObjectModelRec): TLibraryBool; cdecl;
  external AfterwarpLib;

function ObjectModelGetChildCount(ObjectModel: PObjectModelRec): Integer; cdecl; external AfterwarpLib;

function ObjectModelGetChild(ObjectModel: PObjectModelRec; Index: Integer): PObjectModelRec; cdecl;
  external AfterwarpLib;

procedure ObjectModelGetTransform(ObjectModel: PObjectModelRec; Transform: TModelTransform; Matrix: PMatrix4f);
  cdecl; external AfterwarpLib;

function ObjectModelSetTransform(ObjectModel: PObjectModelRec; Transform: TModelTransform; Matrix: PMatrix4f)
  : TLibraryBool; cdecl; external AfterwarpLib;

procedure ObjectModelGetPosition(ObjectModel: PObjectModelRec; Position: PVector3f); cdecl;
  external AfterwarpLib;

function ObjectModelGetDepthBias(ObjectModel: PObjectModelRec): Single; cdecl; external AfterwarpLib;

procedure ObjectModelSetDepthBias(ObjectModel: PObjectModelRec; DepthBias: Single); cdecl;
  external AfterwarpLib;

function ObjectModelGetAttributes(ObjectModel: PObjectModelRec): Cardinal; cdecl; external AfterwarpLib;

procedure ObjectModelSetAttributes(ObjectModel: PObjectModelRec; Attributes: Cardinal); cdecl;
  external AfterwarpLib;

function ObjectModelGetOrderIndex(ObjectModel: PObjectModelRec): Integer; cdecl; external AfterwarpLib;

procedure ObjectModelSetOrderIndex(ObjectModel: PObjectModelRec; OrderIndex: Integer); cdecl;
  external AfterwarpLib;

function ObjectModelGetLayers(ObjectModel: PObjectModelRec): UInt64; cdecl; external AfterwarpLib;

procedure ObjectModelSetLayers(ObjectModel: PObjectModelRec; Layers: UInt64); cdecl; external AfterwarpLib;

procedure ObjectModelGetSize(ObjectModel: PObjectModelRec; Size: PVector3f); cdecl; external AfterwarpLib;

procedure ObjectModelSetSize(ObjectModel: PObjectModelRec; Size: PVector3f); cdecl; external AfterwarpLib;

procedure ObjectModelGetAlignments(ObjectModel: PObjectModelRec; Aligns: PMeshAligns); cdecl;
  external AfterwarpLib;

procedure ObjectModelSetAlignments(ObjectModel: PObjectModelRec; Aligns: PMeshAligns); cdecl;
  external AfterwarpLib;

procedure ObjectModelInvalidate(ObjectModel: PObjectModelRec); cdecl; external AfterwarpLib;

function ObjectModelsCreate(SceneMeshes: PSceneMeshesRec): PObjectModelsRec; cdecl; external AfterwarpLib;

procedure ObjectModelsDestroy(ObjectModels: PObjectModelsRec); cdecl; external AfterwarpLib;

function ObjectModelsGetMeshes(ObjectModels: PObjectModelsRec): PSceneMeshesRec; cdecl; external AfterwarpLib;

procedure ObjectModelsGetView(ObjectModels: PObjectModelsRec; View: PMatrix4f); cdecl; external AfterwarpLib;

procedure ObjectModelsSetView(ObjectModels: PObjectModelsRec; View: PMatrix4f); cdecl; external AfterwarpLib;

function ObjectModelsGetObjectCount(ObjectModels: PObjectModelsRec): Integer; cdecl; external AfterwarpLib;

function ObjectModelsGetObject(ObjectModels: PObjectModelsRec; Index: Integer): PObjectModelRec; cdecl;
  external AfterwarpLib;

function ObjectModelsAdd(ObjectModels: PObjectModelsRec; Name: PAnsiChar; Payload: TObjectPayload)
  : PObjectModelRec; cdecl; external AfterwarpLib;

function ObjectModelsFind(ObjectModels: PObjectModelsRec; ObjectModel: PObjectModelRec): Integer; cdecl;
  external AfterwarpLib;

procedure ObjectModelsErase(ObjectModels: PObjectModelsRec; ObjectModel: PObjectModelRec); cdecl;
  external AfterwarpLib;

procedure ObjectModelsClear(ObjectModels: PObjectModelsRec); cdecl; external AfterwarpLib;

function ObjectModelsGetOrderCount(ObjectModels: PObjectModelsRec): Integer; cdecl; external AfterwarpLib;

function ObjectModelsGetOrder(ObjectModels: PObjectModelsRec; Index: Integer): PObjectModelRec; cdecl;
  external AfterwarpLib;

function ObjectModelsPayload(ObjectModels: PObjectModelsRec; Payload: TObjectPayload): PObjectModelRec; cdecl;
  external AfterwarpLib;

function ObjectModelsGetObjectByName(ObjectModels: PObjectModelsRec; Name: PAnsiChar): PObjectModelRec; cdecl;
  external AfterwarpLib;

function ObjectModelsUpdate(ObjectModels: PObjectModelsRec; Layers: UInt64): TLibraryBool; cdecl;
  external AfterwarpLib;

procedure ObjectModelsSort(ObjectModels: PObjectModelsRec; Compare: TObjectModelCompare); cdecl;
  external AfterwarpLib;

procedure ObjectModelsSortWith(ObjectModels: PObjectModelsRec; CompareFunc: TObjectModelCompareFunc; 
  User: Pointer); cdecl; external AfterwarpLib;

function ObjectModelsSelect(ObjectModels: PObjectModelsRec; Origin, Direction: PVector3f; Distance: PSingle; 
  TestsCount: PInteger): PObjectModelRec; cdecl; external AfterwarpLib;

function ObjectModelsMultiSelect(ObjectModels: PObjectModelsRec; Origin, Direction: PVector3f; 
  Distance: PSingle; TestsCount: PInteger): PObjectModelRec; cdecl; external AfterwarpLib;

function MeshVoxelCreateFromFile(FileName: PAnsiChar): PMeshVoxelRec; cdecl; external AfterwarpLib;

function MeshVoxelCreateFromMemory(Buffer: Pointer; Size: Cardinal): PMeshVoxelRec; cdecl;
  external AfterwarpLib;

procedure MeshVoxelDestroy(MeshVoxel: PMeshVoxelRec); cdecl; external AfterwarpLib;

procedure MeshVoxelExtents(MeshVoxel: PMeshVoxelRec; Position, Size: PVector3f); cdecl; external AfterwarpLib;

procedure MeshVoxelVisualize(MeshVoxel: PMeshVoxelRec; VisualizeFunc: TMeshVoxelVisualizeFunc; User: Pointer);
  cdecl; external AfterwarpLib;

function SceneMeshGetName(SceneMesh: PSceneMeshRec): PAnsiChar; cdecl; external AfterwarpLib;

function SceneMeshGetPayload(SceneMesh: PSceneMeshRec): TObjectPayload; cdecl; external AfterwarpLib;

function SceneMeshGetModel(SceneMesh: PSceneMeshRec): PMeshModelRec; cdecl; external AfterwarpLib;

function SceneMeshGetVoxel(SceneMesh: PSceneMeshRec): PMeshVoxelRec; cdecl; external AfterwarpLib;

function SceneMeshGetTags(SceneMesh: PSceneMeshRec): PMeshMetaTagsRec; cdecl; external AfterwarpLib;

procedure SceneMeshGetBounds(SceneMesh: PSceneMeshRec; MinBounds, MaxBounds: PVector3f); cdecl;
  external AfterwarpLib;

function SceneMeshGetScale(SceneMesh: PSceneMeshRec): Single; cdecl; external AfterwarpLib;

procedure SceneMeshGetSize(SceneMesh: PSceneMeshRec; Size: PVector3f); cdecl; external AfterwarpLib;

function SceneMeshesCreate(Device: PDeviceRec): PSceneMeshesRec; cdecl; external AfterwarpLib;

procedure SceneMeshesDestroy(SceneMeshes: PSceneMeshesRec); cdecl; external AfterwarpLib;

function SceneMeshesAdd(SceneMeshes: PSceneMeshesRec; Name: PAnsiChar; Model: PPMeshModelRec; 
  Voxel: PPMeshVoxelRec; Tags: PPMeshMetaTagsRec; Payload: TObjectPayload; MinBounds, MaxBounds: PVector3f; 
  Scale: Single): PSceneMeshRec; cdecl; external AfterwarpLib;

function SceneMeshesAddFromBinaryFile(SceneMeshes: PSceneMeshesRec; Name, FileName: PAnsiChar; 
  Payload: TObjectPayload; VertexElementEntries: PVertexElementEntry; VertexElementCount, Channel: Cardinal; 
  Scale: Single): PSceneMeshRec; cdecl; external AfterwarpLib;

function SceneMeshesGetDevice(SceneMeshes: PSceneMeshesRec): PDeviceRec; cdecl; external AfterwarpLib;

function SceneMeshesGetCount(SceneMeshes: PSceneMeshesRec): Integer; cdecl; external AfterwarpLib;

function SceneMeshesGetMesh(SceneMeshes: PSceneMeshesRec; Index: Integer): PSceneMeshRec; cdecl;
  external AfterwarpLib;

function SceneMeshesGetMeshByName(SceneMeshes: PSceneMeshesRec; Name: PAnsiChar): PSceneMeshRec; cdecl;
  external AfterwarpLib;

function SceneMeshesPayload(SceneMeshes: PSceneMeshesRec; Payload: TObjectPayload): PSceneMeshRec; cdecl;
  external AfterwarpLib;

function SceneMeshesFind(SceneMeshes: PSceneMeshesRec; SceneMesh: PSceneMeshRec): Integer; cdecl;
  external AfterwarpLib;

procedure SceneMeshesErase(SceneMeshes: PSceneMeshesRec; SceneMesh: PSceneMeshRec); cdecl;
  external AfterwarpLib;

procedure SceneMeshesClear(SceneMeshes: PSceneMeshesRec); cdecl; external AfterwarpLib;

function RandomContextCreate(Seed: Cardinal; ShuffleCount: Integer): PRandomContextRec; cdecl;
  external AfterwarpLib;

procedure RandomContextDestroy(Context: PRandomContextRec); cdecl; external AfterwarpLib;

function RandomContextCompare(Context1, Context2: PRandomContextRec): TLibraryBool; cdecl;
  external AfterwarpLib;

function RandomContextGenerate(Context: PRandomContextRec): Cardinal; cdecl; external AfterwarpLib;

function RandomContextGenerate64(Context: PRandomContextRec): UInt64; cdecl; external AfterwarpLib;

function RandomContextGenerateFloat(Context: PRandomContextRec): Single; cdecl; external AfterwarpLib;

function RandomContextGenerateRanged(Context: PRandomContextRec; Range: Integer): Integer; cdecl;
  external AfterwarpLib;

function RandomContextGenerateGaussStart(Context: PRandomContextRec): Single; cdecl; external AfterwarpLib;

function RandomContextGenerateGaussEnd(Context: PRandomContextRec): Single; cdecl; external AfterwarpLib;

function RandomContextGenerateGaussMiddle(Context: PRandomContextRec): Single; cdecl; external AfterwarpLib;

function RandomContextGenerateGaussOmni(Context: PRandomContextRec): Single; cdecl; external AfterwarpLib;

procedure RayCreate(Position, SurfaceSize: PPoint2f; ViewInverse, Projection: PMatrix4f; Origin, 
  Direction: PVector3f); cdecl; external AfterwarpLib;

function RayIntersectTriangle(Origin, Direction, Vertex1, Vertex2, Vertex3: PVector3f; 
  BackFacing: TLibraryBool; Intersection: PPoint2f; Distance: PSingle): TLibraryBool; cdecl;
  external AfterwarpLib;

function RayIntersectCubeVolume(Origin, Direction: PVector3f; World: PMatrix4f; Distance: PSingle)
  : TLibraryBool; cdecl; external AfterwarpLib;

function RayIntersectPlane(Origin, Direction, PlanePoint, PlaneNormal, Intersection: PVector3f; 
  Distance: PSingle): TLibraryBool; cdecl; external AfterwarpLib;

function PixelFormatValid(Format: TPixelFormat): TLibraryBool; cdecl; external AfterwarpLib;

function PixelFormatBits(Format: TPixelFormat): Cardinal; cdecl; external AfterwarpLib;

procedure PixelFormatConvert(Dest, Source: Pointer; DestFormat, SourceFormat: TPixelFormat); cdecl;
  external AfterwarpLib;

procedure PixelFormatConvertArray(Dest, Source: Pointer; DestFormat, SourceFormat: TPixelFormat; DestPitch, 
  SourcePitch: Cardinal; Width, Height: Integer); cdecl; external AfterwarpLib;

function MakeColor(Color: TIntColor; Alpha: Integer): TIntColor; cdecl; external AfterwarpLib;

function MakeColorF(Color: TIntColor; Alpha: Single): TIntColor; cdecl; external AfterwarpLib;

function MakeColorWithGray(Color: TIntColor; Gray, Alpha: Integer): TIntColor; cdecl; external AfterwarpLib;

function MakeColorWithGrayF(Color: TIntColor; Gray, Alpha: Single): TIntColor; cdecl; external AfterwarpLib;

function MakeColorRGB(Red, Green, Blue, Alpha: Integer): TIntColor; cdecl; external AfterwarpLib;

function MakeColorRGBF(Red, Green, Blue, Alpha: Single): TIntColor; cdecl; external AfterwarpLib;

function MakeColorGray(Gray, Alpha: Integer): TIntColor; cdecl; external AfterwarpLib;

function MakeColorGrayF(Gray, Alpha: Single): TIntColor; cdecl; external AfterwarpLib;

function MakeColorAlpha(Alpha: Integer): TIntColor; cdecl; external AfterwarpLib;

function MakeColorAlphaF(Alpha: Single): TIntColor; cdecl; external AfterwarpLib;

function GetColorAlpha(Color: TIntColor): Integer; cdecl; external AfterwarpLib;

function GetColorAlphaF(Color: TIntColor): Single; cdecl; external AfterwarpLib;

function PremultiplyAlpha(Color: TIntColor): TIntColor; cdecl; external AfterwarpLib;

function UnpremultiplyAlpha(Color: TIntColor): TIntColor; cdecl; external AfterwarpLib;

function DisplaceRB(Color: TIntColor): TIntColor; cdecl; external AfterwarpLib;

function InvertColor(Color: TIntColor): TIntColor; cdecl; external AfterwarpLib;

function AddColors(Color1, Color2: TIntColor): TIntColor; cdecl; external AfterwarpLib;

function SubtractColors(Color1, Color2: TIntColor): TIntColor; cdecl; external AfterwarpLib;

function MultiplyColors(Color1, Color2: TIntColor): TIntColor; cdecl; external AfterwarpLib;

function AverageColors(Color1, Color2: TIntColor): TIntColor; cdecl; external AfterwarpLib;

function AverageFourColors(Color1, Color2, Color3, Color4: TIntColor): TIntColor; cdecl;
  external AfterwarpLib;

function AverageSixColors(Color1, Color2, Color3, Color4, Color5, Color6: TIntColor): TIntColor; cdecl;
  external AfterwarpLib;

function BlendColors(Color1, Color2: TIntColor; Alpha: Integer): TIntColor; cdecl; external AfterwarpLib;

function BlendFourColors(TopLeft, TopRight, BottomRight, BottomLeft: TIntColor; AlphaX, AlphaY: Integer; 
  Horizontal: TLibraryBool): TIntColor; cdecl; external AfterwarpLib;

function LerpColors(Color1, Color2: TIntColor; Alpha: Single): TIntColor; cdecl; external AfterwarpLib;

function LerpFourColors(TopLeft, TopRight, BottomRight, BottomLeft: TIntColor; AlphaX, AlphaY: Single; 
  Horizontal: TLibraryBool): TIntColor; cdecl; external AfterwarpLib;

function ComposeColors(Source, Dest: TIntColor): TIntColor; cdecl; external AfterwarpLib;

function ColorToGray(Color: TIntColor): Integer; cdecl; external AfterwarpLib;

function ColorToGray16(Color: TIntColor): Integer; cdecl; external AfterwarpLib;

function ColorToGrayF(Color: TIntColor): Single; cdecl; external AfterwarpLib;

function Lerp(Value1, Value2, Theta: Single): Single; cdecl; external AfterwarpLib;

function Cubic(PredValue1, Value1, Value2, SuccValue2, Theta: Single): Single; cdecl; external AfterwarpLib;

function CatmullRom(PredValue1, Value1, Value2, SuccValue2, Theta: Single): Single; cdecl;
  external AfterwarpLib;

function Hermite(PredValue1, Value1, Value2, SuccValue2, Theta, Tension, Bias: Single): Single; cdecl;
  external AfterwarpLib;

function SmoothStep(Value1, Value2, Theta: Single): Single; cdecl; external AfterwarpLib;

function SmootherStep(Value1, Value2, Theta: Single): Single; cdecl; external AfterwarpLib;

function SineTransform(Value: Single): Single; cdecl; external AfterwarpLib;

function SineAccelerate(Value: Single): Single; cdecl; external AfterwarpLib;

function SineDecelerate(Value: Single): Single; cdecl; external AfterwarpLib;

function SineCycle(Value: Single): Single; cdecl; external AfterwarpLib;

function SineTwoCycle(Value: Single): Single; cdecl; external AfterwarpLib;

function BiasTransform(Value, Bias: Single): Single; cdecl; external AfterwarpLib;

function GainTransform(Value, Gain: Single): Single; cdecl; external AfterwarpLib;

procedure MeshBoundsToMatrixModel(MinBounds, MaxBounds: PVector3f; Model: PMatrix4f); cdecl;
  external AfterwarpLib;

procedure MeshBoundsToMatrixVolume(MinBounds, MaxBounds: PVector3f; Volume: PMatrix4f; SizeBias: Single);
  cdecl; external AfterwarpLib;

procedure MeshBoundsTagOffset(MeshMinBounds, MeshMaxBounds, TagMinBounds, TagMaxBounds, Offset: PVector3f);
  cdecl; external AfterwarpLib;

procedure MeshBoundsToMatrixVolumeTag(MeshMinBounds, MeshMaxBounds, TagMinBounds, TagMaxBounds: PVector3f; 
  Volume: PMatrix4f; SizeBias: Single); cdecl; external AfterwarpLib;

procedure VolumeCalculateNearFarPlanes(WorldView: PMatrix4f; NearPlane, FarPlane: PSingle); cdecl;
  external AfterwarpLib;

procedure VolumeCalculateVisibleFrame(WorldViewProjection: PMatrix4f; SurfaceSize: PPoint2f; 
  VisibleFrame: PFloatRect); cdecl; external AfterwarpLib;

function VertexElementsEstimatePitch(Channel: Cardinal; VertexElements: PVertexElementEntry; 
  VertexElementCount: Cardinal): Cardinal; cdecl; external AfterwarpLib;

function GetSystemTicks: UInt64; cdecl; external AfterwarpLib;

implementation

end.
