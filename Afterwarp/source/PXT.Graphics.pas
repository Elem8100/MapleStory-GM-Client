unit PXT.Graphics;
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
{< High-level wrappers around Afterwarp classes dealing with graphics, timing and math. }
interface

{$INCLUDE PXT.Config.inc}

uses
  SysUtils, PXT.Types, PXT.Headers;

type
  // Pointer to @link(TRay).
  PRay = ^TRay;

  // Ray in 3D space.
  TRay = record
    // Ray starting location.
    Origin: TVector3f;

    // Direction of the ray.
    Direction: TVector3f;

    { Calculates whether the current ray intersects with the given triangle and if so, calculates the
      resulting barycentric coordinates and distance from origin to triangle. }
    function IntersectTriangle(const AVertex1, AVertex2, AVertex3: TVector3f; const ABackFacing: Boolean;
      out AIntersection: TPoint2f; out ADistance: Single): Boolean; overload; inline;

    // Calculates whether the current ray intersects with the given front-facing triangle.
    function IntersectTriangle(const AVertex1, AVertex2, AVertex3: TVector3f;
      const ABackFacing: Boolean = False): Boolean; overload; inline;

    { Tests whether the current ray intersects with one of triangles of minimalistic cube volume, whose
      vertices are first transformed by the given world matrix, returning distance to intersection point. }
    function IntersectCubeVolume(const AWorld: TMatrix4f; out ADistance: Single): Boolean; overload; inline;

    { Tests whether the current ray intersects with one of triangles of minimalistic cube volume, whose
      vertices are first transformed by the given world matrix. }
    function IntersectCubeVolume(const AWorld: TMatrix4f): Boolean; overload; inline;

    { Tests whether the current ray intersects with a plane specified by a point on the plane and its surface
      normal, returning the actual point of intersection and distance from ray's origin. }
    function IntersectPlane(const APlanePoint, APlaneNormal: TVector3f; out AIntersection: TVector3f;
      out ADistance: Single): Boolean; overload;

    { Tests whether the current ray intersects with a plane specified by a point on the plane and its surface
      normal, returning the actual point of intersection. }
    function IntersectPlane(const APlanePoint, APlaneNormal: TVector3f;
      out AIntersection: TVector3f): Boolean; overload;

    { Tests whether the current ray intersects with a plane specified by a point on the plane and its surface
      normal. }
    function IntersectPlane(const APlanePoint, APlaneNormal: TVector3f): Boolean; overload;

    { Constructs the ray based on 2D surface position, surface size, inverse of the 3D view matrix and the
      projection matrix. }
    class function Create(const APosition, ASurfaceSize: TPoint2f; const AViewInverse,
      AProjection: TMatrix4f): TRay; static;
  end;

  // Pointer to @link(TVolume).
  PVolume = ^TVolume;

  // Volume and boundary functions.
  TVolume = record
    // Creates a model transformation matrix that centers the mesh and places it on top of zero plane.
    class function BoundsToMatrixModel(const AMinBounds, AMaxBounds: TVector3f): TMatrix4f; static; inline;

    /// Creates a volume transformation matrix that centers the mesh and places it on top of zero plane.
    class function BoundsToMatrixVolume(const AMinBounds, AMaxBounds: TVector3f;
      const ASizeBias: Single = 0.1): TMatrix4f; static; inline;

    /// Calculates an offset to displace from mesh to tag origin.
    class function BoundsTagOffset(const AMeshMinBounds, AMeshMaxBounds, ATagMinBounds,
      ATagMaxBounds: TVector3f): TVector3f; static; inline;

    /// Creates a volume transformation matrix that centers the mesh tag inside mesh volume.
    class function BoundsToMatrixVolumeTag(const AMeshMinBounds, AMeshMaxBounds, ATagMinBounds,
      ATagMaxBounds: TVector3f; const ASizeBias: Single = 0.1): TMatrix4f; static; inline;

    // Calculates near and far planes for a bounding box transformed by the given matrix.
    class procedure NearFarPlanes(const AWorldView: TMatrix4f; out ANearPlane,
      AFarPlane: Single); static; inline;

    { Calculates visible surface frame for a bounding box transformed by the given
      world/view/projection matrix. }
    class function VisibleFrame(const AWorldViewProjection: TMatrix4f;
      const ASurfaceSize: TPoint2f): TFloatRect; static; inline;
  end;

  // Pointer to @link(TBaseObject).
  PBaseObject = ^TBaseObject;

  // Base object wrapping existing framework handle.
  TBaseObject = object
  private
    FHandle: TLibraryClassHandle;
    function GetInitialized: Boolean; inline;
  public
    // Wrapped object handle.
    property Handle: TLibraryClassHandle read FHandle;

    // Indicates whether the object has been properly initialized.
    property Initialized: Boolean read GetInitialized;
  end;

  // Base class wrapping existing framework handle.
  TBaseClass = class
  private
    FHandle: TLibraryClassHandle;
    function GetInitialized: Boolean; inline;
  public
    // Wrapped object handle.
    property Handle: TLibraryClassHandle read FHandle;

    // Indicates whether the object has been properly initialized.
    property Initialized: Boolean read GetInitialized;
  end;

const
  // Null object.
  NullObject: TBaseObject = (FHandle: nil);

type
  // Program vertex buffer declaration.
  TVertexElements = array of TVertexElement;

  // Program element declaration.
  TProgramElements = array of TProgramElement;

  // Program variable declaration.
  TProgramVariables = array of TProgramVariable;

  // Array of 4x4 matrices.
  TMatrix4fArray = array of TMatrix4f;

  // Array of floating-point colors.
  TFloatColorArray = array of TFloatColor;

  // Pointer to @link(TDevice).
  PDevice = ^TDevice;

  // Wrapper around graphics device.
  TDevice = object(TBaseObject)
  private
    function GetConfiguration: TDeviceConfiguration; inline;
    function GetCapabilities: TDeviceCapabilities; inline;
    function GetRenderingState: TRenderingState; inline;
    procedure SetRenderingState(const ARenderingState: TRenderingState); inline;
    function GetViewport: TIntRect; inline;
    procedure SetViewport(const AViewport: TIntRect); inline;
    function GetScissor: TIntRect; inline;
    procedure SetScissor(const AScissor: TIntRect); inline;
  public
    // Releases the device.
    procedure Free; inline;

    // Resizes display to the new given size.
    function Resize(const ASize: TPoint2i): Boolean; inline;

    // Starts device rendering phase and prepares destination surface (if such applies).
    function BeginScene: Boolean; inline;

    // Finishes device rendering phase, typically swapping back and front buffers.
    procedure EndScene; inline;

    // Releases any cached device resources.
    procedure ResetCache; inline;

    // Clears rendering surface that is currently active.
    function Clear(const ALayers: TClearLayers; const AColor: TFloatColor; const ADepth: Single = 1.0;
      const AStencil: Cardinal = 0): Boolean; inline;

    // Current device configuration.
    property Configuration: TDeviceConfiguration read GetConfiguration;

    // Current device capabilities.
    property Capabilities: TDeviceCapabilities read GetCapabilities;

    // Current device rendering parameters and operation characteristics.
    property RenderingState: TRenderingState read GetRenderingState write SetRenderingState;

    // Current device rendering viewport.
    property Viewport: TIntRect read GetViewport write SetViewport;

    // Current device rendering scissor rectangle.
    property Scissor: TIntRect read GetScissor write SetScissor;
  end;

// Creates new instance of graphics device.
function DeviceInit(const ADeviceBackend: TDeviceBackend; const AWindowHandle: TUntypedHandle;
  const ASize: TPoint2i; const APixelFormat: TPixelFormat; const ADepthStencil: TPixelFormat;
  const AMultisamples: Integer; const AAttributes: Cardinal): TDevice;

// Creates a shared instance of graphics device.
function DeviceInitShared(const ADevice: TDevice; const AWindowHandle: TUntypedHandle; const ASize: TPoint2i;
  const APixelFormat: TPixelFormat; const ADepthStencil: TPixelFormat; const AMultisamples: Integer;
  const AAttributes: Cardinal): TDevice;

type
  // Pointer to @link(TBuffer).
  PBuffer = ^TBuffer;

  // Graphics buffer that is typically stored on graphics hardware.
  TBuffer = object(TBaseObject)
  private
    function GetParameters: TBufferParameters; inline;
  public
    // Releases the buffer.
    procedure Free; inline;

    { Updates the contents of the buffer with a new data. If @italic(ASize) is left at zero, the remaining
      portion of buffer starting at given byte offset will be updated. Note that if buffer has been created
      with an access type that is non-default, then typically a whole buffer should be updated, attempting to
      update a portion of it may be implementation-dependent. }
    function Update(const AData: Pointer; const AOffset: Cardinal = 0; const ASize: Cardinal = 0): Boolean;

    // Current buffer parameters.
    property Parameters: TBufferParameters read GetParameters;
  end;

// Creates new instance of graphics buffer.
function BufferInit(const ADevice: TDevice; const ADataType: TBufferDataType;
  const AAccessType: TBufferAccessType; const ASize: Cardinal;
  const APitch: Cardinal; const AInitialData: Pointer = nil): TBuffer; inline;

type
  // Pointer to @link(TProgram).
  PProgram = ^TProgram;

  // Shader program that is typically executed on graphics hardware.
  TProgram = object(TBaseObject)
  public
    // Releases the program.
    procedure Free; inline;

    { Updates a portion of a specific variable that corresponds to the given index. The provided size
      indicates how many bytes should be updated. If the provided size is left at zero, the entire contents
      of given variable will be updated. This function should only be used inside BeginScene / EndScene call
      block. }
    function UpdateByIndex(const AVariableIndex: Integer; const AVariableData: Pointer;
      const ASize: Cardinal = 0): Boolean; inline;

    { Updates a portion of a specific variable identified by its name (case-sensitive). The provided size
      indicates how many bytes should be updated. If the provided size is left at zero, the entire contents
      of given variable will be updated. This function should only be used inside BeginScene / EndScene call
      block. }
    function UpdateByName(const AVariableName: Utf8String; const AVariableData: Pointer;
      const ASize: Cardinal = 0): Boolean; inline;

    { Assigns a generic buffer (or a portion of starting from the given offset in bytes) to a particular
      channel. Vertex and index buffers are associated with input stream channels, whereas constant buffers
      are associated with shader program channels - each of these being unrelated and dependent on shader
      vertex and program element declarations. The current bindings can be considered valid until
      @link(EndScene), @link(ResetBindings) or @link(ResetCache) is called. }
    function Bind(const ABuffer: TBuffer; const AChannel: Cardinal = 0;
      const AOffset: Cardinal = 0): Boolean; inline;

    // Removes association that was previously made with @link(Bind) between the buffer and the given channel.
    procedure Unbind(const ABuffer: TBuffer; const AChannel: Cardinal = 0); inline;

    // Resets any bindings that were previously made as if @link(Unbind) would be called for each of them.
    procedure ResetBindings; inline;

    { Resets any cache associated with vertex buffers. In case of OpenGL, this resets internal Vertex Array
      Object (VAO) cache. }
    procedure ResetCache; inline;

    { Purges vertex buffer cache for any entries associated with the given buffer handle. This is typically
      required if the buffer that was bound at least once, has been released, to avoid danging cached buffer
      objects. }
    procedure PurgeCache(const ABuffer: TBuffer); inline;

    { Applies any binding changes to constant and/or vertex buffers, which usually occur when a draw call is
      issued, to occur immediately. }
    function Commit: Boolean; inline;

    // Activates the shader program and prepares for rendering.
    function BeginScene: Boolean; inline;

    // Deactivates the shader program, resets previously active input streams and bindings.
    procedure EndScene; inline;

    // Renders primitives with the given number of vertices.
    function Draw(const ATopology: TPrimitiveTopology; const AVertexCount: Cardinal;
      const ABaseVertex: Integer = 0): Boolean; inline;

    // Renders indexed primitives with the given number of indices.
    function DrawIndexed(const ATopology: TPrimitiveTopology; const AIndexCount: Cardinal;
      const AStartIndex: Cardinal = 0; const ABaseVertex: Integer = 0): Boolean; inline;

    // Renders multiple number of instances of the given primitives.
    function DrawInstances(const ATopology: TPrimitiveTopology; const AVertexCount,
      AInstanceCount: Cardinal; const ABaseVertex: Integer = 0): Boolean; inline;

    // Renders multiple number of instances of the given indexed primitives.
    function DrawInstancesIndexed(const ATopology: TPrimitiveTopology; const AIndexCount,
      AInstanceCount: Cardinal; const AStartIndex: Cardinal = 0;
      const ABaseVertex: Integer = 0): Boolean; inline;
  end;

// Creates new instance of graphics program from its parameters.
function ProgramInit(const ADevice: TDevice;
  const AParameters: TProgramParameters): TProgram; overload; inline;

// Creates new instance of graphics program by specifying each of its parameters.
function ProgramInit(const ADevice: TDevice; const AVertexElements: TVertexElements;
  const AProgramElements: TProgramElements; const AProgramVariables: TProgramVariables;
  const AVertexShader, AGeometryShader, AFragmentShader: TBytes): TProgram; overload;

// Creates new instance of graphics program by specifying each of its parameters.
function ProgramInit(const ADevice: TDevice; const AVertexElements: array of TVertexElement;
  const AProgramElements: array of TProgramElement; const AProgramVariables: array of TProgramVariable;
  const AVertexShader, AGeometryShader, AFragmentShader: TBytes): TProgram; overload;

// Creates new instance of graphics program from files by specifying each of its parameters.
function ProgramInitFromFiles(const ADevice: TDevice; const AVertexElements: TVertexElements;
  const AProgramElements: TProgramElements; const AProgramVariables: TProgramVariables;
  const AVertexShader, AGeometryShader, AFragmentShader: string): TProgram; overload;

// Creates new instance of graphics program from files by specifying each of its parameters.
function ProgramInitFromFiles(const ADevice: TDevice;
  const AVertexElements: array of TVertexElement; const AProgramElements: array of TProgramElement;
  const AProgramVariables: array of TProgramVariable; const AVertexShader, AGeometryShader,
  AFragmentShader: string): TProgram; overload;

type
  // Pointer to @link(TTexture).
  PTexture = ^TTexture;

  // Compute shader program for general processing on GPU (GPGPU).
  TComputeProgram = object(TBaseObject)
  public
    // Releases the program.
    procedure Free; inline;

    { Assigns a generic buffer (or a portion of starting from the given offset in bytes) to a particular
      channel. The current bindings can be considered valid until @link(EndScene) or @link(ResetBindings) is
      called. }
    function Bind(const ABuffer: TBuffer; const AChannel: Cardinal = 0;
      const AOffset: Cardinal = 0): Boolean; overload; inline;

    // Removes association that was previously made with @link(Bind) between the buffer and the given channel.
    procedure Unbind(const ABuffer: TBuffer; const AChannel: Cardinal = 0); overload; inline;

    // Assigns texture to a particular channel.
    function Bind(const ATexture: PTexture;
      const ABindFormat: TComputeBindTextureFormat): Boolean; overload; inline;

    // Removes existing texture association with the particular channel.
    procedure Unbind(const ATexture: PTexture;
      const ABindFormat: TComputeBindTextureFormat); overload; inline;

    // Resets any bindings that were previously made as if @link(Unbind) would be called for each of them.
    procedure ResetBindings; inline;

    { Applies any binding changes to constant buffers, which usually occur when a dispatch call is issued,
      to occur immediately. }
    function Commit: Boolean; inline;

    // Activates the compute shader program.
    function BeginScene: Boolean; inline;

    /// Deactivates the compute shader program.
    procedure EndScene; inline;

    /// Launches one or more compute shader work groups.
    function Dispatch(const AGroupsX, AGroupsY, AGroupsZ: Cardinal): Boolean; inline;
  end;

  // Pointer to @link(TMeshModel).
  PMeshModel = ^TMeshModel;

  // 3D mesh model that has vertex and optionally index buffers for rendering.
  TMeshModel = object(TBaseObject)
  private
    function GetInformation: TMeshModelInformation; inline;
    function GetVertexBuffer: TBuffer; inline;
    function GetIndexBuffer: TBuffer; inline;
  public
    // Releases the 3D mesh model.
    procedure Free; inline;

    // Renders the mesh model with the given program.
    function Draw(const AProgram: TProgram;
      const ATopology: TPrimitiveTopology =
      {$IFDEF DELPHI_LEGACY}ptTriangles{$ELSE}TPrimitiveTopology.Triangles{$ENDIF};
      const AElementCount: Cardinal = 0; const AStartIndex: Cardinal = 0; const ABaseVertex: Integer = 0;
      const APostUnbind: Boolean = False): Boolean; inline;

    // Renders multiple instances of the mesh model with a given program.
    function DrawInstances(const AProgram: TProgram; const AInstanceCount: Cardinal;
      const ATopology: TPrimitiveTopology =
      {$IFDEF DELPHI_LEGACY}ptTriangles{$ELSE}TPrimitiveTopology.Triangles{$ENDIF};
      const AElementCount: Cardinal = 0; const AStartIndex: Cardinal = 0; const ABaseVertex: Integer = 0;
      const APostUnbind: Boolean = False): Boolean; inline;

    { Purges program cache from model's buffers. This is typically needed after the model has been rendered
      at least once and is being destroyed.}
    procedure PurgeProgramCache(const AProgram: TProgram); inline;

    // 3D mesh model important characteristics.
    property Information: TMeshModelInformation read GetInformation;

    // Integrated vertex buffer.
    property VertexBuffer: TBuffer read GetVertexBuffer;

    // Integrated index buffer.
    property IndexBuffer: TBuffer read GetIndexBuffer;
  end;

  // Pointer to @link(TMeshMetaTag).
  PMeshMetaTag = ^TMeshMetaTag;

  // Meta-tag, which describes a certain important axis-aligned bounding area inside a mesh.
  TMeshMetaTag = object(TBaseObject)
  private
    function GetOwner: TBaseObject; inline;
    function GetName: Utf8String;
    function GetMinBounds: TVector3f; inline;
    function GetMaxBounds: TVector3f; inline;
    function GetFirstVertex: Integer; inline;
    function GetVertexCount: Integer; inline;
    function GetFirstIndex: Integer; inline;
    function GetIndexCount: Integer; inline;
  public
    // Mesh meta tag's owner container (must be typecast to TMeshMetaTags).
    property Owner: TBaseObject read GetOwner;

    // Unique name of te tag (case-sensitive).
    property Name: Utf8String read GetName;

    // Minimal tag boundaries in a 3D mesh.
    property MinBounds: TVector3f read GetMinBounds;

    // Maximum tag boundaries in a 3D mesh.
    property MaxBounds: TVector3f read GetMaxBounds;

    // Returns first vertex in the sub-mesh.
    property FirstVertex: Integer read GetFirstVertex;

    // Returns number of vertices in the sub-mesh.
    property VertexCount: Integer read GetVertexCount;

    // Returns first index in the sub-mesh.
    property FirstIndex: Integer read GetFirstIndex;

    // Returns number of indices in the sub-mesh.
    property IndexCount: Integer read GetIndexCount;
  end;

  // Pointer to @link(TMeshMetaTags).
  PMeshMetaTags = ^TMeshMetaTags;

  // Enumerator for 3D mesh meta-tag container.
  TMeshMetaTagsEnumerator = record
  private
    FHandle: TLibraryClassHandle;
    FCurrent: Integer;

    function GetCurrent: TMeshMetaTag; inline;
  public
    // Creates new 3D mesh meta-tag container enumerator.
    constructor Create(const AHandle: TLibraryClassHandle);

    // Moves enumerator to the next element.
    function MoveNext: Boolean; inline;

    // Current enumerator's element.
    property Current: TMeshMetaTag read GetCurrent;
  end;

  // List of meta-tags that describe important axis-aligned bounding areas inside a mesh.
  TMeshMetaTags = object(TBaseObject)
  private
    function GetCount: Integer; inline;
    function GetTags(const AIndex: Integer): TMeshMetaTag; inline;
    function GetTag(const AName: Utf8String): TMeshMetaTag; inline;
  public
    // Releases the given mesh meta tag container instance.
    procedure Free; inline;

    // Creates a new tag with the given parameters. The name of tag must be unique, case-sensitive.
    function Spawn(const AName: Utf8String; const AMinBounds, AMaxBounds: TVector3f; const AStartVertex,
      AVertexCount, AStartIndex, AIndexCount: Integer): TMeshMetaTag; overload; inline;

    // Removes an existing list from container list.
    procedure Erase(const AIndex: Integer); inline;

    // Removes all existing tags from container list.
    procedure Clear; inline;

    // Returns enumerator for container list.
    function GetEnumerator: TMeshMetaTagsEnumerator; inline;

    // Number of existing tags.
    property Count: Integer read GetCount;

    // Returns a particular tag with the given index.
    property Tags[const AIndex: Integer]: TMeshMetaTag read GetTags;

    // Returns a particular tag with the given name (case-sensitive).
    property Tag[const AName: Utf8String]: TMeshMetaTag read GetTag;
  end;

  // Pointer to @link(TMeshBuffer).
  PMeshBuffer = ^TMeshBuffer;

  // Buffer that contains 3D mesh information.
  TMeshBuffer = object(TBaseObject)
  private
    function GetInformation: TMeshBufferInformation; inline;
    procedure SetInformation(AInformation: TMeshBufferInformation);
    function GetTransform: TMatrix4f; inline;
    procedure SetTransform(const ATransform: TMatrix4f); inline;
  public
    // Releases the 3D mesh buffer.
    procedure Free; inline;

    // Creates new instance of 3D mesh model with the given vertex elements and actual data from this buffer.
    function Model(const ADevice: TDevice; const AVertexElements: TVertexElements;
      const AStartVertex: Integer = 0; const AVertexCount: Integer = 0; const AStartIndex: Integer = 0;
      const AIndexCount: Integer = 0; const AChannel: Cardinal = 0): TMeshModel; overload;

    // Creates new instance of 3D mesh model with the given vertex elements and actual data from this buffer.
    function Model(const ADevice: TDevice; const AVertexElements: array of TVertexElement;
      const AStartVertex: Integer = 0; const AVertexCount: Integer = 0; const AStartIndex: Integer = 0;
      const AIndexCount: Integer = 0; const AChannel: Cardinal = 0): TMeshModel; overload;

    { Creates a 3D superellipse with the given parameters. Longitude's initial and ending angles are
      specified in [0, 2 * PI] ranges, whereas latitude's initial and ending angles are specified in a
      different range of [-PI / 2, PI / 2]. }
    procedure SuperEllipse(const ALongitudeSections, ALatitudeSections: Cardinal; const AOrigin,
      ARadius: TVector3f; const AInitLongitudeAngle, AEndLongitudeAngle, ALongitudeShape, AInitLatitudeAngle,
      AEndLatitudeAngle, ALatitudeShape: Single; const AInitTexCoord, AEndTexCoord: TPoint2f;
      const AColor: TFloatColor; const AIndicesClockwise: Boolean = True); inline;

    { Creates a cylinder with the given parameters. Axis vectors define both orientation and size.
      Initial and ending angles are specified in [0, 2 * PI] range. }
    procedure Cylinder(const ARadialSections, AHeightSections: Cardinal; const AOrigin, AHorizAxis,
      AVertAxis, AHeightAxis: TVector3f; const AInitAngle, AEndAngle, AShape: Single; const AInitTexCoord,
      AEndTexCoord: TPoint2f; const ABendAngle, ALateralAngle: Single; const AColor: TFloatColor;
      const AIndicesClockwise: Boolean = True); inline;

    // Creates a simplistic cone with the given parameters in XZ plane with its base at origin.
    procedure Cone(const ASections: Cardinal; const AOrigin: TVector3f; const ARadius, AHeight: Single;
      const AColor: TFloatColor; const AIndicesClockwise: Boolean = True); inline;

    { Creates a disc with the given parameters. Axis vectors define both orientation and size. Initial and
      ending angles are specified in [0, 2 * PI] range. @italic(AInitRadius) defines initial radius in
      proportion to overall disc size. }
    procedure Disc(const ARadialSections, AInnerSections: Cardinal; const AOrigin, AHorizAxis, AVertAxis,
      ANormal: TVector3f; const AInitAngle, AEndAngle, AShape, AInitRadius: Single; const AInitTexCoord,
      AEndTexCoord: TPoint2f; const ALateralAngle: Single; const AColor: TFloatColor;
      const AIndicesClockwise: Boolean = False); inline;

    // Creates a 3D plane with the given parameters. Axis vectors define both orientation and size.
    procedure Plane(const AHorizSections, AVertSections: Cardinal; const AOrigin, AHorizAxis, AVertAxis,
      ANormal: TVector3f; const AInitTexCoord, AEndTexCoord: TPoint2f; const AColor: TFloatColor;
      const AIndicesClockwise: Boolean = True); inline;

    // Creates a 3D cube with the given parameters. Axis vectors define both orientation and size.
    procedure Cube(const AHorizSections, AVertSections, ADepthSections: Cardinal; const AOrigin, AHorizAxis,
      AVertAxis, ADepthAxis: TVector3f; const AInitTexCoord, AEndTexCoord: TPoint2f;
      const AColor: TFloatColor; const AIndicesClockwise: Boolean = False); inline;

    { Creates a 3D cube with minimal number of vertices and indices. Texture coordinates, normals and
      tangents are not generated.}
    procedure CubeMinimal(const AOrigin, ASize: TVector3f; const AColor: TFloatColor;
      const AIndicesClockwise: Boolean = False); inline;

    // Creates a 3D cube with round corners.
    procedure CubeRound(const AOrigin, ASize: TVector3f; const ARoundness: Single; const AColor: TFloatColor;
      const AIndicesClockwise: Boolean = False); inline;

    { Creates a 3D torus with the given parameters. Axis vectors define both orientation and size.
      Initial and ending angles for both inner and outer rings are specified in [0, 2 * PI] range. }
    procedure Torus(const AOuterSections, AInnerSections: Cardinal; const AOrigin, AHorizAxis,
      AVertAxis: TVector3f; const AInitOuterAngle, AEndOuterAngle, AOuterShape, AInitInnerAngle,
      AEndInnerAngle, AInnerShape: Single; const AInnerRadius, AInitTexCoord, AEndTexCoord: TPoint2f;
      const ALateralAngle: Single; const AColor: TFloatColor;
      const AIndicesClockwise: Boolean = True); inline;

    { Creates a 3D p-q torus knot with the given parameters. Initial and ending angles for both inner and
      outer rings are specified in [0, 2 * PI] range. }
    procedure TorusKnot(const AOuterSections, AInnerSections: Cardinal; const AOrigin: TVector3f;
      const AP, AQ: Integer; const AInitOuterAngle, AEndOuterAngle, AOuterRadius, AInitInnerAngle,
      AEndInnerAngle, AInnerShape: Single; const AInnerRadius, AInitTexCoord, AEndTexCoord: TPoint2f;
      const ALateralAngle: Single; const AColor: TFloatColor;
      const AIndicesClockwise: Boolean = True); inline;

    { Creates a 3D supertoroid located in XY plane with the given parameters. Initial and ending angles for
      both inner and outer rings are specified in [0, 2 * PI] range. }
    procedure Supertoroid(const AOuterSections, AInnerSections: Cardinal; const AOrigin: TVector3f;
      const AInitOuterAngle, AEndOuterAngle, AOuterRadius, AOuterShape, AInitInnerAngle, AEndInnerAngle,
      AInnerRadius, AInnerShape: Single; const AInitTexCoord, AEndTexCoord: TPoint2f;
      const AColor: TFloatColor; const AIndicesClockwise: Boolean = True); inline;

    // Applies current transformation to the vertices.
    procedure TransformVertices(const AFirstVertex: Cardinal = 0; const AVertexCount: Cardinal = 0); inline;

    { Recalculates mesh normals for the given range of indices by using normals of triangles.
      This rebuilds vertex and index lists entirely. }
    function CalculateFlatNormals(const AEpsilon: Single = VectorEpsilon): Boolean; inline;

    { Recalculates mesh normals for the given range of indices by calculating face normals first and then
      averaging the resulting values to face vertices. }
    procedure CalculateNormals(const AFirstVertex: Cardinal = 0; const AVertexCount: Cardinal = 0;
      const AFirstIndex: Cardinal = 0; const AIndexCount: Cardinal = 0); inline;

    { Recalculates mesh normals for the given range of indices. Vertices that have their positions almost
      the same (depends on @italic(AWeldEpsilon)) will be considered duplicated and thus will receive weighted
      normals from neighbor faces as if it would be the same shared vertex. }
    procedure CalculateNormalsWeld(const AFirstVertex: Cardinal = 0; const AVertexCount: Cardinal = 0;
      const AFirstIndex: Cardinal = 0; const AIndexCount: Cardinal = 0;
      const AWeldEpsilon: Single = VectorEpsilon); inline;

    { Invert the normals for vertices in the given range. If vertex count is not specified, then the
      remaining vertices starting at the given vertex and up to the end of the list are processed. }
    procedure InvertNormals(const AFirstVertex: Cardinal = 0; const AVertexCount: Cardinal = 0); inline;

    // Inverts order of indices (e.g. from clockwise to counter-clockwise and vice-versa).
    procedure InvertIndexOrder(const AFirstIndex: Cardinal = 0; const AIndexCount: Cardinal = 0); inline;

    // Calculates minimum and maximum vertex coordinate boundaries in the given range of mesh.
    procedure CalculateBounds(var AVertexMin, AVertexMax: TVector3f; const AFirstVertex: Cardinal = 0;
      const AVertexCount: Cardinal = 0); inline;

    // Calculates coordinate boundaries and centralizes the vertices in the given range of mesh.
    procedure Centralize(const AFirstVertex: Cardinal = 0; const AVertexCount: Cardinal = 0); inline;

    { Processes given range of indices, marking vertices that are used and then eliminating vertices that
      were left unused, updating the entire array of indices. }
    procedure EliminateUnusedVertices(const AFirstVertex: Cardinal = 0;
      const AVertexCount: Cardinal = 0); inline;

    { Joins vertices that have duplicate positions, averaging their normals, tangents and colors.
      This has complexity of O(n^2), where n is number of vertices to process. }
    function JoinDuplicateVertices(const AFirstVertex: Cardinal = 0;
      const AVertexCount: Cardinal = 0; const ATreshold: Single = VectorEpsilon): Boolean; inline;

    /// Includes vertices and indices from source mesh buffer into the current one.
    function Combine(const AMeshBufferSource: TBaseObject; const AFirstVertex: Cardinal = 0;
      const AVertexCount: Cardinal = 0; const AFirstIndex: Cardinal = 0;
      const AIndexCount: Cardinal = 0): Boolean; inline;

    // Clears all arrays.
    procedure Clear; inline;

    { Transfers mesh vertex elements to an interleaved system buffer with the given format at the specified
      channel. Returns number of bytes that the resulting data occupies. If @italic(ABuffer) is @nil, this
      function only calculates how many bytes the data will occupy. }
    procedure TransferVertices(const ABuffer: Pointer; const AVertexElements: TVertexElements;
      const AStartVertex, AVertexCount: Integer; const AChannel: Cardinal = 0;
      const ASemanticIndex: Integer = 0); overload;

    { Transfers mesh vertex elements to an interleaved system buffer with the given format at the specified
      channel. Returns number of bytes that the resulting data occupies. If @italic(ABuffer) is @nil, this
      function only calculates how many bytes the data will occupy. }
    procedure TransferVertices(const ABuffer: Pointer; const AVertexElements: array of TVertexElement;
      const AStartVertex, AVertexCount: Integer; const AChannel: Cardinal = 0;
      const ASemanticIndex: Integer = 0); overload;

    { Transfers mesh vertex elements to an interleaved vertex buffer object with the given format at the
      specified channel and offset. }
    procedure TransferVertices(const ABuffer: TBuffer; const AVertexElements: TVertexElements;
      const AStartVertex, AVertexCount: Integer; const AChannel: Cardinal = 0; const AOffset: Cardinal = 0;
      const ASemanticIndex: Integer = 0); overload;

    { Transfers mesh vertex elements to an interleaved vertex buffer object with the given format at the
      specified channel and offset. }
    procedure TransferVertices(const ABuffer: TBuffer;
      const AVertexElements: array of TVertexElement; const AStartVertex, AVertexCount: Integer;
      const AChannel: Cardinal = 0; const AOffset: Cardinal = 0;
      const ASemanticIndex: Integer = 0); overload;

    { Transfers mesh indices to the specified index buffer. The buffer data type is determined by its pitch.
      Returns number of bytes that the resulting data occupies. If @italic(ABuffer) is @nil, this function
      only calculates how many bytes the data will occupy. }
    procedure TransferIndices(const ABuffer: Pointer; const APitch: Cardinal; const AStartVertex,
      AStartIndex, AIndexCount: Integer); overload; inline;

    // Transfers mesh index elements to an index buffer object with the given format at the specified offset.
    procedure TransferIndices(const ABuffer: TBuffer; const AStartVertex, AStartIndex, AIndexCount: Integer;
      const AOffset: Cardinal = 0); overload; inline;

    // Attempts to load the mesh file from disk using Assimp DLL to mesh buffer and calculates its boundaries.
    function LoadMeshFromFile(const AFileName: Utf8String; var AMinBounds,
      AMaxBounds: TVector3f): Boolean; overload; inline;

    { Attempts to load the mesh file from disk using Assimp DLL to mesh buffer and calculates its boundaries.
      Additionally, loads meta-tag information for important sub-mesh locations. }
    function LoadMeshFromFile(const AMeshMetaTags: TMeshMetaTags; const AFileName: Utf8String;
      var AMinBounds, AMaxBounds: TVector3f): Boolean; overload; inline;

    { Attempts to load the mesh file from disk using Assimp DLL to mesh buffer and calculates its boundaries.
      In case of error, returns an error message string. }
    function LoadMeshFromFile(const AFileName: Utf8String; var AMinBounds,
      AMaxBounds: TVector3f; out ADebug: Utf8String): Boolean; overload;

    { Attempts to load the mesh file from disk using Assimp DLL to mesh buffer and calculates its boundaries.
      Additionally, loads meta-tag information for important sub-mesh locations. In case of error, returns
      an error message string. }
    function LoadMeshFromFile(const AMeshMetaTags: TMeshMetaTags; const AFileName: Utf8String; var AMinBounds,
      AMaxBounds: TVector3f; out ADebug: Utf8String): Boolean; overload;

    // Attempts to save the mesh to file on disk using Assimp DLL from an existing mesh buffer.
    function SaveMeshToFile(const AFileName: Utf8String;
      const AExportFlags: TMeshExportFlags): Boolean; overload; inline;

    { Attempts to save the mesh to file on disk using Assimp DLL from an existing mesh buffer.
      In case of error, returns an error message string. }
    function SaveMeshToFile(const AFileName: Utf8String;
      const AExportFlags: TMeshExportFlags; out ADebug: Utf8String): Boolean; overload;

    // 3D mesh buffer information.
    property Information: TMeshBufferInformation read GetInformation write SetInformation;

    // Currently set transformation matrix.
    property Transform: TMatrix4f read GetTransform write SetTransform;
  end;

  // Pointer to @link(TSampler).
  PSampler = ^TSampler;

  // Sampler object that defines how texture is read by the shaders.
  TSampler = object(TBaseObject)
  private
    function GetState: TSamplerState; inline;
    procedure SetState(const AState: TSamplerState); inline;
  public
    // Releases the sampler object.
    procedure Free; inline;

    // Binds sampler object to a particular channel.
    function Bind(const AChannel: Cardinal = 0): Boolean;

    // Unbinds sampler object from the particular channel.
    procedure Unbind(const AChannel: Cardinal = 0);

    // Current sampler object parameters.
    property State: TSamplerState read GetState write SetState;
  end;

  // Pointer to @link(TRasterSurface).
  PRasterSurface = ^TRasterSurface;

  // Surface that contains image in memory for pixel manipulation.
  TRasterSurface = object(TBaseObject)
  private
    function GetParameters: TRasterSurfaceParameters; inline;
    function GetPixel(const AX, AY: Integer): TIntColor; inline;
    procedure SetPixel(const AX, AY: Integer; const AColor: TIntColor); inline;
  public
    // Releases the surface.
    procedure Free; inline;

    { Attempts to update the characteristics of the surface (if the requested parameters are different than
      those that are currently set) and optionally the content of the surface. If succeeded, this may update
      the contents of "parameters" with the values that were actually considered. }
    function Update(var AParameters: TRasterSurfaceParameters): Boolean; inline;

    { Given the provided pixel format, returns one of pixel formats that are currently supported, being a
      closest match. If there is no possible match, "TPixelFormat.Unknown" is returned. }
    function ApproximateFormat(const AFormat: TPixelFormat): TPixelFormat; inline;

    { Converts surface from its currently set pixel format to the new one. If both format match, the function
      does nothing. }
    function ConvertFormat(const AFormat: TPixelFormat): Boolean; inline;

    { Copies a portion of source surface to the current one according to specified source rectangle and
      destination position. If source rectangle is empty (or nil), then the entire source surface will be
      copied. This function does the appropriate clipping and pixel format conversion. It does not change
      current surface size or pixel format. Note that "premultiplied alpha" flag of either current or source
      surfaces is completely ignored. }
    function CopyRect(const ASurface: PRasterSurface; const ASourceRect: TIntRect;
      const ADestPos: TPoint2i): Boolean; inline;

    { Copies entire contents from a source surface to the current one. If the current surface has size and/or
      pixel format not specified, these will be copied from the source surface as well. If current surface is
      not empty, then its pixel format will not be modified - in this case, pixel format conversion may occur.
      This function will try to ensure that current surface size matches the source surface and if if this
      cannot be achieved, it will fail. }
    function CopyFrom(const ASurface: PRasterSurface): Boolean; inline;

    // Clears the entire surface with zeros.
    procedure Clear; overload; inline;

    // Clears the entire surface with a given color. This does pixel format conversion when appropriate.
    function Clear(const AColor: TIntColor): Boolean; overload; inline;

    // Processes surface pixels, setting alpha-channel to either fully translucent or fully opaque.
    function ResetAlpha(const AOpaque: Boolean = True): Boolean; inline;

    { Processes the whole surface to determine whether it has meaningful alpha-channel. A surface that has
      all its pixels with alpha-channel set to fully translucent or fully opaque (but not mixed) is
      considered lacking alpha-channel. On the other hand, a surface that has at least one pixel with
      alpha-channel value different than any other pixel, is considered to have alpha-channel. This is
      useful to determine whether the surface can be stored in one of pixel formats lacking alpha-channel,
      to avoid losing any transparency information. }
    function HasAlphaChannel: Boolean; inline;

    { Processes the whole surface, premultiplying each pixel's red, green and blue values by the
      corresponding alpha-channel value, resulting in image with premultiplied alpha. Note that this is an
      irreversible process, during which some color information is lost permanently (smaller alpha values
      contribute to bigger information loss). This is generally useful to prepare the image for generating
      mipmaps and/or alpha-blending, to get more accurate visual results. }
    function PremultiplyAlpha: Boolean; inline;

    { Processes the whole surface, dividing each pixel by its alpha-value, resulting in image with
      non-premultiplied alpha. During this process, some color information may be lost due to precision
      issues. This can be useful to obtain original pixel information from image that has been previously
      premultiplied; however, this does not recover lost information during premultiplication process.
      For instance, pixels that had alpha value of zero and were premultiplied, lose all information and
      cannot be recovered; pixels with alpha value of 128 (that is, 50% opaque) lose half of their precision
      and after "unpremultiply" process will have values multiples of 2. }
    function UnpremultiplyAlpha: Boolean; inline;

    // Mirrors the visible image on surface horizontally.
    function Mirror: Boolean; inline;

    // Flips the visible image on surface vertically.
    function Flip: Boolean; inline;

    // Draws a rectangle filled with specified gradient onto this surface with alpha-blending.
    procedure FillRect(const ADestRect: TIntRect; const AColors: TColorRect); inline;

    { Draws a single pixel at specified coordinates and color with alpha-blending. It also does a sanity
      check for specified coordinates and if they are outside of valid range, does nothing. }
    procedure DrawPixel(const AX, AY: Integer; const AColor: TIntColor); inline;

    // Draws horizontal line of single color with alpha-blending at the specified coordinates, with clipping.
    procedure HorizLine(const AX, AY, ALineWidth: Integer; const AColor: TIntColor); inline;

    // Draws vertical line of single color with alpha-blending at the specified coordinates, with clipping.
    procedure VertLine(const AX, AY, ALineHeight: Integer; const AColor: TIntColor); inline;

    { Draws rectangle of one pixel wide and single color with alpha-blending at the specified area, with
      clipping. }
    procedure FrameRect(const ADestRect: TIntRect; const AColor: TIntColor); inline;

    { Renders a portion of source surface onto this one at the specified origin, using alpha-blending and
      premultiplying pixels taken from the source surface with specified color gradient. This does pixel
      conversion and clipping as necessary. }
    procedure Draw(const ASurface: PRasterSurface; const ADestPos: TPoint2i; const ASourceRect: TIntRect;
      const AColors: TColorRect); inline;

    { This function works similarly to @link(CopyFrom), except that it produces image with half of size,
      averaging each four pixels to one. This is specifically useful to generate mipmaps. }
    function ShrinkToHalfFrom(const ASurface: PRasterSurface): Boolean; inline;

    { Retrieves pixel from floating-point coordinates, interpolating linearly between four neighbor pixels as
      necessary to get an accurate match. This can be used for limitless stretching, to get color values that
      lie between individual pixels and slowly change from one pixel to another. }
    function GetBilinearPixel(const AX, AY: Single): TIntColor; inline;

    { Returns color value of pixel at the specified coordinates with an additional sanity check: if the
      coordinates are outside of valid range, they will be clamped so that they stay within. }
    function GetPixelWithEdge(const AX, AY: Integer): TIntColor; inline;

    // Calculates signed distance based on alpha-channel at the given position and spread.
    function CalculateSignedDistance(const AX, AY: Integer; const ASpread: VectorFloat): VectorFloat; inline;

    { This function works similarly to @link(CopyRect), but provides stretching and/or shrinking. That is, it
      copies source surface rectangle onto destination rectangle with point filtering. Clipping and pixel
      format conversion is done as necessary. }
    procedure StretchFrom(const ASurface: PRasterSurface; const ADestRect, ASourceRect: TFloatRect); inline;

    { This function works similarly to @link(CopyRect), but provides bilinear stretching. That is, it copies
      source surface rectangle onto destination rectangle with linear filtering. Clipping and pixel format
      conversion is done as necessary. Note that this function is meant for stretching only; shrinking
      although will also work, but result in inaccurate results as shrinking requires calculating average of
      variable number of pixels depending on shrink ratio. }
    procedure StretchBilinearFrom(const ASurface: PRasterSurface; const ADestRect,
      ASourceRect: TFloatRect); inline;

    // Calculates signed distance field from alpha values of the source.
    function MakeSignedDistanceField(const ASurface: PRasterSurface; const ASpread: VectorFloat;
      const ADestPos: TPoint2i; const ASourceRect: TIntRect): Boolean; inline;

    { Saves the contents of the surface to external file. This function typically supports most common file
      formats such as BMP, PNG, JPEG, but may also support other formats like GIF and TIFF.
      @italic(Quality) parameter, in case of formats such as JPEG determines the compression ratio
      (between 0 and 100). }
    function SaveToFile(const AFileName: Utf8String; const AQuality: NativeUInt = 0): Boolean; inline;

    { Saves the contents of surface to a file in memory and returns the size of resulting file. This function
      will save up to @italic(AFileContentSize) bytes to the provided pointer, even if it means that not a
      complete file will be saved. If @italic(fileContent) pointer is @nil, then this function will provide
      an estimate size required to store the file in memory. This function typically supports most common
      file formats such as BMP, PNG, JPEG, but may also support other formats like GIF and TIFF.
      @italic(Quality) parameter, in case of formats such as JPEG determines the compression ratio
      (between 0 and 100). }
    function SaveToFileInMemory(const AFileContent: Pointer; const AFileContentSize: Cardinal;
      const AExtension: Utf8String; const AQuality: NativeUInt = 0): Cardinal; inline;

    // Current surface parameters.
    property Parameters: TRasterSurfaceParameters read GetParameters;

    { Access to surface's pixels. Note that this may involve pixel format conversion and from performance
      point of view might not be the fastest technique (using surface content directly is faster). }
    property Pixels[const AX, AY: Integer]: TIntColor read GetPixel write SetPixel;
  end;

  // Texture that contains image data typically stored in GPU memory.
  TTexture = object(TBaseObject)
  private
    function GetParameters: TTextureParameters; inline;
  public
    // Releases the texture.
    procedure Free; inline;

    { Copies content from user pointer to texture's owned memory at the given location. The data is assumed
      to match texture's pixel format and destination rectangle, if specified, must be within valid bounds
      (no clipping is performed). }
    function Update(const AContent: Pointer; const APitch: Cardinal; const ALayer: Integer;
      const ARect: TIntRect; const AMipLevel: Integer = 0): Boolean; inline;

    { Retrieves content from the given location of texture's owned memory to the user pointer. The data is
      copied in its original format and source rectangle, if specified, must be within valid bounds
      (no clipping is performed). }
    function Retrieve(const AContent: Pointer; const APitch: Cardinal; const ALayer: Integer;
      const ARect: TIntRect; const AMipLevel: Integer = 0): Boolean; inline;

    { Copies a portion of source texture to the current one according to specified source rectangle and
      destination position. If source rectangle is empty, then the entire source texture will be copied.
      This function does the appropriate clipping and pixel format conversion (albeit at significant
      performance cost). Note that @italic(PremultipliedAlpha) attribute of either current or source texture
      is completely ignored. }
    function Copy(const ATexture: PTexture; const ADestLayer: Integer; const ADestPos: TPoint2i;
      const ASrcLayer: Integer; const ASourceRect: TIntRect; const ADestMipLevel: Integer = 0;
      const ASrcMipLevel: Integer = 0): Boolean; overload; inline;

    { Copies a portion of source surface to the current texture according to specified source rectangle and
      destination position. If source rectangle is empty, then the entire source surface will be copied. This
      function does the appropriate clipping and pixel format conversion. Note that
      @italic(PremultipliedAlpha) attribute of either current or source surface is completely ignored. }
    function Copy(const ASurface: TRasterSurface; const ALayer: Integer; const ADestPos: TPoint2i;
      const ASourceRect: TIntRect; const AMipLevel: Integer = 0): Boolean; overload; inline;

    { Copies a portion of current texture to the destination surface according to specified source rectangle
      and destination position. If source rectangle is empty, then the entire texture surface will be copied.
      This function does the appropriate clipping and pixel format conversion. Note that "premultipliedAlpha"
      attribute of either current or source surface is completely ignored. }
    function Save(const ASurface: TRasterSurface; const ALayer: Integer; const ADestPos: TPoint2i;
      const ASourceRect: TIntRect; const AMipLevel: Integer = 0): Boolean; inline;

    // Loads image from disk and copies it to the specified location and parameters.
    function LoadFromFile(const AFileName: Utf8String; const ALayer: Integer; const ADestPos: TPoint2i;
      const ASourceRect: TIntRect; const AMipLevel: Integer = 0): Boolean; inline;

    // Saves the appropriate portion of texture to external file.
    function SaveToFile(const AFileName: Utf8String; const AQuality: Pointer; const ALayer: Integer;
      const ASourceRect: TIntRect; const AMipLevel: Integer = 0): Boolean; inline;

    // Clears entire texture surface and fills pixels with zeros.
    function Clear: Boolean; overload; inline;

    { Clears texture surface with the given color.
      This must be called outside of device and texture @link(BeginScene) / @link(EndScene) blocks. }
    function Clear(const AColor: TFloatColor; const ALayer: Integer = 0): Boolean; overload; inline;

    // Binds texture to the specified rendering channel.
    function Bind(const AChannel: Cardinal = 0): Boolean; inline;

    // Unbinds texture from the specified rendering channel.
    function Unbind(const AChannel: Cardinal = 0): Boolean; inline;

    { Attaches another drawable texture with the current texture into a common MRT rendering stack.
      Drawable textures will be available to shaders in the same order of attachment. }
    function Attach(const ATexture: PTexture; const ALayer: Integer = 0): Boolean; inline;

    // Removes all previous drawable texture attachments.
    procedure Detach; inline;

    // Starts rendering on the drawable texture.
    function BeginScene(const ALayer: Cardinal = 0): Boolean; inline;

    // Finishes rendering on the drawable texture.
    function EndScene: Boolean; inline;

    // Generates texture's mipmaps from its base (level = 0) image.
    function GenerateMipMaps: Boolean; inline;

    { Resets any cache associated with the texture, which means purging any temporary resources.
      This also removes all existing texture attachments. }
    procedure ResetCache; inline;

    // Current texture parameters.
    property Parameters: TTextureParameters read GetParameters;
  end;

// Creates new instance of compute shader program from its parameters.
function ComputeProgramInit(const ADevice: TDevice; const AProgramElements: TProgramElements;
  const AShader: TBytes): TComputeProgram; overload;

// Creates new instance of compute shader program from its parameters.
function ComputeProgramInit(const ADevice: TDevice; const AProgramElements: array of TProgramElement;
  const AShader: TBytes): TComputeProgram; overload;

// Creates new instance of compute shader program loading its code from disk.
function ComputeProgramInitFromFile(const ADevice: TDevice; const AProgramElements: TProgramElements;
  const AShader: string): TComputeProgram; overload;

// Creates new instance of compute shader program loading its code from disk.
function ComputeProgramInitFromFile(const ADevice: TDevice; const AProgramElements: array of TProgramElement;
  const AShader: string): TComputeProgram; overload;

// Creates new instance of 3D mesh model.
function MeshModelInit(const ADevice: TDevice; const AVertexElements: TVertexElements;
  const AVertexCount: Cardinal; const AIndexCount: Cardinal = 0; const AChannel: Cardinal = 0;
  const AInitialVertexData: Pointer = nil;
  const AInitialIndexData: Pointer = nil): TMeshModel; overload;

// Creates new instance of 3D mesh model.
function MeshModelInit(const ADevice: TDevice; const AVertexElements: array of TVertexElement;
  const AVertexCount: Cardinal; const AIndexCount: Cardinal = 0; const AChannel: Cardinal = 0;
  const AInitialVertexData: Pointer = nil;
  const AInitialIndexData: Pointer = nil): TMeshModel; overload;

// Creates a new instance of meta-tags that describe important axis-aligned bounding areas inside a 3D mesh.
function MeshMetaTagsInit: TMeshMetaTags; inline;

// Creates new instance of 3D mesh buffer.
function MeshBufferInit: TMeshBuffer; inline;

// Creates new instance of sampler object.
function SamplerInit(const ADevice: TDevice; const ASamplerState: TSamplerState): TSampler; inline;

// Creates new instance of surface with the given dimensions.
function RasterSurfaceInit(const AWidth, AHeight: Integer; const AFormat: TPixelFormat =
  {$IFDEF DELPHI_LEGACY}pfUnknown{$ELSE}TPixelFormat.Unknown{$ENDIF}
  ): TRasterSurface; overload; inline;

{ Creates new instance of surface containing image loaded from external file. The preference for
  premultiplied or non-premultiplied alpha-channel is merely a suggestion, which can influence how the
  image is loaded (depending on underlying implementation). This function typically supports most common
  file formats such as BMP, PNG, JPEG, but may also support other formats like GIF and TIFF. }
function RasterSurfaceInit(const AFileName: Utf8String; const AFormatRequest: TAlphaFormatRequest =
  {$IFDEF DELPHI_LEGACY}afrDontCare{$ELSE}TAlphaFormatRequest.DontCare{$ENDIF}
  ): TRasterSurface; overload; inline;

{ Creates new instance of surface containing image loaded from file that has been preloaded into memory.
  The internal file format is determined by "extension" parameter, which should contain a valid file
  extension such as ".png" (case-insensitive). The preference for premultiplied or non-premultiplied
  alpha-channel is merely a suggestion, which can influence how the image is loaded (depending on
  underlying implementation). This function typically supports most common file formats such as BMP, PNG,
  JPEG, but may also support other formats like GIF and TIFF. }
function RasterSurfaceInit(const AFileContent: Pointer; const AContentSize: Cardinal;
  const AExtension: Utf8String; const AFormatRequest: TAlphaFormatRequest =
  {$IFDEF DELPHI_LEGACY}afrDontCare{$ELSE}TAlphaFormatRequest.DontCare{$ENDIF}
  ): TRasterSurface; overload; inline;

// Creates new instance of texture.
function TextureInit(const ADevice: TDevice;
  const AParameters: TTextureParameters): TTexture; overload; inline;

{ Creates a new instance of texture loading its contents from an external file. The preference for
  premultiplied or non-premultiplied alpha-channel is merely a suggestion, which can influence how the image
  is loaded (depending on underlying implementation). This function typically supports most common file
  formats such as BMP, PNG, JPEG, but may also support other formats like GIF and TIFF. }
function TextureInit(const ADevice: TDevice; const AFileName: Utf8String;
  const AFormat: TPixelFormat = {$IFDEF DELPHI_LEGACY}pfUnknown{$ELSE}TPixelFormat.Unknown{$ENDIF};
  AAttributes: Cardinal = 0): TTexture; overload; inline;

{ Creates a new instance of texture loading its contents from an image file that has been preloaded into
  memory. The internal file format is determined by "extension" parameter, which should contain a valid file
  extension such as @italic(.png) (case-insensitive). The preference for premultiplied or non-premultiplied
  alpha-channel is merely a suggestion, which can influence how the image is loaded (depending on underlying
  implementation). This function typically supports most common file formats such as @italic(BMP),
  @italic(PNG), @italic(JPEG), but may also support other formats like @italic(GIF) and @italic(TIFF). }
function TextureInit(const ADevice: TDevice; const AFileContent: Pointer; const AContentSize: Cardinal;
  const AExtension: Utf8String;
  const AFormat: TPixelFormat = {$IFDEF DELPHI_LEGACY}pfUnknown{$ELSE}TPixelFormat.Unknown{$ENDIF};
  AAttributes: Cardinal = 0): TTexture; overload; inline;

const
  CanvasDefaultBlendingEffect = {$IFDEF DELPHI_LEGACY}beNormal{$ELSE}TBlendingEffect.Normal{$ENDIF};

type
  // Pointer to @link(TImageAtlas).
  PImageAtlas = ^TImageAtlas;

  // Image atlas, sub-images of which can be rendered with canvas.
  TImageAtlas = object(TBaseObject)
  private
    function GetTextureCount: Integer; inline;
    function GetTexture(const ATextureIndex: Integer): TTexture; inline;
    function GetRegionCount: Integer; inline;
    function GetRegion(const ARegionIndex: Integer): TImageRegion; inline;
  public
    // Releases the image atlas.
    procedure Free; inline;

    // Creates and adds a new region to the list.
    function CreateRegion(const ARect: TIntRect; const ATextureIndex: Integer): Integer; inline;

    // Removes region with given index from the lsit.
    procedure RemoveRegion(const ARegionIndex: Integer); inline;

    // Creates list of regions based on repeated rectangular pattern dimensions.
    procedure MakeRegions(const APatternSize, AVisibleSize: TPoint2i; const APatternCount: Integer); inline;

    // Removes all existing textures.
    procedure ClearTextures; inline;

    // Removes texture with the given index.
    procedure RemoveTexture(const ATextureIndex: Integer); inline;

    { Attempts to create and initialize a texture with the given parameters, which is then added to the
      list. In case of failure, -1 is returned. }
    function CreateTexture(const ASize: TPoint2i; const AFormat: TPixelFormat;
      const AAttributes: Cardinal): Integer; inline;

    { Finds a suitable location to accomodate the given rectangle in existing available space, adds the
      appropriate region to the list and returns its index. If no space is available, -1 is returned. }
    function Pack(const ASize: TPoint2i; const APadding: Integer = 0): Integer; overload; inline;

    { Finds a suitable location to accomodate the given surface in existing available space, adds the
      appropriate region to the list and copies the contents of surface to the appropriate texture,
      returning the final region index. If no space is available or copying fails, -1 is returned. }
    function Pack(const ASurface: TRasterSurface; const ASourceRect: TIntRect;
      const APadding: Integer = 0): Integer; overload; inline;

    // Number of textures contained within the atlas.
    property TextureCount: Integer read GetTextureCount;

    // Texture associated with a particular index or nil object if such is not available.
    property Texture[const ATextureIndex: Integer]: TTexture read GetTexture;

    // Number of regions contained within the atlas.
    property RegionCount: Integer read GetRegionCount;

    // Region information for the given index.
    property Region[const ARegionIndex: Integer]: TImageRegion read GetRegion;
  end;

// Creates new instance of image atlas.
function ImageAtlasInit(const ADevice: TDevice): TImageAtlas; inline;

type
  // Array of @link(Cardinal) values.
  TCardinalArray = array of Cardinal;

  // Pointer to @link(TCanvas).
  PCanvas = ^TCanvas;

  // Canvas that can render geometry either in 2D or on a particular plane in 3D world.
  TCanvas = object(TBaseObject)
  private
    function GetClipRect: TIntRect; inline;
    procedure SetClipRect(const AClipRect: TIntRect); inline;
    function GetTransform: TMatrix4f; inline;
    procedure SetTransform(const ATransform: TMatrix4f); inline;
    function GetBatchCount: Integer; inline;
    function GetAttributes: TCanvasAttributes; inline;
    procedure SetAttributes(const AAttributes: TCanvasAttributes); inline;
    function GetSignedDistanceField: TSignedDistanceField; inline;
    procedure SetSignedDistanceField(const ASignedDistanceField: TSignedDistanceField); inline;
    function GetContextState: TCanvasContextState; inline;
    procedure SetContextState(const AContextState: TCanvasContextState); inline;
  public
    // Releases the canvas.
    procedure Free; inline;

    // Starts rendering with the canvas.
    function BeginScene: Boolean; inline;

    // Finishes rendering with the canvas.
    procedure EndScene; inline;

    // Draws pixels with the given positions and colors.
    procedure Pixels(const APositions: PPoint2f; const AColors: PIntColor; const AElementCount: Integer;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); inline;

    // Draws lines with the given vertices, colors and indices.
    procedure Lines(const AVertices: PPoint2f; const AColors: PIntColor; const AIndices: PCardinal;
      const AVertexCount, APrimitiveCount: Integer;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); inline;

    // Draws triangles with the given vertices, colors and indices.
    procedure Triangles(const AVertices: PPoint2f; const AColors: PIntColor; const AIndices: PCardinal;
      const AVertexCount, APrimitiveCount: Integer;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); overload; inline;

    // Draws triangles with the given vertices, texture coordinates, colors and indices.
    procedure Triangles(const ATexture: TTexture; const AVertices, ATexCoords: PPoint2f;
      const AColors: PIntColor; const AIndices: PCardinal; const AVertexCount, APrimitiveCount: Integer;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); overload; inline;

    // Draws a pixel with the given position and color.
    procedure Pixel(const APosition: TPoint2f; const AColor: TIntColor;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); inline;

    // Draws a line with the given positions and colors.
    procedure Line(const ASrcPos, ADestPos: TPoint2f; const AColors: TColorPair;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); inline;

    // Draws a line of varying thickness with the given positions and colors.
    procedure ThickLine(const ASrcPos, ADestPos: TPoint2f; const AColors: TColorPair;
      const AThickness: Single; const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); inline;

    // Draws ellipse with the given origin, radiuses, color and steps using @link(Line) primitive.
    procedure LineEllipse(const AOrigin, ARadius: TPoint2f; const ASteps: Integer; const AColor: TIntColor;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); inline;

    // Draws circle with the given origin, radius, color and steps using @link(Line) primitive.
    procedure LineCircle(const AOrigin: TPoint2f; const ARadius: VectorFloat; const ASteps: Integer;
      const AColor: TIntColor; const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); inline;

    // Draws ellipse of varying thickness with the given origin, radiuses, color and steps.
    procedure ThickLineEllipse(const AOrigin, ARadius: TPoint2f; const ASteps: Integer;
      const AColor: TIntColor; const AThickness: Single;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); inline;

    // Draws circle of varying thickness with the given origin, radius, color and steps.
    procedure ThickLineCircle(const AOrigin: TPoint2f; const ARadius: VectorFloat; const ASteps: Integer;
      const AColor: TIntColor; const AThickness: Single;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); inline;

    // Draws lines between specified triangle vertices and colors.
    procedure LineTriangle(const AVertex1, AVertex2, AVertex3: TPoint2f; const AColor1, AColor2,
      AColor3: TIntColor; const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); inline;

    // Draws line path of varying thickness between specified triangle vertices and colors.
    procedure ThickLineTriangle(const AVertex1, AVertex2, AVertex3: TPoint2f; const AColor1, AColor2,
      AColor3: TIntColor; const AThickness: VectorFloat; const AJoint: TPathJoint =
      {$IFDEF DELPHI_LEGACY}pjMiter{$ELSE}TPathJoint.Miter{$ENDIF};
      const ASmoothStep: VectorFloat = 1.0;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); inline;

    // Draws a wireframe quadrilateral with the given vertices and colors using @link(Line) primitive.
    procedure LineQuad(const AVertices: TQuad; const AColors: TColorRect;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); inline;

    // Draws a wireframe quadrilateral of varying thickness with the given vertices and colors.
    procedure ThickLineQuad(const AVertices: TQuad; const AColors: TColorRect; const AThickness: Single;
      const AJoint: TPathJoint = {$IFDEF DELPHI_LEGACY}pjMiter{$ELSE}TPathJoint.Miter{$ENDIF};
      const ASmoothStep: Single = 1.0; const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); inline;

    { Draws lines between each vertex in hexagon. The vertices are spaced within a circle with diameter of
      one, multiplied by the given matrix and drawn with gradient of six colors at the corresponding vertices.
      The bounding width of hexagon is exactly one, which simplifies placement of multiple hexagons. The
      final size, position and rotation of hexagon can be given using one or a combination of several 3x3
      matrices multiplied together. This method uses @link(Line) primitive. }
    procedure LineHexagon(const AMatrix: TMatrix3f; const AColor1, AColor2, AColor3, AColor4, AColor5,
      AColor6: TIntColor; const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); overload; inline;

    { Draws lines between each vertex in hexagon. The vertices are spaced within
      a circle with diameter of one, multiplied by the given matrix and filled with four-color gradient interpolated
      to the corresponding vertices. The bounding width of hexagon is exactly one, which simplifies placement of
      multiple hexagons. The final size, position and rotation of hexagon can be given using one or a combination
      of several 3x3 matrices multiplied together. This method uses @link(Line) primitive. }
    procedure LineHexagon(const AMatrix: TMatrix3f; const AColors: TColorRect;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); overload; inline;

    { Draws lines of varying thickness between each vertex in hexagon. The path vertices are spaced within
      a circle with diameter of one, multiplied by the given matrix and drawn with gradient of six colors at
      the corresponding corners. The final size, position and rotation of hexagon can be given using one or
      a combination of several 3x3 matrices multiplied together. }
    procedure ThickLineHexagon(const AMatrix: TMatrix3f; const AColor1, AColor2, AColor3, AColor4, AColor5,
      AColor6: TIntColor; const AThickness: VectorFloat; const AJoint: TPathJoint =
      {$IFDEF DELPHI_LEGACY}pjMiter{$ELSE}TPathJoint.Miter{$ENDIF};
      const ASmoothStep: Single = 1.0;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); overload; inline;

    { Draws lines of varying thickness between each vertex in hexagon. The path vertices are spaced within
      a circle with diameter of one, multiplied by the given matrix and filled with four-color gradient
      interpolated to the corresponding vertices. The final size, position and rotation of hexagon can be
      given using one or a combination of several 3x3 matrices multiplied together. }
    procedure ThickLineHexagon(const AMatrix: TMatrix3f; const AColors: TColorRect;
      const AThickness: VectorFloat; const AJoint: TPathJoint =
      {$IFDEF DELPHI_LEGACY}pjMiter{$ELSE}TPathJoint.Miter{$ENDIF};
      const ASmoothStep: Single = 1.0;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); overload; inline;

    // Draws triangle filled with color gradient specified by given positions and colors.
    procedure Triangle(const AVertex1, AVertex2, AVertex3: TPoint2f; const AColor1, AColor2,
      AColor3: TIntColor; const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); overload; inline;

    // Draws a filled quadrilateral with the given vertices and colors.
    procedure Quad(const AVertices: TQuad; const AColors: TColorRect;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); overload; inline;

    // Draws a filled rectangle with the given vertices and colors.
    procedure FillRect(const ARect: TFloatRect; const AColors: TColorRect;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); inline;

    // Draws a rectangle of varying thickness with the given vertices and colors.
    procedure FrameRect(const ARect: TFloatRect; const AColors: TColorRect; const AThickness: Single;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); inline;

    // Draws a filled rectangle with the given gradient and round corners.
    procedure FillRoundRect(const ARect: TFloatRect; const AColors: TColorRect; const ARadius: Single;
      const ASteps: Cardinal; const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); inline;

    // Draws a round frame for the given rectangle area with varying thickness and gradient.
    procedure FrameRoundRect(const ARect: TFloatRect; const AColors: TColorRect;
      const ARadius: Single; const AThickness: Single; const ASteps: Cardinal;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); inline;

    // Draws a filled rectangle with the given gradient and round corners on top.
    procedure FillRoundRectTop(const ARect: TFloatRect; const AColors: TColorRect; const ARadius: Single;
      const ASteps: Integer; const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); inline;

    // Draws a filled rectangle with the given gradient and round corners on bottom.
    procedure FillRoundRectBottom(const ARect: TFloatRect; const AColors: TColorRect; const ARadius: Single;
      const ASteps: Integer; const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); inline;

    // Draws a filled rectangle with the given gradient and round corners, but top is reverted.
    // This can be used to render rounded window background behind caption.
    procedure FillRoundRectTopInverse(const ARect: TFloatRect; const AColors: TColorRect;
      const ARadius: Single; const ASteps: Integer;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); inline;

    { Draws either a highlight or a shadow around the specified rectangle with the given parameters,
      mimicking effect of gaussian highlight. }
    procedure Highlight(const ARect: TFloatRect; const ACornerRadius, ALuma, ADistance: Single;
      const ASteps: Integer); inline;

    { Draws hexagon where vertices are spaced within a circle with diameter of one, multiplied by the given
      matrix and filled with gradient of six colors at the corresponding vertices. The bounding width of
      hexagon is exactly one, which simplifies placement of multiple hexagons. The final size, position and
      rotation of hexagon can be given using one or a combination of several 3x3 matrices multiplied together. }
    procedure Hexagon(const AMatrix: TMatrix3f; const AColor1, AColor2, AColor3, AColor4, AColor5,
      AColor6: TIntColor; const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); overload; inline;

    { Draws hexagon where vertices are spaced within a circle with diameter of one, multiplied by the given
      matrix and filled with four-color gradient interpolated to the corresponding vertices. The bounding
      width of hexagon is exactly one, which simplifies placement of multiple hexagons. The final size,
      position and rotation of hexagon can be given using one or a combination of several 3x3 matrices
      multiplied together. }
    procedure Hexagon(const AMatrix: TMatrix3f; const AColors: TColorRect;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); overload; inline;

    // Draws a filled arc with the given origin, radiuses, angles, steps and colors.
    procedure Arc(const AOrigin, ARadius: TPoint2f; const AInitAngle, AEndAngle: Single;
      const ASteps: Integer; const AColors: TColorRect;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); inline;

    // Draws a filled ellipse with the given origin, radiuses, steps and colors.
    procedure Ellipse(const AOrigin, ARadius: TPoint2f; const ASteps: Integer; const AColors: TColorRect;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); inline;

    // Draws a filled ribbon between inner and outer radiuses filled with four-color gradient.
    procedure Ribbon(const AOrigin, AInsideRadius, AOutsideRadius: TPoint2f; const ASteps: Integer;
      const AColors: TColorRect; const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); overload; inline;

    // Draws a filled ribbon between inner and outer radiuses filled with continuous three-pair gradient.
    procedure Ribbon(const AOrigin, AInsideRadius, AOutsideRadius: TPoint2f; const ASteps: Integer;
      const AColor1, AColor2, AColor3: TColorPair;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); overload; inline;

    // Draws a filled ribbon between inner and outer radiuses filled with a single color.
    procedure Ribbon(const AOrigin, AInsideRadius, AOutsideRadius: TPoint2f; const ASteps: Integer;
      const AColor: TIntColor; const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); overload; inline;

    // Draws a filled tape between the given radiuses and angles, filled with four-color gradient.
    procedure Tape(const AOrigin, AInsideRadius, AOutsideRadius: TPoint2f; const AInitAngle,
      AEndAngle: Single; const ASteps: Integer; const AColors: TColorRect;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); overload; inline;

    // Draws a filled tape between the given radiuses and angles, filled with continuous three-pair gradient.
    procedure Tape(const AOrigin, AInsideRadius, AOutsideRadius: TPoint2f; const AInitAngle,
      AEndAngle: Single; const ASteps: Integer; const AColor1, AColor2, AColor3: TColorPair;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); overload; inline;

    // Draws a filled tape between the given radiuses and angles, filled with a single color.
    procedure Tape(const AOrigin, AInsideRadius, AOutsideRadius: TPoint2f; const AInitAngle,
      AEndAngle: Single; const ASteps: Integer; const AColor: TIntColor;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); overload; inline;

    // Draws a filled rectangle with a hole inside of it, filled with the given colors.
    procedure RectWithHole(const ARect: TFloatRect; const AHoleOrigin, AHoleRadius: TPoint2f;
      const AColors: TColorPair; const ASteps: Integer;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); inline;

    { Draws a filled path that consists of two or more line points with their appropriate colors and optionally,
      different joint types. This calculates thick lines and their joints using line intersection functions and
      attempts to work for most cases, except when points are too close (less than half of thickness) to each
      other. }
    procedure ThickLinePath(const APoints: TPoint2FArray; const AColors: TIntColorArray;
      const AJoints: TPathJointArray; const AThickness: VectorFloat;
      const ABaseJoint: TPathJoint = {$IFDEF DELPHI_LEGACY}pjMiter{$ELSE}TPathJoint.Miter{$ENDIF};
      const ALineCaps: TLineCaps = {$IFDEF DELPHI_LEGACY}lcButt{$ELSE}TLineCaps.Butt{$ENDIF};
      const AMiterLimit: VectorFloat = 1.0; const ASmoothStep: VectorFloat = 1.0;
      const AClosePath: Boolean = False;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); overload; inline;

    { Draws a filled path that consists of two or more line points with their appropriate colors and optionally,
      different joint types. This calculates thick lines and their joints using line intersection functions and
      attempts to work for most cases, except when points are too close (less than half of thickness) to each
      other. }
    procedure ThickLinePath(const APoints: array of TPoint2f; const AColors: array of TIntColor;
      const AJoints: array of TPathJoint; const AThickness: VectorFloat;
      const ABaseJoint: TPathJoint = {$IFDEF DELPHI_LEGACY}pjMiter{$ELSE}TPathJoint.Miter{$ENDIF};
      const ALineCaps: TLineCaps = {$IFDEF DELPHI_LEGACY}lcButt{$ELSE}TLineCaps.Butt{$ENDIF};
      const AMiterLimit: VectorFloat = 1.0; const ASmoothStep: VectorFloat = 1.0;
      const AClosePath: Boolean = False;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); overload;

    // Draws a filled polygon with the given contours and the appropriate winding rules.
    procedure Polygon(const AContours: TPoint2FArray; const AContourLengths: TCardinalArray;
      const AColor: TIntColor; const AWindingRule: TTessellationWinding =
      {$IFDEF DELPHI_LEGACY}twOdd{$ELSE}TTessellationWinding.Odd{$ENDIF};
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); overload;

    // Draws a filled polygon with the given contours and the appropriate winding rules.
    procedure Polygon(const AContours: array of TPoint2f; const AContourLengths: array of Cardinal;
      const AColor: TIntColor; const AWindingRule: TTessellationWinding =
      {$IFDEF DELPHI_LEGACY}twOdd{$ELSE}TTessellationWinding.Odd{$ENDIF};
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); overload;

    { Draws a textured triangle multiplied with a color gradient specified by given positions, colors and
      texture coordinates (in range from 0 to 1). }
    procedure Triangle(const ATexture: TTexture; const AVertex1, AVertex2, AVertex3, ATexCoord1, ATexCoord2,
      ATexCoord3: TPoint2f; const AColor1, AColor2, AColor3: TIntColor;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); overload; inline;

    { Draws a textured triangle multiplied with a color gradient specified by given positions, colors and
      texture coordinates (in pixels). }
    procedure TriangleRegion(const ATexture: TTexture; const AVertex1, AVertex2, AVertex3, ATexCoord1, ATexCoord2,
      ATexCoord3: TPoint2f; const AColor1, AColor2, AColor3: TIntColor;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); inline;

    { Draws a textured quadrilateral multiplied with a color gradient specified by the given vertices, colors
      and texture coordinates (in range from 0 to 1). }
    procedure Quad(const ATexture: TTexture; const AVertices, ATexCoords: TQuad; const AColors: TColorRect;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); overload; inline;

    { Draws a textured quadrilateral multiplied with a color gradient specified by the given vertices, colors
      and texture coordinates (in pixels). }
    procedure QuadRegion(const ATexture: TTexture; const AVertices, ATexCoords: TQuad;
      const AColors: TColorRect; const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); inline;

    // Draws a textured rectangle with round corners and the given gradient and texture coordinates.
    procedure TexturedRoundRect(const ATexture: TTexture; const ARect: TFloatRect; const ATexCoords: TQuad;
      const AColors: TColorRect; const ARadius: Single; const ASteps: Integer;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); inline;

    { Draws a textured rectangle with round corners and the given gradient and texture
     coordinates (in pixels). }
    procedure TexturedRoundRectRegion(const ATexture: TTexture; const ARect: TFloatRect; const ATexCoords: TQuad;
      const AColors: TColorRect; const ARadius: Single; const ASteps: Integer;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); inline;

    /// Draws a textured quadrilateral from source portion of image atlas, multiplied by a color gradient.
    procedure QuadImage(const AImageAtlas: TImageAtlas; const AVertices: TQuad; const ARegionIndex: Integer;
      const AColors: TColorRect; const ASourceRect: TFloatRect; const AModifiers: TImageAttributes;
      const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); overload; inline;

    /// Draws a textured quadrilateral from source portion of image atlas, multiplied by a color gradient.
    procedure QuadImage(const AImageAtlas: TImageAtlas; const AVertices: TQuad; const ARegionIndex: Integer;
      const AColors: TColorRect; const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect); overload; inline;

    // Sets new canvas sampler state.
    function SetSamplerState(const ASamplerState: TCanvasSamplerState): Boolean; inline;

    // Retrieves current canvas sampler state (if such exists).
    function GetSamplerState(var ASamplerState: TCanvasSamplerState): Boolean; inline;

    // Resets any canvas sampler state that is currently set.
    procedure ResetSamplerState; inline;

    // Flushes the canvas by rendering primitives that are still in batch buffers.
    procedure Flush; inline;

    // Resets canvas internal resources and releases any non-critical memory buffers.
    procedure Reset; inline;

    // Resets canvas internal cache that is used for drawing different primitives.
    procedure ResetCache; inline;

    // Currently set clipping rectangle.
    property ClipRect: TIntRect read GetClipRect write SetClipRect;

    // 3D transformation matrix (includes World, View and Projection matrices).
    property Transform: TMatrix4f read GetTransform write SetTransform;

    { Number of batches that were rendered between current set of begin/end block. Drawing each individual
      batch involves some overhead, so when this parameter happens to be considerably high at some point,
      the rendering code should be revised for better grouping of images, shapes and blending types. }
    property BatchCount: Integer read GetBatchCount;

    // Currently set rendering attributes.
    property Attributes: TCanvasAttributes read GetAttributes write SetAttributes;

    // Currently set signed distance field parameters.
    property SignedDistanceField: TSignedDistanceField read GetSignedDistanceField
      write SetSignedDistanceField;

    // Pre-defined device rendering state configuration for specific canvas state.
    property ContextState: TCanvasContextState read GetContextState write SetContextState;
  end;

// Creates new instance of canvas.
function CanvasInit(const ADevice: TDevice): TCanvas; inline;

type
  // Pointer to @link(TTextRenderer).
  PTextRenderer = ^TTextRenderer;

  // Text renderer utility that can draw text on the canvas.
  TTextRenderer = object(TBaseObject)
  private
    function GetFontSettings: TFontSettings; inline;
    procedure SetFontSettings(const AFontSettings: TFontSettings); inline;
    function GetCanvas: TCanvas; inline;
  public
    // Releases the text renderer.
    procedure Free; inline;

    // Returns the logical dimensions that the given text will occupy when rendered.
    function Extent(const AText: Utf8String; const AModifiers: PTextRenderModifiers = nil): TPoint2f; inline;

    // Calculates the actual rectangle in pixels that the given text will occupy when rendered.
    function ExtentByPixels(const AText: Utf8String;
      const AModifiers: PTextRenderModifiers = nil): TFloatRect; inline;

    // Provides information regarding individual character position and sizes for the given text string when
    // rendered. This can be useful for components such as text edit box, for highlighting and selecting
    // different characters. The actual width and height of the text is, optionally, provided as well.
    // Returns actual number of elements returned. Calling this function with @link(ARects) set to @nil would
    // calculate how many elements need to be allocated.
    function Rects(const AText: Utf8String; const AExtent: PPoint2f; const ARects: PTextEntryRect;
      const AModifiers: PTextRenderModifiers = nil): Integer;

    // Draws text at the given position.
    procedure Draw(const APosition: TPoint2f; const AText: Utf8String; const AColors: TColorPair;
      const AAlpha: VectorFloat = 1.0; const AModifiers: PTextRenderModifiers = nil); inline;

    // Draws text at the given position with specific alignment.
    procedure DrawAligned(const APosition: TPoint2f; const AText: Utf8String; const AColors: TColorPair;
      const AHorizAlign, AVertAlign: TTextAlignment; const AAlpha: VectorFloat = 1.0;
      const AAlignToPixels: Boolean = True; const AModifiers: PTextRenderModifiers = nil); inline;

    // Draws text centered around the specified position.
    procedure DrawCentered(const APosition: TPoint2f; const AText: Utf8String; const AColors: TColorPair;
      const AAlpha: VectorFloat = 1.0; const AAlignToPixels: Boolean = True;
      const AModifiers: PTextRenderModifiers = nil); inline;

    // Draws text at the given position with specific alignment by actual visible pixels.
    procedure DrawAlignedByPixels(const APosition: TPoint2f; const AText: Utf8String;
      const AColors: TColorPair; const AHorizAlign, AVertAlign: TTextAlignment;
      const AAlpha: VectorFloat = 1.0; const AAlignToPixels: Boolean = True;
      const AModifiers: PTextRenderModifiers = nil); inline;

    // Draws text centered around the specified position by actual visible pixels.
    procedure DrawCenteredByPixels(const APosition: TPoint2f; const AText: Utf8String;
      const AColors: TColorPair; const AAlpha: VectorFloat = 1.0; const AAlignToPixels: Boolean = True;
      const AModifiers: PTextRenderModifiers = nil); inline;

    // Currently selected font settings.
    property FontSettings: TFontSettings read GetFontSettings write SetFontSettings;

    // Canvas associated with the text renderer.
    property Canvas: TCanvas read GetCanvas;
  end;

// Creates new instance of text renderer.
function TextRendererInit(const ACanvas: TCanvas; const ATextureSize: TPoint2i;
  const APixelFormat: TPixelFormat = {$IFDEF DELPHI_LEGACY}pfUnknown{$ELSE}TPixelFormat.Unknown{$ENDIF};
  const AMipMapping: Boolean = False): TTextRenderer; inline;

type
  // Pointer to @link(TGrapher).
  PGrapher = ^TGrapher;

  // 3D graph plotting module.
  TGrapher = object(TBaseObject)
  private
    function GetTransform: TMatrix4f; inline;
    procedure SetTransform(const ATransform: TMatrix4f); inline;
    function GetBatchCount: Integer; inline;
  public
    // Releases the 3D plotting grapher.
    procedure Free; inline;

    // Prepares grapher for rendering.
    function BeginScene: Boolean; inline;

    // Finishes grapher rendering.
    procedure EndScene; inline;

    { Draws points at their given vertices with their respective sizes ("w" component of each vertex),
      colors, angles and the appropriate shape in 3D space. }
    procedure Points(const AVertices: PVector4f; const AColors: PIntColor; const AAngles: PSingle;
      const AElementCount: Integer; const AShape: TPointShape); inline;

    // Draws lines at their given vertices, thicknesses ("w" component of each vertex), colors and indices.
    procedure Lines(const AVertices: PVector4f; const AColors: PIntColor; const AIndices: PCardinal;
      const AVertexCount, AIndexCount: Integer; const ACaps: TLineCaps); inline;

    // Draws a single point with the given parameters.
    procedure Point(const APosition: TVector3f; const AColor: TIntColor; const ASize: Single;
      const AShape: TPointShape; const AAngle: Single = 0.0); inline;

    // Draws a single line with the given parameters.
    procedure Line(const APosition1, APosition2: TVector3f; const AColor1, AColor2: TIntColor;
      const AThickness1, AThickness2: Single; const ACaps: TLineCaps =
      {$IFDEF DELPHI_LEGACY}lcButt{$ELSE}TLineCaps.Butt{$ENDIF}); inline;

    { Draws an arrow from origin to destination and the given parameters.
      @italic(targetSize) is the rendering surface size (required for proper arrow angle calculation). }
    procedure Arrow(const ATargetSize: TPoint2f; const AOrigin, ADestination: TVector3f;
      const AColor: TIntColor; const AThickness, ASize: Single;
      const ACaps: TLineCaps = {$IFDEF DELPHI_LEGACY}lcButt{$ELSE}TLineCaps.Butt{$ENDIF}); inline;

    // Draws lines around a bounding box for the given volume matrix.
    procedure BoundingBox(const AVolume: TMatrix4f; const AColor: TIntColor; const AThickness: Single;
      const ALength: Single = 0.5;
      const ACaps: TLineCaps = {$IFDEF DELPHI_LEGACY}lcButt{$ELSE}TLineCaps.Butt{$ENDIF}); inline;

    // Draws a line of dots with the given parameters.
    procedure DottedLine(const APosition1, APosition2: TVector3f; const AColor1, AColor2: TIntColor;
      const AThickness1, AThickness2, ASparsity: Single; const AShape: TPointShape =
      {$IFDEF DELPHI_LEGACY}psSquare{$ELSE}TPointShape.Square{$ENDIF};
      const AAngle: Single = 0.0); inline;

    { Flushes grapher cache and draws any pending primitives on the rendering surface. This can be useful to
      make sure that nothing remains in graph cache before changing any device states. }
    procedure Flush; inline;

    { Resets grapher internal resources and releases any non-critical memory buffers. This can reduce overall
      memory consumption, when switching from one complex visual scene to another, to allow graph to "adapt"
      to a new scene from a fresh start. }
    procedure Reset; inline;

    // Currently set 3D transformation (world / view / projection) matrix.
    property Transform: TMatrix4f read GetTransform write SetTransform;

    { Number of batches that were sent to rendering pipeline. Drawing each individual batch involves some
      overhead, which is implementation-specific. If this parameter happens to be considerably high at some
      point, the rendering code should be revised for better grouping of plotting primitives. }
    property BatchCount: Integer read GetBatchCount;
  end;

// Creates new instance of 3D plotting grapher.
function GrapherInit(const ADevice: TDevice): TGrapher; inline;

type
  // Pointer to @link(TGaussianBlur).
  PGaussianBlur = ^TGaussianBlur;

  // Gaussian Blur module.
  TGaussianBlur = object(TBaseObject)
  private
    function GetSamples: Integer; inline;
    procedure SetSamples(const ASamples: Integer); inline;
    function GetSigma: Single; inline;
    procedure SetSigma(const ASigma: Single); inline;
  public
    // Releases the gaussian blur module.
    procedure Free; inline;

    // Applies a horizontal blur from source to intermediary texture, then a vertical blur from intermediary
    // to destination texture. Both destination and intermediary textures must have drawable attribute.
    // If source texture has drawable attribute, then it can be passed as destination texture as well.
    function Update(const ADestinationTexture, AIntermediaryTexture,
      ASourceTexture: TTexture): Boolean; inline;

    // Number of samples in the kernel.
    property Samples: Integer read GetSamples write SetSamples;

    // Value of sigma (affects blur strength).
    property Sigma: Single read GetSigma write SetSigma;
  end;

// Creates a new instance of gaussian blur module.
function GaussianBlurInit(const ADevice: TDevice): TGaussianBlur; inline;

type
  // Pointer to @link(TGaussianHighlight).
  PGaussianHighlight = ^TGaussianHighlight;

  // Gaussian Highlight module.
  TGaussianHighlight = object(TBaseObject)
  private
    function GetParameters: TGaussianHighlightParameters; inline;
    procedure SetParameters(const AParameters: TGaussianHighlightParameters); inline;
    function GetRectangle: TFloatRect; inline;
    procedure SetRectangle(const ARectangle: TFloatRect); inline;
  public
    // Releases the gaussian blur module.
    procedure Free; inline;

    // Applies a horizontal highlight from source to intermediary texture, then a vertical highlight from
    // intermediary to destination texture. Both destination and intermediary textures must have drawable
    // attribute. If source texture has drawable attribute, then it can be passed as destination texture
    // as well.
    function Update(const ADestinationTexture, AIntermediaryTexture,
      ASourceTexture: TTexture): Boolean; inline;

    // Physical parameters of gaussian highlight's blur.
    property Parameters: TGaussianHighlightParameters read GetParameters write SetParameters;

    // Rectangle, where gaussian highlight is applied.
    property Rectangle: TFloatRect read GetRectangle write SetRectangle;
  end;

// Creates a new instance of gaussian highlight module.
function GaussianHighlightInit(const ADevice: TDevice): TGaussianHighlight; inline;

type
  // Pointer to @link(TSceneTexture).
  PSceneTexture = ^TSceneTexture;

  // Container for 3D scene rendering textures.
  TSceneTexture = object(TBaseObject)
  private
    function GetSize: TPoint2i; inline;
    function GetSamples: Integer; inline;
    function GetRendering: Boolean; inline;
  public
    // Releases the texture container.
    procedure Free; inline;

    // Returns texture for the given type from the integrated list.
    function Retrieve(const ATextureType: TSceneTextureType): TTexture; inline;

    // Begins rendering to the texture.
    function BeginScene: Boolean; inline;

    // Ends rendering to the texture.
    procedure EndScene; inline;

    // The size of the integrated textures.
    property Size: TPoint2i read GetSize;

    // Number of samples used in rendering.
    property Samples: Integer read GetSamples;

    { Indicates that the rendering is currently taking place (inside @link(BeginScene) / @link(EndScene)
      block). }
    property Rendering: Boolean read GetRendering;
  end;

  // Pointer to @link(TShadowTexture).
  PShadowTexture = ^TShadowTexture;

  // Container for rendering shadow map textures.
  TShadowTexture = object(TSceneTexture)
  private
    function GetTechnique: TShadowTechnique; inline;
    function GetParameters: TShadowParameters; inline;
    procedure SetParameters(const AParameters: TShadowParameters); inline;
  public
    // Returns the most recent shadow map texture handle.
    function Retrieve: TTexture; inline;

    // Applies filtering to the texture.
    function Filter: Boolean; inline;

    // Current shadow rendering technique.
    property Technique: TShadowTechnique read GetTechnique;

    // Shadow rendering parameters.
    property Parameters: TShadowParameters read GetParameters write SetParameters;
  end;

// Creates a new instance of shadow map container.
function ShadowTextureInit(const ADevice: TDevice; const AShadowTechnique: TShadowTechnique;
  const ASize: TPoint2i; const ASamples: Integer = 8;
  const ADepthReversed: Boolean = False): TShadowTexture; inline;

type
  // Container for 3D scene rendering textures.
  TModelTexture = object(TSceneTexture)
  private
    function GetFormat: TPixelFormat; inline;
    function GetDepthStencil: TPixelFormat; inline;
  public
    // Updates the size of integrated textures and buffers.
    function SetSize(const ASize: TPoint2i): Boolean; inline;

    // Renders the texture on currently active target surface.
    function Present: Boolean; inline;

    // Format used for color render targets.
    property Format: TPixelFormat read GetFormat;

    // Format used for depth/stencil buffers.
    property DepthStencil: TPixelFormat read GetDepthStencil;
  end;

// Creates a new instance of 3D scene rendering textures.
function ModelTextureInit(const ADevice: TDevice; const ASize: TPoint2i;
  const AFormat: TPixelFormat = {$IFDEF DELPHI_LEGACY}pfUnknown{$ELSE}TPixelFormat.Unknown{$ENDIF};
  const ADepthStencil: TPixelFormat = {$IFDEF DELPHI_LEGACY}pfUnknown{$ELSE}TPixelFormat.Unknown{$ENDIF};
  const ASamples: Integer = 8): TModelTexture; inline;

type
  // Container for order-independent transparency (OIT) textures.
  TGlassTexture = object(TSceneTexture)
  private
    function GetTechnique: TGlassTechnique; inline;
    function GetAttributes: TSceneAttributes; inline;
    function GetFormat: TPixelFormat; inline;
    function GetBackground: TFloatColor; inline;
    procedure SetBackground(const ABackground: TFloatColor); inline;
  public
    // Updates the size of integrated textures and buffers.
    function SetSize(const ASize: TPoint2i): Boolean; inline;

    /// Sets one of important scene textures that will be used during rendering.
    /// This must be called outside of @italic(BeginScene) and @italic(EndScene) block.
    procedure SetInput(const ATextureType: TSceneTextureType;
      const ATexture: TTexture); inline;

    // Renders the texture on currently active target surface.
    function Present: Boolean; inline;

    // Technique used for rendering Order-Independent Transparency (OIT).
    property Technique: TGlassTechnique read GetTechnique;

    // Rendering specialization attributes.
    property Attributes: TSceneAttributes read GetAttributes;

    // Format used for color accumulation render targets.
    property Format: TPixelFormat read GetFormat;

    // Background color for composition (when @link(SceneCompose) is not specified).
    property Background: TFloatColor read GetBackground write SetBackground;
  end;

// Creates a new instance of order-independent transparency (OIT) texture container.
function GlassTextureInit(const ADevice: TDevice; const ATechnique: TGlassTechnique; const ASize: TPoint2i;
  const AAttributes: TSceneAttributes = SceneCompose;
  const AFormat: TPixelFormat = {$IFDEF DELPHI_LEGACY}pfUnknown{$ELSE}TPixelFormat.Unknown{$ENDIF};
  const ASamples: Integer = 8): TGlassTexture; inline;

type
  // Pointer to @link(TScene).
  PScene = ^TScene;

  // 3D rendering scene.
  TScene = object(TBaseObject)
  private
    function GetShadowTechnique: TShadowTechnique; inline;
    function GetShadowParameters: TShadowParameters; inline;
    procedure SetShadowParameters(const AParameters: TShadowParameters); inline;
    function GetAttributes: TSceneAttributes; inline;
    function GetWorld: TMatrix4f; inline;
    procedure SetWorld(const AWorld: TMatrix4f); inline;
    function GetView: TMatrix4f; inline;
    procedure SetView(const AView: TMatrix4f); inline;
    function GetProjection: TMatrix4f; inline;
    procedure SetProjection(const AProjection: TMatrix4f); inline;
    function GetLightView: TMatrix4f; inline;
    procedure SetLightView(const ALightView: TMatrix4f); inline;
    function GetLightProjection: TMatrix4f; inline;
    procedure SetLightProjection(const ALightProjection: TMatrix4f); inline;
    function GetSceneProgram: TProgram; inline;
    function GetRendering: Boolean; inline;
  public
    // Releases the scene module.
    procedure Free; inline;

    // Supplies instance information to the pipeline. The number of instances may not exceed 128.
    function Instances(const ATransforms: TMatrix4fArray; const AColors: TFloatColorArray): Boolean; overload;

    // Supplies instance information to the pipeline. The number of instances may not exceed 128.
    function Instances(const ATransforms: array of TMatrix4f;
      const AColors: array of TFloatColor): Boolean; overload;

    // Supplies instance information to the pipeline. The number of instances may not exceed 128.
    function Instances(const ATransforms: PMatrix4f; const AColors: PFloatColor;
      const AInstanceCount: Integer): Boolean; overload;

    // Activates the appropriate shader program and begins rendering the scene.
    function BeginScene: Boolean; inline;

    // Finishes rendering the scene and deactivates previously activated shader program.
    procedure EndScene; inline;

    // Current shadow rendering technique.
    property ShadowTechnique: TShadowTechnique read GetShadowTechnique;

    // Parameters that define how shadows are rendered.
    property ShadowParameters: TShadowParameters read GetShadowParameters write SetShadowParameters;

    // Current rendering specialization attributes.
    property Attributes: TSceneAttributes read GetAttributes;

    // Current object World matrix.
    property World: TMatrix4f read GetWorld write SetWorld;

    // Current View matrix.
    property View: TMatrix4f read GetView write SetView;

    // Current Projection matrix.
    property Projection: TMatrix4f read GetProjection write SetProjection;

    // Current Light View matrix.
    property LightView: TMatrix4f read GetLightView write SetLightView;

    // Current Light Projection matrix.
    property LightProjection: TMatrix4f read GetLightProjection write SetLightProjection;

    // Currently active shader program.
    property SceneProgram: TProgram read GetSceneProgram;

    { Indicates that the rendering is currently taking place (inside @link(BeginScene) /
      @link(EndScene) block). }
    property Rendering: Boolean read GetRendering;
  end;

// Creates a new instance of shadow map renderer.
function ShadowSceneInit(const ADevice: TDevice; const ATechnique: TShadowTechnique;
  const AAttributes: TSceneAttributes; const AVertexElements: array of TVertexElement): TScene; overload;

// Creates a new instance of shadow map renderer.
function ShadowSceneInit(const ADevice: TDevice; const ATechnique: TShadowTechnique;
  const AAttributes: TSceneAttributes; const AVertexElements: TVertexElements): TScene; overload;

type
  // Pointer to @link(TModelScene).
  PModelScene = ^TModelScene;

  // 3D-lit rendering scene.
  TModelScene = object(TScene)
  private
    function GetLight(const AIndex: Integer): TSceneLight; inline;
    procedure SetLight(const AIndex: Integer; const ALight: TSceneLight); inline;
    function GetTexture(const ATextureType: TSceneTextureType): TTexture; inline;
    procedure SetTexture(const ATextureType: TSceneTextureType; const ATexture: TTexture); inline;
  public
    // Combined light and material parameters.
    property Light[const Index: Integer]: TSceneLight read GetLight write SetLight;

    // Important scene textures that will be used during rendering.
    // Changing these must be done outside of @link(TScene.BeginScene) / @link(TScene.EndScene) block.
    property Texture[const ATextureType: TSceneTextureType]: TTexture read GetTexture write SetTexture;
  end;

// Creates a new instance of 3D-lit rendering scene.
function ModelSceneInit(const ADevice: TDevice; const AShadowTechnique: TShadowTechnique;
  const AAttributes: TSceneAttributes;
  const AVertexElements: array of TVertexElement): TModelScene; overload;

// Creates a new instance of 3D-lit rendering scene.
function ModelSceneInit(const ADevice: TDevice; const AShadowTechnique: TShadowTechnique;
  const AAttributes: TSceneAttributes;
  const AVertexElements: TVertexElements): TModelScene; overload;

type
  // 3D rendering scene with Order-Independent Transparency (OIT).
  TGlassScene = object(TModelScene)
  private
    function GetTechnique: TGlassTechnique; inline;
    function GetSamples: Integer; inline;
    function GetAssociate: TGlassTexture; inline;
    procedure SetAssociate(const AAssociate: TGlassTexture); inline;
  public
    // Indicates whether another iteration is possible or not.
    function Iterate: Boolean; inline;

    // Technique used for rendering Order-Independent Transparency (OIT).
    property Technique: TGlassTechnique read GetTechnique;

    // Number of samples used for rendering Order-Independent Transparency (OIT).
    property Samples: Integer read GetSamples;

    // Currently associated texture container.
    property Associate: TGlassTexture read GetAssociate write SetAssociate;
  end;

// Creates a new instance of 3D rendering scene with Order-Independent Transparency (OIT).
function GlassSceneInit(const ADevice: TDevice; const AGlassTechnique: TGlassTechnique;
  const AShadowTechnique: TShadowTechnique; const AAttributes: TSceneAttributes; const ASamples: Integer;
  const AVertexElements: array of TVertexElement): TGlassScene; overload;

// Creates a new instance of 3D rendering scene with Order-Independent Transparency (OIT).
function GlassSceneInit(const ADevice: TDevice; const AGlassTechnique: TGlassTechnique;
  const AShadowTechnique: TShadowTechnique; const AAttributes: TSceneAttributes;
  const ASamples: Integer; const AVertexElements: TVertexElements): TGlassScene; overload;

type
  // Pointer to @link(TVolumeSurfaceShadowScene).
  PVolumeSurfaceShadowScene = ^TVolumeSurfaceShadowScene;

  // Shadow map rendering scene for 3D volume surfaces.
  TVolumeSurfaceShadowScene = object(TScene)
  private
    function GetParameters: TVolumeSurfaceParameters; inline;
    procedure SetParameters(const AParameters: TVolumeSurfaceParameters); inline;
  public
    { Sets the 3D volume surface texture which should be used to render the field.
      This must be called outside of @link(TScene.BeginScene) / @link(TScene.EndScene) block. }
    procedure SetTexture(const ATexture: TTexture); inline;

    /// Executes the rendering technique.
    procedure Execute; inline;

    /// Current 3D volume surface characteristics.
    property Parameters: TVolumeSurfaceParameters read GetParameters write SetParameters;
  end;

// Creates a new instance of shadow map renderer for 3D volume surfaces.
function VolumeSurfaceShadowSceneInit(const ADevice: TDevice; const ATechnique: TShadowTechnique;
  const AAttributes: TSceneAttributes): TVolumeSurfaceShadowScene; inline;

type
  // Pointer to @link(TVolumeSurfaceScene).
  PVolumeSurfaceScene = ^TVolumeSurfaceScene;

  // 3D-lit volume surface rendering scene.
  TVolumeSurfaceScene = object(TModelScene)
  private
    function GetParameters: TVolumeSurfaceParameters; inline;
    procedure SetParameters(const AParameters: TVolumeSurfaceParameters); inline;
  public
    { Sets the 3D volume surface texture which should be used to render the field.
      This must be called outside of @link(TScene.BeginScene) / @link(TScene.EndScene) block. }
    procedure SetTexture(const ATexture: TTexture); inline;

    /// Executes the rendering technique.
    procedure Execute; inline;

    /// Current 3D volume surface characteristics.
    property Parameters: TVolumeSurfaceParameters read GetParameters write SetParameters;
  end;

// Creates a new instance of 3D-lit volume surface renderer.
function VolumeSurfaceSceneInit(const ADevice: TDevice; const AShadowTechnique: TShadowTechnique;
  const AAttributes: TSceneAttributes): TVolumeSurfaceScene; inline;

type
  // Pointer to @link(TObjectCamera).
  PObjectCamera = ^TObjectCamera;

  // 3D object manipulation camera.
  TObjectCamera = object(TBaseObject)
  private
    function GetPosition: TVector3f; inline;
    procedure SetPosition(const APosition: TVector3f); inline;
    function GetRotation: TVector3f; inline;
    procedure SetRotation(const ARotation: TVector3f); inline;
    function GetDistance: Single; inline;
    procedure SetDistance(const ADistance: Single); inline;
    function GetConstraints: TObjectCameraConstraints; inline;
    procedure SetConstraints(const AConstraints: TObjectCameraConstraints); inline;
    function GetSensitivity: TObjectCameraSensitivity; inline;
    procedure SetSensitivity(const ASensitivity: TObjectCameraSensitivity); inline;
    function GetView: TMatrix4f; inline;
  public
    // Releases the object camera.
    procedure Free; inline;

    // Start camera rotation at the given absolute position in [0, 1] range.
    procedure StartRotation(const ATouch: TPoint2f); inline;

    // Start camera movement at the given absolute position in [0, 1] range.
    procedure StartMovement(const ATouch: TPoint2f); inline;

    // Updates camera position and/or rotation depending on current state.
    function Update(const ATouch: TPoint2f): Boolean; inline;

    // Stops camera rotation and/or movement.
    function Stop: Boolean; inline;

    // Changes camera's zoom by adjusting its distance and position around the specified point.
    procedure Zoom(const APosition, ASize: TPoint2f; const AProjection: TMatrix4f;
      const ADelta: Single); inline;

    // Adjusts camera position while performing orthographic zoom.
    procedure ZoomOrtho(const APosition, ASize: TPoint2f; const ADistance, AAdjustedDistance: Single;
      const AProjection, AAdjustedProjection: TMatrix4f); inline;

    // Position at which the camera is looking at.
    property Position: TVector3f read GetPosition write SetPosition;

    // Rotation that defines overall orientation of the camera.
    property Rotation: TVector3f read GetRotation write SetRotation;

    // The distance between camera's position and its view source.
    property Distance: Single read GetDistance write SetDistance;

    // Current 3D camera constraints.
    property Constraints: TObjectCameraConstraints read GetConstraints write SetConstraints;

    // Current 3D camera sensitivity.
    property Sensitivity: TObjectCameraSensitivity read GetSensitivity write SetSensitivity;

    // Camera's resulting 3D view matrix.
    property View: TMatrix4f read GetView;
  end;

// Creates new instance of object camera.
function ObjectCameraInit(const APosition: TVector3f; const ARotation: TVector3f;
  const ADistance: Single = 100.0): TObjectCamera; overload; inline;

// Creates new instance of object camera.
function ObjectCameraInit(const ADistance: Single = 100.0): TObjectCamera; overload; inline;

type
  // 3D mesh voxel representation object.
  TMeshVoxel = object(TBaseObject)
  public
    // Releases 3D mesh voxel representation.
    procedure Free; inline;

    // Returns position and size of 3D mesh voxel representation.
    procedure Extents(out APosition, ASize: TVector3f); inline;

    // Visualizes 3D mesh voxel representation by invoking callback function for each cube.
    procedure Visualize(const AVisualizeFunc: TMeshVoxelVisualizeFunc; const AUser: Pointer); inline;
  end;

// Creates 3D mesh voxel representation by loading it from an external file.
function MeshVoxelInitFromFile(const AFileName: Utf8String): TMeshVoxel; inline;

// Creates 3D mesh voxel representation by loading it from a file in memory.
function MeshVoxelInitFromMemory(const ABuffer: Pointer; const ASize: Cardinal): TMeshVoxel; inline;

type
  // High-level wrapper around a rendereable and pickable mesh in 3D scene.
  TSceneMesh = object(TBaseObject)
  private
    function GetName: Utf8String; inline;
    function GetPayload: TObjectPayload; inline;
    function GetModel: TMeshModel; inline;
    function GetVoxel: TMeshVoxel; inline;
    function GetTags: TMeshMetaTags; inline;
    function GetMinBounds: TVector3f; inline;
    function GetMaxBounds: TVector3f; inline;
    function GetScale: Single; inline;
    function GetSize: TVector3f; inline;
  public
    // Name of the mesh (case-insensitive).
    property Name: Utf8String read GetName;

    // Payload associated with the mesh.
    property Payload: TObjectPayload read GetPayload;

    // Rendereable mesh model.
    property Model: TMeshModel read GetModel;

    // Voxel representation of a 3D mesh.
    property Voxel: TMeshVoxel read GetVoxel;

    // Meta tags that represent relevant regions in a 3D mesh.
    property Tags: TMeshMetaTags read GetTags;

    // Minimum mesh boundaries.
    property MinBounds: TVector3f read GetMinBounds;

    // Maximum mesh boundaries.
    property MaxBounds: TVector3f read GetMaxBounds;

    // Scale of the mesh.
    property Scale: Single read GetScale;

    // Size of the mesh.
    property Size: TVector3f read GetSize;
  end;

  // Enumerator for high-level scene mesh container.
  TSceneMeshesEnumerator = record
  private
    FHandle: TLibraryClassHandle;
    FCurrent: Integer;

    function GetCurrent: TSceneMesh; inline;
  public
    // Creates new scene meshes enumerator.
    constructor Create(const AHandle: TLibraryClassHandle);

    // Moves enumerator to the next element.
    function MoveNext: Boolean; inline;

    // Current enumerator's element.
    property Current: TSceneMesh read GetCurrent;
  end;

  // Container for high-level wrappers of rendereable and pickable objects in 3D scene.
  TSceneMeshes = object(TBaseObject)
  private
    function GetDevice: TDevice; inline;
    function GetCount: Integer; inline;
    function GetMesh(const AIndex: Integer): TSceneMesh; inline;
    function GetMeshByName(const AName: Utf8String): TSceneMesh; inline;
    function GetPayload(const APayload: TObjectPayload): TSceneMesh; inline;
  public
    // Releases the container.
    procedure Free; inline;

    // Creates new scene mesh with the given parameters, retaining ownership of the provided resources.
    function Add(const AName: Utf8String; var AModel: TMeshModel; var AVoxel: TMeshVoxel;
      var ATags: TMeshMetaTags; const APayload: TObjectPayload; const AMinBounds, AMaxBounds: TVector3f;
      const AScale: Single): TSceneMesh; inline;

    // Creates new mesh loading contents from the given binary file.
    function AddFromBinaryFile(const AName, AFileName: Utf8String; const APayload: TObjectPayload;
      const AVertexElements: TVertexElements; const AChannel: Cardinal = 0;
      const AScale: Single = 1.0): TSceneMesh; overload;

    // Creates new mesh loading contents from the given binary file.
    function AddFromBinaryFile(const AName, AFileName: Utf8String; const APayload: TObjectPayload;
      const AVertexElements: array of TVertexElement; const AChannel: Cardinal = 0;
      const AScale: Single = 1.0): TSceneMesh; overload;

    // Searches through the list of meshes for the given one and returns its index.
    // Returns -1 if the given mesh was not encountered.
    function Find(const ASceneMesh: TSceneMesh): Integer; inline;

    // Erases the given mesh.
    procedure Erase(const ASceneMesh: TSceneMesh); inline;

    // Removes all existing meshes.
    procedure Clear; inline;

    // Returns enumerator for the container.
    function GetEnumerator: TSceneMeshesEnumerator; inline;

    // Device associated with the container.
    property Device: TDevice read GetDevice;

    // Number of existing meshes.
    property Count: Integer read GetCount;

    // Returns mesh with the given index.
    property Meshes[const AIndex: Integer]: TSceneMesh read GetMesh; default;

    // Returns mesh with the given name (case-insensitive) or a null object if no mesh with such name exists.
    property Mesh[const AName: Utf8String]: TSceneMesh read GetMeshByName;

    // Returns a mesh that corresponds to the given payload or a null object if such doesn't exist.
    property Payloads[const APayload: TObjectPayload]: TSceneMesh read GetPayload;
  end;

// Creates a new instance of scene mesh container associated with the given device.
function SceneMeshesInit(const ADevice: TDevice): TSceneMeshes; inline;

type
  // Pointer to @link(TObjectModel).
  PObjectModel = ^TObjectModel;

  // Object that represents a 3D model in the scene.
  TObjectModel = object(TBaseObject)
  private
    function GetPayload: TObjectPayload; inline;
    function GetName: Utf8String; inline;
    procedure SetName(const AName: Utf8String); inline;
    function GetOwner: TBaseObject; inline;
    function GetParent: TObjectModel; inline;
    procedure SetParent(const AParent: TObjectModel); inline;
    function GetVoxel: TMeshVoxel; inline;
    procedure SetVoxel(const AMeshVoxel: TMeshVoxel); inline;
    function GetMesh: TSceneMesh; inline;
    procedure SetMesh(const AMesh: TSceneMesh); inline;
    function GetMeshName: Utf8String; inline;
    procedure SetMeshName(const AName: Utf8String); inline;
    function GetTransform(const ATransform: TModelTransform): TMatrix4f; inline;
    procedure SetTransform(const ATransform: TModelTransform; const AMatrix: TMatrix4f); inline;
    function GetPosition: TVector3f; inline;
    function GetDepthBias: Single; inline;
    procedure SetDepthBias(const ADepthBias: Single); inline;
    function GetAttributes: TModelAttributes; inline;
    procedure SetAttributes(const AAttributes: TModelAttributes); inline;
    function GetOrderIndex: Integer; inline;
    procedure SetOrderIndex(const AOrderIndex: Integer); inline;
    function GetLayers: UInt64; inline;
    procedure SetLayers(const ALayers: UInt64); inline;
    function GetChildCount: Integer; inline;
    function GetChild(const AIndex: Integer): TObjectModel; inline;
    function GetSize: TVector3f; inline;
    procedure SetSize(const ASize: TVector3f); inline;
    function GetAligns: TMeshAligns; inline;
    procedure SetAligns(const AAligns: TMeshAligns); inline;
  public
    // Marks local transform, volume and position as dirty, including children.
    procedure Invalidate; inline;

    // Object model's payload.
    property Payload: TObjectPayload read GetPayload;

    // Object's name (case-insensitive).
    property Name: Utf8String read GetName write SetName;

    // Object model's owner controller (must be typecast to TObjectModels).
    property Owner: TBaseObject read GetOwner;

    // Object model's parent.
    property Parent: TObjectModel read GetParent write SetParent;

    // Object model's voxel representation.
    property Voxel: TMeshVoxel read GetVoxel write SetVoxel;

    // Object's rendereable mesh.
    property Mesh: TSceneMesh read GetMesh write SetMesh;

    // Name of object's rendereable mesh.
    property MeshName: Utf8String read GetMeshName write SetMeshName;

    // Current object model's transforms.
    property Transform[const ATransform: TModelTransform]: TMatrix4f read GetTransform write SetTransform;

    // Current object position in world/view space.
    property Position: TVector3f read GetPosition;

    // Current depth bias, which is added to Z position (in world/view space).
    property DepthBias: Single read GetDepthBias write SetDepthBias;

    // Current object model's attributes.
    property Attributes: TModelAttributes read GetAttributes write SetAttributes;

    // Current object model's priority order index.
    property OrderIndex: Integer read GetOrderIndex write SetOrderIndex;

    { Layers on which the object is visible.
      Note: If no layer bits are set, then object would appear visible on all layers. }
    property Layers: UInt64 read GetLayers write SetLayers;

    // Number of object model's children.
    property ChildCount: Integer read GetChildCount;

    // Returns a particular object model's child with a zero-based index.
    property Child[const AIndex: Integer]: TObjectModel read GetChild; default;

    // Object's size.
    property Size: TVector3f read GetSize write SetSize;

    // Mesh alignments.
    property Aligns: TMeshAligns read GetAligns write SetAligns;
  end;

  // Pointer to @link(TObjectModels).
  PObjectModels = ^TObjectModels;

  // Enumerator for 3D object container that goes through ordered list.
  TObjectModelsEnumerator = record
  private
    FHandle: TLibraryClassHandle;
    FCurrent: Integer;

    function GetCurrent: TObjectModel; inline;
  public
    // Creates new object model enumerator.
    constructor Create(const AHandle: TLibraryClassHandle);

    // Moves enumerator to the next element.
    function MoveNext: Boolean; inline;

    // Current enumerator's element.
    property Current: TObjectModel read GetCurrent;
  end;

  // Container and manager for working with multiple 3D scene objects.
  TObjectModels = object(TBaseObject)
  private
    function GetMeshes: TSceneMeshes; inline;
    function GetView: TMatrix4f; inline;
    procedure SetView(const AView: TMatrix4f); inline;
    function GetCount: Integer; inline;
    function GetItem(const AIndex: Integer): TObjectModel; inline;
    function GetOrderCount: Integer; inline;
    function GetOrder(const AIndex: Integer): TObjectModel; inline;
    function GetPayload(const APayload: TObjectPayload): TObjectModel; inline;
    function GetObject(const AName: Utf8String): TObjectModel; inline;
  public
    // Releases the given 3D object model controller instance.
    procedure Free; inline;

    // Creates a new object with the given name and/or payload association.
    function Add(const AName: Utf8String = '';
      const APayload: TObjectPayload = nil): TObjectModel; inline;

    // Searches the list linearly for the given object and returns its index.
    function Find(const AObjectModel: TObjectModel): Integer; overload; inline;

    // Removes the given object.
    procedure Erase(const AObjectModel: TObjectModel); inline;

    // Removes all objects.
    procedure Clear;

    { Updates the list of objects ordered by depth, for selection and rendering.
      If no layer bits are set, then objects from all layers are visible. }
    function Update(const ALayers: UInt64 = 0): Boolean; inline;

    // Sorts the list of objects using a predefined comparison function.
    procedure Sort(const ACompare: TObjectModelCompare =
      {$IFDEF DELPHI_LEGACY}omcDepth{$ELSE}TObjectModelCompare.Depth{$ENDIF}); overload; inline;

    // Sorts the list of objects using a custom comparison function.
    procedure Sort(const ACompareFunc: TObjectModelCompareFunc; const AUser: Pointer = nil); overload; inline;

    { Performs object selection either through object's voxel hierarchy if such is present and if not,
      through ray-volume intersection. Objects are tested from front to back and the first object to
      intersect is picked. }
    function Select(const APointer: TRay; const ADistance: PSingle = nil;
      const ATestsCount: PInteger = nil): TObjectModel; overload; inline;

    { Performs object selection either through object's voxel hierarchy if such is present and if not,
      through ray-volume intersection. All objects are tested and the object with closest intersection is
      finally chosen. }
    function MultiSelect(const APointer: TRay; const ADistance: PSingle = nil;
      const ATestsCount: PInteger = nil): TObjectModel; overload; inline;

    // Returns enumerator for ordered 3D object model list.
    function GetEnumerator: TObjectModelsEnumerator; inline;

    // Associated mesh container.
    property Meshes: TSceneMeshes read GetMeshes;

    // Current 3D object model controller's view matrix.
    property View: TMatrix4f read GetView write SetView;

    // Number of existing objects in 3D object model controller.
    property Count: Integer read GetCount;

    // Returns a particular object with a zero-based index.
    property Items[const AIndex: Integer]: TObjectModel read GetItem; default;

    // The size of 3D object model controller's ordered list.
    property OrderCount: Integer read GetOrderCount;

    // A particular object with a zero-based index from controller's ordered list.
    property Order[const AIndex: Integer]: TObjectModel read GetOrder;

    // Returns object reference that corresponds to the given payload in 3D object model controller.
    property Payload[const APayload: TObjectPayload]: TObjectModel read GetPayload;

    // Returns object with the given name or a null object, if such doesn't exist.
    property Objects[const AName: Utf8String]: TObjectModel read GetObject;
  end;

// Creates a new instance of 3D object model controller.
function ObjectModelsInit(const AMeshes: TSceneMeshes): TObjectModels; inline;

type
  // Forward declaration of TTimer.
  TTimer = class;

  // Type of event that is invoked by the timer class.
  TTimerEvent = procedure(const ATimer: TTimer) of object;

  // Multimedia timer that can accurately calculate latency, frame rate and provide time-based processing.
  TTimer = class(TBaseClass)
  private
    FOnTimer: TTimerEvent;
    FOnProcess: TTimerEvent;

    function GetSpeed: Double; inline;
    procedure SetSpeed(const ASpeed: Double); inline;
    function GetMaxFrameRate: Integer; inline;
    procedure SetMaxFrameRate(const AMaxFrameRate: Integer); inline;
    function GetFrameRate: Integer; inline;
    function GetLatency: UInt64; inline;
    procedure UpdateEvents;
    procedure SetOnTimer(const AOnTimer: TTimerEvent);
    procedure SetOnProcess(const AOnProcess: TTimerEvent);
  public
    // Creates new instance of multimedia timer.
    constructor Create;

    // Releases the multimedia timer.
    destructor Destroy; override;

    { Executes one iteration of multimedia timer. This method should be called as periodically and as fast as
      possible from within the main application for timer to properly operate. It can be either called from
      message queue processing loop when the queue is empty when, from an idle event or continuously from
      another thread (but any access to this object should be from the same thread).
        @param(AActive Determines whether the timer should do internal processing and invoke events. Some
        minimal internal processing may still occur independently of this value.)
        @param(AAllowSleep Determines whether the timer can put current thread into sleep to ensure maximum
        target frame rate or during inactive state (when @italic(AActive) is @false).) }
    procedure Execute(const AActive: Boolean = True; const AAllowSleep: Boolean = True); inline;

    { Invokes none, one or more calls to @link(OnProcess) event depending on accumulation counter. This
      method should only be called from within @link(OnTimer) event to do constant object movement and
      animation control. Each time this method is called, @link(OnProcess) event may (or may not) be invoked,
      depending on the current rendering frame rate and the desired processing speed.
        @param(ASingleCallOnly If this is set to @true, it will prevent the timer from trying to fix
        situations where the rendering speed is slower than the processing speed (i.e. frame rate is lower
        than speed). Therefore, faster rendering produces constant speed, while slower rendering slows the
        processing down. This is particularly useful for dedicated servers that do no rendering but only
        processing; in this case, the processing cannot be technically any faster than it already is.) }
    procedure Process(const ASingleCallOnly: Boolean = False); inline;

    { Resets internal structures of the timer and restarts the timing calculations. This can be useful when
      a very time-consuming task was executed inside @link(OnTimer) event, that is not part of rendering and
      was expected to take longer. Normally, it would stall the timer making it think that the processing
      takes too long or the rendering is too slow; calling this method will tell the timer that it should
      ignore the situation and prevent the stall. }
    procedure Reset; inline;

    { Multimedia timer processing speed (in terms of frames per second). The value of this property affects
      what is returned by @link(Latency) and has direct impact on occurence rate of @link(OnProcess) events.
      The speed should be within reasonable limits, between 1 and preferably no more than 1000
      (default is 60). }
    property Speed: Double read GetSpeed write SetSpeed;

    { Maximum allowed frame rate at which multimedia timer event should be invoked. This value is an
      approximate and the resulting frame rate may be quite different (the resolution can be as low as
      10 ms). It should be used with reasonable values to prevent the application from using 100% of CPU and
      GPU with unnecessarily high frame rates such as 1000 FPS. @link(MaxFrameRate) should be between 1 and
      reasonably high maximum value (default is 65535). }
    property MaxFrameRate: Integer read GetMaxFrameRate write SetMaxFrameRate;

    { Calculated average frame rate in frames per second. This value is updated roughly two times per second
      and can only be used for informative purposes (e.g. displaying frame rate in the application).
      For precise real-time indications it is recommended to use @link(Latency). }
    property FrameRate: Integer read GetFrameRate;

    { Time (in microseconds) calculated between previous and current frames. This can be a direct indication
      of rendering performance as it tells how much time it took to render (and possibly, process) the frame. }
    property Latency: UInt64 read GetLatency;

    { This event is invoked when @link(Execute) is called (and @italic(AActive) is set to @true), In this
      event, it is possible to perform the rendering and other time-independent tasks. During this event, at
      some location it is recommended to call @link(Process) method, which will invoke @link(OnProcess) event
      for constant object movement and animation control. The idea is to render graphics as fast as possible
      while moving objects and controlling animation at constant speed. }
    property OnTimer: TTimerEvent read FOnTimer write SetOnTimer;

    { This event is invoked when @link(Process) method is called inside @link(OnTimer) event. During this
      event all constant object movement and animation control should be performed. This event can occur more
      than once for each call to @link(Process) or may not occur, depending on the current @link(FrameRate)
      and @link(Speed). For instance, when frame rate is 120 FPS and speed set to 60, this event will occur
      for each second call to @link(Process); on the other hand, if frame rate is 30 FPS with speed set to
      60, this event will occur twice for each call to @link(Process) to maintain constant processing. An
      alternative to this is doing processing inside @link(OnTimer) event using @link(Latency) as coefficient
      for object movement. If the processing takes too much time inside this event so that the target speed
      cannot be achieved, the timer may stall (that is, reduce number of occurrences of this event until the
      balance is restored). }
    property OnProcess: TTimerEvent read FOnProcess write SetOnProcess;
  end;

  // Forward declaration of TApplication.
  TApplication = class;

  // Basic application event.
  TApplicationEvent = procedure(const AApplication: TApplication) of object;

  // Application creation event.
  TApplicationBooleanEvent = function(const AApplication: TApplication): Boolean of object;

  // Application mouse handling event.
  TApplicationMouseEvent = procedure(const AApplication: TApplication; const AEvent: TMouseEvent;
    const AButton: TMouseButton; const APosition: TPoint2i) of object;

  // Application keyboard handling event.
  TApplicationKeyboardEvent = procedure(const AApplication: TApplication; const AEvent: TKeyEvent;
    const AVirtualKey: Integer; const AKeyCode: Word) of object;

  // Custom application message handling event.
  TApplicationHookEvent = function(const AApplication: TApplication;
    const AEvent: Pointer): Boolean of object;

  // Application helper module.
  TApplication = class(TBaseClass)
  private
    FOnCreate: TApplicationBooleanEvent;
    FOnDestroy: TApplicationEvent;
    FOnRender: TApplicationEvent;
    FOnMouse: TApplicationMouseEvent;
    FOnKey: TApplicationKeyboardEvent;
    FOnIdle: TApplicationBooleanEvent;
    FOnResize: TApplicationEvent;
    FOnHook: TApplicationHookEvent;

    function GetWindowHandle: TUntypedHandle; inline;
    function GetTitle: Utf8String;
    procedure SetTitle(const ATitle: Utf8String);
    function GetExecutablePath: Utf8String;
    function GetWindowRect: TIntRect; inline;
    procedure SetWindowRect(const ARect: TIntRect); inline;
    function GetClientRect: TIntRect; inline;
    function GetWindowScale: Single; inline;
    procedure InitializeEvents;
  public
    // Creates new application instance (for Windows platform).
    constructor Create(const ADevice: TDevice; const AWindowTitle, AWindowClassName: Utf8String;
      AInstanceHandle, AIconHandle: TUntypedHandle); overload;

    // Creates new application instance (for Unix-based platforms).
    constructor Create(const ADevice: TDevice; const AWindowTitle, AApplicationLink,
      AIconTitle: Utf8String); overload;

    // Releases the application.
    destructor Destroy; override;

    // Sets new application's window client size.
    procedure SetClientSize(const ASize: TPoint2i); inline;

    // Requests the application window to be repainted.
    procedure Invalidate; inline;

    { Executes application's main loop (or returns @false when there were errors during application create
      event. }
    function Execute: Boolean; inline;

    // The handle of main application window.
    property WindowHandle: TUntypedHandle read GetWindowHandle;

    // Current application title.
    property Title: Utf8String read GetTitle write SetTitle;

    // Application executable path.
    property ExecutablePath: Utf8String read GetExecutablePath;

    // Current application's window rectangle
    property WindowRect: TIntRect read GetWindowRect write SetWindowRect;

    // Current application's window client rectangle.
    property ClientRect: TIntRect read GetClientRect;

    // Current application's window scale.
    property WindowScale: Single read GetWindowScale;

    { Event invoked during startup to load application resources. If this function returns  @false,
      application creation will fail. }
    property OnCreate: TApplicationBooleanEvent read FOnCreate write FOnCreate;

    // Event invoked during application shutdown to release the resources.
    property OnDestroy: TApplicationEvent read FOnDestroy write FOnDestroy;

    // Event invoked when application needs to render contents of the window.
    property OnRender: TApplicationEvent read FOnRender write FOnRender;

    // Event invoked when a mouse event has been registered.
    property OnMouse: TApplicationMouseEvent read FOnMouse write FOnMouse;

    // Event invoked when a key event has been registered.
    property OnKey: TApplicationKeyboardEvent read FOnKey write FOnKey;

    // Event invoked every time the application is idle.
    property OnIdle: TApplicationBooleanEvent read FOnIdle write FOnIdle;

    // Event invoked when application window is resized.
    property OnResize: TApplicationEvent read FOnResize write FOnResize;

    // Event hook invoked to process application events.
    property OnHook: TApplicationHookEvent read FOnHook write FOnHook;
  end;

  /// State-sensitive pseudo-random number generator (PRNG).
  TRandomContext = object(TBaseObject)
  public
    // Releases the random generator's context.
    procedure Free; inline;

    // Tests whether the two given random contexts are the same.
    function Equals(const ARandomContext: TRandomContext): Boolean; inline;

    // Generates next random number in the sequence from the specified context.
    function Raw: Cardinal; inline;

    // Returns 64-bit random number by generating two 32-bit numbers.
    function Raw64: UInt64; inline;

    // Generates random value in range [0, 1) with 23 bits of precision, using relatively linear distribution.
    function Value: Single; overload; inline;

    // Generates random value in range [0, Range) using relatively linear distribution.
    function Value(const ARange: Integer): Integer; overload; inline;

    // Calculates random number in [0, 1] range using gaussian distribution with higher probability located
    // near 0.
    function GaussStart: Single; inline;

    // Calculates random number in [0, 1] range using gaussian distribution with higher probability located
    // near 1.
    function GaussEnd: Single; inline;

    // Calculates random number in [0, 1] range using gaussian distribution with higher probability located
    // near 0.5.
    function GaussMiddle: Single; inline;

    // Calculates random number in [-1..1] range using gaussian distribution with higher probability located
    // near 0.
    function GaussOmni: Single; inline;
  end;

{ Creates random number generation context with initial seed value. Using same seed values for different
  contexts guarantees that the series of generated numbers will be the same. }
function RandomContextInit(const ASeed: Cardinal; const AShuffleCount: Integer = 8): TRandomContext; inline;

implementation

uses
  Classes, Math;

const
  DebugStringBufferLength = 65536;

{$REGION 'Service Functions'}

{$IF NOT DEFINED(DELPHI_MODERN)}
const
  exAllArithmeticExceptions = [exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision];
{$IFEND}

procedure InterpretVertexElements(const AVertexElements: TVertexElements; out AVertexElement: PVertexElement;
  out AVertexElementCount: Cardinal); overload;
begin
  if Length(AVertexElements) > 0 then
  begin
    AVertexElement := @AVertexElements[0];
    AVertexElementCount := Length(AVertexElements);
  end
  else
  begin
    AVertexElement := nil;
    AVertexElementCount := 0;
  end;
end;

procedure InterpretVertexElements(const AVertexElements: array of TVertexElement;
  out AVertexElement: PVertexElement; out AVertexElementCount: Cardinal); overload;
begin
  if Length(AVertexElements) > 0 then
  begin
    AVertexElement := @AVertexElements[0];
    AVertexElementCount := Length(AVertexElements);
  end
  else
  begin
    AVertexElement := nil;
    AVertexElementCount := 0;
  end;
end;

{$ENDREGION}
{$REGION 'TRay'}

class function TRay.Create(const APosition, ASurfaceSize: TPoint2f; const AViewInverse,
  AProjection: TMatrix4f): TRay;
begin
  RayCreate(@APosition, @ASurfaceSize, @AViewInverse, @AProjection, @Result.Origin, @Result.Direction);
end;

function TRay.IntersectTriangle(const AVertex1, AVertex2, AVertex3: TVector3f; const ABackFacing: Boolean;
  out AIntersection: TPoint2f; out ADistance: Single): Boolean;
begin
  Result := RayIntersectTriangle(@Origin, @Direction, @AVertex1, @AVertex2, @AVertex3, ABackFacing,
    @AIntersection, @ADistance);
end;

function TRay.IntersectTriangle(const AVertex1, AVertex2, AVertex3: TVector3f;
  const ABackFacing: Boolean): Boolean;
begin
  Result := RayIntersectTriangle(@Origin, @Direction, @AVertex1, @AVertex2, @AVertex3, ABackFacing, nil, nil);
end;

function TRay.IntersectCubeVolume(const AWorld: TMatrix4f; out ADistance: Single): Boolean;
begin
  Result := RayIntersectCubeVolume(@Origin, @Direction, @AWorld, @ADistance);
end;

function TRay.IntersectCubeVolume(const AWorld: TMatrix4f): Boolean;
begin
  Result := RayIntersectCubeVolume(@Origin, @Direction, @AWorld, nil);
end;

function TRay.IntersectPlane(const APlanePoint, APlaneNormal: TVector3f; out AIntersection: TVector3f;
  out ADistance: Single): Boolean;
begin
  Result := RayIntersectPlane(@Origin, @Direction, @APlanePoint, @APlaneNormal, @AIntersection, @ADistance);
end;

function TRay.IntersectPlane(const APlanePoint, APlaneNormal: TVector3f; out AIntersection: TVector3f): Boolean;
begin
  Result := RayIntersectPlane(@Origin, @Direction, @APlanePoint, @APlaneNormal, @AIntersection, nil);
end;

function TRay.IntersectPlane(const APlanePoint, APlaneNormal: TVector3f): Boolean;
begin
  Result := RayIntersectPlane(@Origin, @Direction, @APlanePoint, @APlaneNormal, nil, nil);
end;

{$ENDREGION}
{$REGION 'TVolume'}

class function TVolume.BoundsToMatrixModel(const AMinBounds, AMaxBounds: TVector3f): TMatrix4f;
begin
  MeshBoundsToMatrixModel(@AMinBounds, @AMaxBounds, @Result);
end;

class function TVolume.BoundsToMatrixVolume(const AMinBounds, AMaxBounds: TVector3f;
  const ASizeBias: Single): TMatrix4f;
begin
  MeshBoundsToMatrixVolume(@AMinBounds, @AMaxBounds, @Result, ASizeBias);
end;

class function TVolume.BoundsTagOffset(const AMeshMinBounds, AMeshMaxBounds, ATagMinBounds,
  ATagMaxBounds: TVector3f): TVector3f;
begin
  MeshBoundsTagOffset(@AMeshMinBounds, @AMeshMaxBounds, @ATagMinBounds, @ATagMaxBounds, @Result);
end;

class function TVolume.BoundsToMatrixVolumeTag(const AMeshMinBounds, AMeshMaxBounds, ATagMinBounds,
  ATagMaxBounds: TVector3f; const ASizeBias: Single): TMatrix4f;
begin
  MeshBoundsToMatrixVolumeTag(@AMeshMinBounds, @AMeshMaxBounds, @ATagMinBounds, @ATagMaxBounds, @Result,
    ASizeBias);
end;

class procedure TVolume.NearFarPlanes(const AWorldView: TMatrix4f; out ANearPlane, AFarPlane: Single);
begin
  VolumeCalculateNearFarPlanes(@AWorldView, @ANearPlane, @AFarPlane);
end;

class function TVolume.VisibleFrame(const AWorldViewProjection: TMatrix4f;
  const ASurfaceSize: TPoint2f): TFloatRect;
begin
  VolumeCalculateVisibleFrame(@AWorldViewProjection, @ASurfaceSize, @Result);
end;

{$ENDREGION}
{$REGION 'TBaseObject'}

function TBaseObject.GetInitialized: Boolean;
begin
  Result := FHandle <> nil;
end;

{$ENDREGION}
{$REGION 'TBaseClass'}

function TBaseClass.GetInitialized: Boolean;
begin
  Result := FHandle <> nil;
end;

{$ENDREGION}
{$REGION 'TDevice'}

procedure TDevice.Free;
begin
  if FHandle <> nil then
  begin
    DeviceDestroy(FHandle);
    FHandle := nil;
  end;
end;

function TDevice.GetCapabilities: TDeviceCapabilities;
begin
  DeviceGetCapabilities(FHandle, @Result);
end;

function TDevice.GetConfiguration: TDeviceConfiguration;
begin
  DeviceGetConfiguration(FHandle, @Result);
end;

function TDevice.GetRenderingState: TRenderingState;
begin
  DeviceGetRenderingState(FHandle, @Result);
end;

procedure TDevice.SetRenderingState(const ARenderingState: TRenderingState);
begin
  DeviceSetRenderingState(FHandle, @ARenderingState);
end;

function TDevice.GetViewport: TIntRect;
begin
  DeviceGetViewport(FHandle, @Result);
end;

procedure TDevice.SetViewport(const AViewport: TIntRect);
begin
  DeviceSetViewport(FHandle, @AViewport);
end;

function TDevice.GetScissor: TIntRect;
begin
  DeviceGetScissor(FHandle, @Result);
end;

procedure TDevice.SetScissor(const AScissor: TIntRect);
begin
  DeviceSetScissor(FHandle, @AScissor);
end;

function TDevice.Resize(const ASize: TPoint2i): Boolean;
begin
  Result := DeviceResize(FHandle, ASize.X, ASize.Y);
end;

function TDevice.BeginScene: Boolean;
begin
  Result := DeviceBegin(FHandle);
end;

procedure TDevice.EndScene;
begin
  DeviceEnd(FHandle);
end;

procedure TDevice.ResetCache;
begin
  DeviceResetCache(FHandle);
end;

function TDevice.Clear(const ALayers: TClearLayers; const AColor: TFloatColor; const ADepth: Single;
  const AStencil: Cardinal): Boolean;
begin
  Result := DeviceClear(FHandle, Byte(ALayers), @AColor, ADepth, AStencil);
end;

function DeviceInit(const ADeviceBackend: TDeviceBackend; const AWindowHandle: TUntypedHandle;
  const ASize: TPoint2i; const APixelFormat, ADepthStencil: TPixelFormat; const AMultisamples: Integer;
  const AAttributes: Cardinal): TDevice;
var
  LConfiguration: TDeviceConfiguration;
begin
  SetExceptionMask(exAllArithmeticExceptions);

  LConfiguration.DeviceType := Cardinal(ADeviceBackend);
  LConfiguration.WindowHandle := AWindowHandle;
  LConfiguration.DisplayWidth := ASize.X;
  LConfiguration.DisplayHeight := ASize.Y;
  LConfiguration.PixelFormat := APixelFormat;
  LConfiguration.DepthStencil := ADepthStencil;
  LConfiguration.Multisamples := AMultisamples;
  LConfiguration.Attributes := AAttributes;

  Result.FHandle := DeviceCreate(@LConfiguration);
end;

function DeviceInitShared(const ADevice: TDevice; const AWindowHandle: TUntypedHandle; const ASize: TPoint2i;
  const APixelFormat: TPixelFormat; const ADepthStencil: TPixelFormat; const AMultisamples: Integer;
  const AAttributes: Cardinal): TDevice;
var
  LConfiguration: TDeviceConfiguration;
begin
  if ADevice.Initialized then
  begin
    LConfiguration := ADevice.Configuration;

    LConfiguration.WindowHandle := AWindowHandle;
    LConfiguration.DisplayWidth := ASize.X;
    LConfiguration.DisplayHeight := ASize.Y;
    LConfiguration.PixelFormat := APixelFormat;
    LConfiguration.DepthStencil := ADepthStencil;
    LConfiguration.Multisamples := AMultisamples;
    LConfiguration.Attributes := AAttributes;

    Result.FHandle := DeviceCreateShared(ADevice.Handle, @LConfiguration);
  end
  else
    Result := TDevice(NullObject);
end;

{$ENDREGION}
{$REGION 'TBuffer'}

procedure TBuffer.Free;
begin
  if FHandle <> nil then
  begin
    BufferDestroy(FHandle);
    FHandle := nil;
  end;
end;

function TBuffer.GetParameters: TBufferParameters;
begin
  BufferGetParameters(FHandle, @Result);
end;

function TBuffer.Update(const AData: Pointer; const AOffset, ASize: Cardinal): Boolean;
begin
  Result := BufferUpdate(FHandle, AData, AOffset, ASize);
end;

function BufferInit(const ADevice: TDevice; const ADataType: TBufferDataType;
  const AAccessType: TBufferAccessType; const ASize, APitch: Cardinal; const AInitialData: Pointer): TBuffer;
var
  LParameters: TBufferParameters;
begin
  LParameters.DataType := ADataType;
  LParameters.AccessType := AAccessType;
  LParameters.Size := ASize;
  LParameters.Pitch := APitch;

  Result.FHandle := BufferCreate(ADevice.Handle, @LParameters, AInitialData);
end;

{$ENDREGION}
{$REGION 'TProgram'}

procedure TProgram.Free;
begin
  if FHandle <> nil then
  begin
    ProgramDestroy(FHandle);
    FHandle := nil;
  end;
end;

function TProgram.UpdateByIndex(const AVariableIndex: Integer; const AVariableData: Pointer;
  const ASize: Cardinal): Boolean;
begin
  Result := ProgramUpdateByIndex(FHandle, AVariableIndex, AVariableData, ASize);
end;

function TProgram.UpdateByName(const AVariableName: Utf8String; const AVariableData: Pointer;
  const ASize: Cardinal): Boolean;
begin
  Result := ProgramUpdateByName(FHandle, PAnsiChar(AVariableName), AVariableData, ASize);
end;

function TProgram.Bind(const ABuffer: TBuffer; const AChannel, AOffset: Cardinal): Boolean;
begin
  Result := ProgramBind(FHandle, ABuffer.Handle, AChannel, AOffset);
end;

procedure TProgram.Unbind(const ABuffer: TBuffer; const AChannel: Cardinal);
begin
  ProgramUnbind(FHandle, ABuffer.Handle, AChannel);
end;

procedure TProgram.ResetBindings;
begin
  ProgramResetBindings(FHandle);
end;

procedure TProgram.ResetCache;
begin
  ProgramResetCache(FHandle);
end;

procedure TProgram.PurgeCache(const ABuffer: TBuffer);
begin
  ProgramPurgeCache(FHandle, ABuffer.Handle);
end;

function TProgram.Commit: Boolean;
begin
  Result := ProgramCommit(FHandle);
end;

function TProgram.BeginScene: Boolean;
begin
  Result := ProgramBegin(FHandle);
end;

procedure TProgram.EndScene;
begin
  ProgramEnd(FHandle);
end;

function TProgram.Draw(const ATopology: TPrimitiveTopology; const AVertexCount: Cardinal;
  const ABaseVertex: Integer): Boolean;
begin
  Result := ProgramDraw(FHandle, ATopology, AVertexCount, ABaseVertex);
end;

function TProgram.DrawIndexed(const ATopology: TPrimitiveTopology; const AIndexCount, AStartIndex: Cardinal;
  const ABaseVertex: Integer): Boolean;
begin
  Result := ProgramDrawIndexed(FHandle, ATopology, AIndexCount, AStartIndex, ABaseVertex);
end;

function TProgram.DrawInstances(const ATopology: TPrimitiveTopology; const AVertexCount,
  AInstanceCount: Cardinal; const ABaseVertex: Integer): Boolean;
begin
  Result := ProgramDrawInstances(FHandle, ATopology, AVertexCount, AInstanceCount, ABaseVertex);
end;

function TProgram.DrawInstancesIndexed(const ATopology: TPrimitiveTopology; const AIndexCount,
  AInstanceCount, AStartIndex: Cardinal; const ABaseVertex: Integer): Boolean;
begin
  Result := ProgramDrawInstancesIndexed(FHandle, ATopology, AIndexCount, AInstanceCount, AStartIndex,
    ABaseVertex);
end;

function LoadShaderFromFile(const AFileName: string): TBytes;
var
  LStream: TFileStream;
  LByteCount: Integer;
begin
  SetLength(Result, 0);
  try
    LStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  except
    Exit;
  end;
  try
    SetLength(Result, LStream.Size);
    LByteCount := LStream.Read(Result[0], LStream.Size);
    SetLength(Result, LByteCount);
  finally
    LStream.Free;
  end;
end;

function ProgramInit(const ADevice: TDevice; const AParameters: TProgramParameters): TProgram;
begin
  Result.FHandle := ProgramCreate(ADevice.Handle, @AParameters);
end;

function ProgramInit(const ADevice: TDevice; const AVertexElements: TVertexElements;
  const AProgramElements: TProgramElements; const AProgramVariables: TProgramVariables; const AVertexShader,
  AGeometryShader, AFragmentShader: TBytes): TProgram;
var
  LParameters: TProgramParameters;
begin
  FillChar(LParameters, SizeOf(TProgramParameters), 0);

  InterpretVertexElements(AVertexElements, LParameters.VertexElements, LParameters.VertexElementCount);

  if Length(AProgramElements) > 0 then
  begin
    LParameters.ProgramElements := @AProgramElements[0];
    LParameters.ProgramElementCount := Length(AProgramElements);
  end;

  if Length(AProgramVariables) > 0 then
  begin
    LParameters.ProgramVariables := @AProgramVariables[0];
    LParameters.ProgramVariableCount := Length(AProgramVariables);
  end;

  if Length(AVertexShader) > 0 then
  begin
    LParameters.VertexShader := @AVertexShader[0];
    LParameters.VertexShaderLength := Length(AVertexShader);
  end;

  if Length(AGeometryShader) > 0 then
  begin
    LParameters.GeometryShader := @AGeometryShader[0];
    LParameters.GeometryShaderLength := Length(AGeometryShader);
  end;

  if Length(AFragmentShader) > 0 then
  begin
    LParameters.FragmentShader := @AFragmentShader[0];
    LParameters.FragmentShaderLength := Length(AFragmentShader);
  end;

  Result.FHandle := ProgramCreate(ADevice.Handle, @LParameters);
end;

function ProgramInit(const ADevice: TDevice; const AVertexElements: array of TVertexElement;
  const AProgramElements: array of TProgramElement; const AProgramVariables: array of TProgramVariable;
  const AVertexShader, AGeometryShader, AFragmentShader: TBytes): TProgram;
var
  LParameters: TProgramParameters;
begin
  FillChar(LParameters, SizeOf(TProgramParameters), 0);

  InterpretVertexElements(AVertexElements, LParameters.VertexElements, LParameters.VertexElementCount);

  if Length(AProgramElements) > 0 then
  begin
    LParameters.ProgramElements := @AProgramElements[0];
    LParameters.ProgramElementCount := Length(AProgramElements);
  end;

  if Length(AProgramVariables) > 0 then
  begin
    LParameters.ProgramVariables := @AProgramVariables[0];
    LParameters.ProgramVariableCount := Length(AProgramVariables);
  end;

  if Length(AVertexShader) > 0 then
  begin
    LParameters.VertexShader := @AVertexShader[0];
    LParameters.VertexShaderLength := Length(AVertexShader);
  end;

  if Length(AGeometryShader) > 0 then
  begin
    LParameters.GeometryShader := @AGeometryShader[0];
    LParameters.GeometryShaderLength := Length(AGeometryShader);
  end;

  if Length(AFragmentShader) > 0 then
  begin
    LParameters.FragmentShader := @AFragmentShader[0];
    LParameters.FragmentShaderLength := Length(AFragmentShader);
  end;

  Result.FHandle := ProgramCreate(ADevice.Handle, @LParameters);
end;

function ProgramInitFromFiles(const ADevice: TDevice; const AVertexElements: TVertexElements;
  const AProgramElements: TProgramElements; const AProgramVariables: TProgramVariables; const AVertexShader,
  AGeometryShader, AFragmentShader: string): TProgram;
var
  LVertexShader, LGeometryShader, LFragmentShader: TBytes;
begin
  if Length(AVertexShader) > 0 then
    LVertexShader := LoadShaderFromFile(AVertexShader)
  else
    SetLength(LVertexShader, 0);

  if Length(AGeometryShader) > 0 then
    LGeometryShader := LoadShaderFromFile(AGeometryShader)
  else
    SetLength(LGeometryShader, 0);

  if Length(AFragmentShader) > 0 then
    LFragmentShader := LoadShaderFromFile(AFragmentShader)
  else
    SetLength(LFragmentShader, 0);

  Result := ProgramInit(ADevice, AVertexElements, AProgramElements, AProgramVariables, LVertexShader,
    LGeometryShader, LFragmentShader);
end;

function ProgramInitFromFiles(const ADevice: TDevice;
  const AVertexElements: array of TVertexElement; const AProgramElements: array of TProgramElement;
  const AProgramVariables: array of TProgramVariable; const AVertexShader, AGeometryShader,
  AFragmentShader: string): TProgram;
var
  LVertexShader, LGeometryShader, LFragmentShader: TBytes;
begin
  if Length(AVertexShader) > 0 then
    LVertexShader := LoadShaderFromFile(AVertexShader)
  else
    SetLength(LVertexShader, 0);

  if Length(AGeometryShader) > 0 then
    LGeometryShader := LoadShaderFromFile(AGeometryShader)
  else
    SetLength(LGeometryShader, 0);

  if Length(AFragmentShader) > 0 then
    LFragmentShader := LoadShaderFromFile(AFragmentShader)
  else
    SetLength(LFragmentShader, 0);

  Result := ProgramInit(ADevice, AVertexElements, AProgramElements, AProgramVariables, LVertexShader,
    LGeometryShader, LFragmentShader);
end;

{$ENDREGION}
{$REGION 'TComputeProgram'}

procedure TComputeProgram.Free;
begin
  if FHandle <> nil then
  begin
    ComputeProgramDestroy(FHandle);
    FHandle := nil;
  end;
end;

function TComputeProgram.Bind(const ABuffer: TBuffer; const AChannel, AOffset: Cardinal): Boolean;
begin
  Result := ComputeProgramBindBuffer(FHandle, ABuffer.Handle, AChannel, AOffset);
end;

procedure TComputeProgram.Unbind(const ABuffer: TBuffer; const AChannel: Cardinal);
begin
  ComputeProgramUnbindBuffer(FHandle, ABuffer.Handle, AChannel);
end;

function TComputeProgram.Bind(const ATexture: PTexture;
  const ABindFormat: TComputeBindTextureFormat): Boolean;
begin
  Result := ComputeProgramBindTexture(FHandle, ATexture.Handle, @ABindFormat);
end;

procedure TComputeProgram.Unbind(const ATexture: PTexture;
  const ABindFormat: TComputeBindTextureFormat);
begin
  ComputeProgramUnbindTexture(FHandle, ATexture.Handle, @ABindFormat);
end;

procedure TComputeProgram.ResetBindings;
begin
  ComputeProgramResetBindings(FHandle);
end;

function TComputeProgram.Commit: Boolean;
begin
  Result := ComputeProgramCommit(FHandle);
end;

function TComputeProgram.BeginScene: Boolean;
begin
  Result := ComputeProgramBegin(FHandle);
end;

procedure TComputeProgram.EndScene;
begin
  ComputeProgramEnd(FHandle);
end;

function TComputeProgram.Dispatch(const AGroupsX, AGroupsY, AGroupsZ: Cardinal): Boolean;
begin
  Result := ComputeProgramDispatch(FHandle, AGroupsX, AGroupsY, AGroupsZ);
end;

function ComputeProgramInit(const ADevice: TDevice; const AProgramElements: TProgramElements;
  const AShader: TBytes): TComputeProgram;
var
  LProgramElements: PProgramElement;
  LProgramElementCount: Cardinal;
  LShader: PAnsiChar;
  LShaderLength: Cardinal;
begin
  if Length(AProgramElements) > 0 then
  begin
    LProgramElements := @AProgramElements[0];
    LProgramElementCount := Length(AProgramElements);
  end
  else
  begin
    LProgramElements := nil;
    LProgramElementCount := 0;
  end;

  if Length(AShader) > 0 then
  begin
    LShader := @AShader[0];
    LShaderLength := Length(AShader);
  end
  else
  begin
    LShader := nil;
    LShaderLength := 0;
  end;

  Result.FHandle := ComputeProgramCreate(ADevice.Handle, LProgramElements, LProgramElementCount, LShader,
    LShaderLength);
end;

function ComputeProgramInit(const ADevice: TDevice;
  const AProgramElements: array of TProgramElement; const AShader: TBytes): TComputeProgram;
var
  LProgramElements: PProgramElement;
  LProgramElementCount: Cardinal;
  LShader: PAnsiChar;
  LShaderLength: Cardinal;
begin
  if Length(AProgramElements) > 0 then
  begin
    LProgramElements := @AProgramElements[0];
    LProgramElementCount := Length(AProgramElements);
  end
  else
  begin
    LProgramElements := nil;
    LProgramElementCount := 0;
  end;

  if Length(AShader) > 0 then
  begin
    LShader := @AShader[0];
    LShaderLength := Length(AShader);
  end
  else
  begin
    LShader := nil;
    LShaderLength := 0;
  end;

  Result.FHandle := ComputeProgramCreate(ADevice.Handle, LProgramElements, LProgramElementCount, LShader,
    LShaderLength);
end;

function ComputeProgramInitFromFile(const ADevice: TDevice; const AProgramElements: TProgramElements;
  const AShader: string): TComputeProgram;
var
  LShader: TBytes;
begin
  if Length(AShader) > 0 then
    LShader := LoadShaderFromFile(AShader)
  else
    SetLength(LShader, 0);

  Result := ComputeProgramInit(ADevice, AProgramElements, LShader);
end;

function ComputeProgramInitFromFile(const ADevice: TDevice; const AProgramElements: array of TProgramElement;
  const AShader: string): TComputeProgram;
var
  LShader: TBytes;
begin
  if Length(AShader) > 0 then
    LShader := LoadShaderFromFile(AShader)
  else
    SetLength(LShader, 0);

  Result := ComputeProgramInit(ADevice, AProgramElements, LShader);
end;

{$ENDREGION}
{$REGION 'TMeshModel'}

procedure TMeshModel.Free;
begin
  if FHandle <> nil then
  begin
    MeshModelDestroy(FHandle);
    FHandle := nil;
  end;
end;

function TMeshModel.GetInformation: TMeshModelInformation;
begin
  MeshModelGetInformation(FHandle, @Result);
end;

function TMeshModel.GetVertexBuffer: TBuffer;
begin
  Result.FHandle := MeshModelGetVertexBuffer(FHandle);
end;

function TMeshModel.GetIndexBuffer: TBuffer;
begin
  Result.FHandle := MeshModelGetIndexBuffer(FHandle);
end;

function TMeshModel.Draw(const AProgram: TProgram; const ATopology: TPrimitiveTopology;
  const AElementCount, AStartIndex: Cardinal; const ABaseVertex: Integer;
  const APostUnbind: Boolean): Boolean;
begin
  Result := MeshModelDraw(FHandle, AProgram.Handle, ATopology, AElementCount, AStartIndex, ABaseVertex,
    APostUnbind);
end;

function TMeshModel.DrawInstances(const AProgram: TProgram; const AInstanceCount: Cardinal;
  const ATopology: TPrimitiveTopology; const AElementCount, AStartIndex: Cardinal;
  const ABaseVertex: Integer; const APostUnbind: Boolean): Boolean;
begin
  Result := MeshModelDrawInstances(FHandle, AProgram.Handle, AInstanceCount, ATopology, AElementCount,
    AStartIndex, ABaseVertex, APostUnbind);
end;

procedure TMeshModel.PurgeProgramCache(const AProgram: TProgram);
begin
  MeshModelPurgeProgramCache(FHandle, AProgram.Handle);
end;

function MeshModelInit(const ADevice: TDevice; const AVertexElements: TVertexElements;
  const AVertexCount, AIndexCount, AChannel: Cardinal; const AInitialVertexData,
  AInitialIndexData: Pointer): TMeshModel;
var
  LParameters: TMeshModelParameters;
begin
  InterpretVertexElements(AVertexElements, LParameters.VertexElements, LParameters.VertexElementCount);
  LParameters.VertexCount := AVertexCount;
  LParameters.IndexCount := AIndexCount;
  LParameters.Channel := AChannel;
  LParameters.InitialVertexData := AInitialVertexData;
  LParameters.InitialIndexData := AInitialIndexData;

  Result.FHandle := MeshModelCreate(ADevice.Handle, @LParameters);
end;

function MeshModelInit(const ADevice: TDevice; const AVertexElements: array of TVertexElement;
  const AVertexCount, AIndexCount, AChannel: Cardinal; const AInitialVertexData,
  AInitialIndexData: Pointer): TMeshModel;
var
  LParameters: TMeshModelParameters;
begin
  InterpretVertexElements(AVertexElements, LParameters.VertexElements, LParameters.VertexElementCount);
  LParameters.VertexCount := AVertexCount;
  LParameters.IndexCount := AIndexCount;
  LParameters.Channel := AChannel;
  LParameters.InitialVertexData := AInitialVertexData;
  LParameters.InitialIndexData := AInitialIndexData;

  Result.FHandle := MeshModelCreate(ADevice.Handle, @LParameters);
end;

{$ENDREGION}
{$REGION 'TMeshMetaTag'}

function TMeshMetaTag.GetOwner: TBaseObject;
begin
  Result.FHandle := MeshMetaTagGetOwner(FHandle);
end;

function TMeshMetaTag.GetName: Utf8String;
var
  LLength: Cardinal;
begin
  MeshMetaTagGetName(FHandle, nil, @LLength);
  SetLength(Result, LLength);
  MeshMetaTagGetName(FHandle, @Result[1], @LLength);
end;

function TMeshMetaTag.GetMinBounds: TVector3f;
begin
  MeshMetaTagGetBounds(FHandle, @Result, nil);
end;

function TMeshMetaTag.GetMaxBounds: TVector3f;
begin
  MeshMetaTagGetBounds(FHandle, nil, @Result);
end;

function TMeshMetaTag.GetFirstVertex: Integer;
begin
  MeshMetaTagGetLimits(FHandle, @Result, nil, nil, nil);
end;

function TMeshMetaTag.GetVertexCount: Integer;
begin
  MeshMetaTagGetLimits(FHandle, nil, @Result, nil, nil);
end;

function TMeshMetaTag.GetFirstIndex: Integer;
begin
  MeshMetaTagGetLimits(FHandle, nil, nil, @Result, nil);
end;

function TMeshMetaTag.GetIndexCount: Integer;
begin
  MeshMetaTagGetLimits(FHandle, nil, nil, nil, @Result);
end;

{$ENDREGION}
{$REGION 'TMeshMetaTagsEnumerator'}

constructor TMeshMetaTagsEnumerator.Create(const AHandle: TLibraryClassHandle);
begin
  FHandle := AHandle;
  FCurrent := -1;
end;

function TMeshMetaTagsEnumerator.GetCurrent: TMeshMetaTag;
begin
  Result.FHandle := MeshMetaTagsGetByIndex(FHandle, FCurrent);
end;

function TMeshMetaTagsEnumerator.MoveNext: Boolean;
begin
  Inc(FCurrent);
  Result := FCurrent < Integer(MeshMetaTagsCount(FHandle));
end;

{$ENDREGION}
{$REGION 'TMeshMetaTags'}

procedure TMeshMetaTags.Free;
begin
  if FHandle <> nil then
  begin
    MeshMetaTagsDestroy(FHandle);
    FHandle := nil;
  end;
end;

function TMeshMetaTags.GetCount: Integer;
begin
  Result := MeshMetaTagsCount(FHandle);
end;

function TMeshMetaTags.GetTags(const AIndex: Integer): TMeshMetaTag;
begin
  Result.FHandle := MeshMetaTagsGetByIndex(FHandle, AIndex);
end;

function TMeshMetaTags.GetTag(const AName: Utf8String): TMeshMetaTag;
begin
  Result.FHandle := MeshMetaTagsGetByName(FHandle, PAnsiChar(AName));
end;

function TMeshMetaTags.Spawn(const AName: Utf8String; const AMinBounds, AMaxBounds: TVector3f;
  const AStartVertex, AVertexCount, AStartIndex, AIndexCount: Integer): TMeshMetaTag;
begin
  Result.FHandle := MeshMetaTagsSpawn(FHandle, PAnsiChar(AName), @AMinBounds, @AMaxBounds, AStartVertex,
    AVertexCount, AStartIndex, AIndexCount);
end;

procedure TMeshMetaTags.Erase(const AIndex: Integer);
begin
  MeshMetaTagsErase(FHandle, AIndex);
end;

procedure TMeshMetaTags.Clear;
begin
  MeshMetaTagsClear(FHandle);
end;

function TMeshMetaTags.GetEnumerator: TMeshMetaTagsEnumerator;
begin
  Result := TMeshMetaTagsEnumerator.Create(FHandle);
end;

function MeshMetaTagsInit: TMeshMetaTags;
begin
  Result.FHandle := MeshMetaTagsCreate;
end;

{$ENDREGION}
{$REGION 'TMeshBuffer'}

procedure TMeshBuffer.Free;
begin
  if FHandle <> nil then
  begin
    MeshBufferDestroy(FHandle);
    FHandle := nil;
  end;
end;

function TMeshBuffer.GetInformation: TMeshBufferInformation;
begin
  MeshBufferGetInformation(FHandle, @Result);
end;

procedure TMeshBuffer.SetInformation(AInformation: TMeshBufferInformation);
begin
  MeshBufferSetInformation(FHandle, @AInformation);
end;

function TMeshBuffer.GetTransform: TMatrix4f;
begin
  MeshBufferGetTransform(FHandle, @Result);
end;

procedure TMeshBuffer.SetTransform(const ATransform: TMatrix4f);
begin
  MeshBufferSetTransform(FHandle, @ATransform);
end;

function TMeshBuffer.Model(const ADevice: TDevice; const AVertexElements: TVertexElements;
  const AStartVertex, AVertexCount, AStartIndex, AIndexCount: Integer; const AChannel: Cardinal): TMeshModel;
var
  LVertexElements: PVertexElement;
  LVertexElementCount: Cardinal;
begin
  InterpretVertexElements(AVertexElements, LVertexElements, LVertexElementCount);
  Result.FHandle := MeshBufferCreateModel(FHandle, ADevice.Handle, LVertexElements, LVertexElementCount,
    AStartVertex, AVertexCount, AStartIndex, AIndexCount, AChannel);
end;

function TMeshBuffer.Model(const ADevice: TDevice; const AVertexElements: array of TVertexElement;
  const AStartVertex, AVertexCount, AStartIndex, AIndexCount: Integer; const AChannel: Cardinal): TMeshModel;
var
  LVertexElements: PVertexElement;
  LVertexElementCount: Cardinal;
begin
  InterpretVertexElements(AVertexElements, LVertexElements, LVertexElementCount);
  Result.FHandle := MeshBufferCreateModel(FHandle, ADevice.Handle, LVertexElements, LVertexElementCount,
    AStartVertex, AVertexCount, AStartIndex, AIndexCount, AChannel);
end;

procedure TMeshBuffer.SuperEllipse(const ALongitudeSections, ALatitudeSections: Cardinal; const AOrigin,
  ARadius: TVector3f; const AInitLongitudeAngle, AEndLongitudeAngle, ALongitudeShape, AInitLatitudeAngle,
  AEndLatitudeAngle, ALatitudeShape: Single; const AInitTexCoord, AEndTexCoord: TPoint2f;
  const AColor: TFloatColor; const AIndicesClockwise: Boolean);
begin
  MeshBufferSuperEllipse(FHandle, ALongitudeSections, ALatitudeSections, @AOrigin, @ARadius,
    AInitLongitudeAngle, AEndLongitudeAngle, ALongitudeShape, AInitLatitudeAngle, AEndLatitudeAngle,
    ALatitudeShape, @AInitTexCoord, @AEndTexCoord, @AColor, AIndicesClockwise);
end;

procedure TMeshBuffer.Cylinder(const ARadialSections, AHeightSections: Cardinal; const AOrigin, AHorizAxis,
  AVertAxis, AHeightAxis: TVector3f; const AInitAngle, AEndAngle, AShape: Single; const AInitTexCoord,
  AEndTexCoord: TPoint2f; const ABendAngle, ALateralAngle: Single; const AColor: TFloatColor;
  const AIndicesClockwise: Boolean);
begin
  MeshBufferCylinder(FHandle, ARadialSections, AHeightSections, @AOrigin, @AHorizAxis, @AVertAxis,
    @AHeightAxis, AInitAngle, AEndAngle, AShape, @AInitTexCoord, @AEndTexCoord, ABendAngle, ALateralAngle,
    @AColor, AIndicesClockwise);
end;

procedure TMeshBuffer.Cone(const ASections: Cardinal; const AOrigin: TVector3f; const ARadius,
  AHeight: Single; const AColor: TFloatColor; const AIndicesClockwise: Boolean);
begin
  MeshBufferConeSimple(FHandle, ASections, @AOrigin, ARadius, AHeight, @AColor, AIndicesClockwise);
end;

procedure TMeshBuffer.Disc(const ARadialSections, AInnerSections: Cardinal; const AOrigin, AHorizAxis,
  AVertAxis, ANormal: TVector3f; const AInitAngle, AEndAngle, AShape, AInitRadius: Single;
  const AInitTexCoord, AEndTexCoord: TPoint2f; const ALateralAngle: Single; const AColor: TFloatColor;
  const AIndicesClockwise: Boolean);
begin
  MeshBufferDisc(FHandle, ARadialSections, AInnerSections, @AOrigin, @AHorizAxis, @AVertAxis, @ANormal,
    AInitAngle, AEndAngle, AShape, AInitRadius, @AInitTexCoord, @AEndTexCoord, ALateralAngle, @AColor,
    AIndicesClockwise);
end;

procedure TMeshBuffer.Plane(const AHorizSections, AVertSections: Cardinal; const AOrigin, AHorizAxis,
  AVertAxis, ANormal: TVector3f; const AInitTexCoord, AEndTexCoord: TPoint2f; const AColor: TFloatColor;
  const AIndicesClockwise: Boolean);
begin
  MeshBufferPlane(FHandle, AHorizSections, AVertSections, @AOrigin, @AHorizAxis, @AVertAxis, @ANormal,
    @AInitTexCoord, @AEndTexCoord, @AColor, AIndicesClockwise);
end;

procedure TMeshBuffer.Cube(const AHorizSections, AVertSections, ADepthSections: Cardinal; const AOrigin,
  AHorizAxis, AVertAxis, ADepthAxis: TVector3f; const AInitTexCoord, AEndTexCoord: TPoint2f;
  const AColor: TFloatColor; const AIndicesClockwise: Boolean);
begin
  MeshBufferCube(FHandle, AHorizSections, AVertSections, ADepthSections, @AOrigin, @AHorizAxis, @AVertAxis,
    @ADepthAxis, @AInitTexCoord, @AEndTexCoord, @AColor, AIndicesClockwise);
end;

procedure TMeshBuffer.CubeMinimal(const AOrigin, ASize: TVector3f; const AColor: TFloatColor;
  const AIndicesClockwise: Boolean);
begin
  MeshBufferCubeMinimal(FHandle, @AOrigin, @ASize, @AColor, AIndicesClockwise);
end;

procedure TMeshBuffer.CubeRound(const AOrigin, ASize: TVector3f; const ARoundness: Single;
  const AColor: TFloatColor; const AIndicesClockwise: Boolean);
begin
  MeshBufferCubeRound(FHandle, @AOrigin, @ASize, ARoundness, @AColor, AIndicesClockwise);
end;

procedure TMeshBuffer.Torus(const AOuterSections, AInnerSections: Cardinal; const AOrigin, AHorizAxis,
  AVertAxis: TVector3f; const AInitOuterAngle, AEndOuterAngle, AOuterShape, AInitInnerAngle, AEndInnerAngle,
  AInnerShape: Single; const AInnerRadius, AInitTexCoord, AEndTexCoord: TPoint2f;
  const ALateralAngle: Single; const AColor: TFloatColor; const AIndicesClockwise: Boolean);
begin
  MeshBufferTorus(FHandle, AOuterSections, AInnerSections, @AOrigin, @AHorizAxis, @AVertAxis,
    AInitOuterAngle, AEndOuterAngle, AOuterShape, AInitInnerAngle, AEndInnerAngle, AInnerShape,
    @AInnerRadius, @AInitTexCoord, @AEndTexCoord, ALateralAngle, @AColor, AIndicesClockwise);
end;

procedure TMeshBuffer.TorusKnot(const AOuterSections, AInnerSections: Cardinal; const AOrigin: TVector3f;
  const AP, AQ: Integer; const AInitOuterAngle, AEndOuterAngle, AOuterRadius, AInitInnerAngle,
  AEndInnerAngle, AInnerShape: Single; const AInnerRadius, AInitTexCoord, AEndTexCoord: TPoint2f;
  const ALateralAngle: Single; const AColor: TFloatColor; const AIndicesClockwise: Boolean);
begin
  MeshBufferTorusKnot(FHandle, AOuterSections, AInnerSections, @AOrigin, AP, AQ, AInitOuterAngle,
    AEndOuterAngle, AOuterRadius, AInitInnerAngle, AEndInnerAngle, AInnerShape, @AInnerRadius,
    @AInitTexCoord, @AEndTexCoord, ALateralAngle, @AColor, AIndicesClockwise);
end;

procedure TMeshBuffer.Supertoroid(const AOuterSections, AInnerSections: Cardinal; const AOrigin: TVector3f;
  const AInitOuterAngle, AEndOuterAngle, AOuterRadius, AOuterShape, AInitInnerAngle, AEndInnerAngle,
  AInnerRadius, AInnerShape: Single; const AInitTexCoord, AEndTexCoord: TPoint2f; const AColor: TFloatColor;
  const AIndicesClockwise: Boolean);
begin
  MeshBufferSupertoroid(FHandle, AOuterSections, AInnerSections, @AOrigin, AInitOuterAngle, AEndOuterAngle,
    AOuterRadius, AOuterShape, AInitInnerAngle, AEndInnerAngle, AInnerRadius, AInnerShape, @AInitTexCoord,
    @AEndTexCoord, @AColor, AIndicesClockwise);
end;

procedure TMeshBuffer.TransformVertices(const AFirstVertex, AVertexCount: Cardinal);
begin
  MeshBufferTransformVertices(FHandle, AFirstVertex, AVertexCount);
end;

function TMeshBuffer.CalculateFlatNormals(const AEpsilon: Single): Boolean;
begin
  Result := MeshBufferCalculateFlatNormals(FHandle, AEpsilon);
end;

procedure TMeshBuffer.CalculateNormals(const AFirstVertex, AVertexCount, AFirstIndex, AIndexCount: Cardinal);
begin
  MeshBufferCalculateNormals(FHandle, AFirstVertex, AVertexCount, AFirstIndex, AIndexCount);
end;

procedure TMeshBuffer.CalculateNormalsWeld(const AFirstVertex, AVertexCount, AFirstIndex,
  AIndexCount: Cardinal; const AWeldEpsilon: Single);
begin
  MeshBufferCalculateNormalsWeld(FHandle, AFirstVertex, AVertexCount, AFirstIndex, AIndexCount,
    AWeldEpsilon);
end;

procedure TMeshBuffer.InvertNormals(const AFirstVertex, AVertexCount: Cardinal);
begin
  MeshBufferInvertNormals(FHandle, AFirstVertex, AVertexCount);
end;

procedure TMeshBuffer.InvertIndexOrder(const AFirstIndex, AIndexCount: Cardinal);
begin
  MeshBufferInvertIndexOrder(FHandle, AFirstIndex, AIndexCount);
end;

procedure TMeshBuffer.CalculateBounds(var AVertexMin, AVertexMax: TVector3f; const AFirstVertex,
  AVertexCount: Cardinal);
begin
  MeshBufferCalculateBounds(FHandle, @AVertexMin, @AVertexMax, AFirstVertex, AVertexCount);
end;

procedure TMeshBuffer.Centralize(const AFirstVertex, AVertexCount: Cardinal);
begin
  MeshBufferCentralize(FHandle, AFirstVertex, AVertexCount);
end;

procedure TMeshBuffer.EliminateUnusedVertices(const AFirstVertex, AVertexCount: Cardinal);
begin
  MeshBufferEliminateUnusedVertices(FHandle, AFirstVertex, AVertexCount);
end;

function TMeshBuffer.JoinDuplicateVertices(const AFirstVertex, AVertexCount: Cardinal;
  const ATreshold: Single): Boolean;
begin
  Result := MeshBufferJoinDuplicateVertices(FHandle, AFirstVertex, AVertexCount, ATreshold);
end;

function TMeshBuffer.Combine(const AMeshBufferSource: TBaseObject; const AFirstVertex, AVertexCount,
  AFirstIndex, AIndexCount: Cardinal): Boolean;
begin
  Result := MeshBufferCombine(FHandle, AMeshBufferSource.Handle, AFirstVertex, AVertexCount, AFirstIndex,
    AIndexCount);
end;

procedure TMeshBuffer.Clear;
begin
  MeshBufferClear(FHandle);
end;

procedure TMeshBuffer.TransferVertices(const ABuffer: Pointer; const AVertexElements: TVertexElements;
  const AStartVertex, AVertexCount: Integer; const AChannel: Cardinal; const ASemanticIndex: Integer);
var
  LVertexElements: PVertexElement;
  LVertexElementCount: Cardinal;
begin
  InterpretVertexElements(AVertexElements, LVertexElements, LVertexElementCount);
  MeshBufferTransferVertexElements(FHandle, ABuffer, LVertexElements, LVertexElementCount, AStartVertex,
    AVertexCount, AChannel, ASemanticIndex);
end;

procedure TMeshBuffer.TransferVertices(const ABuffer: Pointer; const AVertexElements: array of TVertexElement;
  const AStartVertex, AVertexCount: Integer; const AChannel: Cardinal; const ASemanticIndex: Integer);
var
  LVertexElements: PVertexElement;
  LVertexElementCount: Cardinal;
begin
  InterpretVertexElements(AVertexElements, LVertexElements, LVertexElementCount);
  MeshBufferTransferVertexElements(FHandle, ABuffer, LVertexElements, LVertexElementCount, AStartVertex,
    AVertexCount, AChannel, ASemanticIndex);
end;

procedure TMeshBuffer.TransferVertices(const ABuffer: TBuffer; const AVertexElements: TVertexElements;
  const AStartVertex, AVertexCount: Integer; const AChannel, AOffset: Cardinal;
  const ASemanticIndex: Integer);
var
  LVertexElements: PVertexElement;
  LVertexElementCount: Cardinal;
begin
  InterpretVertexElements(AVertexElements, LVertexElements, LVertexElementCount);
  MeshBufferTransferVertices(FHandle, ABuffer.Handle, LVertexElements, LVertexElementCount, AStartVertex,
    AVertexCount, AChannel, AOffset, ASemanticIndex);
end;

procedure TMeshBuffer.TransferVertices(const ABuffer: TBuffer;
  const AVertexElements: array of TVertexElement; const AStartVertex, AVertexCount: Integer; const AChannel,
  AOffset: Cardinal; const ASemanticIndex: Integer);
var
  LVertexElements: PVertexElement;
  LVertexElementCount: Cardinal;
begin
  InterpretVertexElements(AVertexElements, LVertexElements, LVertexElementCount);
  MeshBufferTransferVertices(FHandle, ABuffer.Handle, LVertexElements, LVertexElementCount, AStartVertex,
    AVertexCount, AChannel, AOffset, ASemanticIndex);
end;

procedure TMeshBuffer.TransferIndices(const ABuffer: Pointer; const APitch: Cardinal; const AStartVertex,
  AStartIndex, AIndexCount: Integer);
begin
  MeshBufferTransferIndexElements(FHandle, ABuffer, APitch, AStartVertex, AStartIndex, AIndexCount);
end;

procedure TMeshBuffer.TransferIndices(const ABuffer: TBuffer; const AStartVertex, AStartIndex,
  AIndexCount: Integer; const AOffset: Cardinal);
begin
  MeshBufferTransferIndices(FHandle, ABuffer.Handle, AStartVertex, AStartIndex, AIndexCount, AOffset);
end;

function TMeshBuffer.LoadMeshFromFile(const AFileName: Utf8String; var AMinBounds,
  AMaxBounds: TVector3f): Boolean;
begin
  Result := MeshBufferLoadMeshFromFile(FHandle, nil, PAnsiChar(AFileName), @AMinBounds, @AMaxBounds,
    nil, 0, nil);
end;

function TMeshBuffer.LoadMeshFromFile(const AMeshMetaTags: TMeshMetaTags; const AFileName: Utf8String;
  var AMinBounds, AMaxBounds: TVector3f): Boolean;
begin
  Result := MeshBufferLoadMeshFromFile(FHandle, AMeshMetaTags.Handle, PAnsiChar(AFileName), @AMinBounds,
    @AMaxBounds, nil, 0, nil);
end;

function TMeshBuffer.LoadMeshFromFile(const AFileName: Utf8String; var AMinBounds, AMaxBounds: TVector3f;
  out ADebug: Utf8String): Boolean;
begin
  Result := LoadMeshFromFile(TMeshMetaTags(NullObject), AFileName, AMinBounds, AMaxBounds, ADebug);
end;

function TMeshBuffer.LoadMeshFromFile(const AMeshMetaTags: TMeshMetaTags; const AFileName: Utf8String;
  var AMinBounds, AMaxBounds: TVector3f; out ADebug: Utf8String): Boolean;
var
  LLength: Cardinal;
begin
  LLength := 0;
  SetLength(ADebug, DebugStringBufferLength);

  Result := MeshBufferLoadMeshFromFile(FHandle, AMeshMetaTags.Handle, PAnsiChar(AFileName),
    @AMinBounds, @AMaxBounds, @ADebug[1], DebugStringBufferLength, @LLength);

  SetLength(ADebug, LLength);
end;

function TMeshBuffer.SaveMeshToFile(const AFileName: Utf8String;
  const AExportFlags: TMeshExportFlags): Boolean;
begin
  Result := MeshBufferSaveMeshToFile(FHandle, PAnsiChar(AFileName), Byte(AExportFlags), nil, 0, nil);
end;

function TMeshBuffer.SaveMeshToFile(const AFileName: Utf8String; const AExportFlags: TMeshExportFlags;
  out ADebug: Utf8String): Boolean;
var
  LLength: Cardinal;
begin
  LLength := 0;
  SetLength(ADebug, DebugStringBufferLength);

  Result := MeshBufferSaveMeshToFile(FHandle, PAnsiChar(AFileName), Byte(AExportFlags), @ADebug[1],
    DebugStringBufferLength, @LLength);

  SetLength(ADebug, LLength);
end;

function MeshBufferInit: TMeshBuffer;
begin
  Result.FHandle := MeshBufferCreate;
end;

{$ENDREGION}
{$REGION 'TSampler'}

procedure TSampler.Free;
begin
  if FHandle <> nil then
  begin
    SamplerDestroy(FHandle);
    FHandle := nil;
  end;
end;

function TSampler.GetState: TSamplerState;
begin
  SamplerGetState(FHandle, @Result);
end;

procedure TSampler.SetState(const AState: TSamplerState);
begin
  SamplerUpdate(FHandle, @AState);
end;

function TSampler.Bind(const AChannel: Cardinal): Boolean;
begin
  Result := SamplerBind(FHandle, AChannel);
end;

procedure TSampler.Unbind(const AChannel: Cardinal);
begin
  SamplerUnbind(FHandle, AChannel);
end;

function SamplerInit(const ADevice: TDevice; const ASamplerState: TSamplerState): TSampler;
begin
  Result.FHandle := SamplerCreate(ADevice.Handle, @ASamplerState);
end;

{$ENDREGION}
{$REGION 'TRasterSurface'}

procedure TRasterSurface.Free;
begin
  if FHandle <> nil then
  begin
    RasterSurfaceDestroy(FHandle);
    FHandle := nil;
  end;
end;

function TRasterSurface.GetParameters: TRasterSurfaceParameters;
begin
  RasterSurfaceGetParameters(FHandle, @Result);
end;

function TRasterSurface.GetPixel(const AX, AY: Integer): TIntColor;
begin
  Result := RasterSurfaceGetPixel(FHandle, AX, AY);
end;

procedure TRasterSurface.SetPixel(const AX, AY: Integer; const AColor: TIntColor);
begin
  RasterSurfaceSetPixel(FHandle, AX, AY, AColor);
end;

function TRasterSurface.Update(var AParameters: TRasterSurfaceParameters): Boolean;
begin
  Result := RasterSurfaceUpdate(FHandle, @AParameters);
end;

function TRasterSurface.ApproximateFormat(const AFormat: TPixelFormat): TPixelFormat;
begin
  Result := RasterSurfaceApproximateFormat(FHandle, AFormat);
end;

function TRasterSurface.ConvertFormat(const AFormat: TPixelFormat): Boolean;
begin
  Result := RasterSurfaceConvertFormat(FHandle, AFormat);
end;

function TRasterSurface.CopyRect(const ASurface: PRasterSurface; const ASourceRect: TIntRect;
  const ADestPos: TPoint2i): Boolean;
begin
  Result := (ASurface <> nil) and RasterSurfaceCopyRect(FHandle, ASurface.Handle, @ASourceRect, @ADestPos);
end;

function TRasterSurface.CopyFrom(const ASurface: PRasterSurface): Boolean;
begin
  Result := (ASurface <> nil) and RasterSurfaceCopyFrom(FHandle, ASurface.Handle);
end;

procedure TRasterSurface.Clear;
begin
  RasterSurfaceClear(FHandle);
end;

function TRasterSurface.Clear(const AColor: TIntColor): Boolean;
begin
  Result := RasterSurfaceClearWith(FHandle, AColor);
end;

function TRasterSurface.ResetAlpha(const AOpaque: Boolean): Boolean;
begin
  Result := RasterSurfaceResetAlpha(FHandle, AOpaque);
end;

function TRasterSurface.HasAlphaChannel: Boolean;
begin
  Result := RasterSurfaceHasAlphaChannel(FHandle);
end;

function TRasterSurface.PremultiplyAlpha: Boolean;
begin
  Result := RasterSurfacePremultiplyAlpha(FHandle);
end;

function TRasterSurface.UnpremultiplyAlpha: Boolean;
begin
  Result := RasterSurfaceUnpremultiplyAlpha(FHandle);
end;

function TRasterSurface.Mirror: Boolean;
begin
  Result := RasterSurfaceMirror(FHandle);
end;

function TRasterSurface.Flip: Boolean;
begin
  Result := RasterSurfaceFlip(FHandle);
end;

procedure TRasterSurface.FillRect(const ADestRect: TIntRect; const AColors: TColorRect);
begin
  RasterSurfaceFillRect(FHandle, @ADestRect, @AColors);
end;

procedure TRasterSurface.DrawPixel(const AX, AY: Integer; const AColor: TIntColor);
begin
  RasterSurfaceDrawPixel(FHandle, AX, AY, AColor);
end;

procedure TRasterSurface.HorizLine(const AX, AY, ALineWidth: Integer; const AColor: TIntColor);
begin
  RasterSurfaceHorizLine(FHandle, AX, AY, ALineWidth, AColor);
end;

procedure TRasterSurface.VertLine(const AX, AY, ALineHeight: Integer; const AColor: TIntColor);
begin
  RasterSurfaceVertLine(FHandle, AX, AY, ALineHeight, AColor);
end;

procedure TRasterSurface.FrameRect(const ADestRect: TIntRect; const AColor: TIntColor);
begin
  RasterSurfaceFrameRect(FHandle, @ADestRect, AColor);
end;

procedure TRasterSurface.Draw(const ASurface: PRasterSurface; const ADestPos: TPoint2i;
  const ASourceRect: TIntRect; const AColors: TColorRect);
begin
  if ASurface <> nil then
    RasterSurfaceDraw(FHandle, ASurface.Handle, @ADestPos, @ASourceRect, @AColors);
end;

function TRasterSurface.ShrinkToHalfFrom(const ASurface: PRasterSurface): Boolean;
begin
  if ASurface <> nil then
    Result := RasterSurfaceShrinkToHalfFrom(FHandle, ASurface.Handle)
  else
    Result := False;
end;

function TRasterSurface.GetBilinearPixel(const AX, AY: Single): TIntColor;
begin
  Result := RasterSurfaceGetBilinearPixel(FHandle, AX, AY);
end;

function TRasterSurface.GetPixelWithEdge(const AX, AY: Integer): TIntColor;
begin
  Result := RasterSurfaceGetPixelWithEdge(FHandle, AX, AY);
end;

function TRasterSurface.CalculateSignedDistance(const AX, AY: Integer;
  const ASpread: VectorFloat): VectorFloat;
begin
  Result := RasterSurfaceCalculateSignedDistance(FHandle, AX, AY, ASpread);
end;

procedure TRasterSurface.StretchFrom(const ASurface: PRasterSurface; const ADestRect,
  ASourceRect: TFloatRect);
begin
  if ASurface <> nil then
    RasterSurfaceStretchFrom(FHandle, ASurface.Handle, @ADestRect, @ASourceRect);
end;

procedure TRasterSurface.StretchBilinearFrom(const ASurface: PRasterSurface; const ADestRect,
  ASourceRect: TFloatRect);
begin
  if ASurface <> nil then
    RasterSurfaceStretchBilinearFrom(FHandle, ASurface.Handle, @ADestRect, @ASourceRect);
end;

function TRasterSurface.MakeSignedDistanceField(const ASurface: PRasterSurface; const ASpread: VectorFloat;
  const ADestPos: TPoint2i; const ASourceRect: TIntRect): Boolean;
begin
  if ASurface <> nil then
    Result := RasterSurfaceMakeSignedDistanceField(FHandle, ASurface.Handle, ASpread, @ADestPos,
      @ASourceRect)
  else
    Result := False;
end;

function TRasterSurface.SaveToFile(const AFileName: Utf8String; const AQuality: NativeUInt): Boolean;
begin
  Result := RasterSurfaceSaveToFile(FHandle, PAnsiChar(AFileName), AQuality);
end;

function TRasterSurface.SaveToFileInMemory(const AFileContent: Pointer; const AFileContentSize: Cardinal;
  const AExtension: Utf8String; const AQuality: NativeUInt): Cardinal;
begin
  Result := RasterSurfaceSaveToFileInMemory(FHandle, AFileContent, AFileContentSize, PAnsiChar(AExtension),
    AQuality);
end;

function RasterSurfaceInit(const AWidth, AHeight: Integer; const AFormat: TPixelFormat): TRasterSurface;
begin
  Result.FHandle := RasterSurfaceCreate(AWidth, AHeight, AFormat);
end;

function RasterSurfaceInit(const AFileName: Utf8String;
  const AFormatRequest: TAlphaFormatRequest): TRasterSurface;
begin
  Result.FHandle := RasterSurfaceCreateFromFile(PAnsiChar(AFileName), AFormatRequest);
end;

function RasterSurfaceInit(const AFileContent: Pointer; const AContentSize: Cardinal;
  const AExtension: Utf8String; const AFormatRequest: TAlphaFormatRequest): TRasterSurface;
begin
  Result.FHandle := RasterSurfaceCreateFromFileInMemory(AFileContent, AContentSize, PAnsiChar(AExtension),
    AFormatRequest);
end;

{$ENDREGION}
{$REGION 'TTexture'}

procedure TTexture.Free;
begin
  if FHandle <> nil then
  begin
    TextureDestroy(FHandle);
    FHandle := nil;
  end;
end;

function TTexture.GetParameters: TTextureParameters;
begin
  TextureGetParameters(FHandle, @Result);
end;

function TTexture.Update(const AContent: Pointer; const APitch: Cardinal; const ALayer: Integer;
  const ARect: TIntRect; const AMipLevel: Integer): Boolean;
begin
  Result := TextureUpdate(FHandle, AContent, APitch, ALayer, @ARect, AMipLevel);
end;

function TTexture.Retrieve(const AContent: Pointer; const APitch: Cardinal; const ALayer: Integer;
  const ARect: TIntRect; const AMipLevel: Integer): Boolean;
begin
  Result := TextureRetrieve(FHandle, AContent, APitch, ALayer, @ARect, AMipLevel);
end;

function TTexture.Copy(const ATexture: PTexture; const ADestLayer: Integer; const ADestPos: TPoint2i;
  const ASrcLayer: Integer; const ASourceRect: TIntRect; const ADestMipLevel: Integer;
  const ASrcMipLevel: Integer): Boolean;
begin
  Result := (ATexture <> nil) and TextureCopy(FHandle, ATexture.Handle, ADestLayer, @ADestPos, ASrcLayer,
    @ASourceRect, ADestMipLevel, ASrcMipLevel);
end;

function TTexture.Copy(const ASurface: TRasterSurface; const ALayer: Integer; const ADestPos: TPoint2i;
  const ASourceRect: TIntRect; const AMipLevel: Integer): Boolean;
begin
  Result := TextureCopyFromSurface(FHandle, ASurface.Handle, ALayer, @ADestPos, @ASourceRect, AMipLevel);
end;

function TTexture.Save(const ASurface: TRasterSurface; const ALayer: Integer; const ADestPos: TPoint2i;
  const ASourceRect: TIntRect; const AMipLevel: Integer): Boolean;
begin
  Result := TextureCopyToSurface(FHandle, ASurface.Handle, ALayer, @ADestPos, @ASourceRect, AMipLevel);
end;

function TTexture.LoadFromFile(const AFileName: Utf8String; const ALayer: Integer; const ADestPos: TPoint2i;
  const ASourceRect: TIntRect; const AMipLevel: Integer): Boolean;
begin
  Result := TextureLoadFromFile(FHandle, PAnsiChar(AFileName), ALayer, @ADestPos, @ASourceRect, AMipLevel);
end;

function TTexture.SaveToFile(const AFileName: Utf8String; const AQuality: Pointer; const ALayer: Integer;
  const ASourceRect: TIntRect; const AMipLevel: Integer): Boolean;
begin
  Result := TextureSaveToFile(FHandle, PAnsiChar(AFileName), AQuality, ALayer, @ASourceRect, AMipLevel);
end;

function TTexture.Clear: Boolean;
begin
  Result := TextureClear(FHandle);
end;

function TTexture.Clear(const AColor: TFloatColor; const ALayer: Integer): Boolean;
begin
  Result := TextureClearWith(FHandle, @AColor, ALayer);
end;

function TTexture.Bind(const AChannel: Cardinal): Boolean;
begin
  Result := TextureBind(FHandle, AChannel);
end;

function TTexture.Unbind(const AChannel: Cardinal): Boolean;
begin
  Result := TextureUnbind(FHandle, AChannel);
end;

function TTexture.Attach(const ATexture: PTexture; const ALayer: Integer): Boolean;
begin
  if ATexture <> nil then
    Result := TextureAttach(FHandle, ATexture.Handle, ALayer)
  else
    Result := False;
end;

procedure TTexture.Detach;
begin
  TextureDetach(FHandle);
end;

function TTexture.BeginScene(const ALayer: Cardinal): Boolean;
begin
  Result := TextureBegin(FHandle, ALayer);
end;

function TTexture.EndScene: Boolean;
begin
  Result := TextureEnd(FHandle);
end;

function TTexture.GenerateMipMaps: Boolean;
begin
  Result := TextureGenerateMipMaps(FHandle);
end;

procedure TTexture.ResetCache;
begin
  TextureResetCache(FHandle);
end;

function TextureInit(const ADevice: TDevice; const AParameters: TTextureParameters): TTexture;
begin
  Result.FHandle := TextureCreate(ADevice.Handle, @AParameters);
end;

function TextureInit(const ADevice: TDevice; const AFileName: Utf8String; const AFormat: TPixelFormat;
  AAttributes: Cardinal): TTexture;
begin
  Result.FHandle := TextureCreateFromFile(ADevice.Handle, PAnsiChar(AFileName), AFormat, AAttributes);
end;

function TextureInit(const ADevice: TDevice; const AFileContent: Pointer; const AContentSize: Cardinal;
  const AExtension: Utf8String; const AFormat: TPixelFormat; AAttributes: Cardinal): TTexture;
begin
  Result.FHandle := TextureCreateFromFileInMemory(ADevice.Handle, AFileContent, AContentSize,
    PAnsiChar(AExtension), AFormat, AAttributes);
end;

{$ENDREGION}
{$REGION 'TImageAtlas'}

procedure TImageAtlas.Free;
begin
  if FHandle <> nil then
  begin
    ImageAtlasDestroy(FHandle);
    FHandle := nil;
  end;
end;

function TImageAtlas.GetTextureCount: Integer;
begin
  Result := ImageAtlasTextureCount(FHandle);
end;

function TImageAtlas.GetTexture(const ATextureIndex: Integer): TTexture;
begin
  Result.FHandle := ImageAtlasTexture(FHandle, ATextureIndex);
end;

function TImageAtlas.GetRegionCount: Integer;
begin
  Result := ImageAtlasRegionCount(FHandle);
end;

function TImageAtlas.GetRegion(const ARegionIndex: Integer): TImageRegion;
begin
  if not ImageAtlasRegion(FHandle, ARegionIndex, @Result) then
  begin
    FillChar(Result, SizeOf(TImageRegion), 0);
    Result.Index := High(Word);
  end;
end;

function TImageAtlas.CreateRegion(const ARect: TIntRect; const ATextureIndex: Integer): Integer;
begin
  Result := ImageAtlasCreateRegion(FHandle, @ARect, ATextureIndex);
end;

procedure TImageAtlas.RemoveRegion(const ARegionIndex: Integer);
begin
  ImageAtlasRemoveRegion(FHandle, ARegionIndex);
end;

procedure TImageAtlas.MakeRegions(const APatternSize, AVisibleSize: TPoint2i; const APatternCount: Integer);
begin
  ImageAtlasMakeRegions(FHandle, @APatternSize, @AVisibleSize, APatternCount);
end;

procedure TImageAtlas.ClearTextures;
begin
  ImageAtlasClearTextures(FHandle);
end;

procedure TImageAtlas.RemoveTexture(const ATextureIndex: Integer);
begin
  ImageAtlasRemoveTexture(FHandle, ATextureIndex);
end;

function TImageAtlas.CreateTexture(const ASize: TPoint2i; const AFormat: TPixelFormat;
  const AAttributes: Cardinal): Integer;
begin
  Result := ImageAtlasCreateTexture(FHandle, @ASize, AFormat, AAttributes);
end;

function TImageAtlas.Pack(const ASize: TPoint2i; const APadding: Integer): Integer;
begin
  Result := ImageAtlasPackRegion(FHandle, @ASize, APadding);
end;

function TImageAtlas.Pack(const ASurface: TRasterSurface; const ASourceRect: TIntRect;
  const APadding: Integer): Integer;
begin
  Result := ImageAtlasPackSurface(FHandle, ASurface.Handle, @ASourceRect, APadding);
end;

function ImageAtlasInit(const ADevice: TDevice): TImageAtlas;
begin
  Result.FHandle := ImageAtlasCreate(ADevice.Handle);
end;

{$ENDREGION}
{$REGION 'TCanvas'}

procedure TCanvas.Free;
begin
  if FHandle <> nil then
  begin
    CanvasDestroy(FHandle);
    FHandle := nil;
  end;
end;

function TCanvas.GetClipRect: TIntRect;
begin
  CanvasGetClipRect(FHandle, @Result);
end;

procedure TCanvas.SetClipRect(const AClipRect: TIntRect);
begin
  CanvasSetClipRect(FHandle, @AClipRect);
end;

function TCanvas.GetTransform: TMatrix4f;
begin
  CanvasGetTransform(FHandle, @Result);
end;

procedure TCanvas.SetTransform(const ATransform: TMatrix4f);
begin
  CanvasSetTransform(FHandle, @ATransform);
end;

function TCanvas.GetBatchCount: Integer;
begin
  Result := CanvasGetBatchCount(FHandle);
end;

function TCanvas.GetAttributes: TCanvasAttributes;
var
  LAttributes: Cardinal;
begin
  LAttributes := CanvasGetAttributes(FHandle);
  Result := PCanvasAttributes(@LAttributes)^;
end;

procedure TCanvas.SetAttributes(const AAttributes: TCanvasAttributes);
var
  LAttributes: Cardinal;
begin
  LAttributes := 0;
  PCanvasAttributes(@LAttributes)^ := AAttributes;
  CanvasSetAttributes(FHandle, LAttributes);
end;

function TCanvas.GetSignedDistanceField: TSignedDistanceField;
begin
  CanvasGetSignedDistanceField(FHandle, @Result);
end;

procedure TCanvas.SetSignedDistanceField(const ASignedDistanceField: TSignedDistanceField);
begin
  CanvasSetSignedDistanceField(FHandle, @ASignedDistanceField);
end;

function TCanvas.GetContextState: TCanvasContextState;
begin
  Result := CanvasGetContextState(FHandle);
end;

procedure TCanvas.SetContextState(const AContextState: TCanvasContextState);
begin
  CanvasSetContextState(FHandle, AContextState);
end;

function TCanvas.BeginScene: Boolean;
begin
  Result := CanvasBegin(FHandle);
end;

procedure TCanvas.EndScene;
begin
  CanvasEnd(FHandle);
end;

procedure TCanvas.Pixels(const APositions: PPoint2f; const AColors: PIntColor; const AElementCount: Integer;
  const AEffect: TBlendingEffect);
begin
  CanvasPixels(FHandle, APositions, AColors, AElementCount, AEffect);
end;

procedure TCanvas.Lines(const AVertices: PPoint2f; const AColors: PIntColor; const AIndices: PCardinal;
  const AVertexCount, APrimitiveCount: Integer; const AEffect: TBlendingEffect);
begin
  CanvasLines(FHandle, AVertices, AColors, AIndices, AVertexCount, APrimitiveCount, AEffect);
end;

procedure TCanvas.Triangles(const AVertices: PPoint2f; const AColors: PIntColor; const AIndices: PCardinal;
  const AVertexCount, APrimitiveCount: Integer; const AEffect: TBlendingEffect);
begin
  CanvasTriangles(FHandle, AVertices, AColors, AIndices, AVertexCount, APrimitiveCount, AEffect);
end;

procedure TCanvas.Triangles(const ATexture: TTexture; const AVertices, ATexCoords: PPoint2f;
  const AColors: PIntColor; const AIndices: PCardinal; const AVertexCount, APrimitiveCount: Integer;
  const AEffect: TBlendingEffect);
begin
  CanvasTexturedTriangles(FHandle, ATexture.Handle, AVertices, ATexCoords, AColors, AIndices, AVertexCount,
    APrimitiveCount, AEffect);
end;

procedure TCanvas.Pixel(const APosition: TPoint2f; const AColor: TIntColor; const AEffect: TBlendingEffect);
begin
  CanvasPixel(FHandle, @APosition, AColor, AEffect);
end;

procedure TCanvas.Line(const ASrcPos, ADestPos: TPoint2f; const AColors: TColorPair;
  const AEffect: TBlendingEffect);
begin
  CanvasLine(FHandle, @ASrcPos, @ADestPos, @AColors, AEffect);
end;

procedure TCanvas.ThickLine(const ASrcPos, ADestPos: TPoint2f; const AColors: TColorPair;
  const AThickness: Single; const AEffect: TBlendingEffect);
begin
  CanvasThickLine(FHandle, @ASrcPos, @ADestPos, @AColors, AThickness, AEffect);
end;

procedure TCanvas.LineEllipse(const AOrigin, ARadius: TPoint2f; const ASteps: Integer;
  const AColor: TIntColor; const AEffect: TBlendingEffect);
begin
  CanvasLineEllipse(FHandle, @AOrigin, @ARadius, ASteps, AColor, AEffect);
end;

procedure TCanvas.LineCircle(const AOrigin: TPoint2f; const ARadius: VectorFloat; const ASteps: Integer;
  const AColor: TIntColor; const AEffect: TBlendingEffect);
begin
  CanvasLineCircle(FHandle, @AOrigin, ARadius, ASteps, AColor, AEffect);
end;

procedure TCanvas.ThickLineEllipse(const AOrigin, ARadius: TPoint2f; const ASteps: Integer;
  const AColor: TIntColor; const AThickness: Single; const AEffect: TBlendingEffect);
begin
  CanvasThickLineEllipse(FHandle, @AOrigin, @ARadius, ASteps, AColor, AThickness, AEffect);
end;

procedure TCanvas.ThickLineCircle(const AOrigin: TPoint2f; const ARadius: VectorFloat; const ASteps: Integer;
  const AColor: TIntColor; const AThickness: Single; const AEffect: TBlendingEffect);
begin
  CanvasThickLineCircle(FHandle, @AOrigin, ARadius, ASteps, AColor, AThickness, AEffect);
end;

procedure TCanvas.LineTriangle(const AVertex1, AVertex2, AVertex3: TPoint2f; const AColor1, AColor2,
  AColor3: TIntColor; const AEffect: TBlendingEffect);
begin
  CanvasLineTriangle(FHandle, @AVertex1, @AVertex2, @AVertex3, AColor1, AColor2, AColor3, AEffect);
end;

procedure TCanvas.ThickLineTriangle(const AVertex1, AVertex2, AVertex3: TPoint2f; const AColor1, AColor2,
  AColor3: TIntColor; const AThickness: VectorFloat; const AJoint: TPathJoint;
  const ASmoothStep: VectorFloat; const AEffect: TBlendingEffect);
begin
  CanvasThickLineTriangle(FHandle, @AVertex1, @AVertex2, @AVertex3, AColor1, AColor2, AColor3, AThickness,
    AJoint, ASmoothStep, AEffect);
end;

procedure TCanvas.LineHexagon(const AMatrix: TMatrix3f; const AColor1, AColor2, AColor3, AColor4, AColor5,
  AColor6: TIntColor; const AEffect: TBlendingEffect);
begin
  CanvasLineHexagon(FHandle, @AMatrix, AColor1, AColor2, AColor3, AColor4, AColor5, AColor6, AEffect);
end;

procedure TCanvas.LineHexagon(const AMatrix: TMatrix3f; const AColors: TColorRect;
  const AEffect: TBlendingEffect);
begin
  CanvasLineHexagonGrad(FHandle, @AMatrix, @AColors, AEffect);
end;

procedure TCanvas.ThickLineHexagon(const AMatrix: TMatrix3f; const AColor1, AColor2, AColor3, AColor4,
  AColor5, AColor6: TIntColor; const AThickness: VectorFloat; const AJoint: TPathJoint;
  const ASmoothStep: Single; const AEffect: TBlendingEffect);
begin
  CanvasThickLineHexagon(FHandle, @AMatrix, AColor1, AColor2, AColor3, AColor4, AColor5, AColor6, AThickness,
    AJoint, ASmoothStep, AEffect);
end;

procedure TCanvas.ThickLineHexagon(const AMatrix: TMatrix3f; const AColors: TColorRect;
  const AThickness: VectorFloat; const AJoint: TPathJoint; const ASmoothStep: Single;
  const AEffect: TBlendingEffect);
begin
  CanvasThickLineHexagonGrad(FHandle, @AMatrix, @AColors, AThickness, AJoint, ASmoothStep, AEffect);
end;

procedure TCanvas.LineQuad(const AVertices: TQuad; const AColors: TColorRect; const AEffect: TBlendingEffect);
begin
  CanvasLineQuad(FHandle, @AVertices, @AColors, AEffect);
end;

procedure TCanvas.ThickLineQuad(const AVertices: TQuad; const AColors: TColorRect; const AThickness: Single;
  const AJoint: TPathJoint; const ASmoothStep: Single; const AEffect: TBlendingEffect);
begin
  CanvasThickLineQuad(FHandle, @AVertices, @AColors, AThickness, AJoint, ASmoothStep, AEffect);
end;

procedure TCanvas.Triangle(const AVertex1, AVertex2, AVertex3: TPoint2f; const AColor1, AColor2,
  AColor3: TIntColor; const AEffect: TBlendingEffect);
begin
  CanvasTriangle(FHandle, @AVertex1, @AVertex2, @AVertex3, AColor1, AColor2, AColor3, AEffect);
end;

procedure TCanvas.Quad(const AVertices: TQuad; const AColors: TColorRect; const AEffect: TBlendingEffect);
begin
  CanvasQuad(FHandle, @AVertices, @AColors, AEffect);
end;

procedure TCanvas.FillRect(const ARect: TFloatRect; const AColors: TColorRect;
  const AEffect: TBlendingEffect);
begin
  CanvasFillRect(FHandle, @ARect, @AColors, AEffect);
end;

procedure TCanvas.FrameRect(const ARect: TFloatRect; const AColors: TColorRect; const AThickness: Single;
  const AEffect: TBlendingEffect);
begin
  CanvasFrameRect(FHandle, @ARect, @AColors, AThickness, AEffect);
end;

procedure TCanvas.FillRoundRect(const ARect: TFloatRect; const AColors: TColorRect; const ARadius: Single;
  const ASteps: Cardinal; const AEffect: TBlendingEffect);
begin
  CanvasFillRoundRect(FHandle, @ARect, @AColors, ARadius, ASteps, AEffect);
end;

procedure TCanvas.FrameRoundRect(const ARect: TFloatRect; const AColors: TColorRect; const ARadius,
  AThickness: Single; const ASteps: Cardinal; const AEffect: TBlendingEffect);
begin
  CanvasFrameRoundRect(FHandle, @ARect, @AColors, ARadius, AThickness, ASteps, AEffect);
end;

procedure TCanvas.FillRoundRectTop(const ARect: TFloatRect; const AColors: TColorRect;
  const ARadius: Single; const ASteps: Integer; const AEffect: TBlendingEffect);
begin
  CanvasFillRoundRectTop(FHandle, @ARect, @AColors, ARadius, ASteps, AEffect);
end;

procedure TCanvas.FillRoundRectBottom(const ARect: TFloatRect; const AColors: TColorRect;
  const ARadius: Single; const ASteps: Integer; const AEffect: TBlendingEffect);
begin
  CanvasFillRoundRectBottom(FHandle, @ARect, @AColors, ARadius, ASteps, AEffect);
end;

procedure TCanvas.FillRoundRectTopInverse(const ARect: TFloatRect; const AColors: TColorRect;
  const ARadius: Single; const ASteps: Integer; const AEffect: TBlendingEffect);
begin
  CanvasFillRoundRectTopInverse(FHandle, @ARect, @AColors, ARadius, ASteps, AEffect);
end;

procedure TCanvas.Highlight(const ARect: TFloatRect; const ACornerRadius, ALuma, ADistance: Single;
  const ASteps: Integer);
begin
  CanvasHighlight(FHandle, @ARect, ACornerRadius, ALuma, ADistance, ASteps);
end;

procedure TCanvas.Hexagon(const AMatrix: TMatrix3f; const AColor1, AColor2, AColor3, AColor4, AColor5,
  AColor6: TIntColor; const AEffect: TBlendingEffect);
begin
  CanvasHexagon(FHandle, @AMatrix, AColor1, AColor2, AColor3, AColor4, AColor5, AColor6, AEffect);
end;

procedure TCanvas.Hexagon(const AMatrix: TMatrix3f; const AColors: TColorRect;
  const AEffect: TBlendingEffect);
begin
  CanvasHexagonGrad(FHandle, @AMatrix, @AColors, AEffect);
end;

procedure TCanvas.Arc(const AOrigin, ARadius: TPoint2f; const AInitAngle, AEndAngle: Single;
  const ASteps: Integer; const AColors: TColorRect; const AEffect: TBlendingEffect);
begin
  CanvasArc(FHandle, @AOrigin, @ARadius, AInitAngle, AEndAngle, ASteps, @AColors, AEffect);
end;

procedure TCanvas.Ellipse(const AOrigin, ARadius: TPoint2f; const ASteps: Integer; const AColors: TColorRect;
  const AEffect: TBlendingEffect);
begin
  CanvasEllipse(FHandle, @AOrigin, @ARadius, ASteps, @AColors, AEffect);
end;

procedure TCanvas.Ribbon(const AOrigin, AInsideRadius, AOutsideRadius: TPoint2f; const ASteps: Integer;
  const AColors: TColorRect; const AEffect: TBlendingEffect);
begin
  CanvasRibbonGrad(FHandle, @AOrigin, @AInsideRadius, @AOutsideRadius, ASteps, @AColors, AEffect);
end;

procedure TCanvas.Ribbon(const AOrigin, AInsideRadius, AOutsideRadius: TPoint2f; const ASteps: Integer;
  const AColor1, AColor2, AColor3: TColorPair; const AEffect: TBlendingEffect);
begin
  CanvasRibbonTri(FHandle, @AOrigin, @AInsideRadius, @AOutsideRadius, ASteps, @AColor1, @AColor2, @AColor3,
    AEffect);
end;

procedure TCanvas.Ribbon(const AOrigin, AInsideRadius, AOutsideRadius: TPoint2f; const ASteps: Integer;
  const AColor: TIntColor; const AEffect: TBlendingEffect);
begin
  CanvasRibbon(FHandle, @AOrigin, @AInsideRadius, @AOutsideRadius, ASteps, AColor, AEffect);
end;

procedure TCanvas.Tape(const AOrigin, AInsideRadius, AOutsideRadius: TPoint2f; const AInitAngle,
  AEndAngle: Single; const ASteps: Integer; const AColors: TColorRect; const AEffect: TBlendingEffect);
begin
  CanvasTapeGrad(FHandle, @AOrigin, @AInsideRadius, @AOutsideRadius, AInitAngle, AEndAngle, ASteps, @AColors,
    AEffect);
end;

procedure TCanvas.Tape(const AOrigin, AInsideRadius, AOutsideRadius: TPoint2f; const AInitAngle,
  AEndAngle: Single; const ASteps: Integer; const AColor1, AColor2, AColor3: TColorPair;
  const AEffect: TBlendingEffect);
begin
  CanvasTapeTri(FHandle, @AOrigin, @AInsideRadius, @AOutsideRadius, AInitAngle, AEndAngle, ASteps, @AColor1,
    @AColor2, @AColor3, AEffect);
end;

procedure TCanvas.Tape(const AOrigin, AInsideRadius, AOutsideRadius: TPoint2f;const AInitAngle,
  AEndAngle: Single; const ASteps: Integer; const AColor: TIntColor; const AEffect: TBlendingEffect);
begin
  CanvasTape(FHandle, @AOrigin, @AInsideRadius, @AOutsideRadius, AInitAngle, AEndAngle, ASteps, AColor,
    AEffect);
end;

procedure TCanvas.RectWithHole(const ARect: TFloatRect; const AHoleOrigin, AHoleRadius: TPoint2f;
  const AColors: TColorPair; const ASteps: Integer; const AEffect: TBlendingEffect);
begin
  CanvasRectWithHole(FHandle, @ARect, @AHoleOrigin, @AHoleRadius, @AColors, ASteps, AEffect);
end;

procedure TCanvas.ThickLinePath(const APoints: TPoint2FArray; const AColors: TIntColorArray;
  const AJoints: TPathJointArray; const AThickness: VectorFloat; const ABaseJoint: TPathJoint;
  const ALineCaps: TLineCaps; const AMiterLimit, ASmoothStep: VectorFloat; const AClosePath: Boolean;
  const AEffect: TBlendingEffect);
var
  LJoints: PPathJoint;
begin
  if Length(APoints) > 0 then
  begin
    if Length(AJoints) > 0 then
      LJoints := @AJoints[0]
    else
      LJoints := nil;

    CanvasThickLinePath(FHandle, @APoints[0], @AColors[0], LJoints, Length(APoints), AThickness, ABaseJoint,
      ALineCaps, AMiterLimit, ASmoothStep, AClosePath, AEffect);
  end;
end;

procedure TCanvas.ThickLinePath(const APoints: array of TPoint2f; const AColors: array of TIntColor;
  const AJoints: array of TPathJoint; const AThickness: VectorFloat; const ABaseJoint: TPathJoint;
  const ALineCaps: TLineCaps; const AMiterLimit, ASmoothStep: VectorFloat; const AClosePath: Boolean;
  const AEffect: TBlendingEffect);
var
  LJoints: PPathJoint;
begin
  if Length(APoints) > 0 then
  begin
    if Length(AJoints) > 0 then
      LJoints := @AJoints[0]
    else
      LJoints := nil;

    CanvasThickLinePath(FHandle, @APoints[0], @AColors[0], LJoints, Length(APoints), AThickness, ABaseJoint,
      ALineCaps, AMiterLimit, ASmoothStep, AClosePath, AEffect);
  end;
end;

procedure TCanvas.Polygon(const AContours: TPoint2FArray; const AContourLengths: TCardinalArray;
  const AColor: TIntColor; const AWindingRule: TTessellationWinding; const AEffect: TBlendingEffect);
begin
  if Length(AContourLengths) > 0 then
    CanvasPolygon(FHandle, @AContours[0], @AContourLengths[0], Length(AContourLengths), AColor, AWindingRule,
      AEffect);
end;

procedure TCanvas.Polygon(const AContours: array of TPoint2f; const AContourLengths: array of Cardinal;
  const AColor: TIntColor; const AWindingRule: TTessellationWinding; const AEffect: TBlendingEffect);
begin
  if Length(AContourLengths) > 0 then
    CanvasPolygon(FHandle, @AContours[0], @AContourLengths[0], Length(AContourLengths), AColor, AWindingRule,
      AEffect);
end;

procedure TCanvas.Triangle(const ATexture: TTexture; const AVertex1, AVertex2, AVertex3, ATexCoord1,
  ATexCoord2, ATexCoord3: TPoint2f; const AColor1, AColor2, AColor3: TIntColor;
  const AEffect: TBlendingEffect);
begin
  CanvasTexturedTriangle(FHandle, ATexture.Handle, @AVertex1, @AVertex2, @AVertex3, @ATexCoord1, @ATexCoord2,
    @ATexCoord3, AColor1, AColor2, AColor3, AEffect);
end;

procedure TCanvas.TriangleRegion(const ATexture: TTexture; const AVertex1, AVertex2, AVertex3, ATexCoord1,
  ATexCoord2, ATexCoord3: TPoint2f; const AColor1, AColor2, AColor3: TIntColor;
  const AEffect: TBlendingEffect);
begin
  CanvasTexturedTriangleRegion(FHandle, ATexture.Handle, @AVertex1, @AVertex2, @AVertex3, @ATexCoord1,
    @ATexCoord2, @ATexCoord3, AColor1, AColor2, AColor3, AEffect);
end;

procedure TCanvas.Quad(const ATexture: TTexture; const AVertices, ATexCoords: TQuad;
  const AColors: TColorRect; const AEffect: TBlendingEffect);
begin
  CanvasTexturedQuad(FHandle, ATexture.Handle, @AVertices, @ATexCoords, @AColors, AEffect);
end;

procedure TCanvas.QuadRegion(const ATexture: TTexture; const AVertices, ATexCoords: TQuad;
  const AColors: TColorRect; const AEffect: TBlendingEffect);
begin
  CanvasTexturedQuadRegion(FHandle, ATexture.Handle, @AVertices, @ATexCoords, @AColors, AEffect);
end;

procedure TCanvas.TexturedRoundRect(const ATexture: TTexture; const ARect: TFloatRect;
  const ATexCoords: TQuad; const AColors: TColorRect; const ARadius: Single; const ASteps: Integer;
  const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect);
begin
  CanvasTexturedRoundRect(FHandle, ATexture.Handle, @ARect, @ATexCoords, @AColors, ARadius, ASteps, AEffect);
end;

procedure TCanvas.TexturedRoundRectRegion(const ATexture: TTexture; const ARect: TFloatRect;
  const ATexCoords: TQuad; const AColors: TColorRect; const ARadius: Single; const ASteps: Integer;
  const AEffect: TBlendingEffect = CanvasDefaultBlendingEffect);
begin
  CanvasTexturedRoundRectRegion(FHandle, ATexture.Handle, @ARect, @ATexCoords, @AColors, ARadius, ASteps,
    AEffect);
end;

procedure TCanvas.QuadImage(const AImageAtlas: TImageAtlas; const AVertices: TQuad;
  const ARegionIndex: Integer; const AColors: TColorRect; const ASourceRect: TFloatRect;
  const AModifiers: TImageAttributes; const AEffect: TBlendingEffect);
begin
  CanvasQuadImage(FHandle, AImageAtlas.Handle, @AVertices, ARegionIndex, @AColors, @ASourceRect,
    Byte(AModifiers), AEffect);
end;

procedure TCanvas.QuadImage(const AImageAtlas: TImageAtlas; const AVertices: TQuad;
  const ARegionIndex: Integer; const AColors: TColorRect; const AEffect: TBlendingEffect);
begin
  CanvasQuadImage(FHandle, AImageAtlas.Handle, @AVertices, ARegionIndex, @AColors, nil, 0, AEffect);
end;

function TCanvas.SetSamplerState(const ASamplerState: TCanvasSamplerState): Boolean;
begin
  Result := CanvasSetSamplerState(FHandle, @ASamplerState);
end;

function TCanvas.GetSamplerState(var ASamplerState: TCanvasSamplerState): Boolean;
begin
  Result := CanvasGetSamplerState(FHandle, @ASamplerState);
end;

procedure TCanvas.ResetSamplerState;
begin
  CanvasResetSamplerState(FHandle);
end;

procedure TCanvas.Flush;
begin
  CanvasFlush(FHandle);
end;

procedure TCanvas.Reset;
begin
  CanvasReset(FHandle);
end;

procedure TCanvas.ResetCache;
begin
  CanvasResetCache(FHandle);
end;

function CanvasInit(const ADevice: TDevice): TCanvas;
begin
  Result.FHandle := CanvasCreate(ADevice.Handle);
end;

{$ENDREGION}
{$REGION 'TTextRenderer'}

procedure TTextRenderer.Free;
begin
  if FHandle <> nil then
  begin
    TextRendererDestroy(FHandle);
    FHandle := nil;
  end;
end;

function TTextRenderer.GetFontSettings: TFontSettings;
begin
  TextRendererGetFontSettings(FHandle, @Result);
end;

procedure TTextRenderer.SetFontSettings(const AFontSettings: TFontSettings);
begin
  TextRendererSetFontSettings(FHandle, @AFontSettings);
end;

function TTextRenderer.GetCanvas: TCanvas;
begin
  Result.FHandle := TextRendererGetCanvas(FHandle);
end;

function TTextRenderer.Extent(const AText: Utf8String; const AModifiers: PTextRenderModifiers): TPoint2f;
begin
  TextRendererExtent(FHandle, PAnsiChar(AText), @Result, AModifiers);
end;

function TTextRenderer.ExtentByPixels(const AText: Utf8String;
  const AModifiers: PTextRenderModifiers): TFloatRect;
begin
  TextRendererExtentByPixels(FHandle, PAnsiChar(AText), @Result, AModifiers);
end;

function TTextRenderer.Rects(const AText: Utf8String; const AExtent: PPoint2f; const ARects: PTextEntryRect;
  const AModifiers: PTextRenderModifiers): Integer;
begin
  Result := TextRendererRects(FHandle, AExtent, ARects, PAnsiChar(AText), AModifiers);
end;

procedure TTextRenderer.Draw(const APosition: TPoint2f; const AText: Utf8String; const AColors: TColorPair;
  const AAlpha: VectorFloat; const AModifiers: PTextRenderModifiers);
begin
  TextRendererDraw(FHandle, @APosition, PAnsiChar(AText), @AColors, AAlpha, AModifiers);
end;

procedure TTextRenderer.DrawAligned(const APosition: TPoint2f; const AText: Utf8String;
  const AColors: TColorPair; const AHorizAlign, AVertAlign: TTextAlignment; const AAlpha: VectorFloat;
  const AAlignToPixels: Boolean; const AModifiers: PTextRenderModifiers);
begin
  TextRendererDrawAligned(FHandle, @APosition, PAnsiChar(AText), @AColors, AHorizAlign, AVertAlign, AAlpha,
    AAlignToPixels, AModifiers);
end;

procedure TTextRenderer.DrawCentered(const APosition: TPoint2f; const AText: Utf8String;
  const AColors: TColorPair; const AAlpha: VectorFloat; const AAlignToPixels: Boolean;
  const AModifiers: PTextRenderModifiers);
begin
  TextRendererDrawCentered(FHandle, @APosition, PAnsiChar(AText), @AColors, AAlpha, AAlignToPixels,
    AModifiers);
end;

procedure TTextRenderer.DrawAlignedByPixels(const APosition: TPoint2f; const AText: Utf8String;
  const AColors: TColorPair; const AHorizAlign, AVertAlign: TTextAlignment; const AAlpha: VectorFloat;
  const AAlignToPixels: Boolean; const AModifiers: PTextRenderModifiers);
begin
  TextRendererDrawAlignedByPixels(FHandle, @APosition, PAnsiChar(AText), @AColors, AHorizAlign, AVertAlign,
    AAlpha, AAlignToPixels, AModifiers);
end;

procedure TTextRenderer.DrawCenteredByPixels(const APosition: TPoint2f; const AText: Utf8String;
  const AColors: TColorPair; const AAlpha: VectorFloat; const AAlignToPixels: Boolean;
  const AModifiers: PTextRenderModifiers);
begin
  TextRendererDrawCenteredByPixels(FHandle, @APosition, PAnsiChar(AText), @AColors, AAlpha, AAlignToPixels,
    AModifiers);
end;

function TextRendererInit(const ACanvas: TCanvas; const ATextureSize: TPoint2i;
  const APixelFormat: TPixelFormat; const AMipMapping: Boolean): TTextRenderer;
begin
  Result.FHandle := TextRendererCreate(ACanvas.Handle, @ATextureSize, APixelFormat, AMipMapping);
end;

{$ENDREGION}
{$REGION 'TGrapher'}

procedure TGrapher.Free;
begin
  if FHandle <> nil then
  begin
    GrapherDestroy(FHandle);
    FHandle := nil;
  end;
end;

function TGrapher.GetTransform: TMatrix4f;
begin
  GrapherGetTransform(FHandle, @Result);
end;

procedure TGrapher.SetTransform(const ATransform: TMatrix4f);
begin
  GrapherSetTransform(FHandle, @ATransform);
end;

function TGrapher.GetBatchCount: Integer;
begin
  Result := GrapherGetBatchCount(FHandle);
end;

function TGrapher.BeginScene: Boolean;
begin
  Result := GrapherBegin(FHandle);
end;

procedure TGrapher.EndScene;
begin
  GrapherEnd(FHandle);
end;

procedure TGrapher.Points(const AVertices: PVector4f; const AColors: PIntColor; const AAngles: PSingle;
  const AElementCount: Integer; const AShape: TPointShape);
begin
  GrapherPoints(FHandle, AVertices, AColors, AAngles, AElementCount, AShape);
end;

procedure TGrapher.Lines(const AVertices: PVector4f; const AColors: PIntColor; const AIndices: PCardinal;
  const AVertexCount, AIndexCount: Integer; const ACaps: TLineCaps);
begin
  GrapherLines(FHandle, AVertices, AColors, AIndices, AVertexCount, AIndexCount, ACaps);
end;

procedure TGrapher.Point(const APosition: TVector3f; const AColor: TIntColor; const ASize: Single;
  const AShape: TPointShape; const AAngle: Single);
begin
  GrapherPoint(FHandle, @APosition, AColor, ASize, AShape, AAngle);
end;

procedure TGrapher.Line(const APosition1, APosition2: TVector3f; const AColor1, AColor2: TIntColor;
  const AThickness1, AThickness2: Single; const ACaps: TLineCaps);
begin
  GrapherLine(FHandle, @APosition1, @APosition2, AColor1, AColor2, AThickness1, AThickness2, ACaps);
end;

procedure TGrapher.Arrow(const ATargetSize: TPoint2f; const AOrigin, ADestination: TVector3f;
  const AColor: TIntColor; const AThickness, ASize: Single; const ACaps: TLineCaps);
begin
  GrapherArrow(FHandle, @ATargetSize, @AOrigin, @ADestination, AColor, AThickness, ASize, ACaps);
end;

procedure TGrapher.BoundingBox(const AVolume: TMatrix4f; const AColor: TIntColor; const AThickness,
  ALength: Single; const ACaps: TLineCaps);
begin
  GrapherBoundingBox(FHandle, @AVolume, AColor, AThickness, ALength, ACaps);
end;

procedure TGrapher.DottedLine(const APosition1, APosition2: TVector3f; const AColor1, AColor2: TIntColor;
  const AThickness1, AThickness2, ASparsity: Single; const AShape: TPointShape; const AAngle: Single);
begin
  GrapherDottedLine(FHandle, @APosition1, @APosition2, AColor1, AColor2, AThickness1, AThickness2, ASparsity,
    AShape, AAngle);
end;

procedure TGrapher.Flush;
begin
  GrapherFlush(FHandle);
end;

procedure TGrapher.Reset;
begin
  GrapherReset(FHandle);
end;

function GrapherInit(const ADevice: TDevice): TGrapher;
begin
  Result.FHandle := GrapherCreate(ADevice.Handle);
end;

{$ENDREGION}
{$REGION 'TGaussianBlur'}

procedure TGaussianBlur.Free;
begin
  if FHandle <> nil then
  begin
    GaussianBlurDestroy(FHandle);
    FHandle := nil;
  end;
end;

function TGaussianBlur.GetSamples: Integer;
begin
  Result := GaussianBlurGetSamples(FHandle);
end;

procedure TGaussianBlur.SetSamples(const ASamples: Integer);
begin
  GaussianBlurSetSamples(FHandle, ASamples);
end;

function TGaussianBlur.GetSigma: Single;
begin
  Result := GaussianBlurGetSigma(FHandle);
end;

procedure TGaussianBlur.SetSigma(const ASigma: Single);
begin
  GaussianBlurSetSigma(FHandle, ASigma);
end;

function TGaussianBlur.Update(const ADestinationTexture, AIntermediaryTexture,
  ASourceTexture: TTexture): Boolean;
begin
  Result := GaussianBlurUpdate(FHandle, ADestinationTexture.Handle, AIntermediaryTexture.Handle,
    ASourceTexture.Handle);
end;

function GaussianBlurInit(const ADevice: TDevice): TGaussianBlur;
begin
  Result.FHandle := GaussianBlurCreate(ADevice.Handle);
end;

{$ENDREGION}
{$REGION 'TGaussianHighlight'}

procedure TGaussianHighlight.Free;
begin
  if FHandle <> nil then
  begin
    GaussianHighlightDestroy(FHandle);
    FHandle := nil;
  end;
end;

function TGaussianHighlight.GetParameters: TGaussianHighlightParameters;
begin
  GaussianHighlightGetParameters(FHandle, @Result);
end;

procedure TGaussianHighlight.SetParameters(const AParameters: TGaussianHighlightParameters);
begin
  GaussianHighlightSetParameters(FHandle, @AParameters);
end;

function TGaussianHighlight.GetRectangle: TFloatRect;
begin
  GaussianHighlightGetRectangle(FHandle, @Result);
end;

procedure TGaussianHighlight.SetRectangle(const ARectangle: TFloatRect);
begin
  GaussianHighlightSetRectangle(FHandle, @ARectangle);
end;

function TGaussianHighlight.Update(const ADestinationTexture, AIntermediaryTexture,
  ASourceTexture: TTexture): Boolean;
begin
  Result := GaussianHighlightUpdate(FHandle, ADestinationTexture.Handle, AIntermediaryTexture.Handle,
    ASourceTexture.Handle);
end;

function GaussianHighlightInit(const ADevice: TDevice): TGaussianHighlight;
begin
  Result.FHandle := GaussianHighlightCreate(ADevice.Handle);
end;

{$ENDREGION}
{$REGION 'TSceneTexture'}

procedure TSceneTexture.Free;
begin
  if FHandle <> nil then
  begin
    SceneTextureDestroy(FHandle);
    FHandle := nil;
  end;
end;

function TSceneTexture.GetSize: TPoint2i;
begin
  SceneTextureGetSize(FHandle, @Result);
end;

function TSceneTexture.GetSamples: Integer;
begin
  Result := SceneTextureGetSamples(FHandle);
end;

function TSceneTexture.GetRendering: Boolean;
begin
  Result := SceneTextureRendering(FHandle);
end;

function TSceneTexture.Retrieve(const ATextureType: TSceneTextureType): TTexture;
begin
  Result.FHandle := SceneTextureRetrieve(FHandle, ATextureType);
end;

function TSceneTexture.BeginScene: Boolean;
begin
  Result := SceneTextureBegin(FHandle);
end;

procedure TSceneTexture.EndScene;
begin
  SceneTextureEnd(FHandle);
end;

{$ENDREGION}
{$REGION 'TShadowTexture'}

function TShadowTexture.GetTechnique: TShadowTechnique;
begin
  Result := ShadowTextureGetTechnique(FHandle);
end;

function TShadowTexture.GetParameters: TShadowParameters;
begin
  ShadowTextureGetParameters(FHandle, @Result);
end;

procedure TShadowTexture.SetParameters(const AParameters: TShadowParameters);
begin
  ShadowTextureSetParameters(FHandle, @AParameters);
end;

function TShadowTexture.Retrieve: TTexture;
begin
  Result := inherited Retrieve({$IFDEF DELPHI_LEGACY}sttShadow{$ELSE}TSceneTextureType.Shadow{$ENDIF});
end;

function TShadowTexture.Filter: Boolean;
begin
  Result := ShadowTextureFilter(FHandle);
end;

function ShadowTextureInit(const ADevice: TDevice; const AShadowTechnique: TShadowTechnique;
  const ASize: TPoint2i; const ASamples: Integer; const ADepthReversed: Boolean): TShadowTexture;
begin
  Result.FHandle := ShadowTextureCreate(ADevice.Handle, AShadowTechnique, @ASize, ASamples, ADepthReversed);
end;

{$ENDREGION}
{$REGION 'TModelTexture'}

function TModelTexture.GetFormat: TPixelFormat;
begin
  Result := ModelTextureGetFormat(FHandle);
end;

function TModelTexture.GetDepthStencil: TPixelFormat;
begin
  Result := ModelTextureGetDepthStencil(FHandle);
end;

function TModelTexture.SetSize(const ASize: TPoint2i): Boolean;
begin
  Result := ModelTextureSetSize(FHandle, @ASize);
end;

function TModelTexture.Present: Boolean;
begin
  Result := ModelTexturePresent(FHandle);
end;

function ModelTextureInit(const ADevice: TDevice; const ASize: TPoint2i; const AFormat: TPixelFormat;
  const ADepthStencil: TPixelFormat; const ASamples: Integer): TModelTexture;
begin
  Result.FHandle := ModelTextureCreate(ADevice.Handle, @ASize, AFormat, ADepthStencil, ASamples);
end;

{$ENDREGION}
{$REGION 'TGlassTexture'}

function TGlassTexture.GetTechnique: TGlassTechnique;
begin
  Result := GlassTextureGetTechnique(FHandle);
end;

function TGlassTexture.GetAttributes: TSceneAttributes;
begin
  Result := GlassTextureGetAttributes(FHandle);
end;

function TGlassTexture.GetFormat: TPixelFormat;
begin
  Result := GlassTextureGetFormat(FHandle);
end;

function TGlassTexture.GetBackground: TFloatColor;
begin
  GlassTextureGetBackground(FHandle, @Result);
end;

procedure TGlassTexture.SetBackground(const ABackground: TFloatColor);
begin
  GlassTextureSetBackground(FHandle, @ABackground);
end;

function TGlassTexture.SetSize(const ASize: TPoint2i): Boolean;
begin
  Result := GlassTextureSetSize(FHandle, @ASize);
end;

procedure TGlassTexture.SetInput(const ATextureType: TSceneTextureType;
  const ATexture: TTexture);
begin
  GlassTextureSetInput(FHandle, ATextureType, ATexture.Handle);
end;

function TGlassTexture.Present: Boolean;
begin
  Result := GlassTexturePresent(FHandle);
end;

function GlassTextureInit(const ADevice: TDevice; const ATechnique: TGlassTechnique; const ASize: TPoint2i;
  const AAttributes: TSceneAttributes; const AFormat: TPixelFormat; const ASamples: Integer): TGlassTexture;
begin
  Result.FHandle := GlassTextureCreate(ADevice.Handle, ATechnique, @ASize, AAttributes, AFormat, ASamples);
end;

{$ENDREGION}
{$REGION 'TScene'}

procedure TScene.Free;
begin
  if FHandle <> nil then
  begin
    SceneDestroy(FHandle);
    FHandle := nil;
  end;
end;

function TScene.GetShadowTechnique: TShadowTechnique;
begin
  Result := SceneGetShadowTechnique(FHandle);
end;

function TScene.GetShadowParameters: TShadowParameters;
begin
  SceneGetShadowParameters(FHandle, @Result);
end;

procedure TScene.SetShadowParameters(const AParameters: TShadowParameters);
begin
  SceneSetShadowParameters(FHandle, @AParameters);
end;

function TScene.GetAttributes: TSceneAttributes;
begin
  Result := SceneGetAttributes(FHandle);
end;

function TScene.GetWorld: TMatrix4f;
begin
  SceneGetWorld(FHandle, @Result);
end;

procedure TScene.SetWorld(const AWorld: TMatrix4f);
begin
  SceneSetWorld(FHandle, @AWorld);
end;

function TScene.GetView: TMatrix4f;
begin
  SceneGetView(FHandle, @Result);
end;

procedure TScene.SetView(const AView: TMatrix4f);
begin
  SceneSetView(FHandle, @AView);
end;

function TScene.GetProjection: TMatrix4f;
begin
  SceneGetProjection(FHandle, @Result);
end;

procedure TScene.SetProjection(const AProjection: TMatrix4f);
begin
  SceneSetProjection(FHandle, @AProjection);
end;

function TScene.GetLightView: TMatrix4f;
begin
  SceneGetLightView(FHandle, @Result);
end;

procedure TScene.SetLightView(const ALightView: TMatrix4f);
begin
  SceneSetLightView(FHandle, @ALightView);
end;

function TScene.GetLightProjection: TMatrix4f;
begin
  SceneGetLightProjection(FHandle, @Result);
end;

procedure TScene.SetLightProjection(const ALightProjection: TMatrix4f);
begin
  SceneSetLightProjection(FHandle, @ALightProjection);
end;

function TScene.GetSceneProgram: TProgram;
begin
  Result.FHandle := SceneGetProgram(FHandle);
end;

function TScene.GetRendering: Boolean;
begin
  Result := SceneRendering(FHandle);
end;

function TScene.Instances(const ATransforms: TMatrix4fArray; const AColors: TFloatColorArray): Boolean;
var
  LColors: PFloatColor;
begin
  if (Length(ATransforms) > 0) and ((Length(ATransforms) = Length(AColors)) or (Length(AColors) = 0)) then
  begin
    if Length(AColors) > 0 then
      LColors := @AColors[0]
    else
      LColors := nil;

    Result := SceneInstances(FHandle, @ATransforms[0], LColors, Length(ATransforms));
  end
  else
    Result := False;
end;

function TScene.Instances(const ATransforms: array of TMatrix4f;
  const AColors: array of TFloatColor): Boolean;
var
  LColors: PFloatColor;
begin
  if (Length(ATransforms) > 0) and ((Length(ATransforms) = Length(AColors)) or (Length(AColors) = 0)) then
  begin
    if Length(AColors) > 0 then
      LColors := @AColors[0]
    else
      LColors := nil;

    Result := SceneInstances(FHandle, @ATransforms[0], LColors, Length(ATransforms));
  end
  else
    Result := False;
end;

function TScene.Instances(const ATransforms: PMatrix4f; const AColors: PFloatColor;
  const AInstanceCount: Integer): Boolean;
begin
  Result := SceneInstances(FHandle, ATransforms, AColors, AInstanceCount);
end;

function TScene.BeginScene: Boolean;
begin
  Result := SceneBegin(FHandle);
end;

procedure TScene.EndScene;
begin
  SceneEnd(FHandle);
end;

{$ENDREGION}
{$REGION 'TShadowScene'}

function ShadowSceneInit(const ADevice: TDevice; const ATechnique: TShadowTechnique;
  const AAttributes: TSceneAttributes; const AVertexElements: array of TVertexElement): TScene;
var
  LVertexElements: PVertexElement;
  LVertexElementCount: Cardinal;
begin
  InterpretVertexElements(AVertexElements, LVertexElements, LVertexElementCount);

  Result.FHandle := ShadowSceneCreate(ADevice.Handle, ATechnique, AAttributes, LVertexElements,
    LVertexElementCount);
end;

function ShadowSceneInit(const ADevice: TDevice; const ATechnique: TShadowTechnique;
  const AAttributes: TSceneAttributes; const AVertexElements: TVertexElements): TScene;
var
  LVertexElements: PVertexElement;
  LVertexElementCount: Cardinal;
begin
  InterpretVertexElements(AVertexElements, LVertexElements, LVertexElementCount);

  Result.FHandle := ShadowSceneCreate(ADevice.Handle, ATechnique, AAttributes, LVertexElements,
    LVertexElementCount);
end;

{$ENDREGION}
{$REGION 'TModelScene'}

function TModelScene.GetLight(const AIndex: Integer): TSceneLight;
begin
  if not ModelSceneGetLight(FHandle, AIndex, @Result) then
    FillChar(Result, SizeOf(TSceneLight), 0);
end;

procedure TModelScene.SetLight(const AIndex: Integer; const ALight: TSceneLight);
begin
  ModelSceneSetLight(FHandle, AIndex, @ALight);
end;

function TModelScene.GetTexture(const ATextureType: TSceneTextureType): TTexture;
begin
  Result.FHandle := ModelSceneGetTexture(FHandle, ATextureType);
end;

procedure TModelScene.SetTexture(const ATextureType: TSceneTextureType; const ATexture: TTexture);
begin
  ModelSceneSetTexture(FHandle, ATextureType, ATexture.FHandle);
end;

function ModelSceneInit(const ADevice: TDevice; const AShadowTechnique: TShadowTechnique;
  const AAttributes: TSceneAttributes; const AVertexElements: array of TVertexElement): TModelScene;
var
  LVertexElements: PVertexElement;
  LVertexElementCount: Cardinal;
begin
  InterpretVertexElements(AVertexElements, LVertexElements, LVertexElementCount);

  Result.FHandle := ModelSceneCreate(ADevice.Handle, AShadowTechnique, AAttributes, LVertexElements,
    LVertexElementCount);
end;

function ModelSceneInit(const ADevice: TDevice; const AShadowTechnique: TShadowTechnique;
  const AAttributes: TSceneAttributes; const AVertexElements: TVertexElements): TModelScene;
var
  LVertexElements: PVertexElement;
  LVertexElementCount: Cardinal;
begin
  InterpretVertexElements(AVertexElements, LVertexElements, LVertexElementCount);

  Result.FHandle := ModelSceneCreate(ADevice.Handle, AShadowTechnique, AAttributes, LVertexElements,
    LVertexElementCount);
end;

{$ENDREGION}
{$REGION 'TGlassScene'}

function TGlassScene.GetTechnique: TGlassTechnique;
begin
  Result := GlassSceneGetTechnique(FHandle);
end;

function TGlassScene.GetSamples: Integer;
begin
  Result := GlassSceneGetSamples(FHandle);
end;

function TGlassScene.GetAssociate: TGlassTexture;
begin
  Result.FHandle := GlassSceneGetAssociate(FHandle);
end;

procedure TGlassScene.SetAssociate(const AAssociate: TGlassTexture);
begin
  GlassSceneSetAssociate(FHandle, AAssociate.Handle);
end;

function TGlassScene.Iterate: Boolean;
begin
  Result := GlassSceneIterate(FHandle);
end;

function GlassSceneInit(const ADevice: TDevice; const AGlassTechnique: TGlassTechnique;
  const AShadowTechnique: TShadowTechnique; const AAttributes: TSceneAttributes; const ASamples: Integer;
  const AVertexElements: array of TVertexElement): TGlassScene;
var
  LVertexElements: PVertexElement;
  LVertexElementCount: Cardinal;
begin
  InterpretVertexElements(AVertexElements, LVertexElements, LVertexElementCount);

  Result.FHandle := GlassSceneCreate(ADevice.Handle, AGlassTechnique, AShadowTechnique, AAttributes,
    ASamples, LVertexElements, LVertexElementCount);
end;

function GlassSceneInit(const ADevice: TDevice; const AGlassTechnique: TGlassTechnique;
  const AShadowTechnique: TShadowTechnique; const AAttributes: TSceneAttributes;
  const ASamples: Integer; const AVertexElements: TVertexElements): TGlassScene;
var
  LVertexElements: PVertexElement;
  LVertexElementCount: Cardinal;
begin
  InterpretVertexElements(AVertexElements, LVertexElements, LVertexElementCount);

  Result.FHandle := GlassSceneCreate(ADevice.Handle, AGlassTechnique, AShadowTechnique, AAttributes,
    ASamples, LVertexElements, LVertexElementCount);
end;

{$ENDREGION}
{$REGION 'TVolumeSurfaceShadowScene'}

function TVolumeSurfaceShadowScene.GetParameters: TVolumeSurfaceParameters;
begin
  VolumeSurfaceSceneGetParameters(FHandle, @Result);
end;

procedure TVolumeSurfaceShadowScene.SetParameters(const AParameters: TVolumeSurfaceParameters);
begin
  VolumeSurfaceSceneSetParameters(FHandle, @AParameters);
end;

procedure TVolumeSurfaceShadowScene.SetTexture(const ATexture: TTexture);
begin
  VolumeSurfaceSceneSetTexture(FHandle, ATexture.Handle);
end;

procedure TVolumeSurfaceShadowScene.Execute;
begin
  VolumeSurfaceSceneExecute(FHandle);
end;

function VolumeSurfaceShadowSceneInit(const ADevice: TDevice; const ATechnique: TShadowTechnique;
  const AAttributes: TSceneAttributes): TVolumeSurfaceShadowScene;
begin
  Result.FHandle := VolumeSurfaceShadowSceneCreate(ADevice.Handle, ATechnique, AAttributes);
end;

{$ENDREGION}
{$REGION 'TVolumeSurfaceScene'}

function TVolumeSurfaceScene.GetParameters: TVolumeSurfaceParameters;
begin
  VolumeSurfaceSceneGetParameters(FHandle, @Result);
end;

procedure TVolumeSurfaceScene.SetParameters(const AParameters: TVolumeSurfaceParameters);
begin
  VolumeSurfaceSceneSetParameters(FHandle, @AParameters);
end;

procedure TVolumeSurfaceScene.SetTexture(const ATexture: TTexture);
begin
  VolumeSurfaceSceneSetTexture(FHandle, ATexture.Handle);
end;

procedure TVolumeSurfaceScene.Execute;
begin
  VolumeSurfaceSceneExecute(FHandle);
end;

function VolumeSurfaceSceneInit(const ADevice: TDevice; const AShadowTechnique: TShadowTechnique;
  const AAttributes: TSceneAttributes): TVolumeSurfaceScene;
begin
  Result.FHandle := VolumeSurfaceSceneCreate(ADevice.Handle, AShadowTechnique, AAttributes);
end;

{$ENDREGION}
{$REGION 'TTimer'}

procedure TimerTimerEvent(AHandle: TLibraryClassHandle; AUser: Pointer); cdecl;
begin
  if (AUser <> nil) and Assigned(TTimer(AUser).FOnTimer) then
    TTimer(AUser).FOnTimer(TTimer(AUser));
end;

procedure TimerProcessEvent(AHandle: TLibraryClassHandle; AUser: Pointer); cdecl;
begin
  if (AUser <> nil) and Assigned(TTimer(AUser).FOnProcess) then
    TTimer(AUser).FOnProcess(TTimer(AUser));
end;

constructor TTimer.Create;
begin
  inherited;
  FHandle := TimerCreate;
end;

destructor TTimer.Destroy;
begin
  if FHandle <> nil then
  begin
    TimerDestroy(FHandle);
    FHandle := nil;
  end;
  inherited;
end;

procedure TTimer.UpdateEvents;
begin
  if Assigned(FOnTimer) and Assigned(FOnProcess) then
    TimerSetEvents(FHandle, TimerTimerEvent, TimerProcessEvent, Self)
  else if Assigned(FOnTimer) and (not Assigned(FOnProcess)) then
    TimerSetEvents(FHandle, TimerTimerEvent, nil, Self)
  else if (not Assigned(FOnTimer)) and Assigned(FOnProcess) then
    TimerSetEvents(FHandle, nil, TimerProcessEvent, Self)
  else
    TimerSetEvents(FHandle, nil, nil, nil);
end;

procedure TTimer.SetOnTimer(const AOnTimer: TTimerEvent);
begin
  if @FOnTimer <> @AOnTimer then
  begin
    FOnTimer := AOnTimer;
    UpdateEvents;
  end;
end;

procedure TTimer.SetOnProcess(const AOnProcess: TTimerEvent);
begin
  if @FOnProcess <> @AOnProcess then
  begin
    FOnProcess := AOnProcess;
    UpdateEvents;
  end;
end;

function TTimer.GetSpeed: Double;
begin
  Result := TimerGetSpeed(FHandle);
end;

procedure TTimer.SetSpeed(const ASpeed: Double);
begin
  TimerSetSpeed(FHandle, ASpeed);
end;

function TTimer.GetMaxFrameRate: Integer;
begin
  Result := TimerGetMaxFrameRate(FHandle);
end;

procedure TTimer.SetMaxFrameRate(const AMaxFrameRate: Integer);
begin
  TimerSetMaxFrameRate(FHandle, AMaxFrameRate);
end;

function TTimer.GetFrameRate: Integer;
begin
  Result := TimerGetFrameRate(FHandle);
end;

function TTimer.GetLatency: UInt64;
begin
  Result := TimerGetLatency(FHandle);
end;

procedure TTimer.Execute(const AActive: Boolean; const AAllowSleep: Boolean);
begin
  TimerExecute(FHandle, AActive, AAllowSleep);
end;

procedure TTimer.Process(const ASingleCallOnly: Boolean);
begin
  TimerProcess(FHandle, ASingleCallOnly);
end;

procedure TTimer.Reset;
begin
  TimerReset(FHandle);
end;

{$ENDREGION}
{$REGION 'TObjectCamera'}

procedure TObjectCamera.Free;
begin
  if FHandle <> nil then
  begin
    ObjectCameraDestroy(FHandle);
    FHandle := nil;
  end;
end;

function TObjectCamera.GetPosition: TVector3f;
begin
  ObjectCameraGetPosition(FHandle, @Result);
end;

procedure TObjectCamera.SetPosition(const APosition: TVector3f);
begin
  ObjectCameraSetPosition(FHandle, @APosition);
end;

function TObjectCamera.GetRotation: TVector3f;
begin
  ObjectCameraGetRotation(FHandle, @Result);
end;

procedure TObjectCamera.SetRotation(const ARotation: TVector3f);
begin
  ObjectCameraSetRotation(FHandle, @ARotation);
end;

function TObjectCamera.GetDistance: Single;
begin
  Result := ObjectCameraGetDistance(FHandle);
end;

procedure TObjectCamera.SetDistance(const ADistance: Single);
begin
  ObjectCameraSetDistance(FHandle, ADistance);
end;

function TObjectCamera.GetConstraints: TObjectCameraConstraints;
begin
  ObjectCameraGetConstraints(FHandle, @Result);
end;

procedure TObjectCamera.SetConstraints(const AConstraints: TObjectCameraConstraints);
begin
  ObjectCameraSetConstraints(FHandle, @AConstraints);
end;

function TObjectCamera.GetSensitivity: TObjectCameraSensitivity;
begin
  ObjectCameraGetSensitivity(FHandle, @Result);
end;

procedure TObjectCamera.SetSensitivity(const ASensitivity: TObjectCameraSensitivity);
begin
  ObjectCameraSetSensitivity(FHandle, @ASensitivity);
end;

function TObjectCamera.GetView: TMatrix4f;
begin
  ObjectCameraGetView(FHandle, @Result);
end;

procedure TObjectCamera.StartRotation(const ATouch: TPoint2f);
begin
  ObjectCameraStartRotation(FHandle, @ATouch);
end;

procedure TObjectCamera.StartMovement(const ATouch: TPoint2f);
begin
  ObjectCameraStartMovement(FHandle, @ATouch);
end;

function TObjectCamera.Update(const ATouch: TPoint2f): Boolean;
begin
  Result := ObjectCameraUpdate(FHandle, @ATouch);
end;

function TObjectCamera.Stop: Boolean;
begin
  Result := ObjectCameraStop(FHandle);
end;

procedure TObjectCamera.Zoom(const APosition, ASize: TPoint2f; const AProjection: TMatrix4f;
  const ADelta: Single);
begin
  ObjectCameraZoom(FHandle, @APosition, @ASize, @AProjection, ADelta);
end;

procedure TObjectCamera.ZoomOrtho(const APosition, ASize: TPoint2f; const ADistance,
  AAdjustedDistance: Single; const AProjection, AAdjustedProjection: TMatrix4f);
begin
  ObjectCameraZoomOrtho(FHandle, @APosition, @ASize, ADistance, AAdjustedDistance, @AProjection,
    @AAdjustedProjection);
end;

function ObjectCameraInit(const APosition: TVector3f; const ARotation: TVector3f;
  const ADistance: Single): TObjectCamera;
begin
  Result.FHandle := ObjectCameraCreate(@APosition, @ARotation, ADistance);
end;

function ObjectCameraInit(const ADistance: Single): TObjectCamera;
begin
  Result := ObjectCameraInit(ZeroVector3f, Vector3f(-Pi * 0.25, 0.0, 0.0), ADistance);
end;

{$ENDREGION}
{$REGION 'TMeshVoxel'}

procedure TMeshVoxel.Free;
begin
  if FHandle <> nil then
  begin
    MeshVoxelDestroy(FHandle);
    FHandle := nil;
  end;
end;

procedure TMeshVoxel.Extents(out APosition, ASize: TVector3f);
begin
  MeshVoxelExtents(FHandle, @APosition, @ASize);
end;

procedure TMeshVoxel.Visualize(const AVisualizeFunc: TMeshVoxelVisualizeFunc;
  const AUser: Pointer);
begin
  MeshVoxelVisualize(FHandle, AVisualizeFunc, AUser);
end;

function MeshVoxelInitFromFile(const AFileName: Utf8String): TMeshVoxel;
begin
  Result.FHandle := MeshVoxelCreateFromFile(PAnsiChar(AFileName));
end;

function MeshVoxelInitFromMemory(const ABuffer: Pointer; const ASize: Cardinal): TMeshVoxel;
begin
  Result.FHandle := MeshVoxelCreateFromMemory(ABuffer, ASize);
end;

{$ENDREGION}
{$REGION 'TSceneMesh'}

function TSceneMesh.GetName: Utf8String;
begin
  Result := SceneMeshGetName(FHandle);
end;

function TSceneMesh.GetPayload: TObjectPayload;
begin
  Result := SceneMeshGetPayload(FHandle);
end;

function TSceneMesh.GetModel: TMeshModel;
begin
  Result.FHandle := SceneMeshGetModel(FHandle);
end;

function TSceneMesh.GetVoxel: TMeshVoxel;
begin
  Result.FHandle := SceneMeshGetVoxel(FHandle);
end;

function TSceneMesh.GetTags: TMeshMetaTags;
begin
  Result.FHandle := SceneMeshGetTags(FHandle);
end;

function TSceneMesh.GetMinBounds: TVector3f;
begin
  SceneMeshGetBounds(FHandle, @Result, nil);
end;

function TSceneMesh.GetMaxBounds: TVector3f;
begin
  SceneMeshGetBounds(FHandle, nil, @Result);
end;

function TSceneMesh.GetScale: Single;
begin
  Result := SceneMeshGetScale(FHandle);
end;

function TSceneMesh.GetSize: TVector3f;
begin
  SceneMeshGetSize(FHandle, @Result);
end;

{$ENDREGION}
{$REGION 'TSceneMeshesEnumerator'}

constructor TSceneMeshesEnumerator.Create(const AHandle: TLibraryClassHandle);
begin
  FHandle := AHandle;
  FCurrent := -1;
end;

function TSceneMeshesEnumerator.GetCurrent: TSceneMesh;
begin
  Result.FHandle := SceneMeshesGetMesh(FHandle, FCurrent);
end;

function TSceneMeshesEnumerator.MoveNext: Boolean;
begin
  Inc(FCurrent);
  Result := FCurrent < SceneMeshesGetCount(FHandle);
end;

{$ENDREGION}
{$REGION 'TSceneMeshes'}

procedure TSceneMeshes.Free;
begin
  if FHandle <> nil then
  begin
    SceneMeshesDestroy(FHandle);
    FHandle := nil;
  end;
end;

function TSceneMeshes.GetDevice: TDevice;
begin
  Result.FHandle := SceneMeshesGetDevice(FHandle);
end;

function TSceneMeshes.GetCount: Integer;
begin
  Result := SceneMeshesGetCount(FHandle);
end;

function TSceneMeshes.GetMesh(const AIndex: Integer): TSceneMesh;
begin
  Result.FHandle := SceneMeshesGetMesh(FHandle, AIndex);
end;

function TSceneMeshes.GetMeshByName(const AName: Utf8String): TSceneMesh;
begin
  Result.FHandle := SceneMeshesGetMeshByName(FHandle, PAnsiChar(AName));
end;

function TSceneMeshes.GetPayload(const APayload: TObjectPayload): TSceneMesh;
begin
  Result.FHandle := SceneMeshesPayload(FHandle, APayload);
end;

function TSceneMeshes.Add(const AName: Utf8String; var AModel: TMeshModel; var AVoxel: TMeshVoxel;
  var ATags: TMeshMetaTags; const APayload: TObjectPayload; const AMinBounds, AMaxBounds: TVector3f;
  const AScale: Single): TSceneMesh;
begin
  Result.FHandle := SceneMeshesAdd(FHandle, PAnsiChar(AName), @AModel.FHandle, @AVoxel.FHandle,
    @ATags.FHandle, APayload, @AMinBounds, @AMaxBounds, AScale);
end;

function TSceneMeshes.AddFromBinaryFile(const AName, AFileName: Utf8String; const APayload: TObjectPayload;
  const AVertexElements: TVertexElements; const AChannel: Cardinal; const AScale: Single): TSceneMesh;
var
  LVertexElements: PVertexElement;
  LVertexElementCount: Cardinal;
begin
  InterpretVertexElements(AVertexElements, LVertexElements, LVertexElementCount);

  Result.FHandle := SceneMeshesAddFromBinaryFile(FHandle, PAnsiChar(AName), PAnsiChar(AFileName), APayload,
    LVertexElements, LVertexElementCount, AChannel, AScale);
end;

function TSceneMeshes.AddFromBinaryFile(const AName, AFileName: Utf8String; const APayload: TObjectPayload;
  const AVertexElements: array of TVertexElement; const AChannel: Cardinal; const AScale: Single): TSceneMesh;
var
  LVertexElements: PVertexElement;
  LVertexElementCount: Cardinal;
begin
  InterpretVertexElements(AVertexElements, LVertexElements, LVertexElementCount);

  Result.FHandle := SceneMeshesAddFromBinaryFile(FHandle, PAnsiChar(AName), PAnsiChar(AFileName), APayload,
    LVertexElements, LVertexElementCount, AChannel, AScale);
end;

function TSceneMeshes.Find(const ASceneMesh: TSceneMesh): Integer;
begin
  Result := SceneMeshesFind(FHandle, ASceneMesh.Handle);
end;

procedure TSceneMeshes.Erase(const ASceneMesh: TSceneMesh);
begin
  SceneMeshesErase(FHandle, ASceneMesh.Handle);
end;

procedure TSceneMeshes.Clear;
begin
  SceneMeshesClear(FHandle);
end;

function TSceneMeshes.GetEnumerator: TSceneMeshesEnumerator;
begin
  Result := TSceneMeshesEnumerator.Create(FHandle);
end;

function SceneMeshesInit(const ADevice: TDevice): TSceneMeshes;
begin
  Result.FHandle := SceneMeshesCreate(ADevice.Handle);
end;

{$ENDREGION}
{$REGION 'TObjectModel'}

function TObjectModel.GetPayload: TObjectPayload;
begin
  Result := ObjectModelGetPayload(FHandle);
end;

function TObjectModel.GetName: Utf8String;
begin
  Result := ObjectModelGetName(FHandle);
end;

procedure TObjectModel.SetName(const AName: Utf8String);
begin
  ObjectModelSetName(FHandle, PAnsiChar(AName));
end;

function TObjectModel.GetOwner: TBaseObject;
begin
  Result.FHandle := ObjectModelGetOwner(FHandle);
end;

function TObjectModel.GetParent: TObjectModel;
begin
  Result.FHandle := ObjectModelGetParent(FHandle);
end;

procedure TObjectModel.SetParent(const AParent: TObjectModel);
begin
  ObjectModelSetParent(FHandle, AParent.FHandle);
end;

function TObjectModel.GetVoxel: TMeshVoxel;
begin
  Result.FHandle := ObjectModelGetVoxel(FHandle);
end;

procedure TObjectModel.SetVoxel(const AMeshVoxel: TMeshVoxel);
begin
  ObjectModelSetVoxel(FHandle, AMeshVoxel.FHandle);
end;

function TObjectModel.GetMesh: TSceneMesh;
begin
  Result.FHandle := ObjectModelGetMesh(FHandle);
end;

procedure TObjectModel.SetMesh(const AMesh: TSceneMesh);
begin
  ObjectModelSetMesh(FHandle, AMesh.Handle);
end;

function TObjectModel.GetMeshName: Utf8String;
begin
  Result := ObjectModelGetMeshName(FHandle);
end;

procedure TObjectModel.SetMeshName(const AName: Utf8String);
begin
  ObjectModelSetMeshName(FHandle, PAnsiChar(AName));
end;

function TObjectModel.GetTransform(const ATransform: TModelTransform): TMatrix4f;
begin
  ObjectModelGetTransform(FHandle, ATransform, @Result);
end;

procedure TObjectModel.SetTransform(const ATransform: TModelTransform; const AMatrix: TMatrix4f);
begin
  ObjectModelSetTransform(FHandle, ATransform, @AMatrix);
end;

function TObjectModel.GetPosition: TVector3f;
begin
  ObjectModelGetPosition(FHandle, @Result);
end;

function TObjectModel.GetDepthBias: Single;
begin
  Result := ObjectModelGetDepthBias(FHandle);
end;

procedure TObjectModel.SetDepthBias(const ADepthBias: Single);
begin
  ObjectModelSetDepthBias(FHandle, ADepthBias);
end;

function TObjectModel.GetAttributes: TModelAttributes;
begin
  Result := TModelAttributes(Byte(ObjectModelGetAttributes(FHandle)));
end;

procedure TObjectModel.SetAttributes(const AAttributes: TModelAttributes);
begin
  ObjectModelSetAttributes(FHandle, Byte(AAttributes));
end;

function TObjectModel.GetOrderIndex: Integer;
begin
  Result := ObjectModelGetOrderIndex(FHandle);
end;

procedure TObjectModel.SetOrderIndex(const AOrderIndex: Integer);
begin
  ObjectModelSetOrderIndex(FHandle, AOrderIndex);
end;

function TObjectModel.GetLayers: UInt64;
begin
  Result := ObjectModelGetLayers(FHandle);
end;

procedure TObjectModel.SetLayers(const ALayers: UInt64);
begin
  ObjectModelSetLayers(FHandle, ALayers);
end;

function TObjectModel.GetChildCount: Integer;
begin
  Result := ObjectModelGetChildCount(FHandle);
end;

function TObjectModel.GetChild(const AIndex: Integer): TObjectModel;
begin
  Result.FHandle := ObjectModelGetChild(FHandle, AIndex);
end;

function TObjectModel.GetSize: TVector3f;
begin
  ObjectModelGetSize(FHandle, @Result);
end;

procedure TObjectModel.SetSize(const ASize: TVector3f);
begin
  ObjectModelSetSize(FHandle, @ASize);
end;

function TObjectModel.GetAligns: TMeshAligns;
begin
  ObjectModelGetAlignments(FHandle, @Result);
end;

procedure TObjectModel.SetAligns(const AAligns: TMeshAligns);
begin
  ObjectModelSetAlignments(FHandle, @AAligns);
end;

procedure TObjectModel.Invalidate;
begin
  ObjectModelInvalidate(FHandle);
end;

{$ENDREGION}
{$REGION 'TObjectModelsEnumerator'}

constructor TObjectModelsEnumerator.Create(const AHandle: TLibraryClassHandle);
begin
  FHandle := AHandle;
  FCurrent := -1;
end;

function TObjectModelsEnumerator.GetCurrent: TObjectModel;
begin
  Result.FHandle := ObjectModelsGetOrder(FHandle, FCurrent);
end;

function TObjectModelsEnumerator.MoveNext: Boolean;
begin
  Inc(FCurrent);
  Result := FCurrent < ObjectModelsGetOrderCount(FHandle);
end;

{$ENDREGION}
{$REGION 'TObjectModels'}

procedure TObjectModels.Free;
begin
  if FHandle <> nil then
  begin
    ObjectModelsDestroy(FHandle);
    FHandle := nil;
  end;
end;

function TObjectModels.GetMeshes: TSceneMeshes;
begin
  Result.FHandle := ObjectModelsGetMeshes(FHandle);
end;

function TObjectModels.GetView: TMatrix4f;
begin
  ObjectModelsGetView(FHandle, @Result);
end;

procedure TObjectModels.SetView(const AView: TMatrix4f);
begin
  ObjectModelsSetView(FHandle, @AView);
end;

function TObjectModels.GetCount: Integer;
begin
  Result := ObjectModelsGetObjectCount(FHandle);
end;

function TObjectModels.GetItem(const AIndex: Integer): TObjectModel;
begin
  Result.FHandle := ObjectModelsGetObject(FHandle, AIndex);
end;

function TObjectModels.GetOrderCount: Integer;
begin
  Result := ObjectModelsGetOrderCount(FHandle);
end;

function TObjectModels.GetOrder(const AIndex: Integer): TObjectModel;
begin
  Result.FHandle := ObjectModelsGetOrder(FHandle, AIndex);
end;

function TObjectModels.GetPayload(const APayload: TObjectPayload): TObjectModel;
begin
  Result.FHandle := ObjectModelsPayload(FHandle, APayload);
end;

function TObjectModels.GetObject(const AName: Utf8String): TObjectModel;
begin
  Result.FHandle := ObjectModelsGetObjectByName(FHandle, PAnsiChar(AName));
end;

function TObjectModels.Add(const AName: Utf8String; const APayload: TObjectPayload): TObjectModel;
begin
  Result.FHandle := ObjectModelsAdd(FHandle, PAnsiChar(AName), APayload);
end;

function TObjectModels.Find(const AObjectModel: TObjectModel): Integer;
begin
  Result := ObjectModelsFind(FHandle, AObjectModel.Handle);
end;

procedure TObjectModels.Erase(const AObjectModel: TObjectModel);
begin
  ObjectModelsErase(FHandle, AObjectModel.Handle);
end;

procedure TObjectModels.Clear;
begin
  ObjectModelsClear(FHandle);
end;

function TObjectModels.Update(const ALayers: UInt64): Boolean;
begin
  Result := ObjectModelsUpdate(FHandle, ALayers);
end;

procedure TObjectModels.Sort(const ACompare: TObjectModelCompare);
begin
  ObjectModelsSort(FHandle, ACompare);
end;

procedure TObjectModels.Sort(const ACompareFunc: TObjectModelCompareFunc; const AUser: Pointer);
begin
  ObjectModelsSortWith(FHandle, ACompareFunc, AUser);
end;

function TObjectModels.Select(const APointer: TRay; const ADistance: PSingle;
  const ATestsCount: PInteger): TObjectModel;
begin
  Result.FHandle := ObjectModelsSelect(FHandle, @APointer.Origin, @APointer.Direction, ADistance,
    ATestsCount);
end;

function TObjectModels.MultiSelect(const APointer: TRay; const ADistance: PSingle;
  const ATestsCount: PInteger): TObjectModel;
begin
  Result.FHandle := ObjectModelsMultiSelect(FHandle, @APointer.Origin, @APointer.Direction,
    ADistance, ATestsCount);
end;

function TObjectModels.GetEnumerator: TObjectModelsEnumerator;
begin
  Result := TObjectModelsEnumerator.Create(FHandle);
end;

function ObjectModelsInit(const AMeshes: TSceneMeshes): TObjectModels;
begin
  Result.FHandle := ObjectModelsCreate(AMeshes.Handle);
end;

{$ENDREGION}
{$REGION 'TApplication'}

function FuncAppCreate(ApplicationClass: TLibraryClassHandle; User: Pointer): TLibraryBool; cdecl;
begin
  if Assigned(TApplication(User).FOnCreate) then
    Result := TApplication(User).FOnCreate(TApplication(User))
  else
    Result := True;
end;

procedure FuncAppDestroy(ApplicationClass: TLibraryClassHandle; User: Pointer); cdecl;
begin
  if Assigned(TApplication(User).FOnDestroy) then
    TApplication(User).FOnDestroy(TApplication(User));
end;

procedure FuncAppRender(ApplicationClass: TLibraryClassHandle; User: Pointer); cdecl;
begin
  if Assigned(TApplication(User).FOnRender) then
    TApplication(User).FOnRender(TApplication(User));
end;

procedure FuncAppMouse(ApplicationClass: TLibraryClassHandle; Event: TMouseEvent; Button: TMouseButton;
  Position: PPoint2i; User: Pointer); cdecl;
begin
  if Assigned(TApplication(User).FOnMouse) then
    TApplication(User).FOnMouse(TApplication(User), Event, Button, Position^);
end;

procedure FuncAppKey(ApplicationClass: TLibraryClassHandle; Event: TKeyEvent; VirtualKey: Integer;
  KeyCode: Word; User: Pointer); cdecl;
begin
  if Assigned(TApplication(User).FOnKey) then
    TApplication(User).FOnKey(TApplication(User), Event, VirtualKey, KeyCode);
end;

function FuncAppIdle(ApplicationClass: TLibraryClassHandle; User: Pointer): TLibraryBool; cdecl;
begin
  if Assigned(TApplication(User).FOnIdle) then
    Result := TApplication(User).FOnIdle(TApplication(User))
  else
    Result := True;
end;

procedure FuncAppResize(ApplicationClass: TLibraryClassHandle; User: Pointer); cdecl;
begin
  if Assigned(TApplication(User).FOnResize) then
    TApplication(User).FOnResize(TApplication(User));
end;

function FuncAppHook(ApplicationClass: TLibraryClassHandle; Event: Pointer;
  User: Pointer): TLibraryBool; cdecl;
begin
  if Assigned(TApplication(User).FOnHook) then
    Result := TApplication(User).FOnHook(TApplication(User), Event)
  else
    Result := False;
end;

// Application members.

constructor TApplication.Create(const ADevice: TDevice; const AWindowTitle, AWindowClassName: Utf8String;
  AInstanceHandle, AIconHandle: TUntypedHandle);
var
  LConfiguration: TApplicationConfiguration;
begin
  LConfiguration.DeviceClass := ADevice.Handle;
  LConfiguration.WindowTitle := PAnsiChar(AWindowTitle);
  LConfiguration.Startup.Win.WindowClassName := PAnsiChar(AWindowClassName);
  LConfiguration.Startup.Win.InstanceHandle := AInstanceHandle;
  LConfiguration.Startup.Win.IconHandle := AIconHandle;

  FHandle := ApplicationCreate(@LConfiguration);
  InitializeEvents;
end;

constructor TApplication.Create(const ADevice: TDevice; const AWindowTitle, AApplicationLink,
  AIconTitle: Utf8String);
var
  LConfiguration: TApplicationConfiguration;
begin
  LConfiguration.DeviceClass := ADevice.Handle;
  LConfiguration.WindowTitle := PAnsiChar(AWindowTitle);
  LConfiguration.Startup.Nx.ApplicationLink := PAnsiChar(AApplicationLink);
  LConfiguration.Startup.Nx.IconTitle := PAnsiChar(AIconTitle);
  LConfiguration.Startup.Nx.ParameterCount := 0;
  LConfiguration.Startup.Nx.ParameterStrings := nil;

  FHandle := ApplicationCreate(@LConfiguration);
  InitializeEvents;
end;

procedure TApplication.InitializeEvents;
var
  LEvents: TApplicationEvents;
begin
  LEvents.EventCreate := FuncAppCreate;
  LEvents.EventDestroy := FuncAppDestroy;
  LEvents.EventRender := FuncAppRender;
  LEvents.EventMouse := FuncAppMouse;
  LEvents.EventKey := FuncAppKey;
  LEvents.EventIdle := FuncAppIdle;
  LEvents.EventResize := FuncAppResize;
  LEvents.EventHook := FuncAppHook;

  ApplicationSetEvents(FHandle, @LEvents, Self);
end;

destructor TApplication.Destroy;
begin
  if FHandle <> nil then
  begin
    ApplicationDestroy(FHandle);
    FHandle := nil;
  end;
  inherited;
end;

function TApplication.GetWindowHandle: TUntypedHandle;
begin
  Result := ApplicationGetWindowHandle(FHandle);
end;

function TApplication.GetTitle: Utf8String;
var
  LLength: Cardinal;
begin
  ApplicationGetTitle(FHandle, nil, @LLength);
  SetLength(Result, LLength);
  ApplicationGetTitle(FHandle, @Result[1], @LLength);
end;

procedure TApplication.SetTitle(const ATitle: Utf8String);
begin
  ApplicationSetTitle(FHandle, PAnsiChar(ATitle));
end;

function TApplication.GetExecutablePath: Utf8String;
var
  LLength: Cardinal;
begin
  ApplicationGetExecutablePath(FHandle, nil, @LLength);
  SetLength(Result, LLength);
  ApplicationGetExecutablePath(FHandle, @Result[1], @LLength);
end;

function TApplication.GetWindowRect: TIntRect;
begin
  ApplicationGetWindowRect(FHandle, @Result);
end;

procedure TApplication.SetWindowRect(const ARect: TIntRect);
begin
  ApplicationSetWindowRect(FHandle, @ARect);
end;

function TApplication.GetClientRect: TIntRect;
begin
  ApplicationGetClientRect(FHandle, @Result);
end;

procedure TApplication.SetClientSize(const ASize: TPoint2i);
begin
  ApplicationSetClientSize(FHandle, @ASize);
end;

function TApplication.GetWindowScale: Single;
begin
  Result := ApplicationGetWindowScale(FHandle);
end;

procedure TApplication.Invalidate;
begin
  ApplicationInvalidate(FHandle);
end;

function TApplication.Execute: Boolean;
begin
  Result := ApplicationExecute(FHandle);
end;

{$ENDREGION}
{$REGION 'TRandomContext'}

procedure TRandomContext.Free;
begin
  if FHandle <> nil then
  begin
    RandomContextDestroy(FHandle);
    FHandle := nil;
  end;
end;

function TRandomContext.Equals(const ARandomContext: TRandomContext): Boolean;
begin
  Result := RandomContextCompare(FHandle, ARandomContext.FHandle);
end;

function TRandomContext.Raw: Cardinal;
begin
  Result := RandomContextGenerate(FHandle);
end;

function TRandomContext.Raw64: UInt64;
begin
  Result := RandomContextGenerate64(FHandle);
end;

function TRandomContext.Value: Single;
begin
  Result := RandomContextGenerateFloat(FHandle);
end;

function TRandomContext.Value(const ARange: Integer): Integer;
begin
  Result := RandomContextGenerateRanged(FHandle, ARange);
end;

function TRandomContext.GaussStart: Single;
begin
  Result := RandomContextGenerateGaussStart(FHandle);
end;

function TRandomContext.GaussEnd: Single;
begin
  Result := RandomContextGenerateGaussEnd(FHandle);
end;

function TRandomContext.GaussMiddle: Single;
begin
  Result := RandomContextGenerateGaussMiddle(FHandle);
end;

function TRandomContext.GaussOmni: Single;
begin
  Result := RandomContextGenerateGaussOmni(FHandle);
end;

function RandomContextInit(const ASeed: Cardinal; const AShuffleCount: Integer): TRandomContext;
begin
  Result.FHandle := RandomContextCreate(ASeed, AShuffleCount);
end;

{$ENDREGION}

end.
