unit AsphyreErrors;

//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Windows, SysUtils;

//---------------------------------------------------------------------------
{.$define DebugLog}

//---------------------------------------------------------------------------
const
 errNone                   =  0; // No error

 errInvalidCall            = -100;

 errEnumDisplayAdapters    = -200; // Could not enumerate existing display adapters.
 errEnumDisplayMode        = -201; // Failed enumerating available display modes.
 errChangeDisplayMode      = -202; // Failed changing the current display mode.
 errRestoreDisplayMode     = -203; // Failed to restore previous display mode.
 errUnsupportedDisplayMode = -204; // The specified display mode is not supported.
 errInvalidWindowHandle    = -205; // The specified window handle is invalid.
 errCreateWindowPalette    = -206; // Could not create a valid window palette.
 errCannotSelectTexture    = -207; // Cannot select the texture for operation.
 errCannotCreateTexture    = -208; // Cannot create texture compatible with specified parameters.
 errUploadTexturePixels    = -209; // Failed uploading texture pixel data.
 errDownloadTexturePixels  = -210; // Failed downloading texture pixel data.
 errUnsupportedOperation   = -211; // The attempted operation is not supported.
 errCreateFrameBuffer      = -212; // Failed to create the frame buffer.
 errCreateRenderBuffer     = -213; // Cannot create a new render buffer.
 errBindRenderBuffer       = -214; // Unable to bind the render buffer.
 errFrameBufferIncomplete  = -215; // Unable to verify the completeness of frame buffer.
 errChangeTextureSize      = -216; // Unable to properly change the size of the texture.

 errGeometryTooComplex     = -300; // The geometry is too complex to be rendered.

 errCreateDirectDraw       = -400; // Cannot create DirectDraw interface.
 errCreateDirect3D         = -401; // Cannot create Direct3D interface.
 errCreateDirect3DDevice   = -402; // Cannot create Direct3D interface.
 errRetreiveDeviceCaps     = -403; // Cannot retreive device capabilities.
 errUnsupportedFormat      = -404; // The specified format is not supported.
 errCreateDepthStencil     = -405; // Failed to create depth-stencil buffer.
 errGetTextureSurface      = -406; // Failed to retreive texture's surface.
 errGetRenderTarget        = -407; // Cannot retreive current render target.
 errGetDepthStencilBuffer  = -408; // Cannot retreive current depth-stencil buffer.
 errSetRenderTarget        = -409; // Unable to set new render target.
 errSetDepthStencilBuffer  = -410; // Unable to set new depth-stencil buffer.
 errAccessTexture          = -411; // Unable to get access to texture's pixels.
 errGetSurfaceDesc         = -412; // Failed to retreive surface description.
 errCooperativeLevel       = -413; // Failed to set the cooperative level.
 errSetDisplayMode         = -414; // Failed to set the specified display mode.
 errCreateSurface          = -415; // New surface could not be created.
 errCreateClipper          = -416; // New clipper object could not be created.
 errRetreiveClipper        = -417; // Unable to retreive existing clipper object.

 errWindowInformation      = -700; // The window information could not be retreived.
 errMonitorInformation     = -701; // The monitor information could not be retreived.

 errOpenFile               = -801; // Failed opening the specified file.
 errCreateFile             = -802; // Failed creating the specified file.
 errLoadFile               = -803; // Error loading the specified file.
 errSaveFile               = -804; // Error saving the specified file.
 errUnpackArchive          = -805; // Failed unpacking the specified record from archive.
 errPackArchive            = -806; // Failed packing the specified record to archive.

//---------------------------------------------------------------------------
 errMeshVertices          = -1000; // The specified mesh has no vertices or number of vertices not divisible by 3.
 errMeshNoIndices         = -1001; // The specified mesh has no indexes.
 errMeshVertexNormals     = -1002; // The mesh has different number of normals than vertices.
 errMeshFaceNormals       = -1003; // The mesh has no face normals or number of face normals incorrect.
 errMeshFaceOrigins       = -1004; // The mesh has no face origins or number of face origins incorrect.
 errMeshTextureIndices    = -1005; // The number of texture indices in mesh does not match number of vertex indices.
 errSceneTooComplex       = -1006; // The scene is too complex to be rendered.

//---------------------------------------------------------------------------
 ErrorQueue = 100;

//---------------------------------------------------------------------------
type
 PErrorQueueItem = ^TErrorQueueItem;
 TErrorQueueItem = record
  ErrorCode: Integer;
  OccurTime: Cardinal;
  Reference: Pointer;
  ClassName: ShortString;
  ErrMethod: ShortString;
 end;

//---------------------------------------------------------------------------
 TAsphyreErrors = class
 private
  Queue: array[0..ErrorQueue - 1] of TErrorQueueItem;
  InitTime: Cardinal;

  function GetError(Index: Integer): PErrorQueueItem;
  procedure Shift();
 public
  property Error[Index: Integer]: PErrorQueueItem read GetError;

  procedure Clear();
  procedure Insert(Code: Integer; Reference: Pointer = nil;
   const AClassName: ShortString = ''; const ErrMethod: ShortString = '');

  function InErrorState(): Boolean;

  constructor Create();
 end;

//---------------------------------------------------------------------------
var
 Errors: TAsphyreErrors = nil;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
{$ifdef DebugLog}
uses
 AsphyreDebug;
{$endif}

//---------------------------------------------------------------------------
constructor TAsphyreErrors.Create();
begin
 inherited;

 InitTime:= GetTickCount();

 Clear();
end;

//---------------------------------------------------------------------------
function TAsphyreErrors.GetError(Index: Integer): PErrorQueueItem;
begin
 if (Index >= 0)and(Index < ErrorQueue) then
  Result:= @Queue[Index] else Result:= nil;
end;

//---------------------------------------------------------------------------
procedure TAsphyreErrors.Clear();
begin
 FillChar(Queue, SizeOf(TErrorQueueItem) * ErrorQueue, 0);
end;

//---------------------------------------------------------------------------
procedure TAsphyreErrors.Shift();
var
 i: Integer;
begin
 for i:= ErrorQueue - 1 downto 1 do
  Queue[i]:= Queue[i - 1];

 FillChar(Queue[0], SizeOf(TErrorQueueItem), 0); 
end;

//---------------------------------------------------------------------------
procedure TAsphyreErrors.Insert(Code: Integer; Reference: Pointer = nil;
 const AClassName: ShortString = ''; const ErrMethod: ShortString = '');
begin
 Shift();

 Queue[0].ErrorCode:= Code;
 Queue[0].Reference:= Reference;
 Queue[0].ClassName:= AClassName;
 Queue[0].ErrMethod:= ErrMethod;
 Queue[0].OccurTime:= GetTickCount() - InitTime;

 {$ifdef DebugLog}
 DebugLog('Asphyre Error 0x' + IntToHex(Integer(Reference), 8) + ': ' +
  AClassName + '.' + ErrMethod + ' - Code ' + IntToStr(Code));
 {$endif}
end;

//---------------------------------------------------------------------------
function TAsphyreErrors.InErrorState(): Boolean;
begin
 Result:= Queue[0].ErrorCode <> errNone;
end;

//---------------------------------------------------------------------------
initialization
 Errors:= TAsphyreErrors.Create();

//---------------------------------------------------------------------------
finalization
 FreeAndNil(Errors);

//---------------------------------------------------------------------------
end.
