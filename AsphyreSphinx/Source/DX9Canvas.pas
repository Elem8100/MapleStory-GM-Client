unit DX9Canvas;

//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Windows, Direct3D9, AbstractCanvas, Vectors2, AsphyreTypes, AsphyreUtils,
 AbstractTextures;

//---------------------------------------------------------------------------
const
 // The following parameters roughly affect the rendering performance. The
 // higher values means that more primitives will fit in cache, but it will
 // also occupy more bandwidth, even when few primitives are rendered.
 //
 // These parameters can be fine-tuned in a finished product to improve the
 // overall performance.
 MaxCachedPrimitives = 3072;
 MaxCachedIndices    = 4096;
 MaxCachedVertices   = 4096;

//---------------------------------------------------------------------------
// The following option controls the behavior of antialiased lines.
// Enable the option for compatibility with DirectX 7 wrapper.
// Also note that antialiased lines don't work on Intel GMA X3100.
//---------------------------------------------------------------------------
{$define NoAntialiasedLines}

//---------------------------------------------------------------------------
type
 TDrawingMode = (dmUnknown, dmPoints, dmLines, dmTriangles);

//---------------------------------------------------------------------------
 TDX9Canvas = class(TAsphyreCanvas)
 private
  VertexBuffer: IDirect3DVertexBuffer9;
  IndexBuffer : IDirect3DIndexBuffer9;

  VertexArray : Pointer;
  IndexArray  : Pointer;

  DrawingMode: TDrawingMode;

  FVertexCache: Integer;
  FIndexCache : Integer;
  FVertexCount: Integer;
  FIndexCount : Integer;

  FPrimitives   : Integer;
  FMaxPrimitives: Integer;

  ActiveTex   : TAsphyreCustomTexture;
  CachedTex   : TAsphyreCustomTexture;
  CachedEffect: TDrawingEffect;
  QuadMapping : TPoint4;

  procedure InitCacheSpec();
  procedure PrepareVertexArray();

  procedure CreateStaticObjects();
  procedure DestroyStaticObjects();

  function CreateDynamicBuffers(): Boolean;
  procedure DestroyDynamicBuffers();

  function UploadVertexBuffer(): Boolean;
  function UploadIndexBuffer(): Boolean;
  procedure DrawBuffers();

  function NextVertexEntry(): Pointer;
  procedure AddIndexEntry(Index: Integer);
  function RequestCache(Mode: TDrawingMode; Vertices, Indices: Integer;
   Effect: TDrawingEffect; Texture: TAsphyreCustomTexture): Boolean;

  procedure SetEffectStates(Effect: TDrawingEffect);
 protected
  function HandleDeviceCreate(): Boolean; override;
  procedure HandleDeviceDestroy(); override;
  function HandleDeviceReset(): Boolean; override;
  procedure HandleDeviceLost(); override;

  procedure HandleBeginScene(); override;
  procedure HandleEndScene(); override;

  procedure GetViewport(out x, y, Width, Height: Integer); override;
  procedure SetViewport(x, y, Width, Height: Integer); override;

  function GetAntialias(): Boolean; override;
  procedure SetAntialias(const Value: Boolean); override;
  function GetMipMapping(): Boolean; override;
  procedure SetMipMapping(const Value: Boolean); override;
 public
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
  procedure DrawRotateC(Texture: TAsphyreCustomTexture; X, Y, Angle: Real;
  const Color: TColor4; Effect: TDrawingEffect); override;
  procedure Flush(); override;
  procedure ResetStates(); override;

  constructor Create(); override;
  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
implementation

//--------------------------------------------------------------------------
uses
 DXTypes, DX9Types, AsphyreErrors;

//--------------------------------------------------------------------------
const
 VertexFVFType = D3DFVF_XYZRHW or D3DFVF_DIFFUSE or D3DFVF_TEX1;

//--------------------------------------------------------------------------
type
 PVertexRecord = ^TVertexRecord;
 TVertexRecord = record
  Vertex: TD3DVector;
  rhw   : Single;
  Color : Longword;
  u, v  : Single;
 end;

//--------------------------------------------------------------------------
constructor TDX9Canvas.Create();
begin
 inherited;

 VertexArray := nil;
 IndexArray  := nil;
 VertexBuffer:= nil;
 IndexBuffer := nil;
end;

//---------------------------------------------------------------------------
destructor TDX9Canvas.Destroy();
begin
 DestroyDynamicBuffers();
 DestroyStaticObjects();

 inherited;
end;

//---------------------------------------------------------------------------
procedure TDX9Canvas.InitCacheSpec();
begin
 with Caps9 do
  begin
   FMaxPrimitives:= Min2(MaxPrimitiveCount, MaxCachedPrimitives);
   FVertexCache  := Min2(MaxVertexIndex, MaxCachedVertices);
   FIndexCache   := Min2(MaxVertexIndex, MaxCachedIndices);
  end;
end;

//---------------------------------------------------------------------------
procedure TDX9Canvas.PrepareVertexArray();
var
 Entry: PVertexRecord;
 Index: Integer;
begin
 Entry:= VertexArray;
 for Index:= 0 to MaxCachedVertices - 1 do
  begin
   FillChar(Entry^, SizeOf(TVertexRecord), 0);

   Entry^.Vertex.z:= 0.0;
   Entry^.rhw     := 1.0;

   Inc(Entry);
  end;
end;

//---------------------------------------------------------------------------
procedure TDX9Canvas.CreateStaticObjects();
begin
 ReallocMem(VertexArray, FVertexCache * SizeOf(TVertexRecord));
 FillChar(VertexArray^, FVertexCache * SizeOf(TVertexRecord), 0);

 ReallocMem(IndexArray, FIndexCache * SizeOf(Word));
 FillChar(IndexArray^, FIndexCache * SizeOf(Word), 0);

 PrepareVertexArray();
end;

//---------------------------------------------------------------------------
procedure TDX9Canvas.DestroyStaticObjects();
begin
 if (IndexArray <> nil) then
  begin
   FreeMem(IndexArray);
   IndexArray:= nil;
  end;

 if (VertexArray <> nil) then
  begin
   FreeMem(VertexArray);
   VertexArray:= nil;
  end;
end;

//--------------------------------------------------------------------------
function TDX9Canvas.CreateDynamicBuffers(): Boolean;
begin
 // -> Dynamic Vertex Buffer
 Result:= Succeeded(Device9.CreateVertexBuffer(FVertexCache *
  SizeOf(TVertexRecord), D3DUSAGE_WRITEONLY or D3DUSAGE_DYNAMIC,
  VertexFVFType, D3DPOOL_DEFAULT, VertexBuffer, nil));
 if (not Result) then Exit;

 // -> Dynamic Index Buffer
 Result:= Succeeded(Device9.CreateIndexBuffer(FIndexCache *
  SizeOf(Word), D3DUSAGE_WRITEONLY or D3DUSAGE_DYNAMIC, D3DFMT_INDEX16,
  D3DPOOL_DEFAULT, IndexBuffer, nil));
end;

//---------------------------------------------------------------------------
procedure TDX9Canvas.DestroyDynamicBuffers();
begin
 if (IndexBuffer <> nil) then IndexBuffer:= nil;
 if (VertexBuffer <> nil) then VertexBuffer:= nil;
end;

//---------------------------------------------------------------------------
procedure TDX9Canvas.ResetStates();
begin
 FVertexCount:= 0;
 FIndexCount := 0;
 FPrimitives := 0;
 DrawingMode := dmUnknown;
 CachedEffect:= deUnknown;
 CachedTex   := nil;
 ActiveTex   := nil;

 with Device9 do
  begin
   // Disable 3D fancy stuff.
   SetRenderState(D3DRS_LIGHTING,  iFalse);
   SetRenderState(D3DRS_CULLMODE,  D3DCULL_NONE);
   SetRenderState(D3DRS_ZENABLE,   D3DZB_FALSE);
   SetRenderState(D3DRS_FOGENABLE, iFalse);

   {$ifdef NoAntialiasedLines}
   SetRenderState(D3DRS_ANTIALIASEDLINEENABLE, iFalse);
   {$else}
   SetRenderState(D3DRS_ANTIALIASEDLINEENABLE, iTrue);
   {$endif}

   // Enable Alpha-testing.
   SetRenderState(D3DRS_ALPHATESTENABLE, iTrue);
   SetRenderState(D3DRS_ALPHAFUNC, D3DCMP_GREATEREQUAL);
   SetRenderState(D3DRS_ALPHAREF,  $00000001);

   // Default alpha-blending behavior
   SetRenderState(D3DRS_ALPHABLENDENABLE, iTrue);

   // Alpha-blending stages for COLOR component.
   SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
   SetTextureStageState(0, D3DTSS_COLORARG1, D3DTA_TEXTURE);
   SetTextureStageState(0, D3DTSS_COLORARG2, D3DTA_DIFFUSE);

   SetTextureStageState(1, D3DTSS_COLOROP, D3DTOP_DISABLE);

   // Alpha-blending stages for ALPHA component.
   SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
   SetTextureStageState(0, D3DTSS_ALPHAARG1, D3DTA_TEXTURE);
   SetTextureStageState(0, D3DTSS_ALPHAARG2, D3DTA_DIFFUSE);

   SetTextureStageState(1, D3DTSS_ALPHAOP, D3DTOP_DISABLE);

   // Texture filtering flags.
   SetSamplerState(0, D3DSAMP_MAGFILTER, D3DTEXF_LINEAR);
   SetSamplerState(0, D3DSAMP_MINFILTER, D3DTEXF_LINEAR);
   SetSamplerState(0, D3DSAMP_MIPFILTER, D3DTEXF_NONE);

   // Triangle fill mode.
   SetRenderState(D3DRS_FILLMODE, D3DFILL_SOLID);
  end;
end;

//--------------------------------------------------------------------------
function TDX9Canvas.HandleDeviceCreate(): Boolean;
begin
 InitCacheSpec();
 CreateStaticObjects();

 Result:= True;
end;

//--------------------------------------------------------------------------
procedure TDX9Canvas.HandleDeviceDestroy();
begin
 DestroyStaticObjects();
end;

//--------------------------------------------------------------------------
function TDX9Canvas.HandleDeviceReset(): Boolean;
begin
 Result:= CreateDynamicBuffers();
end;

//--------------------------------------------------------------------------
procedure TDX9Canvas.HandleDeviceLost();
begin
 DestroyDynamicBuffers();
end;

//--------------------------------------------------------------------------
procedure TDX9Canvas.HandleBeginScene();
begin
 ResetStates();
end;

//--------------------------------------------------------------------------
procedure TDX9Canvas.HandleEndScene();
begin
 Flush();
end;

//---------------------------------------------------------------------------
procedure TDX9Canvas.GetViewport(out x, y, Width, Height: Integer);
var
 vp: TD3DViewport9;
begin
 if (Device9 = nil) then
  begin
   x:= 0; y:= 0; Width:= 0; Height:= 0;
   Exit;
  end;

 FillChar(vp, SizeOf(vp), 0);
 Device9.GetViewport(vp);

 x:= vp.X;
 y:= vp.Y;

 Width := vp.Width;
 Height:= vp.Height;
end;

//---------------------------------------------------------------------------
procedure TDX9Canvas.SetViewport(x, y, Width, Height: Integer);
var
 vp: TD3DViewport9;
begin
 if (Device9 = nil) then Exit;

 Flush();

 vp.X:= x;
 vp.Y:= y;
 vp.Width := Width;
 vp.Height:= Height;
 vp.MinZ:= 0.0;
 vp.MaxZ:= 1.0;

 Device9.SetViewport(vp);
end;

//---------------------------------------------------------------------------
function TDX9Canvas.GetAntialias(): Boolean;
var
 MagFlt, MinFlt: Cardinal;
begin
 if (Device9 = nil) then
  begin
   Result:= False;
   Exit;
  end;

 Device9.GetSamplerState(0, D3DSAMP_MAGFILTER, MagFlt);
 Device9.GetSamplerState(0, D3DSAMP_MINFILTER, MinFlt);

 Result:= True;

 if (MagFlt = D3DTEXF_POINT)or(MinFlt = D3DTEXF_POINT) then Result:= False;
end;

//---------------------------------------------------------------------------
procedure TDX9Canvas.SetAntialias(const Value: Boolean);
begin
 if (Device9 = nil) then Exit;

 Flush();

 case Value of
  False:
   begin
    Device9.SetSamplerState(0, D3DSAMP_MAGFILTER, D3DTEXF_POINT);
    Device9.SetSamplerState(0, D3DSAMP_MINFILTER, D3DTEXF_POINT);
   end;

  True:
   begin
    Device9.SetSamplerState(0, D3DSAMP_MAGFILTER, D3DTEXF_LINEAR);
    Device9.SetSamplerState(0, D3DSAMP_MINFILTER, D3DTEXF_LINEAR);
   end;
 end;
end;

//---------------------------------------------------------------------------
function TDX9Canvas.GetMipMapping(): Boolean;
var
 MipFlt: Cardinal;
begin
 if (Device9 = nil) then
  begin
   Result:= False;
   Exit;
  end;

 Device9.GetSamplerState(0, D3DSAMP_MIPFILTER, MipFlt);

 Result:= True;

 if (MipFlt = D3DTEXF_NONE)or(MipFlt = D3DTEXF_POINT) then Result:= False;
end;

//---------------------------------------------------------------------------
procedure TDX9Canvas.SetMipMapping(const Value: Boolean);
begin
 if (Device9 = nil) then Exit;

 Flush();

 case Value of
  False:
   Device9.SetSamplerState(0, D3DSAMP_MIPFILTER, D3DTEXF_NONE);

  True:
   Device9.SetSamplerState(0, D3DSAMP_MIPFILTER, D3DTEXF_LINEAR);
 end;
end;

//---------------------------------------------------------------------------
function TDX9Canvas.UploadVertexBuffer(): Boolean;
var
 MemAddr: Pointer;
 BufSize: Integer;
begin
 BufSize:= FVertexCount * SizeOf(TVertexRecord);
 Result:= Succeeded(VertexBuffer.Lock(0, BufSize, MemAddr, D3DLOCK_DISCARD));

 if (Result) then
  begin
   Move(VertexArray^, MemAddr^, BufSize);
   Result:= Succeeded(VertexBuffer.Unlock());
  end;
end;

//---------------------------------------------------------------------------
function TDX9Canvas.UploadIndexBuffer(): Boolean;
var
 MemAddr: Pointer;
 BufSize: Integer;
begin
 BufSize:= FIndexCount * SizeOf(Word);
 Result:= Succeeded(IndexBuffer.Lock(0, BufSize, MemAddr, D3DLOCK_DISCARD));

 if (Result) then
  begin
   Move(IndexArray^, MemAddr^, BufSize);
   Result:= Succeeded(IndexBuffer.Unlock());
  end;
end;

//---------------------------------------------------------------------------
procedure TDX9Canvas.DrawBuffers();
begin
 with Device9 do
  begin
   SetStreamSource(0, VertexBuffer, 0, SizeOf(TVertexRecord));
   SetIndices(IndexBuffer);
   SetVertexShader(nil);
   SetFVF(VertexFVFType);

   case DrawingMode of
    dmPoints:
     DrawPrimitive(D3DPT_POINTLIST, 0, FPrimitives);

    dmLines:
     DrawPrimitive(D3DPT_LINELIST, 0, FPrimitives);

    dmTriangles:
     DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, FVertexCount, 0,
      FPrimitives);
   end;
  end;

 NextDrawCall();
end;

//---------------------------------------------------------------------------
procedure TDX9Canvas.Flush();
begin
 if (FVertexCount > 0)and(FPrimitives > 0)and(UploadVertexBuffer())and
  (UploadIndexBuffer()) then DrawBuffers();

 FVertexCount:= 0;
 FIndexCount := 0;
 FPrimitives := 0;
 DrawingMode := dmUnknown;
 CachedEffect:= deUnknown;

 Device9.SetTexture(0, nil);

 CachedTex:= nil;
 ActiveTex:= nil;
end;

//---------------------------------------------------------------------------
procedure TDX9Canvas.SetEffectStates(Effect: TDrawingEffect);
begin
 case Effect of
  deNormal:
   with Device9 do
    begin
     SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_SRCALPHA);
     SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deShadow:
   with Device9 do
    begin
     SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_ZERO);
     SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deAdd:
   with Device9 do
    begin
     SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_SRCALPHA);
     SetRenderState(D3DRS_DESTBLEND, D3DBLEND_ONE);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deMultiply:
   with Device9 do
    begin
     SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_ZERO);
     SetRenderState(D3DRS_DESTBLEND, D3DBLEND_SRCCOLOR);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deSrcAlphaAdd:
   with Device9 do
    begin
     SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_SRCALPHA);
     SetRenderState(D3DRS_DESTBLEND, D3DBLEND_ONE);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deSrcColor:
   with Device9 do
    begin
     SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_SRCCOLOR);
     SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCCOLOR);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deSrcColorAdd:
   with Device9 do
    begin
     SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_SRCCOLOR);
     SetRenderState(D3DRS_DESTBLEND, D3DBLEND_ONE);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deInvert:
   with Device9 do
    begin
     SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_INVDESTCOLOR);
     SetRenderState(D3DRS_DESTBLEND, D3DBLEND_ZERO);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deSrcBright:
   with Device9 do
    begin
     SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_SRCCOLOR);
     SetRenderState(D3DRS_DESTBLEND, D3DBLEND_SRCCOLOR);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deInvMultiply:
   with Device9 do
    begin
     SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_ZERO);
     SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCCOLOR);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deMultiplyAlpha:
   with Device9 do
    begin
     SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_ZERO);
     SetRenderState(D3DRS_DESTBLEND, D3DBLEND_SRCALPHA);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deInvMultiplyAlpha:
   with Device9 do
    begin
     SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_ZERO);
     SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deDestBright:
   with Device9 do
    begin
     SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_DESTCOLOR);
     SetRenderState(D3DRS_DESTBLEND, D3DBLEND_DESTCOLOR);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deInvSrcBright:
   with Device9 do
    begin
     SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_INVSRCCOLOR);
     SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCCOLOR);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deInvDestBright:
   with Device9 do
    begin
     SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_INVDESTCOLOR);
     SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVDESTCOLOR);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deBright:
   with Device9 do
    begin
     SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_SRCALPHA);
     SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA);
     SetTextureStageState(0, D3DTSS_COLOROP,  D3DTOP_MODULATE2X);
     SetTextureStageState(0, D3DTSS_ALPHAOP,  D3DTOP_MODULATE);
    end;

  deBrightAdd:
   with Device9 do
    begin
     SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_SRCALPHA);
     SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA);
     SetTextureStageState(0, D3DTSS_COLOROP,  D3DTOP_MODULATE4X);
     SetTextureStageState(0, D3DTSS_ALPHAOP,  D3DTOP_MODULATE);
    end;

  deGrayscale:
   with Device9 do
    begin
     SetRenderState(D3DRS_TextureFactor, D3DCOLOR_ARGB(129, 255, 48, 255));
     SetTextureStageState(0, D3DTSS_COLOROP,   D3DTOP_DOTPRODUCT3);
     SetTextureStageState(0, D3DTSS_COLORARG2, D3DTA_TFACTOR);
     // Note: D3DTSS_COLORARG2 is changed after using this effect and will
     // need to be reset before rendering anything else.
    end;

  deLight:
   with Device9 do
    begin
     SetRenderState(D3DRS_SRCBLEND,  Cardinal(D3DBLEND_DESTCOLOR));
     SetRenderState(D3DRS_DESTBLEND, Cardinal(D3DBLEND_ONE));
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE2X);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deLightAdd:
   with Device9 do
    begin
     SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_DESTCOLOR);
     SetRenderState(D3DRS_DESTBLEND, D3DBLEND_ONE);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE4X);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deAdd2X:
   with Device9 do
    begin
     SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_SRCALPHA);
     SetRenderState(D3DRS_DESTBLEND, D3DBLEND_ONE);
     SetTextureStageState(0, D3DTSS_COLOROP, D3DTOP_MODULATE2X);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
    end;

  deOneColor:
   with Device9 do
    begin
     SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_SRCALPHA);
     SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCALPHA);
     SetTextureStageState(0, D3DTSS_COLOROP, 25);
     SetTextureStageState(0, D3DTSS_ALPHAOP, D3DTOP_MODULATE);
     // Note: '25' is used by DraculaLin here, which is most likely incorrect
     // usage of D3DTSS_COLOROP.
    end;

  deXOR:
   with Device9 do
    begin
     SetRenderState(D3DRS_SRCBLEND,  D3DBLEND_INVDESTCOLOR);
     SetRenderState(D3DRS_DESTBLEND, D3DBLEND_INVSRCCOLOR);
    end;
 end;
end;

//---------------------------------------------------------------------------
function TDX9Canvas.RequestCache(Mode: TDrawingMode; Vertices,
 Indices: Integer; Effect: TDrawingEffect; Texture: TAsphyreCustomTexture): Boolean;
var
 NeedReset: Boolean;
begin
 Result:= (Vertices <= MaxCachedVertices)and(Indices <= MaxCachedIndices);
 if (not Result) then
  begin
   Errors.Insert(errGeometryTooComplex, Self, ClassName, 'RequestCache');
   Exit;
  end;

 NeedReset:= (FVertexCount + Vertices > FVertexCache);
 NeedReset:= (NeedReset)or(FIndexCount + Indices > FIndexCache);
 NeedReset:= (NeedReset)or(DrawingMode = dmUnknown)or(DrawingMode <> Mode);
 NeedReset:= (NeedReset)or(CachedEffect = deUnknown)or(CachedEffect <> Effect);
 NeedReset:= (NeedReset)or(CachedTex <> Texture);

 if (NeedReset) then
  begin
   Flush();

   if (CachedEffect = deUnknown)or(CachedEffect <> Effect) then
    SetEffectStates(Effect);

   if (CachedEffect = deUnknown)or(CachedTex <> Texture) then
    begin
     if (Texture <> nil) then Texture.Bind(0)
      else Device9.SetTexture(0, nil);
    end;

   DrawingMode := Mode;
   CachedEffect:= Effect;
   CachedTex   := Texture;
  end;
end;

//---------------------------------------------------------------------------
function TDX9Canvas.NextVertexEntry(): Pointer;
begin
 Result:= Pointer(PtrInt(VertexArray) + (FVertexCount * SizeOf(TVertexRecord)));
end;

//---------------------------------------------------------------------------
procedure TDX9Canvas.AddIndexEntry(Index: Integer);
var
 Entry: PWord;
begin
 Entry:= Pointer(PtrInt(IndexArray) + (FIndexCount * SizeOf(Word)));
 Entry^:= Index;

 Inc(FIndexCount);
end;

//---------------------------------------------------------------------------
procedure TDX9Canvas.PutPixel(const Point: TPoint2; Color: Cardinal);
var
 Entry: PVertexRecord;
begin
 if (not RequestCache(dmPoints, 1, 0, deNormal, nil)) then Exit;

 Entry:= NextVertexEntry();
 Entry^.Vertex.x:= Point.x;
 Entry^.Vertex.y:= Point.y;
 Entry^.Color   := Color;

 Inc(FVertexCount);
 Inc(FPrimitives);
end;

//---------------------------------------------------------------------------
procedure TDX9Canvas.Line(const Src, Dest: TPoint2; Color0, Color1: Cardinal);
var
 Entry: PVertexRecord;
begin
 if (not RequestCache(dmLines, 2, 0, deNormal, nil)) then Exit;

 Entry:= NextVertexEntry();
 Entry^.Vertex.x:= Src.x;
 Entry^.Vertex.y:= Src.y;
 Entry^.Color   := Color0;
 Inc(FVertexCount);

 Entry:= NextVertexEntry();
 Entry^.Vertex.x:= Dest.x;
 Entry^.Vertex.y:= Dest.y;
 Entry^.Color   := Color1;
 Inc(FVertexCount);

 Inc(FPrimitives);
end;

//---------------------------------------------------------------------------
procedure TDX9Canvas.DrawIndexedTriangles(Vertices: PPoint2;
 Colors: PCardinal; Indices: PInteger; NoVertices, NoTriangles: Integer;
 Effect: TDrawingEffect = deNormal);
var
 Entry : PVertexRecord;
 Index : PInteger;
 Vertex: PPoint2;
 Color : PCardinal;
 i     : Integer;
begin
 if (not RequestCache(dmTriangles, NoVertices, NoTriangles * 3, Effect,
  nil)) then Exit;

 Index:= Indices;

 for i:= 0 to (NoTriangles * 3) - 1 do
  begin
   AddIndexEntry(FVertexCount + Index^);

   Inc(Index);
  end;

 Vertex:= Vertices;
 Color := Colors;

 for i:= 0 to NoVertices - 1 do
  begin
   Entry:= NextVertexEntry();
   Entry^.Vertex.x:= Vertex^.x - 0.5;
   Entry^.Vertex.y:= Vertex^.y - 0.5;
   Entry^.Color   := Color^;
   Inc(FVertexCount);

   Inc(Vertex);
   Inc(Color);
  end;

 Inc(FPrimitives, NoTriangles);
end;

//---------------------------------------------------------------------------
procedure TDX9Canvas.UseTexture(Texture: TAsphyreCustomTexture;
 const Mapping: TPoint4);
begin
 ActiveTex  := Texture;
 QuadMapping:= Mapping;
end;

//---------------------------------------------------------------------------
procedure TDX9Canvas.TexMap(const Points: TPoint4; const Colors: TColor4;
 Effect: TDrawingEffect);
var
 Entry: PVertexRecord;
begin
 RequestCache(dmTriangles, 4, 6, Effect, ActiveTex);

 AddIndexEntry(FVertexCount + 2);
 AddIndexEntry(FVertexCount);
 AddIndexEntry(FVertexCount + 1);

 AddIndexEntry(FVertexCount + 3);
 AddIndexEntry(FVertexCount + 2);
 AddIndexEntry(FVertexCount + 1);

 Entry:= NextVertexEntry();
 Entry^.Vertex.x:= Points[0].x - 0.5;
 Entry^.Vertex.y:= Points[0].y - 0.5;
 Entry^.Color   := Colors[0];
 Entry^.u:= QuadMapping[0].x;
 Entry^.v:= QuadMapping[0].y;
 Inc(FVertexCount);

 Entry:= NextVertexEntry();
 Entry^.Vertex.x:= Points[1].x - 0.5;
 Entry^.Vertex.y:= Points[1].y - 0.5;
 Entry^.Color   := Colors[1];
 Entry^.u:= QuadMapping[1].x;
 Entry^.v:= QuadMapping[1].y;
 Inc(FVertexCount);

 Entry:= NextVertexEntry();
 Entry^.Vertex.x:= Points[3].x - 0.5;
 Entry^.Vertex.y:= Points[3].y - 0.5;
 Entry^.Color   := Colors[3];
 Entry^.u:= QuadMapping[3].x;
 Entry^.v:= QuadMapping[3].y;
 Inc(FVertexCount);

 Entry:= NextVertexEntry();
 Entry^.Vertex.x:= Points[2].x - 0.5;
 Entry^.Vertex.y:= Points[2].y - 0.5;
 Entry^.Color   := Colors[2];
 Entry^.u:= QuadMapping[2].x;
 Entry^.v:= QuadMapping[2].y;
 Inc(FVertexCount);

 Inc(FPrimitives, 2);
end;

procedure TDX9Canvas.Draw(Texture: TAsphyreCustomTexture; X, Y, Scale: Real; MirrorX: Boolean;
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

procedure TDX9Canvas.DrawRotateC(Texture: TAsphyreCustomTexture; X, Y, Angle: Real;
  const Color: TColor4; Effect: TDrawingEffect);
begin
     UseTexturePx(Texture,pxBounds4(0, 0, Texture.Width, Texture.Height));
     TexMap(pRotate4c(Point2(X, Y), Point2( Texture.Width, Texture.Height), Angle,1),
      Color, Effect);
end;

//---------------------------------------------------------------------------
end.
