unit SoftRastCanvas;

//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Types, AbstractCanvas, Vectors2, AsphyreTypes, AbstractTextures, SoftRastTypes;

//---------------------------------------------------------------------------
type
 TSoftRastCanvas = class(TAsphyreCanvas)
 private
  ActiveTex  : TAsphyreCustomTexture;
  QuadMapping: TPoint4;
  ClipRect   : TRect;

  function FilterEffect(Effect: TDrawingEffect): Integer;
  function UpdateAddress(var Address: TSRAddress): Boolean;
 protected
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

  procedure Flush(); override;

  constructor Create(); override;
  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
implementation

//--------------------------------------------------------------------------
uses
 AsphyreErrors, SoftRastCore, SoftRastPutPixel, SoftRastTextures;

//--------------------------------------------------------------------------
constructor TSoftRastCanvas.Create();
begin
 inherited;

end;

//---------------------------------------------------------------------------
destructor TSoftRastCanvas.Destroy();
begin

 inherited;
end;

//--------------------------------------------------------------------------
procedure TSoftRastCanvas.HandleBeginScene();
begin
 if (TargetSurface <> nil) then
  ClipRect:= Bounds(0, 0, TargetSurface.Width, TargetSurface.Height);
end;

//--------------------------------------------------------------------------
procedure TSoftRastCanvas.HandleEndScene();
begin
 // no code
end;

//---------------------------------------------------------------------------
procedure TSoftRastCanvas.GetViewport(out x, y, Width, Height: Integer);
begin
 x:= ClipRect.Left;
 y:= ClipRect.Top;
 Width := ClipRect.Right - ClipRect.Left;
 Height:= ClipRect.Bottom - ClipRect.Top;
end;

//---------------------------------------------------------------------------
procedure TSoftRastCanvas.SetViewport(x, y, Width, Height: Integer);
begin
 ClipRect:= Bounds(x, y, Width, Height);
end;

//---------------------------------------------------------------------------
function TSoftRastCanvas.GetAntialias(): Boolean;
begin
 Result:= False;
end;

//---------------------------------------------------------------------------
procedure TSoftRastCanvas.SetAntialias(const Value: Boolean);
begin
 // no code
end;

//---------------------------------------------------------------------------
function TSoftRastCanvas.GetMipMapping(): Boolean;
begin
 Result:= False;
end;

//---------------------------------------------------------------------------
procedure TSoftRastCanvas.SetMipMapping(const Value: Boolean);
begin
 // no code
end;

//---------------------------------------------------------------------------
procedure TSoftRastCanvas.Flush();
begin
 ActiveTex:= nil;
end;

//---------------------------------------------------------------------------
function TSoftRastCanvas.FilterEffect(Effect: TDrawingEffect): Integer;
begin
 case Effect of
  deShadow:
   Result:= srDiffuse or srDest or srDestAlpha;

  deAdd:
   Result:= srDiffuse or srSrcAlpha or srAdd;

  deMultiply:
   Result:= srDiffuse or srDest or srDestSrc or srSrcAlpha;

  deInvMultiply:
   Result:= srDiffuse or srDest or srDestSrc or srSrcInvert or srSrcAlpha;

  else
   Result:= srDiffuse or srSrcAlpha or srDestAlpha or srAdd;
 end;
end;

//---------------------------------------------------------------------------
function TSoftRastCanvas.UpdateAddress(var Address: TSRAddress): Boolean;
begin
 Result:= TargetSurface <> nil;
 if (not Result) then Exit;

 Address.Bits  := TargetSurface.Bits;
 Address.Pitch := TargetSurface.Pitch;
 Address.Width := TargetSurface.Width;
 Address.Height:= TargetSurface.Height;
end;

//---------------------------------------------------------------------------
procedure TSoftRastCanvas.PutPixel(const Point: TPoint2; Color: Cardinal);
var
 Address: TSRAddress;
begin
 if (not UpdateAddress(Address)) then
  begin
   Errors.Insert(errInvalidCall, Self, ClassName, 'PutPixel');
   Exit;
  end;

 SRPutPixel(Address, ClipRect, Round(Point.x), Round(Point.y), Color,
  srDiffuse or srSrcAlpha or srDestAlpha or srAdd or srNoTex);
end;

//---------------------------------------------------------------------------
procedure TSoftRastCanvas.Line(const Src, Dest: TPoint2; Color0, Color1: Cardinal);
var
 Address: TSRAddress;
begin
 if (not UpdateAddress(Address)) then
  begin
   Errors.Insert(errInvalidCall, Self, ClassName, 'PutPixel');
   Exit;
  end;

 SRLine(Address, ClipRect, Src, Dest, Color0, Color1,
  srDiffuse or srSrcAlpha or srDestAlpha or srAdd or srNoTex);
end;

//---------------------------------------------------------------------------
procedure TSoftRastCanvas.DrawIndexedTriangles(Vertices: PPoint2;
 Colors: PCardinal; Indices: PInteger; NoVertices, NoTriangles: Integer;
 Effect: TDrawingEffect = deNormal);
type
 PQuadIndex = ^TQuadIndex;
 TQuadIndex = array[0..5] of Integer;
var
 Quad   : PQuadIndex;
 Address: TSRAddress;
 NoAddr : TSRAddress;
 TexSrc : TSRTexCoords;
 i, i0, i1, i2: Integer;
 Pt0, Pt1, Pt2: PPoint2;
 c0, c1, c2, c3: PCardinal;
 Coords: TPoint4px;
begin
 if (not UpdateAddress(Address)) then
  begin
   Errors.Insert(errInvalidCall, Self, ClassName, 'DrawIndexedTriangles');
   Exit;
  end;

 FillChar(NoAddr, SizeOf(TSRAddress), 0);

 if (NoTriangles = 2) then
  begin
   Quad:= Pointer(Indices);

   if (Quad[0] = 2)and(Quad[1] = 0)and(Quad[2] = 1)and(Quad[3] = 3)and
    (Quad[4] = 2)and(Quad[5] = 1) then
    begin
     Coords[0]:= PPoint2(Integer(Vertices) + 0 * SizeOf(TPoint2))^;
     Coords[1]:= PPoint2(Integer(Vertices) + 1 * SizeOf(TPoint2))^;
     Coords[2]:= PPoint2(Integer(Vertices) + 3 * SizeOf(TPoint2))^;
     Coords[3]:= PPoint2(Integer(Vertices) + 2 * SizeOf(TPoint2))^;

     c0:= Pointer(Integer(Colors) + 0 * SizeOf(Longword));
     c1:= Pointer(Integer(Colors) + 1 * SizeOf(Longword));
     c2:= Pointer(Integer(Colors) + 3 * SizeOf(Longword));
     c3:= Pointer(Integer(Colors) + 2 * SizeOf(Longword));

     SRTextureMapEx(Address, NoAddr, ClipRect, Coords, cColor4(c0^, c1^, c2^,
      c3^), TexSrc, FilterEffect(Effect) or srNoTex);

     Exit;
    end;
  end;

 for i:= 0 to NoTriangles - 1 do
  begin
   i0:= Indices^; Inc(Indices);

   i1:= Indices^; Inc(Indices);

   i2:= Indices^; Inc(Indices);

   Pt0:= Pointer(Integer(Vertices) + i0 * SizeOf(TPoint2));
   Pt1:= Pointer(Integer(Vertices) + i1 * SizeOf(TPoint2));
   Pt2:= Pointer(Integer(Vertices) + i2 * SizeOf(TPoint2));

   c0:= Pointer(Integer(Colors) + i0 * SizeOf(Longword));
   c1:= Pointer(Integer(Colors) + i1 * SizeOf(Longword));
   c2:= Pointer(Integer(Colors) + i2 * SizeOf(Longword));

   SRTriMap(Address, NoAddr, ClipRect, Pt0^, Pt1^, Pt2^, c0^, c1^, c2^,
    TexSrc, FilterEffect(Effect) or srNoTex);
  end;
end;

//---------------------------------------------------------------------------
procedure TSoftRastCanvas.UseTexture(Texture: TAsphyreCustomTexture;
 const Mapping: TPoint4);
begin
 ActiveTex  := Texture;
 QuadMapping:= Mapping;
end;

//---------------------------------------------------------------------------
procedure TSoftRastCanvas.TexMap(const Points: TPoint4; const Colors: TColor4;
 Effect: TDrawingEffect);
var
 Address: TSRAddress;
 TexAddr: TSRAddress;
 Coords : TPoint4px;
 TexSrc : TSRTexCoords;
 i: Integer;
begin
 if (not UpdateAddress(Address))or(ActiveTex = nil) then
  begin
   Errors.Insert(errInvalidCall, Self, ClassName, 'TexMap');
   Exit;
  end;

 with ActiveTex as TSRLockableTexture do
  begin
   TexAddr.Bits  := Surface.Bits;
   TexAddr.Pitch := Surface.Pitch;
   TexAddr.Width := Surface.Width;
   TexAddr.Height:= Surface.Height;
  end;

 for i:= 0 to 3 do
  Coords[i]:= Points[i];

 TexSrc.u1:= Round(QuadMapping[0].x * ActiveTex.Width);
 TexSrc.v1:= Round(QuadMapping[0].y * ActiveTex.Height);
 TexSrc.u2:= Round(QuadMapping[2].x * ActiveTex.Width);
 TexSrc.v2:= Round(QuadMapping[2].y * ActiveTex.Height);

 SRTextureMapEx(Address, TexAddr, ClipRect, Coords, Colors, TexSrc,
  FilterEffect(Effect));
end;

//---------------------------------------------------------------------------
end.
