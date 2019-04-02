unit SoftRastTypes;

//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 SystemSurfaces;

//---------------------------------------------------------------------------
type
 PSRAddress = ^TSRAddress;
 TSRAddress = packed record
  Bits  : Pointer;
  Pitch : Integer;
  Width : Integer;
  Height: Integer;
 end;

//---------------------------------------------------------------------------
 TSRTexCoords = packed record
  u1, v1: Integer;
  u2, v2: Integer;
 end;

//---------------------------------------------------------------------------
const
 srDiffuse    = $00000001;
 srSrcInvert  = $00000002;
 srInvAlpha   = $00000004;
 srSrcAlpha   = $00000008;
 srDestAlpha  = $00000010;
 srDestSrc    = $00000020;
 srMove       = $00000040;
 srAdd        = $00000080;
 srDest       = $00000100;
 srNoTex      = $00000200;
 srAlphaGrey  = $00000400;

//---------------------------------------------------------------------------
var
 TargetSurface: TSystemSurface = nil;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
end.
