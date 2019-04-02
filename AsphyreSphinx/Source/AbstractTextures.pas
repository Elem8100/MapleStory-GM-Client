unit AbstractTextures;
//---------------------------------------------------------------------------
// AbstractTextures.pas                                 Modified: 05-May-2008
// Asphyre Custom Texture implementation                          Version 1.1
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
// The Original Code is AbstractTextures.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by M. Sc. Yuriy Kotsarenko are Copyright (C) 2007 - 2008,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 System.Types, Vectors2px, Vectors2, AsphyreTypes;

//---------------------------------------------------------------------------
type
 TAsphyreCustomTexture = class
 private
  FWidth : LongInt;
  FHeight: LongInt;
  FActive: Boolean;
  FMipMapping: Boolean;

  procedure SetSize(const Index, Value: Integer);
  function GetBytesPerPixel(): Integer;
  procedure SetMipmapping(const Value: Boolean);
  procedure SetFormat(const Value: TAsphyrePixelFormat);
 protected
  FFormat: TAsphyrePixelFormat;

  procedure UpdateSize(); virtual;

  function CreateTexture(): Boolean; virtual;
  procedure DestroyTexture(); virtual;
 public
  property Format: TAsphyrePixelFormat read FFormat write SetFormat;

  property Width : Longint index 0 read FWidth write SetSize;
  property Height: LongInt index 1 read FHeight write SetSize;

  property Active: Boolean read FActive;

  property BytesPerPixel: Integer read GetBytesPerPixel;

  property Mipmapping: Boolean read FMipMapping write SetMipmapping;

  function Initialize(): Boolean;
  procedure Finalize();

  procedure Bind(Stage: Integer); virtual;

  procedure HandleDeviceReset(); virtual;
  procedure HandleDeviceLost(); virtual;

  function PixelToLogical(const Pos: TPoint2px): TPoint2; overload;
  function PixelToLogical(const Pos: TPoint2): TPoint2; overload;
  function LogicalToPixel(const Pos: TPoint2): TPoint2px;

  procedure UpdateMipmaps(); virtual;

  constructor Create(); virtual;
 end;

//---------------------------------------------------------------------------
 TAsphyreLockableTexture = class(TAsphyreCustomTexture)
 private
  FDynamicTexture: Boolean;

  function GetPixel(x, y: Integer): Cardinal;
  procedure SetPixel(x, y: Integer; const Value: Cardinal);
  procedure SetDynamicTexture(const Value: Boolean);
 public
  property Pixels[x, y: Integer]: Cardinal read GetPixel write SetPixel;

  property DynamicTexture: Boolean read FDynamicTexture write SetDynamicTexture;

  procedure Lock(const Rect: TRect; out Bits: Pointer;
   out Pitch: Integer); virtual; abstract;
  procedure Unlock(); virtual; abstract;

  constructor Create(); override;
 end;

//---------------------------------------------------------------------------
 TAsphyreRenderTargetTexture = class(TAsphyreCustomTexture)
 private
  FDepthStencil: Boolean;

  procedure SetDepthStencil(const Value: Boolean);
 public
  property DepthStencil: Boolean read FDepthStencil write SetDepthStencil;

  function BeginDrawTo(): Boolean; virtual; abstract;
  procedure EndDrawTo(); virtual; abstract;
 end;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 AsphyreConv;

//---------------------------------------------------------------------------
constructor TAsphyreCustomTexture.Create();
begin
 inherited;

 FWidth := 256;
 FHeight:= 256;
 FActive:= False;
 FFormat:= apf_Unknown;
 FMipmapping:= False;
end;

//---------------------------------------------------------------------------
procedure TAsphyreCustomTexture.SetFormat(const Value: TAsphyrePixelFormat);
begin
 if (not FActive) then FFormat:= Value;
end;

//---------------------------------------------------------------------------
procedure TAsphyreCustomTexture.SetSize(const Index, Value: Integer);
begin
 case Index of
  0: FWidth := Value;
  1: FHeight:= Value;
 end;

 if (FActive) then UpdateSize();
end;

//---------------------------------------------------------------------------
procedure TAsphyreCustomTexture.UpdateSize();
begin
 // no code
end;

//---------------------------------------------------------------------------
function TAsphyreCustomTexture.GetBytesPerPixel(): Integer;
begin
 Result:= AsphyrePixelFormatBits[FFormat] div 8;
end;

//---------------------------------------------------------------------------
procedure TAsphyreCustomTexture.SetMipmapping(const Value: Boolean);
begin
 if (not FActive) then FMipmapping:= Value;
end;

//---------------------------------------------------------------------------
function TAsphyreCustomTexture.CreateTexture(): Boolean;
begin
 Result:= True;
end;

//---------------------------------------------------------------------------
procedure TAsphyreCustomTexture.DestroyTexture();
begin
 // no code
end;

//---------------------------------------------------------------------------
function TAsphyreCustomTexture.Initialize(): Boolean;
begin
 Result:= not FActive;
 if (not Result) then Exit;

 Result := CreateTexture();
 FActive:= Result;
end;

//---------------------------------------------------------------------------
procedure TAsphyreCustomTexture.Finalize();
begin
 if (FActive) then DestroyTexture();
 FActive:= False;
end;

//---------------------------------------------------------------------------
procedure TAsphyreCustomTexture.HandleDeviceReset();
begin
 // no code
end;

//---------------------------------------------------------------------------
procedure TAsphyreCustomTexture.HandleDeviceLost();
begin
 // no code
end;

//---------------------------------------------------------------------------
procedure TAsphyreCustomTexture.Bind(Stage: Integer);
begin
 // no code
end;

//---------------------------------------------------------------------------
procedure TAsphyreCustomTexture.UpdateMipmaps();
begin
 // no code
end;

//---------------------------------------------------------------------------
function TAsphyreCustomTexture.PixelToLogical(const Pos: TPoint2): TPoint2;
begin
 if (FWidth > 0) then Result.x:= Pos.x / FWidth else Result.x:= 0.0;
 if (FHeight > 0) then Result.y:= Pos.y / FHeight else Result.y:= 0.0;
end;

//---------------------------------------------------------------------------
function TAsphyreCustomTexture.PixelToLogical(const Pos: TPoint2px): TPoint2;
begin
 if (FWidth > 0) then Result.x:= Pos.x / FWidth else Result.x:= 0.0;
 if (FHeight > 0) then Result.y:= Pos.y / FHeight else Result.y:= 0.0;
end;

//---------------------------------------------------------------------------
function TAsphyreCustomTexture.LogicalToPixel(const Pos: TPoint2): TPoint2px;
begin
 Result.x:= Round(Pos.x * FWidth);
 Result.y:= Round(Pos.y * FHeight);
end;

//---------------------------------------------------------------------------
constructor TAsphyreLockableTexture.Create();
begin
 inherited;

 FDynamicTexture:= False;
end;

//---------------------------------------------------------------------------
procedure TAsphyreLockableTexture.SetDynamicTexture(const Value: Boolean);
begin
 if (not Active) then FDynamicTexture:= Value;
end;

//---------------------------------------------------------------------------
function TAsphyreLockableTexture.GetPixel(x, y: Integer): Cardinal;
var
 Bits : Pointer;
 Pitch: Integer;
begin
 Result:= 0;
 if (x < 0)or(y < 0)or(x >= FWidth)or(y >= FHeight) then Exit;

 Lock(Bounds(0, y, FWidth, 1), Bits, Pitch);
 if (Bits = nil) then Exit;

 Result:= PixelXto32(Pointer(PtrInt(Bits) + (x * BytesPerPixel)), FFormat);
 Unlock();
end;

//---------------------------------------------------------------------------
procedure TAsphyreLockableTexture.SetPixel(x, y: Integer;
 const Value: Cardinal);
var
 Bits : Pointer;
 Pitch: Integer;
begin
 if (x < 0)or(y < 0)or(x >= FWidth)or(y >= FHeight) then Exit;

 Lock(Bounds(0, y, FWidth, 1), Bits, Pitch);
 if (Bits = nil) then Exit;

 Pixel32toX(Value, Pointer(PtrInt(Bits) + (x * BytesPerPixel)), FFormat);

 Unlock();
end;

//---------------------------------------------------------------------------
procedure TAsphyreRenderTargetTexture.SetDepthStencil(const Value: Boolean);
begin
 if (not Active) then FDepthStencil:= Value;
end;

//---------------------------------------------------------------------------
end.
