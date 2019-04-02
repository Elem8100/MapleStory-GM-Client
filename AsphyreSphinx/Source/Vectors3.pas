unit Vectors3;
//---------------------------------------------------------------------------
// Vectors3.pas                                         Modified: 08-Jan-2008
// Definitions and functions working with 3D vectors             Version 1.02
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
// The Original Code is Vectors3.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2007 - 2008,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Classes, SysUtils, Math, Vectors2, Vectors2px;

//---------------------------------------------------------------------------
type
 PVector3 = ^TVector3;
 TVector3 = record
  x, y, z: Single;

  {$ifndef fpc}
  class operator Add(const a, b: TVector3): TVector3;
  class operator Subtract(const a, b: TVector3): TVector3;
  class operator Multiply(const a, b: TVector3): TVector3;
  class operator Divide(const a, b: TVector3): TVector3;

  class operator Negative(const v: TVector3): TVector3;
  class operator Multiply(const v: TVector3; const k: Single): TVector3;
  class operator Divide(const v: TVector3; const k: Single): TVector3;
  class operator Multiply(const k: Single; const v: TVector3): TVector3;
  class operator Divide(const k: Single; const v: TVector3): TVector3;
  {$endif}
 end;

//---------------------------------------------------------------------------
 TVectors3 = class
 private
  Data: array of TVector3;
  DataCount: Integer;
 
  function GetItem(Num: Integer): TVector3;
  procedure SetItem(Num: Integer; const Value: TVector3);
  function GetVector(Num: Integer): PVector3;
  procedure Request(Amount: Integer);
  public
  property Count: Integer read DataCount;
  property Items[Num: Integer]: TVector3 read GetItem write SetItem; default;
  property Vector[Num: Integer]: PVector3 read GetVector;

  function Add(const v: TVector3): Integer; overload;
  function Add(x, y, z: Single): Integer; overload;
  procedure Remove(Index: Integer);
  procedure RemoveAll();

  procedure CopyFrom(Source: TVectors3);
  procedure AddFrom(Source: TVectors3);

  procedure Normalize();
  procedure Rescale(Scale: Single);
  procedure Invert();
  procedure Centralize();

  procedure SaveToStream(Stream: TStream);
  procedure LoadFromStream(Stream: TStream);

  constructor Create();
  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
{$ifdef fpc}
operator + (const a, b: TVector3) c: TVector3;
operator - (const a, b: TVector3) c: TVector3;
operator * (const a, b: TVector3) c: TVector3;
operator / (const a, b: TVector3) c: TVector3;
operator - (const v: TVector3) a: TVector3;
operator * (const v: TVector3; k: Single) a: TVector3;
operator / (const v: TVector3; k: Single) a: TVector3;
operator * (k: Single; const v: TVector3) a: TVector3;
operator / (k: Single; const v: TVector3) a: TVector3;
{$endif}

//---------------------------------------------------------------------------
const
 ZeroVec3 : TVector3 = (x: 0.0; y: 0.0; z: 0.0);
 UnityVec3: TVector3 = (x: 1.0; y: 1.0; z: 1.0);
 AxisXVec3: TVector3 = (x: 1.0; y: 0.0; z: 0.0);
 AxisYVec3: TVector3 = (x: 0.0; y: 1.0; z: 0.0);
 AxisZVec3: TVector3 = (x: 0.0; y: 0.0; z: 1.0);

//---------------------------------------------------------------------------
function Vector3(x, y, z: Single): TVector3;
function Length3(const v: TVector3): Single;
function Norm3(const v: TVector3): TVector3;
function Lerp3(const v0, v1: TVector3; Alpha: Single): TVector3;
function Dot3(const a, b: TVector3): Single;
function Cross3(const a, b: TVector3): TVector3;
function Angle3(const a, b: TVector3): Single;
function Parallel3(const v, n: TVector3): TVector3;
function Perp3(const v, n: TVector3): TVector3;
function Reflect3(const i, n: TVector3): TVector3;
function ColorToVec3(Color: Cardinal): TVector3;
function Vec3ToColor(const v: TVector3): Cardinal;

//---------------------------------------------------------------------------
function Vec3toXY(const v: TVector3): TPoint2;
function Vec3toXYpx(const v: TVector3): TPoint2px;
function Vec3toString(const v: TVector3): string;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
const
 CacheSize = 512;

//---------------------------------------------------------------------------
{$ifdef fpc}
operator + (const a, b: TVector3) c: TVector3;
{$else}
class operator TVector3.Add(const a, b: TVector3): TVector3;
{$endif}
begin
 Result.x:= a.x + b.x;
 Result.y:= a.y + b.y;
 Result.z:= a.z + b.z;
end;

//---------------------------------------------------------------------------
{$ifdef fpc}
operator - (const a, b: TVector3) c: TVector3;
{$else}
class operator TVector3.Subtract(const a, b: TVector3): TVector3;
{$endif}
begin
 Result.x:= a.x - b.x;
 Result.y:= a.y - b.y;
 Result.z:= a.z - b.z;
end;

//---------------------------------------------------------------------------
{$ifdef fpc}
operator * (const a, b: TVector3) c: TVector3;
{$else}
class operator TVector3.Multiply(const a, b: TVector3): TVector3;
{$endif}
begin
 Result.x:= a.x * b.x;
 Result.y:= a.y * b.y;
 Result.z:= a.z * b.z;
end;

//---------------------------------------------------------------------------
{$ifdef fpc}
operator / (const a, b: TVector3) c: TVector3;
{$else}
class operator TVector3.Divide(const a, b: TVector3): TVector3;
{$endif}
begin
 Result.x:= a.x / b.x;
 Result.y:= a.y / b.y;
 Result.z:= a.z / b.z;
end;

//---------------------------------------------------------------------------
{$ifdef fpc}
operator - (const v: TVector3) a: TVector3;
{$else}
class operator TVector3.Negative(const v: TVector3): TVector3;
{$endif}
begin
 Result.x:= -v.x;
 Result.y:= -v.y;
 Result.z:= -v.z;
end;

//---------------------------------------------------------------------------
{$ifdef fpc}
operator * (const v: TVector3; k: Single) a: TVector3;
{$else}
class operator TVector3.Multiply(const v: TVector3;
 const k: Single): TVector3;
{$endif}
begin
 Result.x:= v.x * k;
 Result.y:= v.y * k;
 Result.z:= v.z * k;
end;

//---------------------------------------------------------------------------
{$ifdef fpc}
operator / (const v: TVector3; k: Single) a: TVector3;
{$else}
class operator TVector3.Divide(const v: TVector3;
 const k: Single): TVector3;
{$endif}
begin
 Result.x:= v.x / k;
 Result.y:= v.y / k;
 Result.z:= v.z / k;
end;

//---------------------------------------------------------------------------
{$ifdef fpc}
operator * (k: Single; const v: TVector3) a: TVector3;
{$else}
class operator TVector3.Multiply(const k: Single; const v: TVector3): TVector3;
{$endif}
begin
 Result.x:= k * v.x;
 Result.y:= k * v.y;
 Result.z:= k * v.z;
end;

//---------------------------------------------------------------------------
{$ifdef fpc}
operator / (k: Single; const v: TVector3) a: TVector3;
{$else}
class operator TVector3.Divide(const k: Single; const v: TVector3): TVector3;
{$endif}
begin
 Result.x:= k / v.x;
 Result.y:= k / v.y;
 Result.z:= k / v.z;
end;

//---------------------------------------------------------------------------
function Vector3(x, y, z: Single): TVector3;
begin
 Result.x:= x;
 Result.y:= y;
 Result.z:= z;
end;

//---------------------------------------------------------------------------
function Length3(const v: TVector3): Single;
begin
 Result:= Sqrt(v.x * v.x + v.y * v.y + v.z * v.z);
end;

//---------------------------------------------------------------------------
function Norm3(const v: TVector3): TVector3;
var
 Amp: Single;
begin
 Amp:= Length3(v);

 if (Amp <> 0.0) then
  begin
   Result.x:= v.x / Amp;
   Result.y:= v.y / Amp;
   Result.z:= v.z / Amp;
  end else Result:= ZeroVec3;
end;

//---------------------------------------------------------------------------
function Lerp3(const v0, v1: TVector3; Alpha: Single): TVector3;
begin
 Result.x:= v0.x + (v1.x - v0.x) * Alpha;
 Result.y:= v0.y + (v1.y - v0.y) * Alpha;
 Result.z:= v0.z + (v1.z - v0.z) * Alpha;
end;

//---------------------------------------------------------------------------
function Dot3(const a, b: TVector3): Single;
begin
 Result:= (a.x * b.x) + (a.y * b.y) + (a.z * b.z);
end;

//---------------------------------------------------------------------------
function Cross3(const a, b: TVector3): TVector3;
begin
 Result.x:= (a.y * b.z) - (a.z * b.y);
 Result.y:= (a.z * b.x) - (a.x * b.z);
 Result.z:= (a.x * b.y) - (a.y * b.x);
end;

//---------------------------------------------------------------------------
function Angle3(const a, b: TVector3): Single;
var
 v: Single;
begin
 v:= Dot3(a, b) / (Length3(a) * Length3(b));

 if (v < -1.0) then v:= -1.0
  else if (v > 1.0) then v:= 1.0;

 Result:= ArcCos(v);
end;

//---------------------------------------------------------------------------
function Parallel3(const v, n: TVector3): TVector3;
begin
 Result:= n * (Dot3(v, n) / Sqr(Length3(n)));
end;

//--------------------------------------------------------------------------
function Perp3(const v, n: TVector3): TVector3;
begin
 Result:= v - Parallel3(v, n);
end;

//---------------------------------------------------------------------------
function Reflect3(const i, n: TVector3): TVector3;
begin
 Result:= i - 2.0 * n * Dot3(i, n);
end;

//---------------------------------------------------------------------------
function ColorToVec3(Color: Cardinal): TVector3;
begin
 Result.x:= ((Color shl 8) shr 24) / 255.0;
 Result.y:= ((Color shl 16) shr 24) / 255.0;
 Result.z:= ((Color shl 24) shr 24) / 255.0;
end;

//---------------------------------------------------------------------------
function Vec3ToColor(const v: TVector3): Cardinal;
begin
 Result:= (Round(v.x * 255.0) shl 16) or (Round(v.y * 255.0) shl 8) or
  Round(v.z * 255.0) or $FF000000;
end;

//---------------------------------------------------------------------------
function Vec3toXY(const v: TVector3): TPoint2;
begin
 Result.x:= v.x;
 Result.y:= v.y;
end;

//---------------------------------------------------------------------------
function Vec3toXYpx(const v: TVector3): TPoint2px;
begin
 Result.x:= Round(v.x);
 Result.y:= Round(v.y);
end;

//---------------------------------------------------------------------------
function Vec3toString(const v: TVector3): string;
begin
 Result:= Format('(%1.2f, %1.2f, %1.2f)', [v.x, v.y, v.z]);
end;

//---------------------------------------------------------------------------
constructor TVectors3.Create();
begin
 inherited;

 DataCount:= 0;
end;

//---------------------------------------------------------------------------
destructor TVectors3.Destroy();
begin
 DataCount:= 0;
 SetLength(Data, 0);

 inherited;
end;

//---------------------------------------------------------------------------
function TVectors3.GetItem(Num: Integer): TVector3;
begin
 Result:= Data[Num];
end;

//---------------------------------------------------------------------------
procedure TVectors3.SetItem(Num: Integer; const Value: TVector3);
begin
 Data[Num]:= Value;
end;

//---------------------------------------------------------------------------
function TVectors3.GetVector(Num: Integer): PVector3;
begin
 Result:= @Data[Num];
end;

//---------------------------------------------------------------------------
procedure TVectors3.Request(Amount: Integer);
var
 Required: Integer;
begin
 Required:= ((Amount + CacheSize - 1) div CacheSize) * CacheSize;
 if (Length(Data) < Required) then SetLength(Data, Required);
end;

//---------------------------------------------------------------------------
function TVectors3.Add(const v: TVector3): Integer;
var
 Index: Integer;
begin
 Index:= DataCount;
 Request(DataCount + 1);

 Data[Index]:= v;
 Inc(DataCount);

 Result:= Index;
end;

//---------------------------------------------------------------------------
function TVectors3.Add(x, y, z: Single): Integer;
begin
 Result:= Add(Vector3(x, y, z));
end;

//---------------------------------------------------------------------------
procedure TVectors3.Remove(Index: Integer);
var
 i: Integer;
begin
 if (Index < 0)or(Index >= DataCount) then Exit;

 for i:= Index to DataCount - 2 do
  Data[i]:= Data[i + 1];

 Dec(DataCount);
end;

//---------------------------------------------------------------------------
procedure TVectors3.RemoveAll();
begin
 DataCount:= 0;
end;

//---------------------------------------------------------------------------
procedure TVectors3.CopyFrom(Source: TVectors3);
var
 i: Integer;
begin
 Request(Source.DataCount);

 for i:= 0 to Source.DataCount - 1 do
  Data[i]:= Source.Data[i];

 DataCount:= Source.DataCount;
end;

//---------------------------------------------------------------------------
procedure TVectors3.AddFrom(Source: TVectors3);
var
 i: Integer;
begin
 Request(DataCount + Source.DataCount);

 for i:= 0 to Source.DataCount - 1 do
  Data[i + DataCount]:= Source.Data[i];

 Inc(DataCount, Source.DataCount);
end;

//--------------------------------------------------------------------------
procedure TVectors3.Normalize();
var
 i: Integer;
begin
 for i:= 0 to DataCount - 1 do
  Data[i]:= Norm3(Data[i]);
end;

//--------------------------------------------------------------------------
procedure TVectors3.Rescale(Scale: Single);
var
 i: Integer;
begin
 for i:= 0 to DataCount - 1 do
  Data[i]:= Data[i] * Scale;
end;

//---------------------------------------------------------------------------
procedure TVectors3.Invert();
var
 i: Integer;
begin
 for i:= 0 to DataCount - 1 do
  Data[i]:= -Data[i];
end;

//--------------------------------------------------------------------------
procedure TVectors3.Centralize();
var
 i: Integer;
 MinPoint: TVector3;
 MaxPoint: TVector3;
 Middle  : TVector3;
begin
 if (DataCount < 1) then Exit;

 MinPoint:= Vector3(High(Integer), High(Integer), High(Integer));
 MaxPoint:= Vector3(Low(Integer), Low(Integer), Low(Integer));

 for i:= 0 to DataCount - 1 do
  begin
   MinPoint.x:= Min(MinPoint.x, Data[i].x);
   MinPoint.y:= Min(MinPoint.y, Data[i].y);
   MinPoint.z:= Min(MinPoint.z, Data[i].z);

   MaxPoint.x:= Max(MaxPoint.x, Data[i].x);
   MaxPoint.y:= Max(MaxPoint.y, Data[i].y);
   MaxPoint.z:= Max(MaxPoint.z, Data[i].z);
  end;

 Middle.x:= (MinPoint.x + MaxPoint.x) / 2.0;
 Middle.y:= (MinPoint.y + MaxPoint.y) / 2.0;
 Middle.z:= (MinPoint.z + MaxPoint.z) / 2.0;

 for i:= 0 to DataCount - 1 do
  Data[i]:= Data[i] - Middle;
end;

//---------------------------------------------------------------------------
procedure TVectors3.SaveToStream(Stream: TStream);
var
 i: Integer;
begin
 Stream.WriteBuffer(DataCount, SizeOf(Integer));

 for i:= 0 to DataCount - 1 do
  Stream.WriteBuffer(Data[i], SizeOf(TVector3));
end;

//---------------------------------------------------------------------------
procedure TVectors3.LoadFromStream(Stream: TStream);
var
 NewAmount, i: Integer;
begin
 Stream.ReadBuffer(NewAmount, SizeOf(Integer));
 Request(NewAmount);

 for i:= 0 to NewAmount - 1 do
  Stream.ReadBuffer(Data[i], SizeOf(TVector3));

 DataCount:= NewAmount;
end;

//---------------------------------------------------------------------------
end.
