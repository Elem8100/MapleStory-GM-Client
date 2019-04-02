unit Vectors2px;
//---------------------------------------------------------------------------
// Vectors2px.pas                                       Modified: 20-Jul-2009
// Definitions and functions working with 2D integer vectors     Version 1.02
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
// The Original Code is Vectors2px.pas.
//
// The Initial Developer of the Original Code is Yuriy Kotsarenko.
// Portions created by Yuriy Kotsarenko are Copyright (C) 2007 - 2008,
// Yuriy Kotsarenko. All Rights Reserved.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 System.Types, Math;

//---------------------------------------------------------------------------
type
 PPoint2px = ^TPoint2px;
 TPoint2px = record
  x, y: Integer;

  {$ifndef fpc}
  class operator Add(const a, b: TPoint2px): TPoint2px;
  class operator Subtract(const a, b: TPoint2px): TPoint2px;
  class operator Multiply(const a, b: TPoint2px): TPoint2px;
  class operator Divide(const a, b: TPoint2px): TPoint2px;

  class operator Negative(const v: TPoint2px): TPoint2px;
  class operator Multiply(const v: TPoint2px; k: Single): TPoint2px;
  class operator Multiply(const v: TPoint2px; k: Integer): TPoint2px;
  class operator Divide(const v: TPoint2px; k: Single): TPoint2px;
  class operator Divide(const v: TPoint2px; k: Integer): TPoint2px;
  class operator Implicit(const Point: TPoint): TPoint2px;
  class operator Implicit(const Point: TPoint2px): TPoint;
  class operator Equal(const a, b: TPoint2px): Boolean;
  class operator NotEqual(const a, b: TPoint2px): Boolean;
  {$endif}
 end;

//---------------------------------------------------------------------------
 TPoints2px = class;

//---------------------------------------------------------------------------
 TPoints2pxEnumerator = class
 private
  List : TPoints2px;
  Index: Integer;

  function GetCurrent(): TPoint2px;
 public
  property Current: TPoint2px read GetCurrent;

  function MoveNext(): Boolean;

  constructor Create(AList: TPoints2px);
 end;

//---------------------------------------------------------------------------
 TPoints2px = class
 private
  Data: array of TPoint2px;
  DataCount: Integer;

  function GetItem(Num: Integer): PPoint2px;
  procedure Request(Amount: Integer);
  function GetMemAddr(): Pointer;
  procedure SetCount(const Value: Integer);
 public
  property MemAddr: Pointer read GetMemAddr;
  property Count: Integer read DataCount write SetCount;
  property Item[Num: Integer]: PPoint2px read GetItem; default;

  function IndexOf(const Point: TPoint2px): Integer; overload;
  function IndexOf(x, y: Integer): Integer; overload;
  function Add(const Point: TPoint2px): Integer; overload;
  function Add(x, y: Integer): Integer; overload;
  procedure Remove(Index: Integer);
  procedure RemoveAll();

  procedure CopyFrom(Source: TPoints2px);
  procedure AddFrom(Source: TPoints2px);

  function GetEnumerator(): TPoints2pxEnumerator;

  constructor Create();
  destructor Destroy(); override;
 end;

//---------------------------------------------------------------------------
const
 ZeroPoint2px : TPoint2px = (x: 0; y: 0);
 UnityPoint2px: TPoint2px = (x: 1; y: 1);
 InfPoint2px  : TPoint2px = (x: Low(Integer); y: Low(Integer));

//---------------------------------------------------------------------------
{$ifdef fpc}
operator + (const a, b: TPoint2px) c: TPoint2px;
operator - (const a, b: TPoint2px) c: TPoint2px;
operator * (const a, b: TPoint2px) c: TPoint2px;
operator / (const a, b: TPoint2px) c: TPoint2px;
operator - (const v: TPoint2px) a: TPoint2px;
operator * (const v: TPoint2px; k: Integer) a: TPoint2px;
operator * (const v: TPoint2px; k: Single) a: TPoint2px;
operator / (const v: TPoint2px; k: Integer) a: TPoint2px;
operator / (const v: TPoint2px; k: Single) a: TPoint2px;
operator := (const Point: TPoint) v: TPoint2px;
operator := (const Point: TPoint2px) v: TPoint;
operator = (const a, b: TPoint2px) c: Boolean;
{$endif}

//---------------------------------------------------------------------------
function Point2px(x, y: Integer): TPoint2px;
function Length2px(const v: TPoint2px): Single;
function Angle2px(const v: TPoint2px): Single;
function Lerp2px(const v0, v1: TPoint2px; Alpha: Single): TPoint2px;
function Dot2px(const a, b: TPoint2px): Integer;

//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
const
 CacheSize = 128;

//---------------------------------------------------------------------------
{$ifdef fpc}
operator + (const a, b: TPoint2px) c: TPoint2px;
{$else}
class operator TPoint2px.Add(const a, b: TPoint2px): TPoint2px;
{$endif}
begin
 Result.x:= a.x + b.x;
 Result.y:= a.y + b.y;
end;

//---------------------------------------------------------------------------
{$ifdef fpc}
operator - (const a, b: TPoint2px) c: TPoint2px;
{$else}
class operator TPoint2px.Subtract(const a, b: TPoint2px): TPoint2px;
{$endif}
begin
 Result.x:= a.x - b.x;
 Result.y:= a.y - b.y;
end;

//---------------------------------------------------------------------------
{$ifdef fpc}
operator * (const a, b: TPoint2px) c: TPoint2px;
{$else}
class operator TPoint2px.Multiply(const a, b: TPoint2px): TPoint2px;
{$endif}
begin
 Result.x:= a.x * b.x;
 Result.y:= a.y * b.y;
end;

//---------------------------------------------------------------------------
{$ifdef fpc}
operator / (const a, b: TPoint2px) c: TPoint2px;
{$else}
class operator TPoint2px.Divide(const a, b: TPoint2px): TPoint2px;
{$endif}
begin
 Result.x:= a.x div b.x;
 Result.y:= a.y div b.y;
end;

//---------------------------------------------------------------------------
{$ifdef fpc}
operator - (const v: TPoint2px) a: TPoint2px;
{$else}
class operator TPoint2px.Negative(const v: TPoint2px): TPoint2px;
{$endif}
begin
 Result.x:= -v.x;
 Result.y:= -v.y;
end;

//---------------------------------------------------------------------------
{$ifdef fpc}
operator * (const v: TPoint2px; k: Integer) a: TPoint2px;
{$else}
class operator TPoint2px.Multiply(const v: TPoint2px;
 k: Integer): TPoint2px;
{$endif}
begin
 Result.x:= v.x * k;
 Result.y:= v.y * k;
end;

//---------------------------------------------------------------------------
{$ifdef fpc}
operator * (const v: TPoint2px; k: Single) a: TPoint2px;
{$else}
class operator TPoint2px.Multiply(const v: TPoint2px;
 k: Single): TPoint2px;
{$endif}
begin
 Result.x:= Round(v.x * k);
 Result.y:= Round(v.y * k);
end;

//---------------------------------------------------------------------------
{$ifdef fpc}
operator / (const v: TPoint2px; k: Integer) a: TPoint2px;
{$else}
class operator TPoint2px.Divide(const v: TPoint2px;
 k: Integer): TPoint2px;
{$endif}
begin
 Result.x:= v.x div k;
 Result.y:= v.y div k;
end;

//---------------------------------------------------------------------------
{$ifdef fpc}
operator / (const v: TPoint2px; k: Single) a: TPoint2px;
{$else}
class operator TPoint2px.Divide(const v: TPoint2px;
 k: Single): TPoint2px;
{$endif}
begin
 Result.x:= Round(v.x / k);
 Result.y:= Round(v.y / k);
end;

//---------------------------------------------------------------------------
{$ifdef fpc}
operator := (const Point: TPoint) v: TPoint2px;
{$else}
class operator TPoint2px.Implicit(const Point: TPoint): TPoint2px;
{$endif}
begin
 Result.x:= Point.X;
 Result.y:= Point.Y;
end;

//---------------------------------------------------------------------------
{$ifdef fpc}
operator := (const Point: TPoint2px) v: TPoint;
{$else}
class operator TPoint2px.Implicit(const Point: TPoint2px): TPoint;
{$endif}
begin
 Result.X:= Point.x;
 Result.Y:= Point.y;
end;

//---------------------------------------------------------------------------
{$ifdef fpc}
operator = (const a, b: TPoint2px) c: Boolean;
{$else}
class operator TPoint2px.Equal(const a, b: TPoint2px): Boolean;
{$endif}
begin
 Result:= (a.x = b.x)and(a.y = b.y);
end;

//---------------------------------------------------------------------------
{$ifndef fpc}
class operator TPoint2px.NotEqual(const a, b: TPoint2px): Boolean;
begin
 Result:= (a.x <> b.x)or(a.y <> b.y);
end;
{$endif}

//---------------------------------------------------------------------------
function Point2px(x, y: Integer): TPoint2px;
begin
 Result.x:= x;
 Result.y:= y;
end;

//---------------------------------------------------------------------------
function Length2px(const v: TPoint2px): Single;
begin
 Result:= Hypot(v.x, v.y);
end;

//---------------------------------------------------------------------------
function Angle2px(const v: TPoint2px): Single;
begin
 Result:= ArcTan2(v.y, v.x);
end;

//---------------------------------------------------------------------------
function Lerp2px(const v0, v1: TPoint2px; Alpha: Single): TPoint2px;
begin
 Result.x:= Round(v0.x + (v1.x - v0.x) * Alpha);
 Result.y:= Round(v0.y + (v1.y - v0.y) * Alpha);
end;

//---------------------------------------------------------------------------
function Dot2px(const a, b: TPoint2px): Integer;
begin
 Result:= (a.x * b.x) + (a.y * b.y);
end;

//---------------------------------------------------------------------------
constructor TPoints2px.Create();
begin
 inherited;

 DataCount:= 0;
end;

//---------------------------------------------------------------------------
destructor TPoints2px.Destroy();
begin
 DataCount:= 0;
 SetLength(Data, 0);

 inherited;
end;

//---------------------------------------------------------------------------
function TPoints2px.GetMemAddr(): Pointer;
begin
 Result:= @Data[0];
end;

//---------------------------------------------------------------------------
procedure TPoints2px.SetCount(const Value: Integer);
begin
 Request(Value);
 DataCount:= Value;
end;

//---------------------------------------------------------------------------
function TPoints2px.GetItem(Num: Integer): PPoint2px;
begin
 if (Num >= 0)and(Num < DataCount) then Result:= @Data[Num]
  else Result:= nil;
end;

//---------------------------------------------------------------------------
procedure TPoints2px.Request(Amount: Integer);
var
 Required: Integer;
begin
 Required:= Ceil(Amount / CacheSize) * CacheSize;
 if (Length(Data) < Required) then SetLength(Data, Required);
end;

//---------------------------------------------------------------------------
function TPoints2px.Add(const Point: TPoint2px): Integer;
var
 Index: Integer;
begin
 Index:= DataCount;
 Request(DataCount + 1);

 Data[Index]:= Point;
 Inc(DataCount);

 Result:= Index;
end;

//---------------------------------------------------------------------------
function TPoints2px.Add(x, y: Integer): Integer;
begin
 Result:= Add(Point2px(x, y));
end;

//---------------------------------------------------------------------------
procedure TPoints2px.Remove(Index: Integer);
var
 i: Integer;
begin
 if (Index < 0)or(Index >= DataCount) then Exit;

 for i:= Index to DataCount - 2 do
  Data[i]:= Data[i + 1];

 Dec(DataCount);
end;

//---------------------------------------------------------------------------
procedure TPoints2px.RemoveAll();
begin
 DataCount:= 0;
end;

//---------------------------------------------------------------------------
procedure TPoints2px.CopyFrom(Source: TPoints2px);
var
 i: Integer;
begin
 Request(Source.DataCount);

 for i:= 0 to Source.DataCount - 1 do
  Data[i]:= Source.Data[i];

 DataCount:= Source.DataCount;
end;

//---------------------------------------------------------------------------
procedure TPoints2px.AddFrom(Source: TPoints2px);
var
 i: Integer;
begin
 Request(DataCount + Source.DataCount);

 for i:= 0 to Source.DataCount - 1 do
  Data[i + DataCount]:= Source.Data[i];

 Inc(DataCount, Source.DataCount);
end;

//---------------------------------------------------------------------------
function TPoints2px.IndexOf(const Point: TPoint2px): Integer;
var
 i: Integer;
begin
 Result:= -1;

 for i:= 0 to DataCount - 1 do
  if (Data[i] = Point) then
   begin
    Result:= i;
    Break;
   end;
end;

//---------------------------------------------------------------------------
function TPoints2px.IndexOf(x, y: Integer): Integer;
begin
 Result:= IndexOf(Point2px(x, y));
end;

//---------------------------------------------------------------------------
function TPoints2px.GetEnumerator(): TPoints2pxEnumerator;
begin
 Result:= TPoints2pxEnumerator.Create(Self);
end;

//---------------------------------------------------------------------------
constructor TPoints2pxEnumerator.Create(AList: TPoints2px);
begin
 inherited Create();

 List := AList;
 Index:= -1;
end;

//---------------------------------------------------------------------------
function TPoints2pxEnumerator.GetCurrent(): TPoint2px;
begin
 Result:= List.Item[Index]^;
end;

//---------------------------------------------------------------------------
function TPoints2pxEnumerator.MoveNext(): Boolean;
begin
 Result:= Index < List.Count - 1;
 if (Result) then Inc(Index);
end;

//---------------------------------------------------------------------------
end.
