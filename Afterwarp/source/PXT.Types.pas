unit PXT.Types;
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
{< Essential types, constants and functions working with vectors, colors, pixels and rectangles that are
   used throughout the framework. }
interface

{$INCLUDE PXT.Config.inc}

uses
  Math;

{$REGION 'Basic Types'}

type
{$IFDEF DELPHI_NO_UTF8STRING}
  Utf8String = AnsiString;
{$ENDIF}

{$IFNDEF FPC}
  {$IFDEF DELPHI_MODERN}
    // Pointer type represented as a signed integer.
    PtrInt = NativeInt;

    // Pointer type represented as an unsigned integer.
    PtrUInt = NativeUInt;
  {$ELSE}
    PtrInt = Integer;
    PtrUInt = Cardinal;
  {$ENDIF}
{$ENDIF}

{$IFNDEF FPC}
  // Pointer to @link(SizeInt).
  PSizeInt = ^SizeInt;

  // Signed integer data type having the same size as pointer on the given platform.
  SizeInt = PtrInt;

  // Pointer to @link(SizeUInt).
  PSizeUInt = ^SizeUInt;

  // Unsigned integer data type having the same size as pointer on the given platform.
  SizeUInt = PtrUInt;
{$ENDIF}

  // Pointer to @link(VectorFloat).
  PVectorFloat = ^VectorFloat;

  { Floating-point data type that is commonly used in the framework. Typically, it is an equivalent of
    @italic(Single), unless PXT_SCIENTIFIC_MODE is enabled, in which case it becomes equivalent of
    @italic(Double). }
  VectorFloat = {$IFDEF PXT_SCIENTIFIC_MODE} Double {$ELSE} Single {$ENDIF};

  // Pointer to @link(VectorInt).
  PVectorInt = ^VectorInt;

  { Signed integer data type that is commonly used in the framework. Typically, it is 32-bit and an
    equivalent of @italic(Integer), unless PXT_SCIENTIFIC_MODE is enabled, in which case on 64-bit platforms
    it becomes 64-bit and equivalent of @italic(Int64). }
  VectorInt =
  {$IFDEF FPC}
    {$IF DEFINED(PXT_SCIENTIFIC_MODE)}
      Int64
    {$ELSE}
      Integer
    {$IFEND}
  {$ELSE}
    {$IFDEF PXT_SCIENTIFIC_MODE}
      {$IF DEFINED(PXT_SCIENTIFIC_MODE) AND DEFINED(CPUX64)}
        NativeInt
      {$ELSE}
        Int64
      {$IFEND}
    {$ELSE}
      Integer
    {$ENDIF}
  {$ENDIF};

  // Pointer to @link(VectorUInt).
  PVectorUInt = ^VectorUInt;

  { Unsigned integer data type that is commonly used in the framework. Typically, it is 32-bit and an
    equivalent of @italic(Cardinal), unless PXT_SCIENTIFIC_MODE_MAX is enabled, in which case on 64-bit
    platforms it becomes 64-bit and equivalent of  @italic(UInt64). }
  VectorUInt =
  {$IFDEF FPC}
    {$IF DEFINED(PXT_SCIENTIFIC_MODE)}
      UInt64
    {$ELSE}
      Cardinal
    {$IFEND}
  {$ELSE}
    {$IFDEF PXT_SCIENTIFIC_MODE}
      {$IF DEFINED(PXT_SCIENTIFIC_MODE) AND DEFINED(CPUX64)}
        NativeUInt
      {$ELSE}
        UInt64
      {$IFEND}
    {$ELSE}
      Cardinal
    {$ENDIF}
  {$ENDIF};

{$IFDEF DELPHI_LEGACY}
  {$INCLUDE PXT.Enums.Legacy.inc}
{$ELSE}
  {$INCLUDE PXT.Enums.inc}
{$ENDIF}

type
  { Data type meant for storing cross-platform handles. This is a signed integer with the same size as
    pointer on the given platform. }
  TUntypedHandle = {$IFDEF DELPHI_LEGACY}Integer{$ELSE}NativeInt{$ENDIF};

  // Pointer to @link(TLibraryClassHandle).
  PLibraryClassHandle = ^TLibraryClassHandle;

  // Generic class handle that is used by the library.
  TLibraryClassHandle = Pointer;

  // Pointer to @link(TLibraryBool).
  PLibraryBool = ^TLibraryBool;

  // Boolean type returned by the library.
  TLibraryBool = ByteBool;

const
  // A special value that determines precision limit when comparing vectors and coordinates.
  VectorEpsilon = 1E-6;

{$ENDREGION}
{$REGION 'TIntColor'}

type
  // Pointer to @link(TIntColorValue).
  PIntColorValue = ^TIntColorValue;

  { Raw (untyped) color value is represented as a raw 32-bit unsigned integer, with components allocated
    according to @italic(TPixelFormat.ARGB8) format. }
  TIntColorValue = {$IFDEF NEXTGEN}Cardinal{$ELSE}LongWord{$ENDIF};

  // Pointer to @link(TIntColor).
  PIntColor = ^TIntColor;

  { General-purpose color value that is represented as 32-bit unsigned integer, with components allocated
    according to @italic(TPixelFormat.ARGB8) format. }
  TIntColor = {$IFDEF NEXTGEN}Cardinal{$ELSE}LongWord{$ENDIF};

  // Pointer to @link(TIntColorRec).
  PIntColorRec = ^TIntColorRec;

  { Alternative representation of @link(TIntColor), where each element can be accessed as an individual value.
    This can be safely typecast to @link(TIntColor) and vice-versa. }
  TIntColorRec = record
    case Cardinal of
      0: (// Blue value ranging from 0 (no intensity) to 255 (fully intense).
          Blue: Byte;
          // Green value ranging from 0 (no intensity) to 255 (fully intense).
          Green: Byte;
          // Red value ranging from 0 (no intensity) to 255 (fully intense).
          Red: Byte;
          // Alpha-channel value ranging from 0 (translucent) to 255 (opaque).
          Alpha: Byte;);
      1: { Values represented as an array, with indexes corresponding to blue (0), green (1), red (2) and
          alpha-channel (3). }
         (Values: packed array[0..3] of Byte);
  end;

  // Pointer to @link(TIntColorPalette).
  PIntColorPalette = ^TIntColorPalette;

  // A fixed palette of 256 colors, typically used to emulate legacy 8-bit indexed modes.
  TIntColorPalette = array[0..255] of TIntColor;

const
  // Predefined constant for opaque Black color.
  IntColorBlack = $FF000000;

  // Predefined constant for opaque White color.
  IntColorWhite = $FFFFFFFF;

  // Predefined constant for translucent Black color.
  IntColorTranslucentBlack = $00000000;

  // Predefined constant for translucent White color.
  IntColorTranslucentWhite = $00FFFFFF;

{$ENDREGION}
{$REGION 'TColorPair'}

type
  // Pointer to @link(TColorPair).
  PColorPair = ^TColorPair;

  { A combination of two colors, primarily used for displaying text with the first color being on top and the
    second being on bottom. The format for specifying colors is defined as @italic(TPixelFormat.ARGB8). }
  TColorPair = record
  public
    { @exclude } class operator Implicit(const Color: TIntColor): TColorPair; inline;
    { @exclude } class operator Equal(const Color1, Color2: TColorPair): Boolean;
    { @exclude } class operator NotEqual(const Color1, Color2: TColorPair): Boolean; inline;

    // Returns @True if both values equal to zero.
    function Empty: Boolean;

    { Returns @True if two colors are different at least in one of their red, green, blue or alpha
      components. }
    function HasGradient: Boolean;

    // Returns @True if at least one of the colors has non-zero alpha channel.
    function HasAlpha: Boolean;

    // Returns @True if at least one of the colors has alpha channel less than 255.
    function HasTransparency: Boolean;
  public
    case Cardinal of
      0: (// First color entry, which can be reinterpreted as top or left color depending on context.
          First: TIntColor;

          // Second color entry, which can be reinterpreted as bottom or right color depending on context.
          Second: TIntColor;);
      1:// Two color pair represented as an array.
        (Values: array[0..1] of TIntColor;);
  end;

const
  // Predefined constant for a pair of opaque Black colors.
  ColorPairBlack: TColorPair = (First: $FF000000; Second: $FF000000);

  // Predefined constant for a pair of opaque White colors.
  ColorPairWhite: TColorPair = (First: $FFFFFFFF; Second: $FFFFFFFF);

  // Predefined constant for a pair of translucent Black colors.
  ColorPairTranslucentBlack: TColorPair = (First: $00000000; Second: $00000000);

  // Predefined constant for a pair of translucent White colors.
  ColorPairTranslucentWhite: TColorPair = (First: $00FFFFFF; Second: $00FFFFFF);

// Creates two 32-bit RGBA color gradient from specified pair of values.
function ColorPair(const First, Second: TIntColor): TColorPair; overload; inline;

// Creates two 32-bit RGBA color gradient with both values set to specified color.
function ColorPair(const Color: TIntColor): TColorPair; overload; inline;

// Modifies alpha-channel of a given color pair multiplying it with the given alpha value.
function ColorPairAlpha(const AColorPair: TColorPair; const AAlpha: Integer): TColorPair;

{$ENDREGION}
{$REGION 'TColorRect'}

type
  // Pointer to @link(TColorRect).
  PColorRect = ^TColorRect;

  { A combination of four colors, primarily used for displaying colored quads, where each color corresponds
    to top/left,top/right, bottom/right and bottom/left accordingly (clockwise). The format for specifying
    colors is defined as @italic(TPixelFormat.ARGB8). }
  TColorRect = record
  public
    { @exclude } class operator Implicit(const Color: TIntColor): TColorRect; inline;
    { @exclude } class operator Equal(const Color1, Color2: TColorRect): Boolean;
    { @exclude } class operator NotEqual(const Color1, Color2: TColorRect): Boolean; inline;

    // Returns @True if each of four values equals to zero.
    function Empty: Boolean;

    { Returns @True if at least one of four colors is different from others in red, green, blue or alpha
      components. }
    function HasGradient: Boolean;

    // Returns @True if at least one of the colors has non-zero alpha channel.
    function HasAlpha: Boolean;

    // Returns @True if at least one of the colors has alpha channel less than 255.
    function HasTransparency: Boolean;
  public
    case Cardinal of
      0: (// Color corresponding to top/left corner.
          TopLeft: TIntColor;
          // Color corresponding to top/right corner.
          TopRight: TIntColor;
          // Color corresponding to bottom/right corner.
          BottomRight: TIntColor;
          // Color corresponding to bottom/left corner.
          BottomLeft: TIntColor;
        );
      1: // Four colors represented as an array.
        (Values: array[0..3] of TIntColor);
  end;

const
  // Predefined constant for four opaque Black colors.
  ColorRectBlack: TColorRect = (TopLeft: $FF000000; TopRight: $FF000000; BottomRight: $FF000000;
    BottomLeft: $FF000000);

  // Predefined constant for four opaque White colors.
  ColorRectWhite: TColorRect = (TopLeft: $FFFFFFFF; TopRight: $FFFFFFFF; BottomRight: $FFFFFFFF;
    BottomLeft: $FFFFFFFF);

  // Predefined constant for four translucent Black colors.
  ColorRectTranslucentBlack: TColorRect = (TopLeft: $00000000; TopRight: $00000000; BottomRight: $00000000;
    BottomLeft: $00000000);

  // Predefined constant for four translucent White colors.
  ColorRectTranslucentWhite: TColorRect = (TopLeft: $00FFFFFF; TopRight: $00FFFFFF; BottomRight: $00FFFFFF;
    BottomLeft: $00FFFFFF);

// Creates a construct of four colors using individual components.
function ColorRect(const TopLeft, TopRight, BottomRight, BottomLeft: TIntColor): TColorRect; overload; inline;

// Creates a construct of four colors having the same component in each corner.
function ColorRect(const Color: TIntColor): TColorRect; overload; inline;

// Creates a construct of four colors from two color pair to create horizontal gradient.
function ColorRectH(const Color: TColorPair): TColorRect; overload; inline;

// Creates a construct of four colors from two color values to create horizontal gradient.
function ColorRectH(const Left, Right: TIntColor): TColorRect; overload; inline;

// Creates a construct of four colors from two color pair to create vertical gradient.
function ColorRectV(const Color: TColorPair): TColorRect; overload; inline;

// Creates a construct of four colors from two color values to create vertical gradient.
function ColorRectV(const Top, Bottom: TIntColor): TColorRect; overload; inline;

// Modifies alpha-channel of a given color rectangle multiplying it with the given alpha value.
function ColorRectAlpha(const AColorRect: TColorRect; const AAlpha: Integer): TColorRect;

{$ENDREGION}
{$REGION 'TFloatColor'}

type
  // Pointer to @link(TFloatColor).
  PFloatColor = ^TFloatColor;

  { A special high-precision color value that has each individual component represented as 32-bit
    floating-point value in range of [0, 1]. Although components may have values outside of aforementioned
    range, such colors cannot be reliably displayed on the screen. }
  TFloatColor = record
  public
    { @exclude } class operator Add(const Color1, Color2: TFloatColor): TFloatColor;
    { @exclude } class operator Subtract(const Color1, Color2: TFloatColor): TFloatColor;
    { @exclude } class operator Multiply(const Color1, Color2: TFloatColor): TFloatColor;
    { @exclude } class operator Divide(const Color1, Color2: TFloatColor): TFloatColor;
    { @exclude } class operator Multiply(const Color: TFloatColor; const Theta: VectorFloat): TFloatColor;
    { @exclude } class operator Multiply(const Theta: VectorFloat; const Color: TFloatColor): TFloatColor;
    { @exclude } class operator Divide(const Color: TFloatColor; const Theta: VectorFloat): TFloatColor;
    { @exclude } class operator Equal(const Color1, Color2: TFloatColor): Boolean;
    { @exclude } class operator NotEqual(const Color1, Color2: TFloatColor): Boolean; inline;

    // Inverts current color by applying formula "Xn = 1.0 - X" for each component.
    function Invert: TFloatColor;

    { Takes current color with unpremultiplied alpha and multiplies each of red, green, and blue components
      by its alpha channel, resulting in premultiplied alpha color. }
    function PremultiplyAlpha: TFloatColor;

    { Takes current color with premultiplied alpha channel and divides each of its red, green, and blue
      components by alpha, resulting in unpremultiplied alpha color. }
    function UnpremultiplyAlpha: TFloatColor;

    // Computes average between current and destination floating-point color values.
    function Average(const Color: TFloatColor): TFloatColor;

    { Computes alpha-blending between current and destination floating-point colors values.
        @italic(Alpha) can be in [0..1] range. }
    function Lerp(const Color: TFloatColor; const Alpha: VectorFloat): TFloatColor;

    { Returns grayscale value in range of [0..1] from the current color value. The resulting value can be
      considered the color's @italic(luma). The alpha-channel is ignored. }
    function Gray: VectorFloat;

    // Clamps each component of current color to [0, 1] range.
    function Saturate: TFloatColor;

    // Converts current floating-point color to 32-bit integer representation.
    function ToInt: TIntColor;
  public
    case Cardinal of
      0: (// Red value ranging from 0.0 (no intensity) to 1.0 (fully intense).
          Red: VectorFloat;
          // Green value ranging from 0.0 (no intensity) to 1.0 (fully intense).
          Green: VectorFloat;
          // Blue value ranging from 0.0 (no intensity) to 1.0 (fully intense).
          Blue: VectorFloat;
          // Alpha-channel value ranging from 0.0 (translucent) to 1.0 (opaque).
          Alpha: VectorFloat;);
      1: { Values represented as an array, with indexes corresponding to red (0), green (1), blue (2) and
           alpha-channel (3). }
         (Values: array[0..3] of VectorFloat);
  end;

const
  // Predefined constant for opaque Black color.
  FloatColorBlack: TFloatColor = (Red: 0.0; Green: 0.0; Blue: 0.0; Alpha: 1.0);

  // Predefined constant for opaque White color.
  FloatColorWhite: TFloatColor = (Red: 1.0; Green: 1.0; Blue: 1.0; Alpha: 1.0);

  // Predefined constant for translucent Black color.
  FloatColorTranslucentBlack: TFloatColor = (Red: 0.0; Green: 0.0; Blue: 0.0; Alpha: 0.0);

  // Predefined constant for translucent White color.
  FloatColorTranslucentWhite: TFloatColor = (Red: 1.0; Green: 1.0; Blue: 1.0; Alpha: 0.0);

// Creates floating-point color from its 32-bit integer representation.
function FloatColor(const Color: TIntColor): TFloatColor; overload;

// Creates floating-point color using specified individual components for red, green, blue and alpha channel.
function FloatColor(const Red, Green, Blue: VectorFloat;
  const Alpha: VectorFloat = 1.0): TFloatColor; overload;

{$ENDREGION}
{$REGION 'TPoint2i declarations'}

type
  // Pointer to @link(TPoint2i).
  PPoint2i = ^TPoint2i;

  // 2D integer vector.
  TPoint2i = record
    // The coordinate in 2D space.
    X, Y: VectorInt;

    { @exclude } class operator Add(const Point1, Point2f: TPoint2i): TPoint2i; inline;
    { @exclude } class operator Subtract(const Point1, Point2f: TPoint2i): TPoint2i; inline;
    { @exclude } class operator Multiply(const Point1, Point2f: TPoint2i): TPoint2i; inline;
    { @exclude } class operator Divide(const Point1, Point2f: TPoint2i): TPoint2i; inline;
    { @exclude } class operator Negative(const Point: TPoint2i): TPoint2i; inline;
    { @exclude } class operator Multiply(const Point: TPoint2i; const Theta: VectorInt): TPoint2i; inline;
    { @exclude } class operator Multiply(const Theta: VectorInt; const Point: TPoint2i): TPoint2i; inline;
    { @exclude } class operator Divide(const Point: TPoint2i; const Theta: VectorInt): TPoint2i; inline;
    { @exclude } class operator Divide(const Point: TPoint2i; const Theta: VectorFloat): TPoint2i; inline;
    { @exclude } class operator Equal(const Point1, Point2f: TPoint2i): Boolean; inline;
    { @exclude } class operator NotEqual(const Point1, Point2f: TPoint2i): Boolean; inline;

    // Returns vector with X and Y swapped.
    function Swap: TPoint2i; inline;

    // Tests whether both X and Y are zero.
    function Empty: Boolean; inline;

    // Returns length of current vector.
    function Length: VectorFloat; inline;

    // Calculates distance between current and given points.
    function Distance(const Point: TPoint2i): VectorFloat; inline;

    // Returns an angle (in radians) at which the current vector is pointing at.
    function Angle: VectorFloat; inline;

    { Calculates a dot product between current and the specified 2D vector. The dot product is an indirect
      measure of the angle between two vectors. }
    function Dot(const Point: TPoint2i): VectorInt; inline;

    // Calculates a cross product between current and the specified 2D vector, or analog of thereof.
    function Cross(const Point: TPoint2i): VectorInt; inline;

    { Interpolates between current and destination 2D integer vectors.
        @param(Point The destination vector to be used in the interpolation)
        @param(Theta The mixture of the two vectors with the a range of [0..1].) }
    function Lerp(const Point: TPoint2i; const Theta: VectorFloat): TPoint2i;

    // Tests whether current point is inside the triangle specified by given three vertices.
    function InsideTriangle(const Vertex1, Vertex2, Vertex3: TPoint2i): Boolean;
  end;

const
  // Predefined constant, where X and Y are zero.
  ZeroPoint2i: TPoint2i = (X: 0; Y: 0);

  // Predefined constant, where X and Y are one.
  UnityPoint2i: TPoint2i = (X: 1; Y: 1);

  // Predefined constant, where X = 1 and Y = 0.
  AxisXPoint2i: TPoint2i = (X: 1; Y: 0);

  // Predefined constant, where X = 0 and Y = 1.
  AxisYPoint2i: TPoint2i = (X: 0; Y: 1);

  // Predefined constant for "negative infinity".
  MinusInfinity2i: TPoint2i = (X: Low(VectorInt) + 1; Y: Low(VectorInt) + 1);

  // Predefined constant for "positive infinity".
  PlusInfinity2i: TPoint2i = (X: High(VectorInt); Y: High(VectorInt));

  // Predefined constant that can be interpreted as "undefined value".
  Undefined2i: TPoint2i = (X: Low(VectorInt); Y: Low(VectorInt));

// Creates a @link(TPoint2i) record using the specified coordinates.
function Point2i(const X, Y: VectorInt): TPoint2i;

{$ENDREGION}
{$REGION 'TPoint2f declarations'}

type
  // Pointer to @link(TPoint2f).
  PPoint2f = ^TPoint2f;

  // 2D floating-point vector.
  TPoint2f = record
    // The coordinate in 2D space.
    X, Y: VectorFloat;

    { @exclude } class operator Add(const APoint1, APoint2: TPoint2f): TPoint2f; inline;
    { @exclude } class operator Subtract(const APoint1, APoint2: TPoint2f): TPoint2f; inline;
    { @exclude } class operator Multiply(const APoint1, APoint2: TPoint2f): TPoint2f; inline;
    { @exclude } class operator Divide(const APoint1, APoint2: TPoint2f): TPoint2f; inline;
    { @exclude } class operator Negative(const Point: TPoint2f): TPoint2f; inline;
    { @exclude } class operator Multiply(const Point: TPoint2f; const Theta: VectorFloat): TPoint2f; inline;
    { @exclude } class operator Multiply(const Theta: VectorFloat; const Point: TPoint2f): TPoint2f; inline;
    { @exclude } class operator Divide(const Point: TPoint2f; const Theta: VectorFloat): TPoint2f; inline;
    { @exclude } class operator Divide(const Point: TPoint2f; const Theta: Integer): TPoint2f; inline;
    { @exclude } class operator Implicit(const Point: TPoint2i): TPoint2f; inline;
    { @exclude } class operator Equal(const APoint1, APoint2: TPoint2f): Boolean; inline;
    { @exclude } class operator NotEqual(const APoint1, APoint2: TPoint2f): Boolean; inline;

  {$IFDEF POINT_FLOAT_TO_INT_IMPLICIT}
    { This implicit conversion is only allowed as a compiler directive because it causes ambiguity and precision
      problems when excessively used. In addition, it can make code more confusing. }
    { @exclude } class operator Implicit(const Point: TPoint2f): TPoint2i;
  {$ENDIF}

    // Returns vector with X and Y swapped.
    function Swap: TPoint2f; inline;

    // Tests whether both X and Y are nearly zero.
    function Empty: Boolean; inline;

    // Returns the length of current 2D vector.
    function Length: VectorFloat; inline;

    // Calculates distance between current and given points.
    function Distance(const Point: TPoint2f): VectorFloat; inline;

    // Returns an angle (in radians) at which current is pointing at.
    function Angle: VectorFloat; inline;

    { Calculates a dot product between current and the specified 2D vectors. The dot product is an indirect
      measure of the angle between two vectors. }
    function Dot(const Point: TPoint2f): VectorFloat; inline;

    // Calculates a cross product between current and the specified 2D vectors, or analog of thereof.
    function Cross(const Point: TPoint2f): VectorFloat; inline;

    // Normalizes current vector to unity length. If the vector is of zero length, it will remain unchanged.
    function Normalize: TPoint2f;

    { Interpolates between current and destination 2D vectors.
        @param(Point The destination vector to be used in the interpolation)
        @param(Theta The mixture of the two vectors with the a range of [0..1].) }
    function Lerp(const Point: TPoint2f; const Theta: VectorFloat): TPoint2f;

    // Tests whether current point is inside the triangle specified by given three vertices.
    function InsideTriangle(const Vertex1, Vertex2, Vertex3: TPoint2f): Boolean;

    // Converts @link(TPoint2f) to @link(TPoint2i) by using floating-point rounding.
    function ToInt: TPoint2i;
  end;

const
  // Predefined constant, where X and Y are zero.
  ZeroPoint2f: TPoint2f = (X: 0.0; Y: 0.0);

  { Predefined constant, where X and Y are one. }
  UnityPoint2f: TPoint2f = (X: 1.0; Y: 1.0);

  { Predefined constant, where X = 1 and Y = 0. }
  AxisXPoint2f: TPoint2f = (X: 1.0; Y: 0.0);

  { Predefined constant, where X = 0 and Y = 1. }
  AxisYPoint2f: TPoint2f = (X: 0.0; Y: 1.0);

{ Creates a @link(TPoint2f) record using the specified coordinates. }
function Point2f(const X, Y: VectorFloat): TPoint2f; inline;

{$ENDREGION}
{$REGION 'TVector3i declarations'}

type
  // Pointer to @link(TVector3i).
  PVector3i = ^TVector3i;

  // 3D integer vector.
  TVector3i = record
    // The coordinate in 3D space.
    X, Y, Z: VectorInt;

    { @exclude } class operator Add(const Vector1, Vector2: TVector3i): TVector3i;
    { @exclude } class operator Subtract(const Vector1, Vector2: TVector3i): TVector3i;
    { @exclude } class operator Multiply(const Vector1, Vector2: TVector3i): TVector3i;
    { @exclude } class operator Divide(const Vector1, Vector2: TVector3i): TVector3i;
    { @exclude } class operator Negative(const Vector: TVector3i): TVector3i;
    { @exclude } class operator Multiply(const Vector: TVector3i; const Theta: VectorInt): TVector3i;
    { @exclude } class operator Multiply(const Theta: VectorInt; const Vector: TVector3i): TVector3i;
    { @exclude } class operator Divide(const Vector: TVector3i; const Theta: VectorInt): TVector3i;
    { @exclude } class operator Equal(const Vector1, Vector2: TVector3i): Boolean;
    { @exclude } class operator NotEqual(const Vector1, Vector2: TVector3i): Boolean; inline;

    // Returns length of current vector.
    function Length: VectorFloat;

    { Calculates a dot product between current and the specified 3D vector. The dot product is an indirect
      measure of angle between two vectors. }
    function Dot(const Vector: TVector3i): VectorInt; inline;

    { Calculates a cross product between current and the specified 3D vector. The resulting vector is
      perpendicular to both vectors and normal to the plane containing them. }
    function Cross(const Vector: TVector3i): TVector3i;

    // Calculates angle between current and the specified 3D vector. The returned value has range of [0..Pi].
    function Angle(const Vector: TVector3i): VectorFloat;

    { Interpolates between current and destination integer 3D vectors.
        @param(Vector The destination vector to be used in the interpolation)
        @param(Theta The mixture of the two vectors with the a range of [0..1].) }
    function Lerp(const Vector: TVector3i; const Theta: VectorFloat): TVector3i;

    // Returns (x, y) portion of 3D vector as @link(TPoint2i).
    function GetXY: TPoint2i; inline;
  end;

const
  // Predefined constant, where X, Y and Z are zero.
  ZeroVector3i: TVector3i = (X: 0; Y: 0; Z: 0);

  // Predefined constant, where X, Y and Z are one.
  UnityVector3i: TVector3i = (X: 1; Y: 1; Z: 1);

  // Predefined constant, where X = 1, Y = 0 and Z = 0.
  AxisXVector3i: TVector3i = (X: 1; Y: 0; Z: 0);

  // Predefined constant, where X = 0, Y = 1 and Z = 0.
  AxisYVector3i: TVector3i = (X: 0; Y: 1; Z: 0);

  // Predefined constant, where X = 0, Y = 0 and Z = 1.
  AxisZVector3i: TVector3i = (X: 0; Y: 0; Z: 1);

// Creates @link(TVector3i) record using the specified coordinates.
function Vector3i(const X, Y, Z: VectorInt): TVector3i; overload; inline;

// Creates @link(TVector3i) record using 2D vector and specified Z coordinate.
function Vector3i(const Point: TPoint2i; const Z: VectorInt = 0): TVector3i; overload; inline;

{$ENDREGION}
{$REGION 'TVector3f declarations'}

type
  // Pointer to @link(TVector3fArray).
  PVector3fArray = ^TVector3fArray;

  // Representation of @link(TVector3f) as an array.
  TVector3fArray = array[0..2] of VectorFloat;

  // Pointer to @link(TVector3f).
  PVector3f = ^TVector3f;

  // 3D floating-point vector.
  TVector3f = record
    // The coordinate in 3D space.
    X, Y, Z: VectorFloat;

    { @exclude } class operator Add(const Vector1, Vector2: TVector3f): TVector3f; inline;
    { @exclude } class operator Subtract(const Vector1, Vector2: TVector3f): TVector3f; inline;
    { @exclude } class operator Multiply(const Vector1, Vector2: TVector3f): TVector3f; inline;
    { @exclude } class operator Divide(const Vector1, Vector2: TVector3f): TVector3f; inline;
    { @exclude } class operator Negative(const Vector: TVector3f): TVector3f; inline;
    { @exclude } class operator Multiply(const Vector: TVector3f;
      const Theta: VectorFloat): TVector3f; inline;
    { @exclude } class operator Multiply(const Theta: VectorFloat;
      const Vector: TVector3f): TVector3f; inline;
    { @exclude } class operator Divide(const Vector: TVector3f; const Theta: VectorFloat): TVector3f; inline;
    { @exclude } class operator Implicit(const Vector: TVector3i): TVector3f; inline;
    { @exclude } class operator Equal(const Vector1, Vector2: TVector3f): Boolean;
    { @exclude } class operator NotEqual(const Vector1, Vector2: TVector3f): Boolean; inline;

    // Tests whether X, Y and Z are nearly zero.
    function Empty: Boolean;

    // Returns length of current vector.
    function Length: VectorFloat;

    // Calculates distance between current and given vectors.
    function Distance(const Vector: TVector3f): VectorFloat;

    { Calculates a dot product between current and the specified 3D vector. The dot product is an indirect
      measure of angle between two vectors. }
    function Dot(const Vector: TVector3f): VectorFloat; inline;

    { Calculates a cross product between current and the specified 3D vector. The resulting vector is
      perpendicular to both vectors and normal to the plane containing them. }
    function Cross(const Vector: TVector3f): TVector3f;

    // Calculates angle between current and the specified 3D vector. The returned value has range of [0..Pi].
    function Angle(const Vector: TVector3f): VectorFloat;

    // Normalizes current vector to unity length. If current length is zero, the same vector is returned.
    function Normalize: TVector3f;

    // Calculates a portion of current vector that is parallel to the direction vector.
    function Parallel(const Direction: TVector3f): TVector3f;

    // Calculates a portion of current vector that is perpendicular to the direction vector.
    function Perpendicular(const Direction: TVector3f): TVector3f;

    // Calculates 3D vector that is a reflection of current vector from surface given by specified normal.
    function Reflect(const Normal: TVector3f): TVector3f;

    { Interpolates between current and destination 3D vectors.
        @param(Vector The destination vector to be used in the interpolation)
        @param(Theta The mixture of the two vectors with the a range of [0..1].) }
    function Lerp(const Vector: TVector3f; const Theta: VectorFloat): TVector3f;

    // Converts @link(TVector3f) to @link(TVector3i) by using floating-point rounding.
    function ToInt: TVector3i;

    // Returns (x, y) portion of 3D vector as @link(TPoint2f).
    function GetXY: TPoint2f; inline;
  end;

const
  // Predefined constant, where X, Y and Z are zero.
  ZeroVector3f: TVector3f = (X: 0.0; Y: 0.0; Z: 0.0);

  // Predefined constant, where X, Y and Z are one.
  UnityVector3f: TVector3f = (X: 1.0; Y: 1.0; Z: 1.0);

  // Predefined constant, where X = 1, Y = 0 and Z = 0.
  AxisXVector3f: TVector3f = (X: 1.0; Y: 0.0; Z: 0.0);

  // Predefined constant, where X = 0, Y = 1 and Z = 0.
  AxisYVector3f: TVector3f = (X: 0.0; Y: 1.0; Z: 0.0);

  // Predefined constant, where X = 0, Y = 0 and Z = 1.
  AxisZVector3f: TVector3f = (X: 0.0; Y: 0.0; Z: 1.0);

// Creates @link(TVector3f) record using the specified coordinates.
function Vector3f(const X, Y, Z: VectorFloat): TVector3f; overload; inline;

// Creates @link(TVector3f) record using 2D vector and specified Z coordinate.
function Vector3f(const Point: TPoint2f; const Z: VectorFloat = 0.0): TVector3f; overload; inline;

{$ENDREGION}
{$REGION 'TVector4f declarations'}

type
  // Pointer to @link(TVector4f).
  PVector4f = ^TVector4f;

  // 4D (3D + w) floating-point vector.
  TVector4f = record
    // The coordinate in 3D space.
    X, Y, Z: VectorFloat;

    { Homogeneous transform coordinate, mostly used for perspective projection.
      Typically, this component is set to 1.0. }
    W: VectorFloat;

    { @exclude } class operator Add(const Vector1, Vector2: TVector4f): TVector4f;
    { @exclude } class operator Subtract(const Vector1, Vector2: TVector4f): TVector4f;
    { @exclude } class operator Multiply(const Vector1, Vector2: TVector4f): TVector4f;
    { @exclude } class operator Divide(const Vector1, Vector2: TVector4f): TVector4f;
    { @exclude } class operator Negative(const Vector: TVector4f): TVector4f; inline;
    { @exclude } class operator Multiply(const Vector: TVector4f;
      const Theta: VectorFloat): TVector4f; inline;
    { @exclude } class operator Multiply(const Theta: VectorFloat;
      const Vector: TVector4f): TVector4f; inline;
    { @exclude } class operator Divide(const Vector: TVector4f; const Theta: VectorFloat): TVector4f; inline;
    { @exclude } class operator Equal(const Vector1, Vector2: TVector4f): Boolean;
    { @exclude } class operator NotEqual(const Vector1, Vector2: TVector4f): Boolean; inline;

    { Interpolates between current and the specified 4D vector.
        @param(Vector The destination vector to be used in the interpolation)
        @param(Theta The mixture of the two vectors with the a range of [0..1].) }
    function Lerp(const Vector: TVector4f; const Theta: VectorFloat): TVector4f;

    { Returns (X, Y, Z) portion of 4D vector. }
    function GetXYZ: TVector3f; inline;

    { Returns (X, Y, Z) portion of current 4D (3D + W) vector, projecting to W = 1 whenever necessary. }
    function ProjectToXYZ: TVector3f;
  end;

const
  // Predefined constant, where all components are zero.
  ZeroVector4: TVector4f = (X: 0.0; Y: 0.0; Z: 0.0; W: 0.0);

  // Predefined constant, where X, Y and Z are zero, while W = 1.
  ZeroVector4H: TVector4f = (X: 0.0; Y: 0.0; Z: 0.0; W: 1.0);

  // Predefined constant, where all components are one.
  UnityVector4: TVector4f = (X: 1.0; Y: 1.0; Z: 1.0; W: 1.0);

  // Predefined constant, where X = 1, Y = 0, Z = 0 and W = 1.
  AxisXVector4H: TVector4f = (X: 1.0; Y: 0.0; Z: 0.0; W: 1.0);

  // Predefined constant, where X = 0, Y = 1, Z = 0 and W = 1.
  AxisYVector4H: TVector4f = (X: 0.0; Y: 1.0; Z: 0.0; W: 1.0);

  // Predefined constant, where X = 0, Y = 0, Z = 1 and W = 1.
  AxisZVector4H: TVector4f = (X: 0.0; Y: 0.0; Z: 1.0; W: 1.0);

  // Predefined constant, where X = 1, Y = 0, Z = 0 and W = 0.
  AxisXVector4: TVector4f = (X: 1.0; Y: 0.0; Z: 0.0; W: 0.0);

  // Predefined constant, where X = 0, Y = 1, Z = 0 and W = 0.
  AxisYVector4: TVector4f = (X: 0.0; Y: 1.0; Z: 0.0; W: 0.0);

  // Predefined constant, where X = 0, Y = 0, Z = 1 and W = 0.
  AxisZVector4: TVector4f = (X: 0.0; Y: 0.0; Z: 1.0; W: 0.0);

  // Predefined constant, where X = 0, Y = 0, Z = 0 and W = 1.
  AxisWVector4: TVector4f = (X: 0.0; Y: 0.0; Z: 0.0; W: 1.0);

// Creates a @link(TVector4f) record using the specified X, Y, Z and W coordinates.
function Vector4f(const X, Y, Z: VectorFloat; const W: VectorFloat = 1.0): TVector4f; overload; inline;

// Creates a @link(TVector4f) record using the specified 3D vector and W coordinate.
function Vector4f(const Vector: TVector3f; const W: VectorFloat = 1.0): TVector4f; overload; inline;

// Creates a @link(TVector4f) record using the specified 2D point, Z and W coordinates.
function Vector4f(const Point: TPoint2f; const Z: VectorFloat = 0.0;
  const W: VectorFloat = 1.0): TVector4f; overload; inline;

{$ENDREGION}
{$REGION 'TMatrix3f declarations'}

type
  // Pointer to @link(TMatrix3f).
  PMatrix3f = ^TMatrix3f;

  // 3x3 transformation matrix.
  TMatrix3f = record
    // Individual matrix values.
    Data: array[0..2, 0..2] of VectorFloat;

    { @exclude } class operator Add(const Matrix1, Matrix2: TMatrix3f): TMatrix3f;
    { @exclude } class operator Subtract(const Matrix1, Matrix2: TMatrix3f): TMatrix3f;
    { @exclude } class operator Multiply(const Matrix1, Matrix2: TMatrix3f): TMatrix3f;
    { @exclude } class operator Multiply(const Matrix: TMatrix3f; const Theta: VectorFloat): TMatrix3f;
    { @exclude } class operator Multiply(const Theta: VectorFloat; const Matrix: TMatrix3f): TMatrix3f; inline;
    { @exclude } class operator Divide(const Matrix: TMatrix3f; const Theta: VectorFloat): TMatrix3f;
    { @exclude } class operator Multiply(const Point: TPoint2f; const Matrix: TMatrix3f): TPoint2f;

    // Calculates determinant of current matrix.
    function Determinant: VectorFloat;

    // Returns current matrix transposed. That is, rows become columns and vice-versa.
    function Transpose: TMatrix3f;

    // Calculates adjoint matrix for the current matrix.
    function Adjoint: TMatrix3f;

    // Calculates inverse matrix of the current matrix.
    function Inverse: TMatrix3f;

    // Creates 2D translation matrix with specified offset.
    class function Translate(const Offset: TPoint2f): TMatrix3f; overload; static;

    // Creates 2D translation matrix with specified coordinates.
    class function Translate(const X, Y: VectorFloat): TMatrix3f; overload; static; inline;

    // Creates 2D rotation matrix with specified angle (in radiants).
    class function Rotate(const Angle: VectorFLoat): TMatrix3f; static;

    // Creates 2D scaling matrix with specified coefficients.
    class function Scale(const Scale: TPoint2f): TMatrix3f; overload; static;

    // Creates 2D scaling matrix with specified individual coefficients.
    class function Scale(const X, Y: VectorFloat): TMatrix3f; overload; static; inline;

    // Creates 2D scaling matrix with with X and Y equal to the specified coefficient.
    class function Scale(const Scale: VectorFloat): TMatrix3f; overload; static; inline;
  end;

const
  // Predefined constant with values corresponding to @italic(Identity) matrix.
  IdentityMatrix3f: TMatrix3f = (Data: ((1.0, 0.0, 0.0), (0.0, 1.0, 0.0), (0.0, 0.0, 1.0)));

  // Predefined constant, where all matrix values are zero.
  ZeroMatrix3f: TMatrix3f = (Data: ((0.0, 0.0, 0.0), (0.0, 0.0, 0.0), (0.0, 0.0, 0.0)));

{$ENDREGION}
{$REGION 'TMatrix4f declarations'}

type
  // Pointer to @link(TMatrix4f).
  PMatrix4f = ^TMatrix4f;

  // 4x4 transformation matrix.
  TMatrix4f = record
  private
    class function DetSub3(const A1, A2, A3, B1, B2, B3, C1, C2, C3: VectorFloat): VectorFloat; static;
  public
    // Individual matrix values.
    Data: array[0..3, 0..3] of VectorFloat;

    { @exclude } class operator Add(const Matrix1, Matrix2: TMatrix4f): TMatrix4f;
    { @exclude } class operator Subtract(const Matrix1, Matrix2: TMatrix4f): TMatrix4f;
    { @exclude } class operator Multiply(const Matrix1, Matrix2: TMatrix4f): TMatrix4f;
    { @exclude } class operator Multiply(const Matrix: TMatrix4f; const Theta: VectorFloat): TMatrix4f;
    { @exclude } class operator Multiply(const Theta: VectorFloat;
      const Matrix: TMatrix4f): TMatrix4f; inline;
    { @exclude } class operator Divide(const Matrix: TMatrix4f; const Theta: VectorFloat): TMatrix4f;
    { @exclude } class operator Multiply(const Vector: TVector3f; const Matrix: TMatrix4f): TVector3f;
    { @exclude } class operator Multiply(const Vector: TVector4f; const Matrix: TMatrix4f): TVector4f;

    // Calculates determinant of current matrix.
    function Determinant: VectorFloat;

    { Assuming that the current matrix is a view matrix, this method calculates the 3D position where
      the camera (or "eye") is supposedly located. }
    function EyePos: TVector3f;

    { Assuming that the specified matrix is a world matrix, this method calculates the 3D position where
      the object (or "world") is supposedly located. }
    function WorldPos: TVector3f;

    // Transposes current matrix. That is, rows become columns and vice-versa.
    function Transpose: TMatrix4f;

    // Calculates adjoint values for the current matrix.
    function Adjoint: TMatrix4f;

    { Calculates inverse of current matrix. In other words, the new matrix will "undo" any transformations
      that the given matrix would have made. }
    function Inverse: TMatrix4f;

    { Multiplies the given 3D vector as if it was 4D (3D + 1) vector by current matrix, then divides each of
      the resulting components by the resulting value of W (which is then discarded), and converts absolute
      coordinates to screen pixels by using the given target size. }
    function Project(const Vector: TVector3f; const TargetSize: TPoint2f): TPoint2f;

    // Creates 3D translation matrix with specified offset.
    class function Translate(const Offset: TVector3f): TMatrix4f; overload; static;

    // Creates 3D translation matrix with specified X, Y and (optional) Z coordinates.
    class function Translate(const X, Y: VectorFloat;
      const Z: VectorFloat = 0.0): TMatrix4f; overload; static; inline;

    // Creates 3D rotation matrix around X axis with the specified angle.
    class function RotateX(const Angle: VectorFloat): TMatrix4f; static;

    // Creates 3D rotation matrix around Y axis with the specified angle.
    class function RotateY(const Angle: VectorFloat): TMatrix4f; static;

    // Creates 3D rotation matrix around Z axis with the specified angle.
    class function RotateZ(const Angle: VectorFloat): TMatrix4f; static;

    // Creates 3D rotation matrix around specified axis and angle (in radiants).
    class function Rotate(const Axis: TVector3f; const Angle: VectorFloat): TMatrix4f; static;

    // Creates 3D translation matrix with specified coefficients.
    class function Scale(const Scale: TVector3f): TMatrix4f; overload; static;

    // Creates 3D translation matrix with specified X, Y and (optional) Z coefficients.
    class function Scale(const X, Y: VectorFloat;
      const Z: VectorFloat = 0.0): TMatrix4f; overload; static; inline;

    // Creates 3D translation matrix with X, Y and Z equal to the specified coefficient.
    class function Scale(const Scale: VectorFloat): TMatrix4f; overload; static; inline;

    { Creates 3D rotation matrix based on parameters similar to flight dynamics, specifically heading, pitch
      and Bank. Each of the components is specified individually. }
    class function HeadingPitchBank(const Heading, Pitch, Bank: VectorFloat): TMatrix4f; overload; static;

    { Creates 3D rotation matrix based on parameters similar to flight dynamics, specifically heading, pitch
      and Bank. The components are taken from the specified vector with Y corresponding to heading, X to
      pitch and Z to bank. }
    class function HeadingPitchBank(const Vector: TVector3f): TMatrix4f; overload; static;

    { Creates 3D rotation matrix based on parameters similar to flight dynamics, specifically yaw, pitch and
      roll. Each of the components is specified individually. }
    class function YawPitchRoll(const Yaw, Pitch, Roll: VectorFloat): TMatrix4f; overload; static;

    { Creates 3D rotation matrix based on parameters similar to flight dynamics, specifically yaw, pitch and
      roll. The components are taken from the specified vector with Y corresponding to yaw, X to pitch and Z
      to roll. }
    class function YawPitchRoll(const Vector: TVector3f): TMatrix4f; overload; static;

    // Creates a reflection matrix specified by the given vector defining the orientation of the reflection.
    class function Reflect(const Axis: TVector3f): TMatrix4f; static;

    { Creates a view matrix that is defined by the camera's position, its target and vertical
      axis or "ceiling". }
    class function LookAt(const Origin, Target, Ceiling: TVector3f): TMatrix4f; static;

    { Creates perspective projection matrix defined by a field of view on Y axis. This is a common way for
      typical 3D applications. In 3D shooters special care is to be taken because on wide-screen monitors the
      visible area will be bigger when using this constructor. The parameters that define the viewed range
      are important for defining the precision of the depth transformation or a depth-buffer.
        @param(FieldOfView The camera's field of view in radians. For example Pi/4.)
        @param(AspectRatio The screen's aspect ratio. Can be calculated as y/x.)
        @param(MinRange The closest range at which the scene will be viewed.)
        @param(MaxRange The farthest range at which the scene will be viewed.)
        @param(NegativeDepthRange Whether to use OpenGL [-1, 1] depth range instead of [0, 1].) }
    class function PerspectiveFOVY(const FieldOfView, AspectRatio, MinRange,
      MaxRange: VectorFloat; const NegativeDepthRange: Boolean = False): TMatrix4f; static;

    { Creates perspective projection matrix defined by a field of view on X axis. In 3D shooters the field of
      view needs to be adjusted to allow more visible area on wide-screen monitors. The parameters that
      define the viewed range are important for defining the precision of the depth transformation or a
      depth-buffer.
        @param(FieldOfView The camera's field of view in radians. For example Pi/4.)
        @param(AspectRatio The screen's aspect ratio. Can be calculated as x/y.)
        @param(MinRange The closest range at which the scene will be viewed.)
        @param(MaxRange The farthest range at which the scene will be viewed.)
        @param(NegativeDepthRange Whether to use OpenGL [-1, 1] depth range instead of [0, 1].) }
    class function PerspectiveFOVX(const FieldOfView, AspectRatio, MinRange,
      MaxRange: VectorFloat; const NegativeDepthRange: Boolean = False): TMatrix4f; static;

    // Creates perspective projection matrix defined by the viewing volume in 3D space.
    class function PerspectiveVOL(const Width, Height, MinRange, MaxRange: VectorFloat): TMatrix4f; static;

    // Creates perspective projection matrix defined by the individual axis's boundaries.
    class function PerspectiveBDS(const Left, Right, Top, Bottom, MinRange,
      MaxRange: VectorFloat): TMatrix4f; static;

    // Creates orthogonal projection matrix defined by the viewing volume in 3D space.
    class function OrthogonalVOL(const Width, Height, MinRange, MaxRange: VectorFloat): TMatrix4f; static;

    // Creates orthogonal projection matrix defined by the individual axis's boundaries.
    class function OrthogonalBDS(const Left, Right, Top, Bottom, MinRange,
      MaxRange: VectorFloat): TMatrix4f; static;
  end;

const
  // Predefined constant with values corresponding to @italic(Identity) matrix.
  IdentityMatrix4f: TMatrix4f = (Data: ((1.0, 0.0, 0.0, 0.0), (0.0, 1.0, 0.0, 0.0), (0.0, 0.0, 1.0, 0.0),
    (0.0, 0.0, 0.0, 1.0)));

  // Predefined constant, where all matrix values are zero.
  ZeroMatrix4f: TMatrix4f = (Data: ((0.0, 0.0, 0.0, 0.0), (0.0, 0.0, 0.0, 0.0), (0.0, 0.0, 0.0, 0.0),
    (0.0, 0.0, 0.0, 0.0)));

{$ENDREGION}
{$REGION 'TQuaternion declarations'}

type
  // Pointer to @link(TQuaternion).
  PQuaternion = ^TQuaternion;

  // 3D quaternion.
  TQuaternion = record
    // Individual quaternion values.
    X, Y, Z, W: VectorFloat;

    { @exclude } class operator Multiply(const Quaternion1, Quaternion2: TQuaternion): TQuaternion;
    { @exclude } class operator Implicit(const Quaternion: TQuaternion): TMatrix4f;
    { @exclude } class operator Explicit(const Matrix: TMatrix4f): TQuaternion;

    // Returns the magnitude of current quaternion.
    function Length: VectorFloat;

    { Normalizes the current quaternion. Note that normally quaternions are always normalized (of course,
      within limits of numerical precision). This function is provided mainly to combat floating point
      "error creep", which occurs after many successive quaternion operations. }
    function Normalize: TQuaternion;

    // Returns rotational angle "theta" that is present in current quaternion.
    function Angle: VectorFloat;

    // Returns rotational axis that is present in current quaternion.
    function Axis: TVector3f;

    { Computes quaternion's conjugate. The resulting quaternion has opposite rotation to the original
      quaternion. }
    function Conjugate: TQuaternion;

    // Computes exponentiation of the given quaternion.
    function Exponentiate(const Exponent: VectorFloat): TQuaternion;

    // Computes the dot product of two given quaternions.
    function Dot(const Quaternion: TQuaternion): VectorFloat;

    { Applies spherical linear interpolation between current and given quaternions.
        @param(Quaternion Destination quaternion to be used in the interpolation)
        @param(Theta The mixture of the two quaternions with range of [0..1].) }
    function Slerp(const Quaternion: TQuaternion; const Theta: VectorFloat): TQuaternion;

    // Creates 3D quaternion containing rotation around X axis with given angle (in radians).
    class function RotateX(const Angle: VectorFloat): TQuaternion; static;

    // Creates 3D quaternion containing rotation around Y axis with given angle (in radians).
    class function RotateY(const Angle: VectorFloat): TQuaternion; static;

    // Creates 3D quaternion containing rotation around Z axis with given angle (in radians).
    class function RotateZ(const Angle: VectorFloat): TQuaternion; static;

    // Creates 3D quaternion containing rotation around an arbitrary axis with given angle (in radians).
    class function Rotate(const Axis: TVector3f; const Angle: VectorFloat): TQuaternion; static;

    { Creates 3D quaternion setup to perform Object-To-Inertial rotation using the angles specified in
      Euler format. }
    class function RotateObjectToIntertial(const Heading, Pitch, Bank: VectorFloat): TQuaternion; static;

    { Creates 3D quaternion setup to perform Inertial-To-Object rotation using the angles specified in
      Euler format. }
    class function RotateInertialToObject(const Heading, Pitch, Bank: VectorFloat): TQuaternion; static;
  end;

const
  // Identity quaternion that can be used to specify an object with no rotation.
  IdentityQuaternion: TQuaternion = (X: 0.0; Y: 0.0; Z: 0.0; W: 1.0);

{$ENDREGION}
{$REGION 'TIntRect declarations'}
type
  // Pointer to @link(TIntRect).
  PIntRect = ^TIntRect;

  // General-purpose integer rectangle type defined by top and left margins, width and height.
  TIntRect = record
  private
    function GetRight: VectorInt; inline;
    procedure SetRight(const Value: VectorInt); inline;
    function GetBottom: VectorInt; inline;
    procedure SetBottom(const Value: VectorInt); inline;
    function GetBottomRight: TPoint2i; inline;
    procedure SetBottomRight(const Value: TPoint2i); inline;
  public
    { @exclude } class operator Equal(const Rect1, Rect2: TIntRect): Boolean;
    { @exclude } class operator NotEqual(const Rect1, Rect2: TIntRect): Boolean; inline;

    // Tests whether the rectangle is empty, that is, having width and height of zero or less.
    function Empty: Boolean; inline;

    // Tests whether the given point is inside specified current rectangle.
    function Contains(const Point: TPoint2i): Boolean; overload; inline;

    // Tests whether the given rectangle is contained within current rectangle.
    function Contains(const Rect: TIntRect): Boolean; overload; inline;

    // Tests whether the given rectangle overlaps current one.
    function Overlaps(const Rect: TIntRect): Boolean; inline;

    // Calculates rectangle that results from intersection between current and the given rectangles.
    function Intersect(const Rect: TIntRect): TIntRect; inline;

    // Calculates rectangle that results from union between current and the given rectangles.
    function Union(const Rect: TIntRect): TIntRect; inline;

    // Displaces current rectangle by the given offset.
    function Offset(const Delta: TPoint2i): TIntRect; overload; inline;

    // Displaces current rectangle by each of the given offset values.
    function Offset(const DeltaX, DeltaY: VectorInt): TIntRect; overload; inline;

    // Returns rectangle with left and top decremented, while right and bottom incremented by given offset.
    function Inflate(const Delta: TPoint2i): TIntRect; overload; inline;

    // Returns rectangle with left and top decremented, while right and bottom incremented by given offset.
    function Inflate(const DeltaX, DeltaY: VectorInt): TIntRect; overload;

    // Returns rectangle with left and top decremented, while right and bottom incremented by given offset.
    function Inflate(const Delta: VectorInt): TIntRect; overload; inline;

    { Takes source and destination sizes, source rectangle and destination position, then applies clipping to ensure that
      final rectangle stays within valid boundaries of both source and destination sizes. }
    class function ClipCoords(const SourceSize, DestSize: TPoint2i; var SourceRect: TIntRect;
      var DestPos: TPoint2i): Boolean; static;

    // Right (non-inclusive) margin of the rectangle.
    property Right: VectorInt read GetRight write SetRight;

    // Bottom (non-inclusive) margin of the rectangle.
    property Bottom: VectorInt read GetBottom write SetBottom;

    // Bottom/right (non-inclusive) corner of the rectangle.
    property BottomRight: TPoint2i read GetBottomRight write SetBottomRight;

    case Integer of
      0: (// Left position of the rectangle.
          Left: VectorInt;
          // Top position of the rectangle.
          Top: VectorInt;
          // Width of the rectangle.
          Width: VectorInt;
          // Height of the rectangle.
          Height: VectorInt;);
      1: (// Top/left corner (position) of the rectangle.
          TopLeft: TPoint2i;
          // Size of the rectangle.
          Size: TPoint2i;);
{      2: // Individual values represented as an array.
        (Values: array[0..3] of VectorInt);}
  end;

const
  // Zero (empty) rectangle with integer coordinates.
  ZeroIntRect: TIntRect = (Left: 0; Top: 0; Width: 0; Height: 0);

// Creates rectangle based on top/left position, width and height.
function IntRect(const Left, Top, Width, Height: VectorInt): TIntRect; overload;

// Creates rectangle based on top/left position and size.
function IntRect(const Origin, Size: TPoint2i): TIntRect; overload;

// Creates rectangle based on individual margin bounds.
function IntRectBDS(const Left, Top, Right, Bottom: VectorInt): TIntRect; overload;

// Creates rectangle based on top/left and bottom/right margin bounds.
function IntRectBDS(const TopLeft, BottomRight: TPoint2i): TIntRect; overload;

{$ENDREGION}
{$REGION 'TFloatRect declarations'}

type
  // Pointer to @link(TFloatRect).
  PFloatRect = ^TFloatRect;

  // General-purpose floating-point rectangle type defined by top and left margins, width and height.
  TFloatRect = record
  private
    function GetRight: VectorFloat; inline;
    procedure SetRight(const Value: VectorFloat); inline;
    function GetBottom: VectorFloat; inline;
    procedure SetBottom(const Value: VectorFloat); inline;
    function GetBottomRight: TPoint2f; inline;
    procedure SetBottomRight(const Value: TPoint2f); inline;
  public
    { @exclude } class operator Implicit(const ARect: TIntRect): TFloatRect; inline;
    { @exclude } class operator Equal(const ARect1, ARect2: TFloatRect): Boolean;
    { @exclude } class operator NotEqual(const ARect1, ARect2: TFloatRect): Boolean; inline;
    { @exclude } class operator Multiply(const ARect: TFloatRect; const AScale: VectorFloat): TFloatRect;

    // Tests whether the rectangle is empty, that is, having width and height of zero or less.
    function Empty: Boolean;

    // Tests whether the given point is inside specified current rectangle.
    function Contains(const Point: TPoint2f): Boolean; overload;

    // Tests whether the given rectangle is contained within current rectangle.
    function Contains(const Rect: TFloatRect): Boolean; overload; inline;

    // Tests whether the given rectangle overlaps current one.
    function Overlaps(const Rect: TFloatRect): Boolean;

    // Calculates rectangle that results from intersection between current and the given rectangles.
    function Intersect(const Rect: TFloatRect): TFloatRect;

    // Calculates rectangle that results from union between current and the given rectangles.
    function Union(const Rect: TFloatRect): TFloatRect;

    // Displaces current rectangle by the given offset.
    function Offset(const Delta: TPoint2f): TFloatRect; overload;

    // Displaces current rectangle by each of the given offset values.
    function Offset(const DeltaX, DeltaY: VectorFloat): TFloatRect; overload; inline;

    // Returns rectangle with left and top decremented, while right and bottom incremented by given offset.
    function Inflate(const Delta: TPoint2f): TFloatRect; overload;

    // Returns rectangle with left and top decremented, while right and bottom incremented by given offset.
    function Inflate(const DeltaX, DeltaY: VectorFloat): TFloatRect; overload; inline;

    // Returns rectangle with left and top decremented, while right and bottom incremented by given offset.
    function Inflate(const Delta: VectorFloat): TFloatRect; overload; inline;

    // Converts floating-point rectangle to integer rectangle that completely encompasses current rectangle.
    function InnerBounds: TIntRect;

    // Converts floating-point rectangle to integer rectangle that is completely contained within.
    function OuterBounds: TIntRect;

    { Takes source and destination sizes, source and destination rectangles, then applies clipping to ensure
      that final rectangle stays within valid boundaries of both source and destination sizes. }
    class function ClipCoords(const SourceSize, DestSize: TPoint2f; var SourceRect,
      DestRect: TFloatRect): Boolean; static;

    // Right (non-inclusive) margin of the rectangle.
    property Right: VectorFloat read GetRight write SetRight;

    // Bottom (non-inclusive) margin of the rectangle.
    property Bottom: VectorFloat read GetBottom write SetBottom;

    // Bottom/right (non-inclusive) corner of the rectangle.
    property BottomRight: TPoint2f read GetBottomRight write SetBottomRight;

    case Integer of
      0: (// Left position of the rectangle.
          Left: VectorFloat;
          // Top position of the rectangle.
          Top: VectorFloat;
          // Width of the rectangle.
          Width: VectorFloat;
          // Height of the rectangle.
          Height: VectorFloat;);
      1: (// Top/left corner (position) of the rectangle.
          TopLeft: TPoint2f;
          // Size of the rectangle.
          Size: TPoint2f;);
{      2: // Individual values represented as an array.
         (Values: array[0..3] of VectorFloat);}
  end;

const
  // Zero (empty) rectangle with floating-point coordinates.
  ZeroFloatRect: TFloatRect = (Left: 0.0; Top: 0.0; Width: 0.0; Height: 0.0);

// Creates rectangle based on top/left position, width and height.
function FloatRect(const Left, Top, Width, Height: VectorFloat): TFloatRect; overload;

// Creates rectangle based on top/left position and size.
function FloatRect(const Origin, Size: TPoint2f): TFloatRect; overload;

// Creates rectangle based on individual margin bounds.
function FloatRectBDS(const Left, Top, Right, Bottom: VectorFloat): TFloatRect; overload;

// Creates rectangle based on top/left and bottom/right margin bounds.
function FloatRectBDS(const TopLeft, BottomRight: TPoint2f): TFloatRect; overload;

{$ENDREGION}
{$REGION 'TQuad declarations'}

type
  // Pointer to @link(TQuad).
  PQuad = ^TQuad;

  { Special floating-point quadrilateral defined by four vertices starting from top/left in clockwise order.
    This is typically used for rendering color filled and textured quads. }
  TQuad = record
    { @exclude } class operator Equal(const Rect1, Rect2: TQuad): Boolean;
    { @exclude } class operator NotEqual(const Rect1, Rect2: TQuad): Boolean; inline;

    { Rescales vertices of the given quadrilateral by provided coefficient, optionally centering them around
      zero origin. }
    function Scale(const Scale: VectorFloat; const Centered: Boolean = True): TQuad;

    { Creates quadrilateral from another quadrilateral but having left vertices exchanged with the right
      ones, effectively mirroring it horizontally. }
    function Mirror: TQuad;

    { Creates quadrilateral from another quadrilateral but having top vertices exchanged with the bottom
      ones, effectively flipping it vertically. }
    function Flip: TQuad;

    // Transforms (multiplies) vertices of given quadrilateral by the specified matrix.
    function Transform(const Matrix: TMatrix3f): TQuad;

    // Displaces vertices of given quadrilateral by the specified offset.
    function Offset(const Delta: TPoint2f): TQuad; overload;

    // Displaces vertices of given quadrilateral by the specified displacement values.
    function Offset(const DeltaX, DeltaY: VectorFloat): TQuad; overload; inline;

    // Tests whether the point is inside current quadrilateral.
    function Contains(const Point: TPoint2f): Boolean;

    { Creates quadrilateral with the specified top left corner and the given dimensions, which are scaled by
      the provided coefficient. }
    class function Scaled(const Left, Top, Width, Height, Scale: VectorFloat;
      const Centered: Boolean = True): TQuad; static;

    { Creates quadrilateral specified by its dimensions. The rectangle is then rotated and scaled around the
      given middle point (assumed to be inside rectangle's dimensions) and placed in center of the specified
      origin. }
    class function Rotated(const RotationOrigin, Size, RotationCenter: TPoint2f; const Angle: VectorFloat;
      const Scale: VectorFloat = 1.0): TQuad; overload; static;

    { Creates quadrilateral specified by its dimensions. The rectangle is then rotated and scaled around its
      center and placed at the specified origin. }
    class function Rotated(const RotationOrigin, Size: TPoint2f; const Angle: VectorFloat;
      const Scale: VectorFloat = 1.0): TQuad; overload; static; inline;

    { Creates quadrilateral specified by top-left corner and size. The rectangle is then rotated and scaled
      around the specified middle point (assumed to be inside rectangle's dimensions) and placed in the
      center of the specified origin. The difference between this method and @link(Rotated) is that the
      rotation does not preserve centering of the rectangle in case where middle point is not actually
      located in the middle. }
    class function RotatedTL(const TopLeft, Size, RotationCenter: TPoint2f; const Angle: VectorFloat;
      const Scale: VectorFloat = 1.0): TQuad; static; inline;

    case Integer of
      0:( // Top/left vertex position.
          TopLeft: TPoint2f;
          // Top/right vertex position.
          TopRight: TPoint2f;
          // Bottom/right vertex position.
          BottomRight: TPoint2f;
          // Bottom/left vertex position.
          BottomLeft: TPoint2f;);
      1: // Quadrilateral vertices represented as an array. }
         (Values: array[0..3] of TPoint2f);
  end;

const
  // Quadrilateral with all vertices set to zero.
  QuadZero: TQuad = (TopLeft: (X: 0.0; Y: 0.0); TopRight: (X: 0.0; Y: 0.0);
    BottomRight: (X: 0.0; Y: 0.0); BottomLeft: (X: 0.0; Y: 0.0));

  // Quadrilateral with vertices set at four corners set between (0, 0) and (1, 1).
  QuadUnity: TQuad = (TopLeft: (X: 0.0; Y: 0.0); TopRight: (X: 1.0; Y: 0.0);
    BottomRight: (X: 1.0; Y: 1.0); BottomLeft: (X: 0.0; Y: 1.0));

// Creates quadrilateral with individually specified vertex coordinates.
function Quad(const TopLeftX, TopLeftY, TopRightX, TopRightY, BottomRightX, BottomRightY, BottomLeftX,
  BottomLeftY: VectorFloat): TQuad; overload;

// Creates quadrilateral with individually specified vertices.
function Quad(const TopLeft, TopRight, BottomRight, BottomLeft: TPoint2f): TQuad; overload;

// Creates quadrilateral rectangle with top/left position, width and height.
function Quad(const Left, Top, Width, Height: VectorFloat): TQuad; overload;

// Creates quadrilateral rectangle from specified floating-point rectangle.
function Quad(const Rect: TFloatRect): TQuad; overload;

// Creates quadrilateral rectangle from specified integer rectangle.
function Quad(const Rect: TIntRect): TQuad; overload;

{$ENDREGION}
{$REGION 'Device Types and Functions'}

type
  // Pointer to @link(TRenderingState).
  PRenderingState = ^TRenderingState;

{$IFDEF DELPHI_LEGACY}
 // Stencil state that is independent for each front and back facing.
 TRenderingStencilState = record
   // Stencil test function.
   Func: TComparisonFunc;

   // Action to take when stencil test fails.
   FailOp: TStencilOp;

   // Action to take when stencil test passes but depth test fails.
   DepthFailOp: TStencilOp;

   { Action to take when both stencil and depth tests pass, or when stencil test passes and either there is no depth
     buffer or depth testing is disabled. }
   DepthPassOp: TStencilOp;
 end;

 // Blending parameters that are specific to a certain component, or a portion of color.
 TRenderingBlend = record
   // Blending factor used for the source component.
   Source: TBlendFactor;

   // Blending factor used for the destination component.
   Dest: TBlendFactor;

   // Blending operation used between source and destination components.
   Op: TBlendOp;
 end;
{$ENDIF}

  // Parameters that define Depth, Stencil, Rasterizer and Blending operations.
  TRenderingState = record
  {$IFNDEF DELPHI_LEGACY}
  public type
    // Stencil state that is independent for each front and back facing.
    TStencilState = record
      // Stencil test function.
      Func: TComparisonFunc;

      // Action to take when stencil test fails.
      FailOp: TStencilOp;

      // Action to take when stencil test passes but depth test fails.
      DepthFailOp: TStencilOp;

      { Action to take when both stencil and depth tests pass, or when stencil test passes and either there is no depth
        buffer or depth testing is disabled. }
      DepthPassOp: TStencilOp;
    end;

    // Blending parameters that are specific to a certain component, or a portion of color.
    TBlend = record
      // Blending factor used for the source component.
      Source: TBlendFactor;

      // Blending factor used for the destination component.
      Dest: TBlendFactor;

      // Blending operation used between source and destination components.
      Op: TBlendOp;
    end;

  public
  {$ENDIF}
    // Combination of state flags that define rendering operation.
    States: Cardinal;

    // How triangle faces should be culled.
    CullFace: TTriangleFace;

    // Depth buffer comparison function.
    DepthFunc: TComparisonFunc;

    // Constant offset added to depth to alleviate z-fighting issues.
    DepthBias: Single;

    // Slope coefficient used for relative depth when calculating depth bias.
    SlopeDepthBias: Single;

    // Value to clamp depth bias, preventing artifacts on very high slopes.
    ClampDepthBias: Single;

    // Reference value for the stencil test.
    StencilRefValue: Byte;

    // Mask that is applied to reference value and stored value after the test.
    StencilRefMask: Byte;

    // Mask that is used to enable or disable writing of individual bits to stencil buffer.
    StencilWriteMask: Byte;

    // Custom alpha-blending constant.
    BlendConstant: TFloatColor;

  {$IFDEF DELPHI_LEGACY}
    // Stencil function and operation that are performed for front-facing triangles.
    StencilFront: TRenderingStencilState;

    // Stencil function and operation that are performed for back-facing triangles.
    StencilBack: TRenderingStencilState;

    // Blending parameters that should be applied to color portion of components.
    BlendColor: TRenderingBlend;

    // Blending parameters that should be applied to alpha portion of components.
    BlendAlpha: TRenderingBlend;
  {$ELSE}
    // Stencil function and operation that are performed for front-facing triangles.
    StencilFront: TStencilState;

    // Stencil function and operation that are performed for back-facing triangles.
    StencilBack: TStencilState;

    // Blending parameters that should be applied to color portion of components.
    BlendColor: TBlend;

    // Blending parameters that should be applied to alpha portion of components.
    BlendAlpha: TBlend;
  {$ENDIF}
  end;

  // Pointer to @link(TDeviceConfiguration).
  PDeviceConfiguration = ^TDeviceConfiguration;

  // Configuration parameters that define how the rendering is performed by device.
  TDeviceConfiguration = record
    // Type of device. Currently should be set to zero.
    DeviceType: Cardinal;

    // Handle of the window that should be associated with the device.
    WindowHandle: TUntypedHandle;

    // Width of window client area in pixels (optional).
    DisplayWidth: Integer;

    // Height of window client area in pixels (optional).
    DisplayHeight: Integer;

    // Pixel format that should be used for the rendering.
    PixelFormat: TPixelFormat;

    // Depth/stencil configuration that should be used for rendering.
    DepthStencil: TPixelFormat;

    // Number of multisamples to be used for antialiasing.
    Multisamples: Integer;

    // Additional device attributes as bit flags.
    Attributes: Cardinal;
  end;

  // Pointer to @link(TDeviceCapabilities).
  PDeviceCapabilities = ^TDeviceCapabilities;

  // Parameters that define the capabilities of a particular device.
  TDeviceCapabilities = record
    // Graphics technology that is being used by the device.
    Technology: TDeviceTechnology;

    { Indicates the version of current technology that is currently being used. The values are specified in
      hexadecimal format. That is, a value of 0x100 indicates version 1.0, while a value of $247 would
      indicate version 2.4.7. The value is used in combination with "technology" field, so if Direct3D
      technology is being used and this function returns $A10, it means that Direct3D 10.1 version. }
    TechVersion: Cardinal;

    { Indicates the feature level version of current technology that is currently being used. The difference
      between this function and "TechVersion" is that the second one indicates type of technology being used
      (for example, Direct3D), while this one indicates the level of features available (for example,
      Direct3D 9.0c). The values here are specified in hexadecimal format; that is, a value of $213 would
      indicate version 2.1.3. }
    TechFeatureVersion: Cardinal;

    // Device behavior that affect rendering codepath.
    Behavior: Cardinal;

    // Legacy feature bits that define older hardware features.
    LegacyBits: Cardinal;

    // Checks whether the given technology feature version is currently supported.
    function VersionCheck(const AMajorVersion, AMinorVersion: Integer): Boolean; inline;
  end;

 const
   // Graphics hardware supports executing fragment shader per each MSAA sample.
   BehaviorPerSampleShading = $00000001;

   { 3D clip space uses negative values for depth in range of [-1, 1] instead of [0, 1].
     This typically occurs on OpenGL (ES) backend that do not support clip control functions. }
   BehaviorDepthClipNegative = $00000002;

   // OpenGL / Intel HD Graphics bug workaround: always unbind buffers before updating their contents.
   BehaviorForceBufferUnbind = $00001000;

   /// Direct3D 11 / Parallels bug workaround: clear depth buffer with value lower than one.
   BehaviorDepthClearBadPrecision = $00002000;

   /// Direct3D 9 without depth texture support.
   LegacyMissingDepthTexture = $00000001;

   /// Direct3D 9 with Nvidia Geforce 6/7 series use RAWZ depth buffer format.
   LegacyDepthFormatRawZ = $00000002;

   /// Direct3D 9 with capability bits set to support different bit-depths in MRT stack.
   LegacyDifferentBitDepthMRT = $00000004;

   // Depth testing should be performed. If this state is not present, depth writing is also disabled.
   StateDepthTest = $0001;

   // Depth values should be written to Depth Buffer.
   StateDepthWrite = $0002;

   { Stencil testing should be performed. If this state is not present, no other stencil operation will
    occur. }
   StateStencilTest = $0004;

   // Geometry should be clipped by the corresponding depth limits.
   StateDepthClip = $0008;

   // Geometry should be clipped by the corresponding scissor rectangles.
   StateScissorClip = $0010;

   // Primitives should be rendered in a wireframe mode.
   StateWireframe = $0020;

   { Triangles having vertices in clockwise order would be culled. If this flag is not set, then triangles
     having vertices in counter-clockwise order would be culled. }
   StateCullClockwise = $0040;

   // Geometry should be rendered with multisampling.
   StateMultisampling = $0080;

   // Standard 3D lines should be rendered with antialiasing.
   StateLineAntialias = $0100;

   // Writing to color buffer should be performed.
   StateColorWrite = $0200;

   // Blending operation should be performed.
   StateBlendEnable = $0400;

   // Alpha channel should be converted to coverage samples.
   StateAlphaToCoverage = $0800;

   // Cube Maps should be sampled from neighbor faces at seams.
   StateCubeMapSeamless = $1000;

   { Enables fragment shader execution for each individual MSAA sample.
     This has only effect on OpenGL (ES) based implementations. }
   StatePerSampleShading = $2000;

 // Converts device attribute set to the appropriate 32-bit enumeration.
 function DeviceAttributes(const Attributes: TDeviceAttributes;
   const LimitedExtensions: Integer = -1): Cardinal;

{$ENDREGION}
{$REGION 'Buffer Types'}

type
  // Pointer to @link(TBufferParameters).
  PBufferParameters = ^TBufferParameters;

  // Parameters that define the characteristics of a particular buffer.
  TBufferParameters = record
    // Type of the data that the buffer will hold.
    DataType: TBufferDataType;

    // How the buffer's data is going to be accessed.
    AccessType: TBufferAccessType;

    // The size of buffer's data in bytes.
    Size: Cardinal;

    // Number of bytes each buffer's logical element occupies.
    Pitch: Cardinal;
  end;

{$ENDREGION}
{$REGION 'Program Types'}

type
  // Pointer to @link(TVertexElement).
  PVertexElement = ^TVertexElement;

  // Structure that describes the layout of a single buffer element.
  TVertexElement = record
    // Name of the component (variable name in GLSL or semantic name in HLSL, null-terminated).
    Name: PAnsiChar;

    // Data format of each value in this element.
    Format: TElementFormat;

    { Number of values used by this element. If @link(Format) is @link(TElementFormat.Undefined), this
      indicates number of bytes the element occupies. }
    Count: Integer;

    { Channel to which the values of this element will be bound to. The most significant bit of this field
      has a special purpose, meaning that the data values are normalized (applies only to integers), so the
      number of channel is stored in first 31 bits. }
    Channel: Cardinal;

    // Offset in bytes relative to origin of the first buffer element to the current one.
    Offset: Cardinal;
  end;

  // Alias to @link(PVertexElement).
  PVertexElementEntry = PVertexElement;

  // Pointer to @link(TProgramElement).
  PProgramElement = ^TProgramElement;

  // Structure that describes a single physical element of shader program.
  TProgramElement = record
    // Name of the element (e.g. variable, buffer or texture name, null-terminated).
    Name: PAnsiChar;

    // Type of shader element.
    Element: TShaderElement;

    // Shader to which this element belongs to.
    Shader: TShaderType;

    // A virtual channel index, which will be associated with the appropriate buffer attached to that channel.
    Channel: Cardinal;

    { Index of physical slot to which the element should be bound to. This can be either an actual binding
      point or a register number. }
    Index: Integer;

    // Size of element in bytes.
    Size: Cardinal;
  end;

  // Alias to @link(PProgramElement);
  PProgramElementEntry = PProgramElement;

  // Pointer to @link(TProgramVariable).
  PProgramVariable = ^TProgramVariable;

  // Structure that describes a single (uniform) variable in a shader program.
  TProgramVariable = record
    // Name of the element (e.g. variable, buffer or texture name, null-terminated).
    Name: PAnsiChar;

    // Index that uniquely identifies this variable.
    Index: Integer;

    // Shader to which this element belongs to.
    Shader: TShaderType;

    // Data format of each value in this variable.
    Format: TElementFormat;

    { Number of values used by this variable. If @link(Format) is @link(TElementFormat.Undefined), this
      indicates number of bytes the element occupies. }
    Count: Integer;

    // Size of variable in bytes.
    Size: Cardinal;

    // Offset in a buffer where variable starts.
    Offset: Cardinal;
  end;

  // Pointer to @link(TProgramParameters).
  PProgramParameters = ^TProgramParameters;

  // Parameters that define the characteristics of a particular program.
  TProgramParameters = record
    // Pointer to a list of elements that describe how vertices are fed into the pipeline.
    VertexElements: PVertexElement;

    // Number of entries in vertex element list.
    VertexElementCount: Cardinal;

    // Pointer to a list of elements that describe certain program specifics.
    ProgramElements: PProgramElement;

    // Number of entries in program element list.
    ProgramElementCount: Cardinal;

    // Pointer to a list of variables that are used throughout the program.
    ProgramVariables: PProgramVariable;

    // Number of entries in program variable list.
    ProgramVariableCount: Cardinal;

    // Pointer to the byte code of vertex shader.
    VertexShader: PAnsiChar;

    // Size of vertex shader byte code in bytes.
    VertexShaderLength: Cardinal;

    // Pointer to the byte code of geometry shader (or @nil, if no geometry shader is used).
    GeometryShader: PAnsiChar;

    // Size of geometry shader byte code in bytes (or zero, if no geometry shader is used)).
    GeometryShaderLength: Cardinal;

    // Pointer to the byte code of fragment shader.
    FragmentShader: PAnsiChar;

    // Size of fragment shader byte code in bytes.
    FragmentShaderLength: Cardinal;
  end;

  // Pointer to @link(TComputeBindTextureFormat).
  PComputeBindTextureFormat = ^TComputeBindTextureFormat;

  // Compute texture parameters and characteristics.
  TComputeBindTextureFormat = record
    // Channel to which the texture is bound to.
    Channel: Cardinal;

    // Number of mipmap level that should be attached.
    MipLevel: Integer;

    // Layer number from a volume or array texture to bound (-1 means an entire texture should be bound).
    Layer: Integer;

    // Type of access that will be used with the texture.
    Access: TComputeTextureAccess;

    // Format of the texture's pixels.
    Format: TPixelFormat;
  end;

const
  // Special bit added to @link(TVertexElement.Channel) indicating that the integer values are normalized.
  ChannelNormalized = $80000000;

{$ENDREGION}
{$REGION 'Mesh Buffer and Model Types'}

type
  // Pointer to @link(TMeshModelParameters).
  PMeshModelParameters = ^TMeshModelParameters;

  // Parameters that define the content of mesh model.
  TMeshModelParameters = record
    // Pointer to a list of elements that describe how vertices are fed into the pipeline.
    VertexElements: PVertexElement;

    // Number of entries in vertex element list.
    VertexElementCount: Cardinal;

    // Number of vertices in the model.
    VertexCount: Cardinal;

    // Optional number of indices in the model.
    IndexCount: Cardinal;

    // Channel which is used to store vertex information.
    Channel: Cardinal;

    // Optional initial data that should be used to fill the model's vertex buffer.
    InitialVertexData: Pointer;

    // Optional initial data that should be used to fill the model's index buffer.
    InitialIndexData: Pointer;
  end;

  // Pointer to @link(TMeshModelInformation).
  PMeshModelInformation = ^TMeshModelInformation;

  // Information regarding important characteristics of a mesh model.
  TMeshModelInformation = record
    // Number of vertices in the mesh model.
    VertexCount: Cardinal;

    // Number of indices in the mesh model.
    IndexCount: Cardinal;

    // Channel which is used to store vertex information.
    Channel: Cardinal;
  end;

  // Pointer to @link(TMeshBufferEntry).
  PMeshBufferEntry = ^TMeshBufferEntry;

  // Calculated values for a single mesh vertex that can be modified by callback.
  TMeshBufferEntry = record
    // Vertex 3D position.
    Position: TVector3f;

    // Vertex 3D normal (normalized).
    Normal: TVector3f;

    // Vertex 3D tangent (normalized).
    Tangent: TVector3f;

    // Vertex 2D texture coordinates.
    TexCoord: TPoint2f;

    // Vertex RGBA color.
    Color: TFloatColor;
  end;

  // Pointer to @link(TMeshBufferInformation).
  PMeshBufferInformation = ^TMeshBufferInformation;

  // Information regarding important characteristics of a mesh buffer.
  TMeshBufferInformation = record
    // Pointer to vertex information in the mesh buffer.
    Vertices: PMeshBufferEntry;

    // Pointer to index information in the mesh buffer.
    Indices: PInteger;

    // Number of vertices in the mesh buffer.
    VertexCount: Integer;

    // Number of indices in the mesh buffer.
    IndexCount: Integer;
  end;

type
  // Mesh voxel rendering function.
  TMeshVoxelVisualizeFunc = procedure(Position, Size: PVector3f; User: Pointer); cdecl;

{$ENDREGION}
{$REGION 'Sampler and Texture Types'}

type
  // Pointer to @link(TSamplerState).
  PSamplerState = ^TSamplerState;

  // Sampler parameters that are associated with a particular texture unit.
  TSamplerState = record
    // Minification texture filtering.
    FilterMin: TTextureFilter;

    // Magnification texture filtering.
    FilterMag: TTextureFilter;

    // Mipmapping texture filtering.
    FilterMip: TTextureFilter;

    // Texture coordinate addressing for S, T and R (U, V and W) coordinates.
    Address: array[0..2] of TTextureAddress;

    // Special color constant to be used for TextureAddress::Border addressing mode.
    BorderColor: TFloatColor;

    // Total number of samples to be used with anisotropic filtering.
    Anisotropy: Integer;

    // Minimum level of detail for texture mipmap range.
    MinLOD: Single;

    // Maximum level of detail for texture mipmap range.
    MaxLOD: Single;

    // Offset from the calculated mipmap level.
    BiasLOD: Single;

    // Comparison function for texture values.
    CompareFunc: TComparisonFunc;

    // Indicates whether comparison should be made against a particular reference value or simply fetched.
    CompareToRef: ByteBool;

    // Creates default sampler state.
    class function Default: TSamplerState; static;

    // Creates typical sampler state for shadow mapping.
    class function ShadowMap: TSamplerState; static;
  end;

{$ENDREGION}
{$REGION 'Raster Surface Types'}

type
  // Pointer to @link(TRasterSurfaceParameters).
  PRasterSurfaceParameters = ^TRasterSurfaceParameters;

  // Parameters that define the characteristics of raster surface.
  TRasterSurfaceParameters = record
  private
    function GetScanline(const AIndex: Integer): Pointer; inline;
    function GetPixelPtr(const AX, AY: Integer): Pointer; inline;
  public
    { Memory reference to top/left corner of pixel data contained by this surface, with horizontal rows
      arranged linearly from top to bottom. If this parameter is not @nil when passed as part of update
      parameters and is not the same as returned by get function, then an update function will also try to
      read the whole surface from this memory location (assuming it has dimensions and format specified in
      this structure) and update the contents of the surface. }
    Bits: Pointer;

    { Number of bytes that each horizontal row of pixels in the surface occupies. This may differ than the
      actual calculated number and may include unused or even protected memory locations, which should be
      considered for all purposes inaccessible and be skipped. }
    Pitch: Cardinal;

    // Number of bytes each pixel occupies.
    BytesPerPixel: Cardinal;

    // Width of the surface in pixels.
    Width: Integer;

    // Height of the surface in pixels.
    Height: Integer;

    // Size of the surface in bytes.
    BufferSize: Cardinal;

    // Pixel format in which surface pixels are stored.
    Format: TPixelFormat;

    { Indicates whether the pixels in the surface have their alpha premultiplied or not. This parameter
      serves merely as a hint as it doesn't affect how the actual data is stored physically in the surface. }
    PremultipliedAlpha: Boolean;

    // Provides pointer to left corner of pixel data at the given scanline index (that is, row number).
    property Scanline[const AIndex: Integer]: Pointer read GetScanline;

    // Provides pointer to the pixel data at the given coordinates.
    property PixelPtr[const AX, AY: Integer]: Pointer read GetPixelPtr;
  end;

{$ENDREGION}
{$REGION 'Texture Types'}

type
  // Pointer to @link(TTextureParameters).
  PTextureParameters = ^TTextureParameters;

  // Texture parameters and characteristics.
  TTextureParameters = record
    // Texture width (in pixels).
    Width: Integer;

    // Texture height (in pixels).
    Height: Integer;

    // Number of texture layers in case of Texture Array or depth in case of 3D textures.
    Layers: Integer;

    // Format of the texture's pixels.
    Format: TPixelFormat;

    // Texture type that defines how it is composed.
    TextureType: TTextureType;

    // Attributes that define the characteristics of the texture.
    Attributes: Cardinal;

    // Number of multisamples used in the render target.
    Multisamples: Integer;

    // Type of depth/stencil buffers used in the render target.
    DepthStencil: TPixelFormat;

    // Returns texture size (in pixels).
    function GetSize: TPoint2i; inline;
  end;

const
  // Attribute that defines special behavior and/or unique characteristics of the texture.

  { Texture has a full mipmap surface chain attached. MipMapping technique can improve visual quality when
    the texture is drawn in different sizes, especially in smaller ones. }
  TextureMipMapping = $01;

  { Texture contains a special type of surface that can be rendered to. In other words, the texture can be
    considered a render target (Direct3D) or render buffer object (OpenGL). }
  TextureDrawable = $02;

  { Texture is meant for frequent write-only access. Dynamic textures cannot ontain mipmap surface chain
    attached and cannot be drawable either. }
  TextureDynamic = $04;

  { Texture has its color components premultiplied by alpha-channel. This implies permanent loss of
    information as the components are multiplied by alpha value and stored (so, for example, pixels with
    alpha value of zero permanently lose all color information), however this can improve visual quality on
    mipmaps with translucent pixels. The parameter is merely a hint for rendering system, it does not change
    the actual pixels - an action that should be performed separately. }
  TexturePremultipliedAlpha = $08;

  // Texture will be used for Compute shader access (Direct3D-only, makes texture UAV-capable).
  TextureCompute = $40;

type
  // One or more texture attribute flags combined together.
  TTextureAttributes = Cardinal;

{$ENDREGION}
{$REGION 'Canvas Types'}

type
  // Pointer to @link(TSignedDistanceField).
  PSignedDistanceField = ^TSignedDistanceField;

  // Signed Distance Field (SDF) parameters.
  TSignedDistanceField = record
    // Type of super-sampling for the field's rendering.
    SuperSampleSDF: TSuperSampleSDF;

    // Distance that was used for generating the field.
    SignedFieldDistance: Single;

    // Color offset in YCH color space for rendering the shape's outline.
    OutlineOffsetSDF: TVector3f;

    // Minimum field distance for the shape's outline.
    OutlineDistanceMinSDF: Single;

    // Maximum field distance for the shape's outline.
    OutlineDistanceMaxSDF: Single;
  end;

  // Array of @link(TPoint2f) structure.
  TPoint2FArray = array of TPoint2f;

  // Array of @link(TIntColor) structure.
  TIntColorArray = array of TIntColor;

  // Pointer to @link(TCanvasSamplerState).
  PCanvasSamplerState = ^TCanvasSamplerState;

  // Sampler parameters that can be easily configured by the canvas.
  TCanvasSamplerState = record
    // Minification texture filtering.
    FilterMin: TTextureFilter;

    // Magnification texture filtering.
    FilterMag: TTextureFilter;

    // Mipmapping texture filtering.
    FilterMip: TTextureFilter;

    // Horizontal texture coordinate addressing.
    AddressU: TTextureAddress;

    // Vertical texture coordinate addressing.
    AddressV: TTextureAddress;

    // Special color constant to be used for TextureAddress::Border addressing mode.
    BorderColor: TIntColor;

    // Helper constructor to instantiate canvas sampler parameters.
  {$IFDEF DELPHI_LEGACY}
    class function Create(const AFilterMin, AFilterMag: TTextureFilter;
      const AFilterMip: TTextureFilter = tfNone; const AAddressU: TTextureAddress = taWrap;
      const AAddressV: TTextureAddress = taWrap;
      const ABorderColor: TIntColor = IntColorBlack): TCanvasSamplerState; static; inline;
  {$ELSE}
    class function Create(const AFilterMin, AFilterMag: TTextureFilter;
      const AFilterMip: TTextureFilter = TTextureFilter.None;
      const AAddressU: TTextureAddress = TTextureAddress.Wrap;
      const AAddressV: TTextureAddress = TTextureAddress.Wrap;
      const ABorderColor: TIntColor = IntColorBlack): TCanvasSamplerState; static; inline;
  {$ENDIF}
  end;

  // Image region modifier flags that can be combined together.
  TImageAttribute = (
    // Flip region vertically.
    Flip,

    // Mirror region horizontally.
    Mirror,

    // Rotate region by 90 degrees clockwise.
    Rotate);

  // Image region modifier attributes.
  TImageAttributes = set of TImageAttribute;

  // Pointer to @link(TImageRegion).
  PImageRegion = ^TImageRegion;

  // An image region defined by its texture number and bounding rectangle on that texture.
  TImageRegion = record
    // Left position of the region.
    Left: Word;

    // Top position of the region.
    Top: Word;

    // Width of the region.
    Width: Word;

    // Height of the region.
    Height: Word;

    // Texture index.
    Index: Word;

    // Helper constructor to instantiate image region.
    class function Create(const ALeft, ATop, AWidth, AHeight: Integer;
      const AIndex: Integer = 0): TImageRegion; overload; static; inline;

    // Helper constructor to instantiate image region.
    class function Create(const ARect: TIntRect;
      const AIndex: Integer = 0): TImageRegion; overload; static; inline;

    // Tests whether the current region is valid.
    function Valid: Boolean;
  end;

{$ENDREGION}
{$REGION 'Text Rendering Types'}

type
  // Pointer to @link(TTextEntryRect).
  PTextEntryRect = ^TTextEntryRect;

  // Individual text entry that will appear as rendered.
  TTextEntryRect = record
    // Starting character position inside the text (for Unicode, first byte).
    Position: SizeInt;

    // Character's rectangle.
    Rectangle: TFloatRect;
  end;

  // Pointer to @link(TFontEffect).
  PFontEffect = ^TFontEffect;

  // Attributes and characteristics that define how text is rendered on font glyphs.
  TFontEffect = record
    { Brightness of the letter's fill in range of [0, 1], where zero means the letter will appear completely
      dark and one means that letter will appear completely white. }
    FillBrightness: Single;

    { Opacity of the letter's fill in range of [0, 1], where zero means the letter will appear completely
      transparent (that is, not appear at all) and one means that the letter will appear completely opaque. }
    FillOpacity: Single;

    // Type of border that will appear around the letters.
    BorderType: TFontBorder;

    // Thickness of the border that will appear (in pixels).
    BorderThickness: Single;

    { Brightness of the letter's border in range of [0, 1], where zero means the border will appear
      completely dark and one means that the border will appear completely white. }
    BorderBrightness: Single;

    { Opacity of the letter's border in range of [0, 1], where zero means the border will appear completely
      transparent (that is, not appear at all) and one means that the border will appear completely opaque. }
    BorderOpacity: Single;

    // How strong the blur should be applied to letter's shadow.
    ShadowSmoothness: Single;

    // The distance between letter and its shadow (in pixels).
    ShadowDistance: TPoint2f;

    { Brightness of the letter's shadow in range of [0, 1], where zero means the shadow will appear
      completely dark and one means that the shadow will appear completely white. }
    ShadowBrightness: Single;

    { Opacity of the letter's shadow in range of [0, 1], where zero means the shadow will appear completely
      transparent (that is, not appear at all) and one means that the shadow will appear completely opaque. }
    ShadowOpacity: Single;

    { Distance in pixels for generation of Signed Distance Field (SDF) font glyphs. The value must be
      positive. If this parameter is not zero, then the font is considered SDF. }
    SignedFieldDistance: Single;

    { @exclude } class operator Equal(const AEffect1, AEffect2: TFontEffect): Boolean;
    { @exclude } class operator NotEqual(const AEffect1, AEffect2: TFontEffect): Boolean; inline;
  end;

  // Pointer to @link(TTextRenderModifiers).
  PTextRenderModifiers = ^TTextRenderModifiers;

  // Modifier attributes that can be applied to the rendered text.
  TTextRenderModifiers = record
    { Global font scale that is applied to the whole rendered text. A default value of zero means that font
      scale will not be modified. Using values other than zero or one would likely result in not
      pixel-perfect text rendering, appearing blurry. However, this can be used for real-time text animations. }
    Scale: VectorFloat;

    { Global spacing that will be added horizontally between text letters. This can be used to expand or
      shrink the text. }
    Interleave: VectorFloat;

    // Global spacing that will be added vertically when drawing multiple lines of text.
    VerticalSpace: VectorFloat;

    // Blending effect to use (if set to @link(TBlendingEffect.Undefined), normal effect will be used).
    Effect: TBlendingEffect;
  end;

  // Pointer to @link(TFontSettings).
  PFontSettings = ^TFontSettings;

  // Parameters that define visual characteristics and effects of the font.
  TFontSettings = record
    // Font family (e.g. "Helvetica").
    Family: array[0..127] of AnsiChar;

    // Physical size of the font.
    Size: Single;

    // Font letter weight.
    Weight: TFontWeight;

    // Font letter stretch.
    Stretch: TFontStretch;

    // Font letter slant.
    Slant: TFontSlant;

    // Additional font attributes.
    Attributes: TFontAttributes;

    // Effects applied to font letters.
    Effect: TFontEffect;

    // Creates a new structure with most parameters set to their default values while pre-setting some of them.
  {$IFDEF DELPHI_LEGACY}
    class function Create(const AFamily: Utf8String; const ASize: Single;
      const AWeight: TFontWeight = fwNormal; const AStretch: TFontStretch = fsNormal;
      const ASlant: TFontSlant = fsNone; const AAttributes: TFontAttributes = []): TFontSettings; static;
  {$ELSE}
    class function Create(const AFamily: Utf8String; const ASize: Single;
      const AWeight: TFontWeight = TFontWeight.Normal; const AStretch: TFontStretch = TFontStretch.Normal;
      const ASlant: TFontSlant = TFontSlant.None;
      const AAttributes: TFontAttributes = []): TFontSettings; static;
  {$ENDIF}

    { @exclude } class operator Equal(const ASettings1, ASettings2: TFontSettings): Boolean;
    { @exclude } class operator NotEqual(const ASettings1, ASettings2: TFontSettings): Boolean; inline;
  end;

  // 32-bit Unicode character type used for text operations.
  TUnicodeChar = Cardinal;

{$ENDREGION}
{$REGION 'Scene Types'}

const
  // 3D scene rendering specialization attributes.

  // Lighting is present in the scene (requires vertex normals to be present).
  SceneLighting = $0001;

  { Texturing is enabled for the scene (requires vertex coordinates to be present and texture bound to
    first channel). }
  SceneTexture = $0002;

  // Vertex color is used in the scene (requires vertex color to be present).
  SceneColor = $0004;

  // Shadows are applied to the scene (requires shadow map to be bound to second channel).
  SceneShadow = $0008;

  // More than one light source is enabled at the same time (requires modelSceneLighting).
  SceneMultipleLights = $0010;

  // Use cubic interpolation when sampling shadow texture to produce higher quality shadows.
  SceneShadowCubic = $0020;

  // Use instancing for rendering multiple objects in the scene.
  SceneInstancing = $0040;

  // Depth buffer is reversed to provide a more uniform precision distribution.
  SceneDepthReversed = $1000;

  // Compose background color and/or depth buffer from another scene.
  SceneCompose = $2000;

type
  // One or more rendering attributes that affect how 3D scene operates.
  TSceneAttributes = Cardinal;

  // Pointer to @link(TShadowParameters).
  PShadowParameters = ^TShadowParameters;

  // Parameters that define how shadows are rendered.
  TShadowParameters = record
    // Primary exponent for ESM and EVSM techniques.
    Exponent1: Single;

    // Secondary exponent for ESM and EVSM techniques.
    Exponent2: Single;

    // Maximum variance delimiter used with VSM and EVSM techniques.
    Variance: Single;

    // Light bleeding sigma for reducing light bleeding artifacts with VSM and EVSM techniques.
    BleedSigma: Single;

    // Bias parameter that is applied to depth derivatives in VSM technique.
    Bias: Single;

    // Strength of the shadows (0: no shadow, 1: maximum strength).
    Strength: Single;

    // Sigma value for shadow map's Gaussian Blur.
    BlurSigma: Single;

    // Number of samples for shadow map's Gaussian Blur.
    BlurSamples: Integer;
  end;

  // Pointer to @link(TSceneLight).
  PSceneLight = ^TSceneLight;

  // Light parameters in 3D scene.
  TSceneLight = record
    // Ambient color (alpha-channel is unused).
    AmbientColor: TFloatColor;

    // Diffuse color (alpha-channel will be multiplied with model's color).
    MaterialColor: TFloatColor;

    // Specular color (alpha-channel is unused).
    ReflectionColor: TFloatColor;

    // Range at which light attenuation starts (use High(Single) to disable).
    AttenuationStart: Single;

    // Range at which light attenuation ends. This must be equal to or higher than @link(AttenuationStart).
    AttenuationEnd: Single;

    // Power coefficient for specular lighting.
    ReflectionPower: Single;

    // Position of light source.
    Position: TVector3f;

    // Intensity of the light.
    Intensity: Single;
  end;

  // Pointer to @link(TVolumeSurfaceParameters).
  PVolumeSurfaceParameters = ^TVolumeSurfaceParameters;

  // Parameters that define the characteristics of a volume surface.
  TVolumeSurfaceParameters = record
    // The physical size of the surface field.
    FieldSize: TVector3f;

    // Dimensions of the surface field when subdivided.
    Dimensions: array[0..2] of Integer;

    // Coefficient used to tessellate the surface field.
    IsoLevel: Single;
  end;

{$ENDREGION}
{$REGION 'GaussianHighlight types'}

type
  // Pointer to @link(TGaussianHighlightParameters).
  PGaussianHighlightParameters = ^TGaussianHighlightParameters;

  // Parameters that define the characteristics of gaussian highlight.
  TGaussianHighlightParameters = record
    // Distance over which the blur is gradually applied.
    DistanceBlur: Single;

    // Blur distribution's sigma.
    Sigma: Single;

    // Blur's fading power.
    Power: Single;

    // Number of samples in blur kernel.
    Samples: Integer;

    // Distance over which the color change is gradually performed.
    DistanceColor: Single;

    // Change in luma around highlight.
    Luma: Single;

    // Change in chroma around highlight.
    Chroma: Single;
  end;

{$ENDREGION}
{$REGION 'Timer Types'}

type
  // Timer callback function.
  TTimerFunc = procedure(TimerClass: TLibraryClassHandle; User: Pointer); cdecl;

{$ENDREGION}
{$REGION 'ObjectCamera Types'}

type
  // Pointer to @link(TObjectCameraConstraints).
  PObjectCameraConstraints = ^TObjectCameraConstraints;

  // Constraints for 3D camera.
  TObjectCameraConstraints = record
    // Minimum position allowed for camera positioning.
    MinPosition: TVector3f;

    // Maximum position allowed for camera positioning.
    MaxPosition: TVector3f;

    // Minimum camera rotation angles.
    MinRotation: TPoint2f;

    // Maximum camera rotation angles.
    MaxRotation: TPoint2f;

    // Minimum distance allowed for camera distance.
    MinDistance: Single;

    // Maximum distance allowed for camera distance.
    MaxDistance: Single;
  end;

  // Pointer to @link(TObjectCameraSensitivity).
  PObjectCameraSensitivity = ^TObjectCameraSensitivity;

  // Movement and rotation sensitivity.
  TObjectCameraSensitivity = record
    // Movement sensitivity.
    Movement: TPoint2f;

    // Rotation sensitivity.
    Rotation: TPoint2f;
  end;

{$ENDREGION}
{$REGION 'Object Model Types'}

type
  // Pointer to @link(TMeshAligns).
  PMeshAligns = ^TMeshAligns;

  // Alignment for all three axes that determine the placement of mesh around its model.
  TMeshAligns = record
    // Alignment for X axis.
    X: TMeshAlign;

    // Alignment for Y axis.
    Y: TMeshAlign;

    // Alignment for Z axis.
    Z: TMeshAlign;

    // Bias value used for volume size adjustment.
    Bias: Single;
  end;

  // Object model comparison function.
  TObjectModelCompareFunc = function(AObject1, AObject2: TLibraryClassHandle; AUser: Pointer): Boolean; cdecl;

{$ENDREGION}
{$REGION 'Application Types'}

type
  // Pointer to @link(TApplicationConfiguration).
  PApplicationConfiguration = ^TApplicationConfiguration;

{$IFDEF DELPHI_LEGACY}
  // Fields specific to Windows platform.
  TApplicationConfigurationWin = record
    // Name of application window class.
    WindowClassName: PAnsiChar;

    // Handle of application's instance.
    InstanceHandle: TUntypedHandle;

    // Handle of application's icon.
    IconHandle: TUntypedHandle;
  end;

  // Fields specific to Linux platform.
  TApplicationConfigurationNx = record
    // Name of application window class.
    ApplicationLink: PAnsiChar;

    // Title of the application icon.
    IconTitle: PAnsiChar;

    // Number of parameters passed to the application.
    ParameterCount: Integer;

    // Parameter strings that were passed to the application.
    ParameterStrings: ^PAnsiChar;
  end;

  // Platform-specific fields that is required for startup.
  TApplicationConfigurationStartup = record
    case Integer of
      0: (Win: TApplicationConfigurationWin);
      1: (Nx: TApplicationConfigurationNx);
  end;
{$ENDIF}

  // Structure that contains all the parameters necessary to create a new application and its window.
  TApplicationConfiguration = record
  {$IFNDEF DELPHI_LEGACY}
  public type
    // Fields specific to Windows platform.
    TWin = record
      // Name of application window class.
      WindowClassName: PAnsiChar;

      // Handle of application's instance.
      InstanceHandle: TUntypedHandle;

      // Handle of application's icon.
      IconHandle: TUntypedHandle;
    end;

    // Fields specific to Linux platform.
    TNx = record
      // Name of application window class.
      ApplicationLink: PAnsiChar;

      // Title of the application icon.
      IconTitle: PAnsiChar;

      // Number of parameters passed to the application.
      ParameterCount: Integer;

      // Parameter strings that were passed to the application.
      ParameterStrings: ^PAnsiChar;
    end;

    // Platform-specific fields that is required for startup.
    TStartup = record
      case Integer of
        0: (Win: TWin);
        1: (Nx: TNx);
    end;
  public
  {$ENDIF}
    // Handle of a device that will be used with the application.
    DeviceClass: TLibraryClassHandle;

    // Title of the application window.
    WindowTitle: PAnsiChar;

    /// Initial size of the application window.
    Size: TPoint2i;

    // Platform-specific fields that is required for startup.
  {$IFDEF DELPHI_LEGACY}
    Startup: TApplicationConfigurationStartup;
  {$ELSE}
    Startup: TStartup;
  {$ENDIF}
  end;

  // Basic application event.
  TEventFunc = procedure(ApplicationClass: TLibraryClassHandle; User: Pointer); cdecl;

  // Basic application event that returns boolean.
  TBoolFunc = function(ApplicationClass: TLibraryClassHandle; User: Pointer): TLibraryBool; cdecl;

  // Application mouse handling event.
  TMouseFunc = procedure(ApplicationClass: TLibraryClassHandle; Event: TMouseEvent; Button: TMouseButton;
    Position: PPoint2i; User: Pointer); cdecl;

  // Application keyboard handling event.
  TKeyboardFunc = procedure(ApplicationClass: TLibraryClassHandle; Event: TKeyEvent; VirtualKey: Integer;
    KeyCode: Word; User: Pointer); cdecl;

  // Custom application message handling event.
  TEventHookFunc = function(ApplicationClass: TLibraryClassHandle; Event: Pointer;
    User: Pointer): TLibraryBool; cdecl;

  // Pointer to @link(TApplicationEvents).
  PApplicationEvents = ^TApplicationEvents;

  { Events that are invoked by the application. Each of these fields is optional, so if a particular event
    is set to @nil, it will not be invoked and default handler will be used. }
  TApplicationEvents = record
    { Event invoked during startup to load application resources. If this function returns  @false,
      application creation will fail. }
    EventCreate: TBoolFunc;

    // Event invoked during application shutdown to release the resources.
    EventDestroy: TEventFunc;

    // Event invoked when application needs to render contents of the window.
    EventRender: TEventFunc;

    // Event invoked when a mouse event has been registered.
    EventMouse: TMouseFunc;

    // Event invoked when a key event has been registered.
    EventKey: TKeyboardFunc;

    // Event invoked every time the application is idle.
    EventIdle: TBoolFunc;

    // Event invoked when application window is resized.
    EventResize: TEventFunc;

    // Event hook invoked to process application events.
    EventHook: TEventHookFunc;
  end;

{$ENDREGION}
{$REGION 'Vector and Model Types'}

type
  // Array of TPathJoint structure.
  TPathJointArray = array of TPathJoint;

  // Custom model object's payload.
  TObjectPayload = Pointer;

{$ENDREGION}
{$REGION 'Global declarations'}

// Tests whether two floating-points are nearly equal.
function NearlyEqual(const AValue1, AValue2: Single; const AEpsilon: Single = VectorEpsilon): Boolean; inline;

// Tests whether a given float is nearly zero.
function NearlyZero(const AValue: Single; const AEpsilon: Single = VectorEpsilon): Boolean; inline;

{ Ensures that the given value stays within specified range limit, clamping it if necessary. }
function Saturate(const Value, MinLimit, MaxLimit: VectorFloat): VectorFloat; inline;
{$IFNDEF PASDOC} overload; {$ENDIF}

{$IF SIZEOF(VectorFloat) > 4} { @exclude }
function Saturate(const Value, MinLimit, MaxLimit: Single): Single; overload; inline;
{$IFEND}

{ Ensures that the given value stays within specified range limit, clamping it if necessary. }
function Saturate(const Value, MinLimit, MaxLimit: VectorInt): VectorInt; inline;
{$IFNDEF PASDOC} overload; {$ENDIF}

{$IF SIZEOF(VectorInt) <> 4} { @exclude }
function Saturate(const Value, MinLimit, MaxLimit: LongInt): LongInt; overload; inline;
{$IFEND}

{ Returns @True if the specified value is a power of two or @False otherwise. }
function IsPowerOfTwo(const Value: VectorInt): Boolean;

{ Returns the least power of two greater or equal to the specified value. }
function CeilPowerOfTwo(const Value: VectorInt): VectorInt;

{ Returns the greatest power of two lesser or equal to the specified value. }
function FloorPowerOfTwo(const Value: VectorInt): VectorInt;

{$ENDREGION}

implementation

uses
{$IFDEF FPC}
  SysUtils,
{$ELSE}
  {$IFNDEF DELPHI_LEGACY}
    System.AnsiStrings,
  {$ELSE}
    SysUtils,
  {$ENDIF}
{$ENDIF}
  PXT.Headers;

{$REGION 'Global Functions'}

const
  TwoPi = Pi * 2.0;
  PiHalf = Pi * 0.5;

function NearlyEqual(const AValue1, AValue2, AEpsilon: Single): Boolean;
begin
  Result := (AValue1 = AValue2) or (Abs(AValue1 - AValue2) <= Max(Abs(AValue1), Abs(AValue2)) * AEpsilon);
end;

function NearlyZero(const AValue, AEpsilon: Single): Boolean;
begin
  Result := Abs(AValue) <= AEpsilon;
end;

function Saturate(const Value, MinLimit, MaxLimit: VectorFloat): VectorFloat;
begin
  Result := Value;

  if Result < MinLimit then
    Result := MinLimit;

  if Result > MaxLimit then
    Result := MaxLimit;
end;

{$IF SIZEOF(VectorFloat) > 4}
function Saturate(const Value, MinLimit, MaxLimit: Single): Single;
begin
  Result := Value;

  if Result < MinLimit then
    Result := MinLimit;

  if Result > MaxLimit then
    Result := MaxLimit;
end;
{$IFEND}

function Saturate(const Value, MinLimit, MaxLimit: VectorInt): VectorInt;
begin
  Result := Value;

  if Result < MinLimit then
    Result := MinLimit;

  if Result > MaxLimit then
    Result := MaxLimit;
end;

{$IF SIZEOF(VectorInt) <> 4}
function Saturate(const Value, MinLimit, MaxLimit: LongInt): LongInt; overload;
begin
  Result := Value;

  if Result < MinLimit then
    Result := MinLimit;

  if Result > MaxLimit then
    Result := MaxLimit;
end;
{$IFEND}

function IsPowerOfTwo(const Value: VectorInt): Boolean;
begin
  Result := (Value >= 1) and ((Value and (Value - 1)) = 0);
end;

function CeilPowerOfTwo(const Value: VectorInt): VectorInt;
begin
  Result := Round(Power(2, Ceil(Log2(Value))))
end;

function FloorPowerOfTwo(const Value: VectorInt): VectorInt;
begin
  Result := Round(Power(2, Floor(Log2(Value))))
end;

function TDeviceCapabilities.VersionCheck(const AMajorVersion, AMinorVersion: Integer): Boolean;
begin
  Result := (Integer(TechFeatureVersion shr 8) > AMajorVersion) or
    ((Integer(TechFeatureVersion shr 8) = AMajorVersion) and
    (Integer((TechFeatureVersion shr 4) and $0F) >= AMinorVersion));
end;

function DeviceAttributes(const Attributes: TDeviceAttributes; const LimitedExtensions: Integer): Cardinal;
begin
  Result := Byte(Attributes);
  if LimitedExtensions <> -1 then
    Result := (Result or $80) or (Cardinal(Min(LimitedExtensions + 1, 4)) shl 29);
end;

class function TSamplerState.Default: TSamplerState;
begin
  FillChar(Result, SizeOf(TSamplerState), 0);

{$IFDEF DELPHI_LEGACY}
  Result.FilterMin := tfNearest;
  Result.FilterMag := tfNearest;
  Result.CompareFunc := cfAlways;
{$ELSE}
  Result.FilterMin := TTextureFilter.Nearest;
  Result.FilterMag := TTextureFilter.Nearest;
  Result.CompareFunc := TComparisonFunc.Always;
{$ENDIF}

  Result.BorderColor := FloatColorWhite;
  Result.Anisotropy := 1;
  Result.MinLOD := -1000.0;
  Result.MaxLOD := 100.0;
end;

class function TSamplerState.ShadowMap: TSamplerState;
begin
{$IFDEF DELPHI_LEGACY}
  Result.FilterMin := tfNearest;
  Result.FilterMag := tfNearest;
  Result.FilterMip := tfNone;
  Result.Address[0] := taBorder;
  Result.Address[1] := taBorder;
  Result.Address[2] := taBorder;
  Result.CompareFunc := cfAlways;
{$ELSE}
  Result.FilterMin := TTextureFilter.Nearest;
  Result.FilterMag := TTextureFilter.Nearest;
  Result.FilterMip := TTextureFilter.None;
  Result.Address[0] := TTextureAddress.Border;
  Result.Address[1] := TTextureAddress.Border;
  Result.Address[2] := TTextureAddress.Border;
  Result.CompareFunc := TComparisonFunc.Always;
{$ENDIF}

  Result.BorderColor := FloatColorWhite;
  Result.Anisotropy := 1;
  Result.MinLOD := -1000.0;
  Result.MaxLOD := 100.0;
  Result.CompareToRef := False;
end;

class function TCanvasSamplerState.Create(const AFilterMin, AFilterMag, AFilterMip: TTextureFilter;
  const AAddressU, AAddressV: TTextureAddress; const ABorderColor: TIntColor): TCanvasSamplerState;
begin
  Result.FilterMin := AFilterMin;
  Result.FilterMag := AFilterMag;
  Result.FilterMip := AFilterMip;
  Result.AddressU := AAddressU;
  Result.AddressV := AAddressV;
  Result.BorderColor := ABorderColor;
end;

class function TImageRegion.Create(const ALeft, ATop, AWidth, AHeight: Integer;
  const AIndex: Integer): TImageRegion;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Width := AWidth;
  Result.Height := AHeight;
  Result.Index := AIndex;
end;

class function TImageRegion.Create(const ARect: TIntRect; const AIndex: Integer): TImageRegion;
begin
  Result.Left := ARect.Left;
  Result.Top := ARect.Top;
  Result.Width := ARect.Width;
  Result.Height := ARect.Height;
  Result.Index := AIndex;
end;

function TImageRegion.Valid: Boolean;
begin
  Result := (Width > 0) and (Height > 0) and (Index <> High(Word));
end;

function TRasterSurfaceParameters.GetScanline(const AIndex: Integer): Pointer;
begin
  Result := Pointer(PtrUInt(Bits) + Pitch * Cardinal(AIndex));
end;

function TRasterSurfaceParameters.GetPixelPtr(const AX, AY: Integer): Pointer;
begin
  Result := Pointer(PtrUInt(Bits) + Pitch * Cardinal(AY) + BytesPerPixel * Cardinal(AX));
end;

function TTextureParameters.GetSize: TPoint2i;
begin
  Result := Point2i(Width, Height);
end;

class operator TFontEffect.Equal(const AEffect1, AEffect2: TFontEffect): Boolean;
begin
  Result :=
    (AEffect1.BorderType = AEffect2.BorderType) and
    NearlyEqual(AEffect1.FillBrightness, AEffect2.FillBrightness) and
    NearlyEqual(AEffect1.FillOpacity, AEffect2.FillOpacity) and
    NearlyEqual(AEffect1.BorderThickness, AEffect2.BorderThickness) and
    NearlyEqual(AEffect1.BorderBrightness, AEffect2.BorderBrightness) and
    NearlyEqual(AEffect1.BorderOpacity, AEffect2.BorderOpacity) and
    NearlyEqual(AEffect1.ShadowSmoothness, AEffect2.ShadowSmoothness) and
    (AEffect1.ShadowDistance = AEffect2.ShadowDistance) and
    NearlyEqual(AEffect1.ShadowBrightness, AEffect2.ShadowBrightness) and
    NearlyEqual(AEffect1.ShadowOpacity, AEffect2.ShadowOpacity) and
    NearlyEqual(AEffect1.SignedFieldDistance, AEffect2.SignedFieldDistance);
end;

class operator TFontEffect.NotEqual(const AEffect1, AEffect2: TFontEffect): Boolean;
begin
  Result := not (AEffect1 = AEffect2);
end;

class function TFontSettings.Create(const AFamily: Utf8String; const ASize: Single;
  const AWeight: TFontWeight; const AStretch: TFontStretch; const ASlant: TFontSlant;
  const AAttributes: TFontAttributes): TFontSettings;
begin
  FillChar(Result, SizeOf(TFontSettings), 0);
  if Length(AFamily) > 0 then
    Move(AFamily[1], Result.Family, Min(Length(AFamily), 128));
  Result.Size := ASize;
  Result.Weight := AWeight;
  Result.Stretch := AStretch;
  Result.Slant := ASlant;
  Result.Attributes := AAttributes;
  Result.Effect.FillBrightness := 1.0;
  Result.Effect.FillOpacity := 1.0;
  Result.Effect.BorderThickness := 1.0;
  Result.Effect.BorderBrightness := 0.25;
  Result.Effect.BorderOpacity := 0.75;
  Result.Effect.ShadowSmoothness := 3.0;
  Result.Effect.ShadowBrightness := 0.15;
  Result.Effect.ShadowDistance := Point2f(2.0, 2.0);
end;

class operator TFontSettings.Equal(const ASettings1, ASettings2: TFontSettings): Boolean;
begin
  Result :=
    (ASettings1.Weight = ASettings2.Weight) and
    (ASettings1.Stretch = ASettings2.Stretch) and
    (ASettings1.Slant = ASettings2.Slant) and
    (ASettings1.Attributes = ASettings2.Attributes) and
    (StrLIComp(ASettings1.Family, ASettings2.Family, SizeOf(ASettings1.Family)) = 0) and
    NearlyEqual(ASettings1.Size, ASettings2.Size) and
    (ASettings1.Effect = ASettings2.Effect);
end;

class operator TFontSettings.NotEqual(const ASettings1, ASettings2: TFontSettings): Boolean;
begin
  Result := not (ASettings1 = ASettings2);
end;

{$ENDREGION}
{$REGION 'TColorPair'}

class operator TColorPair.Implicit(const Color: TIntColor): TColorPair;
begin
  Result.First := Color;
  Result.Second := Color;
end;

class operator TColorPair.Equal(const Color1, Color2: TColorPair): Boolean;
begin
  Result := (Color1.First = Color2.First) and (Color1.Second = Color2.Second);
end;

class operator TColorPair.NotEqual(const Color1, Color2: TColorPair): Boolean;
begin
  Result := not (Color1 = Color2);
end;

function TColorPair.Empty: Boolean;
begin
  Result := (First = 0) and (Second = 0);
end;

function TColorPair.HasGradient: Boolean;
begin
  Result := First <> Second;
end;

function TColorPair.HasAlpha: Boolean;
begin
  Result := ((First shr 24) > 0) or ((Second shr 24) > 0);
end;

function TColorPair.HasTransparency: Boolean;
begin
  Result := (First shr 24 < 255) or (Second shr 24 < 255);
end;

function ColorPair(const First, Second: TIntColor): TColorPair;
begin
  Result.First := First;
  Result.Second := Second;
end;

function ColorPair(const Color: TIntColor): TColorPair;
begin
  Result.First := Color;
  Result.Second := Color;
end;

function ColorPairAlpha(const AColorPair: TColorPair; const AAlpha: Integer): TColorPair;
begin
  if AAlpha < 255 then
    Result := ColorPair(MakeColor(AColorPair.First, AAlpha), MakeColor(AColorPair.Second, AAlpha))
  else
    Result := AColorPair;
end;

{$ENDREGION}
{$REGION 'TColorRect'}

class operator TColorRect.Implicit(const Color: TIntColor): TColorRect;
begin
  Result.TopLeft := Color;
  Result.TopRight := Color;
  Result.BottomRight := Color;
  Result.BottomLeft := Color;
end;

class operator TColorRect.Equal(const Color1, Color2: TColorRect): Boolean;
begin
  Result := (Color1.TopLeft = Color2.TopLeft) and (Color1.TopRight = Color2.TopRight) and
    (Color1.BottomRight = Color2.BottomRight) and (Color1.BottomLeft = Color2.BottomLeft);
end;

class operator TColorRect.NotEqual(const Color1, Color2: TColorRect): Boolean;
begin
  Result := not (Color1 = Color2);
end;

function TColorRect.Empty: Boolean;
begin
  Result := (TopLeft = 0) and (TopRight = 0) and (BottomRight = 0) and (BottomLeft = 0);
end;

function TColorRect.HasGradient: Boolean;
begin
  Result := (TopLeft <> TopRight) or (TopRight <> BottomRight) or (BottomRight <> BottomLeft);
end;

function TColorRect.HasAlpha: Boolean;
begin
  Result := (TopLeft shr 24 > 0) or (TopRight shr 24 > 0) or (BottomRight shr 24 > 0) or (BottomLeft shr 24 > 0);
end;

function TColorRect.HasTransparency: Boolean;
begin
  Result := (TopLeft shr 24 < 255) or (TopRight shr 24 < 255) or (BottomRight shr 24 < 255) or
    (BottomLeft shr 24 < 255);
end;

function ColorRect(const TopLeft, TopRight, BottomRight, BottomLeft: TIntColor): TColorRect;
begin
  Result.TopLeft := TopLeft;
  Result.TopRight := TopRight;
  Result.BottomRight := BottomRight;
  Result.BottomLeft := BottomLeft;
end;

function ColorRect(const Color: TIntColor): TColorRect;
begin
  Result.TopLeft := Color;
  Result.TopRight := Color;
  Result.BottomRight := Color;
  Result.BottomLeft := Color;
end;

function ColorRectH(const Color: TColorPair): TColorRect;
begin
  Result.TopLeft := Color.First;
  Result.TopRight := Color.Second;
  Result.BottomRight := Color.Second;
  Result.BottomLeft := Color.First;
end;

function ColorRectH(const Left, Right: TIntColor): TColorRect;
begin
  Result.TopLeft := Left;
  Result.TopRight := Right;
  Result.BottomRight := Right;
  Result.BottomLeft := Left;
end;

function ColorRectV(const Color: TColorPair): TColorRect;
begin
  Result.TopLeft := Color.First;
  Result.TopRight := Color.First;
  Result.BottomRight := Color.Second;
  Result.BottomLeft := Color.Second;
end;

function ColorRectV(const Top, Bottom: TIntColor): TColorRect;
begin
  Result.TopLeft := Top;
  Result.TopRight := Top;
  Result.BottomRight := Bottom;
  Result.BottomLeft := Bottom;
end;

function ColorRectAlpha(const AColorRect: TColorRect; const AAlpha: Integer): TColorRect;
begin
  if AAlpha < 255 then
    Result := ColorRect(MakeColor(AColorRect.TopLeft, AAlpha), MakeColor(AColorRect.TopRight, AAlpha),
      MakeColor(AColorRect.BottomRight, AAlpha), MakeColor(AColorRect.BottomLeft, AAlpha))
  else
    Result := AColorRect;
end;

{$ENDREGION}
{$REGION 'TFloatColor'}

class operator TFloatColor.Add(const Color1, Color2: TFloatColor): TFloatColor;
begin
  Result.Red := Color1.Red + Color2.Red;
  Result.Green := Color1.Green + Color2.Green;
  Result.Blue := Color1.Blue + Color2.Blue;
  Result.Alpha := Color1.Alpha + Color2.Alpha;
end;

class operator TFloatColor.Subtract(const Color1, Color2: TFloatColor): TFloatColor;
begin
  Result.Red := Color1.Red - Color2.Red;
  Result.Green := Color1.Green - Color2.Green;
  Result.Blue := Color1.Blue - Color2.Blue;
  Result.Alpha := Color1.Alpha - Color2.Alpha;
end;

class operator TFloatColor.Multiply(const Color1, Color2: TFloatColor): TFloatColor;
begin
  Result.Red := Color1.Red * Color2.Red;
  Result.Green := Color1.Green * Color2.Green;
  Result.Blue := Color1.Blue * Color2.Blue;
  Result.Alpha := Color1.Alpha * Color2.Alpha;
end;

class operator TFloatColor.Divide(const Color1, Color2: TFloatColor): TFloatColor;
begin
  Result.Red := Color1.Red / Color2.Red;
  Result.Green := Color1.Green / Color2.Green;
  Result.Blue := Color1.Blue / Color2.Blue;
  Result.Alpha := Color1.Alpha / Color2.Alpha;
end;

class operator TFloatColor.Multiply(const Color: TFloatColor; const Theta: VectorFloat): TFloatColor;
begin
  Result.Red := Color.Red * Theta;
  Result.Green := Color.Green * Theta;
  Result.Blue := Color.Blue * Theta;
  Result.Alpha := Color.Alpha * Theta;
end;

class operator TFloatColor.Multiply(const Theta: VectorFloat; const Color: TFloatColor): TFloatColor;
begin
  Result.Red := Theta * Color.Red;
  Result.Green := Theta * Color.Green;
  Result.Blue := Theta * Color.Blue;
  Result.Alpha := Theta * Color.Alpha;
end;

class operator TFloatColor.Divide(const Color: TFloatColor; const Theta: VectorFloat): TFloatColor;
begin
  Result.Red := Color.Red / Theta;
  Result.Green := Color.Green / Theta;
  Result.Blue := Color.Blue / Theta;
  Result.Alpha := Color.Alpha / Theta;
end;

class operator TFloatColor.Equal(const Color1, Color2: TFloatColor): Boolean;
begin
  Result := NearlyEqual(Color1.Red, Color2.Red) and NearlyEqual(Color1.Green, Color2.Green) and
    NearlyEqual(Color1.Blue, Color2.Blue) and NearlyEqual(Color1.Alpha, Color2.Alpha);
end;

class operator TFloatColor.NotEqual(const Color1, Color2: TFloatColor): Boolean;
begin
  Result := not (Color1 = Color2);
end;

function TFloatColor.Invert: TFloatColor;
begin
  Result.Red := 1.0 - Red;
  Result.Green := 1.0 - Green;
  Result.Blue := 1.0 - Blue;
  Result.Alpha := 1.0 - Alpha;
end;

function TFloatColor.PremultiplyAlpha: TFloatColor;
begin
  Result.Red := Red * Alpha;
  Result.Green := Green * Alpha;
  Result.Blue := Blue * Alpha;
  Result.Alpha := Alpha;
end;

function TFloatColor.UnpremultiplyAlpha: TFloatColor;
begin
  if Alpha > 0.0 then
  begin
    Result.Red := Red / Alpha;
    Result.Green := Green / Alpha;
    Result.Blue := Blue / Alpha;
    Result.Alpha := Alpha;
  end
  else
    Result := Self;
end;

function TFloatColor.Average(const Color: TFloatColor): TFloatColor;
begin
  Result.Red := (Red + Color.Red) * 0.5;
  Result.Green := (Green + Color.Green) * 0.5;
  Result.Blue := (Blue + Color.Blue) * 0.5;
  Result.Alpha := (Alpha + Color.Alpha) * 0.5;
end;

function TFloatColor.Lerp(const Color: TFloatColor; const Alpha: VectorFloat): TFloatColor;
begin
  Result.Red := Red + (Color.Red - Red) * Alpha;
  Result.Green := Green + (Color.Green - Green) * Alpha;
  Result.Blue := Blue + (Color.Blue - Blue) * Alpha;
  Result.Alpha := Self.Alpha + (Color.Alpha - Self.Alpha) * Alpha;
end;

function TFloatColor.Gray: VectorFloat;
begin
  Result := Red * 0.299 + Green * 0.587 + Blue * 0.114;
end;

function TFloatColor.Saturate: TFloatColor;
begin
  Result.Red := PXT.Types.Saturate(Red, 0.0, 1.0);
  Result.Green := PXT.Types.Saturate(Green, 0.0, 1.0);
  Result.Blue := PXT.Types.Saturate(Blue, 0.0, 1.0);
  Result.Alpha := PXT.Types.Saturate(Alpha, 0.0, 1.0);
end;

function TFloatColor.ToInt: TIntColor;
begin
  Result := Cardinal(Round(Red * 255.0)) or (Cardinal(Round(Green * 255.0)) shl 8) or
    (Cardinal(Round(Blue * 255.0)) shl 16) or (Cardinal(Round(Alpha * 255.0)) shl 24);
end;

function FloatColor(const Color: TIntColor): TFloatColor;
begin
  Result.Red := (Color and $FF) / 255.0;
  Result.Green := ((Color shr 8) and $FF) / 255.0;
  Result.Blue := ((Color shr 16) and $FF) / 255.0;
  Result.Alpha := ((Color shr 24) and $FF) / 255.0;
end;

function FloatColor(const Red, Green, Blue, Alpha: VectorFloat): TFloatColor;
begin
  Result.Red := Red;
  Result.Green := Green;
  Result.Blue := Blue;
  Result.Alpha := Alpha;
end;

{$ENDREGION}
{$REGION 'TPoint2i functions'}

class operator TPoint2i.Add(const Point1, Point2f: TPoint2i): TPoint2i;
begin
  Result.X := Point1.X + Point2f.X;
  Result.Y := Point1.Y + Point2f.Y;
end;

class operator TPoint2i.Subtract(const Point1, Point2f: TPoint2i): TPoint2i;
begin
  Result.X := Point1.X - Point2f.X;
  Result.Y := Point1.Y - Point2f.Y;
end;

class operator TPoint2i.Multiply(const Point1, Point2f: TPoint2i): TPoint2i;
begin
  Result.X := Point1.X * Point2f.X;
  Result.Y := Point1.Y * Point2f.Y;
end;

class operator TPoint2i.Divide(const Point1, Point2f: TPoint2i): TPoint2i;
begin
  Result.X := Point1.X div Point2f.X;
  Result.Y := Point1.Y div Point2f.Y;
end;

class operator TPoint2i.Negative(const Point: TPoint2i): TPoint2i;
begin
  Result.X := -Point.X;
  Result.Y := -Point.Y;
end;

class operator TPoint2i.Multiply(const Point: TPoint2i; const Theta: VectorInt): TPoint2i;
begin
  Result.X := Point.X * Theta;
  Result.Y := Point.Y * Theta;
end;

class operator TPoint2i.Multiply(const Theta: VectorInt; const Point: TPoint2i): TPoint2i;
begin
  Result.X := Theta * Point.X;
  Result.Y := Theta * Point.Y;
end;

class operator TPoint2i.Divide(const Point: TPoint2i; const Theta: VectorInt): TPoint2i;
begin
  Result.X := Point.X div Theta;
  Result.Y := Point.Y div Theta;
end;

class operator TPoint2i.Divide(const Point: TPoint2i; const Theta: VectorFloat): TPoint2i;
begin
  Result.X := Round(Point.X / Theta);
  Result.Y := Round(Point.Y / Theta);
end;

class operator TPoint2i.Equal(const Point1, Point2f: TPoint2i): Boolean;
begin
  Result := (Point1.X = Point2f.X) and (Point1.Y = Point2f.Y);
end;

class operator TPoint2i.NotEqual(const Point1, Point2f: TPoint2i): Boolean;
begin
  Result := (Point1.X <> Point2f.X) or (Point1.Y <> Point2f.Y);
end;

function TPoint2i.Swap: TPoint2i;
begin
  Result.X := Y;
  Result.Y := X;
end;

function TPoint2i.Empty: Boolean;
begin
  Result := (X = 0) and (Y = 0);
end;

function TPoint2i.Length: VectorFloat;
begin
  Result := Hypot(X, Y);
end;

function TPoint2i.Distance(const Point: TPoint2i): VectorFloat;
begin
  Result := (Self - Point).Length;
end;

function TPoint2i.Angle: VectorFloat;
begin
  if X <> 0 then
    Result := ArcTan2(-Y, X)
  else
    Result := 0;
end;

function TPoint2i.Dot(const Point: TPoint2i): VectorInt;
begin
  Result := (X * Point.X) + (Y * Point.Y);
end;

function TPoint2i.Cross(const Point: TPoint2i): VectorInt;
begin
  Result := (X * Point.Y) - (Y * Point.X);
end;

function TPoint2i.Lerp(const Point: TPoint2i; const Theta: VectorFloat): TPoint2i;
begin
  Result.X := Round(X + (Point.X - X) * Theta);
  Result.Y := Round(Y + (Point.Y - Y) * Theta);
end;

function TPoint2i.InsideTriangle(const Vertex1, Vertex2, Vertex3: TPoint2i): Boolean;
var
  Det: VectorInt;
begin
  Det := (Y - Vertex2.Y) * (Vertex3.X - Vertex2.X) - (X - Vertex2.X) * (Vertex3.Y - Vertex2.Y);

  Result := (Det * ((Y - Vertex1.Y) * (Vertex2.X - Vertex1.X) - (X - Vertex1.X) *
    (Vertex2.Y - Vertex1.Y)) > 0) and (Det * ((Y - Vertex3.Y) * (Vertex1.X - Vertex3.X) -
    (X - Vertex3.X) * (Vertex1.Y - Vertex3.Y)) > 0);
end;

function Point2i(const X, Y: VectorInt): TPoint2i;
begin
  Result.X := X;
  Result.Y := Y;
end;

{$ENDREGION}
{$REGION 'TPoint2f functions'}

class operator TPoint2f.Add(const APoint1, APoint2: TPoint2f): TPoint2f;
begin
  Result.X := APoint1.X + APoint2.X;
  Result.Y := APoint1.Y + APoint2.Y;
end;

class operator TPoint2f.Subtract(const APoint1, APoint2: TPoint2f): TPoint2f;
begin
  Result.X := APoint1.X - APoint2.X;
  Result.Y := APoint1.Y - APoint2.Y;
end;

class operator TPoint2f.Multiply(const APoint1, APoint2: TPoint2f): TPoint2f;
begin
  Result.X := APoint1.X * APoint2.X;
  Result.Y := APoint1.Y * APoint2.Y;
end;

class operator TPoint2f.Divide(const APoint1, APoint2: TPoint2f): TPoint2f;
begin
  Result.X := APoint1.X / APoint2.X;
  Result.Y := APoint1.Y / APoint2.Y;
end;

class operator TPoint2f.Negative(const Point: TPoint2f): TPoint2f;
begin
  Result.X := -Point.X;
  Result.Y := -Point.Y;
end;

class operator TPoint2f.Multiply(const Point: TPoint2f; const Theta: VectorFloat): TPoint2f;
begin
  Result.X := Point.X * Theta;
  Result.Y := Point.Y * Theta;
end;

class operator TPoint2f.Multiply(const Theta: VectorFloat; const Point: TPoint2f): TPoint2f;
begin
  Result.X := Theta * Point.X;
  Result.Y := Theta * Point.Y;
end;

class operator TPoint2f.Divide(const Point: TPoint2f; const Theta: VectorFloat): TPoint2f;
begin
  Result.X := Point.X / Theta;
  Result.Y := Point.Y / Theta;
end;

class operator TPoint2f.Divide(const Point: TPoint2f; const Theta: Integer): TPoint2f;
begin
  Result.X := Point.X / Theta;
  Result.Y := Point.Y / Theta;
end;

class operator TPoint2f.Implicit(const Point: TPoint2i): TPoint2f;
begin
  Result.X := Point.X;
  Result.Y := Point.Y;
end;

class operator TPoint2f.Equal(const APoint1, APoint2: TPoint2f): Boolean;
begin
  Result := NearlyEqual(APoint1.X, APoint2.X) and NearlyEqual(APoint1.Y, APoint2.Y);
end;

class operator TPoint2f.NotEqual(const APoint1, APoint2: TPoint2f): Boolean;
begin
  Result := not (APoint1 = APoint2);
end;

{$IFDEF POINT_FLOAT_TO_INT_IMPLICIT}
class operator TPoint2f.Implicit(const Point: TPoint2f): TPoint2i;
begin
  Result.X := Round(Point.X);
  Result.Y := Round(Point.Y);
end;
{$ENDIF}

function TPoint2f.Swap: TPoint2f;
begin
  Result.X := Y;
  Result.Y := X;
end;

function TPoint2f.Empty: Boolean;
begin
  Result := (Abs(X) < VectorEpsilon) and (Abs(Y) < VectorEpsilon);
end;

function TPoint2f.Length: VectorFloat;
begin
  Result := Hypot(X, Y);
end;

function TPoint2f.Distance(const Point: TPoint2f): VectorFloat;
begin
  Result := (Self - Point).Length;
end;

function TPoint2f.Angle: VectorFloat;
begin
  Result := ArcTan2(-Y, X);
end;

function TPoint2f.Dot(const Point: TPoint2f): VectorFloat;
begin
  Result := (X * Point.X) + (Y * Point.Y);
end;

function TPoint2f.Cross(const Point: TPoint2f): VectorFloat;
begin
  Result := (X * Point.Y) - (Y * Point.X);
end;

function TPoint2f.Normalize: TPoint2f;
begin
  Result := Self / Length;
end;

function TPoint2f.Lerp(const Point: TPoint2f; const Theta: VectorFloat): TPoint2f;
begin
  Result.X := X + (Point.X - X) * Theta;
  Result.Y := Y + (Point.Y - Y) * Theta;
end;

function TPoint2f.InsideTriangle(const Vertex1, Vertex2, Vertex3: TPoint2f): Boolean;
var
  Det: VectorFloat;
begin
  Det := (Y - Vertex2.Y) * (Vertex3.X - Vertex2.X) - (X - Vertex2.X) * (Vertex3.Y - Vertex2.Y);

  Result := (Det * ((Y - Vertex1.Y) * (Vertex2.X - Vertex1.X) - (X - Vertex1.X) *
    (Vertex2.Y - Vertex1.Y)) > 0.0) and (Det * ((Y - Vertex3.Y) * (Vertex1.X - Vertex3.X) -
    (X - Vertex3.X) * (Vertex1.Y - Vertex3.Y)) > 0.0);
end;

function TPoint2f.ToInt: TPoint2i;
begin
  Result.X := Round(X);
  Result.Y := Round(Y);
end;

function Point2f(const X, Y: VectorFloat): TPoint2f; inline;
begin
  Result.X := X;
  Result.Y := Y;
end;

{$ENDREGION}
{$REGION 'TVector3i functions'}

class operator TVector3i.Add(const Vector1, Vector2: TVector3i): TVector3i;
begin
  Result.X := Vector1.X + Vector2.X;
  Result.Y := Vector1.Y + Vector2.Y;
  Result.Z := Vector1.Z + Vector2.Z;
end;

class operator TVector3i.Subtract(const Vector1, Vector2: TVector3i): TVector3i;
begin
  Result.X := Vector1.X - Vector2.X;
  Result.Y := Vector1.Y - Vector2.Y;
  Result.Z := Vector1.Z - Vector2.Z;
end;

class operator TVector3i.Multiply(const Vector1, Vector2: TVector3i): TVector3i;
begin
  Result.X := Vector1.X * Vector2.X;
  Result.Y := Vector1.Y * Vector2.Y;
  Result.Z := Vector1.Z * Vector2.Z;
end;

class operator TVector3i.Divide(const Vector1, Vector2: TVector3i): TVector3i;
begin
  Result.X := Vector1.X div Vector2.X;
  Result.Y := Vector1.Y div Vector2.Y;
  Result.Z := Vector1.Z div Vector2.Z;
end;

class operator TVector3i.Negative(const Vector: TVector3i): TVector3i;
begin
  Result.X := -Vector.X;
  Result.Y := -Vector.Y;
  Result.Z := -Vector.Z;
end;

class operator TVector3i.Multiply(const Vector: TVector3i; const Theta: VectorInt): TVector3i;
begin
  Result.X := Vector.X * Theta;
  Result.Y := Vector.Y * Theta;
  Result.Z := Vector.Z * Theta;
end;

class operator TVector3i.Multiply(const Theta: VectorInt; const Vector: TVector3i): TVector3i;
begin
  Result.X := Theta * Vector.X;
  Result.Y := Theta * Vector.Y;
  Result.Z := Theta * Vector.Z;
end;

class operator TVector3i.Divide(const Vector: TVector3i; const Theta: VectorInt): TVector3i;
begin
  Result.X := Vector.X div Theta;
  Result.Y := Vector.Y div Theta;
  Result.Z := Vector.Z div Theta;
end;

class operator TVector3i.Equal(const Vector1, Vector2: TVector3i): Boolean;
begin
  Result := (Vector1.X = Vector2.X) and (Vector1.Y = Vector2.Y) and (Vector1.Z = Vector2.Z);
end;

class operator TVector3i.NotEqual(const Vector1, Vector2: TVector3i): Boolean;
begin
  Result := not (Vector1 = Vector2);
end;

function TVector3i.Length: VectorFloat;
begin
  Result := Sqrt(Dot(Self));
end;

function TVector3i.Dot(const Vector: TVector3i): VectorInt;
begin
  Result := (X * Vector.X) + (Y * Vector.Y) + (Z * Vector.Z);
end;

function TVector3i.Cross(const Vector: TVector3i): TVector3i;
begin
  Result.X := (Y * Vector.Z) - (Z * Vector.Y);
  Result.Y := (Z * Vector.X) - (X * Vector.Z);
  Result.Z := (X * Vector.Y) - (Y * Vector.X);
end;

function TVector3i.Angle(const Vector: TVector3i): VectorFloat;
var
  Amp, CosValue: VectorFloat;
begin
  Amp := Sqrt(Dot(Self) * Vector.Dot(Vector));

  if Amp > VectorEpsilon then
    CosValue := Dot(Vector) / Amp
  else
    CosValue := Dot(Vector) / VectorEpsilon;

  Result := ArcCos(Saturate(CosValue, -1.0, 1.0));
end;

function TVector3i.Lerp(const Vector: TVector3i; const Theta: VectorFloat): TVector3i;
begin
  Result.X := Round(X + (Vector.X - X) * Theta);
  Result.Y := Round(Y + (Vector.Y - Y) * Theta);
  Result.Z := Round(Z + (Vector.Z - Z) * Theta);
end;

function TVector3i.GetXY: TPoint2i;
begin
  Result.X := X;
  Result.Y := Y;
end;

function Vector3i(const X, Y, Z: VectorInt): TVector3i;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function Vector3i(const Point: TPoint2i; const Z: VectorInt): TVector3i; overload;
begin
  Result.X := Point.X;
  Result.Y := Point.Y;
  Result.Z := Z;
end;

{$ENDREGION}
{$REGION 'TVector3f functions'}

class operator TVector3f.Add(const Vector1, Vector2: TVector3f): TVector3f;
begin
  Result.X := Vector1.X + Vector2.X;
  Result.Y := Vector1.Y + Vector2.Y;
  Result.Z := Vector1.Z + Vector2.Z;
end;

class operator TVector3f.Subtract(const Vector1, Vector2: TVector3f): TVector3f;
begin
  Result.X := Vector1.X - Vector2.X;
  Result.Y := Vector1.Y - Vector2.Y;
  Result.Z := Vector1.Z - Vector2.Z;
end;

class operator TVector3f.Multiply(const Vector1, Vector2: TVector3f): TVector3f;
begin
  Result.X := Vector1.X * Vector2.X;
  Result.Y := Vector1.Y * Vector2.Y;
  Result.Z := Vector1.Z * Vector2.Z;
end;

class operator TVector3f.Divide(const Vector1, Vector2: TVector3f): TVector3f;
begin
  Result.X := Vector1.X / Vector2.X;
  Result.Y := Vector1.Y / Vector2.Y;
  Result.Z := Vector1.Z / Vector2.Z;
end;

class operator TVector3f.Negative(const Vector: TVector3f): TVector3f;
begin
  Result.X := -Vector.X;
  Result.Y := -Vector.Y;
  Result.Z := -Vector.Z;
end;

class operator TVector3f.Multiply(const Vector: TVector3f; const Theta: VectorFloat): TVector3f;
begin
  Result.X := Vector.X * Theta;
  Result.Y := Vector.Y * Theta;
  Result.Z := Vector.Z * Theta;
end;

class operator TVector3f.Multiply(const Theta: VectorFloat; const Vector: TVector3f): TVector3f;
begin
  Result.X := Theta * Vector.X;
  Result.Y := Theta * Vector.Y;
  Result.Z := Theta * Vector.Z;
end;

class operator TVector3f.Divide(const Vector: TVector3f; const Theta: VectorFloat): TVector3f;
var
  InvTheta: VectorFloat;
begin
  InvTheta := 1.0 / Theta;
  Result.X := Vector.X * InvTheta;
  Result.Y := Vector.Y * InvTheta;
  Result.Z := Vector.Z * InvTheta;
end;

class operator TVector3f.Implicit(const Vector: TVector3i): TVector3f;
begin
  Result.X := Vector.X;
  Result.Y := Vector.Y;
  Result.Z := Vector.Z;
end;

class operator TVector3f.Equal(const Vector1, Vector2: TVector3f): Boolean;
begin
  Result := NearlyEqual(Vector1.X, Vector2.X) and NearlyEqual(Vector1.Y, Vector2.Y) and
    NearlyEqual(Vector1.Z, Vector2.Z);
end;

class operator TVector3f.NotEqual(const Vector1, Vector2: TVector3f): Boolean;
begin
  Result := not (Vector1 = Vector2);
end;

function TVector3f.Empty: Boolean;
begin
  Result := (Abs(X) < VectorEpsilon) and (Abs(Y) < VectorEpsilon) and (Abs(Z) < VectorEpsilon);
end;

function TVector3f.Length: VectorFloat;
begin
  Result := Sqrt(Dot(Self));
end;

function TVector3f.Distance(const Vector: TVector3f): VectorFloat;
begin
  Result := (Self - Vector).Length;
end;

function TVector3f.Dot(const Vector: TVector3f): VectorFloat;
begin
  Result := (X * Vector.X) + (Y * Vector.Y) + (Z * Vector.Z);
end;

function TVector3f.Cross(const Vector: TVector3f): TVector3f;
begin
  Result.X := (Y * Vector.Z) - (Z * Vector.Y);
  Result.Y := (Z * Vector.X) - (X * Vector.Z);
  Result.Z := (X * Vector.Y) - (Y * Vector.X);
end;

function TVector3f.Angle(const Vector: TVector3f): VectorFloat;
var
  CosValue: VectorFloat;
begin
  CosValue := Dot(Vector) / (Sqrt(Dot(Self) * Vector.Dot(Vector)));
  Result := ArcCos(Saturate(CosValue, -1.0, 1.0));
end;

function TVector3f.Normalize: TVector3f;
var
  InvAmp: VectorFloat;
begin
  InvAmp := 1.0 / Length;
  Result := Self * InvAmp;
end;

function TVector3f.Parallel(const Direction: TVector3f): TVector3f;
begin
  Result := Direction * (Dot(Direction) / Sqr(Direction.Length));
end;

function TVector3f.Perpendicular(const Direction: TVector3f): TVector3f;
begin
  Result := Self - Parallel(Direction);
end;

function TVector3f.Reflect(const Normal: TVector3f): TVector3f;
begin
  Result := Self - (Normal * (Dot(Normal) * 2.0));
end;

function TVector3f.Lerp(const Vector: TVector3f; const Theta: VectorFloat): TVector3f;
begin
  Result.X := X + (Vector.X - X) * Theta;
  Result.Y := Y + (Vector.Y - Y) * Theta;
  Result.Z := Z + (Vector.Z - Z) * Theta;
end;

function TVector3f.ToInt: TVector3i;
begin
  Result.X := Round(X);
  Result.Y := Round(Y);
  Result.Z := Round(Z);
end;

function TVector3f.GetXY: TPoint2f;
begin
  Result.X := X;
  Result.Y := Y;
end;

function Vector3f(const X, Y, Z: VectorFloat): TVector3f;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function Vector3f(const Point: TPoint2f; const Z: VectorFloat): TVector3f; overload;
begin
  Result.X := Point.X;
  Result.Y := Point.Y;
  Result.Z := Z;
end;

{$ENDREGION}
{$REGION 'TVector4f functions'}

class operator TVector4f.Add(const Vector1, Vector2: TVector4f): TVector4f;
begin
  Result.X := Vector1.X + Vector2.X;
  Result.Y := Vector1.Y + Vector2.Y;
  Result.Z := Vector1.Z + Vector2.Z;
  Result.W := Vector1.W + Vector2.W;
end;

class operator TVector4f.Subtract(const Vector1, Vector2: TVector4f): TVector4f;
begin
  Result.X := Vector1.X - Vector2.X;
  Result.Y := Vector1.Y - Vector2.Y;
  Result.Z := Vector1.Z - Vector2.Z;
  Result.W := Vector1.W - Vector2.W;
end;

class operator TVector4f.Multiply(const Vector1, Vector2: TVector4f): TVector4f;
begin
  Result.X := Vector1.X * Vector2.X;
  Result.Y := Vector1.Y * Vector2.Y;
  Result.Z := Vector1.Z * Vector2.Z;
  Result.W := Vector1.W * Vector2.W;
end;

class operator TVector4f.Divide(const Vector1, Vector2: TVector4f): TVector4f;
begin
  Result.X := Vector1.X / Vector2.X;
  Result.Y := Vector1.Y / Vector2.Y;
  Result.Z := Vector1.Z / Vector2.Z;
  Result.W := Vector1.W / Vector2.W;
end;

class operator TVector4f.Negative(const Vector: TVector4f): TVector4f;
begin
  Result.X := -Vector.X;
  Result.Y := -Vector.Y;
  Result.Z := -Vector.Z;
  Result.W := -Vector.W;
end;

class operator TVector4f.Multiply(const Vector: TVector4f; const Theta: VectorFloat): TVector4f;
begin
  Result.X := Vector.X * Theta;
  Result.Y := Vector.Y * Theta;
  Result.Z := Vector.Z * Theta;
  Result.W := Vector.W * Theta;
end;

class operator TVector4f.Multiply(const Theta: VectorFloat; const Vector: TVector4f): TVector4f;
begin
  Result.X := Theta * Vector.X;
  Result.Y := Theta * Vector.Y;
  Result.Z := Theta * Vector.Z;
  Result.W := Theta * Vector.W;
end;

class operator TVector4f.Divide(const Vector: TVector4f; const Theta: VectorFloat): TVector4f;
begin
  Result.X := Vector.X / Theta;
  Result.Y := Vector.Y / Theta;
  Result.Z := Vector.Z / Theta;
  Result.W := Vector.W / Theta;
end;

class operator TVector4f.Equal(const Vector1, Vector2: TVector4f): Boolean;
begin
  Result := NearlyEqual(Vector1.X, Vector2.X) and NearlyEqual(Vector1.Y, Vector2.Y) and
    NearlyEqual(Vector1.Z, Vector2.Z) and NearlyEqual(Vector1.W, Vector2.W);
end;

class operator TVector4f.NotEqual(const Vector1, Vector2: TVector4f): Boolean;
begin
  Result := not (Vector1 = Vector2);
end;

function TVector4f.Lerp(const Vector: TVector4f; const Theta: VectorFloat): TVector4f;
begin
  Result.X := X + (Vector.X - X) * Theta;
  Result.Y := Y + (Vector.Y - Y) * Theta;
  Result.Z := Z + (Vector.Z - Z) * Theta;
  Result.W := W + (Vector.W - W) * Theta;
end;

function TVector4f.GetXYZ: TVector3f;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function TVector4f.ProjectToXYZ: TVector3f;
begin
  Result := GetXYZ * (1.0 / W);
end;

function Vector4f(const X, Y, Z, W: VectorFloat): TVector4f;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result.W := W;
end;

function Vector4f(const Vector: TVector3f; const W: VectorFloat): TVector4f;
begin
  Result.X := Vector.X;
  Result.Y := Vector.Y;
  Result.Z := Vector.Z;
  Result.W := W;
end;

function Vector4f(const Point: TPoint2f; const Z, W: VectorFloat): TVector4f;
begin
  Result.X := Point.X;
  Result.Y := Point.Y;
  Result.Z := Z;
  Result.W := W;
end;

{$ENDREGION}
{$REGION 'TMatrix3f functions'}

class operator TMatrix3f.Add(const Matrix1, Matrix2: TMatrix3f): TMatrix3f;
var
  I, J: Integer;
begin
  for J := 0 to 2 do
    for I := 0 to 2 do
      Result.Data[J, I] := Matrix1.Data[J, I] + Matrix2.Data[J, I];
end;

class operator TMatrix3f.Subtract(const Matrix1, Matrix2: TMatrix3f): TMatrix3f;
var
  I, J: Integer;
begin
  for J := 0 to 2 do
    for I := 0 to 2 do
      Result.Data[J, I] := Matrix1.Data[J, I] - Matrix2.Data[J, I];
end;

class operator TMatrix3f.Multiply(const Matrix1, Matrix2: TMatrix3f): TMatrix3f;
var
  I, J: Integer;
begin
  for J := 0 to 2 do
    for I := 0 to 2 do
      Result.Data[J, I] := (Matrix1.Data[J, 0] * Matrix2.Data[0, I]) + (Matrix1.Data[J, 1] * Matrix2.Data[1, I]) +
        (Matrix1.Data[J, 2] * Matrix2.Data[2, I]);
end;

class operator TMatrix3f.Multiply(const Matrix: TMatrix3f; const Theta: VectorFloat): TMatrix3f;
var
  I, J: Integer;
begin
  for J := 0 to 2 do
    for I := 0 to 2 do
      Result.Data[J, I] := Matrix.Data[J, I] * Theta;
end;

class operator TMatrix3f.Multiply(const Theta: VectorFloat; const Matrix: TMatrix3f): TMatrix3f;
begin
  Result := Matrix * Theta;
end;

class operator TMatrix3f.Divide(const Matrix: TMatrix3f; const Theta: VectorFloat): TMatrix3f;
var
  I, J: Integer;
begin
  for J := 0 to 2 do
    for I := 0 to 2 do
      Result.Data[J, I] := Matrix.Data[J, I] / Theta;
end;

class operator TMatrix3f.Multiply(const Point: TPoint2f; const Matrix: TMatrix3f): TPoint2f;
begin
  Result.X := (Point.X * Matrix.Data[0, 0]) + (Point.Y * Matrix.Data[1, 0]) + Matrix.Data[2, 0];
  Result.Y := (Point.X * Matrix.Data[0, 1]) + (Point.Y * Matrix.Data[1, 1]) + Matrix.Data[2, 1];
end;

function TMatrix3f.Determinant: VectorFloat;
begin
  Result := Self.Data[0, 0] * (Self.Data[1, 1] * Self.Data[2, 2] - Self.Data[2, 1] * Self.Data[1, 2]) -
    Self.Data[0, 1] * (Self.Data[1, 0] * Self.Data[2, 2] - Self.Data[2, 0] * Self.Data[1, 2]) + Self.Data[0, 2] *
    (Self.Data[1, 0] * Self.Data[2, 1] - Self.Data[2, 0] * Self.Data[1, 1]);
end;

function TMatrix3f.Transpose: TMatrix3f;
var
  I, J: Integer;
begin
  for I := 0 to 2 do
    for J := 0 to 2 do
      Result.Data[I, J] := Data[J, I];
end;

function TMatrix3f.Adjoint: TMatrix3f;
begin
  Result.Data[0, 0] := Data[1, 1] * Data[2, 2] - Data[2, 1] * Data[1, 2];
  Result.Data[0, 1] := Data[2, 1] * Data[0, 2] - Data[0, 1] * Data[2, 2];
  Result.Data[0, 2] := Data[0, 1] * Data[1, 2] - Data[1, 1] * Data[0, 2];
  Result.Data[1, 0] := Data[2, 0] * Data[1, 2] - Data[1, 0] * Data[2, 2];
  Result.Data[1, 1] := Data[0, 0] * Data[2, 2] - Data[2, 0] * Data[0, 2];
  Result.Data[1, 2] := Data[1, 0] * Data[0, 2] - Data[0, 0] * Data[1, 2];
  Result.Data[2, 0] := Data[1, 0] * Data[2, 1] - Data[2, 0] * Data[1, 1];
  Result.Data[2, 1] := Data[2, 0] * Data[0, 1] - Data[0, 0] * Data[2, 1];
  Result.Data[2, 2] := Data[0, 0] * Data[1, 1] - Data[1, 0] * Data[0, 1];
end;

function TMatrix3f.Inverse: TMatrix3f;
begin
  Result := Adjoint * (1.0 / Determinant);
end;

class function TMatrix3f.Translate(const Offset: TPoint2f): TMatrix3f;
begin
  Result := IdentityMatrix3f;
  Result.Data[2, 0] := Offset.X;
  Result.Data[2, 1] := Offset.Y;
end;

class function TMatrix3f.Translate(const X, Y: VectorFloat): TMatrix3f;
begin
  Result := Translate(Point2f(X, Y));
end;

class function TMatrix3f.Rotate(const Angle: VectorFLoat): TMatrix3f;
var
  SinValue, CosValue: {$IFDEF DELPHI_LEGACY}Extended{$ELSE}VectorFloat{$ENDIF};
begin
  SinCos(Angle, SinValue, CosValue);

  Result := IdentityMatrix3f;
  Result.Data[0, 0] := CosValue;
  Result.Data[0, 1] := SinValue;
  Result.Data[1, 0] := -Result.Data[0, 1];
  Result.Data[1, 1] := Result.Data[0, 0];
end;

class function TMatrix3f.Scale(const Scale: TPoint2f): TMatrix3f;
begin
  Result := IdentityMatrix3f;
  Result.Data[0, 0] := Scale.X;
  Result.Data[1, 1] := Scale.Y;
end;

class function TMatrix3f.Scale(const X, Y: VectorFloat): TMatrix3f;
begin
  Result := Scale(Point2f(X, Y));
end;

class function TMatrix3f.Scale(const Scale: VectorFloat): TMatrix3f;
begin
  Result := TMatrix3f.Scale(Point2f(Scale, Scale));
end;

{$ENDREGION}
{$REGION 'TMatrix4f functions'}

class operator TMatrix4f.Add(const Matrix1, Matrix2: TMatrix4f): TMatrix4f;
var
  I, J: Integer;
begin
  for J := 0 to 3 do
    for I := 0 to 3 do
      Result.Data[J, I] := Matrix1.Data[J, I] + Matrix2.Data[J, I];
end;

class operator TMatrix4f.Subtract(const Matrix1, Matrix2: TMatrix4f): TMatrix4f;
var
  I, J: Integer;
begin
  for J := 0 to 3 do
    for I := 0 to 3 do
      Result.Data[J, I] := Matrix1.Data[J, I] - Matrix2.Data[J, I];
end;

class operator TMatrix4f.Multiply(const Matrix1, Matrix2: TMatrix4f): TMatrix4f;
var
  I, J: Integer;
begin
  for J := 0 to 3 do
    for I := 0 to 3 do
      Result.Data[J, I] := (Matrix1.Data[J, 0] * Matrix2.Data[0, I]) + (Matrix1.Data[J, 1] * Matrix2.Data[1, I]) +
        (Matrix1.Data[J, 2] * Matrix2.Data[2, I]) + (Matrix1.Data[J, 3] * Matrix2.Data[3, I]);
end;

class operator TMatrix4f.Multiply(const Matrix: TMatrix4f; const Theta: VectorFloat): TMatrix4f;
var
  I, J: Integer;
begin
  for J := 0 to 3 do
    for I := 0 to 3 do
      Result.Data[J, I] := Matrix.Data[J, I] * Theta;
end;

class operator TMatrix4f.Multiply(const Theta: VectorFloat; const Matrix: TMatrix4f): TMatrix4f;
begin
  Result := Matrix * Theta;
end;

class operator TMatrix4f.Divide(const Matrix: TMatrix4f; const Theta: VectorFloat): TMatrix4f;
var
  I, J: Integer;
begin
  for J := 0 to 3 do
    for I := 0 to 3 do
      Result.Data[J, I] := Matrix.Data[J, I] / Theta;
end;

class operator TMatrix4f.Multiply(const Vector: TVector3f; const Matrix: TMatrix4f): TVector3f;
begin
  Result.X := Vector.X * Matrix.Data[0, 0] + Vector.Y * Matrix.Data[1, 0] + Vector.Z * Matrix.Data[2, 0] +
    Matrix.Data[3, 0];
  Result.Y := Vector.X * Matrix.Data[0, 1] + Vector.Y * Matrix.Data[1, 1] + Vector.Z * Matrix.Data[2, 1] +
    Matrix.Data[3, 1];
  Result.Z := Vector.X * Matrix.Data[0, 2] + Vector.Y * Matrix.Data[1, 2] + Vector.Z * Matrix.Data[2, 2] +
    Matrix.Data[3, 2];
end;

class operator TMatrix4f.Multiply(const Vector: TVector4f; const Matrix: TMatrix4f): TVector4f;
begin
  Result.X := Vector.X * Matrix.Data[0, 0] + Vector.Y * Matrix.Data[1, 0] + Vector.Z * Matrix.Data[2, 0] + Vector.W *
    Matrix.Data[3, 0];
  Result.Y := Vector.X * Matrix.Data[0, 1] + Vector.Y * Matrix.Data[1, 1] + Vector.Z * Matrix.Data[2, 1] + Vector.W *
    Matrix.Data[3, 1];
  Result.Z:= Vector.X * Matrix.Data[0, 2] + Vector.Y * Matrix.Data[1, 2] + Vector.Z * Matrix.Data[2, 2] + Vector.W *
    Matrix.Data[3, 2];
  Result.W:= Vector.X * Matrix.Data[0, 3] + Vector.Y * Matrix.Data[1, 3] + Vector.Z * Matrix.Data[2, 3] + Vector.W *
    Matrix.Data[3, 3];
end;

class function TMatrix4f.DetSub3(const A1, A2, A3, B1, B2, B3, C1, C2, C3: VectorFloat): VectorFloat;
begin
  Result := A1 * (B2 * C3 - B3 * C2) - B1 * (A2 * C3 - A3 * C2) + C1 * (A2 * B3 - A3 * B2);
end;

function TMatrix4f.Determinant: VectorFloat;
begin
  Result := Self.Data[0, 0] * DetSub3(Self.Data[1, 1], Self.Data[2, 1], Self.Data[3, 1], Self.Data[1, 2],
    Self.Data[2, 2], Self.Data[3, 2], Self.Data[1, 3], Self.Data[2, 3], Self.Data[3, 3]) - Self.Data[0, 1] *
    DetSub3(Self.Data[1, 0], Self.Data[2, 0], Self.Data[3, 0], Self.Data[1, 2], Self.Data[2, 2], Self.Data[3, 2],
    Self.Data[1, 3], Self.Data[2, 3], Self.Data[3, 3]) + Self.Data[0, 2] * DetSub3(Self.Data[1, 0], Self.Data[2, 0],
    Self.Data[3, 0], Self.Data[1, 1], Self.Data[2, 1], Self.Data[3, 1], Self.Data[1, 3], Self.Data[2, 3],
    Self.Data[3, 3]) - Self.Data[0, 3] * DetSub3(Self.Data[1, 0], Self.Data[2, 0], Self.Data[3, 0], Self.Data[1, 1],
    Self.Data[2, 1], Self.Data[3, 1], Self.Data[1, 2], Self.Data[2, 2], Self.Data[3, 2]);
end;

function TMatrix4f.EyePos: TVector3f;
begin
  Result.X := -Self.Data[0, 0] * Self.Data[3, 0] - Self.Data[0, 1] * Self.Data[3, 1] - Self.Data[0, 2] *
    Self.Data[3, 2];
  Result.Y := -Self.Data[1, 0] * Self.Data[3, 0] - Self.Data[1, 1] * Self.Data[3, 1] - Self.Data[1, 2] *
    Self.Data[3, 2];
  Result.Z := -Self.Data[2, 0] * Self.Data[3, 0] - Self.Data[2, 1] * Self.Data[3, 1] - Self.Data[2, 2] *
    Self.Data[3, 2];
end;

function TMatrix4f.WorldPos: TVector3f;
begin
  Result.X := Self.Data[3, 0];
  Result.Y := Self.Data[3, 1];
  Result.Z := Self.Data[3, 2];
end;

function TMatrix4f.Transpose: TMatrix4f;
var
  I, J: Integer;
begin
  for I := 0 to 3 do
    for J := 0 to 3 do
      Result.Data[I, J] := Data[J, I];
end;

function TMatrix4f.Adjoint: TMatrix4f;
begin
  Result.Data[0, 0] := DetSub3(Data[1, 1], Data[2, 1], Data[3, 1], Data[1, 2], Data[2, 2], Data[3, 2],
    Data[1, 3], Data[2, 3], Data[3, 3]);
  Result.Data[1, 0] := -DetSub3(Data[1, 0], Data[2, 0], Data[3, 0], Data[1, 2], Data[2, 2], Data[3, 2],
    Data[1, 3], Data[2, 3], Data[3, 3]);
  Result.Data[2, 0] := DetSub3(Data[1, 0], Data[2, 0], Data[3, 0], Data[1, 1], Data[2, 1], Data[3, 1],
    Data[1, 3], Data[2, 3], Data[3, 3]);
  Result.Data[3, 0] := -DetSub3(Data[1, 0], Data[2, 0], Data[3, 0], Data[1, 1], Data[2, 1], Data[3, 1],
    Data[1, 2], Data[2, 2], Data[3, 2]);

  Result.Data[0, 1] := -DetSub3(Data[0, 1], Data[2, 1], Data[3, 1], Data[0, 2], Data[2, 2], Data[3, 2],
    Data[0, 3], Data[2, 3], Data[3, 3]);
  Result.Data[1, 1] := DetSub3(Data[0, 0], Data[2, 0], Data[3, 0], Data[0, 2], Data[2, 2], Data[3, 2],
    Data[0, 3], Data[2, 3], Data[3, 3]);
  Result.Data[2, 1] := -DetSub3(Data[0, 0], Data[2, 0], Data[3, 0], Data[0, 1], Data[2, 1], Data[3, 1],
    Data[0, 3], Data[2, 3], Data[3, 3]);
  Result.Data[3, 1] := DetSub3(Data[0, 0], Data[2, 0], Data[3, 0], Data[0, 1], Data[2, 1], Data[3, 1],
    Data[0, 2], Data[2, 2], Data[3, 2]);

  Result.Data[0, 2] := DetSub3(Data[0, 1], Data[1, 1], Data[3, 1], Data[0, 2], Data[1, 2], Data[3, 2],
    Data[0, 3], Data[1, 3], Data[3, 3]);
  Result.Data[1, 2] := -DetSub3(Data[0, 0], Data[1, 0], Data[3, 0], Data[0, 2], Data[1, 2], Data[3, 2],
    Data[0, 3], Data[1, 3], Data[3, 3]);
  Result.Data[2, 2] := DetSub3(Data[0, 0], Data[1, 0], Data[3, 0], Data[0, 1], Data[1, 1], Data[3, 1],
    Data[0, 3], Data[1, 3], Data[3, 3]);
  Result.Data[3, 2] := -DetSub3(Data[0, 0], Data[1, 0], Data[3, 0], Data[0, 1], Data[1, 1], Data[3, 1],
    Data[0, 2], Data[1, 2], Data[3, 2]);

  Result.Data[0, 3] := -DetSub3(Data[0, 1], Data[1, 1], Data[2, 1], Data[0, 2], Data[1, 2], Data[2, 2],
    Data[0, 3], Data[1, 3], Data[2, 3]);
  Result.Data[1, 3] := DetSub3(Data[0, 0], Data[1, 0], Data[2, 0], Data[0, 2], Data[1, 2], Data[2, 2],
    Data[0, 3], Data[1, 3], Data[2, 3]);
  Result.Data[2, 3] := -DetSub3(Data[0, 0], Data[1, 0], Data[2, 0], Data[0, 1], Data[1, 1], Data[2, 1],
    Data[0, 3], Data[1, 3], Data[2, 3]);
  Result.Data[3, 3] := DetSub3(Data[0, 0], Data[1, 0], Data[2, 0], Data[0, 1], Data[1, 1], Data[2, 1],
    Data[0, 2], Data[1, 2], Data[2, 2]);
end;

function TMatrix4f.Inverse: TMatrix4f;
begin
  Result := Adjoint * (1.0 / Determinant);
end;

function TMatrix4f.Project(const Vector: TVector3f; const TargetSize: TPoint2f): TPoint2f;
var
  Last: VectorFloat;
begin
  Last := Vector.X * Data[0, 3] + Vector.Y * Data[1, 3] + Vector.Z * Data[2, 3] + Data[3, 3];

  Result.X := Vector.X * Data[0, 0] + Vector.Y * Data[1, 0] + Vector.Z * Data[2, 0] + Data[3, 0];
  Result.Y := Vector.X * Data[0, 1] + Vector.Y * Data[1, 1] + Vector.Z * Data[2, 1] + Data[3, 1];

  Result := Result / Last;

  Result.X := (Result.X * 0.5 + 0.5) * TargetSize.X;
  Result.Y := (0.5 - Result.Y * 0.5) * TargetSize.Y;
end;

class function TMatrix4f.Translate(const Offset: TVector3f): TMatrix4f;
begin
  Result := IdentityMatrix4f;
  Result.Data[3, 0] := Offset.X;
  Result.Data[3, 1] := Offset.Y;
  Result.Data[3, 2] := Offset.Z;
end;

class function TMatrix4f.Translate(const X, Y, Z: VectorFloat): TMatrix4f;
begin
  Result := Translate(Vector3f(X, Y, Z));
end;

class function TMatrix4f.RotateX(const Angle: VectorFloat): TMatrix4f;
var
  SinValue, CosValue: {$IFDEF DELPHI_LEGACY}Extended{$ELSE}VectorFloat{$ENDIF};
begin
  SinCos(Angle, SinValue, CosValue);

  Result := IdentityMatrix4f;
  Result.Data[1, 1] := CosValue;
  Result.Data[1, 2] := SinValue;
  Result.Data[2, 1] := -Result.Data[1, 2];
  Result.Data[2, 2] := Result.Data[1, 1];
end;

class function TMatrix4f.RotateY(const Angle: VectorFloat): TMatrix4f;
var
  SinValue, CosValue: {$IFDEF DELPHI_LEGACY}Extended{$ELSE}VectorFloat{$ENDIF};
begin
  SinCos(Angle, SinValue, CosValue);

  Result := IdentityMatrix4f;
  Result.Data[0, 0] := CosValue;
  Result.Data[0, 2] := -SinValue;
  Result.Data[2, 0] := -Result.Data[0, 2];
  Result.Data[2, 2] := Result.Data[0, 0];
end;

class function TMatrix4f.RotateZ(const Angle: VectorFloat): TMatrix4f;
var
  SinValue, CosValue: {$IFDEF DELPHI_LEGACY}Extended{$ELSE}VectorFloat{$ENDIF};
begin
  SinCos(Angle, SinValue, CosValue);

  Result := IdentityMatrix4f;
  Result.Data[0, 0] := CosValue;
  Result.Data[0, 1] := SinValue;
  Result.Data[1, 0] := -Result.Data[0, 1];
  Result.Data[1, 1] := Result.Data[0, 0];
end;

class function TMatrix4f.Rotate(const Axis: TVector3f; const Angle: VectorFloat): TMatrix4f;
var
  CosValue, SinValue: {$IFDEF DELPHI_LEGACY}Extended{$ELSE}VectorFloat{$ENDIF};
  InvCosValue, XYMul, XZMul, YZMul, XSin, YSin, ZSin: VectorFloat;
begin
  SinCos(Angle, SinValue, CosValue);
  InvCosValue := 1.0 - CosValue;

  XYMul := Axis.X * Axis.Y * InvCosValue;
  XZMul := Axis.X * Axis.Z * InvCosValue;
  YZMul := Axis.Y * Axis.Z * InvCosValue;

  XSin := Axis.X * SinValue;
  YSin := Axis.Y * SinValue;
  ZSin := Axis.Z * SinValue;

  Result := IdentityMatrix4f;
  Result.Data[0, 0] := Sqr(Axis.X) * InvCosValue + CosValue;
  Result.Data[0, 1] := XYMul + ZSin;
  Result.Data[0, 2] := XZMul - YSin;
  Result.Data[1, 0] := XYMul - ZSin;
  Result.Data[1, 1] := Sqr(Axis.Y) * InvCosValue + CosValue;
  Result.Data[1, 2] := YZMul + XSin;
  Result.Data[2, 0] := XZMul + YSin;
  Result.Data[2, 1] := YZMul - XSin;
  Result.Data[2, 2] := Sqr(Axis.Z) * InvCosValue + CosValue;
end;

class function TMatrix4f.Scale(const Scale: TVector3f): TMatrix4f;
begin
  Result := IdentityMatrix4f;
  Result.Data[0, 0] := Scale.X;
  Result.Data[1, 1] := Scale.Y;
  Result.Data[2, 2] := Scale.Z;
end;

class function TMatrix4f.Scale(const X, Y, Z: VectorFloat): TMatrix4f;
begin
  Result := Scale(Vector3f(X, Y, Z));
end;

class function TMatrix4f.Scale(const Scale: VectorFloat): TMatrix4f;
begin
  Result := TMatrix4f.Scale(Vector3f(Scale, Scale, Scale));
end;

class function TMatrix4f.HeadingPitchBank(const Heading, Pitch, Bank: VectorFloat): TMatrix4f;
var
  CosHeading, SinHeading, CosPitch: {$IFDEF DELPHI_LEGACY}Extended{$ELSE}VectorFloat{$ENDIF};
  SinPitch, CosBank, SinBank: {$IFDEF DELPHI_LEGACY}Extended{$ELSE}VectorFloat{$ENDIF};
begin
  SinCos(Heading, SinHeading, CosHeading);
  SinCos(Pitch, SinPitch, CosPitch);
  SinCos(Bank, SinBank, CosBank);

  Result := IdentityMatrix4f;
  Result.Data[0, 0] := CosHeading * CosBank + SinHeading * SinPitch * SinBank;
  Result.Data[0, 1] := SinHeading * SinPitch * CosBank - CosHeading * SinBank;
  Result.Data[0, 2] := SinHeading * CosPitch;
  Result.Data[1, 0] := SinBank * CosPitch;
  Result.Data[1, 1] := CosBank * CosPitch;
  Result.Data[1, 2] := - SinPitch;
  Result.Data[2, 0] := CosHeading * SinPitch * SinBank - SinHeading * CosBank;
  Result.Data[2, 1] := SinBank * SinHeading + CosHeading * SinPitch * CosBank;
  Result.Data[2, 2] := CosHeading * CosPitch;
end;

class function TMatrix4f.HeadingPitchBank(const Vector: TVector3f): TMatrix4f;
begin
  Result := HeadingPitchBank(Vector.Y, Vector.X, Vector.Z);
end;

class function TMatrix4f.YawPitchRoll(const Yaw, Pitch, Roll: VectorFloat): TMatrix4f;
var
  SinYaw, CosYaw, SinPitch: {$IFDEF DELPHI_LEGACY}Extended{$ELSE}VectorFloat{$ENDIF};
  CosPitch, SinRoll, CosRoll: {$IFDEF DELPHI_LEGACY}Extended{$ELSE}VectorFloat{$ENDIF};
begin
  SinCos(Yaw, SinYaw, CosYaw);
  SinCos(Pitch, SinPitch, CosPitch);
  SinCos(Roll, SinRoll, CosRoll);

  Result := IdentityMatrix4f;
  Result.Data[0, 0] := CosRoll * CosYaw + SinPitch * SinRoll * SinYaw;
  Result.Data[0, 1] := CosYaw * SinPitch * SinRoll - CosRoll * SinYaw;
  Result.Data[0, 2] := -CosPitch * SinRoll;
  Result.Data[1, 0] := CosPitch * SinYaw;
  Result.Data[1, 1] := CosPitch * CosYaw;
  Result.Data[1, 2] := SinPitch;
  Result.Data[2, 0] := CosYaw * SinRoll - CosRoll * SinPitch * SinYaw;
  Result.Data[2, 1] := -CosRoll * CosYaw * SinPitch - SinRoll * SinYaw;
  Result.Data[2, 2] := CosPitch * CosRoll;
end;

class function TMatrix4f.YawPitchRoll(const Vector: TVector3f): TMatrix4f;
begin
  Result := YawPitchRoll(Vector.Y, Vector.X, Vector.Z);
end;

class function TMatrix4f.Reflect(const Axis: TVector3f): TMatrix4f;
var
  XYMul, YZMul, XZMul: VectorFloat;
begin
  XYMul := -2.0 * Axis.X * Axis.Y;
  XZMul := -2.0 * Axis.X * Axis.Z;
  YZMul := -2.0 * Axis.Y * Axis.Z;

  Result := IdentityMatrix4f;
  Result.Data[0, 0] := 1.0 - (2.0 * Sqr(Axis.X));
  Result.Data[0, 1] := XYMul;
  Result.Data[0, 2] := XZMul;
  Result.Data[1, 0] := XYMul;
  Result.Data[1, 1] := 1.0 - (2.0 * Sqr(Axis.Y));
  Result.Data[1, 2] := YZMul;
  Result.Data[2, 0] := XZMul;
  Result.Data[2, 1] := YZMul;
  Result.Data[2, 2] := 1.0 - (2.0 * Sqr(Axis.Z));
end;

class function TMatrix4f.LookAt(const Origin, Target, Ceiling: TVector3f): TMatrix4f;
var
  XAxis, YAxis, ZAxis: TVector3f;
begin
  ZAxis := (Target - Origin).Normalize;
  XAxis := Ceiling.Cross(ZAxis).Normalize;
  YAxis := ZAxis.Cross(XAxis);

  Result.Data[0, 0] := XAxis.X;
  Result.Data[0, 1] := YAxis.X;
  Result.Data[0, 2] := ZAxis.X;
  Result.Data[0, 3] := 0.0;

  Result.Data[1, 0] := XAxis.Y;
  Result.Data[1, 1] := YAxis.Y;
  Result.Data[1, 2] := ZAxis.Y;
  Result.Data[1, 3] := 0.0;

  Result.Data[2, 0] := XAxis.Z;
  Result.Data[2, 1] := YAxis.Z;
  Result.Data[2, 2] := ZAxis.Z;
  Result.Data[2, 3] := 0.0;

  Result.Data[3, 0] := -XAxis.Dot(Origin);
  Result.Data[3, 1] := -YAxis.Dot(Origin);
  Result.Data[3, 2] := -ZAxis.Dot(Origin);
  Result.Data[3, 3] := 1.0;
end;

class function TMatrix4f.PerspectiveFOVY(const FieldOfView, AspectRatio, MinRange,
  MaxRange: VectorFloat; const NegativeDepthRange: Boolean): TMatrix4f;
var
  XScale, YScale: VectorFloat;
begin
  Result := ZeroMatrix4f;

  YScale := Cot(FieldOfView * 0.5);
  XScale := YScale / AspectRatio;

  Result.Data[0, 0] := XScale;
  Result.Data[1, 1] := YScale;
  Result.Data[2, 3] := 1.0;

  if NegativeDepthRange then
  begin
    Result.Data[2, 2] := (MinRange + MaxRange) / (MaxRange - MinRange);
    Result.Data[3, 2] := -2.0 * MinRange * MaxRange / (MaxRange - MinRange);
  end
  else
  begin
    Result.Data[2, 2] := MaxRange / (MaxRange - MinRange);
    Result.Data[3, 2] := -MinRange * Result.Data[2, 2];
  end;
end;

class function TMatrix4f.PerspectiveFOVX(const FieldOfView, AspectRatio, MinRange,
  MaxRange: VectorFloat; const NegativeDepthRange: Boolean): TMatrix4f;
var
  XScale, YScale: VectorFloat;
begin
  Result := ZeroMatrix4f;

  XScale := Cot(FieldOfView * 0.5);
  YScale := XScale / AspectRatio;

  Result.Data[0, 0] := XScale;
  Result.Data[1, 1] := YScale;
  Result.Data[2, 3] := 1.0;

  if NegativeDepthRange then
  begin
    Result.Data[2, 2] := (MinRange + MaxRange) / (MaxRange - MinRange);
    Result.Data[3, 2] := -2.0 * MinRange * MaxRange / (MaxRange - MinRange);
  end
  else
  begin
    Result.Data[2, 2] := MaxRange / (MaxRange - MinRange);
    Result.Data[3, 2] := -MinRange * Result.Data[2, 2];
  end;
end;

class function TMatrix4f.PerspectiveVOL(const Width, Height, MinRange, MaxRange: VectorFloat): TMatrix4f;
begin
  Result := ZeroMatrix4f;

  Result.Data[0, 0] := (2.0 * MinRange) / Width;
  Result.Data[1, 1] := (2.0 * MinRange) / Height;
  Result.Data[2, 2] := MaxRange / (MaxRange - MinRange);
  Result.Data[2, 3] := 1.0;
  Result.Data[3, 2] := MinRange * (MinRange - MaxRange);
end;

class function TMatrix4f.PerspectiveBDS(const Left, Right, Top, Bottom, MinRange,
  MaxRange: VectorFloat): TMatrix4f;
begin
  Result := ZeroMatrix4f;

  Result.Data[0, 0] := (2.0 * MinRange) / (Right - Left);
  Result.Data[1, 1] := (2.0 * MinRange) / (Top - Bottom);

  Result.Data[2, 0] := (Left + Right) / (Left - Right);
  Result.Data[2, 1] := (Top + Bottom) / (Bottom - Top);
  Result.Data[2, 2] := MaxRange / (MaxRange - MinRange);
  Result.Data[2, 3] := 1.0;
  Result.Data[3, 2] := MinRange * MaxRange / (MinRange - MaxRange);
end;

class function TMatrix4f.OrthogonalVOL(const Width, Height, MinRange, MaxRange: VectorFloat): TMatrix4f;
begin
  Result := ZeroMatrix4f;

  Result.Data[0, 0] := 2.0 / Width;
  Result.Data[1, 1] := 2.0 / Height;
  Result.Data[2, 2] := 1.0 / (MaxRange - MinRange);
  Result.Data[3, 1] := MinRange / (MinRange - MaxRange);
  Result.Data[3, 3] := 1.0;
end;

class function TMatrix4f.OrthogonalBDS(const Left, Right, Top, Bottom, MinRange,
  MaxRange: VectorFloat): TMatrix4f;
begin
  Result := ZeroMatrix4f;

  Result.Data[0, 0] := 2.0 / (Right - Left);
  Result.Data[1, 1] := 2.0 / (Top - Bottom);
  Result.Data[2, 2] := 1.0 / (MaxRange - MinRange);
  Result.Data[3, 0] := (Left + Right) / (Left - Right);
  Result.Data[3, 1] := (Top + Bottom) / (Bottom - Top);
  Result.Data[3, 2] := MinRange / (MinRange - MaxRange);
  Result.Data[3, 3] := 1.0;
end;

{$ENDREGION}
{$REGION 'TQuaternion functions'}

class operator TQuaternion.Multiply(const Quaternion1, Quaternion2: TQuaternion): TQuaternion;
begin
  Result.X := Quaternion2.W * Quaternion1.X + Quaternion2.X * Quaternion1.W + Quaternion2.Z * Quaternion1.Y -
    Quaternion2.Y * Quaternion1.Z;
  Result.Y := Quaternion2.W * Quaternion1.Y + Quaternion2.Y * Quaternion1.W + Quaternion2.X * Quaternion1.Z -
    Quaternion2.Z * Quaternion1.X;
  Result.Z := Quaternion2.W * Quaternion1.Z + Quaternion2.Z * Quaternion1.W + Quaternion2.Y * Quaternion1.X -
    Quaternion2.X * Quaternion1.Y;
  Result.W := Quaternion2.W * Quaternion1.W - Quaternion2.X * Quaternion1.X - Quaternion2.Y * Quaternion1.Y -
    Quaternion2.Z * Quaternion1.Z;
end;

class operator TQuaternion.Implicit(const Quaternion: TQuaternion): TMatrix4f;
begin
  Result.Data[0, 0] := 1.0 - 2.0 * Quaternion.Y * Quaternion.Y - 2.0 * Quaternion.Z * Quaternion.Z;
  Result.Data[0, 1] := 2.0 * Quaternion.X * Quaternion.Y + 2.0 * Quaternion.W * Quaternion.Z;
  Result.Data[0, 2] := 2.0 * Quaternion.X * Quaternion.Z - 2.0 * Quaternion.W * Quaternion.Y;
  Result.Data[0, 3] := 0.0;
  Result.Data[1, 0] := 2.0 * Quaternion.X * Quaternion.Y - 2.0 * Quaternion.W * Quaternion.Z;
  Result.Data[1, 1] := 1.0 - 2.0 * Quaternion.X * Quaternion.X - 2 * Quaternion.Z * Quaternion.Z;
  Result.Data[1, 2] := 2.0 * Quaternion.Y * Quaternion.Z + 2.0 * Quaternion.W * Quaternion.X;
  Result.Data[1, 3] := 0.0;
  Result.Data[2, 0] := 2.0 * Quaternion.X * Quaternion.Z + 2.0 * Quaternion.W * Quaternion.Y;
  Result.Data[2, 1] := 2.0 * Quaternion.Y * Quaternion.Z - 2.0 * Quaternion.W * Quaternion.X;
  Result.Data[2, 2] := 1.0 - 2.0 * Quaternion.X * Quaternion.X - 2.0 * Quaternion.Y * Quaternion.Y;
  Result.Data[2, 3] := 0.0;
  Result.Data[3, 0] := 0.0;
  Result.Data[3, 1] := 0.0;
  Result.Data[3, 2] := 0.0;
  Result.Data[3, 3] := 1.0;
end;

class operator TQuaternion.Explicit(const Matrix: TMatrix4f): TQuaternion;
var
  MaxValue, HighValue, MultValue: VectorFloat;
  TempQuat: TQuaternion;
  Index: Integer;
begin
  // Determine wich of W, X, Y, Z has the largest absolute value.
  TempQuat.X := Matrix.Data[0, 0] - Matrix.Data[1, 1] - Matrix.Data[2, 2];
  TempQuat.Y := Matrix.Data[1, 1] - Matrix.Data[0, 0] - Matrix.Data[2, 2];
  TempQuat.Z := Matrix.Data[2, 2] - Matrix.Data[0, 0] - Matrix.Data[1, 1];
  TempQuat.W := Matrix.Data[0, 0] + Matrix.Data[1, 1] + Matrix.Data[2, 2];

  Index := 0;
  MaxValue := TempQuat.W;
  if TempQuat.X > MaxValue then
  begin
    MaxValue := TempQuat.X;
    Index := 1;
  end;
  if TempQuat.Y > MaxValue then
  begin
    MaxValue := TempQuat.Y;
    Index := 2;
  end;
  if TempQuat.Z > MaxValue then
  begin
    MaxValue := TempQuat.Z;
    Index := 3;
  end;

  // Perform square root and division.
  HighValue := Sqrt(MaxValue + 1.0) * 0.5;
  MultValue := 0.25 / HighValue;

  // Apply table to compute quaternion values.
  case Index of
    0:
      begin
        Result.W := HighValue;
        Result.X := (Matrix.Data[1, 2] - Matrix.Data[2, 1]) * MultValue;
        Result.Y := (Matrix.Data[2, 0] - Matrix.Data[0, 2]) * MultValue;
        Result.Z := (Matrix.Data[0, 1] - Matrix.Data[1, 0]) * MultValue;
      end;
    1:
      begin
        Result.X := HighValue;
        Result.W := (Matrix.Data[1, 2] - Matrix.Data[2, 1]) * MultValue;
        Result.Z := (Matrix.Data[2, 0] + Matrix.Data[0, 2]) * MultValue;
        Result.Y := (Matrix.Data[0, 1] + Matrix.Data[1, 0]) * MultValue;
      end;
    2:
      begin
        Result.Y := HighValue;
        Result.Z := (Matrix.Data[1, 2] + Matrix.Data[2, 1]) * MultValue;
        Result.W := (Matrix.Data[2, 0] - Matrix.Data[0, 2]) * MultValue;
        Result.X := (Matrix.Data[0, 1] + Matrix.Data[1, 0]) * MultValue;
      end;
  else
    begin
      Result.Z := HighValue;
      Result.Y := (Matrix.Data[1, 2] + Matrix.Data[2, 1]) * MultValue;
      Result.X := (Matrix.Data[2, 0] + Matrix.Data[0, 2]) * MultValue;
      Result.W := (Matrix.Data[0, 1] - Matrix.Data[1, 0]) * MultValue;
    end;
  end;
end;

function TQuaternion.Length: VectorFloat;
begin
  Result := Sqrt((X * X) + (Y * Y) + (Z * Z) + (W * W));
end;

function TQuaternion.Normalize: TQuaternion;
var
  Amp, InvMag: VectorFloat;
begin
  Amp := Length;
  if Amp > VectorEpsilon then
  begin
    InvMag := 1.0 / Amp;
    Result.X := X * InvMag;
    Result.Y := Y * InvMag;
    Result.Z := Z * InvMag;
    Result.W := W * InvMag;
  end
  else
    Result := Self;
end;

function TQuaternion.Angle: VectorFloat;
begin
  Result := ArcCos(W) * 2.0;
end;

function TQuaternion.Axis: TVector3f;
var
  Temp1, Temp2: VectorFloat;
begin
  Temp1 := 1.0 - (W * W);
  if Temp1 <= 0.0 then
  begin
    Result := AxisYVector3f;
    Exit;
  end;

  Temp2 := 1.0 / Sqrt(Temp1);

  Result.X := X * Temp2;
  Result.Y := Y * Temp2;
  Result.Z := Z * Temp2;
end;

function TQuaternion.Conjugate: TQuaternion;
begin
  Result.X := -X;
  Result.Y := -Y;
  Result.Z := -Z;
  Result.W := W;
end;

function TQuaternion.Exponentiate(const Exponent: VectorFloat): TQuaternion;
var
  CosNewAlpha, SinNewAlpha: {$IFDEF DELPHI_LEGACY}Extended{$ELSE}VectorFloat{$ENDIF};
  Alpha, NewAlpha, CompMult: VectorFloat;
begin
  if Abs(W) > 1.0 - VectorEpsilon then
  begin
    Result := Self;
    Exit;
  end;

  Alpha := ArcCos(W);
  NewAlpha := Alpha * Exponent;

  SinCos(NewAlpha, SinNewAlpha, CosNewAlpha);

  CompMult := SinNewAlpha / Sin(Alpha);

  Result.X := X * CompMult;
  Result.Y := Y * CompMult;
  Result.Z := Z * CompMult;
  Result.W := CosNewAlpha;
end;

function TQuaternion.Dot(const Quaternion: TQuaternion): VectorFloat;
begin
  Result := (X * Quaternion.X) + (Y * Quaternion.Y) + (Z * Quaternion.Z) + (W * Quaternion.W);
end;

function TQuaternion.Slerp(const Quaternion: TQuaternion; const Theta: VectorFloat): TQuaternion;
var
  TempQuat: TQuaternion;
  SinOmega, CosOmega, Omega, Kappa1, Kappa2: VectorFloat;
  OneOverSinOmega: VectorFloat;
begin
  if Theta <= 0.0 then
  begin
    Result := Self;
    Exit;
  end
  else if Theta >= 1.0 then
  begin
    Result := Quaternion;
    Exit;
  end;

  CosOmega := Dot(Quaternion);
  TempQuat := Self;

  if CosOmega < 0.0 then
  begin
    TempQuat.X := -TempQuat.X;
    TempQuat.Y := -TempQuat.Y;
    TempQuat.Z := -TempQuat.Z;
    TempQuat.W := -TempQuat.W;
    CosOmega := -CosOmega;
  end;

  if CosOmega > 1.0 - VectorEpsilon then
  begin
    Kappa1 := 1.0 - Theta;
    Kappa2 := Theta;
  end
  else
  begin
    SinOmega := Sqrt(1.0 - CosOmega * CosOmega);
    Omega := ArcTan2(SinOmega, CosOmega);

    OneOverSinOmega := 1.0 / SinOmega;

    Kappa1 := Sin((1.0 - Theta) * Omega) * OneOverSinOmega;
    Kappa2 := Sin(Theta * Omega) * OneOverSinOmega;
  end;

  Result.Z := Kappa1 * Z + Kappa2 * TempQuat.X;
  Result.Y := Kappa1 * Y + Kappa2 * TempQuat.Y;
  Result.Z := Kappa1 * Z + Kappa2 * TempQuat.Z;
  Result.W := Kappa1 * W + Kappa2 * TempQuat.W;
end;

class function TQuaternion.RotateX(const Angle: VectorFloat): TQuaternion;
var
  SinValue, CosValue: {$IFDEF DELPHI_LEGACY}Extended{$ELSE}VectorFloat{$ENDIF};
begin
  SinCos(Angle * 0.5, SinValue, CosValue);

  Result.X := SinValue;
  Result.Y := 0.0;
  Result.Z := 0.0;
  Result.W := CosValue;
end;

class function TQuaternion.RotateY(const Angle: VectorFloat): TQuaternion;
var
  SinValue, CosValue: {$IFDEF DELPHI_LEGACY}Extended{$ELSE}VectorFloat{$ENDIF};
begin
  SinCos(Angle * 0.5, SinValue, CosValue);

  Result.X := 0.0;
  Result.Y := SinValue;
  Result.Z := 0.0;
  Result.W := CosValue;
end;

class function TQuaternion.RotateZ(const Angle: VectorFloat): TQuaternion;
var
  SinValue, CosValue: {$IFDEF DELPHI_LEGACY}Extended{$ELSE}VectorFloat{$ENDIF};
begin
  SinCos(Angle * 0.5, SinValue, CosValue);

  Result.X := 0.0;
  Result.Y := 0.0;
  Result.Z := SinValue;
  Result.W := CosValue;
end;

class function TQuaternion.Rotate(const Axis: TVector3f; const Angle: VectorFloat): TQuaternion;
var
  SinValue, CosValue: {$IFDEF DELPHI_LEGACY}Extended{$ELSE}VectorFloat{$ENDIF};
  Amp, SinDivAmp: VectorFloat;
begin
  Amp := Axis.Length;
  if Amp > VectorEpsilon then
  begin
    SinCos(Angle * 0.5, SinValue, CosValue);
    SinDivAmp := SinValue / Amp;

    Result.X := Axis.X * SinDivAmp;
    Result.Y := Axis.Y * SinDivAmp;
    Result.Z := Axis.Z * SinDivAmp;
    Result.W := CosValue;
  end
  else
    Result := IdentityQuaternion;
end;

class function TQuaternion.RotateObjectToIntertial(const Heading, Pitch, Bank: VectorFloat): TQuaternion;
var
  SinPitch, SinBank, SinHeading: {$IFDEF DELPHI_LEGACY}Extended{$ELSE}VectorFloat{$ENDIF};
  CosPitch, CosBank, CosHeading: {$IFDEF DELPHI_LEGACY}Extended{$ELSE}VectorFloat{$ENDIF};
begin
  SinCos(Heading * 0.5, SinHeading, CosHeading);
  SinCos(Pitch * 0.5, SinPitch, CosPitch);
  SinCos(Bank * 0.5, SinBank, CosBank);

  Result.X := CosHeading * SinPitch * CosBank + SinHeading * CosPitch * SinBank;
  Result.Y := -CosHeading * SinPitch * SinBank + SinHeading * CosPitch * CosBank;
  Result.Z := -SinHeading * SinPitch * CosBank + CosHeading * CosPitch * SinBank;
  Result.W := CosHeading * CosPitch * CosBank + SinHeading * SinPitch * SinBank;
end;

class function TQuaternion.RotateInertialToObject(const Heading, Pitch, Bank: VectorFloat): TQuaternion;
var
  SinPitch, SinBank, SinHeading: {$IFDEF DELPHI_LEGACY}Extended{$ELSE}VectorFloat{$ENDIF};
  CosPitch, CosBank, CosHeading: {$IFDEF DELPHI_LEGACY}Extended{$ELSE}VectorFloat{$ENDIF};
begin
  SinCos(Heading * 0.5, SinHeading, CosHeading);
  SinCos(Pitch * 0.5, SinPitch, CosPitch);
  SinCos(Bank * 0.5, SinBank, CosBank);

  Result.X := -CosHeading * SinPitch * CosBank - SinHeading * CosPitch * SinBank;
  Result.Y := CosHeading * SinPitch * SinBank - SinHeading * CosPitch * CosBank;
  Result.Z := SinHeading * SinPitch * CosBank - CosHeading * CosPitch * SinBank;
  Result.W := CosHeading * CosPitch * CosBank + SinHeading * SinPitch * SinBank;
end;

{$ENDREGION}
{$REGION 'TIntRect'}

class operator TIntRect.Equal(const Rect1, Rect2: TIntRect): Boolean;
begin
  Result := (Rect1.TopLeft = Rect2.TopLeft) and (Rect1.Size = Rect2.Size);
end;

class operator TIntRect.NotEqual(const Rect1, Rect2: TIntRect): Boolean;
begin
  Result := not (Rect1 = Rect2);
end;

function TIntRect.GetRight: VectorInt;
begin
  Result := Left + Width;
end;

procedure TIntRect.SetRight(const Value: VectorInt);
begin
  Width := Value - Left;
end;

function TIntRect.GetBottom: VectorInt;
begin
  Result := Top + Height;
end;

procedure TIntRect.SetBottom(const Value: VectorInt);
begin
  Height := Value - Top;
end;

function TIntRect.GetBottomRight: TPoint2i;
begin
  Result.X := GetRight;
  Result.Y := GetBottom;
end;

procedure TIntRect.SetBottomRight(const Value: TPoint2i);
begin
  SetRight(Value.X);
  SetBottom(Value.Y);
end;

function TIntRect.Empty: Boolean;
begin
  Result := (Width <= 0) or (Height <= 0);
end;

function TIntRect.Contains(const Point: TPoint2i): Boolean;
begin
  Result := (Point.X >= Left) and (Point.X < Right) and (Point.Y >= Top) and (Point.Y < Bottom);
end;

function TIntRect.Contains(const Rect: TIntRect): Boolean;
begin
  Result := (Rect.Left >= Left) and (Rect.Right <= Right) and (Rect.Top >= Top) and (Rect.Bottom <= Bottom);
end;

function TIntRect.Overlaps(const Rect: TIntRect): Boolean;
begin
  Result := (Rect.Left < Right) and (Rect.Right > Left) and (Rect.Top < Bottom) and (Rect.Bottom > Top);
end;

function TIntRect.Intersect(const Rect: TIntRect): TIntRect;
begin
  if Overlaps(Rect) then
    Result := IntRectBDS(
      Max(Left, Rect.Left), Max(Top, Rect.Top),
      Min(Right, Rect.Right), Min(Bottom, Rect.Bottom))
  else
    Result := ZeroIntRect;
end;

function TIntRect.Union(const Rect: TIntRect): TIntRect;
begin
  Result := IntRectBDS(
    Min(Left, Rect.Left), Min(Top, Rect.Top),
    Max(Right, Rect.Right), Max(Bottom, Rect.Bottom));
end;

function TIntRect.Offset(const Delta: TPoint2i): TIntRect;
begin
  Result := IntRectBDS(TopLeft + Delta, BottomRight + Delta);
end;

function TIntRect.Offset(const DeltaX, DeltaY: VectorInt): TIntRect;
begin
  Result := Offset(Point2i(DeltaX, DeltaY));
end;

function TIntRect.Inflate(const Delta: TPoint2i): TIntRect;
begin
  Result := IntRectBDS(TopLeft - Delta, BottomRight + Delta);
end;

function TIntRect.Inflate(const DeltaX, DeltaY: VectorInt): TIntRect;
begin
  Result := Inflate(Point2i(DeltaX, DeltaY));
end;

function TIntRect.Inflate(const Delta: VectorInt): TIntRect;
begin
  Result := Inflate(Point2i(Delta, Delta));
end;

class function TIntRect.ClipCoords(const SourceSize, DestSize: TPoint2i; var SourceRect: TIntRect;
  var DestPos: TPoint2i): Boolean;
var
  Delta: VectorInt;
begin
  if SourceRect.Left < 0 then
  begin
    Delta := -SourceRect.Left;
    Inc(SourceRect.Left, Delta);
    Inc(DestPos.X, Delta);
  end;

  if SourceRect.Top < 0 then
  begin
    Delta := -SourceRect.Top;
    Inc(SourceRect.Top, Delta);
    Inc(DestPos.Y, Delta);
  end;

  if SourceRect.Right > SourceSize.X then
    SourceRect.Right := SourceSize.X;

  if SourceRect.Bottom > SourceSize.Y then
    SourceRect.Bottom := SourceSize.Y;

  if DestPos.X < 0 then
  begin
    Delta := -DestPos.X;
    Inc(DestPos.X, Delta);
    Inc(SourceRect.Left, Delta);
  end;

  if DestPos.Y < 0 then
  begin
    Delta := -DestPos.Y;
    Inc(DestPos.Y, Delta);
    Inc(SourceRect.Top, Delta);
  end;

  if DestPos.X + SourceRect.Width > DestSize.X then
  begin
    Delta := DestPos.X + SourceRect.Width - DestSize.X;
    Dec(SourceRect.Width, Delta);
  end;

  if DestPos.Y + SourceRect.Height > DestSize.Y then
  begin
    Delta := DestPos.Y + SourceRect.Height - DestSize.Y;
    Dec(SourceRect.Height, Delta);
  end;

  Result := not SourceRect.Empty;
end;

function IntRect(const Left, Top, Width, Height: VectorInt): TIntRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Width := Width;
  Result.Height := Height;
end;

function IntRect(const Origin, Size: TPoint2i): TIntRect;
begin
  Result.TopLeft := Origin;
  Result.Size := Size;
end;

function IntRectBDS(const Left, Top, Right, Bottom: VectorInt): TIntRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Right;
  Result.Bottom := Bottom;
end;

function IntRectBDS(const TopLeft, BottomRight: TPoint2i): TIntRect;
begin
  Result.TopLeft := TopLeft;
  Result.BottomRight := BottomRight;
end;

{$ENDREGION}
{$REGION 'TFloatRect'}

class operator TFloatRect.Implicit(const ARect: TIntRect): TFloatRect;
begin
  Result.Left := ARect.Left;
  Result.Top := ARect.Top;
  Result.Width := ARect.Width;
  Result.Height := ARect.Height;
end;

class operator TFloatRect.Equal(const ARect1, ARect2: TFloatRect): Boolean;
begin
  Result := (ARect1.TopLeft = ARect2.TopLeft) and (ARect1.BottomRight = ARect2.BottomRight);
end;

class operator TFloatRect.NotEqual(const ARect1, ARect2: TFloatRect): Boolean;
begin
  Result := not (ARect1 = ARect2);
end;

class operator TFloatRect.Multiply(const ARect: TFloatRect; const AScale: VectorFloat): TFloatRect;
begin
  Result.Left := ARect.Left * AScale;
  Result.Top := ARect.Top * AScale;
  Result.Width := ARect.Width * AScale;
  Result.Height := ARect.Height * AScale;
end;

function TFloatRect.GetRight: VectorFloat;
begin
  Result := Left + Width;
end;

procedure TFloatRect.SetRight(const Value: VectorFloat);
begin
  Width := Value - Left;
end;

function TFloatRect.GetBottom: VectorFloat;
begin
  Result := Top + Height;
end;

procedure TFloatRect.SetBottom(const Value: VectorFloat);
begin
  Height := Value - Top;
end;

function TFloatRect.GetBottomRight: TPoint2f;
begin
  Result.X := GetRight;
  Result.Y := GetBottom;
end;

procedure TFloatRect.SetBottomRight(const Value: TPoint2f);
begin
  SetRight(Value.X);
  SetBottom(Value.Y);
end;

function TFloatRect.Empty: Boolean;
begin
  Result := (Width < VectorEpsilon) or (Height < VectorEpsilon);
end;

function TFloatRect.Contains(const Point: TPoint2f): Boolean;
begin
  Result := (Point.X >= Left) and (Point.X < Right) and (Point.Y >= Top) and (Point.Y < Bottom);
end;

function TFloatRect.Contains(const Rect: TFloatRect): Boolean;
begin
  Result := (Rect.Left >= Left) and (Rect.Right <= Right) and (Rect.Top >= Top) and (Rect.Bottom <= Bottom);
end;

function TFloatRect.Overlaps(const Rect: TFloatRect): Boolean;
begin
  Result := (Rect.Left < Right) and (Rect.Right > Left) and (Rect.Top < Bottom) and (Rect.Bottom > Top);
end;

function TFloatRect.Intersect(const Rect: TFloatRect): TFloatRect;
begin
  if Overlaps(Rect) then
    Result := FloatRectBDS(
      Max(Left, Rect.Left), Max(Top, Rect.Top),
      Min(Right, Rect.Right), Min(Bottom, Rect.Bottom))
  else
    Result := ZeroFloatRect;
end;

function TFloatRect.Union(const Rect: TFloatRect): TFloatRect;
begin
  Result := FloatRectBDS(
    Min(Left, Rect.Left), Min(Top, Rect.Top),
    Max(Right, Rect.Right), Max(Bottom, Rect.Bottom));
end;

function TFloatRect.Offset(const Delta: TPoint2f): TFloatRect;
begin
  Result := FloatRectBDS(TopLeft + Delta, BottomRight + Delta);
end;

function TFloatRect.Offset(const DeltaX, DeltaY: VectorFloat): TFloatRect;
begin
  Result := Offset(Point2f(DeltaX, DeltaY));
end;

function TFloatRect.Inflate(const Delta: TPoint2f): TFloatRect;
begin
  Result := FloatRectBDS(TopLeft - Delta, BottomRight + Delta);
end;

function TFloatRect.Inflate(const DeltaX, DeltaY: VectorFloat): TFloatRect;
begin
  Result := Inflate(Point2f(DeltaX, DeltaY));
end;

function TFloatRect.Inflate(const Delta: VectorFloat): TFloatRect;
begin
  Result := Inflate(Point2f(Delta, Delta));
end;

function TFloatRect.InnerBounds: TIntRect;
begin
  Result.TopLeft := Point2i(Floor(Left), Floor(Top));
  Result.Width := Ceil(Right) - Result.Left;
  Result.Height := Ceil(Bottom) - Result.Top;
end;

function TFloatRect.OuterBounds: TIntRect;
begin
  Result.TopLeft := Point2i(Ceil(Left), Ceil(Top));
  Result.Width := Floor(Right) - Result.Left;
  Result.Height := Floor(Bottom) - Result.Top;
end;

class function TFloatRect.ClipCoords(const SourceSize, DestSize: TPoint2f; var SourceRect,
  DestRect: TFloatRect): Boolean;
var
  Delta: VectorFloat;
  Scale: TPoint2f;
begin
  if SourceRect.Empty or DestRect.Empty then
  begin
    Result := False;
    Exit;
  end;

  Scale.X := DestRect.Width / SourceRect.Width;
  Scale.Y := DestRect.Height / SourceRect.Height;

  if SourceRect.Left < 0 then
  begin
    Delta := -SourceRect.Left;
    SourceRect.Left := SourceRect.Left + Delta;
    DestRect.Left := DestRect.Left + (Delta * Scale.X);
  end;

  if SourceRect.Top < 0 then
  begin
    Delta := -SourceRect.Top;
    SourceRect.Top := SourceRect.Top + Delta;
    DestRect.Top := DestRect.Top + (Delta * Scale.Y);
  end;

  if SourceRect.Right > SourceSize.X then
  begin
    Delta := SourceRect.Right - SourceSize.X;
    SourceRect.Right := SourceRect.Right - Delta;
    DestRect.Right := DestRect.Right - (Delta * Scale.X);
  end;

  if SourceRect.Bottom > SourceSize.Y then
  begin
    Delta := SourceRect.Bottom - SourceSize.Y;
    SourceRect.Bottom := SourceRect.Bottom - Delta;
    DestRect.Bottom := DestRect.Bottom - (Delta * Scale.Y);
  end;

  if DestRect.Left < 0 then
  begin
    Delta := -DestRect.Left;
    DestRect.Left := DestRect.Left + Delta;
    SourceRect.Left := SourceRect.Left + (Delta / Scale.X);
  end;

  if DestRect.Top < 0 then
  begin
    Delta := -DestRect.Top;
    DestRect.Top := DestRect.Top + Delta;
    SourceRect.Top := SourceRect.Top + (Delta / Scale.Y);
  end;

  if DestRect.Right > DestSize.X then
  begin
    Delta := DestRect.Right - DestSize.X;
    DestRect.Right := DestRect.Right - Delta;
    SourceRect.Right := SourceRect.Right - (Delta / Scale.X);
  end;

  if DestRect.Bottom > DestSize.Y then
  begin
    Delta := DestRect.Bottom - DestSize.Y;
    DestRect.Bottom := DestRect.Bottom - Delta;
    SourceRect.Bottom := SourceRect.Bottom - (Delta / Scale.Y);
  end;

  Result := (not SourceRect.Empty) and (not DestRect.Empty);
end;

function FloatRect(const Left, Top, Width, Height: VectorFloat): TFloatRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Width := Width;
  Result.Height := Height;
end;

function FloatRect(const Origin, Size: TPoint2f): TFloatRect;
begin
  Result.TopLeft := Origin;
  Result.Size := Size;
end;

function FloatRectBDS(const Left, Top, Right, Bottom: VectorFloat): TFloatRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Right;
  Result.Bottom := Bottom;
end;

function FloatRectBDS(const TopLeft, BottomRight: TPoint2f): TFloatRect;
begin
  Result.TopLeft := TopLeft;
  Result.BottomRight := BottomRight;
end;

{$ENDREGION}
{$REGION 'TQuad'}

class operator TQuad.Equal(const Rect1, Rect2: TQuad): Boolean;
begin
  Result := (Rect1.TopLeft = Rect2.TopLeft) and (Rect1.TopRight = Rect2.TopRight) and
    (Rect1.BottomRight = Rect2.BottomRight) and (Rect1.BottomLeft = Rect2.BottomLeft);
end;

class operator TQuad.NotEqual(const Rect1, Rect2: TQuad): Boolean;
begin
  Result := not (Rect1 = Rect2);
end;

function TQuad.Scale(const Scale: VectorFloat; const Centered: Boolean): TQuad;
var
  Center: TPoint2f;
begin
  if Abs(Scale - 1.0) <= VectorEpsilon then
  begin
    Result := Self;
    Exit;
  end;

  if Centered then
  begin
    Center := (TopLeft + TopRight + BottomRight + BottomLeft) * 0.25;
    Result.TopLeft := Center.Lerp(TopLeft, Scale);
    Result.TopRight := Center.Lerp(TopRight, Scale);
    Result.BottomRight := Center.Lerp(BottomRight, Scale);
    Result.BottomLeft := Center.Lerp(BottomLeft, Scale);
  end
  else
  begin
    Result.TopLeft := TopLeft * Scale;
    Result.TopRight := TopRight * Scale;
    Result.BottomRight := BottomRight * Scale;
    Result.BottomLeft := BottomLeft * Scale;
  end;
end;

function TQuad.Mirror: TQuad;
begin
  Result.TopLeft := TopRight;
  Result.TopRight := TopLeft;
  Result.BottomRight := BottomLeft;
  Result.BottomLeft := BottomRight;
end;

function TQuad.Flip: TQuad;
begin
  Result.TopLeft := BottomLeft;
  Result.TopRight := BottomRight;
  Result.BottomRight := TopRight;
  Result.BottomLeft := TopLeft;
end;

function TQuad.Transform(const Matrix: TMatrix3f): TQuad;
begin
  Result.TopLeft := TopLeft * Matrix;
  Result.TopRight := TopRight * Matrix;
  Result.BottomRight := BottomRight * Matrix;
  Result.BottomLeft := BottomLeft * Matrix;
end;

function TQuad.Offset(const Delta: TPoint2f): TQuad;
begin
  Result.TopLeft := TopLeft + Delta;
  Result.TopRight := TopRight + Delta;
  Result.BottomRight := BottomRight + Delta;
  Result.BottomLeft := BottomLeft + Delta;
end;

function TQuad.Offset(const DeltaX, DeltaY: VectorFloat): TQuad;
begin
  Result := Offset(Point2f(DeltaX, DeltaY));
end;

function TQuad.Contains(const Point: TPoint2f): Boolean;
begin
  Result :=
    Point.InsideTriangle(Values[0], Values[1], Values[2]) or
    Point.InsideTriangle(Values[2], Values[3], Values[0]);
end;

class function TQuad.Scaled(const Left, Top, Width, Height, Scale: VectorFloat;
  const Centered: Boolean): TQuad;
var
  NewLeft, NewTop, NewWidth, NewHeight: VectorFloat;
begin
  if Abs(Scale - 1.0) <= VectorEpsilon then
  begin
    Result := Quad(Left, Top, Width, Height);
    Exit;
  end;

  if Centered then
  begin
    NewWidth := Width * Scale;
    NewHeight := Height * Scale;
    NewLeft := Left + (Width - NewWidth) * 0.5;
    NewTop := Top + (Height - NewHeight) * 0.5;

    Result := Quad(NewLeft, NewTop, NewWidth, NewHeight);
  end
  else
    Result := Quad(Left, Top, Width * Scale, Height * Scale);
end;

class function TQuad.Rotated(const RotationOrigin, Size, RotationCenter: TPoint2f; const Angle,
  Scale: VectorFloat): TQuad;
var
  SinAngle, CosAngle: {$IFDEF DELPHI_LEGACY}Extended{$ELSE}VectorFloat{$ENDIF};
  IsScaled: Boolean;
  NewPoint: TPoint2f;
  Index: Integer;
begin
  SinCos(Angle, SinAngle, CosAngle);

  Result := Quad(-RotationCenter.X, -RotationCenter.Y, Size.X, Size.Y);

  IsScaled := Abs(Scale - 1.0) > VectorEpsilon;

  for Index := 0 to High(Result.Values) do
  begin
    if IsScaled then
      Result.Values[Index] := Result.Values[Index] * Scale;

    NewPoint.X := Result.Values[Index].X * CosAngle - Result.Values[Index].Y * SinAngle;
    NewPoint.Y := Result.Values[Index].Y * CosAngle + Result.Values[Index].X * SinAngle;

    Result.Values[Index] := NewPoint + RotationOrigin;
  end;
end;

class function TQuad.Rotated(const RotationOrigin, Size: TPoint2f; const Angle, Scale: VectorFloat): TQuad;
begin
  Result := Rotated(RotationOrigin, Size, Size * 0.5, Angle, Scale);
end;

class function TQuad.RotatedTL(const TopLeft, Size, RotationCenter: TPoint2f; const Angle: VectorFloat;
  const Scale: VectorFloat): TQuad;
begin
  Result := Rotated(TopLeft, Size, RotationCenter, Angle, Scale).Offset(RotationCenter);
end;

function Quad(const TopLeftX, TopLeftY, TopRightX, TopRightY, BottomRightX, BottomRightY, BottomLeftX,
  BottomLeftY: VectorFloat): TQuad;
begin
  Result.TopLeft.X := TopLeftX;
  Result.TopLeft.Y := TopLeftY;
  Result.TopRight.X := TopRightX;
  Result.TopRight.Y := TopRightY;
  Result.BottomRight.X := BottomRightX;
  Result.BottomRight.Y := BottomRightY;
  Result.BottomLeft.X := BottomLeftX;
  Result.BottomLeft.Y := BottomLeftY;
end;

function Quad(const TopLeft, TopRight, BottomRight, BottomLeft: TPoint2f): TQuad;
begin
  Result.TopLeft := TopLeft;
  Result.TopRight := TopRight;
  Result.BottomRight := BottomRight;
  Result.BottomLeft := BottomLeft;
end;

function Quad(const Left, Top, Width, Height: VectorFloat): TQuad;
begin
  Result.TopLeft.X := Left;
  Result.TopLeft.Y := Top;
  Result.TopRight.X := Left + Width;
  Result.TopRight.Y := Top;
  Result.BottomRight.X := Result.TopRight.X;
  Result.BottomRight.Y := Top + Height;
  Result.BottomLeft.X := Left;
  Result.BottomLeft.Y := Result.BottomRight.Y;
end;

function Quad(const Rect: TFloatRect): TQuad;
begin
  Result.TopLeft := Rect.TopLeft;
  Result.TopRight := Point2f(Rect.Right, Rect.Top);
  Result.BottomRight := Rect.BottomRight;
  Result.BottomLeft := Point2f(Rect.Left, Rect.Bottom);
end;

function Quad(const Rect: TIntRect): TQuad;
begin
  Result.TopLeft := Rect.TopLeft;
  Result.TopRight := Point2f(Rect.Right, Rect.Top);
  Result.BottomRight := Rect.BottomRight;
  Result.BottomLeft := Point2f(Rect.Left, Rect.Bottom);
end;

{$ENDREGION}

end.
