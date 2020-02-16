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

type
  // Pointer to @link(TPixelFormat).
  PPixelFormat = ^TPixelFormat;

  { Defines how individual pixels and their colors are encoded in images and textures. The order of letters
    in the constants defines the order of the  encoded components; @bold(R) stands for Red, @bold(G) for
    Green, @bold(B) for Blue, @bold(A) for Alpha, @bold(L) for Luminance and @bold(X) for Not Used (or
    discarded); @bold(F) at the end means floating-point format. }
  TPixelFormat = (
    { Unknown pixel format. It is usually returned when no valid pixel format is available. In some cases,
      it can be specified to indicate that the format should be selected by default or automatically. @br @br }
    Unknown,

    // Unsigned Normalized formats.

    // 8-bit unsigned normalized format with red channel only.
    R8,

    // 16-bit format with 8-bit unsigned normalized red and green channels.
    RG8,

    // 32-bit format with 8-bit unsigned normalized red, green, blue and alpha channels.
    RGBA8,

    // 8-bit unsigned normalized format with red channel only.
    R16,

    // 32-bit format with 8-bit unsigned normalized red and green channels.
    RG16,

    // 64-bit ARGB pixel format with each channel having 16 bits.
    RGBA16,

    // Signed Normalized formats.

    // 8-bit signed normalized format with red channel only.
    R8S,

    // 16-bit format with 8-bit signed normalized red and green channels.
    RG8S,

    // 32-bit format with 8-bit signed normalized red, green, blue and alpha channels.
    RGBA8S,

    // 16-bit signed normalized format with red channel only.
    R16S,

    // 32-bit format with 16-bit signed normalized red and green channels.
    RG16S,

    // 64-bit format with 16-bit signed normalized red, green, blue and alpha channels.
    RGBA16S,

    // Unsigned formats.

    // 8-bit format with 8-bit unsigned integer red channel.
    R8U,

    // 16-bit format with 8-bit unsigned integer red and green channels.
    RG8U,

    // 32-bit format with 8-bit unsigned integer red and green channels.
    RGBA8U,

    // 16-bit format with 16-bit unsigned integer red channel.
    R16U,

    // 32-bit format with 16-bit unsigned integer red and green channels.
    RG16U,

    // 64-bit format with 16-bit unsigned integer red and green channels.
    RGBA16U,

    // 32-bit format with 32-bit unsigned integer red channel.
    R32U,

    // 64-bit format with 32-bit unsigned integer red and green channels.
    RG32U,

    // 128-bit format with 32-bit unsigned integer red and green channels.
    RGBA32U,

    // Signed formats.

    // 8-bit format with 8-bit signed integer red channel.
    R8I,

    // 16-bit format with 8-bit signed integer red and green channels.
    RG8I,

    // 32-bit format with 8-bit signed integer red and green channels.
    RGBA8I,

    // 16-bit format with 16-bit signed integer red channel.
    R16I,

    // 32-bit format with 16-bit signed integer red and green channels.
    RG16I,

    // 64-bit format with 16-bit signed integer red and green channels.
    RGBA16I,

    // 32-bit format with 32-bit signed integer red channel.
    R32I,

    // 64-bit format with 32-bit signed integer red and green channels.
    RG32I,

    // 128-bit format with 16-bit unsigned integer red and green channels.
    RGBA32I,

    // Floating-point formats.

    // 16-bit floating-point pixel format, which has only one component.
    R16F,

    // 32-bit floating-point pixel format containing two components with 16 bits each.
    RG16F,

    // 64-bit floating-point ARGB pixel format with each component having 16 bits.
    RGBA16F,

    // 32-bit floating-point pixel format, which has only one component.
    R32F,

    // 64-bit floating-point pixel format containing two components with 32 bits each.
    RG32F,

    // 128-bit floating-point ARGB pixel format with each component having 32 bits.
    RGBA32F,

    // Specialized floating-point formats.

    // 32-bit format with 11-bit unsigned floating-point red and green channels, and 10-bit blue channel.
    RG11B10F,

    // 32-bit format with 9-bit floating-point red, green and blue channels with shared 5-bit exponent.
    RGB9E5F,

    // Specialized color formats.

    // 32-bit format with 10-bit unsigned normalized red, green and blue channels, and 2-bit alpha-channel.
    RGB10A2,

    // 32-bit format with 10-bit unsigned integer red, green and blue channels, and 2-bit alpha-channel.
    RGB10A2U,

    // 32-bit format with 8-bit unsigned normalized red, green and blue channels (8 bits unused).
    RGBX8,

    // 32-bit format with 8-bit unsigned normalized red, green, blue and alpha channels with sRGB curve.
    RGBA8_SRGB,

    // 32-bit format with 8-bit unsigned normalized blue, green, red and alpha channels.
    BGRA8,

    // 32-bit format with 8-bit unsigned normalized blue, green and red channels (8 bits unused).
    BGRX8,

    // 32-bit format with 8-bit unsigned normalized blue, green, red and alpha channels with sRGB curve.
    BGRA8_SRGB,

    // 32-bit format with 10-bit unsigned normalized blue, green and red channels, and 2-bit alpha-channel.
    BGR10A2,

    // 32-bit format with 10-bit unsigned normalized blue, green and red channels (2 bits unused).
    BGR10X2,

    // 16-bit format with 4-bit unsigned normalized blue, green, red and alpha channels.
    BGRA4,

    // 16-bit format with 4-bit unsigned normalized blue, green and red channels (4 bits unused).
    BGRX4,

    // 16-bit format with 5-bit unsigned normalized blue and red, and 6 bits for green channels.
    B5G6R5,

    // 16-bit format with 5-bit unsigned normalized blue, green and red, 1 bit for alpha channels.
    BGR5A1,

    // 16-bit format with 5-bit unsigned normalized blue, green and red channels (1 bit unused).
    BGR5X1,

    // Storage formats (not hardware accelerated).

    // 24-bit format with 8-bit unsigned normalized red, green and blue channels.
    RGB8,

    // 24-bit format with 8-bit unsigned normalized blue, green and red channels.
    BGR8,

    // Luminance and alpha formats that might require channel swizzling in shader.

    // 8-bit alpha-channel pixel format.
    A8,

    // 8-bit luminance pixel format.
    L8,

    // 16-bit luminance pixel format.
    L16,

    // 8-bit luminance/alpha pixel format with 4 bits per channel.
    LA4,

    // 16-bit luminance/alpha pixel format with 8 bits per channel.
    LA8,

    // Legacy pallette formats.

    // 8-bit palette indexed format.
    I8,

    // Compressed formats.

    // Compressed format with red channel only.
    R_BC,

    // Compressed format with red and green channels.
    RG_BC,

    // Compressed format with red, green and blue channels.
    RGB_BC,

    // Compressed format with red, green, blue and alpha channels.
    RGBA_BC,

    // Compressed format with red, green and blue channels with sRGB curve.
    RGB_BC_SRGB,

    // Compressed format with red, green and blue channels with sRGB curve, and an alpha-channel.
    RGBA_BC_SRGB,

    // Depth/stencil formats.

    // Depth/stencil format with 16-bit (unsigned normalized) depth values and no storage for stencil.
    D16,

    // Depth/stencil format with 24-bit (unsigned normalized) depth values and 8 bits for stencil.
    D24S8,

    // Depth/stencil format with 32-bit (floating-point) depth values and no storage for stencil.
    D32F,

    // Depth/stencil format with 32-bit (floating-point) depth values and 8 bits for stencil.
    D32S8F);

  // Defines how alpha-channel should be handled in the loaded image.
  TAlphaFormatRequest = (
    // Alpha-channel can be handled either way. @br @br
    DontCare,

    { Alpha-channel in the image should not be premultiplied. Under normal circumstances, this is the
      recommended approach as it preserves RGB color information in its original form. However, when using
      mipmapping for images that have alpha-channel, @italic(Premultiplied) gives more accurate
      results. @br @br }
    NonPremultiplied,

    { Alpha-channel in the image should be premultiplied. Under normal circumstances, this is not
      recommended as the image would lose information after RGB components are premultiplied by alpha (and
      for smaller alpha values, less information is preserved). However, when using mipmapping for images
      that have alpha-channel, this gives more accurate results. }
    Premultiplied);

  // Type of device used in configuration.
  TDeviceBackend = (
    // Default device that is defined for the current platform. @br @br
    Default,

    // OpenGL device backend. @br @br
    OpenGL,

    // Direct3D device backend.
    Direct3D);

  // Device attributes that define its behavior.
  TDeviceAttribute = (
    // Device should use vertical sync for tear-free rendering. @br @br
    VSync,

    // Device should use debug-type context that helps to resolve issues. @br @br
    Debug,

    // Device should prefer to use software-based solution instead of GPU acceleration.
    Software,

    // Compatibility device that is meant for older technologies.
    Legacy);

  // One or more device attributes.
  TDeviceAttributes = set of TDeviceAttribute;

  // Type of graphics technology used in application.
  TDeviceTechnology = (
    // The technology has not yet been established. @br @br
    Unknown,

    // Microsoft Direct3D technology. @br @br
    Direct3D,

    // OpenGL by Khronos Group. @br @br
    OpenGL,

    // OpenGL ES by Khronos Group. @br @br
    OpenGL_ES,

    // Software rasterizer. @br @br
    Software,

    // Private proprietary technology.
    Proprietary);

  // Type of triangle face that should have operation applied to.
  TTriangleFace = (
    // Neither back nor front faces should be processed. @br @br
    None,

    // Back faces should be processed. @br @br
    Back,

    // Front faces should be processed. @br @br
    Front,

    // Both back and front faces should be processed.
    Both);

  // Function that defines how depth/stencil operands should be compared.
  TComparisonFunc = (
    // Comparison is always true. @br @br
    Always,

    // Comparison results true when source is less than destination. @br @br
    Less,

    // Comparison results true when source is less than or equal to destination. @br @br
    LessEqual,

    // Comparison results true when source is greater than destination. @br @br
    Greater,

    // Comparison results true when source is greater than or equal to destination. @br @br
    GreaterEqual,

    // Comparison results true when source equals to destination. @br @br
    Equal,

    // Comparison results true when source is not equal to destination. @br @br
    NotEqual,

    // Comparison is always false.
    Never);

  // Operation that should be performed on stencil value.
  TStencilOp = (
    // Preserve destination value. @br @br
    Keep,

    // Set destination value to zero. @br @br
    Zero,

    // Replace destination value with the source one. @br @br
    Replace,

    // Invert the destination value. @br @br
    Invert,

    // Increment the destination value by the source one and clamp, if necessary. @br @br
    Increment,

    // Decrement the destination value by the source one and clamp, if necessary. @br @br
    Decrement,

    // Increment the destination value by the source one with wrap-around. @br @br
    IncrementWarp,

    // Decrement the destination value by the source one with wrap-around. @br @br
    DecrementWarp);

  // Factor that determines how alpha-blending operand is considered.
  TBlendFactor = (
    // Blending factor is a constant that equals to one. @br @br
    One,

    // Blending factor is a constant that equals to zero. @br @br
    Zero,

    // Source color is used as a blending factor. @br @br
    SourceColor,

    // One minus source color is used as a blending factor. @br @br
    InvSourceColor,

    // Source alpha value is used as a blending factor. @br @br
    SourceAlpha,

    // One minus source alpha value is used as a blending factor. @br @br
    InvSourceAlpha,

    // Destination color is used as a blending factor. @br @br
    DestColor,

    // One minus destination color is used as a blending factor. @br @br
    InvDestColor,

    // Destination alpha value is used as a blending factor. @br @br
    DestAlpha,

    // One minus destination alpha value is used as a blending factor. @br @br
    InvDestAlpha,

    // Blending factor is calculated as min(SrcAlpha, InvDestAlpha). @br @br
    SourceAlphaSat,

    // Blending factor is a user-defined color constant. @br @br
    ConstantColor,

    // Blending factor is one minus user-defined color constant. @br @br
    InvConstantColor,

    // Blending factor is an alpha value from user-defined color constant. @br @br
    ConstantAlpha,

    // Blending factor is one minus alpha value from user-defined color constant. @br @br
    InvConstantAlpha,

    // Source color (from 2nd pixel shader output) is used as a blending factor. @br @br
    SourceColor1,

    // One minus source color (from 2nd pixel shader output) is used as a blending factor. @br @br
    InvSourceColor1,

    // Source alpha value (from 2nd pixel shader output) is used as a blending factor. @br @br
    SourceAlpha1,

    // One minus source alpha (from 2nd pixel shader output) value is used as a blending factor.
    InvSourceAlpha1);

  // Operation that should be performed between two blending operands.
  TBlendOp = (
    // Add source and destination values. @br @br
    Add,

    // Subtract source value from destination value. @br @br
    Subtract,

    // Subtract destination value from source value. @br @br
    InvSubtract,

    // Calculate minimum between source and destination values. @br @br
    Minimum,

    // Calculate maximum between source and destination values. @br @br
    Maximum);

  // Type of data that a generic buffer contains.
  TBufferDataType = (
    // Vertex information is stored in the buffer (Vertex Buffer). @br @br
    Vertex,

    // Index information is stored in the buffer (Index Buffer). @br @br
    Index,

    // Shader constant information is stored in the buffer (Constant Buffer).
    Constant);

  // Type of access that is performed with mesh buffer.
  TBufferAccessType = (
    // The contents of buffer are updated occasionally. @br @br
    Default,

    // The contents of buffer are defined during construction and typically remain unchanged. @br @br
    Static,

    // The contents of buffer are changed very frequently.
    Dynamic);

  // Format in which a single value of the given element is represented.
  TElementFormat = (
    // Undefined element format (@link(TVertexElement.Count) will define number of actual bytes). @br @br
    Undefined = 0,

    // 32-bit floating-point format. @br @br
    Float,

    // 16-bit floating-point format. @br @br
    HalfFloat,

    // 64-bit floating-point format. @br @br
    Double,

    // 32-bit signed integer format. @br @br
    Int,

    // 32-bit unsigned integer format. @br @br
    UnsignedInt,

    // 16-bit signed integer format. @br @br
    Short,

    // 16-bit unsigned integer format. @br @br
    UnsignedShort,

    // 8-bit signed integer format. @br @br
    Byte,

    // 8-bit unsigned integer format. @br @br
    UnsignedByte,

    // For internal use (to make 16-bit enumeration).
    ElementFormatEnum = 65535);

  // Rendering primitive topology.
  TPrimitiveTopology = (
    // Unknown (undefined) topology. @br @br
    Unknown,

    // Points or point lists. @br @br
    Points,

    // Lines or line lists. @br @br
    Lines,

    // Line strips. @br @br
    LineStrip,

    // Triangles or triangle lists. @br @br
    Triangles,

    // Triangle strips. @br @br
    TriangleStrip,

    // Lines or line lists with adjacency information. @br @br
    LinesAdjacency,

    // Line strips with adjacency information. @br @br
    LineStripAdjacency,

    // Triangles or triangle lists with adjacency information. @br @br
    TrianglesAdjacency,

    // Triangle strips with adjacency information.
    TriangleStripAdjacency);

  // Type of shader in graphics rendering pipeline.
  TShaderType = (
    // Undefined or unknown shader. @br @br
    Undefined,

    // Fragment (pixel) shader. @br @br
    Fragment,

    // Vertex shader. @br @br
    Vertex,

    // Geometry shader. @br @br
    Geometry,

    // Compute shader.
    Compute);

  // Type that characterizes shader element.
  TShaderElement = (
    // Undefined element type. @br @br
    Undefined,

    // Element is a texture. @br @br
    Texture,

    // Element is a constant buffer (uniform buffer object).
    ConstantBuffer);

  // Compute program texture access type.
  TComputeTextureAccess = (
    // Texture will be read from, no writes allowed.
    Read,

    // Texture will only be written to, no reads allowed.
    Write,

    // Texture will be read from and written to.
    ReadWrite);

  // Filtering mode that defines how colors are sampled from the texture.
  TTextureFilter = (
    // No filtering is performed (applies only to mipmap filter, in which case it gets disabled). @br @br
    None,

    // Nearest integer sample filtering. @br @br
    Nearest,

    // Linear interpolation filtering. @br @br
    Linear,

    // Linear anisotropic filtering.
    Anisotropic);

  // Addressing mode that defines how texture coordinates outside of [0, 1] range are treated.
  TTextureAddress = (
    // Texture coordinates are wrapped; that is, only fractional part is considered. @br @br
    Wrap,

    // Texture coordinates are clamped to stay within [0, 1] range. @br @br
    Clamp,

    // Texture coordinates are mirrored in each odd cycle (e.g. from 1 to 2, from 3 to 4 and so on). @br @br
    Mirror,

    // Texture coordinates are mirrored for one cycle and then clamped. @br @br
    MirrorOnce,

    // When texture coordinates are outside of [0, 1] range, a separate border color will be used instead.
    Border);

  // Type of texture that defines how it is composed.
  TTextureType = (
    // Standard 2D texture surface. @br @br
    Surface,

    // Cube Map consisting of 6 individual 2D faces. @br @br
    CubeMap,

    // Layered 3D volume texture.
    Volume);

  // Blending effect that defines how primitives are rendered on drawing surface.
  TBlendingEffect = (
    { Undefined blending effect: canvas does not change blending parameters configured in the device context
      and uses whatever configuration is currently active. @br @br }
    Undefined = 0,

    // "Normal" blending effect: Cd = (Cs * As) + (Cd * (1 - As). @br @br
    Normal,

    // "Shadow drawing" effect: Cd = Cd * (1 - As). @br @br
    Shadow,

    // Additive blending effect: Cd = (Cs * As) + Cd. @br @br
    Add,

    // Subtractive blending effect: Cd = (Cs * As) - Cd. @br @br
    Subtract,

    // Multiplication blending effect: Cd = Cs * Cd. @br @br
    Multiply,

    // Inverse multiplication effect: Cd = (1 - Cs) * Cd. @br @br
    InverseMultiply,

    // Source color blending effect: Cd = (Cs * Cs) + (Cd * (1 - Cs)). @br @br
    SourceColor,

    // Source color additive blending effect: Cd = (Cs * Cs) + Cd. @br @br
    SourceColorAdd,

    // Alpha-blending disabled, drawing is just a copy operation.
    Copy = 255);

  // Attributes that change the rendering behavior of the canvas.
  TCanvasAttribute = (
    { Transform 2D coordinates as 3D vectors (with Z = 0) by the appropriate 4x4 matrix as if drawing to an
      arbitrary plane in 3D. @br @br }
    Holographic,

    // Preprocess the texture by using its alpha-channel as a signed distance field. @br @br
    SDF,

    // Include outline when rendering signed distance field (requires "SDF" to be set).
    Outline,

    // Sample texture using bicubic interpolation for higher-quality results.
    Cubic,

    // Adjusts texture color using YCH instead of RGB (only works for textures).
    ColorAdjust);

  // Pointer to @link(TCanvasAttributes).
  PCanvasAttributes = ^TCanvasAttributes;

  // One or more attributes that define canvas behavior.
  TCanvasAttributes = set of TCanvasAttribute;

  // Pointer to @link(TSuperSampleSDF).
  PSuperSampleSDF = ^TSuperSampleSDF;

  // Type of super-sampling used for rendering Signed Distance Field (SDF).
  TSuperSampleSDF = (
    { Non-supersampled rendering (for low-performance systems). Note that if outline is enabled, its color
      will be calculated using simplified scheme, where luma is calculated as usual, chroma is calculated
      in a simple fashion and hue is not affected. @br @br }
    NoSuperSample,

    // 4x super-sampled rendering. @br @br
    SuperSample4x,

    // 16x super-sampled rendering.
    SuperSample16x);

  // Pointer to @link(TCanvasContextState).
  PCanvasContextState = ^TCanvasContextState;

  // One of possible defined context states that can be pre-configured by the canvas.
  TCanvasContextState = (
    // Geometric shapes and images rendered in a flat 2D scene. @br @br
    FlatScene,

    // Text rendered in a flat 2D scene. @br @br
    FlatText,

    // Geometric shapes, images and text rendered in a 3D surface projection.
    Projected,

    // Undefined context state.
    Undefined = 255);

  // Weight of the font, or letter "thickness".
  TFontWeight = (
    // Letters should appear as thin as technically possible. @br @br
    Thin,

    // Letters should appear very thin. @br @br
    ExtraLight,

    // Letters should appear even thinner. @br @br
    Light,

    // Letters should have slightly lighter thickness. @br @br
    SemiLight,

    // Letters should appear normal (default thickness). @br @br
    Normal,

    // Letters should have medium thicness, which is slightly above than normal. @br @br
    Medium,

    // Letters should appear semi-bold. @br @br
    SemiBold,

    // Letters should appear as bold. @br @br
    Bold,

    // Letters should appear as extra thick. @br @br
    ExtraBold,

    // Letters should appear very thick. @br @br
    Heavy,

    // Letters should appear as thick as technically possible.
    ExtraHeavy);

  // Relative width of the font.
  TFontStretch = (
    // Ultra-condensed width. @br @br
    UltraCondensed,

    // Extra-condensed width. @br @br
    ExtraCondensed,

    // Condensed width. @br @br
    Condensed,

    // Semi-condensed width. @br @br
    SemiCondensed,

    // Normal width. @br @br
    Normal,

    // Semi-expanded width. @br @br
    SemiExpanded,

    // Expanded width. @br @br
    Expanded,

    // Extra-expanded width. @br @br
    ExtraExpanded,

    // Ultra-expanded width.
    UltraExpanded);

  // Font slant styles.
  TFontSlant = (
    // Upright font. @br @br
    None,

    // Slanted font in a roman style. @br @br
    Oblique,

    // Slanted in an italic style.
    Italic);

  // Border that will be used around the text letters.
  TFontBorder = (
    // No border will appear around letters. @br @br
    None,

    // A light border will appear around letters. @br @br
    Normal,

    // A semi-heavy border will appear around letters. @br @br
    SemiHeavy,

    // A heavy border will appear around letters.
    Heavy);

  // Text alignment when drawing with certain functions.
  TTextAlignment = (
    // Text should be aligned to the beginning (either top or left depending on context). @br @br
    Start,

    // Text should be centered in the middle. @br @br
    Middle,

    // Text should be aligned to the end (either bottom or right depending on context).
    Ending);

  // Font attribute bits that define some visual characteristics such as underline and strikeout.
  TFontAttribute = (
    // Font with underlined text.
    Underline,

    // Font with strike-out text.
    StrikeOut);

  // Font attribute flags that define some visual characteristics.
  TFontAttributes = set of TFontAttribute;

  // Shadow rendering technique.
  TShadowTechnique = (
    // No shadows are rendered or used.
    None,

    // Debugging mode to detect problems with shadows.
    Debug,

    // Exponential Shadow Maps (ESM) using a single depth component.
    ESM,

    // Exponential Shadow Maps (ESM) using two depth components for positive and negative parts.
    ESM_Warp,

    // Variance Shadow Maps (VSM) using two depth components.
    VSM,

    // Exponential Variance Shadow Maps (EVSM) using four depth components.
    EVSM);

  // Order-Independent Transparency (OIT) technique.
  TGlassTechnique = (
    // No Order-Independent Transparency (OIT) used.
    None,

    // Weighted Average technique.
    WAVG,

    // Dual Depth-Peeling technique.
    DDP);

  // Texture type used in the scene.
  TSceneTextureType = (
    // Shadow Map texture.
    Shadow,

    // Model texture.
    Model,

    // Color buffer of the scene.
    Color,

    // Depth buffer of the scene.
    Depth);

  // Type of mouse event corresponding to the notification.
  TMouseEvent = (
    // Mouse button is pressed.
    MouseDown,

    // Mouse cursor is being moved.
    MouseMove,

    // Mouse button is de-pressed.
    MouseUp,

    // Mouse wheel has been rotated up.
    WheelUp,

    // Mouse wheel has been rotated down.
    WheelDown,

    // Mouse has entered component's area.
    MouseEnter,

    // Mouse has left component's area.
    MouseLeave);

  // Type of mouse button that corresponds to the notification.
  TMouseButton = (
    // None or unknown button (typically specified during mouse move event).
    None,

    // Left mouse button.
    Left,

    // Right mouse button.
    Right,

    // Middle mouse button.
    Middle);

  // Type of keyboard event corresponding to the notification.
  TKeyEvent = (
    // Key has been pressed.
    Pressed,

    // Key has been released.
    Released);

  // Key constants (portable).
  TKey = (
    Null = $00,
    // $01: SOH (Start of Heading)
    Home = $02, // STX (Start of Text)
  {$IFNDEF PASDOC}    
    &End = $03, // ETX (End of Text)
  {$ENDIF}
    // $04: EOT (End of Transmission)
    PrintScreen = $05, // ENQ (Enquiry)
    // $06: ACK (Acknowledge)
    // $07: BEL (Bell)
    Backspace = $08,
    Tab = $09,
    Linefeed = $0A, // LF, NL (Newline)
    Clear = $0B, // VT (Vertical Tab)
    // $0C: FF (NP form feed, new page)
    Return = $0D, // Enter, CR (Carriage Return)
    PageUp = $0E, // SO (Shift Out)
    PageDown = $0F, // SI (Shift In)
    // $10: DLE (Data Link Escape)
    CapsLock = $11, // DC1 (Device Control 1)
    NumLock = $12, // DC2 (Device Control 2)
    Pause = $13, // Hold, DC3 (Device Control 3)
    ScrollLock = $14, // DC4 (Device Control 4)
    SysReq = $15, // SysRq, NAK (Negative Acknowledge)
    // $16: SYN (Synchronous Idle)
    // $17: ETB (End of Trans. Block)
    Cancel = $18,
    // $19: EM (End of Medium)
    Insert = $1A, // SUB (Substitute)
    Escape = $1B,
    Left = $1C, // $1C: FS (File Separator)
    Up = $1D, // $1D: GS (Group Separator)
    Right = $1E, // $1E: RS (Record Separator)
    Down = $1F, // $1F: US (Unit Separator)
    Space = $20,
    // $21 - $7E: ASCII characters.
    Delete = $7F, // DEL
    // Extended keys
    F1 = $80,
    F2 = $81,
    F3 = $82,
    F4 = $83,
    F5 = $84,
    F6 = $85,
    F7 = $86,
    F8 = $87,
    F9 = $88,
    F10 = $89,
    F11 = $8A,
    F12 = $8B,
    // $8C - $97 (F13-F24 keys)
    // Control keys
    ShiftLeft = $98,
    ShiftRight = $99,
    CtrlLeft = $9A,
    CtrlRight = $9B,
    AltLeft = $9C,
    AltRight = $9D,
    SuperLeft = $9E, // Left Windows key
    SuperRight = $9F, // Right Windows key
    // Numpad keys
    Numpad0 = $A0,
    Numpad1 = $A1,
    Numpad2 = $A2,
    Numpad3 = $A3,
    Numpad4 = $A4,
    Numpad5 = $A5,
    Numpad6 = $A6,
    Numpad7 = $A7,
    Numpad8 = $A8,
    Numpad9 = $A9,
    // Numpad operators
    Multiply = $AA,
    Divide = $AB,
    Add = $AC,
    Subtract = $AD,
    // Numpad misc
    Separator = $AE,
    Decimal = $AF,
    // $B0 - $BF: reserved
    // Power keys
    Sleep = $C0,
    // $C1 - $CB: reserved
    VolumeUp = $CC,
    VolumeDown = $CE,
    VolumeMute  = $CF,
    // Media playback keys
    MediaNextTrack = $D0,
    MediaPrevTrack = $D1,
    MediaStop = $D2,
    MediaPlayPause = $D3,
    // $D4 - $DF: reserved
    // Misc keys
    Select = $E0,
    Print = $E1,
    Execute = $E2,
    Help = $E3,
    Apps = $E4 // Applications key
    // $E5 - $FE: reserved
    // $FF: reserved
  );

  // Pointer to @link(TPathJoint).
  PPathJoint = ^TPathJoint;

  // Type of joint for connecting path lines.
  TPathJoint = (
    // No joint is produced, lines on each joint end with a butt cap.
    None,

    // Simple interpolated joint that does not guarantee correct line width on sharp corners.
    Simple,

    // Miter joint with angular corners.
    Miter,

    // Beveled joint.
    Bevel,

    // Round joint.
    Round,

    // Miter joint unless the miter would extend beyond its limit multiplied by half thickness.
    MiterBevel);

  // Pointer to @link(TLineCaps).
  PLineCaps = ^TLineCaps;

  // Type of caps that covers line end points.
  TLineCaps = (
    // Cap that starts and ends exactly at the center of end points.
    Butt,

    // Cap that extends by half thickness after the end points.
    Square,

    // Round cap with a radius equal to half thickness with center being each line end point.
    Round);

  // Pointer to @link(TPointShape).
  PPointShape = ^TPointShape;

  // Shape of point primitive.
  TPointShape = (
    // Square point.
    Square,

    // Round point.
    Round,

    // Equilateral triangle point.
    Triangle,

    // 5-sided star point.
    Star,

    // Cross point.
    Cross);

  // Pointer to @link(TTessellationWinding).
  PTessellationWinding = ^TTessellationWinding;

  // Polygon tessellation winding rules.
  TTessellationWinding = (
    // Odd numbers are filled.
    Odd,

    // Positive numbers are filled.
    Positive,

    // Negative numbers are filled.
    Negative,

    // Non-zero numbers are filled.
    NonZero,

    // Absolute values greater or equal to two are filled.
    AbsoluteGreaterEqualTwo);

  // Type of surface layer that should be cleared.
  TClearLayer = (
    // Color buffer. @br @br
    Color,

    // Depth buffer. @br @br
    Depth,

    // Stencil buffer.
    Stencil);

  // One or more layer clear options.
  TClearLayers = set of TClearLayer;

  // Axis alignment that defines mesh placement in a 3D scene.
  TMeshAlign = (
    // Mesh is aligned from the edge of positive axis.
    Positive,

    // Mesh is aligned in the middle of axis origin.
    Origin,

    // Mesh is aligned from the edge of negative axis.
    Negative);

  // Type of transform as used by object models.
  TModelTransform = (
    // Local object transform.
    Local,

    // Local Model object transform (for rendering).
    LocalModel,

    // Local Volume object transform (AABB Unity Cube).
    LocalVolume,

    // Global object transform.
    Global,

    // Global Model object transform (for rendering).
    GlobalModel,

    // Global Volume object transform (AABB Unity Cube).
    GlobalVolume);

  // Cumulative flags for a 3D object model that define its status.
  TModelAttribute = (
    // Object model is visible so can be rendered and selected (if selected flag is set).
    Visible,

    // Object model can be selected.
    Selectable,

    // Object model is semi-transparent (might be rendered through a different mechanism).
    Transparent);

  // One or more 3D object model flags.
  TModelAttributes = set of TModelAttribute;

  // Type of comparison applied to object model's ordered list.
  TObjectModelCompare = (
    // Objects are compared by their depth.
    Depth = $00,

    // Objects are compared by their biased depth.
    Biased = $01,

    // Objects are compared first by their order index, then by depth.
    Ordered = $02);

  // Bit flags for optional mesh export features.
  TMeshExportFlag = (
    // Include vertex normals in the exported file.
    Normals,

    // Include vertex tangents in the exported file.
    Tangents,

    // Include vertex texture coordinates in the exported file.
    TexCoords,

    // Include vertex colors in the exported file.
    Colors);

  // One or more bit flags for optional mesh export features.
  TMeshExportFlags = set of TMeshExportFlag;
