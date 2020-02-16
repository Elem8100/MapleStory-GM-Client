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
    pfUnknown,

    // Unsigned Normalized formats.

    // 8-bit unsigned normalized format with red channel only.
    pfR8,

    // 16-bit format with 8-bit unsigned normalized red and green channels.
    pfRG8,

    // 32-bit format with 8-bit unsigned normalized red, green, blue and alpha channels.
    pfRGBA8,

    // 8-bit unsigned normalized format with red channel only.
    pfR16,

    // 32-bit format with 8-bit unsigned normalized red and green channels.
    pfRG16,

    // 64-bit ARGB pixel format with each channel having 16 bits.
    pfRGBA16,

    // Signed Normalized formats.

    // 8-bit signed normalized format with red channel only.
    pfR8S,

    // 16-bit format with 8-bit signed normalized red and green channels.
    pfRG8S,

    // 32-bit format with 8-bit signed normalized red, green, blue and alpha channels.
    pfRGBA8S,

    // 16-bit signed normalized format with red channel only.
    pfR16S,

    // 32-bit format with 16-bit signed normalized red and green channels.
    pfRG16S,

    // 64-bit format with 16-bit signed normalized red, green, blue and alpha channels.
    pfRGBA16S,

    // Unsigned formats.

    // 8-bit format with 8-bit unsigned integer red channel.
    pfR8U,

    // 16-bit format with 8-bit unsigned integer red and green channels.
    pfRG8U,

    // 32-bit format with 8-bit unsigned integer red and green channels.
    pfRGBA8U,

    // 16-bit format with 16-bit unsigned integer red channel.
    pfR16U,

    // 32-bit format with 16-bit unsigned integer red and green channels.
    pfRG16U,

    // 64-bit format with 16-bit unsigned integer red and green channels.
    pfRGBA16U,

    // 32-bit format with 32-bit unsigned integer red channel.
    pfR32U,

    // 64-bit format with 32-bit unsigned integer red and green channels.
    pfRG32U,

    // 128-bit format with 32-bit unsigned integer red and green channels.
    pfRGBA32U,

    // Signed formats.

    // 8-bit format with 8-bit signed integer red channel.
    pfR8I,

    // 16-bit format with 8-bit signed integer red and green channels.
    pfRG8I,

    // 32-bit format with 8-bit signed integer red and green channels.
    pfRGBA8I,

    // 16-bit format with 16-bit signed integer red channel.
    pfR16I,

    // 32-bit format with 16-bit signed integer red and green channels.
    pfRG16I,

    // 64-bit format with 16-bit signed integer red and green channels.
    pfRGBA16I,

    // 32-bit format with 32-bit signed integer red channel.
    pfR32I,

    // 64-bit format with 32-bit signed integer red and green channels.
    pfRG32I,

    // 128-bit format with 16-bit unsigned integer red and green channels.
    pfRGBA32I,

    // Floating-point formats.

    // 16-bit floating-point pixel format, which has only one component.
    pfR16F,

    // 32-bit floating-point pixel format containing two components with 16 bits each.
    pfRG16F,

    // 64-bit floating-point ARGB pixel format with each component having 16 bits.
    pfRGBA16F,

    // 32-bit floating-point pixel format, which has only one component.
    pfR32F,

    // 64-bit floating-point pixel format containing two components with 32 bits each.
    pfRG32F,

    // 128-bit floating-point ARGB pixel format with each component having 32 bits.
    pfRGBA32F,

    // Specialized floating-point formats.

    // 32-bit format with 11-bit unsigned floating-point red and green channels, and 10-bit blue channel.
    pfRG11B10F,

    // 32-bit format with 9-bit floating-point red, green and blue channels with shared 5-bit exponent.
    pfRGB9E5F,

    // Specialized color formats.

    // 32-bit format with 10-bit unsigned normalized red, green and blue channels, and 2-bit alpha-channel.
    pfRGB10A2,

    // 32-bit format with 10-bit unsigned integer red, green and blue channels, and 2-bit alpha-channel.
    pfRGB10A2U,

    // 32-bit format with 8-bit unsigned normalized red, green and blue channels (8 bits unused).
    pfRGBX8,

    // 32-bit format with 8-bit unsigned normalized red, green, blue and alpha channels with sRGB curve.
    pfRGBA8_SRGB,

    // 32-bit format with 8-bit unsigned normalized blue, green, red and alpha channels.
    pfBGRA8,

    // 32-bit format with 8-bit unsigned normalized blue, green and red channels (8 bits unused).
    pfBGRX8,

    // 32-bit format with 8-bit unsigned normalized blue, green, red and alpha channels with sRGB curve.
    pfBGRA8_SRGB,

    // 32-bit format with 10-bit unsigned normalized blue, green and red channels, and 2-bit alpha-channel.
    pfBGR10A2,

    // 32-bit format with 10-bit unsigned normalized blue, green and red channels (2 bits unused).
    pfBGR10X2,

    // 16-bit format with 4-bit unsigned normalized blue, green, red and alpha channels.
    pfBGRA4,

    // 16-bit format with 4-bit unsigned normalized blue, green and red channels (4 bits unused).
    pfBGRX4,

    // 16-bit format with 5-bit unsigned normalized blue and red, and 6 bits for green channels.
    pfB5G6R5,

    // 16-bit format with 5-bit unsigned normalized blue, green and red, 1 bit for alpha channels.
    pfBGR5A1,

    // 16-bit format with 5-bit unsigned normalized blue, green and red channels (1 bit unused).
    pfBGR5X1,

    // Storage formats (not hardware accelerated).

    // 24-bit format with 8-bit unsigned normalized red, green and blue channels.
    pfRGB8,

    // 24-bit format with 8-bit unsigned normalized blue, green and red channels.
    pfBGR8,

    // Luminance and alpha formats that might require channel swizzling in shader.

    // 8-bit alpha-channel pixel format.
    pfA8,

    // 8-bit luminance pixel format.
    pfL8,

    // 16-bit luminance pixel format.
    pfL16,

    // 8-bit luminance/alpha pixel format with 4 bits per channel.
    pfLA4,

    // 16-bit luminance/alpha pixel format with 8 bits per channel.
    pfLA8,

    // Legacy pallette formats.

    // 8-bit palette indexed format.
    pfI8,

    // Compressed formats.

    // Compressed format with red channel only.
    pfR_BC,

    // Compressed format with red and green channels.
    pfRG_BC,

    // Compressed format with red, green and blue channels.
    pfRGB_BC,

    // Compressed format with red, green, blue and alpha channels.
    pfRGBA_BC,

    // Compressed format with red, green and blue channels with sRGB curve.
    pfRGB_BC_SRGB,

    // Compressed format with red, green and blue channels with sRGB curve, and an alpha-channel.
    pfRGBA_BC_SRGB,

    // Depth/stencil formats.

    // Depth/stencil format with 16-bit (unsigned normalized) depth values and no storage for stencil.
    pfD16,

    // Depth/stencil format with 24-bit (unsigned normalized) depth values and 8 bits for stencil.
    pfD24S8,

    // Depth/stencil format with 32-bit (floating-point) depth values and no storage for stencil.
    pfD32F,

    // Depth/stencil format with 32-bit (floating-point) depth values and 8 bits for stencil.
    pfD32S8F);

  // Defines how alpha-channel should be handled in the loaded image.
  TAlphaFormatRequest = (
    // Alpha-channel can be handled either way. @br @br
    afrDontCare,

    { Alpha-channel in the image should not be premultiplied. Under normal circumstances, this is the
      recommended approach as it preserves RGB color information in its original form. However, when using
      mipmapping for images that have alpha-channel, @italic(Premultiplied) gives more accurate
      results. @br @br }
    afrNonPremultiplied,

    { Alpha-channel in the image should be premultiplied. Under normal circumstances, this is not
      recommended as the image would lose information after RGB components are premultiplied by alpha (and
      for smaller alpha values, less information is preserved). However, when using mipmapping for images
      that have alpha-channel, this gives more accurate results. }
    afrPremultiplied);

  // Type of device used in configuration.
  TDeviceBackend = (
    // Default device that is defined for the current platform. @br @br
    dbDefault,

    // OpenGL device backend. @br @br
    dbOpenGL,

    // Direct3D device backend.
    dbDirect3D);

  // Device attributes that define its behavior.
  TDeviceAttribute = (
    // Device should use vertical sync for tear-free rendering. @br @br
    daVSync,

    // Device should use debug-type context that helps to resolve issues. @br @br
    daDebug,

    // Device should prefer to use software-based solution instead of GPU acceleration.
    daSoftware,
    
    // Compatibility device that is meant for older technologies.
    daLegacy);

  // One or more device attributes.
  TDeviceAttributes = set of TDeviceAttribute;

  // Type of graphics technology used in application.
  TDeviceTechnology = (
    // The technology has not yet been established. @br @br
    dtUnknown,

    // Microsoft Direct3D technology. @br @br
    dtDirect3D,

    // OpenGL by Khronos Group. @br @br
    dtOpenGL,

    // OpenGL ES by Khronos Group. @br @br
    dtOpenGL_ES,

    // Software rasterizer. @br @br
    dtSoftware,

    // Private proprietary technology.
    dtProprietary);

  // Type of triangle face that should have culling applied to.
  TTriangleFace = (
    // Neither back nor front faces should be culled. @br @br
    tfaNone,

    // Back faces should be culled. @br @br
    tfaBack,

    // Front faces should be culled. @br @br
    tfaFront,

    // Both back and front faces should be culled (but points and lines would not be affected).
    tfaBoth);

  // Function that defines how depth/stencil operands should be compared.
  TComparisonFunc = (
    // Comparison is always true. @br @br
    cfAlways,

    // Comparison results true when source is less than destination. @br @br
    cfLess,

    // Comparison results true when source is less than or equal to destination. @br @br
    cfLessEqual,

    // Comparison results true when source is greater than destination. @br @br
    cfGreater,

    // Comparison results true when source is greater than or equal to destination. @br @br
    cfGreaterEqual,

    // Comparison results true when source equals to destination. @br @br
    cfEqual,

    // Comparison results true when source is not equal to destination. @br @br
    cfNotEqual,

    // Comparison is always false.
    cfNever);

  // Operation that should be performed on stencil value.
  TStencilOp = (
    // Preserve destination value. @br @br
    soKeep,

    // Set destination value to zero. @br @br
    soZero,

    // Replace destination value with the source one. @br @br
    soReplace,

    // Invert the destination value. @br @br
    soInvert,

    // Increment the destination value by the source one and clamp, if necessary. @br @br
    soIncrement,

    // Decrement the destination value by the source one and clamp, if necessary. @br @br
    soDecrement,

    // Increment the destination value by the source one with wrap-around. @br @br
    soIncrementWarp,

    // Decrement the destination value by the source one with wrap-around. @br @br
    soDecrementWarp);

  // Factor that determines how alpha-blending operand is considered.
  TBlendFactor = (
    // Blending factor is a constant that equals to one. @br @br
    bfOne,

    // Blending factor is a constant that equals to zero. @br @br
    bfZero,

    // Source color is used as a blending factor. @br @br
    bfSourceColor,

    // One minus source color is used as a blending factor. @br @br
    bfInvSourceColor,

    // Source alpha value is used as a blending factor. @br @br
    bfSourceAlpha,

    // One minus source alpha value is used as a blending factor. @br @br
    bfInvSourceAlpha,

    // Destination color is used as a blending factor. @br @br
    bfDestColor,

    // One minus destination color is used as a blending factor. @br @br
    bfInvDestColor,

    // Destination alpha value is used as a blending factor. @br @br
    bfDestAlpha,

    // One minus destination alpha value is used as a blending factor. @br @br
    bfInvDestAlpha,

    // Blending factor is calculated as min(SrcAlpha, InvDestAlpha). @br @br
    bfSourceAlphaSat,

    // Blending factor is a user-defined color constant. @br @br
    bfConstantColor,

    // Blending factor is one minus user-defined color constant. @br @br
    bfInvConstantColor,

    // Blending factor is an alpha value from user-defined color constant. @br @br
    bfConstantAlpha,

    // Blending factor is one minus alpha value from user-defined color constant. @br @br
    bfInvConstantAlpha,

    // Source color (from 2nd pixel shader output) is used as a blending factor. @br @br
    bfSourceColor1,

    // One minus source color (from 2nd pixel shader output) is used as a blending factor. @br @br
    bfInvSourceColor1,

    // Source alpha value (from 2nd pixel shader output) is used as a blending factor. @br @br
    bfSourceAlpha1,

    // One minus source alpha (from 2nd pixel shader output) value is used as a blending factor.
    bfInvSourceAlpha1);

  // Operation that should be performed between two blending operands.
  TBlendOp = (
    // Add source and destination values. @br @br
    boAdd,

    // Subtract source value from destination value. @br @br
    boSubtract,

    // Subtract destination value from source value. @br @br
    boInvSubtract,

    // Calculate minimum between source and destination values. @br @br
    boMinimum,

    // Calculate maximum between source and destination values. @br @br
    boMaximum);

  // Type of data that a generic buffer contains.
  TBufferDataType = (
    // Vertex information is stored in the buffer (Vertex Buffer). @br @br
    bdtVertex,

    // Index information is stored in the buffer (Index Buffer). @br @br
    bdtIndex,

    // Shader constant information is stored in the buffer (Constant Buffer).
    bdtConstant);

  // Type of access that is performed with mesh buffer.
  TBufferAccessType = (
    // The contents of buffer are updated occasionally. @br @br
    batDefault,

    // The contents of buffer are defined during construction and typically remain unchanged. @br @br
    batStatic,

    // The contents of buffer are changed very frequently.
    batDynamic);

  // Format in which a single value of the given element is represented.
  TElementFormat = (
    // Undefined element format (@link(TVertexElement.Count) will define number of actual bytes). @br @br
    efUndefined = 0,

    // 32-bit floating-point format. @br @br
    efFloat,

    // 16-bit floating-point format. @br @br
    efHalfFloat,

    // 64-bit floating-point format. @br @br
    efDouble,

    // 32-bit signed integer format. @br @br
    efInt,

    // 32-bit unsigned integer format. @br @br
    efUnsignedInt,

    // 16-bit signed integer format. @br @br
    efShort,

    // 16-bit unsigned integer format. @br @br
    efUnsignedShort,

    // 8-bit signed integer format. @br @br
    efByte,

    // 8-bit unsigned integer format. @br @br
    efUnsignedByte,

    // For internal use (to make 16-bit enumeration).
    efElementFormatEnum = 65535);

  // Rendering primitive topology.
  TPrimitiveTopology = (
    // Unknown (undefined) topology. @br @br
    ptUnknown,

    // Points or point lists. @br @br
    ptPoints,

    // Lines or line lists. @br @br
    ptLines,

    // Line strips. @br @br
    ptLineStrip,

    // Triangles or triangle lists. @br @br
    ptTriangles,

    // Triangle strips. @br @br
    ptTriangleStrip,

    // Lines or line lists with adjacency information. @br @br
    ptLinesAdjacency,

    // Line strips with adjacency information. @br @br
    ptLineStripAdjacency,

    // Triangles or triangle lists with adjacency information. @br @br
    ptTrianglesAdjacency,

    // Triangle strips with adjacency information.
    ptTriangleStripAdjacency);

  // Type of shader in graphics rendering pipeline.
  TShaderType = (
    // Undefined or unknown shader. @br @br
    stUndefined,

    // Fragment (pixel) shader. @br @br
    stFragment,

    // Vertex shader. @br @br
    stVertex,

    // Geometry shader. @br @br
    stGeometry,

    // Compute shader.
    stCompute);

  // Type that characterizes shader element.
  TShaderElement = (
    // Undefined element type. @br @br
    seUndefined,

    // Element is a texture. @br @br
    seTexture,

    // Element is a constant buffer (uniform buffer object).
    seConstantBuffer);

  // Compute program texture access type.
  TComputeTextureAccess = (
    // Texture will be read from, no writes allowed.
    ctaRead,

    // Texture will only be written to, no reads allowed.
    ctaWrite,

    // Texture will be read from and written to.
    ctaReadWrite);

  // Filtering mode that defines how colors are sampled from the texture.
  TTextureFilter = (
    // No filtering is performed (applies only to mipmap filter, in which case it gets disabled). @br @br
    tfNone,

    // Nearest integer sample filtering. @br @br
    tfNearest,

    // Linear interpolation filtering. @br @br
    tfLinear,

    // Linear anisotropic filtering.
    tfAnisotropic);

  // Addressing mode that defines how texture coordinates outside of [0, 1] range are treated.
  TTextureAddress = (
    // Texture coordinates are wrapped; that is, only fractional part is considered. @br @br
    taWrap,

    // Texture coordinates are clamped to stay within [0, 1] range. @br @br
    taClamp,

    // Texture coordinates are mirrored in each odd cycle (e.g. from 1 to 2, from 3 to 4 and so on). @br @br
    taMirror,

    // Texture coordinates are mirrored for one cycle and then clamped. @br @br
    taMirrorOnce,

    // When texture coordinates are outside of [0, 1] range, a separate border color will be used instead.
    taBorder);

  // Type of texture that defines how it is composed.
  TTextureType = (
    // Standard 2D texture surface. @br @br
    ttSurface,

    // Cube Map consisting of 6 individual 2D faces. @br @br
    ttCubeMap,

    // Layered 3D volume texture.
    ttVolume);

  // Blending effect that defines how primitives are rendered on drawing surface.
  TBlendingEffect = (
    { Undefined blending effect: canvas does not change blending parameters configured in the device context
      and uses whatever configuration is currently active. @br @br }
    beUndefined = 0,

    // "Normal" blending effect: Cd = (Cs * As) + (Cd * (1 - As). @br @br
    beNormal,

    // "Shadow drawing" effect: Cd = Cd * (1 - As). @br @br
    beShadow,

    // Additive blending effect: Cd = (Cs * As) + Cd. @br @br
    beAdd,

    // Subtractive blending effect: Cd = (Cs * As) - Cd. @br @br
    beSubtract,

    // Multiplication blending effect: Cd = Cs * Cd. @br @br
    beMultiply,

    // Inverse multiplication effect: Cd = (1 - Cs) * Cd. @br @br
    beInverseMultiply,

    // Source color blending effect: Cd = (Cs * Cs) + (Cd * (1 - Cs)). @br @br
    beSourceColor,

    // Source color additive blending effect: Cd = (Cs * Cs) + Cd. @br @br
    beSourceColorAdd,

    // Alpha-blending disabled, drawing is just a copy operation.
    beCopy = 255);

  // Attributes that change the rendering behavior of the canvas.
  TCanvasAttribute = (
    { Transform 2D coordinates as 3D vectors (with Z = 0) by the appropriate 4x4 matrix as if drawing to an
      arbitrary plane in 3D. @br @br }
    caHolographic,

    // Preprocess the texture by using its alpha-channel as a signed distance field. @br @br
    caSDF,

    // Include outline when rendering signed distance field (requires "SDF" to be set).
    caOutline,

    // Sample texture using bicubic interpolation for higher-quality results.
    caCubic,

    // Adjusts texture color using YCH instead of RGB (only works for textures).
    caColorAdjust);

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
    ssNoSuperSample,

    // 4x super-sampled rendering. @br @br
    ssSuperSample4x,

    // 16x super-sampled rendering.
    ssSuperSample16x);

  // Pointer to @link(TCanvasContextState).
  PCanvasContextState = ^TCanvasContextState;

  // One of possible defined context states that can be pre-configured by the canvas.
  TCanvasContextState = (
    // Geometric shapes and images rendered in a flat 2D scene. @br @br
    ccsFlatScene,

    // Text rendered in a flat 2D scene. @br @br
    ccsFlatText,

    // Geometric shapes, images and text rendered in a 3D surface projection.
    ccsProjected,

    // Undefined context state.
    ccsUndefined = 255);

  // Weight of the font, or letter "thickness".
  TFontWeight = (
    // Letters should appear as thin as technically possible. @br @br
    fwThin,

    // Letters should appear very thin. @br @br
    fwExtraLight,

    // Letters should appear even thinner. @br @br
    fwLight,

    // Letters should have slightly lighter thickness. @br @br
    fwSemiLight,

    // Letters should appear normal (default thickness). @br @br
    fwNormal,

    // Letters should have medium thicness, which is slightly above than normal. @br @br
    fwMedium,

    // Letters should appear semi-bold. @br @br
    fwSemiBold,

    // Letters should appear as bold. @br @br
    fwBold,

    // Letters should appear as extra thick. @br @br
    fwExtraBold,

    // Letters should appear very thick. @br @br
    fwHeavy,

    // Letters should appear as thick as technically possible.
    fwExtraHeavy);

  // Relative width of the font.
  TFontStretch = (
    // Ultra-condensed width. @br @br
    fsUltraCondensed,

    // Extra-condensed width. @br @br
    fsExtraCondensed,

    // Condensed width. @br @br
    fsCondensed,

    // Semi-condensed width. @br @br
    fsSemiCondensed,

    // Normal width. @br @br
    fsNormal,

    // Semi-expanded width. @br @br
    fsSemiExpanded,

    // Expanded width. @br @br
    fsExpanded,

    // Extra-expanded width. @br @br
    fsExtraExpanded,

    // Ultra-expanded width.
    fsUltraExpanded);

  // Font slant styles.
  TFontSlant = (
    // Upright font. @br @br
    fsNone,

    // Slanted font in a roman style. @br @br
    fsOblique,

    // Slanted in an italic style.
    fsItalic);

  // Border that will be used around the text letters.
  TFontBorder = (
    // No border will appear around letters. @br @br
    fbNone,

    // A light border will appear around letters. @br @br
    fbNormal,

    // A semi-heavy border will appear around letters. @br @br
    fbSemiHeavy,

    // A heavy border will appear around letters.
    fbHeavy);

  // Text alignment when drawing with certain functions.
  TTextAlignment = (
    // Text should be aligned to the beginning (either top or left depending on context). @br @br
    taStart,

    // Text should be centered in the middle. @br @br
    taMiddle,

    // Text should be aligned to the end (either bottom or right depending on context).
    taEnding);

  // Font attribute bits that define some visual characteristics such as underline and strikeout.
  TFontAttribute = (
    // Font with underlined text.
    faUnderline,

    // Font with strike-out text.
    faStrikeOut);

  // Font attribute flags that define some visual characteristics.
  TFontAttributes = set of TFontAttribute;

  // Shadow rendering technique.
  TShadowTechnique = (
    // No shadows are rendered or used.
    stNone,

    // Debugging mode to detect problems with shadows.
    stDebug,

    // Exponential Shadow Maps (ESM) using a single depth component.
    stESM,

    // Exponential Shadow Maps (ESM) using two depth components for positive and negative parts.
    stESM_Warp,

    // Variance Shadow Maps (VSM) using two depth components.
    stVSM,

    // Exponential Variance Shadow Maps (EVSM) using four depth components.
    stEVSM);

  // Order-Independent Transparency (OIT) technique.
  TGlassTechnique = (
    // No Order-Independent Transparency (OIT) used.
    gtNone,

    // Weighted Average technique.
    gtWAVG,

    // Dual Depth-Peeling technique.
    gtDDP);

  // Texture type used in the scene.
  TSceneTextureType = (
    // Shadow Map texture.
    sttShadow,

    // Model texture.
    sttModel,

    // Color buffer of the scene.
    sttColor,

    // Depth buffer of the scene.
    sttDepth);

  // Type of mouse event corresponding to the notification.
  TMouseEvent = (
    // Mouse button is pressed.
    meMouseDown,

    // Mouse cursor is being moved.
    meMouseMove,

    // Mouse button is de-pressed.
    meMouseUp,

    // Mouse wheel has been rotated up.
    meWheelUp,

    // Mouse wheel has been rotated down.
    meWheelDown,

    // Mouse has entered component's area.
    meMouseEnter,

    // Mouse has left component's area.
    meMouseLeave);

  // Type of mouse button that corresponds to the notification.
  TMouseButton = (
    // None or unknown button (typically specified during mouse move event).
    mbtNone,

    // Left mouse button.
    mbtLeft,

    // Right mouse button.
    mbtRight,

    // Middle mouse button.
    mbtMiddle);

  // Type of keyboard event corresponding to the notification.
  TKeyEvent = (
    // Key has been pressed.
    kePressed,

    // Key has been released.
    keReleased);

  // Key constants (portable).
  TKey = (
    keyNull = $00,
    // $01: SOH (Start of Heading)
    keyHome = $02, // STX (Start of Text)
    keyEnd = $03, // ETX (End of Text)
    // $04: EOT (End of Transmission)
    keyPrintScreen = $05, // ENQ (Enquiry)
    // $06: ACK (Acknowledge)
    // $07: BEL (Bell)
    keyBackspace = $08,
    keyTab = $09,
    keyLinefeed = $0A, // LF, NL (Newline)
    keyClear = $0B, // VT (Vertical Tab)
    // $0C: FF (NP form feed, new page)
    keyReturn = $0D, // Enter, CR (Carriage Return)
    keyPageUp = $0E, // SO (Shift Out)
    keyPageDown = $0F, // SI (Shift In)
    // $10: DLE (Data Link Escape)
    keyCapsLock = $11, // DC1 (Device Control 1)
    keyNumLock = $12, // DC2 (Device Control 2)
    keyPause = $13, // Hold, DC3 (Device Control 3)
    keyScrollLock = $14, // DC4 (Device Control 4)
    keySysReq = $15, // SysRq, NAK (Negative Acknowledge)
    // $16: SYN (Synchronous Idle)
    // $17: ETB (End of Trans. Block)
    keyCancel = $18,
    // $19: EM (End of Medium)
    keyInsert = $1A, // SUB (Substitute)
    keyEscape = $1B,
    keyLeft = $1C, // $1C: FS (File Separator)
    keyUp = $1D, // $1D: GS (Group Separator)
    keyRight = $1E, // $1E: RS (Record Separator)
    keyDown = $1F, // $1F: US (Unit Separator)
    keySpace = $20,
    // $21 - $7E: ASCII characters.
    keyDelete = $7F, // DEL
    // Extended keys
    keyF1 = $80,
    keyF2 = $81,
    keyF3 = $82,
    keyF4 = $83,
    keyF5 = $84,
    keyF6 = $85,
    keyF7 = $86,
    keyF8 = $87,
    keyF9 = $88,
    keyF10 = $89,
    keyF11 = $8A,
    keyF12 = $8B,
    // $8C - $97 (F13-F24 keys)
    // Control keys
    keyShiftLeft = $98,
    keyShiftRight = $99,
    keyCtrlLeft = $9A,
    keyCtrlRight = $9B,
    keyAltLeft = $9C,
    keyAltRight = $9D,
    keySuperLeft = $9E, // Left Windows key
    keySuperRight = $9F, // Right Windows key
    // Numpad keys
    keyNumpad0 = $A0,
    keyNumpad1 = $A1,
    keyNumpad2 = $A2,
    keyNumpad3 = $A3,
    keyNumpad4 = $A4,
    keyNumpad5 = $A5,
    keyNumpad6 = $A6,
    keyNumpad7 = $A7,
    keyNumpad8 = $A8,
    keyNumpad9 = $A9,
    // Numpad operators
    keyMultiply = $AA,
    keyDivide = $AB,
    keyAdd = $AC,
    keySubtract = $AD,
    // Numpad misc
    keySeparator = $AE,
    keyDecimal = $AF,
    // $B0 - $BF: reserved
    // Power keys
    keySleep = $C0,
    // $C1 - $CB: reserved
    keyVolumeUp = $CC,
    keyVolumeDown = $CE,
    keyVolumeMute  = $CF,
    // Media playback keys
    keyMediaNextTrack = $D0,
    keyMediaPrevTrack = $D1,
    keyMediaStop = $D2,
    keyMediaPlayPause = $D3,
    // $D4 - $DF: reserved
    // Misc keys
    keySelect = $E0,
    keyPrint = $E1,
    keyExecute = $E2,
    keyHelp = $E3,
    keyApps = $E4 // Applications key
    // $E5 - $FE: reserved
    // $FF: reserved
  );

  // Pointer to @link(TPathJoint).
  PPathJoint = ^TPathJoint;

  // Type of joint for connecting path lines.
  TPathJoint = (
    // No joint is produced, lines on each joint end with a butt cap.
    pjNone,

    // Simple interpolated joint that does not guarantee correct line width on sharp corners.
    pjSimple,

    // Miter joint with angular corners.
    pjMiter,

    // Beveled joint.
    pjBevel,

    // Round joint.
    pjRound,

    // Miter joint unless the miter would extend beyond its limit multiplied by half thickness.
    pjMiterBevel);

  // Pointer to @link(TLineCaps).
  PLineCaps = ^TLineCaps;

  // Type of caps that covers line end points.
  TLineCaps = (
    // Cap that starts and ends exactly at the center of end points.
    lcButt,

    // Cap that extends by half thickness after the end points.
    lcSquare,

    // Round cap with a radius equal to half thickness with center being each line end point.
    lcRound);

  // Pointer to @link(TPointShape).
  PPointShape = ^TPointShape;

  // Shape of point primitive.
  TPointShape = (
    // Square point.
    psSquare,

    // Round point.
    psRound,

    // Equilateral triangle point.
    psTriangle,

    // 5-sided star point.
    psStar,

    // Cross point.
    psCross);

  // Pointer to @link(TTessellationWinding).
  PTessellationWinding = ^TTessellationWinding;

  // Polygon tessellation winding rules.
  TTessellationWinding = (
    // Odd numbers are filled.
    twOdd,

    // Positive numbers are filled.
    twPositive,

    // Negative numbers are filled.
    twNegative,

    // Non-zero numbers are filled.
    twNonZero,

    // Absolute values greater or equal to two are filled.
    twAbsoluteGreaterEqualTwo);

  // Type of surface layer that should be cleared.
  TClearLayer = (
    // Color buffer. @br @br
    clColor,

    // Depth buffer. @br @br
    clDepth,

    // Stencil buffer.
    clStencil);

  // One or more layer clear options.
  TClearLayers = set of TClearLayer;

  // Axis alignment that defines mesh placement in a 3D scene.
  TMeshAlign = (
    // Mesh is aligned from the edge of positive axis.
    maPositive,

    // Mesh is aligned in the middle of axis origin.
    maOrigin,

    // Mesh is aligned from the edge of negative axis.
    maNegative);

  // Type of transform as used by object models.
  TModelTransform = (
    // Local object transform.
    mtLocal,

    // Local Model object transform (for rendering).
    mtLocalModel,

    // Local Volume object transform (AABB Unity Cube).
    mtLocalVolume,

    // Global object transform.
    mtGlobal,

    // Global Model object transform (for rendering).
    mtGlobalModel,

    // Global Volume object transform (AABB Unity Cube).
    mtGlobalVolume);

  // Cumulative flags for a 3D object model that define its status.
  TModelAttribute = (
    // Object model is visible so can be rendered and selected (if selected flag is set).
    maVisible,

    // Object model can be selected.
    maSelectable,

    // Object model is semi-transparent (might be rendered through a different mechanism).
    maTransparent);

  // One or more 3D object model flags.
  TModelAttributes = set of TModelAttribute;

  // Type of comparison applied to object model's ordered list.
  TObjectModelCompare = (
    // Objects are compared by their depth.
    omcDepth = $00,

    // Objects are compared by their biased depth.
    omcBiased = $01,

    // Objects are compared first by their order index, then by depth.
    omcOrdered = $02);

  // Bit flags for optional mesh export features.
  TMeshExportFlag = (
    // Include vertex normals in the exported file.
    mefNormals,

    // Include vertex tangents in the exported file.
    mefTangents,

    // Include vertex texture coordinates in the exported file.
    mefTexCoords,

    // Include vertex colors in the exported file.
    mefColors);

  // One or more bit flags for optional mesh export features.
  TMeshExportFlags = set of TMeshExportFlag;
