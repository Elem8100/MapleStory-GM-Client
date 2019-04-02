unit AsphyreEGL;

interface


type
  EGLint = Integer;
  EGLenum = Integer;
  EGLBoolean = Cardinal;
  EGLConfig = Pointer;
  EGLContext = Pointer;
  EGLDisplay = Pointer;
  EGLSurface = Pointer;
  EGLClientBuffer = Pointer;   //Client API buffer handle type definition
  NativeDisplayType = EGLint;
  NativeWindowType = Pointer; //Native window handle type definition, HWND on win32
  NativePixmapType = Pointer; //Native pixmap handle type definition, HBITMAP on win32


  PEGLint = ^EGLint;
  PEGLenum = ^EGLenum;
  PEGLBoolean = ^EGLBoolean;
  PEGLConfig = ^EGLConfig;
  PEGLContext = ^EGLContext;
  PEGLDisplay = ^EGLDisplay;
  PEGLSurface = ^EGLSurface;

const
  EGL_DEFAULT_DISPLAY: NativeDisplayType = 0;	// Default native display handle
  EGL_NO_CONTEXT      = EGLContext(0);			//  Null EGL context handle		
  EGL_NO_DISPLAY      = EGLDisplay(0);			//  Null EGL display handle		
  EGL_NO_SURFACE      = EGLSurface(0);			//  Null EGL surface handle		

//------------------------------------------------------------------------
// EGL Enumerants.
//----------------------------------------------------------------------*/
  EGL_FALSE = 0;		// EGL boolean variable value FALSE
  EGL_TRUE =  1;		// EGL boolean variable value TRUE

// Errors
  EGL_SUCCESS                    = $3000;	//*!< See EGL specification */
  EGL_NOT_INITIALIZED            = $3001;	//*!< See EGL specification */
  EGL_BAD_ACCESS                 = $3002;	//*!< See EGL specification */
  EGL_BAD_ALLOC                  = $3003;	//*!< See EGL specification */
  EGL_BAD_ATTRIBUTE              = $3004;	//*!< See EGL specification */
  EGL_BAD_CONFIG                 = $3005;	//*!< See EGL specification */
  EGL_BAD_CONTEXT                = $3006;	//*!< See EGL specification */
  EGL_BAD_CURRENT_SURFACE        = $3007;	//*!< See EGL specification */
  EGL_BAD_DISPLAY                = $3008;	//*!< See EGL specification */
  EGL_BAD_MATCH                  = $3009;	//*!< See EGL specification */
  EGL_BAD_NATIVE_PIXMAP          = $300A;	//*!< See EGL specification */
  EGL_BAD_NATIVE_WINDOW          = $300B;	//*!< See EGL specification */
  EGL_BAD_PARAMETER              = $300C;	//*!< See EGL specification */
  EGL_BAD_SURFACE                = $300D;	//*!< See EGL specification */
  EGL_CONTEXT_LOST               = $300E;	//*!< See EGL specification */

//* Config attributes */
  EGL_BUFFER_SIZE                = $3020;	//*!< See EGL specification */
  EGL_ALPHA_SIZE                 = $3021;	//*!< See EGL specification */
  EGL_BLUE_SIZE                  = $3022;	//*!< See EGL specification */
  EGL_GREEN_SIZE                 = $3023;	//*!< See EGL specification */
  EGL_RED_SIZE                   = $3024;	//*!< See EGL specification */
  EGL_DEPTH_SIZE                 = $3025;	//*!< See EGL specification */
  EGL_STENCIL_SIZE               = $3026;	//*!< See EGL specification */
  EGL_CONFIG_CAVEAT              = $3027;	//*!< See EGL specification */
  EGL_CONFIG_ID                  = $3028;	//*!< See EGL specification */
  EGL_LEVEL                      = $3029;	//*!< See EGL specification */
  EGL_MAX_PBUFFER_HEIGHT         = $302A;	//*!< See EGL specification */
  EGL_MAX_PBUFFER_PIXELS         = $302B;	//*!< See EGL specification */
  EGL_MAX_PBUFFER_WIDTH          = $302C;	//*!< See EGL specification */
  EGL_NATIVE_RENDERABLE          = $302D;	//*!< See EGL specification */
  EGL_NATIVE_VISUAL_ID           = $302E;	//*!< See EGL specification */
  EGL_NATIVE_VISUAL_TYPE         = $302F;	//*!< See EGL specification */
  EGL_PRESERVED_RESOURCES        = $3030;	//*!< See EGL specification */
  EGL_SAMPLES                    = $3031;	//*!< See EGL specification */
  EGL_SAMPLE_BUFFERS             = $3032;	//*!< See EGL specification */
  EGL_SURFACE_TYPE               = $3033;	//*!< See EGL specification */
  EGL_TRANSPARENT_TYPE           = $3034;	//*!< See EGL specification */
  EGL_TRANSPARENT_BLUE_VALUE     = $3035;	//*!< See EGL specification */
  EGL_TRANSPARENT_GREEN_VALUE    = $3036;	//*!< See EGL specification */
  EGL_TRANSPARENT_RED_VALUE      = $3037;	//*!< See EGL specification */
  EGL_BIND_TO_TEXTURE_RGB        = $3039;	//*!< See EGL specification */
  EGL_BIND_TO_TEXTURE_RGBA       = $303A;	//*!< See EGL specification */
  EGL_MIN_SWAP_INTERVAL          = $303B;	//*!< See EGL specification */
  EGL_MAX_SWAP_INTERVAL          = $303C;	//*!< See EGL specification */
  EGL_LUMINANCE_SIZE			       = $303D;	//*!< See EGL specification */
  EGL_ALPHA_MASK_SIZE				     = $303E;	//*!< See EGL specification */
  EGL_COLOR_BUFFER_TYPE			     = $303F;	//*!< See EGL specification */
  EGL_RENDERABLE_TYPE				     = $3040;	//*!< See EGL specification */
  EGL_MATCH_NATIVE_PIXMAP			   = $3041;	//*!< See EGL specification \todo Right token values */

  //*! Unknown display resolution/aspect ratio */
  EGL_UNKNOWN	=	EGLint(-1);

  EGL_RENDER_BUFFER			  	= $3086;	//*!< See EGL specification */
  EGL_COLORSPACE				  	= $3087;	//*!< See EGL specification */
  EGL_ALPHA_FORMAT			  	= $3088;	//*!< See EGL specification */
  EGL_COLORSPACE_sRGB		  	= $3089;	//*!< See EGL specification */
  EGL_COLORSPACE_LINEAR	  	= $308A;	//*!< See EGL specification */
  EGL_ALPHA_FORMAT_NONPRE	  = $308B;	//*!< See EGL specification */
  EGL_ALPHA_FORMAT_PRE		  = $308C;	//*!< See EGL specification */
  EGL_CLIENT_APIS					  = $308D;	//*!< See EGL specification */
  EGL_RGB_BUFFER					  = $308E;	//*!< See EGL specification */
  EGL_LUMINANCE_BUFFER			= $308F;	//*!< See EGL specification */
  EGL_HORIZONTAL_RESOLUTION	= $3090;	//*!< See EGL specification */
  EGL_VERTICAL_RESOLUTION		= $3091;	//*!< See EGL specification */
  EGL_PIXEL_ASPECT_RATIO		= $3092;	//*!< See EGL specification */
  EGL_SWAP_BEHAVIOR			  	= $3093;	//*!< See EGL specification */
  EGL_BUFFER_PRESERVED			= $3094;	//*!< See EGL specification */
  EGL_BUFFER_DESTROYED			= $3095;	//*!< See EGL specification */
// CreatePbufferFromClientBuffer buffer types */
  EGL_OPENVG_IMAGE				  = $3096;	//*!< See EGL specification */
// QueryContext targets */
  EGL_CONTEXT_CLIENT_TYPE		= $3097;	//*!< See EGL specification */
  EGL_CONTEXT_CLIENT_VERSION= $3098;	//*!< See EGL specification.\todo Right token values */

  EGL_OPENGL_ES_API			  	= $30A0;	//*!< See EGL specification */
  EGL_OPENVG_API				  	= $30A1;	//*!< See EGL specification */

  //* Config attribute and value */
   EGL_NONE                       = $3038;			//*!< See EGL specification */

  //* Config values */
  EGL_DONT_CARE                  = EGLint(-1);	//*!< See EGL specification */
  EGL_PBUFFER_BIT                = $01;					//*!< See EGL specification */
  EGL_PIXMAP_BIT                 = $02;					//*!< See EGL specification */
  EGL_WINDOW_BIT                 = $04;					//*!< See EGL specification */
  EGL_SLOW_CONFIG                = $3050;				//*!< See EGL specification */
  EGL_NON_CONFORMANT_CONFIG      = $3051;				//*!< See EGL specification */
  EGL_TRANSPARENT_RGB            = $3052;				//*!< See EGL specification */

  EGL_NO_TEXTURE                 = $305C;				//*!< See EGL specification */
  EGL_TEXTURE_RGB                = $305D;				//*!< See EGL specification */
  EGL_TEXTURE_RGBA               = $305E;				//*!< See EGL specification */
  EGL_TEXTURE_2D                 = $305F;				//*!< See EGL specification */

  EGL_OPENGL_ES_BIT			 = $01;				  //*!< See EGL specification */
  EGL_OPENVG_BIT				 = $02;			  	//*!< See EGL specification */
  EGL_OPENGL_ES2_BIT		 = $04;				  //*!< See EGL specification \todo Right token value */
  EGL_DISPLAY_SCALING		 = 10000;			//*!< See EGL specification */

  //* String names */
  EGL_VENDOR                     = $3053;			//*!< Vendor string token */
  EGL_VERSION                    = $3054;			//*!< Version string token */
  EGL_EXTENSIONS                 = $3055;			//*!< Extensions string token */

  //* Surface attributes */
  EGL_HEIGHT                     = $3056;			//*!< Surface attribute */
  EGL_WIDTH                      = $3057;			//*!< Surface attribute */
  EGL_LARGEST_PBUFFER            = $3058;			//*!< Pixel buffer surface attribute */
  EGL_TEXTURE_FORMAT             = $3080;			//*!< Pixel buffer surface attribute */
  EGL_TEXTURE_TARGET             = $3081;			//*!< Pixel buffer surface attribute */
  EGL_MIPMAP_TEXTURE             = $3082;			//*!< Pixel buffer surface attribute */
  EGL_MIPMAP_LEVEL               = $3083;			//*!< Pixel buffer surface attribute */

  //* BindTexImage/ReleaseTexImage buffer target */
  EGL_BACK_BUFFER                = $3084;			//*!< Pixel buffer surface attribute */
  EGL_SINGLE_BUFFER			   = $3085;			//*!< Pixel buffer surface attribute */

  //* Current surfaces */
  EGL_DRAW                       = $3059;			//*!< Surface attribute */
  EGL_READ                       = $305A;			//*!< Surface attribute */

  //* Engines */
  EGL_CORE_NATIVE_ENGINE         = $305B;			//*!< See EGL specification */

//*------------------------------------------------------------------------
//* EGL Functions.
//*----------------------------------------------------------------------*/

function eglGetError(): EGLint; cdecl; external 'libEGL.dll';

function eglGetDisplay(displayID: NativeDisplayType): EGLDisplay; cdecl	; external 'libEGL.dll';
function eglInitialize(dpy: EGLDisplay; major: PEGLint; minor: PEGLint): EGLBoolean; cdecl; external 'libEGL.dll';
function eglTerminate(dpy: EGLDisplay): EGLBoolean; cdecl; external 'libEGL.dll';

function eglQueryString(dpy: EGLDisplay; name: EGLint): PChar; cdecl; external 'libEGL.dll';

function eglGetConfigs(dpy: EGLDisplay; configs: PEGLConfig; config_size: EGLint; num_config: PEGLint): EGLBoolean; cdecl; external 'libEGL.dll';
function eglChooseConfig(dpy: EGLDisplay; attrib_list: PEGLint; configs: PEGLConfig; config_size: EGLint; num_config: PEGLint): EGLBoolean; cdecl; external 'libEGL.dll';
function eglGetConfigAttrib(dpy: EGLDisplay; config: EGLConfig; attribute: EGLint; value: PEGLint): EGLBoolean; cdecl; external 'libEGL.dll';

function eglCreateWindowSurface(dpy: EGLDisplay; config: EGLConfig; win: NativeWindowType; attrib_list: PEGLint): EGLSurface; cdecl; external 'libEGL.dll';
function eglCreatePbufferSurface(dpy: EGLDisplay; config: EGLConfig; attrib_list: PEGLint): EGLSurface; cdecl; external 'libEGL.dll';
function eglCreatePixmapSurface(dpy: EGLDisplay; config: EGLConfig; pixmap: NativePixmapType; attrib_list: PEGLint): EGLSurface; cdecl; external 'libEGL.dll';
function eglDestroySurface(dpy: EGLDisplay; surface: EGLSurface): EGLBoolean; cdecl; external 'libEGL.dll';
function eglQuerySurface(dpy: EGLDisplay; surface: EGLSurface; attribute: EGLint; value: PEGLint): EGLBoolean; cdecl; external 'libEGL.dll';

function eglSurfaceAttrib(dpy: EGLDisplay; surface: EGLSurface; attribute: EGLint; value: EGLint): EGLBoolean ; cdecl; external 'libEGL.dll';
function eglBindTexImage(dpy: EGLDisplay; surface: EGLSurface; buffer: EGLint): EGLBoolean ; cdecl; external 'libEGL.dll';
function eglReleaseTexImage(dpy: EGLDisplay; surface: EGLSurface; buffer: EGLint): EGLBoolean ; cdecl; external 'libEGL.dll';

function eglSwapInterval(dpy: EGLDisplay; interval: EGLint): EGLBoolean; cdecl; external 'libEGL.dll';

function eglCreateContext(dpy: EGLDisplay; config: EGLConfig; share_list: EGLContext; attrib_list: PEGLint): EGLContext; cdecl; external 'libEGL.dll';
function eglDestroyContext(dpy: EGLDisplay; ctx: EGLContext): EGLBoolean; cdecl; external 'libEGL.dll';
function eglMakeCurrent(dpy: EGLDisplay; draw: EGLSurface; read: EGLSurface; ctx: EGLContext): EGLBoolean; cdecl; external 'libEGL.dll';

function eglGetCurrentContext(): EGLContext; cdecl; external 'libEGL.dll';
function eglGetCurrentSurface(readdraw: EGLint): EGLSurface; cdecl; external 'libEGL.dll';
function eglGetCurrentDisplay(): EGLDisplay; cdecl; external 'libEGL.dll';
function eglQueryContext(dpy: EGLDisplay; ctx: EGLContext; attribute: EGLint; value: PEGLint): EGLBoolean; cdecl; external 'libEGL.dll';

function eglWaitGL(): EGLBoolean; cdecl; external 'libEGL.dll';
function eglWaitNative(engine: EGLint): EGLBoolean; cdecl; external 'libEGL.dll';
function eglSwapBuffers(dpy: EGLDisplay; surface: EGLSurface): EGLBoolean; cdecl; external 'libEGL.dll';
function eglCopyBuffers(dpy: EGLDisplay; surface: EGLSurface; target: NativePixmapType): EGLBoolean; cdecl; external 'libEGL.dll';

function eglGetProcAddress(procname: PChar): Pointer; cdecl; external 'libEGL.dll';

function eglCreatePbufferFromClientBuffer(dpy: EGLDisplay; buftype: EGLenum; buffer: EGLClientBuffer; config: EGLConfig; attrib_list: PEGLint): EGLSurface; cdecl; external 'libEGL.dll';
function eglWaitClient(): EGLBoolean; cdecl; external 'libEGL.dll';
function eglBindAPI(api: EGLenum): EGLBoolean; cdecl; external 'libEGL.dll';
function QueryAPI(): EGLenum; cdecl; external 'libEGL.dll';
function eglReleaseThread(): EGLBoolean; cdecl; external 'libEGL.dll';


implementation


end.
