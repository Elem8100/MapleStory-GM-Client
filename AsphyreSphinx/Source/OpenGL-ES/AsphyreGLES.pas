unit AsphyreGLES;

interface

type
  GLenum = cardinal;
  GLboolean = byte;
  GLbitfield = cardinal;
  GLbyte = Shortint;
  GLshort = Smallint;
  GLint = integer;
  GLsizei = integer;
  GLubyte = byte;
  GLushort = word;
  GLuint = cardinal;
  GLfloat = single;
  GLclampf = single;
  GLclampx = integer;
  GLfixed = integer;
  GLintptr =  integer;
  GLsizeiptr = integer;

  PGLvoid = Pointer;
  PPGLvoid = PGLvoid;
  PGLubyte = ^GLubyte;
  PGLuint = ^GLuint;
  PGLfloat = ^GLfloat;
  PGLfixed = ^GLfixed;
  PGLint = ^GLint;
  PGLboolean = ^GLboolean;
  PGLshort = ^GLshort;

//-------------------------------------------------------------------------
// API enumerations
//-----------------------------------------------------------------------*/
const
//* extensions */
   GL_OES_VERSION_1_0							    = 1;
   GL_OES_byte_coordinates						= 1;
   GL_OES_fixed_point							    = 1;
   GL_OES_single_precision						= 1;
   GL_OES_read_format							    = 1;
   GL_OES_query_matrix							  = 1;
   GL_OES_compressed_paletted_texture	=	1;

  GL_OES_VERSION_1_1					=	1;
  GL_OES_matrix_palette				=	1;
  GL_OES_point_sprite					=	1;
  GL_OES_point_size_array			=	1;
  GL_OES_draw_texture					=	1;
  GL_OES_matrix_get						=	1;

//* boolean values */
   GL_FALSE								=	0;
   GL_TRUE								=	1;

//* common values */
   GL_ZERO								=	0;
   GL_ONE									=	1;

//* comparison functions */
   GL_NEVER									= $0200;
   GL_LESS									= $0201;
   GL_EQUAL									= $0202;
   GL_LEQUAL								= $0203;
   GL_GREATER								= $0204;
   GL_NOTEQUAL							= $0205;
   GL_GEQUAL								= $0206;
   GL_ALWAYS								= $0207;

//* data types */
   GL_BYTE									= $1400;
   GL_UNSIGNED_BYTE					= $1401;
   GL_SHORT									= $1402;
   GL_UNSIGNED_SHORT				= $1403;
   GL_FLOAT									= $1406;
   GL_FIXED									= $140C;

//* primitives */
  GL_POINTS								= $0000;
  GL_LINES								= $0001;
  GL_LINE_LOOP						= $0002;
  GL_LINE_STRIP						= $0003;
  GL_TRIANGLES						= $0004;
  GL_TRIANGLE_STRIP				= $0005;
  GL_TRIANGLE_FAN					= $0006;

//* vertex arrays */
  GL_VERTEX_ARRAY								= $8074;
  GL_NORMAL_ARRAY								= $8075;
  GL_COLOR_ARRAY								= $8076;
  GL_TEXTURE_COORD_ARRAY				= $8078;
  GL_MATRIX_INDEX_ARRAY_OES			= $8844;
  GL_WEIGHT_ARRAY_OES						= $86AD;
  GL_POINT_SIZE_ARRAY_OES				= $8B9C;

//* vertex buffer objects */
  GL_ARRAY_BUFFER							= $8892;
  GL_ELEMENT_ARRAY_BUFFER			= $8893;
  GL_STREAM_DRAW							= $88E0;
  GL_STATIC_DRAW							= $88E4;
  GL_DYNAMIC_DRAW							= $88E8;
  GL_BUFFER_SIZE							= $8764;
  GL_BUFFER_USAGE							= $8765;
  GL_BUFFER_ACCESS						= $88BB;
  GL_WRITE_ONLY						  	= $88B9;

//* matrix mode */
   GL_MODELVIEW								= $1700;
   GL_PROJECTION							= $1701;
   GL_TEXTURE									= $1702;

  GL_MATRIX_PALETTE_OES				= $8840;

//* smoothing */
   GL_POINT_SMOOTH						= $0B10;
   GL_LINE_SMOOTH							= $0B20;

//* geometry */
   GL_CW										= $0900;
   GL_CCW										= $0901;
   GL_FRONT									= $0404;
   GL_BACK									= $0405;
   GL_CULL_FACE							= $0B44;
   GL_POLYGON_OFFSET_FILL		= $8037;

//* lighting */
   GL_LIGHTING								= $0B50;
   GL_LIGHT0									= $4000;
   GL_LIGHT1									= $4001;
   GL_LIGHT2									= $4002;
   GL_LIGHT3									= $4003;
   GL_LIGHT4									= $4004;
   GL_LIGHT5									= $4005;
   GL_LIGHT6									= $4006;
   GL_LIGHT7									= $4007;
   GL_SPOT_EXPONENT						= $1205;
   GL_SPOT_CUTOFF							= $1206;
   GL_CONSTANT_ATTENUATION		= $1207;
   GL_LINEAR_ATTENUATION			= $1208;
   GL_QUADRATIC_ATTENUATION		= $1209;
   GL_AMBIENT									= $1200;
   GL_DIFFUSE									= $1201;
   GL_SPECULAR								= $1202;
   GL_EMISSION								= $1600;
   GL_SHININESS								= $1601;
   GL_POSITION								= $1203;
   GL_SPOT_DIRECTION					= $1204;
   GL_AMBIENT_AND_DIFFUSE			= $1602;
   GL_LIGHT_MODEL_TWO_SIDE		= $0B52;
   GL_LIGHT_MODEL_AMBIENT			= $0B53;
   GL_FRONT_AND_BACK					= $0408;
   GL_FLAT										= $1D00;
   GL_SMOOTH									= $1D01;
   GL_COLOR_MATERIAL					= $0B57;
   GL_NORMALIZE								= $0BA1;

//* blending */
   GL_ADD										= $0104;
   GL_BLEND									= $0BE2;
   GL_SRC_COLOR							= $0300;
   GL_ONE_MINUS_SRC_COLOR		= $0301;
   GL_SRC_ALPHA							= $0302;
   GL_ONE_MINUS_SRC_ALPHA		= $0303;
   GL_DST_ALPHA							= $0304;
   GL_ONE_MINUS_DST_ALPHA		= $0305;
   GL_DST_COLOR							= $0306;
   GL_ONE_MINUS_DST_COLOR		= $0307;
   GL_SRC_ALPHA_SATURATE		= $0308;

//* fog */
   GL_FOG										= $0B60;
   GL_FOG_DENSITY						= $0B62;
   GL_FOG_START						  = $0B63;
   GL_FOG_END								= $0B64;
   GL_FOG_MODE							= $0B65;
   GL_FOG_COLOR							= $0B66;
   GL_EXP										= $0800;
   GL_EXP2									= $0801;

//* logic ops */
   GL_CLEAR									= $1500;
   GL_AND										= $1501;
   GL_AND_REVERSE								= $1502;
   GL_COPY										= $1503;
   GL_AND_INVERTED								= $1504;
   GL_NOOP										= $1505;
   GL_XOR										= $1506;
   GL_OR										= $1507;
   GL_NOR										= $1508;
   GL_EQUIV									= $1509;
   GL_INVERT									= $150A;
   GL_OR_REVERSE								= $150B;
   GL_COPY_INVERTED							= $150C;
   GL_OR_INVERTED								= $150D;
   GL_NAND										= $150E;
   GL_SET										= $150F;

//* capabilities */
   GL_DEPTH_TEST								= $0B71;
   GL_STENCIL_TEST								= $0B90;
   GL_ALPHA_TEST								= $0BC0;
   GL_DITHER									= $0BD0;
   GL_COLOR_LOGIC_OP							= $0BF2;
   GL_SCISSOR_TEST								= $0C11;
   GL_RESCALE_NORMAL							= $803A;

  GL_POINT_SPRITE_OES						= $8861;

//* stencil ops */
   GL_KEEP										= $1E00;
   GL_REPLACE									= $1E01;
   GL_INCR										= $1E02;
   GL_DECR										= $1E03;

//* buffers, pixel reading */
   GL_ALPHA									= $1906;
   GL_RGB										= $1907;
   GL_RGBA										= $1908;
   GL_LUMINANCE								= $1909;
   GL_LUMINANCE_ALPHA							= $190A;

//* getters */
   GL_SMOOTH_POINT_SIZE_RANGE					= $0B12;
   GL_SMOOTH_LINE_WIDTH_RANGE					= $0B22;
   GL_ALIASED_POINT_SIZE_RANGE					= $846D;
   GL_ALIASED_LINE_WIDTH_RANGE					= $846E;
   GL_MAX_LIGHTS								= $0D31;
   GL_MAX_TEXTURE_SIZE							= $0D33;
   GL_MAX_MODELVIEW_STACK_DEPTH				= $0D36;
   GL_MAX_PROJECTION_STACK_DEPTH				= $0D38;
   GL_MAX_TEXTURE_STACK_DEPTH					= $0D39;
   GL_MAX_VIEWPORT_DIMS						= $0D3A;
   GL_SUBPIXEL_BITS							= $0D50;
   GL_RED_BITS									= $0D52;
   GL_GREEN_BITS								= $0D53;
   GL_BLUE_BITS								= $0D54;
   GL_ALPHA_BITS								= $0D55;
   GL_DEPTH_BITS								= $0D56;
   GL_STENCIL_BITS								= $0D57;
   GL_MAX_ELEMENTS_VERTICES					= $80E8;
   GL_MAX_ELEMENTS_INDICES						= $80E9;
   GL_MAX_TEXTURE_UNITS						= $84E2;
   GL_NUM_COMPRESSED_TEXTURE_FORMATS			= $86A2;
   GL_COMPRESSED_TEXTURE_FORMATS				= $86A3;
   GL_IMPLEMENTATION_COLOR_READ_TYPE_OES		= $8B9A;
   GL_IMPLEMENTATION_COLOR_READ_FORMAT_OES		= $8B9B;

  GL_MAX_PALETTE_MATRICES_OES				= $8842;
  GL_MAX_VERTEX_UNITS_OES					= $86A4;
  GL_MAX_CLIP_PLANES						= $0D32;

//* dynamic gets */
  GL_CLIENT_ACTIVE_TEXTURE					= $84E1;
  GL_VERTEX_ARRAY_SIZE						= $807A;
  GL_VERTEX_ARRAY_TYPE						= $807B;
  GL_VERTEX_ARRAY_POINTER					= $808E;
  GL_VERTEX_ARRAY_STRIDE					= $807C;
  GL_NORMAL_ARRAY_TYPE						= $807E;
  GL_NORMAL_ARRAY_STRIDE					= $807F;
  GL_NORMAL_ARRAY_POINTER					= $808F;
  GL_COLOR_ARRAY_SIZE						= $8081;
  GL_COLOR_ARRAY_TYPE						= $8082;
  GL_COLOR_ARRAY_STRIDE					= $8083;
  GL_COLOR_ARRAY_POINTER					= $8090;
  GL_TEXTURE_COORD_ARRAY_SIZE				= $8088;
  GL_TEXTURE_COORD_ARRAY_TYPE				= $8089;
  GL_TEXTURE_COORD_ARRAY_STRIDE			= $808A;
  GL_TEXTURE_COORD_ARRAY_POINTER			= $8092;
  GL_ARRAY_BUFFER_BINDING					= $8894;
  GL_VERTEX_ARRAY_BUFFER_BINDING			= $8896;
  GL_NORMAL_ARRAY_BUFFER_BINDING			= $8897;
  GL_COLOR_ARRAY_BUFFER_BINDING			= $8898;
  GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING	= $889A;
  GL_ELEMENT_ARRAY_BUFFER_BINDING			= $8895;
  GL_VIEWPORT								= $0BA2;
  GL_DEPTH_RANGE							= $0B70;
  GL_MATRIX_MODE							= $0BA0;
  GL_SHADE_MODEL							= $0B54;
  GL_POINT_SIZE							= $0B11;
  GL_LINE_WIDTH							= $0B21;
  GL_CULL_FACE_MODE						= $0B45;
  GL_FRONT_FACE							= $0B46;
  GL_POLYGON_OFFSET_FACTOR					= $8038;
  GL_POLYGON_OFFSET_UNITS					= $2A00;
  GL_TEXTURE_BINDING_2D					= $8069;
  GL_ACTIVE_TEXTURE						= $84E0;
  GL_SCISSOR_BOX							= $0C10;
  GL_ALPHA_TEST_FUNC						= $0BC1;
  GL_ALPHA_TEST_REF						= $0BC2;
  GL_STENCIL_FUNC							= $0B92;
  GL_STENCIL_VALUE_MASK					= $0B93;
  GL_STENCIL_REF							= $0B97;
  GL_STENCIL_FAIL							= $0B94;
  GL_STENCIL_PASS_DEPTH_FAIL				= $0B95;
  GL_STENCIL_PASS_DEPTH_PASS				= $0B96;
  GL_DEPTH_FUNC							= $0B74;
  GL_BLEND_SRC								= $0BE1;
  GL_BLEND_DST								= $0BE0;
  GL_LOGIC_OP_MODE							= $0BF0;
  GL_COLOR_WRITEMASK						= $0C23;
  GL_DEPTH_WRITEMASK						= $0B72;
  GL_STENCIL_WRITEMASK						= $0B98;
  GL_COLOR_CLEAR_VALUE						= $0C22;
  GL_DEPTH_CLEAR_VALUE						= $0B73;
  GL_STENCIL_CLEAR_VALUE					= $0B91;
  GL_MODELVIEW_MATRIX						= $0BA6;
  GL_PROJECTION_MATRIX						= $0BA7;
  GL_TEXTURE_MATRIX						= $0BA8;
  GL_MODELVIEW_STACK_DEPTH					= $0BA3;
  GL_PROJECTION_STACK_DEPTH				= $0BA4;
  GL_TEXTURE_STACK_DEPTH					= $0BA5;
  GL_MATRIX_INDEX_ARRAY_SIZE_OES			= $8846;
  GL_MATRIX_INDEX_ARRAY_TYPE_OES			= $8847;
  GL_MATRIX_INDEX_ARRAY_STRIDE_OES			= $8848;
  GL_MATRIX_INDEX_ARRAY_POINTER_OES		= $8849;
  GL_MATRIX_INDEX_ARRAY_BUFFER_BINDING_OES = $8B9E;
  GL_WEIGHT_ARRAY_SIZE_OES					= $86AB;
  GL_WEIGHT_ARRAY_TYPE_OES					= $86A9;
  GL_WEIGHT_ARRAY_STRIDE_OES				= $86AA;
  GL_WEIGHT_ARRAY_POINTER_OES				= $86AC;
  GL_WEIGHT_ARRAY_BUFFER_BINDING_OES		= $889E;
  GL_POINT_SIZE_ARRAY_TYPE_OES				= $898A;
  GL_POINT_SIZE_ARRAY_STRIDE_OES			= $898B;
  GL_POINT_SIZE_ARRAY_POINTER_OES			= $898C;
  GL_POINT_SIZE_ARRAY_BUFFER_BINDING_OES	= $8B9F;
  GL_SAMPLE_COVERAGE_INVERT				= $80AB;
  GL_SAMPLE_COVERAGE_VALUE					= $80AA;
  GL_POINT_SIZE_MIN						= $8126;
  GL_POINT_SIZE_MAX						= $8127;
  GL_POINT_FADE_THRESHOLD_SIZE				= $8128;
  GL_POINT_DISTANCE_ATTENUATION			= $8129;
  GL_CURRENT_COLOR							= $0B00;
  GL_CURRENT_NORMAL						= $0B02;
  GL_CURRENT_TEXTURE_COORDS				= $0B03;
  GL_MODELVIEW_MATRIX_FLOAT_AS_INT_BITS_OES	= $898D;
  GL_PROJECTION_MATRIX_FLOAT_AS_INT_BITS_OES	= $898E;
  GL_TEXTURE_MATRIX_FLOAT_AS_INT_BITS_OES		= $898F;

//* clip planes */
  GL_CLIP_PLANE0							= $3000;
  GL_CLIP_PLANE1							= $3001;
  GL_CLIP_PLANE2							= $3002;
  GL_CLIP_PLANE3							= $3003;
  GL_CLIP_PLANE4							= $3004;
  GL_CLIP_PLANE5							= $3005;

//* hints */
   GL_PERSPECTIVE_CORRECTION_HINT				= $0C50;
   GL_LINE_SMOOTH_HINT							= $0C52;
   GL_POINT_SMOOTH_HINT						= $0C51;
   GL_FOG_HINT									= $0C54;
   GL_DONT_CARE								= $1100;
   GL_FASTEST									= $1101;
   GL_NICEST									= $1102;

  GL_GENERATE_MIPMAP_HINT					= $8192;

//* pixel store */
   GL_UNPACK_ALIGNMENT							= $0CF5;
   GL_PACK_ALIGNMENT							= $0D05;

//* multisample */
   GL_MULTISAMPLE								= $809D;
   GL_SAMPLE_ALPHA_TO_COVERAGE					= $809E;
   GL_SAMPLE_ALPHA_TO_ONE						= $809F;
   GL_SAMPLE_COVERAGE							= $80A0;
   GL_SAMPLE_BUFFERS							= $80A8;
   GL_SAMPLES									= $80A9;

//* texture mapping */
   GL_TEXTURE_2D								= $0DE1;
   GL_TEXTURE_ENV								= $2300;
   GL_TEXTURE_ENV_MODE							= $2200;
   GL_TEXTURE_MAG_FILTER						= $2800;
   GL_TEXTURE_MIN_FILTER						= $2801;
   GL_TEXTURE_WRAP_S							= $2802;
   GL_TEXTURE_WRAP_T							= $2803;
   GL_TEXTURE_ENV_COLOR						= $2201;
   GL_MODULATE									= $2100;
   GL_DECAL									= $2101;
   GL_NEAREST									= $2600;
   GL_LINEAR									= $2601;
   GL_NEAREST_MIPMAP_NEAREST					= $2700;
   GL_LINEAR_MIPMAP_NEAREST					= $2701;
   GL_NEAREST_MIPMAP_LINEAR					= $2702;
   GL_LINEAR_MIPMAP_LINEAR						= $2703;
   GL_REPEAT									= $2901;
   GL_CLAMP_TO_EDGE							= $812F;

  GL_GENERATE_MIPMAP						= $8191;
  GL_COORD_REPLACE_OES						= $8862;
  GL_TEXTURE_CROP_RECT_OES					= $8B9D;

//* tex env combine */
  GL_COMBINE								= $8570;
  GL_COMBINE_RGB							= $8571;
  GL_COMBINE_ALPHA							= $8572;
  GL_SOURCE0_RGB							= $8580;
  GL_SOURCE1_RGB							= $8581;
  GL_SOURCE2_RGB							= $8582;
  GL_SOURCE0_ALPHA							= $8588;
  GL_SOURCE1_ALPHA							= $8589;
  GL_SOURCE2_ALPHA							= $858A;
  GL_SRC0_RGB							        = GL_SOURCE0_RGB;
  GL_SRC1_RGB								= GL_SOURCE1_RGB;
  GL_SRC2_RGB								= GL_SOURCE2_RGB;
  GL_SRC0_ALPHA							        = GL_SOURCE0_ALPHA;
  GL_SRC1_ALPHA							        = GL_SOURCE1_ALPHA;
  GL_SRC2_ALPHA							        = GL_SOURCE2_ALPHA;
  GL_OPERAND0_RGB							= $8590;
  GL_OPERAND1_RGB							= $8591;
  GL_OPERAND2_RGB							= $8592;
  GL_OPERAND0_ALPHA						= $8598;
  GL_OPERAND1_ALPHA						= $8599;
  GL_OPERAND2_ALPHA						= $859A;
  GL_RGB_SCALE								= $8573;
  GL_ALPHA_SCALE							= $0D1C;
  GL_ADD_SIGNED							= $8574;
  GL_INTERPOLATE							= $8575;
  GL_SUBTRACT								= $84E7;
  GL_DOT3_RGB								= $86AE;
  GL_DOT3_RGBA								= $86AF;
  GL_CONSTANT								= $8576;
  GL_PRIMARY_COLOR							= $8577;
  GL_PREVIOUS								= $8578;

//* paletted internal formats */
   GL_PALETTE4_RGB8_OES						= $8B90;
   GL_PALETTE4_RGBA8_OES						= $8B91;
   GL_PALETTE4_R5_G6_B5_OES					= $8B92;
   GL_PALETTE4_RGBA4_OES						= $8B93;
   GL_PALETTE4_RGB5_A1_OES						= $8B94;
   GL_PALETTE8_RGB8_OES						= $8B95;
   GL_PALETTE8_RGBA8_OES						= $8B96;
   GL_PALETTE8_R5_G6_B5_OES					= $8B97;
   GL_PALETTE8_RGBA4_OES						= $8B98;
   GL_PALETTE8_RGB5_A1_OES						= $8B99;

//* utility */
   GL_VENDOR									= $1F00;
   GL_RENDERER									= $1F01;
   GL_VERSION									= $1F02;
   GL_EXTENSIONS								= $1F03;

//* errors */
   GL_NO_ERROR									= 0;
   GL_INVALID_ENUM								= $0500;
   GL_INVALID_VALUE							= $0501;
   GL_INVALID_OPERATION						= $0502;
   GL_STACK_OVERFLOW							= $0503;
   GL_STACK_UNDERFLOW							= $0504;
   GL_OUT_OF_MEMORY							= $0505;

//* texture formats */
   GL_UNSIGNED_SHORT_5_6_5						= $8363;
   GL_UNSIGNED_SHORT_4_4_4_4					= $8033;
   GL_UNSIGNED_SHORT_5_5_5_1					= $8034;

//* buffer bits */
   GL_DEPTH_BUFFER_BIT							= $00000100;
   GL_STENCIL_BUFFER_BIT						= $00000400;
   GL_COLOR_BUFFER_BIT							= $00004000;

//* ARB_multitexture */
   GL_TEXTURE0									= $84C0;
   GL_TEXTURE1									= $84C1;
   GL_TEXTURE2									= $84C2;
   GL_TEXTURE3									= $84C3;
   GL_TEXTURE4									= $84C4;
   GL_TEXTURE5									= $84C5;
   GL_TEXTURE6									= $84C6;
   GL_TEXTURE7									= $84C7;
   GL_TEXTURE8									= $84C8;
   GL_TEXTURE9									= $84C9;
   GL_TEXTURE10								= $84CA;
   GL_TEXTURE11								= $84CB;
   GL_TEXTURE12								= $84CC;
   GL_TEXTURE13								= $84CD;
   GL_TEXTURE14								= $84CE;
   GL_TEXTURE15								= $84CF;
   GL_TEXTURE16								= $84D0;
   GL_TEXTURE17								= $84D1;
   GL_TEXTURE18								= $84D2;
   GL_TEXTURE19								= $84D3;
   GL_TEXTURE20								= $84D4;
   GL_TEXTURE21								= $84D5;
   GL_TEXTURE22								= $84D6;
   GL_TEXTURE23								= $84D7;
   GL_TEXTURE24								= $84D8;
   GL_TEXTURE25								= $84D9;
   GL_TEXTURE26								= $84DA;
   GL_TEXTURE27								= $84DB;
   GL_TEXTURE28								= $84DC;
   GL_TEXTURE29								= $84DD;
   GL_TEXTURE30								= $84DE;
   GL_TEXTURE31								= $84DF;

//*-------------------------------------------------------------------------
// * Function prototypes             cdecl  cdecl
// *-----------------------------------------------------------------------*/

procedure	glActiveTexture(texture: GLenum); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glAlphaFunc(func: GLenum; ref: GLclampf); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glAlphaFuncx(func: GLenum;  ref: GLclampx); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glBindTexture(target: GLenum;  texture: GLuint);  cdecl; external 'libGLES_CM_NoE.dll';
procedure	glBlendFunc(sfactor: GLenum;  dfactor: GLenum); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glClear(mask: GLbitfield); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glClearColor(red: GLclampf; green: GLclampf; blue: GLclampf; alpha: GLclampf); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glClearColorx(red: GLclampx; green: GLclampx; blue: GLclampx; alpha: GLclampx); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glClearDepthf(depth: GLclampf); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glClearDepthx(depth: GLclampx); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glClearStencil			(s: GLint); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glClientActiveTexture	(texture: GLenum); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glColor4f				(red: GLfloat; green: GLfloat; blue: GLfloat; alpha: GLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glColor4x				(red: GLfixed; green: GLfixed; blue: GLfixed; alpha: GLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glColorMask				(red: GLboolean; green: GLboolean; blue: GLboolean; alpha: GLboolean); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glColorPointer			(size: GLint; typee: GLenum; stride: GLsizei; ptr: PGLvoid); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glCompressedTexImage2D	(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; border: GLint; imageSize: GLsizei; data: PGLvoid); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glCompressedTexSubImage2D(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; height: GLsizei; format: GLenum; imageSize: GLsizei; data: PGLvoid); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glCopyTexImage2D		(target: GLenum; level: GLint; internalformat: GLenum; x : GLint; y: GLint; width: GLsizei; height: GLsizei; border: GLint); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glCopyTexSubImage2D		(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; x: GLint; y: GLint; width: GLsizei; height: GLsizei);  cdecl; external 'libGLES_CM_NoE.dll';
procedure	glCullFace				(mode: GLenum); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glDeleteTextures		(n: GLsizei; textures: PGLuint); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glDepthFunc				(func: GLenum); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glDepthMask				(flag: GLboolean); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glDepthRangef			(nearValue: GLclampf; farValue: GLclampf); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glDepthRangex			(nearValue: GLclampx; farValue: GLclampx); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glDisable				(cap: GLenum); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glDisableClientState	(cap: GLenum); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glDrawArrays			(mode: GLenum; first: GLint; count: GLsizei); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glDrawElements			(mode: GLenum; count: GLsizei; typee: GLenum; indices: PGLvoid); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glEnable				(cap: GLenum); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glEnableClientState		(cap: GLenum); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glFinish				(); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glFlush					(); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glFogf					(pname: GLenum; param: GLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glFogfv					(pname: GLenum; params: PGLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glFogx					(pname: GLenum; param: GLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glFogxv					(pname: GLenum; params: PGLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glFrontFace				(mode: GLenum); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glFrustumf				(left: GLfloat; right: GLfloat; bottom: GLfloat; top: GLfloat; near_val: GLfloat; far_val: GLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure glFrustumx				(left: GLfixed; right: GLfixed; bottom: GLfixed; top: GLfixed; near_val: GLfixed; far_val: GLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glGenTextures			(n: GLsizei; textures: PGLuint); cdecl; external 'libGLES_CM_NoE.dll';
function 			glGetError				(): GLenum; cdecl; external 'libGLES_CM_NoE.dll';
procedure	glGetIntegerv			(pname: GLenum; params: PGLint); cdecl; external 'libGLES_CM_NoE.dll';
function        glGetString				( name: GLenum): PGLubyte;  cdecl; external 'libGLES_CM_NoE.dll';
procedure	glHint					(target: GLenum; mode: GLenum); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glLightf				(light: GLenum; pname: GLenum; param:  GLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glLightfv				(light: GLenum; pname: GLenum; params: PGLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glLightx				(light: GLenum; pname: GLenum; param: GLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glLightxv				(light: GLenum; pname: GLenum; params: PGLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glLightModelf			(pname: GLenum; param: GLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glLightModelfv			(pname: GLenum; params: PGLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glLightModelx			(pname: GLenum; param: GLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glLightModelxv			(pname: GLenum; params: PGLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glLineWidth				(width: GLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glLineWidthx			(width: GLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glLoadIdentity			(); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glLoadMatrixf			(m: PGLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glLoadMatrixx			(m: PGLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glLogicOp			(opcode: GLenum); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glMaterialf				(face: GLenum; pname: GLenum; param: GLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glMaterialfv			(face: GLenum; pname: GLenum; params: PGLfloat);  cdecl; external 'libGLES_CM_NoE.dll';
procedure	glMaterialx				(face: GLenum; pname: GLenum; param: GLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glMaterialxv			(face: GLenum; pname: GLenum; params: PGLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glMatrixMode			(mode: GLenum); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glMultiTexCoord4f		(target: GLenum; s: GLfloat; t: GLfloat;  r: GLfloat; q: GLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glMultiTexCoord4x		(target: GLenum; s: GLfixed; t: GLfixed; r: GLfixed; q: GLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glMultMatrixf			(m: PGLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glMultMatrixx			(m: PGLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glNormal3f			(nx: GLfloat; ny: GLfloat; nz: GLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glNormal3x			(nx: GLfixed; ny: GLfixed; nz: GLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glNormalPointer			(typee: GLenum; stride: GLsizei; ptr: PGLvoid); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glOrthof			 (left: GLfloat; right: GLfloat; bottom: GLfloat; top: GLfloat; near_val: GLfloat; far_val: GLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glOrthox				(left: GLfixed; right: GLfixed; bottom: GLfixed; top: GLfixed; near_val: GLfixed; far_val: GLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glPixelStorei			(pname: GLenum; param: GLint); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glPointSize			(size: GLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glPointSizex			(size: GLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glPolygonOffset			(factor: GLfloat; units: GLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glPolygonOffsetx		(factor: GLfixed; units: GLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glPopMatrix			(); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glPushMatrix			(); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glReadPixels			(x: GLint; y: GLint; width: GLsizei; height: GLsizei; format: GLenum; typee: GLenum; pixels: PGLvoid); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glRotatef				(angle: GLfloat; x: GLfloat; y: GLfloat; z: GLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glRotatex				(angle: GLfixed; x: GLfixed; y: GLfixed; z: GLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glSampleCoverage		(value: GLclampf; invert: GLboolean); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glSampleCoveragex		(value: GLclampx; invert: GLboolean); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glScalef				(x: GLfloat; y: GLfloat; z: GLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glScalex				(x: GLfixed; y: GLfixed; z: GLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glScissor				(x: GLint; y: GLsizei; width: GLsizei; height: GLint); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glShadeModel			(mode: GLenum); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glStencilFunc			(func: GLenum; ref: GLint; mask: GLuint); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glStencilMask			(mask: GLuint); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glStencilOp			(fail: GLenum; zfail: GLenum; zpass: GLenum); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glTexCoordPointer		(size: GLint; typee: GLenum; stride: GLsizei; ptr: PGLvoid); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glTexEnvf			(target: GLenum; pname: GLenum; param: GLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glTexEnvfv			(target: GLenum; pname: GLenum; params: PGLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glTexEnvx			(target: GLenum; pname: GLenum; param: GLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glTexEnvxv			(target: GLenum; pname: GLenum; params: PGLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glTexImage2D			(target: GLenum; level: GLint; internalFormat: GLint; width: GLsizei; height: GLsizei; border: GLint; format: GLenum; typee: GLenum; pixels: PGLvoid); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glTexParameterf			(target: GLenum; pname: GLenum; param: GLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glTexParameterx			(target: GLenum; pname: GLenum; param: GLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glTexSubImage2D			(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLsizei; width: GLsizei; height: GLint; format: GLenum; typee: GLenum; pixels: PGLvoid); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glTranslatef			(x: GLfloat; y: GLfloat; z: GLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glTranslatex			(x: GLfixed; y: GLfixed; z: GLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glVertexPointer			(size: GLint; typee: GLenum; stride: GLsizei; ptr: PGLvoid); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glViewport			(x: GLint; y: GLint; width: GLsizei; height: GLsizei); cdecl; external 'libGLES_CM_NoE.dll';

procedure	glBindBuffer							(target: GLenum; buffer: GLuint); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glBufferData							(target: GLenum; size: GLsizeiptr; data: PGLvoid; usage: GLenum); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glBufferSubData							(target: GLenum; offset: GLintptr; size: GLsizeiptr; data: PGLvoid); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glColor4ub								(red: GLubyte; green: GLubyte; blue: GLubyte; alpha: GLubyte); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glCurrentPaletteMatrixOES				(matrix: GLuint); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glDeleteBuffers							(n: GLsizei; buffers: PGLuint);  cdecl; external 'libGLES_CM_NoE.dll';
procedure	glGenBuffers							(n: GLsizei; buffers: PGLuint);  cdecl; external 'libGLES_CM_NoE.dll';
procedure	glGetBooleanv							(pname: GLenum; params: PGLboolean); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glGetBufferParameteriv					(target: GLenum; pname: GLenum; params: PGLint); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glGetClipPlanef							(plane: GLenum; equation: PGLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glGetClipPlanex							(plane: GLenum; equation: PGLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glGetFloatv								(pname: GLenum; params: PGLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glGetFixedv								(pname: GLenum; params: PGLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure glGetLightfv							(light: GLenum; pname: GLenum; params: PGLfloat);  cdecl; external 'libGLES_CM_NoE.dll';
procedure	glGetLightxv							(light: GLenum; pname: GLenum; params: PGLfixed);  cdecl; external 'libGLES_CM_NoE.dll';
procedure	glGetMaterialfv							(face: GLenum; pname: GLenum; params: PGLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glGetMaterialxv							(face: GLenum; pname: GLenum; params: PGLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glGetPointerv							(pname: GLenum; params: PPGLvoid); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glGetTexEnvfv							(target: GLenum; pname: GLenum; params: PGLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glGetTexEnviv							(target: GLenum; pname: GLenum; params: PGLint); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glGetTexEnvxv							(target: GLenum; pname: GLenum; params: PGLfixed);  cdecl; external 'libGLES_CM_NoE.dll';
procedure	glGetTexParameteriv						(target: GLenum; pname: GLenum; params: PGLint);  cdecl; external 'libGLES_CM_NoE.dll';
procedure	glGetTexParameterfv						(target: GLenum; pname: GLenum; params: PGLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glGetTexParameterxv						(target: GLenum; pname: GLenum; params: PGLfixed); cdecl; external 'libGLES_CM_NoE.dll';
function 	glIsEnabled								(cap: GLenum): GLboolean; cdecl; external 'libGLES_CM_NoE.dll';
function 	glIsTexture								(texture: GLuint): GLboolean; cdecl; external 'libGLES_CM_NoE.dll';
function        glIsBuffer								(buffer: GLuint): GLboolean; cdecl; external 'libGLES_CM_NoE.dll';
procedure	glLoadPaletteFromModelViewMatrixOES		(); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glMatrixIndexPointerOES					(size: GLint; typee: GLenum; stride: GLsizei; pointer: PGLvoid); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glWeightPointerOES						(size: GLint; typee: GLenum; stride: GLsizei; pointer: PGLvoid); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glClipPlanef							(plane: GLenum; equation: PGLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glClipPlanex							(plane: GLenum; equation: PGLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glPointSizePointerOES					(typee: GLenum; stride: GLsizei; pointer: PGLvoid); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glPointParameterfv						(pname: GLenum; params: PGLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glPointParameterxv						(pname: GLenum; params: PGLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glPointParameterf						(pname: GLenum; params: GLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glPointParameterx						(pname: GLenum; params: GLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glDrawTexfOES							(sx: GLfloat; sy: GLfloat; sz: GLfloat; sw: GLfloat; sh: GLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glDrawTexxOES							(sx: GLfixed; sy: GLfixed; sz: GLfixed; sw: GLfixed; sh: GLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glDrawTexiOES							(sx: GLint; sy: GLint; sz: GLint; sw: GLint; sh: GLint); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glDrawTexsOES							(sx: GLshort; sy: GLshort; sz: GLshort; sw: GLshort; sh: GLshort); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glDrawTexfvOES							(params: PGLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glDrawTexxvOES							(params: PGLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glDrawTexivOES							(params: PGLint); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glDrawTexsvOES							(params: PGLshort); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glTexEnvi								(target: GLenum; pname: GLenum; param: GLint); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glTexEnviv								(target: GLenum; pname: GLenum; params: PGLint); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glTexParameteri							(target: GLenum; pname: GLenum; param: GLint); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glTexParameterfv						(target: GLenum; pname: GLenum; param: PGLfloat); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glTexParameterxv						(target: GLenum; pname: GLenum; param: PGLfixed); cdecl; external 'libGLES_CM_NoE.dll';
procedure	glTexParameteriv						(target: GLenum; pname: GLenum; param: PGLint); cdecl; external 'libGLES_CM_NoE.dll';


//*-------------------------------------------------------------------------
// * Extensions
// *-----------------------------------------------------------------------*/

type
   glf16 = array[0..15] of GLfixed;
   gli16 = array[0..15] of GLint;



function glQueryMatrixxOES(mantissa: glf16; exponent: gli16): GLbitfield; cdecl; external 'libGLES_CM_NoE.dll';


implementation


end.


