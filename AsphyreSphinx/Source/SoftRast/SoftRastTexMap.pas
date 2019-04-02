unit SoftRastTexMap;

//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 Windows, SoftRastTypes;

//---------------------------------------------------------------------------
type
 TTexUVLineFx = procedure(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);

//---------------------------------------------------------------------------
function SRTexUVLineFx(Op: Integer): TTexUVLineFx; stdcall;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
procedure TexUVLineaa(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE srMove}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLinebb(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE OpAdd}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLinecc(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE OpDest}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLinecc_A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpDest}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLinecc_S(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE OpDest}
{$DEFINE DestMulSource}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineccia_A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpDest}
{$DEFINE InvertSourceAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineccG__A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpDest}
{$DEFINE SrcAlphaGrey}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineccG_ia_A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpDest}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLinebbA_(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpAdd}
{$DEFINE SourceMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLinebb_A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpAdd}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLinebb_S(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE OpAdd}
{$DEFINE DestMulSource}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLinebbA__A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpAdd}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLinebbiaA_(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpAdd}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLinebbia_A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpAdd}
{$DEFINE InvertSourceAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLinebbiaA__A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpAdd}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLinebbG_A_(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE SourceMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLinebbG__A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLinebbG_A__A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLinebbG_iaA_(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLinebbG_ia_A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLinebbG_iaA__A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineI_aa(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE InvertSourceColor}
{$DEFINE srMove}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineI_bb(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineI_cc_S(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpDest}
{$DEFINE DestMulSource}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineI_ccG__A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpDest}
{$DEFINE SrcAlphaGrey}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineI_ccG_ia_A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpDest}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineI_bbA_(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SourceMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineI_bb_A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineI_bb_S(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE DestMulSource}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineI_bbA__A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineI_bbiaA_(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineI_bbia_A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE InvertSourceAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineI_bbiaA__A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineI_bbG_A_(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE SourceMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineI_bbG__A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineI_bbG_A__A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineI_bbG_iaA_(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineI_bbG_ia_A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineI_bbG_iaA__A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_aa(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE ApplyDiffuse}
{$DEFINE srMove}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_bb(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_cc(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpDest}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_cc_A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpDest}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_cc_S(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpDest}
{$DEFINE DestMulSource}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_ccia_A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpDest}
{$DEFINE InvertSourceAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_ccG__A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpDest}
{$DEFINE SrcAlphaGrey}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_ccG_ia_A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpDest}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_bbA_(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE SourceMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_bb_A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_bb_S(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE DestMulSource}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_bbA__A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_bbiaA_(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_bbia_A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE InvertSourceAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_bbiaA__A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_bbG_A_(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE SourceMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_bbG__A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_bbG_A__A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_bbG_iaA_(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_bbG_ia_A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_bbG_iaA__A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_I_aa(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE srMove}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_I_bb(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_I_cc_S(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpDest}
{$DEFINE DestMulSource}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_I_ccG__A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpDest}
{$DEFINE SrcAlphaGrey}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_I_ccG_ia_A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpDest}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_I_bbA_(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SourceMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_I_bb_A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_I_bb_S(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE DestMulSource}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_I_bbA__A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_I_bbiaA_(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_I_bbia_A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE InvertSourceAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_I_bbiaA__A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_I_bbG_A_(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE SourceMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_I_bbG__A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_I_bbG_A__A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_I_bbG_iaA_(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_I_bbG_ia_A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLineD_I_bbG_iaA__A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLinePPD_aa(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE NoTexture}
{$DEFINE ApplyDiffuse}
{$DEFINE srMove}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLinePPD_bb(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE NoTexture}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLinePPD_cc(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE NoTexture}
{$DEFINE ApplyDiffuse}
{$DEFINE OpDest}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLinePPD_cc_A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE NoTexture}
{$DEFINE ApplyDiffuse}
{$DEFINE OpDest}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLinePPD_cc_S(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE NoTexture}
{$DEFINE ApplyDiffuse}
{$DEFINE OpDest}
{$DEFINE DestMulSource}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLinePPD_bbA_(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE NoTexture}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE SourceMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLinePPD_bb_A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE NoTexture}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLinePPD_bb_S(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE NoTexture}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE DestMulSource}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
procedure TexUVLinePPD_bbA__A(Dest, Source: Pointer; Count: Cardinal; u, v, uDelta,
vDelta, Pitch: Integer; Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE NoTexture}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexUVLineFx.inc}

//---------------------------------------------------------------------------
function SRTexUVLineFx(Op: Integer): TTexUVLineFx; stdcall;
begin
 if (Op and srNoTex > 0) then
  begin
       if (Op and srMove > 0) then
        begin
                       Result:= TexUVLinePPD_aa;
        end else
        begin
         if (Op and srAdd > 0) then
          begin
                 if (Op and srSrcAlpha > 0) then
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexUVLinePPD_bbA__A;
                    end else
                    begin
                       Result:= TexUVLinePPD_bbA_;
                    end;
                  end else
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexUVLinePPD_bb_A;
                    end else
                    begin
                     if (Op and srDestSrc > 0) then
                      begin
                       Result:= TexUVLinePPD_bb_S;
                      end else
                      begin
                       Result:= TexUVLinePPD_bb;
                      end;
                    end;
                  end;
          end else
          begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexUVLinePPD_cc_A;
                    end else
                    begin
                     if (Op and srDestSrc > 0) then
                      begin
                       Result:= TexUVLinePPD_cc_S;
                      end else
                      begin
                       Result:= TexUVLinePPD_cc;
                      end;
                    end;
          end;
        end;
  end else
  begin
   if (Op and srDiffuse > 0) then
    begin
     if (Op and srSrcInvert > 0) then
      begin
       if (Op and srMove > 0) then
        begin
                       Result:= TexUVLineD_I_aa;
        end else
        begin
         if (Op and srAdd > 0) then
          begin
             if (Op and srAlphaGrey > 0) then
              begin
               if (Op and srInvAlpha > 0) then
                begin
                 if (Op and srSrcAlpha > 0) then
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexUVLineD_I_bbG_iaA__A;
                    end else
                    begin
                       Result:= TexUVLineD_I_bbG_iaA_;
                    end;
                  end else
                  begin
                       Result:= TexUVLineD_I_bbG_ia_A;
                  end;
                end else
                begin
                 if (Op and srSrcAlpha > 0) then
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexUVLineD_I_bbG_A__A;
                    end else
                    begin
                       Result:= TexUVLineD_I_bbG_A_;
                    end;
                  end else
                  begin
                       Result:= TexUVLineD_I_bbG__A;
                  end;
                end;
              end else
              begin
               if (Op and srInvAlpha > 0) then
                begin
                 if (Op and srSrcAlpha > 0) then
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexUVLineD_I_bbiaA__A;
                    end else
                    begin
                       Result:= TexUVLineD_I_bbiaA_;
                    end;
                  end else
                  begin
                       Result:= TexUVLineD_I_bbia_A;
                  end;
                end else
                begin
                 if (Op and srSrcAlpha > 0) then
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexUVLineD_I_bbA__A;
                    end else
                    begin
                       Result:= TexUVLineD_I_bbA_;
                    end;
                  end else
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexUVLineD_I_bb_A;
                    end else
                    begin
                     if (Op and srDestSrc > 0) then
                      begin
                       Result:= TexUVLineD_I_bb_S;
                      end else
                      begin
                       Result:= TexUVLineD_I_bb;
                      end;
                    end;
                  end;
                end;
              end;
          end else
          begin
             if (Op and srAlphaGrey > 0) then
              begin
               if (Op and srInvAlpha > 0) then
                begin
                       Result:= TexUVLineD_I_ccG_ia_A;
                end else
                begin
                       Result:= TexUVLineD_I_ccG__A;
                end;
              end else
              begin
                       Result:= TexUVLineD_I_cc_S;
              end;
          end;
        end;
      end else
      begin
       if (Op and srMove > 0) then
        begin
                       Result:= TexUVLineD_aa;
        end else
        begin
         if (Op and srAdd > 0) then
          begin
             if (Op and srAlphaGrey > 0) then
              begin
               if (Op and srInvAlpha > 0) then
                begin
                 if (Op and srSrcAlpha > 0) then
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexUVLineD_bbG_iaA__A;
                    end else
                    begin
                       Result:= TexUVLineD_bbG_iaA_;
                    end;
                  end else
                  begin
                       Result:= TexUVLineD_bbG_ia_A;
                  end;
                end else
                begin
                 if (Op and srSrcAlpha > 0) then
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexUVLineD_bbG_A__A;
                    end else
                    begin
                       Result:= TexUVLineD_bbG_A_;
                    end;
                  end else
                  begin
                       Result:= TexUVLineD_bbG__A;
                  end;
                end;
              end else
              begin
               if (Op and srInvAlpha > 0) then
                begin
                 if (Op and srSrcAlpha > 0) then
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexUVLineD_bbiaA__A;
                    end else
                    begin
                       Result:= TexUVLineD_bbiaA_;
                    end;
                  end else
                  begin
                       Result:= TexUVLineD_bbia_A;
                  end;
                end else
                begin
                 if (Op and srSrcAlpha > 0) then
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexUVLineD_bbA__A;
                    end else
                    begin
                       Result:= TexUVLineD_bbA_;
                    end;
                  end else
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexUVLineD_bb_A;
                    end else
                    begin
                     if (Op and srDestSrc > 0) then
                      begin
                       Result:= TexUVLineD_bb_S;
                      end else
                      begin
                       Result:= TexUVLineD_bb;
                      end;
                    end;
                  end;
                end;
              end;
          end else
          begin
             if (Op and srAlphaGrey > 0) then
              begin
               if (Op and srInvAlpha > 0) then
                begin
                       Result:= TexUVLineD_ccG_ia_A;
                end else
                begin
                       Result:= TexUVLineD_ccG__A;
                end;
              end else
              begin
               if (Op and srInvAlpha > 0) then
                begin
                       Result:= TexUVLineD_ccia_A;
                end else
                begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexUVLineD_cc_A;
                    end else
                    begin
                     if (Op and srDestSrc > 0) then
                      begin
                       Result:= TexUVLineD_cc_S;
                      end else
                      begin
                       Result:= TexUVLineD_cc;
                      end;
                    end;
                end;
              end;
          end;
        end;
      end;
    end else
    begin
     if (Op and srSrcInvert > 0) then
      begin
       if (Op and srMove > 0) then
        begin
                       Result:= TexUVLineI_aa;
        end else
        begin
         if (Op and srAdd > 0) then
          begin
             if (Op and srAlphaGrey > 0) then
              begin
               if (Op and srInvAlpha > 0) then
                begin
                 if (Op and srSrcAlpha > 0) then
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexUVLineI_bbG_iaA__A;
                    end else
                    begin
                       Result:= TexUVLineI_bbG_iaA_;
                    end;
                  end else
                  begin
                       Result:= TexUVLineI_bbG_ia_A;
                  end;
                end else
                begin
                 if (Op and srSrcAlpha > 0) then
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexUVLineI_bbG_A__A;
                    end else
                    begin
                       Result:= TexUVLineI_bbG_A_;
                    end;
                  end else
                  begin
                       Result:= TexUVLineI_bbG__A;
                  end;
                end;
              end else
              begin
               if (Op and srInvAlpha > 0) then
                begin
                 if (Op and srSrcAlpha > 0) then
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexUVLineI_bbiaA__A;
                    end else
                    begin
                       Result:= TexUVLineI_bbiaA_;
                    end;
                  end else
                  begin
                       Result:= TexUVLineI_bbia_A;
                  end;
                end else
                begin
                 if (Op and srSrcAlpha > 0) then
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexUVLineI_bbA__A;
                    end else
                    begin
                       Result:= TexUVLineI_bbA_;
                    end;
                  end else
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexUVLineI_bb_A;
                    end else
                    begin
                     if (Op and srDestSrc > 0) then
                      begin
                       Result:= TexUVLineI_bb_S;
                      end else
                      begin
                       Result:= TexUVLineI_bb;
                      end;
                    end;
                  end;
                end;
              end;
          end else
          begin
             if (Op and srAlphaGrey > 0) then
              begin
               if (Op and srInvAlpha > 0) then
                begin
                       Result:= TexUVLineI_ccG_ia_A;
                end else
                begin
                       Result:= TexUVLineI_ccG__A;
                end;
              end else
              begin
                       Result:= TexUVLineI_cc_S;
              end;
          end;
        end;
      end else
      begin
       if (Op and srMove > 0) then
        begin
                       Result:= TexUVLineaa;
        end else
        begin
         if (Op and srAdd > 0) then
          begin
             if (Op and srAlphaGrey > 0) then
              begin
               if (Op and srInvAlpha > 0) then
                begin
                 if (Op and srSrcAlpha > 0) then
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexUVLinebbG_iaA__A;
                    end else
                    begin
                       Result:= TexUVLinebbG_iaA_;
                    end;
                  end else
                  begin
                       Result:= TexUVLinebbG_ia_A;
                  end;
                end else
                begin
                 if (Op and srSrcAlpha > 0) then
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexUVLinebbG_A__A;
                    end else
                    begin
                       Result:= TexUVLinebbG_A_;
                    end;
                  end else
                  begin
                       Result:= TexUVLinebbG__A;
                  end;
                end;
              end else
              begin
               if (Op and srInvAlpha > 0) then
                begin
                 if (Op and srSrcAlpha > 0) then
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexUVLinebbiaA__A;
                    end else
                    begin
                       Result:= TexUVLinebbiaA_;
                    end;
                  end else
                  begin
                       Result:= TexUVLinebbia_A;
                  end;
                end else
                begin
                 if (Op and srSrcAlpha > 0) then
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexUVLinebbA__A;
                    end else
                    begin
                       Result:= TexUVLinebbA_;
                    end;
                  end else
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexUVLinebb_A;
                    end else
                    begin
                     if (Op and srDestSrc > 0) then
                      begin
                       Result:= TexUVLinebb_S;
                      end else
                      begin
                       Result:= TexUVLinebb;
                      end;
                    end;
                  end;
                end;
              end;
          end else
          begin
             if (Op and srAlphaGrey > 0) then
              begin
               if (Op and srInvAlpha > 0) then
                begin
                       Result:= TexUVLineccG_ia_A;
                end else
                begin
                       Result:= TexUVLineccG__A;
                end;
              end else
              begin
               if (Op and srInvAlpha > 0) then
                begin
                       Result:= TexUVLineccia_A;
                end else
                begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexUVLinecc_A;
                    end else
                    begin
                     if (Op and srDestSrc > 0) then
                      begin
                       Result:= TexUVLinecc_S;
                      end else
                      begin
                       Result:= TexUVLinecc;
                      end;
                    end;
                end;
              end;
          end;
        end;
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------
end.
