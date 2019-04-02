unit SoftRastTexRect;

//---------------------------------------------------------------------------

interface

//---------------------------------------------------------------------------
uses
 Windows, SoftRastTypes;

//---------------------------------------------------------------------------
type
 TTexStretchLineFx = procedure(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);

//---------------------------------------------------------------------------
function SRTexStretchLineFx(Op: Integer): TTexStretchLineFx; stdcall;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
procedure TexStretchLineaa(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE OpMove}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLinebb(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE OpAdd}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLinecc(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE OpDest}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLinecc_A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpDest}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLinecc_S(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE OpDest}
{$DEFINE DestMulSource}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineccia_A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpDest}
{$DEFINE InvertSourceAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineccG__A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpDest}
{$DEFINE SrcAlphaGrey}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineccG_ia_A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpDest}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLinebbA_(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpAdd}
{$DEFINE SourceMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLinebb_A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpAdd}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLinebb_S(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE OpAdd}
{$DEFINE DestMulSource}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLinebbA__A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpAdd}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLinebbiaA_(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpAdd}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLinebbia_A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpAdd}
{$DEFINE InvertSourceAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLinebbiaA__A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpAdd}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLinebbG_A_(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE SourceMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLinebbG__A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLinebbG_A__A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLinebbG_iaA_(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLinebbG_ia_A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLinebbG_iaA__A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineI_aa(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE InvertSourceColor}
{$DEFINE OpMove}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineI_bb(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineI_cc_S(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpDest}
{$DEFINE DestMulSource}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineI_ccG__A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpDest}
{$DEFINE SrcAlphaGrey}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineI_ccG_ia_A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpDest}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineI_bbA_(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SourceMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineI_bb_A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineI_bb_S(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE DestMulSource}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineI_bbA__A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineI_bbiaA_(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineI_bbia_A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE InvertSourceAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineI_bbiaA__A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal;
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineI_bbG_A_(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE SourceMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineI_bbG__A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineI_bbG_A__A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineI_bbG_iaA_(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineI_bbG_ia_A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineI_bbG_iaA__A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_aa(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE ApplyDiffuse}
{$DEFINE OpMove}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_bb(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_cc(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal;
 Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpDest}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_cc_A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpDest}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_cc_S(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpDest}
{$DEFINE DestMulSource}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_ccia_A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpDest}
{$DEFINE InvertSourceAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_ccG__A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpDest}
{$DEFINE SrcAlphaGrey}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_ccG_ia_A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal;
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpDest}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_bbA_(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE SourceMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_bb_A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_bb_S(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE DestMulSource}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_bbA__A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_bbiaA_(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal;
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_bbia_A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE InvertSourceAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_bbiaA__A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_bbG_A_(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE SourceMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_bbG__A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_bbG_A__A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_bbG_iaA_(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_bbG_ia_A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_bbG_iaA__A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_I_aa(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpMove}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_I_bb(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_I_cc_S(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpDest}
{$DEFINE DestMulSource}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_I_ccG__A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpDest}
{$DEFINE SrcAlphaGrey}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_I_ccG_ia_A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal;
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpDest}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_I_bbA_(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SourceMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_I_bb_A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_I_bb_S(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE DestMulSource}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_I_bbA__A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_I_bbiaA_(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_I_bbia_A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE InvertSourceAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_I_bbiaA__A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_I_bbG_A_(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE SourceMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_I_bbG__A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_I_bbG_A__A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_I_bbG_iaA_(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_I_bbG_ia_A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLineD_I_bbG_iaA__A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE ApplyDiffuse}
{$DEFINE InvertSourceColor}
{$DEFINE OpAdd}
{$DEFINE SrcAlphaGrey}
{$DEFINE InvertSourceAlpha}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLinePPD_aa(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE NoTexture}
{$DEFINE ApplyDiffuse}
{$DEFINE OpMove}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLinePPD_bb(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE NoTexture}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLinePPD_cc(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE NoTexture}
{$DEFINE ApplyDiffuse}
{$DEFINE OpDest}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLinePPD_cc_A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE NoTexture}
{$DEFINE ApplyDiffuse}
{$DEFINE OpDest}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLinePPD_cc_S(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE NoTexture}
{$DEFINE ApplyDiffuse}
{$DEFINE OpDest}
{$DEFINE DestMulSource}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLinePPD_bbA_(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE NoTexture}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE SourceMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLinePPD_bb_A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE NoTexture}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLinePPD_bb_S(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadDestPixel}
{$DEFINE NoTexture}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE DestMulSource}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
procedure TexStretchLinePPD_bbA__A(Dest, Source: Pointer; Count, SrcPos, SrcMove: Cardinal; 
 Color1, Color2: Cardinal);
{$DEFINE LoadSourceAlpha}
{$DEFINE LoadDestPixel}
{$DEFINE NoTexture}
{$DEFINE ApplyDiffuse}
{$DEFINE OpAdd}
{$DEFINE SourceMulAlpha}
{$DEFINE DestMulAlpha}
{$include include\srTexLineStretchFx.inc}

//---------------------------------------------------------------------------
function SRTexStretchLineFx(Op: Integer): TTexStretchLineFx; stdcall;
begin
 if (Op and srNoTex > 0) then
  begin
       if (Op and srMove > 0) then
        begin
                       Result:= TexStretchLinePPD_aa;
        end else
        begin
         if (Op and srAdd > 0) then
          begin
                 if (Op and srSrcAlpha > 0) then
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexStretchLinePPD_bbA__A;
                    end else
                    begin
                       Result:= TexStretchLinePPD_bbA_;
                    end;
                  end else
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexStretchLinePPD_bb_A;
                    end else
                    begin
                     if (Op and srDestSrc > 0) then
                      begin
                       Result:= TexStretchLinePPD_bb_S;
                      end else
                      begin
                       Result:= TexStretchLinePPD_bb;
                      end;
                    end;
                  end;
          end else
          begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexStretchLinePPD_cc_A;
                    end else
                    begin
                     if (Op and srDestSrc > 0) then
                      begin
                       Result:= TexStretchLinePPD_cc_S;
                      end else
                      begin
                       Result:= TexStretchLinePPD_cc;
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
                       Result:= TexStretchLineD_I_aa;
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
                       Result:= TexStretchLineD_I_bbG_iaA__A;
                    end else
                    begin
                       Result:= TexStretchLineD_I_bbG_iaA_;
                    end;
                  end else
                  begin
                       Result:= TexStretchLineD_I_bbG_ia_A;
                  end;
                end else
                begin
                 if (Op and srSrcAlpha > 0) then
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexStretchLineD_I_bbG_A__A;
                    end else
                    begin
                       Result:= TexStretchLineD_I_bbG_A_;
                    end;
                  end else
                  begin
                       Result:= TexStretchLineD_I_bbG__A;
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
                       Result:= TexStretchLineD_I_bbiaA__A;
                    end else
                    begin
                       Result:= TexStretchLineD_I_bbiaA_;
                    end;
                  end else
                  begin
                       Result:= TexStretchLineD_I_bbia_A;
                  end;
                end else
                begin
                 if (Op and srSrcAlpha > 0) then
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexStretchLineD_I_bbA__A;
                    end else
                    begin
                       Result:= TexStretchLineD_I_bbA_;
                    end;
                  end else
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexStretchLineD_I_bb_A;
                    end else
                    begin
                     if (Op and srDestSrc > 0) then
                      begin
                       Result:= TexStretchLineD_I_bb_S;
                      end else
                      begin
                       Result:= TexStretchLineD_I_bb;
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
                       Result:= TexStretchLineD_I_ccG_ia_A;
                end else
                begin
                       Result:= TexStretchLineD_I_ccG__A;
                end;
              end else
              begin
                       Result:= TexStretchLineD_I_cc_S;
              end;
          end;
        end;
      end else
      begin
       if (Op and srMove > 0) then
        begin
                       Result:= TexStretchLineD_aa;
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
                       Result:= TexStretchLineD_bbG_iaA__A;
                    end else
                    begin
                       Result:= TexStretchLineD_bbG_iaA_;
                    end;
                  end else
                  begin
                       Result:= TexStretchLineD_bbG_ia_A;
                  end;
                end else
                begin
                 if (Op and srSrcAlpha > 0) then
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexStretchLineD_bbG_A__A;
                    end else
                    begin
                       Result:= TexStretchLineD_bbG_A_;
                    end;
                  end else
                  begin
                       Result:= TexStretchLineD_bbG__A;
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
                       Result:= TexStretchLineD_bbiaA__A;
                    end else
                    begin
                       Result:= TexStretchLineD_bbiaA_;
                    end;
                  end else
                  begin
                       Result:= TexStretchLineD_bbia_A;
                  end;
                end else
                begin
                 if (Op and srSrcAlpha > 0) then
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexStretchLineD_bbA__A;
                    end else
                    begin
                       Result:= TexStretchLineD_bbA_;
                    end;
                  end else
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexStretchLineD_bb_A;
                    end else
                    begin
                     if (Op and srDestSrc > 0) then
                      begin
                       Result:= TexStretchLineD_bb_S;
                      end else
                      begin
                       Result:= TexStretchLineD_bb;
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
                       Result:= TexStretchLineD_ccG_ia_A;
                end else
                begin
                       Result:= TexStretchLineD_ccG__A;
                end;
              end else
              begin
               if (Op and srInvAlpha > 0) then
                begin
                       Result:= TexStretchLineD_ccia_A;
                end else
                begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexStretchLineD_cc_A;
                    end else
                    begin
                     if (Op and srDestSrc > 0) then
                      begin
                       Result:= TexStretchLineD_cc_S;
                      end else
                      begin
                       Result:= TexStretchLineD_cc;
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
                       Result:= TexStretchLineI_aa;
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
                       Result:= TexStretchLineI_bbG_iaA__A;
                    end else
                    begin
                       Result:= TexStretchLineI_bbG_iaA_;
                    end;
                  end else
                  begin
                       Result:= TexStretchLineI_bbG_ia_A;
                  end;
                end else
                begin
                 if (Op and srSrcAlpha > 0) then
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexStretchLineI_bbG_A__A;
                    end else
                    begin
                       Result:= TexStretchLineI_bbG_A_;
                    end;
                  end else
                  begin
                       Result:= TexStretchLineI_bbG__A;
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
                       Result:= TexStretchLineI_bbiaA__A;
                    end else
                    begin
                       Result:= TexStretchLineI_bbiaA_;
                    end;
                  end else
                  begin
                       Result:= TexStretchLineI_bbia_A;
                  end;
                end else
                begin
                 if (Op and srSrcAlpha > 0) then
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexStretchLineI_bbA__A;
                    end else
                    begin
                       Result:= TexStretchLineI_bbA_;
                    end;
                  end else
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexStretchLineI_bb_A;
                    end else
                    begin
                     if (Op and srDestSrc > 0) then
                      begin
                       Result:= TexStretchLineI_bb_S;
                      end else
                      begin
                       Result:= TexStretchLineI_bb;
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
                       Result:= TexStretchLineI_ccG_ia_A;
                end else
                begin
                       Result:= TexStretchLineI_ccG__A;
                end;
              end else
              begin
                       Result:= TexStretchLineI_cc_S;
              end;
          end;
        end;
      end else
      begin
       if (Op and srMove > 0) then
        begin
                       Result:= TexStretchLineaa;
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
                       Result:= TexStretchLinebbG_iaA__A;
                    end else
                    begin
                       Result:= TexStretchLinebbG_iaA_;
                    end;
                  end else
                  begin
                       Result:= TexStretchLinebbG_ia_A;
                  end;
                end else
                begin
                 if (Op and srSrcAlpha > 0) then
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexStretchLinebbG_A__A;
                    end else
                    begin
                       Result:= TexStretchLinebbG_A_;
                    end;
                  end else
                  begin
                       Result:= TexStretchLinebbG__A;
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
                       Result:= TexStretchLinebbiaA__A;
                    end else
                    begin
                       Result:= TexStretchLinebbiaA_;
                    end;
                  end else
                  begin
                       Result:= TexStretchLinebbia_A;
                  end;
                end else
                begin
                 if (Op and srSrcAlpha > 0) then
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexStretchLinebbA__A;
                    end else
                    begin
                       Result:= TexStretchLinebbA_;
                    end;
                  end else
                  begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexStretchLinebb_A;
                    end else
                    begin
                     if (Op and srDestSrc > 0) then
                      begin
                       Result:= TexStretchLinebb_S;
                      end else
                      begin
                       Result:= TexStretchLinebb;
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
                       Result:= TexStretchLineccG_ia_A;
                end else
                begin
                       Result:= TexStretchLineccG__A;
                end;
              end else
              begin
               if (Op and srInvAlpha > 0) then
                begin
                       Result:= TexStretchLineccia_A;
                end else
                begin
                   if (Op and srDestAlpha > 0) then
                    begin
                       Result:= TexStretchLinecc_A;
                    end else
                    begin
                     if (Op and srDestSrc > 0) then
                      begin
                       Result:= TexStretchLinecc_S;
                      end else
                      begin
                       Result:= TexStretchLinecc;
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
