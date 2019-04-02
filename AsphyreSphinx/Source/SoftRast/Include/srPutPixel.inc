asm
 push edi
 // General Programming idea:
 // MM0 <- source color
 // MM1 <- destination color

 // MM2 <- [unused - generic use]
 // MM3 <- Primary Diffuse Color
 // MM4 <- Diffuse Color 1
 // MM5 <- Diffuse Color 2
 // MM6 <- source alpha-channel, unpacked (specify LoadSourceAlpha symbol!)

 // EAX <- Source Color
 // EBX <- Destination Pointer
 // MM7 <- zero-based register
 mov   eax, Color
 movd  mm0, eax
 mov   edi, DestPtr
 pxor  mm7, mm7

//---------------------------------------------------------------------------
// 1. MM1 <- destination pixel
//---------------------------------------------------------------------------
 {$IFDEF LoadDestPixel}
 movd  mm1, [edi]
 {$ENDIF}

//---------------------------------------------------------------------------
// 2. Invert Source color?
//---------------------------------------------------------------------------
 {$IFDEF InvertSourceColor}
 mov   ecx, 0FFFFFFh
 movd  mm2, ecx
 psubb mm2, mm0
 movq  mm0, mm2
 {$ENDIF}

//---------------------------------------------------------------------------
// 4. Load alpha-channel to MM6
//---------------------------------------------------------------------------
 {$IFDEF LoadSourceAlpha}
 shr eax, 24

 // 4a. Invert Alpha-channel?
 {$IFDEF InvertSourceAlpha}
 mov ecx, 0FFh
 sub ecx, eax
 mov eax, ecx
 {$ENDIF}

 mov ecx, eax
 shl ecx, 8
 or  eax, ecx
 shl ecx, 8
 or  eax, ecx

 // MM6 <- Alpha-Channel
 movd mm6, eax
 punpcklbw mm6, mm7
 {$ENDIF}

//---------------------------------------------------------------------------
// 5. Multiply Source Color by Alpha-Channel
//---------------------------------------------------------------------------
 {$IFDEF SourceMulAlpha}
 punpcklbw mm0, mm7

 pmullw mm0, mm6
 psrlw mm0, 8

 packuswb mm0, mm0
 {$ENDIF}

//---------------------------------------------------------------------------
// 6. Multiply Destination Color by inverse Alpha-Channel
//---------------------------------------------------------------------------
 {$IFDEF DestMulAlpha}
 mov ecx, 0FFFFFFh
 movd mm2, ecx
 punpcklbw mm2, mm7
 psubw mm2, mm6

 punpcklbw mm1, mm7

 pmullw mm1, mm2
 psrlw mm1, 8

 packuswb mm1, mm7
 {$ENDIF}

//---------------------------------------------------------------------------
// 7. Multiply Destination Color by Source Color
//---------------------------------------------------------------------------
 {$IFDEF DestMulSource}
 punpcklbw mm0, mm7
 punpcklbw mm1, mm7

 pmullw mm1, mm0
 psrlw mm1, 8

 packuswb mm0, mm7
 packuswb mm1, mm7
 {$ENDIF}

//---------------------------------------------------------------------------
// (1) Operation: Move
//---------------------------------------------------------------------------
 {$IFDEF OpMove}
 movd [edi], mm0
 {$ENDIF}

//---------------------------------------------------------------------------
// (2) Operation: Add
//---------------------------------------------------------------------------
 {$IFDEF OpAdd}
 paddusb mm0, mm1
 movd [edi], mm0
 {$ENDIF}

//---------------------------------------------------------------------------
// (3) Operation: Destination
//---------------------------------------------------------------------------
 {$IFDEF OpDest}
 movd [edi], mm1
 {$ENDIF}

 // restore FPU state
 emms

 // restore general registers
 pop edi
end;
{$UNDEF ApplyDiffuse}
{$UNDEF SimpleLoad}
{$UNDEF StretchLoad}
{$UNDEF LoadDestPixel}
{$UNDEF InvertSourceColor}
{$UNDEF LoadSourceAlpha}
{$UNDEF InvertSourceAlpha}
{$UNDEF SourceMulAlpha}
{$UNDEF DestMulAlpha}
{$UNDEF DestMulSource}
{$UNDEF OpMove}
{$UNDEF OpAdd}
{$UNDEF OpDest}






