asm
 // General Programming idea:
 // MM0 <- source color
 // MM1 <- destination color
 // MM2 <- [unused - generic use]
 // MM3 <- Primary Diffuse Color
 // MM4 <- Diffuse Color 1
 // MM5 <- Diffuse Color 2
 // MM6 <- source alpha-channel, unpacked (specify LoadSourceAlpha symbol!)
 // MM7 <- zero-based register

 // EAX <- source color
 // ECX <- general use

 // preserve general registers
 push edi
 push esi
 push ebx

 // ESI : EDI - source : destination
 {$IFNDEF NoTexture}
 mov esi, Source
 {$ENDIF}
 mov edi, Dest

 // EBX - Fixed-Point Position (16.16)
 // EDX - Fixed-Point Change (16.16)
 {$IFNDEF NoTexture}
 {$IFDEF StretchLoad}
 mov ebx, u
 mov edx, v
 {$ENDIF}
 {$ENDIF}

 // MM7 - zero register (used to for unpack operations)
 pxor mm7, mm7

 {$IFDEF ApplyDiffuse}
 movd mm4, Color1
 punpcklbw mm4, mm7
 movd mm5, Color2
 punpcklbw mm5, mm7
 {$ENDIF}

//---------------------------------------------------------------------------
// FX Loop
//---------------------------------------------------------------------------
@PixelsFx:
//---------------------------------------------------------------------------
// (a) MM3 <- Primary Diffuse Color
//---------------------------------------------------------------------------
 {$IFDEF ApplyDiffuse}
 mov eax, ColorChange
 mov ecx, eax

 shr eax, 16
 shl ecx, 16
 shr ecx, 16
 add ax, cx
 shl eax, 16
 or eax, ecx

 mov ColorChange, eax
 shr eax, 24

 mov ecx, eax
 shl ecx, 8
 or  eax, ecx
 shl ecx, 8
 or  eax, ecx
 shl ecx, 8
 or eax, ecx

 // MM2 - Alpha0
 movd mm2, eax
 punpcklbw mm2, mm7

 // MM3 - Alpha2
 shr eax, 24
 mov ecx, 0FFh
 sub ecx, eax
 mov eax, ecx
 shl ecx, 8
 or eax, ecx
 shl ecx, 8
 or eax, ecx

 movd mm3, eax
 punpcklbw mm3, mm7

 // Multiply both Alphas
 pmullw mm2, mm4
 psrlw mm2, 8
 packuswb mm2, mm7

 pmullw mm3, mm5
 psrlw mm3, 8
 packuswb mm3, mm7

 paddusb mm3, mm2

 {$IFDEF NoTexture}
 movd eax, mm3
 mov  ecx, eax
 movq mm0, mm3
 {$ENDIF}

 punpcklbw mm3, mm7

 {$ENDIF}
//---------------------------------------------------------------------------
// (b) EAX, MM0 <- source pixel (Simple Load)
//---------------------------------------------------------------------------
 {$IFNDEF NoTexture}
 {$IFDEF SimpleLoad}
 mov eax, [esi]
 movd mm0, eax
 {$ENDIF}

//---------------------------------------------------------------------------
// (c) EAX, MM0 <- source pixel (Stretch Load)
//---------------------------------------------------------------------------
 {$IFDEF StretchLoad}
 mov eax, edx
 shr eax, 16
 mov ecx, Pitch
 imul eax, ecx
 mov ecx, ebx
 shr ecx, 16
 shl ecx, 2
 add eax, ecx
 add eax, esi

 mov eax, [eax]
 movd mm0, eax
 {$ENDIF}
 {$ENDIF}

//---------------------------------------------------------------------------
// 1. MM1 <- destination pixel
//---------------------------------------------------------------------------
 {$IFDEF LoadDestPixel}
 movd mm1, [edi]
 {$ENDIF}

//---------------------------------------------------------------------------
// 2. Invert Source color?
//---------------------------------------------------------------------------
 {$IFDEF InvertSourceColor}
 mov ecx, 0FFFFFFh
 movd mm2, ecx
 psubb mm2, mm0
 movq mm0, mm2


 {$ENDIF}

//---------------------------------------------------------------------------
// 3. Multiply Source color by Diffuse Color
//---------------------------------------------------------------------------
 {$IFDEF ApplyDiffuse}
 {$IFNDEF NoTexture}
 punpcklbw mm0, mm7

 pmullw mm0, mm3
 psrlw mm0, 8

 packuswb mm0, mm0
 {$ENDIF}
 {$ENDIF}

//---------------------------------------------------------------------------
// 4a. Make an alpha-channel based on Source Color
//---------------------------------------------------------------------------
 {$IFDEF SrcAlphaGrey}
 punpcklbw mm0, mm7
 mov ecx, 0050803h
 movd mm2, ecx
 punpcklbw mm2, mm7

 pmullw mm2, mm0
 psrlw mm2, 8

 movd ecx, mm2
 movd mm2, eax

 mov eax, ecx
 shr ecx, 8
 add eax, ecx
 shr ecx, 8
 add eax, ecx
 shl eax, 24
 mov ecx, eax
 movd eax, mm2
 and eax, 0FFFFFFh
 or eax, ecx
 movd mm0, eax
 mov ecx, eax
 {$ENDIF}

//---------------------------------------------------------------------------
// 4b. Load alpha-channel to MM6
//---------------------------------------------------------------------------
 {$IFDEF LoadSourceAlpha}
 movd eax, mm0
 shr eax, 24

 // 4a. Invert Alpha-channel?
 {$IFDEF InvertSourceAlpha}
 mov ecx, 0FFh
 sub ecx, eax
 mov eax, ecx
 {$ENDIF}

 mov ecx, eax
 shl ecx, 8
 or eax, ecx
 shl ecx, 8
 or eax, ecx

 // MM6 <- Alpha-Channel
 movd mm6, eax
 punpcklbw mm6, mm7

 // 4b. Multiply Alpha-channel by Diffuse Color
(* {$IFNDEF NoTexture}
 {$IFDEF ApplyDiffuse}
 pmullw mm6, mm3
 psrlw mm6, 8
 {$ENDIF}
 {$ENDIF}*)
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

 {$IFNDEF NoTexture}
 {$IFDEF SimpleLoad}
 add esi, 4
 {$ENDIF}

 {$IFDEF StretchLoad}
 add ebx, uDelta
 add edx, vDelta
 {$ENDIF}
 {$ENDIF}
 
 add edi, 4

 dec PixCount
 jnz @PixelsFx

 // restore FPU state
 emms

 // restore general registers
 pop ebx
 pop esi
 pop edi
end;
{$UNDEF NoTexture}
{$UNDEF ApplyDiffuse}
{$UNDEF SrcAlphaGrey}
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






