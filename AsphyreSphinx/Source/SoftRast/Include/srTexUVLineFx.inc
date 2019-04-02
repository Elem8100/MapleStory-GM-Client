var
 PixCount{$IFDEF ApplyDiffuse}, DiffuseAlpha{$ENDIF}: Integer;
begin
 PixCount:= Count;
 {$IFDEF ApplyDiffuse}
 DiffuseAlpha:= Cardinal(($100 * 255) div Integer(Count)); {16.16 color change}
 {$ENDIF}

 asm
  //   * Registers used ALWAYS *
  // MM5 <- Diffuse Color2 (high dword), Diffuse Color1 (low dword)
  // MM7 <- zero-based register

  //   * Registers used in sequence *
  // MM0 <- Source Color (packed)
  // MM1 <- Destination Color (packed)
  // MM4 <- Diffuse Color (unpacked)
  // MM6 <- Alpha-Channel (unpacked)

  // preserve general registers
  push edi
  push esi
  push ebx

//---------------------------------------------------------------------------
// MM7  -  Zero Register
//---------------------------------------------------------------------------
  pxor mm7, mm7

//---------------------------------------------------------------------------
// ESI  -  Source      Pointer
// EDI  -  Destination Pointer
//---------------------------------------------------------------------------
  {$IFNDEF NoTexture}
  mov esi, Source
  {$ENDIF}
  mov edi, Dest

//---------------------------------------------------------------------------
// EBX  -  Fixed-Point Position (16.16)
// EDX  -  Fixed-Point Change   (16.16)
//---------------------------------------------------------------------------
  {$IFNDEF NoTexture}
  mov ebx, u
  mov edx, v
  {$ENDIF}

//---------------------------------------------------------------------------
// Load diffuse colors to MM5
//---------------------------------------------------------------------------
  {$IFDEF ApplyDiffuse}
  {$include include\opDiffuse_load.asm}
  {$ENDIF}

//---------------------------------------------------------------------------
// FX Loop
//---------------------------------------------------------------------------
 @PixelsFx:
//---------------------------------------------------------------------------
// (a) MM3 <- Primary Diffuse Color
//---------------------------------------------------------------------------
  {$IFDEF ApplyDiffuse}
  // Advance DiffuseAlpha position
  {$include include\opDiffuse_alpha.asm}

  // Blend diffuse colors to retreive current Diffuse Color in MM4
  {$include include\opDiffuse_blend.asm}

  // -------------------------------------------------------
  // D. If no texture is used, pack and save in MM0
  // -------------------------------------------------------
  {$IFDEF NoTexture}
  packuswb mm4, mm7
  movq     mm0, mm4
  {$ENDIF}
  {$ENDIF}

//---------------------------------------------------------------------------
// load Source pixel
//---------------------------------------------------------------------------
  {$IFNDEF NoTexture}
  {$include include\opPixelLoad.asm}
  {$ENDIF}

//---------------------------------------------------------------------------
// load Destination pixel
//---------------------------------------------------------------------------
  {$IFDEF LoadDestPixel}
  movd mm1, [edi]
  {$ENDIF}

//---------------------------------------------------------------------------
// Invert Source color
//---------------------------------------------------------------------------
  {$IFDEF InvertSourceColor}
  mov   ecx, 0FFFFFFFFh
  movd  mm2, ecx
  psubb mm2, mm0
  movq  mm0, mm2
  {$ENDIF}

//---------------------------------------------------------------------------
// Apply DIFFUSE color
//---------------------------------------------------------------------------
  {$IFDEF ApplyDiffuse}
  {$IFNDEF NoTexture}
  punpcklbw mm0, mm7
  pmullw    mm0, mm4
  psrlw     mm0, 8
  packuswb  mm0, mm7
  {$ENDIF}
  {$ENDIF}

//---------------------------------------------------------------------------
// Use Grayscale image as an Alpha-Channel
//---------------------------------------------------------------------------
  {$IFDEF SrcAlphaGrey}
  {$include include\opSrcAlphaGray.asm}
  {$ENDIF}

//---------------------------------------------------------------------------
// load Alpha-Channel?
//---------------------------------------------------------------------------
  {$IFDEF LoadSourceAlpha}
  {$include include\opLoadAlphaCh.asm}
  {$ENDIF}

//---------------------------------------------------------------------------
// multiply Source Color by Alpha-Channel
//---------------------------------------------------------------------------
  {$IFDEF SourceMulAlpha}
  punpcklbw mm0, mm7

  pmullw mm0, mm6
  psrlw  mm0, 8

  packuswb mm0, mm0
  {$ENDIF}

//---------------------------------------------------------------------------
// multiply Destination Color by inverse Alpha-Channel
//---------------------------------------------------------------------------
  {$IFDEF DestMulAlpha}
  mov       ecx, 0FFFFFFh
  movd      mm2, ecx
  punpcklbw mm2, mm7
  psubw     mm2, mm6

  punpcklbw mm1, mm7

  pmullw    mm1, mm2
  psrlw     mm1, 8

  packuswb  mm1, mm7
  {$ENDIF}

//---------------------------------------------------------------------------
// multiply Destination color by Source color
//---------------------------------------------------------------------------
  {$IFDEF DestMulSource}
  punpcklbw mm0, mm7
  punpcklbw mm1, mm7

  pmullw mm1, mm0
  psrlw  mm1, 8

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
  movd  [edi], mm0
  {$ENDIF}

//---------------------------------------------------------------------------
// (3) Operation: Destination
//---------------------------------------------------------------------------
  {$IFDEF OpDest}
  movd [edi], mm1
  {$ENDIF}

  {$IFNDEF NoTexture}
  add ebx, uDelta
  add edx, vDelta
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
end;




