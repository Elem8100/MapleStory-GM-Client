//---------------------------------------------------------------------------
// Description:
//
//   Calculates grayscale value of source pixel so it can become new
//   alpha-channel (source alpha-channel is overwritten!)
//
// Input:
//
//   MM0  -  Source Color
//
// Output:
//
//   MM0  -  New Source Color
//
// Registers Used:
//
//   EAX, ECX, MM0, MM2, MM7
//---------------------------------------------------------------------------

 // prepare gray coefficients
 punpcklbw mm0, mm7
 mov       ecx, 0050803h
 movd      mm2, ecx
 punpcklbw mm2, mm7

 // multiply source color by gray coefficients
 pmullw    mm2, mm0
 psrlw     mm2, 4
 packuswb  mm2, mm7
 packuswb  mm0, mm7

 // make grayscale color
 movd ecx, mm2
 mov  eax, ecx
 shr  ecx, 8
 add  eax, ecx
 shr  ecx, 8
 add  eax, ecx

 // update the alpha channel of source color
 shl  eax, 24
 movd ecx, mm0
 shl  ecx, 8
 shr  ecx, 8
 or   ecx, eax
 movd mm0, ecx
 mov  eax, ecx

