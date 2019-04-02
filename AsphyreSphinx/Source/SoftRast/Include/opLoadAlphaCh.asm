//---------------------------------------------------------------------------
// Description:
//
//   Loads source alpha-channel to MMX register
//
// Input:
//
//   MM0  -  Source Color
//
// Output:
//
//   MM6  -  Alpha-channel (unpacked, A16R16G16B16)
//
// Registers Used:
//
//   EAX, ECX, MM0, MM6, MM7
//---------------------------------------------------------------------------

 movd eax, mm0
 shr  eax, 24

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
 movd      mm6, eax
 punpcklbw mm6, mm7

