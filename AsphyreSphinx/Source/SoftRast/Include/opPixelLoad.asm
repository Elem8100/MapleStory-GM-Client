//---------------------------------------------------------------------------
// Description:
//
//   Loads pixel based on UV coordinates
//
// Input:
//
//   EBX   -  u
//   EDX   -  v
//   Pitch -  Texture Pitch (bytes per line)
//
// Output:
//
//   MM0  -  Source Color (unpacked)
//
// Registers Used:
//
//   EAX, EBX, ECX, EDX, MM0
//---------------------------------------------------------------------------  

 mov  eax, edx
 shr  eax, 16
 mov  ecx, Pitch
 imul eax, ecx
 mov  ecx, ebx
 shr  ecx, 16
 shl  ecx, 2
 add  eax, ecx
 add  eax, esi
 movd mm0, [eax]
