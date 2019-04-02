//---------------------------------------------------------------------------
// Description:
//
//   Loads "DiffuseAlpha" fixed-point (16.16) value, advances it and then
//   returns the integer part of a new alpha value
//
// Input:
//
//   [DiffuseAlpha] - DWORD Fixed-Pont Diffuse Alpha (16.16)
//
// Output:
//
//   EAX  -  Alpha Value (A8A8A8A8 - each A of 0..255)
//
// Registers Used:
//   
//   EAX, ECX
//---------------------------------------------------------------------------

 mov eax, DiffuseAlpha
 mov ecx, eax
 shr eax, 16         // EAX - Diffuse Alpha Position
 shl ecx, 16
 shr ecx, 16         // ECX - Diffuse Alpha Velocity
 add  ax, cx         //  AX - New Diffuse Alpha Position
 shl eax, 16
 or  eax, ecx        // EAX - New Diffuse Alpha
 mov DiffuseAlpha, eax

 shr eax, 24         // EAX - Diffuse Alpha (0..255)
 mov ecx, eax
 shl ecx, 8
 or  eax, ecx
 shl ecx, 8
 or  eax, ecx
 shl ecx, 8
 or  eax, ecx        // EAX - Diffuse Alpha (A8A8A8A8 - 32bit alpha-only)
