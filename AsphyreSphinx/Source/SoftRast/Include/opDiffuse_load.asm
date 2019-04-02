//---------------------------------------------------------------------------
// Description:
//
//   Loads diffuse colors into MMX register for further processing
//
// Input:
//
//   [Color1] - DWORD Initial Diffuse Color
//   [Color2] - DWORD Final   Diffuse COlor
//
// Output:
//
//   MM5  -  [unpacked]: Diffuse Color1 (low dword), Diffuse Color 2 (high dword)
//
// Registers Used:
//   
//   MM2, MM5
//---------------------------------------------------------------------------



 movq mm5, mm7       // MM5 - zero
 pxor mm2, mm2       // MM2 - zero
 
 movd mm5, Color1    // MM5 (low dword) - color1
 movd mm2, Color2    // MM2 (low dword) - color2

 psllq mm2, 32       // MM2 (high dword) - color2
 por   mm5, mm2      // MM5 - color2 (high), color1 (low)
