unit TransformUtils;

//---------------------------------------------------------------------------
{$ifdef fpc}{$asmmode intel}{$endif}

//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
// BatchMultiply5()
//
// Multiplies a list of 4D vectors by 4x4 matrix. Both source and destination
// must be 16-byte aligned. The number of vectors must a multiple of two.
// Thus, Count defines how many groups of two vectors should be multiplied.
//---------------------------------------------------------------------------
procedure BatchMultiply5(Source, Dest: Pointer; Count: Integer;
 Matrix: Pointer);

//---------------------------------------------------------------------------
// BatchTransform1()
//
// Same as BatchMultiply5, but assumes that w component is 1.0 in source
// vector list. It is used in multiplication however, so transforming vectors
// by the projection matrix is still possible using this method.
 //---------------------------------------------------------------------------
procedure BatchTransform1(Source, Dest: Pointer; Count: Integer;
 Matrix: Pointer);

//---------------------------------------------------------------------------
var
 SupportedSSE: Boolean = False;

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
uses
 Vectors4, Matrices4;

//---------------------------------------------------------------------------
procedure TransformNoSSE(Source, Dest: Pointer; Count: Integer;
 Matrix: Pointer);
var
 SourceVec, DestVec: PVector4;
 SourceMtx: PMatrix4;
 i: Integer;
begin
 SourceMtx:= Matrix;
 SourceVec:= Source;
 DestVec  := Dest;

 for i:= 0 to (Count * 2) - 1 do
  begin
   DestVec^:= SourceVec^ * SourceMtx^;
   Inc(SourceVec);
   Inc(DestVec); 
  end;
end;

//---------------------------------------------------------------------------
// Note: EDI, ESI, ESP, EBP, and EBX registers should be preserved.
// However, EAX, ECX, and EDX registers can be freely modified.
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// BatchMultiply5 -- A modified version of BatchMultiply4 which loads
// vector components individually from memory, thereby allowing us
// to work on TWO VECTORS SIMULTANEOUSLY!
//
// Performance: 20 cycles/vector
//
// Original C code is copyright (c) 2002 Cort Stratton (cort@andrew.cmu.edu)
// Translated to Delphi by Yuriy Kotsarenko, 2005 (lifepower@mail333.com)
//---------------------------------------------------------------------------
procedure BatchMultiply5(Source, Dest: Pointer; Count: Integer;
 Matrix: Pointer);
begin
 if (not SupportedSSE) then
  begin
   TransformNoSSE(Source, Dest, Count, Matrix);
   Exit;
  end;

 asm
  push esi
  push edi

  mov esi, Source
  mov edi, Dest
  mov ecx, Count

 // load columns of matrix into xmm4-7
  mov	    edx, Matrix
  movaps xmm4, [edx]
  movaps xmm5, [edx + $10]
  movaps xmm6, [edx + $20]
  movaps xmm7, [edx + $30]

@bm5_start:

  // process x
  movss	 xmm1, [esi + $00]
  movss	 xmm3, [esi + $10]
  shufps xmm1, xmm1, $00
  prefetchnta	[esi + $30]
  shufps xmm3, xmm3, $00
  mulps	 xmm1, xmm4
  prefetchnta [edi + $30]
  mulps	 xmm3, xmm4

  // process y
  movss	 xmm0, [esi + $04]
  movss	 xmm2, [esi + $14]
  shufps xmm0, xmm0, $00
  shufps xmm2, xmm2, $00
  mulps	 xmm0, xmm5
  mulps	 xmm2, xmm5
  addps	 xmm1, xmm0
  addps	 xmm3, xmm2

  // process z
  movss	 xmm0, [esi + $08]
  movss	 xmm2, [esi + $18]
  shufps xmm0, xmm0, $00
  shufps xmm2, xmm2, $00
  mulps	 xmm0, xmm6
  mulps	 xmm2, xmm6
  addps	 xmm1, xmm0
  addps	 xmm3, xmm2

  // process w (hiding some pointer increments between the
  // multiplies)
  movss	 xmm0, [esi + $0C]
  movss	 xmm2, [esi + $1C]
  shufps xmm0, xmm0, $00
  shufps xmm2, xmm2, $00
  mulps	 xmm0, xmm7
  add		 esi, 32 // size of TVector4
  mulps	 xmm2, xmm7
  add		 edi, 32 // size of TVector4
  addps	 xmm1, xmm0
  addps	 xmm3, xmm2

  // write output vectors to memory, and loop
  movaps [edi - $20], xmm1
  movaps [edi - $10], xmm3

  dec	ecx
  jnz	@bm5_start

  pop edi
  pop esi
 end;
end;

//---------------------------------------------------------------------------
// BatchTransform1 -- A modified version of BatchMultiply4 which makes
// an additional assumption about the vectors in vin: if each vector's
// 4th element (the homogenous coordinate w) is assumed to be 1.0 (as is
// the case for 3D vertices), we can eliminate a move, a shuffle and a
// multiply instruction.
//
// Performance: 17 cycles/vector
//
// Original C code is copyright (c) 2002 Cort Stratton (cort@andrew.cmu.edu)
// Translated to Delphi by Yuriy Kotsarenko, 2005 (lifepower@mail333.com)
//---------------------------------------------------------------------------
procedure BatchTransform1(Source, Dest: Pointer; Count: Integer;
 Matrix: Pointer);
begin
 if (not SupportedSSE) then
  begin
   TransformNoSSE(Source, Dest, Count, Matrix);
   Exit;
  end;

 asm
  push esi
  push edi

  mov esi, Source
  mov edi, Dest
  mov ecx, Count

  // load columns of matrix into xmm4-7
  mov     edx, Matrix
  movaps xmm4, [edx]
  movaps xmm5, [edx + $10]
  movaps xmm6, [edx + $20]
  movaps xmm7, [edx + $30]

@bt2_start:

  // process x (hiding the prefetches in the delays)
  movss  xmm1, [esi + $00]
  movss  xmm3, [esi + $10]
  shufps xmm1, xmm1, $00
  prefetchnta  [edi + $30]
  shufps xmm3, xmm3, $00
  mulps  xmm1, xmm4
  prefetchnta  [esi + $30]
  mulps  xmm3, xmm4

  // process y
  movss  xmm0, [esi + $04]
  movss  xmm2, [esi + $14]
  shufps xmm0, xmm0, $00
  shufps xmm2, xmm2, $00
  mulps  xmm0, xmm5
  mulps  xmm2, xmm5
  addps  xmm1, xmm0
  addps  xmm3, xmm2

  // process z (hiding some pointer arithmetic between
  // the multiplies)
  movss  xmm0, [esi + $08]
  movss  xmm2, [esi + $18]
  shufps xmm0, xmm0, $00
  shufps xmm2, xmm2, $00
  mulps  xmm0, xmm6
  add    esi,  32 // size of TVector4
  mulps  xmm2, xmm6
  add    edi,  32 // size of TVector4
  addps  xmm1, xmm0
  addps  xmm3, xmm2

  // process w
  addps xmm1, xmm7
  addps xmm3, xmm7

  // write output vectors to memory and loop
  movaps [edi - $20], xmm1
  movaps [edi - $10], xmm3
  dec ecx
  jnz @bt2_start

  pop edi
  pop esi
 end;
end;

//---------------------------------------------------------------------------
function HasCpuId(): LongBool; assembler;
asm
 pushfd
 pop eax
 mov ecx, eax
 xor eax, $00200000
 push eax
 popfd
 pushfd
 pop eax
 xor eax, ecx
end;

//---------------------------------------------------------------------------
procedure CpuId(Flag: Longword; var Signature, Features: Longword); assembler;
asm
 push ebx
 push esi
 push edi
 mov esi, edx
 mov edi, ecx
 db  $0F, $A2 // cpuid
 mov [esi], eax
 mov [edi], edx
 pop edi
 pop esi
 pop ebx
end;

//---------------------------------------------------------------------------
procedure CheckForCPUSupport();
var
 Signature, Features: Longword;
begin
 if (HasCpuId()) then
  begin // The CPUID instruction is supported.
   CpuId(0, Signature, Features);

   if (Signature > 0) then
    begin // Can ask for SSE support.
     CpuId(1, Signature, Features);

     SupportedSSE:= LongBool(Features and(1 shl 26))
    end;
  end;
end;

//---------------------------------------------------------------------------
initialization
 CheckForCPUSupport();

//---------------------------------------------------------------------------
end.
