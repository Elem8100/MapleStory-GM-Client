asm
 xor eax, eax

 {$IFDEF PxInDWORD}
 mov eax, Color
 {$ENDIF}

 {$IFDEF PxInTRIBYTE}
 mov eax, Color
 {$ENDIF}

 {$IFDEF PxInWORD}
 mov ax, Color
 {$ENDIF}

 {$IFDEF PxInBYTE}
 mov al, Color
 {$ENDIF}

 mov ecx, eax

 // red component
 shl eax, (x1 + a1 + b1 + g1)
 shr eax, (x1 + a1 + b1 + g1 + (r1 - r2))
 // green component
 mov edx, ecx
 shl edx, (x1 + a1 + b1)
 shr edx, (x1 + a1 + b1 + (g1 - g2) + r1)
 shl edx, (r2)
 or eax, edx
 // blue component
 mov edx, ecx
 shl edx, (x1 + a1)
 shr edx, (x1 + a1 + (b1 - b2) + g1 + r1)
 shl edx, (r2 + g2)
 or eax, edx

 {$IFDEF PxEXTRA}
 // extra mask
 or eax, xMask
 {$ENDIF}

 {$IFDEF PxALPHA}
 // alpha component
 mov edx, ecx
 shl edx, x1
 shr edx, (x1 + (a1 - a2) + b1 + g1 + r1)
 shl edx, (r2 + g2 + b2)
 or eax, edx
 {$ENDIF}

 {$IFDEF PxOutBYTE}
 mov Result, al
 {$ENDIF}

 {$IFDEF PxOutWORD}
 mov Result, ax
 {$ENDIF}

 {$IFDEF PxOutTRIBYTE}
 mov Result, eax
 {$ENDIF}

 {$IFDEF PxOutDWORD}
 mov Result, eax
 {$ENDIF}
end;
// unset all code parameters
{$UNDEF PxInDWORD}
{$UNDEF PxInTRIBYTE}
{$UNDEF PxInWORD}
{$UNDEF PxInBYTE}
{$UNDEF PxEXTRA}
{$UNDEF PxALPHA}
{$UNDEF PxOutDWORD}
{$UNDEF PxOutTRIBYTE}
{$UNDEF PxOutWORD}
{$UNDEF PxOutBYTE}

