asm
 push esi
 push edi
 push ebx

 mov esi, Source
 mov edi, Dest
 mov ecx, Count

@pixloop:
 xor eax, eax

 {$IFDEF PxInDWORD}
 mov eax, [esi]
 add esi, 4
 {$ENDIF}

 {$IFDEF PxInTRIBYTE}
 mov al, [esi + 2]
 shl eax, 8
 mov al, [esi + 1]
 shl eax, 8
 mov al, [esi]
 add esi, 3
 {$ENDIF}

 {$IFDEF PxInWORD}
 mov ax, [esi]
 add esi, 2
 {$ENDIF}

 {$IFDEF PxInBYTE}
 mov al, [esi]
 add esi, 1
 {$ENDIF}

 mov ebx, eax
 // red component
 shl eax, (x1 + a1 + b1 + g1)
 shr eax, (x1 + a1 + b1 + g1 + (r1 - r2))
 // green component
 mov edx, ebx
 shl edx, (x1 + a1 + b1)
 shr edx, (x1 + a1 + b1 + (g1 - g2) + r1)
 shl edx, (r2)
 or eax, edx
 // blue component
 mov edx, ebx
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
 mov edx, ebx
 shl edx, x1
 shr edx, (x1 + (a1 - a2) + b1 + g1 + r1)
 shl edx, (r2 + g2 + b2)
 or eax, edx
 {$ENDIF}

 {$IFDEF PxOutBYTE}
 mov [edi], al
 inc edi
 {$ENDIF}

 {$IFDEF PxOutWORD}
 mov [edi], ax
 add edi, 2
 {$ENDIF}

 {$IFDEF PxOutTRIBYTE}
 mov [edi], al
 shr eax, 8
 mov [edi + 1], al
 shr eax, 8
 mov [Edi + 2], al
 add edi, 3
 {$ENDIF}

 {$IFDEF PxOutDWORD}
 mov [edi], eax
 add edi, 4
 {$ENDIF}

 dec ecx
 jnz @pixloop

 pop ebx
 pop edi
 pop esi
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

