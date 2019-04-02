asm
 push edi
 mov eax, Color
 mov ecx, Count
 mov edi, Addr
@loop1:
 mov [edi], eax
 add edi, 4
 dec ecx
 jnz @loop1
 pop edi
end;
