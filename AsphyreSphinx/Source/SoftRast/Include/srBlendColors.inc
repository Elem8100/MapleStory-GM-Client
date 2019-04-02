asm
 jcxz @OnlyColor2
 cmp  ecx, 0FFh
 je   @OnlyColor1

 push ebx

 mov  ebx, eax
 and  eax, 000FF00FFh
 and  ebx, 0FF00FF00h
 imul eax, ecx
 shr  ebx, 8
 imul ebx, ecx
 add  eax, 000800080h
 and  eax, 0FF00FF00h
 shr  eax, 8
 add  ebx, 000800080h
 and  ebx, 0FF00FF00h
 or   eax, ebx

 xor  ecx, 0000000FFh
 mov  ebx, edx
 and  edx, 000FF00FFh
 and  ebx, 0FF00FF00h 
 imul edx, ecx
 shr  ebx, 8
 imul ebx, ecx
 add  edx, 000800080h
 and  edx, 0FF00FF00h
 shr  edx, 8
 add  ebx, 000800080h
 and  ebx, 0FF00FF00h
 or   ebx, edx

 add  eax, ebx
 pop  ebx
 ret

@OnlyColor2:
 mov  eax, edx

@OnlyColor1:
end;
