main:
     mov eax, 4
     call alloc
     mov     dword [eax] , 0
     mov eax, 4
     call alloc
     mov     dword [eax] , 0
     mov eax, 4
     call alloc
     mov     dword [eax] , 1
     ret
foo:
     mov eax, 4
     call alloc
     mov     dword [eax] , 0
     mov eax, 4
     call alloc
     mov     dword [eax] , 7
     mov eax, 4
     call alloc
     mov     dword [eax] , 9
     ret
