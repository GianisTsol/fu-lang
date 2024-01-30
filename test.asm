section .bss
                buffer resb 8 ; 64 bits 
section .text
     global _start
main:
     mov rax, 32
     call alloc
     lea rbx, qword [rax + 24]
    mov qword [rbx], 119
     mov rbx, rax ; not preserved
     mov rax, 8
     call alloc
     lea rdx, qword [rbx + 24]
    mov rcx, qword [rdx]
    mov qword [rax], rcx ; h
;Load arg 0: g
    mov rax, qword [rax]
     call printn

    ret
printn:
     ; n: rax

    call int_to_string
    mov rbx, rdi
    add rbx, 31 
    mov byte [rbx], 0x0A   
    mov rcx, rdi
    mov rax, 0x1
    mov rdi, 0x1 
    mov rsi, rcx
    mov rdx, 32 
    syscall
    ;
    ret
exit:

    mov     rax, 0x3C          
    xor     rdi, rdi         
    syscall             
    ;
    ret
_start:
     call main
     call exit

    ret
int_to_string:
     ; var: rax

    xor rcx, rcx
    lea rdi, byte [buffer + 30] 
    call _int_to_string_loop
    lea rdi, qword [buffer] 
    ;
    ret
_int_to_string_loop:

    mov rdx, 0             
    mov rbx, 0xA           
    div rbx       
    inc rcx 
    add rdx, 0x30   
    lea rbx, [rdi]
    sub rbx, rcx
    mov [rbx], dl
    cmp rax, 0
    jnz _int_to_string_loop
    ;
    ret
alloc:
     ; size: rax

    mov rbp, rax
    mov	rdi, 0  
    mov	rax, 0xC		
    syscall
    lea rdi, [rax + rbp]    
    mov rbp, rax
    mov	rax, 0xC		 
    syscall
    mov rax, rbp
    ;
    ret
