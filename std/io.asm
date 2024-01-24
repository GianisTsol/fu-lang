section .bss
    buffer resb 8 ; 64 bits



section .text
    global _start


_start:
    ; Your main code here
    ; Your main code continues...
    call main

    ; Exit the program
    mov     rax, 0x3C          ; syscall: sys_exit
    xor     rdi, rdi         ; exit code 0
    syscall             ; Call kernel


section .text
    global alloc


alloc:
    ; Example: allocate memory for a 32-bit integer

    ;sys brk needs two calls
    ; rax is argument of size
    mov rbp, rax

    mov	rdi, 0  ; first call
    mov	rax, 0xC		 ;sys_brk
    syscall

    lea rdi, [rax + rbp] ; end of alloc ptr
    mov rbp, rax
    mov	rax, 0xC		 ;sys_brk
    syscall

    mov rax, rbp
    ret


printn:
    ; Convert number to string
    ; args in rax
    call int_to_string
    ; now string mem ptr in rdi
    ; size in rcx
    mov rbx, rdi
    add rbx, 31 ; end byte
    mov byte [rbx], 0x0A   ; newline character


    mov rcx, rdi
    ; Write to console
    mov rax, 0x1
    mov rdi, 0x1 ;stdout
    mov rsi, rcx
    mov rdx, 32 ;buffer size
    syscall
    ret

int_to_string:
    ; rax needs to contain int
    ; get modulo


    xor rcx, rcx
    lea rdi, byte [buffer + 30] ;return buffer pointer
    call _int_to_string_loop
    lea rdi, qword [buffer] ;return buffer pointer
    ;size already in rcx

    ret

_int_to_string_loop:
    mov rdx, 0             ; dividend high half = 0.  prefer  xor edx,edx
    mov rbx, 0xA           ; divisor can be any register or memory

    div rbx       ; Divides rax by rbx.
        ; RDX =   4 = 1234 % 10  remainder
        ; RAX = 123 = 1234 / 10  quotient

    inc rcx ; increment size

    add rdx, 0x30 ;convert to ascii byte ascii
    lea rbx, [rdi]
    sub rbx, rcx
    mov [rbx], dl

    cmp rax, 0
    jnz _int_to_string_loop
    ret

