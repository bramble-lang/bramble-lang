%define sys_exit 60
%define sys_read 0
%define sys_write 1

%define stdin 0
%define stdout 1

section .data
    dest_i64: dq 0 ; 64-bits integer = 8 bytes.  Destination for scanf to write to
    fmt_i64: db "%ld", 0


section .text
global std_io_readi64
std_io_readi64:
    extern scanf
    push rbp
    mov rbp, rsp
    
    ; This is to make sure that the boundary of the stack frame is 16 byte aligned
    ; before calling scanf
    ; !! This is a temporary hack and what should happen is that Braid compiler sizes
    ; each stack frame to be 16byte aligned
    and rsp, 18446744073709551600
    
    xor rax, rax
    mov rdi, fmt_i64
    lea rsi, QWORD [rel dest_i64]
    call scanf
    mov rax, QWORD [rel dest_i64]
    
    mov rsp, rbp
    pop rbp
    
    ret