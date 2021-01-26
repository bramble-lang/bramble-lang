%define sys_exit 60
%define sys_read 0
%define sys_write 1

%define stdin 0
%define stdout 1

section .data
    dest_i64: dq 0 ; 64-bits integer = 8 bytes.  Destination for scanf to write to
    fmt_i64: db "%ld", 0

section .text
global root_std_io_write
root_std_io_write:
    call str_len
    mov rsi, rax
    mov rax, sys_write
    mov rdi, stdout
    syscall
    
    ret


global root_std_io_readi64
root_std_io_readi64:
    extern scanf
    push rbp
    mov rbp, rsp
    
    ; This is to make sure that the boundary of the stack frame is 16 byte aligned
    ; before calling scanf
    ; !! This is a temporary hack and what should happen is that Braid compiler sizes
    ; each stack frame to be 16byte aligned
    mov rax, rsp
    and rax, 15
    sub rsp, rax
    
    xor rax, rax
    mov rdi, fmt_i64
    lea rsi, QWORD [dest_i64]
    call scanf
    mov rax, QWORD [dest_i64]
    
    mov rsp, rbp
    pop rbp
    
    ret
    
    
str_len:
    ; set counter to 0
    mov rdx, 0
    ; check if value is 0
    ; if not, then increment the counter
.loop:
    cmp [rax+rdx], BYTE 0
    jz .done
    inc rdx
    jmp .loop
.done:
    ret