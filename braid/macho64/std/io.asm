%define sys_exit 60
%define sys_read 0
%define sys_write 0x2000004

%define stdin 0
%define stdout 1


section .data
    dest_i64: dq 0 ; 64-bits integer = 8 bytes.  Destination for scanf to write to
    fmt_i64: db "%ld", 0
    fmt_str: db "%s", 0


section .text
global root_std_io_write
root_std_io_write:
    extern _printf
    push rbp
    mov rbp, rsp

    ; This is to make sure that the boundary of the stack frame is 16 byte aligned
    ; before calling scanf
    ; !! This is a temporary hack and what should happen is that Braid compiler sizes
    ; each stack frame to be 16byte aligned
    and rsp, 18446744073709551600

    mov rdi, fmt_str
    mov rsi, rax
    xor rax, rax
    call _printf

    mov rsp, rbp
    pop rbp
    ret


global root_std_io_readi64
root_std_io_readi64:
    extern _scanf
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
    call _scanf
    mov rax, QWORD [rel dest_i64]
    
    mov rsp, rbp
    pop rbp
    
    ret


global root_std_io_writei64
root_std_io_writei64:
    extern _printf
    push rbp
    mov rbp, rsp

    ; This is to make sure that the boundary of the stack frame is 16 byte aligned
    ; before calling scanf
    ; !! This is a temporary hack and what should happen is that Braid compiler sizes
    ; each stack frame to be 16byte aligned
    and rsp, 18446744073709551600

    mov rdi, fmt_i64
    mov rsi, rax
    xor rax, rax
    call _printf

    mov rsp, rbp
    pop rbp
    ret


;;;;
;; internal support functions
;;;;

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