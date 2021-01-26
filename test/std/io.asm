%define sys_exit 60
%define sys_read 0
%define sys_write 1

%define stdin 0
%define stdout 1

section .text
global root_std_io_write
root_std_io_write:
    call str_len
    mov rsi, rax
    mov rax, sys_write
    mov rdi, stdout
    syscall
    
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