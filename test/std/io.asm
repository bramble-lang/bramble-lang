%define sys_exit 60
%define sys_read 0
%define sys_write 1

%define stdin 0
%define stdout 1

section .text
global root_std_io_writeline
root_std_io_writeline:
    mov rsi, rax
    mov rax, sys_write
    mov rdi, stdout
    mov rdx, 5
    syscall
    
    ret