%define sys_exit 60
%define sys_read 0
%define sys_write 0x2000004

%define stdin 0
%define stdout 1


section .data
    dest_i64: dq 0 ; 64-bits integer = 8 bytes.  Destination for scanf to write to
    fmt_u8: db "%hhu", 0
    fmt_u8ln: db `%hhu\n`, 0
    fmt_u16: db "%hu", 0
    fmt_u16ln: db `%hu\n`, 0
    fmt_u32: db "%lu", 0
    fmt_u32ln: db `%lu\n`, 0
    fmt_u64: db "%llu", 0
    fmt_u64ln: db `%llu\n`, 0
    fmt_i64: db "%ld", 0
    fmt_i64ln: db `%ld\n`, 0
    fmt_i32: db "%d", 0
    fmt_i32ln: db `%d\n`, 0
    fmt_i16: db "%hd", 0
    fmt_i16ln: db `%hd\n`, 0
    fmt_i8: db "%hhd", 0
    fmt_i8ln: db `%hhd\n`, 0
    fmt_str: db "%s", 0

    true: db "true", 0
    false: db "false", 0
    true_ln: db `true\n`, 0
    false_ln: db `false\n`, 0


section .text
global _root_std_io_write
_root_std_io_write:
    extern _printf
    push rbp
    mov rbp, rsp

    ; This is to make sure that the boundary of the stack frame is 16 byte aligned
    ; before calling scanf
    ; !! This is a temporary hack and what should happen is that Braid compiler sizes
    ; each stack frame to be 16byte aligned
    and rsp, 18446744073709551600

    mov rax, rdi
    mov rdi, fmt_str
    mov rsi, rax
    xor rax, rax
    call _printf

    mov rsp, rbp
    pop rbp
    ret


global _root_std_io_readi64
_root_std_io_readi64:
    extern _scanf
    push rbp
    mov rbp, rsp
    
    ; This is to make sure that the boundary of the stack frame is 16 byte aligned
    ; before calling scanf
    ; !! This is a temporary hack and what should happen is that Braid compiler sizes
    ; each stack frame to be 16byte aligned
    and rsp, 18446744073709551600
    
    mov rax, rdi
    xor rax, rax
    mov rdi, fmt_i64
    lea rsi, QWORD [rel dest_i64]
    call _scanf
    mov rax, QWORD [rel dest_i64]
    
    mov rsp, rbp
    pop rbp
    
    ret


global _root_std_io_writeu8
_root_std_io_writeu8:
    extern _printf
    push rbp
    mov rbp, rsp

    ; This is to make sure that the boundary of the stack frame is 16 byte aligned
    ; before calling scanf
    ; !! This is a temporary hack and what should happen is that Braid compiler sizes
    ; each stack frame to be 16byte aligned
    and rsp, 18446744073709551600

    mov rax, rdi
    mov rdi, fmt_u8
    mov rsi, rax
    xor rax, rax
    call _printf

    mov rsp, rbp
    pop rbp
    ret


global _root_std_io_writeu8ln
_root_std_io_writeu8ln:
    extern _printf
    push rbp
    mov rbp, rsp

    ; This is to make sure that the boundary of the stack frame is 16 byte aligned
    ; before calling scanf
    ; !! This is a temporary hack and what should happen is that Braid compiler sizes
    ; each stack frame to be 16byte aligned
    and rsp, 18446744073709551600

    mov rax, rdi
    mov rdi, fmt_u8ln
    mov rsi, rax
    xor rax, rax
    call _printf

    mov rsp, rbp
    pop rbp
    ret


global _root_std_io_writeu16
_root_std_io_writeu16:
    extern _printf
    push rbp
    mov rbp, rsp

    ; This is to make sure that the boundary of the stack frame is 16 byte aligned
    ; before calling scanf
    ; !! This is a temporary hack and what should happen is that Braid compiler sizes
    ; each stack frame to be 16byte aligned
    and rsp, 18446744073709551600

    mov rax, rdi
    mov rdi, fmt_u16
    mov rsi, rax
    xor rax, rax
    call _printf

    mov rsp, rbp
    pop rbp
    ret


global _root_std_io_writeu16ln
_root_std_io_writeu16ln:
    extern _printf
    push rbp
    mov rbp, rsp

    ; This is to make sure that the boundary of the stack frame is 16 byte aligned
    ; before calling scanf
    ; !! This is a temporary hack and what should happen is that Braid compiler sizes
    ; each stack frame to be 16byte aligned
    and rsp, 18446744073709551600

    mov rax, rdi
    mov rdi, fmt_u16ln
    mov rsi, rax
    xor rax, rax
    call _printf

    mov rsp, rbp
    pop rbp
    ret


global _root_std_io_writeu32
_root_std_io_writeu32:
    extern _printf
    push rbp
    mov rbp, rsp

    ; This is to make sure that the boundary of the stack frame is 16 byte aligned
    ; before calling scanf
    ; !! This is a temporary hack and what should happen is that Braid compiler sizes
    ; each stack frame to be 16byte aligned
    and rsp, 18446744073709551600

    mov rax, rdi
    mov rdi, fmt_u32
    mov rsi, rax
    xor rax, rax
    call _printf

    mov rsp, rbp
    pop rbp
    ret


global _root_std_io_writeu32ln
_root_std_io_writeu32ln:
    extern _printf
    push rbp
    mov rbp, rsp

    ; This is to make sure that the boundary of the stack frame is 16 byte aligned
    ; before calling scanf
    ; !! This is a temporary hack and what should happen is that Braid compiler sizes
    ; each stack frame to be 16byte aligned
    and rsp, 18446744073709551600

    mov rax, rdi
    mov rdi, fmt_u32ln
    mov rsi, rax
    xor rax, rax
    call _printf

    mov rsp, rbp
    pop rbp
    ret


global _root_std_io_writeu64
_root_std_io_writeu64:
    extern _printf
    push rbp
    mov rbp, rsp

    ; This is to make sure that the boundary of the stack frame is 16 byte aligned
    ; before calling scanf
    ; !! This is a temporary hack and what should happen is that Braid compiler sizes
    ; each stack frame to be 16byte aligned
    and rsp, 18446744073709551600

    mov rax, rdi
    mov rdi, fmt_u64
    mov rsi, rax
    xor rax, rax
    call _printf

    mov rsp, rbp
    pop rbp
    ret


global _root_std_io_writeu64ln
_root_std_io_writeu64ln:
    extern _printf
    push rbp
    mov rbp, rsp

    ; This is to make sure that the boundary of the stack frame is 16 byte aligned
    ; before calling scanf
    ; !! This is a temporary hack and what should happen is that Braid compiler sizes
    ; each stack frame to be 16byte aligned
    and rsp, 18446744073709551600

    mov rax, rdi
    mov rdi, fmt_u64ln
    mov rsi, rax
    xor rax, rax
    call _printf

    mov rsp, rbp
    pop rbp
    ret


global _root_std_io_writei64
_root_std_io_writei64:
    extern _printf
    push rbp
    mov rbp, rsp

    ; This is to make sure that the boundary of the stack frame is 16 byte aligned
    ; before calling scanf
    ; !! This is a temporary hack and what should happen is that Braid compiler sizes
    ; each stack frame to be 16byte aligned
    and rsp, 18446744073709551600

    mov rax, rdi
    mov rdi, fmt_i64
    mov rsi, rax
    xor rax, rax
    call _printf

    mov rsp, rbp
    pop rbp
    ret


global _root_std_io_writei64ln
_root_std_io_writei64ln:
    extern _printf
    push rbp
    mov rbp, rsp

    ; This is to make sure that the boundary of the stack frame is 16 byte aligned
    ; before calling scanf
    ; !! This is a temporary hack and what should happen is that Braid compiler sizes
    ; each stack frame to be 16byte aligned
    and rsp, 18446744073709551600

    mov rax, rdi
    mov rdi, fmt_i64ln
    mov rsi, rax
    xor rax, rax
    call _printf

    mov rsp, rbp
    pop rbp
    ret


global _root_std_io_writei32
_root_std_io_writei32:
    extern _printf
    push rbp
    mov rbp, rsp

    ; This is to make sure that the boundary of the stack frame is 16 byte aligned
    ; before calling scanf
    ; !! This is a temporary hack and what should happen is that Braid compiler sizes
    ; each stack frame to be 16byte aligned
    and rsp, 18446744073709551600

    mov rax, rdi
    mov rdi, fmt_i32
    mov esi, eax
    xor rax, rax
    call _printf

    mov rsp, rbp
    pop rbp
    ret


global _root_std_io_writei32ln
_root_std_io_writei32ln:
    extern _printf
    push rbp
    mov rbp, rsp

    ; This is to make sure that the boundary of the stack frame is 16 byte aligned
    ; before calling scanf
    ; !! This is a temporary hack and what should happen is that Braid compiler sizes
    ; each stack frame to be 16byte aligned
    and rsp, 18446744073709551600

    mov rax, rdi
    mov rdi, fmt_i32ln
    mov esi, eax
    xor rax, rax
    call _printf

    mov rsp, rbp
    pop rbp
    ret


global _root_std_io_writei16
_root_std_io_writei16:
    extern _printf
    push rbp
    mov rbp, rsp

    ; This is to make sure that the boundary of the stack frame is 16 byte aligned
    ; before calling scanf
    ; !! This is a temporary hack and what should happen is that Braid compiler sizes
    ; each stack frame to be 16byte aligned
    and rsp, 18446744073709551600

    mov rax, rdi
    mov rdi, fmt_i16
    mov esi, eax
    xor rax, rax
    call _printf

    mov rsp, rbp
    pop rbp
    ret


global _root_std_io_writei16ln
_root_std_io_writei16ln:
    extern _printf
    push rbp
    mov rbp, rsp

    ; This is to make sure that the boundary of the stack frame is 16 byte aligned
    ; before calling scanf
    ; !! This is a temporary hack and what should happen is that Braid compiler sizes
    ; each stack frame to be 16byte aligned
    and rsp, 18446744073709551600

    mov rax, rdi
    mov rdi, fmt_i16ln
    mov esi, eax
    xor rax, rax
    call _printf

    mov rsp, rbp
    pop rbp
    ret


global _root_std_io_writei8
_root_std_io_writei8:
    extern _printf
    push rbp
    mov rbp, rsp

    ; This is to make sure that the boundary of the stack frame is 16 byte aligned
    ; before calling scanf
    ; !! This is a temporary hack and what should happen is that Braid compiler sizes
    ; each stack frame to be 16byte aligned
    and rsp, 18446744073709551600

    mov rax, rdi
    mov rdi, fmt_i8
    mov esi, eax
    xor rax, rax
    call _printf

    mov rsp, rbp
    pop rbp
    ret


global _root_std_io_writei8ln
_root_std_io_writei8ln:
    extern _printf
    push rbp
    mov rbp, rsp

    ; This is to make sure that the boundary of the stack frame is 16 byte aligned
    ; before calling scanf
    ; !! This is a temporary hack and what should happen is that Braid compiler sizes
    ; each stack frame to be 16byte aligned
    and rsp, 18446744073709551600

    mov rax, rdi
    mov rdi, fmt_i8ln
    mov esi, eax
    xor rax, rax
    call _printf

    mov rsp, rbp
    pop rbp
    ret


global _root_std_io_writebool
_root_std_io_writebool:
    extern _printf
    push rbp
    mov rbp, rsp

    mov rax, rdi
    cmp rax, 0
    jz .false
    mov rdi, true
    jmp .done
.false:
    mov rdi, false
.done:

    ; This is to make sure that the boundary of the stack frame is 16 byte aligned
    ; before calling scanf
    ; !! This is a temporary hack and what should happen is that Braid compiler sizes
    ; each stack frame to be 16byte aligned
    and rsp, 18446744073709551600

    xor rax, rax
    call _printf

    mov rsp, rbp
    pop rbp
    ret


global _root_std_io_writeboolln
_root_std_io_writeboolln:
    extern _printf
    push rbp
    mov rbp, rsp

    mov rax, rdi
    cmp rax, 0
    jz .false
    mov rdi, true_ln
    jmp .done
.false:
    mov rdi, false_ln
.done:

    ; This is to make sure that the boundary of the stack frame is 16 byte aligned
    ; before calling scanf
    ; !! This is a temporary hack and what should happen is that Braid compiler sizes
    ; each stack frame to be 16byte aligned
    and rsp, 18446744073709551600

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