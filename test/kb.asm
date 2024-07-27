SECTION .text

global kb_read

kbc_data    equ 0x60
kbc_ctrl    equ 0x64

scancodes:      db 0x1e, 'a'
                db 0x30, 'b'
                db 0x2e, 'c'
                db 0x20, 'd'
                db 0x12, 'e'
                db 0x21, 'f'
                db 0x22, 'g'
                db 0x23, 'h'
                db 0x17, 'i'
                db 0x24, 'j'
                db 0x25, 'k'
                db 0x26, 'l'
                db 0x32, 'm'
                db 0x31, 'n'
                db 0x18, 'o'
                db 0x19, 'p'
                db 0x10, 'q'
                db 0x13, 'r'
                db 0x1f, 's'
                db 0x14, 't'
                db 0x16, 'u'
                db 0x2f, 'v'
                db 0x11, 'w'
                db 0x2d, 'x'
                db 0x15, 'y'
                db 0x2c, 'z'
                db 0x02, '1'
                db 0x03, '2'
                db 0x04, '3'
                db 0x05, '4'
                db 0x06, '5'
                db 0x07, '6'
                db 0x08, '7'
                db 0x09, '8'
                db 0x0a, '9'
                db 0x0b, '0'
                db 0x1c, 0x0d ;enter
                db 0x39, ' '
                db 0x1a, '['
                db 0x1b, ']'
                db 0x27, ';'
                db 0x0c, '-'
                db 0x0d, '='
                db 0x0e, 0x08 ;backspace
                db 0x28, 0x27 ;'
                db 0x29, '`'
                db 0x33, ','
                db 0x34, '.'
                db 0x35, '/'
                db 0x2b, 0x5c ;backslash
scancodes_end:  db 0x01, 0x1b ;esc

convert:
    push bx
    mov bx, 0
.loop:
    cmp al, cs:[scancodes+bx]
    je .convert
    add bx, 2
    cmp bx, scancodes_end - scancodes
    ja .not_found
    jmp .loop
.convert:
    mov al, cs:[scancodes+bx+1]
    stc
.not_found:
    pop bx
    ret

kb_read:
    clc
    in al, kbc_ctrl
    test al, 1
    jz .no_data
    in al, kbc_data
    test al, 0x80
    jnz .no_data
    call convert
.no_data:
    ret