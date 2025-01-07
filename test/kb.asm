SECTION .text

global kb_read


kbd_flags_1	equ	17h ; modifier falgs variable

kbc_data    equ 0x60
kbc_ctrl    equ 0x64

; scancodes for special keys
kbd_rshift_code	equ	36h
kbd_lshift_code	equ	2ah

; Bits for the various modifier keys
kbd_shft_bit	equ	1


; regular scancodes
scancodes:      db 0x1e, 'a', 'A'
                db 0x30, 'b', 'B'
                db 0x2e, 'c', 'C'
                db 0x20, 'd', 'D'
                db 0x12, 'e', 'E'
                db 0x21, 'f', 'F'
                db 0x22, 'g', 'G'
                db 0x23, 'h', 'H'
                db 0x17, 'i', 'I'
                db 0x24, 'j', 'J'
                db 0x25, 'k', 'K'
                db 0x26, 'l', 'L'
                db 0x32, 'm', 'M'
                db 0x31, 'n', 'N'
                db 0x18, 'o', 'O'
                db 0x19, 'p', 'P'
                db 0x10, 'q', 'Q'
                db 0x13, 'r', 'R'
                db 0x1f, 's', 'S'
                db 0x14, 't', 'T'
                db 0x16, 'u', 'U'
                db 0x2f, 'v', 'V'
                db 0x11, 'w', 'W'
                db 0x2d, 'x', 'X'
                db 0x15, 'y', 'Y'
                db 0x2c, 'z', 'Z'
                db 0x02, '1', '!'
                db 0x03, '2', '@'
                db 0x04, '3', '#'
                db 0x05, '4', '$'
                db 0x06, '5', '%'
                db 0x07, '6', '^'
                db 0x08, '7', '&'
                db 0x09, '8', '*'
                db 0x0a, '9', '('
                db 0x0b, '0', ')'
                db 0x1c, 0x0d, 0    ;enter
                db 0x39, ' ',  0
                db 0x1a, '[', '{'
                db 0x1b, ']', '}'
                db 0x27, ';', ':'
                db 0x0c, '-', '_'
                db 0x0d, '=', '+'
                db 0x0e, 0x08, 0    ;backspace
                db 0x28, 0x27, '"'  ;'
                db 0x29, '`', '~'
                db 0x33, ',', '<'
                db 0x34, '.', '>'
                db 0x35, '/', '?'
                db 0x2b, '\', '|'
scancodes_end:  db 0x01, 0x1b, 0    ;esc

convert:
    push bx
    mov bx, 0
.loop:
    cmp al, cs:[scancodes+bx]
    je .convert
    add bx, 3
    cmp bx, scancodes_end - scancodes
    ja .not_found
    jmp .loop
.convert:
    test byte [kbd_flags_1], kbd_shft_bit
    jz .no_shift
    mov al, cs:[scancodes+bx+2]
    jmp .converted
.no_shift:
    mov al, cs:[scancodes+bx+1]
.converted:
    cmp al, 0
    je .not_found
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
    jnz .key_released
.key_pressed:
    cmp al, kbd_rshift_code
    je .shift_pressed
    cmp al, kbd_lshift_code
    je .shift_pressed
    call convert
    ret

.shift_pressed:
    or byte [kbd_flags_1], kbd_shft_bit
    ret

.key_released:
    and al, 0x7f
    cmp al, kbd_rshift_code
    je .shift_released
    cmp al, kbd_lshift_code
    je .shift_released
    clc
    ret

.shift_released:
    and byte [kbd_flags_1], ~kbd_shft_bit

.no_data:
    ret