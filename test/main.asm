global _start

global vga_putch
global kb_getch

extern monitor_entry
extern setupVectors
extern kb_read

extern print
extern print_dec
extern print_hex
extern print_digit
extern print_byte

SECTION .text
OPTION_ROM_MARKER   equ 0xAA55
LF      equ     0Ah
CR      equ     0Dh
welcome_msg     db "8086 Monitor Test", CR, LF, LF, 0

initVga:
    mov bx, 0xC000 
    mov ds, bx
    mov word ax, [0]
    cmp ax, OPTION_ROM_MARKER
    jne noVga
    sti
    call 0xC000:0003
    ret

_start:
    mov	ax, biosStackSeg
	mov	ss, ax
	mov	sp, biosStackSize

	call setupVectors

	call initVga
    mov	ax, biosDataSeg	
	mov	ds, ax

    mov si, welcome_msg
	call print
    call monitor_entry

vga_putch:
    mov ah, 0x0E
    int 0x10
    ret

kb_getch:
    call kb_read
    ret

noVga:
    hlt

SECTION .reset
    cli
    nop

    jmp 0xF000:_start

