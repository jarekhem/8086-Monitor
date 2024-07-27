%include "InterruptHandlers.inc"

pic1_reg0   equ 20h
pic1_reg1   equ 21h

interrupt_table:
	dw	defaultHandler		; INT 00 - Divide by zero
	dw	defaultHandler		; INT 01 - Single step
	dw	defaultHandler; int_02			; INT 02 - Non-maskable interrupt
	dw	int_ignore		; INT 03 - Debugger breakpoint
	dw	int_ignore		; INT 04 - Integer overlow (into)
	dw	int_ignore;  int_05			; INT 05 - BIOS Print Screen
	dw	int_ignore		; INT 06
	dw	int_ignore		; INT 07
	dw	int_ignore;  int_08			; INT 08 - IRQ0 - Timer Channel 0
	dw	int_ignore;  int_09			; INT 09 - IRQ1 - Keyboard
	dw	defaultHandler;  int_ignore		; INT 0A - IRQ2
	dw	defaultHandler;  int_ignore		; INT 0B - IRQ3
	dw	defaultHandler;  int_ignore		; INT 0C - IRQ4
	dw	defaultHandler;  int_ignore		; INT 0D - IRQ5
	dw	defaultHandler;  int_0E			; INT 0E - IRQ6 - Floppy
	dw	defaultHandler;  int_ignore		; INT 0F - IRQ7
	dw	int_10			; INT 10 - BIOS Video Services
	dw	defaultHandler;  int_11			; INT 11 - BIOS Get Equipment List
	dw	defaultHandler;  int_12			; INT 12 - BIOS Get Memory Size
	dw	defaultHandler;  int_13			; INT 13 - BIOS Floppy Disk Services
	dw	defaultHandler;  int_14			; INT 14 - BIOS Serial Communications
	dw	defaultHandler;  int_15			; INT 15 - BIOS Misc. System Services
	dw	defaultHandler;  int_16			; INT 16 - BIOS Keyboard Services
	dw	defaultHandler;  int_17			; INT 17 - BIOS Parallel Printer svc.
	dw	defaultHandler;  int_18			; INT 18 - BIOS Start ROM BASIC
	dw	defaultHandler;  int_19			; INT 19 - BIOS Boot the OS
	dw	defaultHandler;  int_1A			; INT 1A - BIOS Time Services
	dw	defaultHandler		; INT 1B - DOS Keyboard Break
	dw	defaultHandler		; INT 1C - User Timer Tick
	dw	int_1D			; INT 1D - Video Parameters Table
	dw	defaultHandler;  int_1E			; INT 1E - Floppy Parameters Table
VECTORS_COUNT: equ ($-interrupt_table) / 2

SECTION .text

global setupVectors
setupVectors:
    push cs
    pop ds
    xor di, di
    mov es, di
    mov si, interrupt_table
    mov cx, VECTORS_COUNT
    mov ax, cs
.loop:
    movsw
    stosw
    loop .loop
    ret


defaultHandler:
    iret

int_ignore:
	push	ax
	push	ds
	mov	ax,biosDataSeg
	mov	ds,ax
	mov	al,0Bh			; PIC OCW3 - read in-service register
	out	pic1_reg0,al
	nop
	in	al,pic1_reg0		; get IRQ number
	mov	ah,al
	or	al,al
	jnz	.1
	mov	ah,0FFh
	jmp	.2
.1:
	in	al,pic1_reg1		; clear the interrupt
	or	al,ah
	out	pic1_reg1,al
	mov	al,20h			; end of interrupt
	out	pic1_reg0,al		; signal end of interrupt
.2:
	; mov	byte [last_irq],ah
	pop	ds
	pop	ax
	iret