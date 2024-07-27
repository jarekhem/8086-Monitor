BITS 16
CPU 8086

SECTION .text
global monitor_entry

%include "monitor_config.inc"


; extern print
; extern print_dec
; extern print_hex
; extern print_digit
; extern print_byte

; Constants
prompt		equ '>'
colon		equ ':'
space		equ 0x20
backspace	equ 0x08
escape		equ 0x1B
enter_key	equ 0x0D

LF      equ     0Ah
CR      equ     0Dh

; xaml	equ 0x24    ;  Last "opened" location
; xamh	equ 0x25		
; stl		equ 0x26	;  Store address
; sth		equ 0x27	;  Store address 
; l	    equ 0x28    ;  Hex value parsing
; h	    equ 0x29    ;  Hex value parsing
; ysav    equ 0x2A    ;  Used to see if hex value is given
; mode    equ 0x2B    ;  $00=XAM, $74=STOR, $B8=BLOCK XAM
; segm 	equ 0x2D

command		equ 0x20

inBuf       equ 0x200


; command 0b0000bwda
;	* (a)ccess: 		0 - mem, 1 - port	(not used in block read)
;	* (d)irection:		0 - read, 1 - write	(not used in block read)
;	* (w)idth:			0 - byte, 1 - word	(not used in block read)
;	* (b)lock read:		0 - no, 1- yes


monitor_entry:
	xor ax, ax
	xor cx, cx
	mov es, ax

start_prompt:
	xor bx, bx
	mov al, CR
	call monitor_putch
    mov al, LF
    call monitor_putch
	mov al, prompt
	call monitor_putch
read_char:
	call monitor_getch		
	jnc read_char		; Loop until ready.
	cmp al, 'a'		; convert to upper if needed
	jnge .already_upper
	sub al, 32
.already_upper:
; 	cmp bl, 0
; 	jne .not_mem_port
; .not_mem_port:
; 	cmp bl, 1
; 	jne .not_read_write
; .not_read_write:
; 	cmp bl, 2
; 	jne .not_byte_word
; .not_byte_word:
	xor bh, bh
	mov [inBuf+bx], al		; Add to text buffer.
	cmp al, enter_key	
	je .line_complete
	cmp al, backspace
	jne .not_backspace
	dec bl
	js .max_backspace
	call monitor_putch		
	mov al, space
	call monitor_putch
	mov al, backspace
	call monitor_putch
	jmp read_char
.max_backspace:
	xor bx, bx
	jmp read_char
.not_backspace:
	cmp al, escape
	je start_prompt
	call monitor_putch		; Display character.
	inc bl
	js start_prompt
	jmp read_char
.line_complete:
; decode command:
	xor ax, ax
	xor bx, bx
	xor cx, cx
	xor dx, dx

	mov al, [inBuf+bx]
	cmp al, 'M'	; is mem?
	je .is_mem
	cmp al, 'P'	; is port?
	jne wrong_cmd
	or dl, 0x01
.is_mem:

	inc bl
	mov al, [inBuf+bx]
	cmp al, 'R'	; is read?
	je .is_read
	cmp al, 'W'	; is write?
	jne wrong_cmd
	or dl, 0x02
.is_read:

	inc bl
	mov al, [inBuf+bx]
	cmp al, 'B'	; is byte?
	je .is_byte
	cmp al, 'W'	; is word?
	jne wrong_cmd
	or dl, 0x04
.is_byte:

	mov al, CR
	call monitor_putch
    mov al, LF
    call monitor_putch

; parse address:
; parse_hex:
; 	mov al, [inBuf+bx]	; Get character for hex test.
; 	xor al, 0x30		; Map digits $0-9
; 	cmp al, 0x0A		; Digit?
; 	jc digit			; Yes.
; 	adc al, 0x89		; Map letter "A"-"F" to $FA-$FF
; 	cmp al, 0xFA		; Hex letter?
; 	jc not_hex		; No, character not hex.
; digit:
; 	sal al, 4		; Hex digit to MSD of A.
; 	mov cx, 0x04		; Shift count.
; shift_hex:
; 	sal al, 1		; Hex digit left, MSB to carry.
; 	rcl byte[l], 1
; 	rcl byte[h], 1
; 	loop shift_hex		; 4 shifts
; not_hex:
	; cmp bl, byte [ysav]	; Check if L, H empty (no hex digits).
	; je ESCAPE		; Yes, generate ESC sequence.
	; test byte [mode], 0b01000000
	; jz NOTSTOR		; B6 = 0 for STOR, 1 for XAM and BLOCK XAM
	; 			; LSD's of hex data.

	mov al, enter_key
	call monitor_putch
	jmp start_prompt

wrong_cmd:
	mov al, ' '
	call monitor_putch
	mov al, '?'
	call monitor_putch
	jmp start_prompt

print_hex:
	push ax
	and	al, 0x0F
	add	al,'0'			; convert to ASCII
	cmp	al,'9'			; less or equal 9?
	jna	.1
	add	al,'A'-'9'-1		; a hex digit
.1:
    call monitor_putch
	pop	ax
	ret

print_byte:
	rol	al,1
	rol	al,1
	rol	al,1
	rol	al,1
	call print_hex
	rol	al,1
	rol	al,1
	rol	al,1
	rol	al,1
	call print_hex
	ret

;----------------------------------------------------------
; RESET:
; 	xor ax, ax
; 	xor bx, bx
; 	xor cx, cx
; 	mov es, ax
; 	clc
; 	mov bl, 0x7F		
; NOTCR:
; 	cmp al, backspace	; Backspace?
; 	je BACKSPACE		; Yes.
; 	cmp al, escape		; ESC?
; 	je ESCAPE		; Yes.
; 	inc bl			; Advance text index.
; 	jns NEXTCHAR		; Auto ESC if > 127.
; ESCAPE:
; 	; mov al, enter_key
; 	; call serial_putch
; 	mov al, prompt		; "\".
; 	call serial_putch		; Output it.
; GETLINE:
; 	mov al, enter_key		; CR.
; 	call serial_putch		; Output it.
; 	mov bl, 1		; Initialize text index.
; BACKSPACE:
; 	dec bl			; Back up text index.
; 	js GETLINE		; Beyond start of line, reinitialize.
; 	; mov al, space		; Overwrite the old character with a space
; 	; call serial_putch
; 	; mov al, backspace
; 	; call serial_putch	; Move back by one character again
; NEXTCHAR:
; 	call serial_getch		; Key ready?
; 	jnc NEXTCHAR		; Loop until ready.
; 	cmp al, 'a'		; convert to upper if needed
; 	jnge .ISUPPER
; 	sub al, 32
; .ISUPPER:
; 	xor bh, bh
; 	mov [inBuf+bx], al	; Add to text buffer.
; 	call serial_putch		; Display character.
; 	cmp al, enter_key	; CR?
; 	jne NOTCR		; No.
; 	mov bl, 0xFF		; Reset text index.
; 	mov ax, 0x00		; For XAM mode.
; 	mov cx, ax		; 0 -> X.
; SETBLOCK:
; 	shl al, 1		; Leaves $7B if setting STOR mode
; SETSTOR:
; 	shl al, 1		; Leaves $7B if setting STOR mode
; SETMODE:
; 	mov [mode], al		; $00 = XAM, $74 = STOR, $B8 = BLOK XAM
; BLSKIP:
; 	inc bl			; Advance text index.
; NEXTITEM:
; 	xor bh, bh
; 	mov al, [inBuf+bx]	; Get character.
; 	cmp al, enter_key	; CR?
; 	je GETLINE		; Yes, done this line.
; 	cmp al, '.'		; "."?
; 	jl BLSKIP		; Skip delimiter.
; 	je SETBLOCK		; Set BLOCK XAM mode.
; 	cmp al, ':'		; ":"?
; 	je SETSTOR		; Yes, set STOR mode.
; 	cmp al, 'R'		; "R"?
; 	je RUN			; Yes, run user program.
; 	cmp al, 'S'		; "S"?
; 	je SETSEG			; Yes, set the segment.
; 	cmp al, 'I'		; "I"?
; 	je IOREADB			; Yes, read byte from IO addr.
; 	mov word [l], 0
; 	mov [ysav], bl		; Save Y for comparison.
; NEXTHEX:
; 	xor bh, bh
; 	mov al, [inBuf+bx]	; Get character for hex test.
; 	xor al, 0x30		; Map digits $0-9
; 	cmp al, 0x0A		; Digit?
; 	jc DIG			; Yes.
; 	adc al, 0x89		; Map letter "A"-"F" to $FA-$FF
; 	cmp al, 0xFA		; Hex letter?
; 	jc NOTHEX		; No, character not hex.
; DIG:
; 	sal al, 4		; Hex digit to MSD of A.
; 	mov cx, 0x04		; Shift count.
; HEXSHIFT:
; 	sal al, 1		; Hex digit left, MSB to carry.
; 	rcl byte[l], 1
; 	rcl byte[h], 1
; 	loop HEXSHIFT		; 4 shifts

; 	inc bl			; Advance text index.
; 	jmp NEXTHEX		; Always taken. Check next character for hex.
; NOTHEX:
; 	cmp bl, byte [ysav]	; Check if L, H empty (no hex digits).
; 	je ESCAPE		; Yes, generate ESC sequence.
; 	test byte [mode], 0b01000000
; 	jz NOTSTOR		; B6 = 0 for STOR, 1 for XAM and BLOCK XAM
; 				; LSD's of hex data.
	
; 	push bx
; 	mov cl, [l] 
; 	mov bx, [stl]
; 	mov es:[bx], cl	; Store at current 'store index'
; 	xor cx, cx
; 	pop bx

; 	inc byte [stl]	; Increment store index.
; 	jne NEXTITEM
; 	inc byte [sth]	; Increment store index.
; TONEXTITEM:
; 	jmp NEXTITEM		; Get next command item.
; RUN:
; 	call word [xaml]		; Run at current XAM index.
; 	jmp GETLINE
; SETSEG:
; 	mov ax, [xaml]
; 	mov es, ax		; xam -> ES.
; 	jmp GETLINE
; IOWRITEB:
; 	mov dx, [xaml]
; 	mov es, ax		; xam -> ES.
; 	jmp GETLINE
; IOREADB:
; 	xor ax, ax
; 	mov dx, [xaml]
; 	in al, dx		
; 	call PRBYTE
; 	jmp GETLINE
; NOTSTOR:
; 	test byte al, [mode]
; 	js XAMNEXT		; B7 = 0 for XAM, 1 for BLOCK XAM
; SETADR:
; 				; Copy hex data to
; 	push ax
; 	mov ax, [l] 
; 	mov [stl], ax
; 	mov [xaml], ax
; 	pop ax
; NXTPRNT:
; 	jnz PRDATA		; NE means no address to print.
; 	mov al, enter_key		; CR.
; 	call serial_putch		; Output it.
; 	mov ax, [xaml]
; 	call print_hex
; 	mov al, ':'		; ":".
; 	call serial_putch		; Output it.
; PRDATA:
; 	mov al, ' '		; Blank.
; 	call serial_putch		; Output it.

; 	push bx
; 	mov bx, [xaml]
; 	mov al, es:[bx]
; 	pop bx
; 	call PRBYTE		; Output it in hex format.
; XAMNEXT:
; 	mov byte [mode], 0x00	; 0 -> MODE (XAM mode).
; 	push ax
; 	mov al, [xaml]
; 	cmp al, [l]
; 	mov al, [xamh]
; 	sbb al, [h]
; 	pop ax
; 	jnc TONEXTITEM		; Not less, so no more data to output.
; 	inc byte [xaml]			; Increment 'examine index'.
; 	jnz MOD16HK
; 	inc byte [xamh]
; MOD16HK:
; 	mov al, [xaml]		; Check low-order 'examine index' byte
; 	and al, 0x0F		; For MOD 16 = 0
; 	jmp NXTPRNT		; Always taken.
; PRBYTE:
; 	push ax			; Save A for LSD.
; 	shr al, 4		; MSD to LSD position.
; 	call PRHEX		; Output hex digit.
; 	pop ax			; Restore A.
; PRHEX:
; 	and al, 0x0F		; Mask LSD for hex print.
; 	or al, '0'		; Add "0".
; 	cmp al, '9'+1		; Digit?
; 	jl .PR			; Yes, output it.
; 	add al, 7		; Add offset for character.
; .PR:
; 	call serial_putch
; 	ret