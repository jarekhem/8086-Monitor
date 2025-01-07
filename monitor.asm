BITS 16
CPU 8086

SECTION .text
global monitor_entry

%include "monitor_config.inc"

; Constants
prompt		equ '>'
colon		equ ':'
space		equ 0x20
backspace	equ 0x08
escape		equ 0x1B
enter_key	equ 0x0D

LF      equ     0Ah
CR      equ     0Dh

command		equ 0x20
addr        equ 0x22
segm        equ 0x24
value       equ 0x26
repeat      equ 0x28

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
;decode command
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
    mov byte [command], dl

    mov al, CR
	call monitor_putch
    mov al, LF
    call monitor_putch

decode_addr:
    mov word [addr], 0
    mov word [repeat], 0

    call skip_blank
    call parse_hex
 	mov al, [inBuf+bx]	; Get character for hex test.
    cmp al, ':' ; was it segment?.
    jne .not_segment
    test byte [command], 0x01 ; is it port access?
    jnz wrong_cmd ; yes, wrong command - segment is not allowed
    ; memory access, store segment
    mov word [segm], dx
    call print_segm
    inc bl
    call parse_hex
.not_segment:
    mov word [addr], dx
    call print_addr
    test byte [command], 0x02 ; is it write?
    jz .is_read                ; no, read - parse value
    mov al, [inBuf+bx]
    cmp al, '=' ; assign?
    jne .is_read
    inc bl
    call parse_hex
    mov word [value], dx
    call print_value
.is_read:
    mov al, [inBuf+bx]
    cmp al, '*' ; repeat?
    jne execute
    inc bl
    xor dx, dx
    call parse_hex
    mov word [repeat], dx


execute:
    mov dx, [segm]
    mov es, dx

    cmp byte [command], 0x04 ; mem read word?
    je .read_mem_word
    cmp byte [command], 0x00 ; mem read byte?
    je .read_mem_byte
    cmp byte [command], 0x05 ; port read word?
    je .read_port_word
    cmp byte [command], 0x01 ; port read byte?
    je .read_port_byte
    cmp byte [command], 0x06 ; mem write word?
    je .write_mem_word
    cmp byte [command], 0x02 ; mem write byte?
    je .write_mem_byte
    cmp byte [command], 0x07 ; port write word?
    je .write_port_word
    cmp byte [command], 0x03 ; port write byte?
    je .write_port_byte
    jmp wrong_cmd

.read_mem_byte:
    xor ax, ax
    mov bx, [addr]
    mov al, [es:bx]
    mov word [value], ax
    jmp .end

.read_mem_word:
    mov bx, [addr]
    mov ax, [es:bx]
    mov word [value], ax
    jmp .end

.read_port_byte:
    xor ax, ax
    mov dx, [addr]
    in al, dx
    mov word [value], ax
    jmp .end

.read_port_word:
    mov cx, [repeat]
.prw_loop:
    mov dx, [addr]
    in ax, dx
    mov word [value], ax
    mov ax, cx
    and ax, 0x0007    ; check repeat mod 8
    jnz .prw_loop_print
    mov al, CR
    call monitor_putch
    mov al, LF
    call monitor_putch
.prw_loop_print:
    mov al, ' '
    call monitor_putch
    mov dx, [value]
    mov al, dl
    call print_byte
    mov al, ' '
    call monitor_putch
    mov al, dh
    call print_byte

    loop .prw_loop
    jmp start_prompt

.write_mem_byte:
    mov bx, [addr]
    mov al, [value]
    mov byte [es:bx], al
    jmp .end

.write_mem_word:
    mov bx, [addr]
    mov ax, [value]
    mov word [es:bx], ax
    jmp .end

.write_port_byte:
    mov dx, [addr]
    mov al, [value]
    out dx, al
    jmp .end

.write_port_word:
    mov dx, [addr]
    mov ax, [value]
    out dx, ax
.end:
    call print_value
    jmp start_prompt

; parse hex value
; dx - returns the parsed value
parse_hex:
    mov dx, 0
.next_char:
 	mov al, [inBuf+bx]	; Get character for hex test.
 	xor al, 0x30		; Map digits $0-9
 	cmp al, 0x0A		; Digit?
 	jc .digit			; Yes.
 	adc al, 0x89		; Map letter "A"-"F" to $FA-$FF
 	cmp al, 0xFA		; Hex letter?
 	jc .not_hex		; No, character not hex.
.digit:
    inc bl            ; Advance text index.
    mov cl, 4       ; Shift count.
 	sal al, cl		; Hex digit to MSD of A.
.shift_hex:
 	sal al, 1		; Hex digit left, MSB to carry.
 	rcl dx, 1
 	loop .shift_hex		; 4 shifts
 	jmp .next_char
.not_hex:
    ret

print_segm:
    mov word dx, [segm]
    call print_addr
    mov al, ':'
    call monitor_putch
    ret

print_value:
    mov al, ' '
    call monitor_putch
    mov word dx, [value]
    call print_addr
    ret

; print hex address
; dx - the address
print_addr:
  	mov al, dh
  	call print_byte
  	mov al, dl
  	call print_byte
  	ret

wrong_cmd:
	mov al, ' '
	call monitor_putch
	mov al, '?'
	call monitor_putch
	jmp start_prompt

skip_blank:
    inc bl
    mov al, [inBuf+bx]
    cmp al, ' '
    jbe skip_blank
    ret

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
