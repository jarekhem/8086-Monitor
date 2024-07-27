%ifndef ARCHITECTURE_INC
%define ARCHITECTURE_INC

BITS 16
CPU 8086

biosCodeSeg	    equ	0xF000
biosStackSeg    equ 0x0030
biosDataSeg 	equ	0x0040

biosStackSize   equ 256

%endif