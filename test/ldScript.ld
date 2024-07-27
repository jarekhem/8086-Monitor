_Segment_Size = 64k;
_ROM_Size   = 32k;
_ROM_Reserved = 4;
_Default_Alignment = 2;

MEMORY
{
  rom       (rx)  : ORIGIN = _Segment_Size - _ROM_Size, LENGTH = _ROM_Size - 16
  reset     (rx)  : ORIGIN = _Segment_Size - 16, LENGTH = 16 - _ROM_Reserved
  rom_res   (rx)  : ORIGIN = _Segment_Size - _ROM_Reserved, LENGTH = _ROM_Reserved
/*  ram      (rwx) : ORIGIN = 0x0000 + 1k, LENGTH = _Segment_Size - 1k*/
}

STACK_SIZE = 0x2000;

ENTRY(_start)

/* Section Definitions */
SECTIONS
{
    .text : ALIGN(_Default_Alignment)
    {
        . = ALIGN(_Default_Alignment);
        /* KEEP(*(.vectors .vectors.*)) */
        *(.text)
        *(.text*)
        *(.rodata*)
    } > rom

    /* .bss section which is used for uninitialized data */
    /* .bss (NOLOAD) :
    {
        *(.bss*)
        *(COMMON)
    } > ram

    .data :
    {
        *(.data);
    } > ram AT >rom

    /* stack section */
    /*.stack (NOLOAD):
    {
        . = ALIGN(8);
        . = . + STACK_SIZE;
        . = ALIGN(8);
    } > ram  */

    /* .fill :
    {
        FILL(0xF4F4F4F4);
        . = ORIGIN(rom) + LENGTH(rom);
    } > rom */

    .reset :
    {
        KEEP(*(.reset))
    } > reset

    .reserved :
    {
        KEEP(*(.reserved))
        FILL(0xFFFFFFFF);
        . = ORIGIN(rom_res) + LENGTH(rom_res) ;
    } > rom_res

    _end = . ;
}