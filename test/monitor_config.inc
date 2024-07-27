%ifndef MONITOR_CONFIG_INC
%define MONITOR_CONFIG_INC

extern vga_putch
extern kb_getch

%define monitor_getch   kb_getch
%define monitor_putch   vga_putch

%endif
