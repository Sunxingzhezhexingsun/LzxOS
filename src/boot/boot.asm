; Boot Program of LzxOSs

; BIOS中断向量表：https://blog.csdn.net/piaopiaopiaopiaopiao/article/details/9735633
; 通过BIOS中断来实现各种功能

org 0x7c00

StackBase equ 0x7c00

Lable_Start:

;===将CS寄存器的段基地址设置到DS、ES、SS等寄存器中
mov ax, cs
mov ds, ax
mov es, ax
mov ss, ax
mov sp, StackBase   ;设置栈指针（栈顶）

;========== clear screen
mov ax, 0600h       ;AH功能编号（滚动窗口） AL=滚动的列数，为0实现清空屏幕
mov bx, 0700h       ;
mov cx, 0
mov dx, 0184fh
int 10h             ;BIOS中断（显示服务）

;========== set focus
mov ax, 0200h       ;AH功能编号（设定游标位置）
mov bx, 0000h       ;BH=页码
mov dx, 0000h       ;DH=列数 DL=行数
int 10h             ;BIOS中断（显示服务）

;========== display on screen
;========== "LzxOS Start Booting ..."
mov ax, 1301h       ;AH功能编号（写字符串） AL=写入模式，AL=1时字符串属性由BL提供，CX提供字符串长度(字节)，光标移动至字符串末端
mov bx, 0002h       ;0 000 0 010 => 字体不闪烁 背景黑色 正常亮度 字体绿色 
mov dx, 0           ;DH=游标行号  DL=游标列号
mov cx, 24          ;字符串长度
push ax
mov ax, ds          ;DS 数据段基地址
mov es, ax
pop ax
mov bp, StartBootMsg ; ES:BP 字符串地址
int 10h

;========== reset floppy
;========== 实现了软盘的复位功能，相当于重新初始化了一次软盘驱动器，将软盘驱动器得磁头移动到默认得位置，这里无实质意义仅是示范
xor ah, ah          ;AH功能编号（重置磁盘驱动器）
xor dl, dl          ;DL代表驱动器号，DL=0 第一个软盘驱动器
int 13h             ;BIOS中断（低阶磁盘服务）
jmp $

;========== Data:
StartBootMsg: db "LzxOS Start Booting ..."
times 510-($-$$) db 0   ;将当前扇区用0字节填充
                        ;($-$$)表示当前行编译后的地址-本节程序的起始地址(这里只有一个section)，times伪指令用于重复操作
dw 0xaa55           ; 0x55 0xAA 引导扇区的结尾标记 