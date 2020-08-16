; Boot Program of LzxOSs

; BIOS中断向量表：https://blog.csdn.net/piaopiaopiaopiaopiao/article/details/9735633
; 通过BIOS中断来实现各种功能
; 为软盘创建FAT12文件系统

org 0x7c00          ;伪指令，指定起始地址
StackBase equ 0x7c00
LoaderBase equ 0x1000
LoaderOffset equ 0x00           ;Loader addr = LodaerBase<<4 + LoaderOffset

TotalRootDirSectors equ 14      ;根目录区占用的扇区数 (224 * 32)/512 (上取整)
FAT1SectorStart equ 1           ;FAT表1的起始扇区号
RootDirSectorStart equ 19       ;根目录起始扇区号

FAT12_Info:                     ;FAT12文件系统结构信息
    jmp	short Boot_Start
    nop
    BS_OEMName	db	'Lzx Boot'
    BPB_BytesPerSec	dw	512     ;每扇区字节数
    BPB_SecPerClus	db	1       ;每簇扇区数
    BPB_RsvdSecCnt	dw	1       ;保留扇区数
    BPB_NumFATs	db	2           ;FAT表数
    BPB_RootEntCnt	dw	224     ;根目录最大目录项数
    BPB_TotSec16	dw	2880    ;总扇区数
    BPB_Media	db	0xf0        ;存储介质类型
    BPB_FATSz16	dw	9           ;FAT占用的扇区数
    BPB_SecPerTrk	dw	18      ;每磁道扇区数
    BPB_NumHeads	dw	2       ;磁头数
    BPB_HiddSec	dd	0           ;隐藏扇区数
    BPB_TotSec32	dd	0       ;总扇区数(当BPB_TotSec16为0时此字段生效)
    BS_DrvNum	db	0           ;int 13h的驱动器号
    BS_Reserved1	db	0
    BS_BootSig	db	0x29        ;扩展引导标记(0x29)
    BS_VolID	dd	0           ;卷序列号
    BS_VolLab	db	'boot loader'   ;卷标(磁盘名)
    BS_FileSysType	db	'FAT12   '  ;文件系统类型

;========== Boot 引导代码
Boot_Start:
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

;========== 从软盘中读取一个扇区
; arg1 AX 起始扇区号
; arg2 CL 读入扇区数量
; arg3 ES:BX 目标缓冲区起始地址
Func_ReadOneSector:
    push bp
    mov bp, bp
    sub esp, 2
    mov byte [bp-2], cl ;需要读的扇区数量
    push bx ;目标缓冲区
    mov bl, [BPB_SecPerTrk] ;每磁道扇区数
    div bl  ; ax/bl, AL=商，AH=余数
    inc ah  ; ah += 1 (起始扇区号，磁道内的扇区号从1开始计数，所以+1)
    mov cl, ah  ;扇区号
    mov dh, al
    shr al, 1   ; al = al >> 1 (柱面号)
    mov ch, al  ;磁道号(柱面号)的低8位
    and dh, 1  ; dh = al & 1 (磁头号)
    pop bx  ;目标缓冲区(物理内存的地址)
    mov dl, [BS_DrvNum] ;int13h的驱动器号
Label_GoOnReading:
    mov ah, 2   ;中断功能号(读取磁盘扇区)
    mov al, byte [bp-2] ;读入的扇区数
    int 13h     ;发起中断
    jc Label_GoOnReading  ;读取成功时，CF标志位=0，继续往下执行，否则再跳回去尝试读取
    add esp, 2  ;平衡栈
    pop bp
    ret

    ;========== Data:
StartBootMsg: db "LzxOS Start Booting ..."


    ;========== fill sector
    times 510-($-$$) db 0   ;将当前扇区用0字节填充
                            ;($-$$)表示当前行编译后的地址-本节程序的起始地址(这里只有一个section)，times伪指令用于重复操作
    dw 0xaa55           ; 0x55 0xAA 引导扇区的结尾标记 
