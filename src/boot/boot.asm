; Boot Program of LzxOSs
; nasm boot.asm -o boot.bin
; 编译出来是Intel8086 16bit指令格式
; BIOS中断向量表：https://blog.csdn.net/piaopiaopiaopiaopiao/article/details/9735633
; 通过BIOS中断来实现各种功能
; 为软盘创建FAT12文件系统

org 0x7c00          ;伪指令，指定起始地址
StackBase equ 0x7c00
LoaderBase equ 0x1000
LoaderOffset equ 0x00           ;Loader addr = LodaerBase<<4 + LoaderOffset

TotalRootDirSectors equ 14      ;根目录区占用的扇区数 (224 * 32)/512 (上取整)
FAT1SectorStart equ 1           ;FAT表1的起始扇区号
RootDirStartSectorNum equ 19       ;根目录起始扇区号

FAT12_Info:                     ;FAT12文件系统结构信息
    jmp	short Label_Boot_Start
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
Label_Boot_Start:
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
    mov cx, 23          ;字符串长度
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

;========== search loader.bin
    mov	word	[SectorNumber],	RootDirStartSectorNum ;搜索的起始扇区号=根目录区起始扇区号

Lable_Search_In_Root_Dir_Begin:
    cmp	word [RootDirSizeForLoop], 0 ;当RootDirSizeForLoop=0表示根目录区的所有扇区都搜索完了
    jz Label_LoaderBin_NotFound           ;跳转至搜索失败分支
    dec	word [RootDirSizeForLoop]	
    mov	ax,	00h
    mov	es,	ax                      ;
    mov	bx,	8000h                   ;目标缓冲区地址=es:bx
    mov	ax,	[SectorNumber]              ;待读取的扇区号
    mov	cl,	1                       ;读取的扇区数
    call Func_ReadSector            ;调用，将扇区中的数据读到内存中
    mov	si,	LoaderFileName
    mov	di,	8000h                   ;存放扇区数据的内存缓冲区地址
    cld         ;方向标志位 DF=0, 方向标志DF,决定内存地址是增大(DF=0)还是减小(DF=1)
    mov	dx,	10h                     ;计数，每个扇区可容纳的目录项数(512/32 = 16)

Label_Search_For_LoaderBin:
    cmp	dx,	0
    jz	Label_Goto_Next_Sector_In_Root_Dir  ;dx=0表示本扇区搜索完成，Jmp出去读下一个扇区
    dec	dx                          ;计数 dx -= 1
    mov	cx,	11                      ;文件名长度(FAT12文件系统目录项中规定的文件名长度为11字节)

Label_Cmp_FileName:
    cmp	cx,	0                       ;当cx=0,表示11个字节都比对完成，即找到了文件名为loader.bin的目录项
    jz	Label_LoaderBin_Found        ;跳转到搜索成功的分支
    dec	cx
    lodsb	                        ;从si寄存器指定地址读取一字节到al寄存器, 且si+=1 (DF=0则si增大)
    cmp	al,	byte [es:di]            ;比较al和es:di第一个字节的值
    jz	Label_Go_On                 ;相等，继续比对下一个字节
    jmp	Label_Different             ;不相等，直接跳转到比对失败的分支，

Label_Go_On:
    inc	di                          ;缓冲区地址+=1
    jmp	Label_Cmp_FileName          ;继续比对当前目录项的下一个字节

Label_Different:
    and	di,	0ffe0h                  ;0x20对齐
    add	di,	20h                     ;缓冲区地址+=32 (下一个目录项)
    mov	si,	LoaderFileName
    jmp	Label_Search_For_LoaderBin

Label_Goto_Next_Sector_In_Root_Dir:	
    add	word [SectorNumber],	1
    jmp	Lable_Search_In_Root_Dir_Begin

Label_LoaderBin_Found:

;========== display Loader not found message
Label_LoaderBin_NotFound:
    mov	ax,	1301h
    mov	bx,	008ch
    mov	dx,	0100h
    mov	cx,	27
    push ax
    mov	ax,	ds
    mov	es,	ax
    pop	ax
    mov	bp,	NoLoaderMsg
    int	10h
    jmp	$

;========== 函数Func_ReadSector 从软盘中读取N个扇区
; arg1 AX 起始扇区号
; arg2 CL 读入扇区数量
; arg3 ES:BX 目标缓冲区起始地址
Func_ReadSector:
    push bp
    mov bp, sp
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

    ;========== Data (相当于 .data)
StartBootMsg db "LzxOS Start Booting ...", 0
NoLoaderMsg db "ERROR: Loader Not Found !!!", 0
LoaderFileName db "LOADER  BIN", 0

    ;========== Variable (相当于 .bss)
SectorNumber dw 0
RootDirSizeForLoop dw TotalRootDirSectors

    ;========== fill sector
    times 510-($-$$) db 0   ;将当前扇区用0字节填充
                            ;($-$$)表示当前行编译后的地址-本节程序的起始地址(这里只有一个section)，times伪指令用于重复操作
    dw 0xaa55           ; 0x55 0xAA 引导扇区的结尾标记 
