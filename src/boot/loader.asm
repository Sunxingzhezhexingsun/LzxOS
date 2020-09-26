; Loader
; nasm loader.asm -o loader.bin

org	10000h
    jmp Label_Start

%include "fat12.inc"

BaseOfKernelFile equ 0x00
OffsetOfKernelFile equ 0x100000 ;1MB kernel base

BaseOfTmpKernelFile equ 0x00
OffsetOfTmpKernelFile equ 0x7E00 ;0x7C00 + 512bytes  temp buffer of kernel

MemoryStructBuffer equ 0x7E00 ; buffer of memory struct

;=======    global descriptor table
[SECTION gdt]
Label_GDT:              dd 0,0
Label_DESC_CODE32:      dd 0x0000FFFF,0x00CF9A00
Label_DESC_DATA32:      dd 0x0000FFFF,0x00CF9200

GdtLen  equ $-Label_GDT

GdtPtr:     dw GdtLen
            dd Label_GDT

SelectorCode32  equ Label_DESC_CODE32 - Label_GDT
SelectorData32  equ Label_DESC_DATA32 - Label_GDT

[SECTION .s16]
[BITS 16]
Label_Start:
    mov	ax,	cs
    mov	ds,	ax
    mov	es,	ax
    mov	ax,	0x00
    mov	ss,	ax
    mov	sp,	0x7c00

;=======	display on screen :LzxOS Start Loader......
    mov	ax,	1301h
    mov	bx,	000fh
    mov	dx,	0200h		;row 3
    mov	cx,	18
    push ax
    mov	ax,	ds
    mov	es,	ax
    pop	ax
    mov	bp,	StartLoaderMessage
    int	10h

;=======    open address A20
    push ax
    in al, 92h
    or al, 00000010b    ; set 92h port
    out 92h, al
    pop ax
    cli ; clear interrupt
    db 0x66
    lgdt [GdtPtr]       ; load gdtr
    mov eax, cr0
    or eax, 00000001b
    mov cr0, eax    ; set CR0.PE, Protected Mode
    mov ax, SelectorData32
    mov fs, ax
    mov eax, cr0
    and eax, 111111110b
    mov cr0, eax    ; clear CR0.PE, Real Mode
    sti ; set interrupt

;=======    search kernel.bin
    mov	word	[SectorNumber],	RootDirStartSectorNum ;搜索的起始扇区号=根目录区起始扇区号

Lable_Search_In_Root_Dir_Begin:
    cmp	word [RootDirSizeForLoop], 0 ;当RootDirSizeForLoop=0表示根目录区的所有扇区都搜索完了
    jz LabelKernelBin_NotFound           ;跳转至搜索失败分支
    dec	word [RootDirSizeForLoop]	
    mov	ax,	00h
    mov	es,	ax                      ;
    mov	bx,	8000h                   ;目标缓冲区地址=es:bx
    mov	ax,	[SectorNumber]              ;待读取的扇区号
    mov	cl,	1                       ;读取的扇区数
    call Func_ReadSector            ;调用，将扇区中的数据读到内存中
    mov	si,	KernelFileName
    mov	di,	8000h                   ;存放扇区数据的内存缓冲区地址
    cld         ;方向标志位 DF=0, 方向标志DF,决定内存地址是增大(DF=0)还是减小(DF=1)
    mov	dx,	10h                     ;计数，每个扇区可容纳的目录项数(512/32 = 16)

Label_Search_For_KernelBin:
    cmp	dx,	0
    jz	Label_Goto_Next_Sector_In_Root_Dir  ;dx=0表示本扇区搜索完成，Jmp出去读下一个扇区
    dec	dx                          ;计数 dx -= 1
    mov	cx,	11                      ;文件名长度(FAT12文件系统目录项中规定的文件名长度为11字节)

Label_Cmp_FileName:
    cmp	cx,	0                       ;当cx=0,表示11个字节都比对完成，即找到了文件名为loader.bin的目录项
    jz	LabelKernelBin_Found        ;跳转到搜索成功的分支
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
    mov	si,	KernelFileName
    jmp	Label_Search_For_KernelBin

Label_Goto_Next_Sector_In_Root_Dir:	
    add	word [SectorNumber],	1
    jmp	Lable_Search_In_Root_Dir_Begin

;========== display Kernel not found message
LabelKernelBin_NotFound:
    mov	ax,	1301h
    mov	bx,	008ch
    mov	dx,	0400h   ;DH=游标行号  DL=游标列号
    mov	cx,	27
    push ax
    mov	ax,	ds
    mov	es,	ax
    pop	ax
    mov	bp,	NoKernelMsg
    int	10h
    jmp	$

;========== load kernel.bin to memory
LabelKernelBin_Found:
    and di, 0ffe0h               ;0x20对齐
    add di, 01ah                 ;DIR_FstClus
    mov cx, word [es:di]        ;2bytes
    mov ax, BaseOfTmpKernelFile ;0
    mov es, ax
    mov bx, OffsetOfTmpKernelFile   ;0x7E00
Label_Go_On_Loading_KernelBin:
    push cx                     ;cx=簇号(FAT表项中的)，还需加上前面的扇区才是绝对簇号
    add cx, RootDirStartSectorNum
    add cx, TotalRootDirSectors
    sub cx, 2                   ;FAT表项0和1是不能用的，FAT[2]对应数据区的0簇
    mov ax, cx
    push ax
    push bx
    mov	ah,	0eh
    mov	al,	'.'
    mov	bl,	0fh
    int	10h                     ;输出一个字符 “.”
    pop bx
    pop ax
    mov cl, 1
    call Func_ReadSector
    pop ax                      ;ax=簇号(FAT表项中的)     
    ;;;;;;;;;;;;                copy sector from tmp to kernelBase
    push eax
    push cx
    push edi
    push esi
    push ds

    mov cx, 200h
    ; mov ax, BaseOfKernelFile
    ; mov fs, ax
    mov edi, dword [OffsetOfKernelFileCount]
    mov esi, OffsetOfTmpKernelFile
    mov ax, BaseOfTmpKernelFile
    mov ds, ax
Label_Mov_Kernel:
    mov al, byte [ds:esi]
    mov byte [fs:edi], al
    inc esi
    inc edi
    loop Label_Mov_Kernel       ;loop [cx] times Label_Mov_Kernel
    mov eax, 0x1000
    mov ds, eax
    mov dword [OffsetOfKernelFileCount], edi

    pop ds
    pop esi
    pop edi
    pop cx
    pop eax
    ;;;;;;;;;;
    
    call Func_ParseFATEntry
    mov cx, ax
    cmp cx, 0fffh               ;FAT表项内容为0xfff表示这是文件的最后一个簇
    jz Label_Load_KernelBin_Done    ;kernel.bin加载完成
    jmp Label_Go_On_Loading_KernelBin   ;继续加载下一个簇到内存

;========== load kernel.bin done
Label_Load_KernelBin_Done:
    mov	ax, 0B800h
    mov	gs, ax
    mov	ah, 0Fh				; 0000: 黑底    1111: 白字
    mov	al, 'G'
    mov	[gs:((80 * 0 + 39) * 2)], ax	; 屏幕第 0 行, 第 39 列。
;========== Kill Motor, 关闭软盘驱动马达
    push dx
    mov dx, 3F2h
    mov al, 0
    out dx, al          ; 端口号必须为8位常量或16位dx中的值
    pop dx

;========== Get Memory Map Struct
    mov bx, 0       ;style
    mov	cx,	23      ;length
    mov	dx,	0400h   ;DH=游标行号  DL=游标列号
    mov bp, StartGetMemStructMsg
    call Func_PrintLine

    mov ax, 00h
    mov es, ax
    mov di, MemoryStructBuffer
    xor ebx, ebx

Label_Get_Mem_Struct:
    ; mov edx, 534D4150h
    mov edx, [SMAP]
    mov eax, 0E820h
    mov ecx, 20
    int 15h
    jc Label_Get_Mem_Fail
    add di, 20
    cmp ebx, 0
    jnz Label_Get_Mem_Struct
    jmp Label_Get_Mem_Done

Label_Get_Mem_Fail:
    mov bx, 1
    mov cx, 23
    mov dx, 0600h   ;row 6
    mov bp, GetMemStructErrMessage
    call Func_PrintLine
    jmp $

Label_Get_Mem_Done:
    mov bx, 0
    mov cx, 29
    mov dx, 0600h   ;row 6
    mov bp, GetMemStructOKMessage
    call Func_PrintLine
    jmp $


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

;========== Parse FAT Entry 根据FAT表项索引出下一个簇号
; arg1 AX = FAT Entry Number
; ret AX = Next FAT Entry Number
Func_ParseFATEntry:
    push es
    push bx
    push ax
    mov ax, 00
    mov es, ax
    pop ax              ; ax=FAT表项号
    mov byte [Odd], 0
    mov bx, 3
    mul bx              ; ax=ax*3
    mov bx, 2
    div bx              ; ax=ax/2  ax=商 dx=余数
    cmp dx, 0
    jz Label_Even       ; 余数为0则跳转
    mov byte [Odd], 1   ; FAT号为奇数
Label_Even:
    xor dx, dx
    mov bx, [BPB_BytesPerSec]
    div bx              ; ax=ax/bx 商值ax为FAT表项的偏移扇区号，余数dx为扇区内偏移
    push dx
    add ax, FAT1SectorStart    ; arg1=扇区号
    mov cl, 2                  ; arg2=读入的扇区数量 这里读取2个扇区，可以应对FAT表项跨扇区的情况
    mov bx, 8000h              ; arg3=目标缓冲区地址
    call Func_ReadSector
    pop dx
    add bx, dx          ; FAT表项所在的地址
    mov ax, [es:bx]     ; 直接读取16字节
    cmp byte [Odd], 0
    jz Label_Even_2    ; 表项号是偶数则跳转
    shr ax, 4           ; 表项号是奇数，高12位才是表项的数据，因此右移四位，舍弃低4位
Label_Even_2:
    and ax, 0fffh       ; 表项号是偶数，低12位才是表项的数据，因此只取低12字节
    pop bx
    pop es
    ret

;========== Print One Line 打印一行字符串
; arg1 BX = Style (0=common, !0=warnning)
; arg1 CX = length of string
; arg2 DH = line number, DL = column number
; arg3 BP = pointer to message
Func_PrintLine:
    push bx
    push es
    push ax
    cmp bx, 0
    jnz Label_Warn
    mov	bx,	000Fh
    jmp Label_Continue
Label_Warn:
    mov bx, 008Ch
Label_Continue:
    mov	ax,	ds
    mov	es,	ax
    mov	ax,	1301h
    
    int 10h
    pop ax
    pop es
    pop bx
    ret

;=======	Data (.data)
StartLoaderMessage:	db	"LzxOS Start Loader", 0
KernelFileName: db "KERNEL  BIN", 0
NoKernelMsg: db "Kernel not found !!!", 0
KernelFoundMsg: db "Kernel found", 0
StartGetMemStructMsg: db "Start Get Memory Struct", 0
GetMemStructErrMessage:	db	"Get Memory Struct ERROR", 0
GetMemStructOKMessage:	db	"Get Memory Struct Successful!"
SMAP: db "PAMS"
      db 0*28


;========== Variable (相当于 .bss)
SectorNumber: dw 0
RootDirSizeForLoop: dw TotalRootDirSectors
Odd: db 0  ;FAT表项的奇偶校验位
OffsetOfKernelFileCount: dd OffsetOfKernelFile

times 512*5-($-$$) db 0   ;用0字节填充5个扇区

