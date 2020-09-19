TotalRootDirSectors equ 14      ;根目录区占用的扇区数 (224 * 32)/512 (上取整)
FAT1SectorStart equ 1           ;FAT表1的起始扇区号
RootDirStartSectorNum equ 19       ;根目录起始扇区号

FAT12_Info:                     ;FAT12文件系统结构信息

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
    BS_VolLab	db	'bootloader',0   ;卷标(磁盘名)
    BS_FileSysType	db	'FAT12   '  ;文件系统类型