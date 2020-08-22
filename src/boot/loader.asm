; Loader
; nasm loader.asm -o loader.bin

org	10000h
    mov	ax,	cs
    mov	ds,	ax
    mov	es,	ax
    mov	ax,	0x00
    mov	ss,	ax
    mov	sp,	0x7c00

;=======	display on screen :LzxOS Start Loader......
    mov	ax,	1301h
    mov	bx,	000fh
    mov	dx,	0200h		;row 2
    mov	cx,	18
    push ax
    mov	ax,	ds
    mov	es,	ax
    pop	ax
    mov	bp,	StartLoaderMessage
    int	10h
    jmp	$

;=======	display messages

StartLoaderMessage:	db	"LzxOS Start Loader"
times 512*5-($-$$) db 0   ;用0字节填充5个扇区





