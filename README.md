# LzxOS
> 《一个64位操作系统的设计与实现》 学习记录及代码
>
> 这个README.MD就当是记录过程了

# 虚拟环境

原书用的bochs，我在ubuntu18上装最新版的bochs有问题没解决，所以用了qemu

```
sudo apt-get install qemu
```

编译boot.asm

```
nasm ./boot.asm -o boot.bin
```

将二进制文件写入软盘映像

```
dd if=boot.bin of=./boot.img bs=512 count=1 conv=notrunc
```

qemu启动脚本：

```shell
# boot.sh
qemu-system-x86_64 \
 -boot a \ #使用
   -fda $1 \ #挂载$1到第一个软盘
   -m 2048 
```

Run:

```
./boot.sh ./boot.img
```

<img src="README.assets/image-20200814002712828.png" style="zoom:50%;" />

> 鼠标点进去QEMU窗口后会锁定出不来，我是mac上开的vmware ubuntu，直接Ctrl+空格的话是鼠标回到mac主机，要从QEMU回到ubuntu虚拟机的话，得按ctrl+option+空格

