# 在VM虚拟机中安装配置archlinux
#### hellmonky


####（1）设置虚拟机的基本硬件属性和引导光盘路径：


####（2）打开虚拟机，用光盘引导启动之后，到达启动界面：

![](./pic/1.PNG)

####（3）livecd会自动以root用户登录，准备开始安装：

![image](./pic/2.PNG)

####（4）进行分区：
常见的分区表有GPT和MBR，此处以MBR分区表为例。
GPT分区有常用工具cgdisk，对应的MBR分区有工具cfdisk，但是Arch Linux的官方文档指出，cfdisk工具此处存在一定的问题，原文如下：

Note: There is also cfdisk, which is similar in UI to cgdisk, but it currently does not automatically align the first partition properly. That is why the classicfdisk tool is used here.

所以命令运行 fdisk 进行分区操作：

```shell
fdisk /dev/sda
```

![image](./pic/3.PNG)
按照提示开始建立分区


建立第一个分区:
Command (m for help): 输入 n 并按下 Enter
Partition type: Select (default p): 按下 Enter
Partition number (1-4, default 1): 按下 Enter
First sector (2048-209715199, default 2048): 按下 Enter
Last sector, +sectors or +size{K,M,G} (2048-209715199....., default 209715199): 输入 +15G 并按下 Enter
然后建立第二个分区:

Command (m for help): 输入 n 并按下 Enter
Partition type: Select (default p): 按下 Enter
Partition number (1-4, default 2): 按下 Enter
First sector (31459328-209715199, default 31459328): 按下 Enter
Last sector, +sectors or +size{K,M,G} (31459328-209715199....., default 209715199): 按下 Enter 

在安装中因为在虚拟机中不存在多个硬盘，只建立一个分区：
Command (m for help): 输入 n 并按下 Enter
Partition type: Select (default p): 按下 Enter
Partition number (1-4, default 1): 按下 Enter
First sector (2048-209715199, default 2048): 按下 Enter
Last sector, +sectors or +size{K,M,G} (2048-209715199....., default 209715199): 按下 Enter
![image](./pic/4.PNG)
通过上述命令对整个硬盘完成了分区操作。
现在预览下新的分区表:

```shell
Command (m for help): 输入 p 并按下 Enter
```

![image](./pic/5.PNG)

可以通过查看当前分区是否满足要求。当前在虚拟机中就按照这种分区来简化操作。
向磁盘写入这些改动:

```shell
Command (m for help): 输入 w 并按下 Enter
```

如果一切顺利无错误的话 fdisk 程序将会显示以下信息:

![image](./pic/6.PNG)

如果因 fdisk 遇到错误导致以上操作无法完成, 你能使用 q 命令来退出。
####（5）创建文件系统、挂在分区
简历分区完毕之后就需要创建文件系统来准备安装了，使用EXT4格式的文件系统对当前分区格式化操作：
```shell
mkfs.ext4 /dev/sda1
```

![image](./pic/7.PNG)
如果你还分了/home，就继续 mkfs.ext4 /dev/sdaX x是你的home的分区名。
同理，如果你分了一个swap区，不要忘了格式化和启用它：

```shell
mkswap /dev/sdaX
```

```shell
swapon /dev/sdaX
```

（需要注意的是，UEFI 系统需要格式化 ESP 分区(假设是 /dev/sda1)，命令是：没有查找到结果）
挂载分区之前，要检查当前磁盘的标识符和布局：

```shell
lsblk /dev/sda
```

![image](./pic/8.PNG)

注意要按照顺序挂载，先挂载根分区到/mnt.

```shell
mount /dev/sda1 /mnt
```


然后挂载/home分区和其余单独分区(/boot, /var 等)。

```shell
mkdir /mnt/home
```


```shell
mount /dev/sda2 /mnt/home
```


因为虚拟机中没有多个分区，所以只进行如下处理就好：

```shell
mount /dev/sda1 /mnt
```

![image](./pic/9.PNG)



####（6）编译源列表，选择下载路径（虚拟机安装中为了方便没有做源的替换，跳过这个步骤了，请按照网络的具体情况选择）
选择安装镜像，首先要编辑源列表。

livecd自带的源列表中，位于中国的是华中科大的源，可以另外添加163的源，速度很快，注意要将自己添加的源放置在其他的源之前

```shell
vim /etc/pacman.d/mirrorlist
```


根据archlinux官方给出的源列表（https://wiki.archlinux.org/index.php/Mirrors），选择搜狐的源：

http://mirrors.sohu.com/archlinux/

添加内容为：

```shell
Server = http://mirrors.sohu.com/archlinux/$repo/os/i686
```


![image](./pic/10.PNG)

一旦更改了镜像列表，请务必记得使用 pacman -Syy 强制刷新
```shell
pacman -Syy
```




####（7）安装最小系统

#####<1>使用 pacstrap 脚本安装基本系统
```shell
pacstrap /mnt base base-devel
```


![image](./pic/11.PNG)


base系统主要的内容在网页（https://www.archlinux.org/groups/x86_64/base/ ）上有详细的说明，这儿没有安装base-devel是因为目前暂时不需要编译开发，先搭建基本执行环境。

![image](./pic/12.PNG)

下载完毕之后自动开始安装，完毕之后显示执行的时间等内容。

#####<2>生成 fstab

用下面命令生成 fstab。如果想使用 UUIDs，使用 -U 选项；如果想使用标签，用 -L 选项.

```shell
genfstab -U -p /mnt >> /mnt/etc/fstab
```


![image](./pic/13.PNG)

然后使用vim或者nano来查看是否生成成功：

```shell
nano /mnt/etc/fstab
```


![image](./pic/14.PNG)

然后ctrl+x退出。


#####<3>下面要 chroot 到新安装的系统：

```shell
arch-chroot /mnt
```


![image](./pic/15.PNG)

注意: 可以使用arch-chroot /mnt /bin/bash进入 bash shell.

到这一步之后，开始系统的主要配置，如果下面文件不存在，需要手动创建。

#####<4>配置locale，设置默认语言

glibc 和其他一些支持本地化的程序或者库使用 Locales 自动翻译输出各种语言的"独特" 文本, 并且合适的显示地域、货币、时区以及日期格式、字符排列方式和其他一些本地化的特定标准。

需要编辑两个文件：/etc/locale.gen 和 /etc/locale.gen.

其中/etc/locale.gen，这个文件所有的内容都是注释的，所以在最上面添加en_US.UTF-8 UTF-8即可。

```shell
nano /etc/locale.gen
```


![image](./pic/16.PNG)

去掉 en_US.UTF-8 UTF-8 之前的#，然后Ctrl+o保存Ctrl+x退出。

![image](./pic/17.PNG)

然后执行

```shell
locale-gen
```


每次glibc更新之后就会运行 locale-gen 一次， 重新生成 /etc/locale.gen 指定的本地化文件。

接着配置locale.conf

```shell
echo LANG=en_US.UTF-8 > /etc/locale.conf 
```


```shell
export LANG=en_US.UTF-8
```


![image](./pic/18.PNG)


#####<5>给计算机起名

使用如下命令设置计算机的名称：

```shell
echo myhostname > /etc/hostname
```


其中的myhostname就是你要设定的计算机名称

![image](./pic/19.PNG)

#####<6>设置终端字体和键盘映射

编辑/etc/vconsole.conf.使用vim生成对应的文件。

KEYMAP：            可用的键盘映射位于/usr/share/kbd/keymaps. 注意此设置仅对 TTY 起作用，不改变图形窗口或X的设置。

FONT：                 可用字体位于 /usr/share/kbd/consolefonts/，一般可用留空。

FONT_MAP：        可选设置，定义要加载的映射，请参考 man setfont。可以删除或者留空。

所以编辑的内容如下：

KEYMAP=us

FONT=

![image](./pic/20.PNG)

#####<7>配置系统时区

```shell
vim /etc/timezone
```


输入如下内容：

Asia/Shanghai

![image](./pic/21.PNG)


然后执行如下命令完成时区设置

```shell
ln -s /usr/share/zoneinfo/Asia/Shanghai /etc/localtime
```


![image](./pic/22.PNG)

#####<8>配置系统 硬件时间

```shell
hwclock --systohc --utc
```

![image](./pic/23.PNG)


#####<9>安装NetworkManager并启动服务

安装networkmanager来完成网络服务的设置和启动

```shell
pacman -S networkmanager
```


![image](./pic/24.PNG)
![image](./pic/25.PNG)



```shell
systemctl enable NetworkManager.service
```


![image](./pic/26.PNG)

#####<10>用 passwd 设置一个root密码

```shell
passwd
```


![image](./pic/27.PNG)

#####<11>安装GRUB

这是非常重要的一步，使用grub来引导已经安装在硬盘上的系统内核，从而让系统启动。

```shell
pacman -S grub
```

![image](./pic/28.PNG)


```shell
grub-install --target=i386-pc --recheck /dev/sda
```


![image](./pic/29.PNG)

然后配置grub，这是非常关键的一步，如果没有配置，那么grub就无法正确的找到启动的内核文件了。

```shell
grub-mkconfig -o /boot/grub/grub.cfg
```


![image](./pic/30.PNG)

grub会自动识别出引导的内容，并且给出提示。

#####<12>卸载mnt，重启

如果还在 chroot 环境，先用 exit 命令退出系统：

```shell
exit
```

![image](./pic/31.PNG)


卸载/mnt中挂载的系统：

 ```shell
umount -R /mnt
```


![image](./pic/32.PNG)

重启：

```shell
reboot
```

![image](./pic/33.PNG)


至此，一个Arch的最小安装已经完成。
![image](./pic/34.PNG)


自动进入系统，看见了熟悉的tty1界面，使用之前的密码登入就可以了。
![image](./pic/35.PNG)



####（8）安装桌面环境支持

只是用命令行的linux还是有些不方便，所以安装一个你自己喜欢的桌面环境来正式开始使用吧。

#####<1>安装xorg-server

linux中安装图形界面，还需要安装Xorg等服务的支持，使用如下命令安装基本的图形界面支持：

```shell
pacman -S xorg-server xorg-server-utils xorg-xinit
```

![image](./pic/36.PNG)


会提示你需要安装的libgl版本，因为我是在虚拟机中安装没有独立显卡，所以选择版本1，就是mesa提供的gl库，如果你是在实体机上安装并且使用英伟达的显卡就可以安装对应的gl驱动，如果不确定显卡是否支持就继续选择mesa提供的库，等安装完毕之后再做出调整。

![image](./pic/37.PNG)

安装完毕之后基本的图形显示支持就完成了。

安装 mesa 以获得 3D 支持:

```shell
pacman -S mesa
```


![image](./pic/38.PNG)

一般情况下在安装xorg支持的时候，如果默认选择mesa的gl驱动，会自动安装mesa，所以上述步骤中会显示重新安装了mesa。

#####<2>安装显卡驱动

如果你不知道自己是什么显卡，就用下面的命令查看下：

```shell
lspci | grep VGA
```

![image](./pic/39.PNG)


根据上述检查PCI接口的VGA设备名称来执行下面的命令搜索下匹配你显卡的驱动

```shell
pacman -Ss xf86-video | less
```

![image](./pic/40.PNG)


因为我使用了VM虚拟机来安装，所以使用：

```shell
pacman -Ss xf86-video|grep vmware
```


找到对应的显卡驱动为：

```shell
pacman -S xf86-video-vmware 
```


![image](./pic/41.PNG)

你们安装匹配的，比如你是Intel集成的就执行：

```shell
pacman -S xf86-video-intel
```


虚拟机还可以安装一个万能的

```shell
pacman -S xf86-video-vesa
```


笔记本还可以装下触摸板驱动

```shell
pacman -S xf86-input-synaptics
```


#####<3>安装vmmouse虚拟机下鼠标驱动

$  pacman -S xf86-input-vmmouse

![image](./pic/42.PNG)

#####<4>测试X环境是否安装好了，可以执行下面的命令，其实不用测试。

```shell
pacman -S xorg-twm xorg-xclock xterm
```


![image](./pic/43.PNG)

```shell
startx
```


![image](./pic/44.PNG)

```shell
exit 
```


![image](./pic/45.PNG)

```shell
pkill X
```

![image](./pic/46.PNG)



####（9）安装自己喜欢的桌面环境

经过上述基本xorg支持，现在就可以根据自己的偏好或者硬件资源状况来选择GUI桌面支持了，常见的有KDE,Gnome,Xfce等，内部嵌入的组件不同，需要的资源也是不同的。

Xorg只提供图形环境的基本框架，完整的用户体验还需要其他组件。桌面环境(DE): 在X之上并与其共同运作，提供完整的功能和动态图形界面。

所以我们需要针对自己的偏好来设置需要的桌面环境。面环境通常提供图标、小程序（applets）、窗口、工具栏、文件夹、壁纸、应用程序和拖放等功能。使用GNOME、KDE、LXDE、Xfce这类桌面环境，是最简单的配置方法. Category:Desktop environments 包含了各种桌面环境。

#####<1>安装登陆管理器（Display manager）

显示管理器也叫登录管理器，它会提供给用户一个登录界面并提示输入用户名和密码。当用户成功输入正确的用户名和密码时，显示管理器会开始一个会话（窗口管理器或者桌面环境），如 GNOME Shell、Deepin和KDE 等。一个图形服务器只能由一个登录管理器来管理，但是系统中可以安装多个显示管理器。

常见的有：

XDM: X 显示管理器 (xorg-xdm)

GDM: GNOME 显示管理器 (gdm)

KDM: KDE 显示管理器 (kdebase-workspace)

SLiM: 简单登录管理器 (slim)

LXDM: LXDE 显示管理器 (独立于桌面环境) (lxdm)

Qingy: getty 使用 DirectFB 的替代者 (qingy)

wdm: WINGs 显示管理器 (wdm)

CDM: 控制台显示管理器 (available in the AUR: cdm-git)

LightDM: Ubuntu 开发的 GDM 替代品，使用 WebKit (位于AUR: lightdm, lightdm-bzr)



因为我在虚拟机中使用，追求的最小化，所以这儿选择安装SLiM：

```shell
pacman -S slim
```


![image](./pic/47.PNG)

会提示你选择字体，英文字体优先选择dejavu字体，所以这儿选择dejavu字体：

![image](./pic/48.PNG)

如果需要其他字体，可以再安装完毕之后重新选择字体安装，也可以在安装slim之前先安装字体。

#####<2>安装桌面环境

安装完毕启动管理器之后，就需要安装桌面环境了，我为了简单实用同时降低虚拟机对主机的负荷，选择轻量级的xfce环境（反正也不需要炫）：

```shell
pacman -S xfce4
```


![image](./pic/49.PNG)

这儿选择默认，就是安装所有的组件，防止后续依赖出现问题：

![image](./pic/50.PNG)

可以看到整个环境还是比较大的。

![image](./pic/51.PNG)

安装完毕之后启动xfce4桌面环境：

```shell
startxfce4
```


![image](./pic/52.PNG)

选择默认设置，开始界面初始化：
![image](./pic/53.PNG)


#####<3>安装用户权限管理

GNU/Linux 通过用户和用户组实现访问控制 —— 包括对文件访问、设备使用的控制。Linux 默认的访问控制机制相对简单直接，不过还有一些更加高级的机制，包括 ACL 和 LDAP Authentication.

用户一般指使用计算机的人。计算机给每个账户分配了特定的名称，而用户则使用这些名称访问计算机。除了人之外，一些系统服务也以有部分限制，又享有部分特权的用户账户身份运行。

由于安全需要，「用户管理」应运而生，以加以明确限制各个用户账户的权限。超级用户 root 于计算机里拥有至高无上的管理权限，所以一般只作管理用。非特权用户则可以用 su 或 sudo 程序以临时获得特权。

总不能用root工作，所以我们要添加一个普通账户。

首先安装权限控制，打开命令行，输入如下命令：

```shell
pacman -S sudo
```


![image](./pic/54.PNG)

然后添加一个用户：

```shell
useradd -m yourname 
```


```shell
passwd yourname
```


其中的yourname就是新增账户的用户名，passwd通过用户名来设置密码：

![image](./pic/55.PNG)

然后需要设置新增加的用户到某些已经存在的用户组，以获取该组所拥有的特权。可以把该用户添加到一些组：　audio disk locate network optical power storage video wheel systemd-journal，例如：

```shell
gpasswd -a yourname wheel
```


因为虚拟机基本只是用terminal学习一些其他的语言，其他不干，所以添加如下的用户组（注意：只能一个一个的添加）：

```shell
gpasswd -a yourname power
```


```shell
gpasswd -a yourname storage
```


```shell
gpasswd -a yourname network
```


```shell
gpasswd -a yourname  wheel
```


```shell
gpasswd -a yourname systemd-journal
```


![image](./pic/56.PNG)

当然如果有其他的需求也是可以继续添加的。



注意：上述两步骤，可以合并为一个：

```shell
useradd -m -g users -G audio,lp,optical,storage,video,wheel,games,network,power -s /bin/bash yourname
```


```shell
passwd yourname
```




将新增的用户 yourname 加入了wheel组，当用户使用sudo命令时，要使系统给予wheel组中所有用户完全的root权限，只需要以root身份执行visudo命令，编辑sudo的配置文件：

```shell
visudo
```


将下面一行取消注释：

```shell
%wheel    ALL=(ALL) ALL
```


这样，当用户使用sudo命令时，就可以以root身份执行命令了。



#####<4>设置xfce4自启动

安装搞vim，方便编辑文件（也可以继续使用nano编辑器）

```shell
pacman -S vim
```


注销或重启，然后用普通用户登录，来设置普通用户的登入桌面环境自启动。

注意：root用户就需要在root登入的时候按照如下设置进入，才能通过启动管理器来进入root权限的桌面环境。
![image](./pic/57.PNG)


xfce4通过用户目录下的xinitrc文件来自动启动

```shell
nano .xinitrc
```


添加如下命令：

```shell
exec startxfce4
```

![image](./pic/58.PNG)


保存退出，一定要改变xinitrc的权限，否则slim无法加载桌面环境：

```shell
chmod +x ~/.xinitrc
```


![image](./pic/59.PNG)

然后执行命令：

```shell
systemctl enable slim.service
```


![image](./pic/60.PNG)

然后重启，用普通用户登入：

```shell
reboot
```

![image](./pic/61.PNG)


就自动启动xfce4了。
![image](./pic/62.PNG)


到此为止，基本的桌面环境设置完毕。其他相关的设置就可以直接在桌面环境中设置使用了。

####（10）安装基本编译开发环境

使用linux的目的就是为了方便自己的开发，所以设置基本的编译开发环境是非常重要的步骤，下面进行基本的设置：

```shell
pacman -S base-devel
```


![image](./pic/63.PNG)

安装调试工具gdb

```shell
pacman -S gdb
```


![image](./pic/64.PNG)

安装完毕之后就可以做基本开发了。



done。



参考网页：

http://www.oschina.net/question/1421678_138088

http://www.bubuko.com/infodetail-31234.html

http://www.cnblogs.com/meetrice/archive/2014/04/22/3681406.html

http://www.cnblogs.com/mad/p/3280041.html

http://tieba.baidu.com/p/3279256455

http://my.oschina.net/codeaxe/blog/127533

https://wiki.archlinux.org/index.php/Installation_guide_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87)

http://ichon.me/post/373.html

http://blog.chinaunix.net/uid-641286-id-3453746.html

http://blog.chinaunix.net/uid-641286-id-3465006.html

http://www.bestzhou.org/2012/09/20/archlinux-2012-09-07-and-xfce4/

https://bbs.archlinux.org/viewtopic.php?id=156792


正则处理：
#\s+(.*)
```shell\r\n$1\r\n```\r\n

\（([0-9]+.*)
###\（$1




