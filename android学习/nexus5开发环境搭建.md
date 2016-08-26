# nexus5开发环境搭建：

开发环境：centos7_X64 gnome桌面版
本文集中了以nexus5为例的AOSP项目完整编译开发过程，用于整体学习。


## 1 基本系统环境设置：
### 1.1 设置ss代理服务器：
需要一个可以直接访问google的服务器，然后上面安装ss服务端为我们提供代理服务，这一步如果已经购买了搭建好的ss或者vpn可以忽略：

#### 1.1.1 首先安装ss：

1. 安装 python setup tools：
```shell
yum install python-setuptools
```
2. 安装pip
```shell
easy_install pip
```
3. 升级 pip
```shell
pip install --upgrade pip
```
4. 安装 shadowsocks
```shell
pip install shadowsocks
```
5. 然后创建 shadowsocks 服务, 随系统启动：
```shell
nano /usr/lib/systemd/system/shadowsocks.service
```
6. 写入如下内容：
```shell
[Unit]
Description=Shadowsocks Server
Documentation=https://github.com/shadowsocks/shadowsocks
After=network.target remote-fs.target nss-lookup.target

[Service]
Type=forking
# 设置启动时的配置文件,根据自己的需求改。这儿使用：/usr/shadowsocks.json作为要读入的配置文件
ExecStart=/usr/bin/ssserver -c /usr/shadowsocks.json -d start
ExecReload=/bin/kill -HUP $MAINPID
ExecStop=/usr/bin/ssserver -d stop

[Install]
WantedBy=multi-user.target
```
其中ssserver表示启动的是ss的服务端，目的是为其他计算机提供代理服务。

7. 添加配置文件:
改为上一步中的路径, 确保各级目录存在：
```shell
nano /usr/shadowsocks.json
```
写入配置内容（包含大括号，是一个json串）:
```shell
  "server": "0.0.0.0",
  "server_port": "8388",
  "password": "uzon57jd0v869t7w",
  "method": "aes-256-cfb"
```
需要注意的是这儿的server内容为0，表示为本机IP。保存后退出，然后启动服务：
```shell
systemctl enable shadowsocks
systemctl start shadowsocks
```
为了检查 shadowsocks 服务是否已成功启动，可以执行以下命令查看服务的状态：
```shell
systemctl status shadowsocks -l
```
如果回显：
```shell
目前自己没有VPS，所以这儿只能在本机测试
```
表示已经启动成功了。


### 1.2 使用ss代理：
PS：注意这儿是sslocal，而不是ssserver，表示服务器。


### 1.3 SOCKS5协议转换：
1. 安装privoxy做转换：
```shell
yum -y install privoxy
```
编辑Privoxy配置文件，将SOCKS5协议转化为HTTP协议：
```shell
nano /etc/privoxy/config
```
添加以下内容（这最后面确实有个英文句号，不要遗漏）：
```shell
forward-socks5 / 127.0.0.1:1080 .
```
设置Privoxy随系统自动启动：
```shell
systemctl enable privoxy
```
启动Privoxy：
```shell
systemctl start privoxy
```
查看Privoxy状态：
```shell
systemctl status privoxy
```
如果有 running 和 active 字样，说明成功运行。

2. 安装polipo做转换：polipo是一个轻量级的缓存web代理程序。
```shell
yum install polipo
```
打开配置文件：
```shell
nano /etc/polipo/config
```
设置ParentProxy为Shadowsocks，通常情况下本机shadowsocks的地址如下：
```shell
# Uncomment this if you want to use a parent SOCKS proxy:
socksParentProxy = "localhost:1080"
socksProxyType = socks5
```
设置日志输出文件：
```shell
logFile=/var/log/polipo
logLevel=4
```

3. 如果上述两个程序都无法安装，建议使用第三方源：
```shell
yum install epel-release
```
根据这个[参考](http://www.cnblogs.com/edward2013/p/5021308.html)，这样就可以直接安装上面两个软件了。

4. 如果还不放心，可以下载软件的源代码包自己编译安装：
[privoxy官方网站](http://www.privoxy.org/sf-download-mirror/Sources/)找到最新的稳定版，然后编译后安装。
```shell
wget http://www.privoxy.org/sf-download-mirror/Sources/3.0.24%20%28stable%29/privoxy-3.0.24-stable-src.tar.gz
```
编译需要的工具安装：
```shell
yum install w3m lynx links
```
然后解压缩、编译安装：
```shell
tar xf privoxy-3.0.24-stable-src.tar.gz
autoheader && autoconf
./configure
make && make install
```
然后根据安装位置修改配置文件完成配置。


### 1.4 使用VPN代理：
如果需要整个系统访问外网，可以使用VPN来做整个系统的代理：
安装软件包：
```shell
yum install pptp pptp-setup
```
初始化一个VPN的连接通道：myvpn
```shell
pptpsetup --create ss --server xxx.xxx.xxx.xxx --username xxx --password xxx --encrypt --start
```
这个命令会在/etc/ppp/peers目录下面，会生成一个叫ss的文件，并且在/etc/ppp目录下面，用户名和密码会写在chap-secrets文件中：
回显：
```shell
Using interface ppp0
Connect: ppp0 <--> /dev/pts/1
CHAP authentication succeeded
MPPE 128-bit stateless compression enabled
local  IP address 192.168.0.136
```
开启/关闭VPN：添加 pon、poff 到/usr/sbin下：
```shell
cp /usr/share/doc/ppp-2.4.5/scripts/pon /usr/sbin
cp /usr/share/doc/ppp-2.4.5/scripts/poff /usr/sbin
chmod +x /usr/sbin/pon /usr/sbin/poff
```
这样就可以通过pon/poff <tunnel> 来启停VPN连接，开启VPN：
```shell
pon ss
```
关闭VPN：
```shell
pon ss
```
删除vpn连接通道：
```shell
pptpsetup --delete ss
```


## 2 下载部署AOSP和kernel源代码：

AOSP作为整个google针对android的开源项目，包含了所有相关的功能模块和代码，通过AOSP可以完整的进行android的应用开发。
但是从源代码树下载下来的最新Android源代码，是不包括内核代码的，也就是Android源代码工程默认不包含Linux Kernel代码，而是使用预先编译好的内核，这个预编译内核一般位于：
```shell
../device/<vendor>/<name>
```
目录下，例如：nexus5所使用的内核就位于：
```shell
../device/lge/hammerhead-kernel
```
目录中的zImage文件。
并且kernel作为整个android的底层，提供了对整体硬件的控制和性能的限制，很多开源硬件运行用户自定义内核来达到某些特殊需求，所以能够编译和定制内核是必须的。
本节主要分为两个部分针对AOSP工程代码和google所维护的linux kernel代码进行编译测试，为后续的开发和学习打下基础。

### 2.1 下载编译AOSP的framework源代码：
AOSP源代码包含了android相关的所有工具和代码，通过编译这部分代码能够满足android开发的大部分内容。
> 不包含第三方适配，关于适配需要参考cyanogenmod.com

#### 2.2.1 下载AOSP的framework代码：
1. 基础环境设置：
```shell
# fetch source
sudo yum install git
sudo yum install wget
# to compile
sudo yum install java-1.7.0-openjdk
sudo yum install java-1.7.0-openjdk-devel
sudo yum install glibc.i686
sudo yum install libstdc++.i686
sudo yum install bison
sudo yum install zip
sudo yum install unzip
```
综合一条命令搞定：
```shell
yum -y install git wget java-1.7.0-openjdk java-1.7.0-openjdk-devel glibc.i686 libstdc++.i686 bison zip unzip
```
这儿选取的openJDK版本应该参考google官方给出的建议进行选取。
> 注意：由于源代码编译需要的资源过多，建议使用8G内存的pc，否则会导致jvm内存不足错误，建议使用实体机或者服务器虚拟机进行编译。

2. 下载repo工具：
国内因为被墙的原因，无法直接访问google服务器，所以需要照国内的代理源来下载android的源代码。
android开源使用repo来完成源代码的管理，这个repo文件是一个python脚本，并且使用git来下载源代码，所以系统中必须安装git和python。现在在root目录下建立source文件夹包含所有内容，包含bin目录和androidSource目录。
```shell
mkdir /root/source/bin
PATH=/root/source/bin:$PATH
curl https://storage.googleapis.com/git-repo-downloads/repo > /root/source/bin/repo
chmod a+x /root/source/binrepo
```
或者使用相同的数字权限：
```shell
chmod 777 /root/source/binrepo
```

3. 修改repo中的下载源为清华大学下载源:
编辑/root/source/bin/repo，将REPO_URL 一行替换成下面的：
```shell
REPO_URL = 'https://gerrit-google.tuna.tsinghua.edu.cn/git-repo'
```

4. 选择特定的Android版本，如果是Nexus系列，你可以从关于手机中的版本号（build number）中从列表中找到对应的版本。也可以访问官方网站找到[对应的版本](https://source.android.com/source/build-numbers.html#source-code-tags-and-builds)，这儿选取nexus 5的源代码包，分支为：android-6.0.1_r60，所以下载代码为：
```shell
mkdir androidSource
cd androidSource
repo init -u https://aosp.tuna.tsinghua.edu.cn/platform/manifest -b android-6.0.1_r60
```

5. 同步下载源码树：
```shell
repo sync -j4
```
>其中，-j4表示并发数为4，清华镜像只支持最大并发数4。

下载完源代码大约需要35G的空间，清华大学的源速度还不错，自测下载速度可以达到3M/s,大约3小时下载完。如果为了节约空间的话，那么可以删除下载好的源代码目录下的.repo文件夹，但是这样就失去了同步的能力。

6. 下载官方提供的驱动包：
根据[官方网站](https://developers.google.com/android/nexus/drivers) 给出的android设备对应android版本来下载对应的AOSP驱动包。
这儿选择：Nexus 5 (GSM/LTE) binaries for Android 6.0.1 (MOB30Y)，然后添加进入源代码中。进入android源代码目录，下载驱动代码包：
```shell
cd source
# Broadcom for NFC, Bluetooth, Wi-Fi
wget https://dl.google.com/dl/android/aosp/broadcom-hammerhead-mob30y-d12b1bea.tgz
# LG for Camera, Sensors, Audio
wget https://dl.google.com/dl/android/aosp/lge-hammerhead-mob30y-546d280a.tgz
# Qualcomm for Graphics, GSM, Camera, GPS, Sensors, Media, DSP, USB
wget https://dl.google.com/dl/android/aosp/qcom-hammerhead-mob30y-d90ee87e.tgz
```
然后解压进入驱动目录：
```shell
ls *.tgz |xargs -n1 tar -zxvf
for i in extract*; do sed -n '/tail/p' $i | sed "s/\$0/$i/" | sh; done
```
>增加驱动代码包非常重要，如果不加入这个，可以用来进行模拟器启动，但是刷入实体机的时候因为驱动的缺失导致无法开机。

这三个压缩包都只包含了一个.sh脚本，但是在这个脚本中则是二进制的驱动，使用这个脚本来动态生成驱动。这个也就是linux主线为什么要去除android内核的缘故吧。封闭和开放。


#### 2.2.2 准备AOSP编译环境：
1. 使用AOSP源代码目录下的../build/envsetup.sh脚本初始化环境：
注意后面的lunch命令等都跟这一步有没有执行有关。如果没有执行后面会提示找不到lunch命令。
```shell
source build/envsetup.sh
```

2. 选择编译目标：
用lunch命令选择编译目标.额外的配置可以用参数传递。例如：
```shell
lunch aosp_arm-eng
```
其中参数aosp_arm-eng，指的是一个适用于模拟器的完整编译版本,带编译的版本。如果直接使用lunch命令不带参数，系统会弹出选项卡，让你选择对应的版本。
所有的编译目标的格式都是:BUILD-BUILDTYPE，BUILD指的是指定特性的结合。其中BUILDTYPE是下面列出的其中一个：
```shell
	Buildtype		用途
	user			有限的权限；适合一般用户
	userdebug		类似user模式，但有root权限和debug能力，适合debug
	eng				带有额外的debug工具的开发配置。
```
这儿因为选取的是nexus 5，所以代号为hammerhead，对应的编译选项为：
```shell
lunch aosp_hammerhead-userdebug
```
或者输入lunch，然后选择19，出现的结果为：
```shell
============================================
PLATFORM_VERSION_CODENAME=REL
PLATFORM_VERSION=6.0.1
TARGET_PRODUCT=aosp_hammerhead
TARGET_BUILD_VARIANT=userdebug
TARGET_BUILD_TYPE=release
TARGET_BUILD_APPS=
TARGET_ARCH=arm
TARGET_ARCH_VARIANT=armv7-a-neon
TARGET_CPU_VARIANT=krait
TARGET_2ND_ARCH=
TARGET_2ND_ARCH_VARIANT=
TARGET_2ND_CPU_VARIANT=
HOST_ARCH=x86_64
HOST_OS=linux
HOST_OS_EXTRA=Linux-3.10.0-327.28.2.el7.x86_64-x86_64-with-centos-7.2.1511-Core
HOST_BUILD_TYPE=release
BUILD_ID=MOB30Y
OUT_DIR=out
=============================================
```

3. 开始编译：
```shell
make -j4 >> compile.log
```
很简单，就直接make好了，然后将日志写入到文件中，方便查看记录和排查错误。
等待编译完成后，会产生如下重要文件：
```shell
android-info.txt
boot.img
cache.img
ramdisk.img
recovery.img
system.img
userdata.img
```
这些文件跟后面烧录的过程有关，非常重要。一般他们在源代码路径下的out文件夹中。具体来说就是在：源代码根目录/out/debug/target/product/hammerhead/目录下。
也可以用源代码根目录下输入：
```shell
find . -name system.img
```
查找具体的路径在哪里。

4. 刷机测试：
编译完毕之后，列出的img文件就是我们需要的最终结果了，现在需要将这些生成的镜像烧录到手机上实测是否可以正确运行。
由于本人是在服务器上进行编译的，然后再本机进行刷机测试，故采用打包的方式进行刷机。将上述文件打包为zip格式，然后编辑脚本：flash-all.bat
```shell
@ECHO OFF
:: 其中-w 选项清空设备上的/data分区，在第一次烧录的时候很有必要，但其他时候就不是必须的。
fastboot -w update images.zip
fastboot reboot
echo Press any key to exit...
pause >nul
exit
```
将手机切换到fastboot模式，使用这个脚本更新系统：
```shell
flash-all.bat
```
稍等片刻，刷入完毕之后就自动重启进入系统。
如果一切顺利，你已经用上了自己根据官方开源编译出来的完整android系统。恭喜！


### 2.3 下载编译AOSP的kernel源代码：
因为2.2节中下载的AOSP代码不包含内核代码，所以需要独立从google下载他修改和维护的内核代码进行编译。本节主要进行官方维护内核的编译测试。

#### 2.3.1 内核编译工具的安装设置：
因为android内核也是属于linux内核的一种，只需要编译内核而不需要依赖别的组件，所以对toolchain(NDK)的选择上宽松得多。大体来讲有这样几种可能性：自己编译arm-eabi的gcc toolchain，使用Sourcery的toolchain，使用Google提供的NDK构建toolchain，或者使用第三方（比如crystax）修改过的NDK。

##### 2.3.1.1 系统初始环境设置：
首先系统必须安装有基本的编译环境：
```shell
yum -y install gcc make git
```
##### 2.3.1.2 工具链安装：
在x86上编译arm架构的内核需要安装交叉编译环境，所以首先需要安装交叉编译环境。
1. 使用EPEL源安装交叉编译器：
```shell
rpm -ivh http://dl.fedoraproject.org/pub/epel/7/x86_64/e/epel-release-7-7.noarch.rpm
rpm --import /etc/pki/rpm-gpg/RPM-GPG-KEY-EPEL-7
```
检查是否安装成功：
```shell
yum repolist
```
查看某个包的详细信息：
```shell
yum --enablerepo=epel info htop
```
列出epel源的所有包列表：
```shell
yum --disablerepo="*" --enablerepo="epel" list available | less
```
然后安装交叉编译器：
```shell
yum -y install gcc-arm-linux-gnu
```
检查是否安装成功：
```shell
arm-linux-gnu-gcc --version
```
回显：
```shell
arm-linux-gnu-gcc (GCC) 4.8.1 20130717 (Red Hat 4.8.1-5)
Copyright (C) 2013 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
```
表示已经安装成功。并且这样安装的交叉编译器已经添加到环境变量中，不再需要用户设置。

2. 从googleNDK获取开发工具：
编译android相关的内容，最好使用google提供的工具链，包括编译内核，可以减少不必要的麻烦。可以根据[官方下载地址](https://developer.android.com/ndk/downloads/index.html)进行工具下载。
例如下载：android-ndk-r12b-linux-x86_64.zip
下载之后根据当前的系统，在压缩包的toolchains目录下选择对应的包，解压到某一个目录中，然后将这个目录添加到系统环境变量中。
```shell
$export PATH=$PATH:/myandroid/androidsrc/prebuilt/linux-x86/toolchain/arm-eabi-4.2.1/bin
```

3. 从google官方工具库获取：
从[官方维护网站](https://android.googlesource.com/platform/prebuilts/gcc/)，下载对应版本的工具链，然后安装：
```shell
git clone https://android.googlesource.com/platform/prebuilts/gcc/linux-x86/arm/arm-linux-androideabi-4.9
```
安装方法同上。

4. 第三方修改的工具链：
和上面类似，网络上有很多人根据最新的GCC代码修改出来的预编译好的交叉编译器，下载之后需要手动安装并且添加到环境变量中去。
关键就是选取的第三方工具链的来源。

> 最后选取<3>方法的4.7版本交叉编译器作为编译内核的工具。

#### 2.3.2 下载AOSP的开源kernel代码：
在AOSP的官方内核代码页面找到代码同步目录：
http://source.android.com/source/building-kernels.html#figuring-out-which-kernel-to-build
选择：
```shell
Device		Binary location					Source location		Build configuration
hammerhead	device/lge/hammerhead-kernel	kernel/msm			hammerhead_defconfig
```
确定git路径：
```shell
git clone https://android.googlesource.com/kernel/msm
```
挂载代理下载完整的代码；如果已经有了，可以更新到最新：
```shell
git pull
```
> 或者使用国内的内核git服务器：
```shell
# 中科大源
git clone git://mirrors.ustc.edu.cn/aosp/kernel/msm.git
# 清华源
git clone https://aosp.tuna.tsinghua.edu.cn/kernel/msm.git
```

然后查看当前代码树的分支：
```shell
git branch -r
```
显示结果为：
```shell
origin/HEAD -> origin/master
  origin/android-msm-2.6.35
  origin/android-msm-3.9-usb-and-mmc-hacks
  origin/android-msm-angler-3.10-marshmallow-dr
  origin/android-msm-angler-3.10-marshmallow-dr1.5
  origin/android-msm-angler-3.10-marshmallow-dr1.6
  origin/android-msm-angler-3.10-marshmallow-dr1.6-1
  origin/android-msm-angler-3.10-marshmallow-mr1
  origin/android-msm-angler-3.10-n-preview-1
  origin/android-msm-angler-3.10-n-preview-2
  origin/android-msm-angler-3.10-n-preview-3
  origin/android-msm-angler-3.10-n-preview-4
  origin/android-msm-angler-3.10-n-preview-5
  origin/android-msm-anthias-3.10-lollipop-mr1-wear-release
  origin/android-msm-anthias-3.10-lollipop-wear-release
  origin/android-msm-anthias-3.10-marshmallow-mr1-wear-release
  origin/android-msm-asus-3.10-lollipop-mr1-wear-release
  origin/android-msm-asus-3.10-marshmallow-mr1-wear-release
  origin/android-msm-bass-3.10-lollipop-mr1-wear-release
  origin/android-msm-bass-3.10-marshmallow-mr1-wear-release
  origin/android-msm-bullhead-3.10-marshmallow-dr
  origin/android-msm-bullhead-3.10-marshmallow-dr-0
  origin/android-msm-bullhead-3.10-marshmallow-dr1.5
  origin/android-msm-bullhead-3.10-marshmallow-dr1.6
  origin/android-msm-bullhead-3.10-marshmallow-mr1
  origin/android-msm-bullhead-3.10-marshmallow-mr1-eas
  origin/android-msm-bullhead-3.10-marshmallow-mr2
  origin/android-msm-bullhead-3.10-n-preview-1
  origin/android-msm-bullhead-3.10-n-preview-2
  origin/android-msm-bullhead-3.10-n-preview-3
  origin/android-msm-bullhead-3.10-n-preview-4
  origin/android-msm-bullhead-3.10-n-preview-5
  origin/android-msm-dory-3.10-kitkat-wear
  origin/android-msm-dory-3.10-lollipop-mr1-wear-release
  origin/android-msm-dory-3.10-lollipop-wear-release
  origin/android-msm-dory-3.10-marshmallow-mr1-wear-release
  origin/android-msm-flo-3.4-jb-mr2
  origin/android-msm-flo-3.4-kitkat-mr0
  origin/android-msm-flo-3.4-kitkat-mr1
  origin/android-msm-flo-3.4-kitkat-mr2
  origin/android-msm-flo-3.4-l-preview
  origin/android-msm-flo-3.4-lollipop-mr1
  origin/android-msm-flo-3.4-lollipop-mr1.1
  origin/android-msm-flo-3.4-lollipop-mr1.2
  origin/android-msm-flo-3.4-lollipop-release
  origin/android-msm-flo-3.4-marshmallow
  origin/android-msm-flo-3.4-marshmallow-mr1
  origin/android-msm-flo-3.4-marshmallow-mr2
  origin/android-msm-hammerhead-3.4-kitkat-mr1
  origin/android-msm-hammerhead-3.4-kitkat-mr2
  origin/android-msm-hammerhead-3.4-kk-fr1
  origin/android-msm-hammerhead-3.4-kk-fr2
  origin/android-msm-hammerhead-3.4-kk-r1
  origin/android-msm-hammerhead-3.4-l-preview
  origin/android-msm-hammerhead-3.4-lollipop-mr1
  origin/android-msm-hammerhead-3.4-lollipop-mr1.1
  origin/android-msm-hammerhead-3.4-lollipop-release
  origin/android-msm-hammerhead-3.4-m-preview
  origin/android-msm-hammerhead-3.4-marshmallow
  origin/android-msm-hammerhead-3.4-marshmallow-mr1
  origin/android-msm-hammerhead-3.4-marshmallow-mr2
  origin/android-msm-huawei-3.10-lollipop-mr1-wear-release
  origin/android-msm-huawei-3.10-lollipop-mr1-wear-release-1
  origin/android-msm-huawei-3.10-marshmallow-mr1-wear-release
  origin/android-msm-huawei-3.10-marshmallow-mr1-wear-release-1
  origin/android-msm-huawei-3.10-marshmallow-mr1-wear-release-2
  origin/android-msm-lego-3.10-marshmallow-dr
  origin/android-msm-lenok-3.10-kitkat-wear
  origin/android-msm-lenok-3.10-lollipop-mr1-wear-release
  origin/android-msm-lenok-3.10-lollipop-wear-release
  origin/android-msm-lenok-3.10-marshmallow-mr1-wear-release
  origin/android-msm-mako-3.4-jb-mr1
  origin/android-msm-mako-3.4-jb-mr1-fr
  origin/android-msm-mako-3.4-jb-mr1-kgsl
  origin/android-msm-mako-3.4-jb-mr1.1
  origin/android-msm-mako-3.4-jb-mr2
  origin/android-msm-mako-3.4-kitkat-mr0
  origin/android-msm-mako-3.4-kitkat-mr1
  origin/android-msm-mako-3.4-kitkat-mr2
  origin/android-msm-mako-3.4-lollipop-mr1
  origin/android-msm-mako-3.4-lollipop-mr1.1
  origin/android-msm-mako-3.4-lollipop-release
  origin/android-msm-moto-3.10-lollipop-mr1-wear-release
  origin/android-msm-nemo-3.10-marshmallow-mr1-wear-release
  origin/android-msm-nemo-3.10-n-preview-1-wear-release
  origin/android-msm-nemo-3.10-n-preview-2-wear-release
  origin/android-msm-seed-3.10-lollipop-mr1
  origin/android-msm-seed-3.10-marshmallow
  origin/android-msm-seed-3.10-marshmallow-mr1
  origin/android-msm-seed-3.10-marshmallow-mr2
  origin/android-msm-seed-3.10-n-preview-2
  origin/android-msm-seed-3.10-n-preview-3
  origin/android-msm-seed-3.10-n-preview-4
  origin/android-msm-seed-3.10-n-preview-5
  origin/android-msm-shamu-3.10-lollipop-mr1
  origin/android-msm-shamu-3.10-lollipop-release
  origin/android-msm-shamu-3.10-m-preview
  origin/android-msm-shamu-3.10-marshmallow
  origin/android-msm-shamu-3.10-marshmallow-mr1
  origin/android-msm-shamu-3.10-marshmallow-mr1-r0.15
  origin/android-msm-shamu-3.10-marshmallow-mr2
  origin/android-msm-shamu-3.10-n-preview-1
  origin/android-msm-shamu-3.10-n-preview-2
  origin/android-msm-shamu-3.10-n-preview-3
  origin/android-msm-shamu-3.10-n-preview-4
  origin/android-msm-shamu-3.10-n-preview-5
  origin/android-msm-smelt-3.10-lollipop-mr1-wear-release
  origin/android-msm-smelt-3.10-marshmallow-mr1-wear-release
  origin/android-msm-sony-cm-jb-3.0
  origin/android-msm-sparrow-3.10-marshmallow-mr1-wear-release
  origin/android-msm-sprat-3.10-kitkat-wear
  origin/android-msm-sprat-3.10-lollipop-mr1-wear-release
  origin/android-msm-sprat-3.10-lollipop-wear-release
  origin/android-msm-sprat-3.10-marshmallow-mr1-wear-release
  origin/android-msm-sturgeon-3.10-marshmallow-mr1-wear-release
  origin/android-msm-sturgeon-3.10-n-preview-1-wear-release
  origin/android-msm-sturgeon-3.10-n-preview-2-wear-release
  origin/android-msm-wren-3.10-marshmallow-mr1-wear-release
  origin/master
```
检出需要编译的分支，这儿选取最新的分支：
```shell
git checkout origin/android-msm-hammerhead-3.4-marshmallow-mr2
```
这个时候目录下除了.git文件夹，多出来完整的代码了。

#### 2.3.3 选择交叉编译器：
1. 选择4.7版本的交叉编译器：
目前使用的是来自[这儿]( https://android.googlesource.com/platform/prebuilts/gcc/linux-x86/arm/arm-eabi-4.7/ )的交叉编译器，而不是上面小节中的[这个]( https://android.googlesource.com/platform/prebuilts/gcc/linux-x86/arm/arm-linux-androideabi-4.9 )交叉编译器，因为就算是修改了权限，还是有问题。
不同版本的交叉编译器无法正确编译，这个问题留给以后解决，GCC的优化也会带来内核效率的提升，因为内核也是可执行程序。

2. 设置交叉编译器的权限（这一步非常关键）：
因为是从windows下代理下载官方的构建工具，所以权限是有问题的，所以一定要对交叉编译器目录进行整体的权限设置：
```shell
chmod -R +x arm-eabi-4.7/
```
然后对其中的bin目录设置为755权限：
```shell
chmod -R 755 bin/*
```
之前没有这样设置，会有各种编译的权限问题，并且造成无法写入的错误，所以如果编译的时候遇到写入权限问题，先看看工具是否具有正确的权限。


#### 2.3.4 设置编译环境：
我们可以通过编写一个shell脚本来简化每次编译都需要进行的环境变量设置：
```shell
nano run_this_android.sh
```
写入如下内容：
```shell
# 设置交叉编译器位置
export CC=$(pwd)/arm-eabi-4.7/bin/arm-eabi-
export CROSS_COMPILE=$(pwd)/arm-eabi-4.7/bin/arm-eabi-
# 设置编译内核架构
export ARCH=arm
export SUBARCH=arm
```
然后设置为可执行，然后再当前终端中生效：
```shell
chmod +x run_this_android.sh
source run_this_android.sh
```

#### 2.3.5 编译内核：
按理来说，编译device对应的内核，最好从当前使用的内核中拷贝出配置文件，然后进行修改，这个最为保险。
一般而言当前使用的内核配置可以用adb来拷贝出来：
```shell
adb pull /proc/config.gz
```
如果没有这个配置文件，就使用当前内核代码中的工具，生成一个默认配置（亲儿子系列独有）：
```shell
make hammerhead_defconfig
```
回显：
```shell
  HOSTCC  scripts/basic/fixdep
  HOSTCC  scripts/kconfig/conf.o
  SHIPPED scripts/kconfig/zconf.tab.c
  SHIPPED scripts/kconfig/zconf.lex.c
  SHIPPED scripts/kconfig/zconf.hash.c
  HOSTCC  scripts/kconfig/zconf.tab.o
  HOSTLD  scripts/kconfig/conf
#
# configuration written to .config
#
```
生成了.config文件，然后生成编译需要的配置文件：
```shell
make menuconfig
```
如果有需要修改的就保存后退出。
然后开始编译，并且把编译过程写入compile.log文件中：
```shell
make -j4 >> compile.log
```
当出现：
```shell
OBJCOPY arch/arm/boot/zImage
Kernel: arch/arm/boot/zImage is ready
CAT     arch/arm/boot/zImage-dtb
Kernel: arch/arm/boot/zImage-dtb is ready
```
表示编译内核成功结束。

#### 2.3.6 打包编译的内核文件：
1. 打包工具编译安装：
因为交叉编译之后的内核文件zImage不能直接作为img文件刷入手机，所以需要打包内核文件进行处理。
下载boot.img打包程序：
```shell
git clone https://github.com/pbatard/bootimg-tools.git
cd bootimg-tools/
make
cd cpio/
gcc mkbootfs.c  -o mkbootfs -I../include
cd ../..
mkdir andorid_boot_tools_bin
cd andorid_boot_tools_bin/
cp ../bootimg-tools/mkbootimg/mkbootimg .
cp ../bootimg-tools/mkbootimg/unmkbootimg .
cp ../bootimg-tools/cpio/mkbootfs .
cd ..
```
这样就在bootimg-tools文件夹中生成了构建boot镜像的可执行文件。
（这儿编译cpio/mkbootfs.c文件的时候报错，但是整个流程中并没有使用到mkbootfs来构建boot时文件系统，而是直接使用了已有打包boot.img中解压出来的文件系统）

2. 打包内核为img文件：
接下来就需要根据编译成功的zImage来制作可以写入的内核镜像文件了。
首先从原来的boot.img入手，将这个旧的镜像用unmkbootimg分解：
```shell
mkdir image
cd image
unmkbootimg -i boot.img
```
回显：
```shell
kernel written to 'kernel' (8331496 bytes)
ramdisk written to 'ramdisk.cpio.gz' (498796 bytes)
	To rebuild this boot image, you can use the command:
    mkbootimg --base 0 --pagesize 2048 --kernel_offset 0x00008000 --ramdisk_offset 0x02900000 --second_offset 0x00f00000 --tags_offset 0x02700000 --cmdline 'console=ttyHSL0,115200,n8 androidboot.hardware=hammerhead  user_debug=31 maxcpus=2 msm_watchdog_v2.enable=1' --kernel kernel --ramdisk ramdisk.cpio.gz -o boot_img/boot.img
```
然后原来的boot.img解压出：kernel和ramdisk.cpio.gz 两个文件。
然后用我们自己编译的进行替换：
```shell
cp ../msm/arch/arm/boot/zImage-dtb kernel_new
```
> 注意：这儿使用的是zImage-dtb镜像，表示使用了dtb的驱动，否则之前下载的闭源驱动不会被包含。

然后开始用这个新的内核进行打包：
```shell
mkbootimg --base 0 --pagesize 2048 --kernel_offset 0x00008000 --ramdisk_offset 0x02900000 --second_offset 0x00f00000 --tags_offset 0x02700000 --cmdline 'console=ttyHSL0,115200,n8 androidboot.hardware=hammerhead  user_debug=31 maxcpus=2 msm_watchdog_v2.enable=1' --kernel kernel_new --ramdisk ramdisk.cpio.gz -o boot_new.img
```
这样新的可以直接刷入的boot_new.img就成功制作好了。
通过上述分析，可以知道android所使用的内核格式是Linux标准的zImage，根文件系统采用ramdisk格式。这两者在Android下是直接合并在一起取名为boot.img,会放在一个独立分区当中。一般而言，这个分区会独立存在，称为boot分区。

最后刷入新的内核进行开机测试：
```shell
adb start-server
adb reboot bootloader
fastboot flash boot boot_new.img
fastboot reboot
```
测试一下自己编译的内核有没有正常运行吧。



## 3 搭建framework源代码开发IDE环境：
上述章节中我们已经可以完整的编译整个代码了，但是查找和修改代码还是很不方便。如果有IDE支持就更加方便我们修改和编译整个AOSP。
google为我们提供了Android Studio，可以用它来完成对AOSP源代码的查看和修改：

### 3.1 安装Android Studio：
unzip解压缩到/usr/local/bin目录下，然后进入解压目录的bin目录下，执行：
```shell
./studio.sh
```
就可以启动了。需要注意的是比较耗资源，最好在实体机上进行操作。

1. 下载安装android SDK来支持开发：
如果已经下载了，可以在studio启动的时候手动设置位置来免去下载。

2. 编译安装android SDK来支持开发：
可以通过编译AOSP代码来生成SDK，然后在android studio中导入这个自己编译生成的SDK来进行应用开发。
需要注意的是，下载AOSP源代码的时候需要注明参数来确保可以编译生成SDK：
```shell
repo init -u https://aosp.tuna.tsinghua.edu.cn/platform/manifest -b android-6.0.1_r60 -g all, -notdefault,tools
repo sync -j4
```
其中参数“-g all, -notdefault,tools”，表示下载完整的AOSP源代码。
并且需要重新设定编译目标：
```shell
lunch sdk-eng
```
否则无法正确编译SDK。
最后在AOSP源代码目录下输入命令：
```shell
make sdk
```
生成的结果放在：
../out/host/linux-x86/sdk/目录下面，是一个zip文件包。
然后解压到目标路径，然后在android studio中导入这个自己编译生成的SDK来进行应用开发。
>注意：
这个步骤中需要执行安装：
```shell
yum install zlib.i686
```
否则报错：
```shell
out/host/linux-x86/bin/aapt: error while loading shared libraries: libz.so.1: cannot open shared object file: No such file or directory
```
安装：
```shell
yum -y install ncurses-libs.i686
```
否则报错：
```shell
out/host/linux-x86/bin/llvm-tblgen: error while loading shared libraries: libncurses.so.5: cannot open shared object file: No such file or
 directory
```

### 3.2 从AOSP中生成idegen.jar文件：
1. 生成idegen工具需要使用到的jar包：idegen.jar
需要完整编译一次AOSP，如果你make成功之后，发现在../out/host/linux-x86/framework目录下没有生成这个文件，那么可以自己手动编译：
```shell
mmma development/tools/idegen/
```
这之后就会生成idegen.jar。

2. 执行idegen.sh：
在AOSP源码根目录执行下面的命令：
```shell
development/tools/idegen/idegen.sh
```
耐心等待，过一会就会在根目录下生成两个文件，android.iml和android.ipr，这就是我们需要的。

### 3.3 使用Intellij/Android Studio打开工程文件：
打开Intellij和Android Studio：
```shell
File->open->android.ipr
```
或者Andoird Stuido：
```shell
Open an existing Android Studio project -> android.ipr所在目录
```
之后就是漫长的等待，需要花很久的时间建立索引。

总体上：先进行AOSP代码的命令行编译，执行成功之后再安装android studio是一个不错的选择。可以说整个AOSP包含了所有相关的代码，生成的文件也能满足android开发的所有需求，再结合IDEA这个编辑器可以非常好的进行开发工作。剩下的事情就是结合linux内核知识RTFSC了。
