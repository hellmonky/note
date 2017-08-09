<!-- TOC -->

- [KVM原理和学习](#kvm原理和学习)
    - [基本概念：](#基本概念)
    - [基本环境搭建：](#基本环境搭建)
        - [环境检查：](#环境检查)
            - [硬件环境检测：](#硬件环境检测)
            - [打开BIOS中的虚拟化支持：](#打开bios中的虚拟化支持)
            - [软件环境检查：](#软件环境检查)
                - [检查当前是否有kvm内核模块：](#检查当前是否有kvm内核模块)
                - [编译kvm模块，然后加载到内核：](#编译kvm模块然后加载到内核)
        - [安装kvm运行环境：](#安装kvm运行环境)
            - [ubuntu安装基本运行环境：](#ubuntu安装基本运行环境)
            - [centos7环境下安装运行环境：](#centos7环境下安装运行环境)
            - [安装kvm-qemu：](#安装kvm-qemu)
    - [kvm基本使用流程：](#kvm基本使用流程)
        - [创建镜像，并且安装centos7：](#创建镜像并且安装centos7)
            - [（1）首先创建一个虚拟机镜像，类型为qcow2，文件名为centos.img，大小为50G：](#1首先创建一个虚拟机镜像类型为qcow2文件名为centosimg大小为50g)
            - [（2）然后使用IOS启动虚拟机：](#2然后使用ios启动虚拟机)
            - [（3）安装虚拟机，然后做基本设置，保证基础镜像准确：](#3安装虚拟机然后做基本设置保证基础镜像准确)
            - [（4）从基础镜像做差分镜像，减少空间用量：](#4从基础镜像做差分镜像减少空间用量)
            - [（5）搭建差分镜像的集群：](#5搭建差分镜像的集群)
            - [（6）命令总结：](#6命令总结)
    - [kvm中的网络设置：](#kvm中的网络设置)
        - [NAT方式：](#nat方式)
        - [Bridge方式：](#bridge方式)
    - [tun虚拟化网卡：](#tun虚拟化网卡)
        - [tun原理：](#tun原理)
        - [启用tun内核支持：](#启用tun内核支持)
        - [使用tun创建多个网卡：](#使用tun创建多个网卡)
    - [KVM基本原理学习：](#kvm基本原理学习)
    - [Libvirt学习：](#libvirt学习)
        - [libvirt的python API调用封装：](#libvirt的python-api调用封装)
    - [Virtio学习：](#virtio学习)

<!-- /TOC -->


# KVM原理和学习
因为需要将环境迁移到虚拟化平台，目前使用最广泛的就是kvm了，本文主要学习kvm中的虚拟化迁移遇到的问题。

## 基本概念：
KVM是linux系统提供的高性能虚拟化模块，可以在满足硬件需求的机器上提供虚拟化支持。

KVM是linux系统的一个内核模块，需要硬件虚拟化的支持，并且只支持64位处理器。
KVM模块与用户空间的qemu的通信接口主要是一系列针对特殊设备文件的IOCTL调用。
KVM模块在加载之初，只存在dev/kvm文件，对这个文件最重要的IOCTL调用就“创建虚拟机”。


以KVM在Intel公司的CPU上运行为例，在被内核加载的时候，KVM模块会先初始化内部的数据结构；做好准备之后，KVM模块检测系统当前的CPU，然后打开CPU控制寄存器CR4中的虚拟化模式开关，并通过执行VMXON 指令将宿主操作系统（包括KVM模块本身）置于虚拟化模式中的根模式；最后，KVM模块创建特殊设备文件/dev/kvm并等待来自用户空间的命令。接下来虚拟机的创建和运行将是一个用户空间的应用程序（QEMU）和KVM模块相互配合的过程。
1）/dev/kvm
/dev/kvm是一个字符设备，工作于Hypervisor。在用户空间，可通过这个接口（调用ioctl系统调用）管理虚拟机或通过这个接口编程序用来管理虚拟机，实现VM创建、删除、分配内存、读写VCPU的寄存器、想VCPU注入中断、运行VCPU等功能。如virsh或qemu-kvm这些工具都是都可以用来管理KVM虚拟机。

2）QEMU
QEMU本身并不是KVM的一部分，其自身就是一个著名的开源虚拟机软件。与KVM不同，QEMU虚拟机是一个纯软件的实现，所以性能低下。但是，其优点是在支持QEMU本身编译运行的平台上就可以实现虚拟机的功能，甚至虚拟机可以与宿主机并不是同一个架 构。作为一个存在已久的虚拟机，QEMU的代码中有整套的虚拟机实现，包括处理器虚拟化、内存虚拟化，以及KVM使用到的虚拟设备模拟（比如网卡、显卡、存储控制器和硬盘等）。
为了简化开发和代码重用，KVM在QEMU的基础上进行了修改。虚拟机运行期间，QEMU 会通过KVM模块提供的系统调用进入内核，由KVM模块负责将虚拟机置于处理器的特殊模式运行。遇到虚拟机进行输入输出操作，KVM模块会从上次的系统调用出口处返回 QEMU，由QEMU来负责解析和模拟这些设备。
从QEMU角度来看，也可以说QEMU使用了KVM模块的虚拟化功能，为自己的虚拟机提供硬件虚拟化的加速，从而极大地提高了虚拟机的性能。除此之外，虚拟机的配置和创 建，虚拟机运行依赖的虚拟设备，虚拟机运行时的用户操作环境和交互，以及一些针对虚拟机的特殊技术(诸如动态迁移)，都是由QEMU自己实现的。

每个由KVM创建的GuestOS都是HostOS(或VMM)上的一个单个进程。而在GuestOS上的User-space中运行的Applications可以理解为就是进程中的线程。
KVM只是虚拟化解决方案的一部分，想要实现全虚拟化，还需要其他的条件： 
1. CPU处理器提供的虚拟化支持(VT-x 硬件辅助虚拟化，可以为GuestOS创建虚拟化处理器，本质是对寄存器的隔离模拟和对指令集的划分)。 
2. 内存可以通过kvm虚拟化成独立的虚拟化地址(/dev/kvm) 
3. I/O虚拟化(QEMU)

**KVM虚拟化 = KVM内核模块 + /dev/kvm + QEMU**


在 Linux 全虚拟化解决方案 中, KVM 负责提供 CPU 虚拟化和内存虚拟化, 但是 KVM 对于一些计算机硬件设备还是无法进行完美的虚拟(如: 网卡/磁盘/ IO 设备…). 于是就引入了 QEMU, QEMU 负责提供硬件设备的虚拟化, 以此弥补来 KVM 的缺陷. 同时, 为了提高 QEMU 虚拟出来的虚拟硬件设备性能, 于是产生了 pass through 半虚拟化设备virtio_blk/virtio_net. KVM + QEMU 才能实现真正意义上虚拟化. 而 qemu-kvm 就是将两者整合到了一起的媒介.

qemu-kvm 通过 ioctl 调用 KVM 的 /dev/kvm 接口, 将 KVM 内核模块相关的 CPU 指令传递到内核模块中执行。

Qemu将KVM整合进来，通过ioctl调用/dev/kvm接口，将有关CPU指令的部分交由内核模块来做。kvm负责cpu虚拟化+内存虚拟化，实现了cpu和内存的虚拟化，但kvm不能模拟其他设备。qemu模拟IO设备（网卡，磁盘等），kvm加上qemu之后就能实现真正意义上服务器虚拟化。因为用到了上面两个东西，所以称之为qemu-kvm。
Qemu模拟其他的硬件，如Network, Disk，同样会影响这些设备的性能，于是又产生了pass through半虚拟化设备virtio_blk, virtio_net，提高设备性能。


参考文档：
> - [KVM/QEMU/qemu-kvm/libvirt 概念全解](http://blog.csdn.net/jmilk/article/details/68947277)
> - [KVM原理介绍](http://www.ywnds.com/?p=5842)
> - [KVM架构与原理详解](http://www.linuxidc.com/Linux/2015-01/112328.htm)
> - [openstack, kvm, qemu-kvm以及libvirt之间的关系](http://blog.csdn.net/yueyihua/article/details/65631630)
> - [带你走进虚拟化世界之KVM](http://blog.oldboyedu.com/kvm/)


## 基本环境搭建：
为了使用kvm，我们需要一个运行linux系统的物理机，这儿选择了ThinkPad X1 Carbon，安装ubuntu 16.04作为基本环境。

### 环境检查：
#### 硬件环境检测： 
KVM 需要有 CPU 的支持（Intel vmx 或 AMD svm）,在安装 KVM 之前检查一下 CPU 是否提供了虚拟技术的支持,有显示,有显示则说明宿主机处理器具有VT功能。
全虚拟化支持可以通过获取cpu信息来检查：
```shell
cat /proc/cpuinfo | egrep '(vmx|svm)' | wc -l;
egrep '(vmx|svm)' --color=always /proc/cpuinfo
```

如果使用了阿里云ECS节点，这个检查返回为0，因为阿里云中的ECS默认关闭kvm，不支持虚拟化功能。但是查看：
```shell
lscpu
```
显示支持kvm。
然后检查当前环境是否已经默认安装了qemu相关的内容：
```shell
rpm -qa | grep qemu*
```
返回结果为空，所以需要重新搭建环境。

参考：
> - [阿里云ECS上能否再做虚拟化？](https://bbs.aliyun.com/read/273042.html)


#### 打开BIOS中的虚拟化支持：
要使用kvm，必须首先确保当前的处理器有虚拟化支持，并且需要在BIOS中打开。

#### 软件环境检查：
##### 检查当前是否有kvm内核模块：
lsmod | grep kvm
然后检查是否有 /device/kvm 这个块存储设备。
如果都存在就说明当前内核支持kvm模块，并且提供了接口，只需要安装后端模拟器和管理工具就可以了。
如果没有就需要重新编译内核，打开对kvm的支持。

##### 编译kvm模块，然后加载到内核：
kvm作为kernel的一个模块，如果需要在特定的内核版本上完成支持，就需要结合当前内核版本进行kvm的编译安装。
并且内核模块可以方便的动态加载，可以单独编译KVM模块，而不必每次重新编译内核，增加了灵活性。




编译安装kvm-kmod：
（1）首先下载kvm-kmod源码并解压。
（2）进入源码目录。
（3）./configure--kerneldir=/lib/modules/2.6.38.8/source   //注意该路径是自己想要使用的内核路径
（4）make
（5）make install

如果过程中一切正常就是安装完成了。

加载编译完成的内核模块：
此时需要加载kvm.ko以及kvm-intel.ko模块。
（1）首先查看一下当前系统有没有加载这两个模块。lsmod |grep kvm
（2）如果已经加载了这两个模块，卸载掉。rmmod kvm-intel.ko        rmmod kvm.ko
卸载完成后加载自己编译好的模块。
这里注意要选中自己新编译的ko文件，这个可以在前面限定路径
insmod kvm.ko
insmod kvm-intel.ko

每次修改源码可以通过这种方式重新编译加载。
    




### 安装kvm运行环境：
完成上述基本硬件和软件检查后，我们就可以正式搭建kvm的运行环境了。
因为除了在内核空间的KVM模块之外，在用户空间需要QEMU来模拟所需要CPU和设备模型以及用于启动客户机进程，这样才有了一个完整的KVM运行环境。
也可以使用qemu-kvm，这是一个为了针对KVM专门做了修改和优化的QEMU分支。可以从官方网站下载最新的qemu-kvm源码：
git clone git://git.kernel.org/pub/scm/virt/kvm/qemu-kvm.git

#### ubuntu安装基本运行环境：
需要注意的是：安装qemu-kvm和libvirt-bin将自动创建用户组：
https://linux.cn/article-7060-1.html
然后就可以使用界面管理了，否则会出现没有权限访问的错误。

因为需要频繁使用终端，安装了guake之后效率提升非常大，太方便了。


#### centos7环境下安装运行环境：
没有任何东西，所以就需要安装了：
```shell
yum groupinstall Virtualization 'Virtualization Client' 'Virtualization Platform' 'Virtualization Tools'
```
其中：
Virtualization:提供虚拟机的环境,主要包含qumu-kvm
virtualization-client:管理和安装虚拟机实例的客户端，主要有python-virtinst,virt-manager,virt-viewer
virtualization-platform:提供访问和控制虚拟客户端的接口，主要有libvirt，libvirt-client
virtualization-tools:管理离线虚拟机镜像的工具，主要有libguestfs根据需求选择软件包。

安装完毕后，就可以开启libvirtd服务操作：
状态/开启/停止/重启:
```shell
service libvirtd {status|start|stop|restart}
```
加入开机启动
```shell
chkconfig libvirtd on
```
查看libvirtd开机启动runlevel
```shell
chkconfig --list libvirtd
```
验证内核是否已经载入了kvm模块：
```shell
lsmod | grep kvm
```
发现并没有，通过：

参考文档：
> - [CentOS 7部署KVM虚拟化环境(上)架构介绍](http://www.onlyeric.com/2016/05/21/CentOS-7%E9%83%A8%E7%BD%B2KVM%E8%99%9A%E6%8B%9F%E5%8C%96%E7%8E%AF%E5%A2%83-%E4%B8%8A-%E6%9E%B6%E6%9E%84%E7%AF%87/)
> - [CentOS 7部署KVM虚拟化环境(中)安装配置](http://www.onlyeric.com/2016/05/24/CentOS-7%E9%83%A8%E7%BD%B2KVM%E8%99%9A%E6%8B%9F%E5%8C%96%E7%8E%AF%E5%A2%83-%E4%B8%AD-%E5%AE%89%E8%A3%85%E7%AF%87/)
> - [CentOS 7部署KVM虚拟化环境(下)管理篇](http://www.onlyeric.com/2016/06/29/CentOS-7%E9%83%A8%E7%BD%B2KVM%E8%99%9A%E6%8B%9F%E5%8C%96%E7%8E%AF%E5%A2%83-%E4%B8%8B-%E7%AE%A1%E7%90%86%E7%AF%87/)


#### 安装kvm-qemu：
kvm负责cpu虚拟化+内存虚拟化,实现了cpu和内存的虚拟化,但kvm不能模拟其他设备；qemu是模拟IO设备（网卡,磁盘）,kvm加上qemu之后就能实现真正意义上服务器虚拟化。

参考：
[kvm虚拟机之centos6.5安装配置 KVM篇一](http://wangying.sinaapp.com/archives/1693)
[kvm虚拟化学习笔记(一)之kvm虚拟化环境安装](http://koumm.blog.51cto.com/703525/1288795)
[KVM源代码分析1:基本工作原理](http://oenhan.com/kvm-src-1)
[Openstack使用ISO镜像启动云主机](http://int32bit.me/2016/08/01/Opentack%E4%BD%BF%E7%94%A8ISO%E9%95%9C%E5%83%8F%E5%90%AF%E5%8A%A8%E4%BA%91%E4%B8%BB%E6%9C%BA/)
[kvm虚拟机之centos6.5安装配置 KVM篇一](http://wangying.sinaapp.com/archives/1693)
[虚拟化方案之－－kvm简单教程](http://xstarcd.github.io/wiki/Cloud/kvm_tap_bridge_virtio.html)


## kvm基本使用流程：

### 创建镜像，并且安装centos7：
#### （1）首先创建一个虚拟机镜像，类型为qcow2，文件名为centos.img，大小为50G：
```shell
ubuntu@ubuntu:~/kvm_dev/centos$ qemu-img create -f qcow2 centos.img 50G
Formatting 'centos.img', fmt=qcow2 size=53687091200 encryption=off cluster_size=65536 lazy_refcounts=off refcount_bits=16
```

#### （2）然后使用IOS启动虚拟机：
```shell
ubuntu@ubuntu:~/kvm_dev/centos$ qemu-system-x86_64 -cpu kvm64 -m 2048 /home/ubuntu/kvm_dev/centos/centos.img -cdrom /home/ubuntu/Downloads/CentOS-7-x86_64-Minimal-1611.iso 
warning: TCG doesn't support requested feature: CPUID.01H:EDX.vme [bit 1]
```
这儿因为没有明确的指定启动选项为--enable-kvm，导致启动卡住，重新启动：
```shell
ubuntu@ubuntu:~/kvm_dev/centos$ qemu-system-x86_64 --enable-kvm  -cpu kvm64 -m 2048 /home/ubuntu/kvm_dev/centos/centos.img -cdrom /home/ubuntu/Downloads/CentOS-7-x86_64-Minimal-1611.iso
```
也就不存在TCG错误信息了。

#### （3）安装虚拟机，然后做基本设置，保证基础镜像准确：
主要的设置为网络设置。
install centos7 in vm, and after configuration reboot

#### （4）从基础镜像做差分镜像，减少空间用量：
```shell
qemu-img create -b centos.img -f qcow2 vm_1.qcow2
qemu-img create -b centos.img -f qcow2 vm_2.qcow2
qemu-img create -b centos.img -f qcow2 vm_3.qcow2
```
这个命令创建一个vm_1到vm_3的qcow2格式镜像，这个镜像派生自当前的centos.img镜像，也就是差分镜像。
需要注意的是，创建了差分镜像，基础镜像就不能改变了。

参考文档：
> - [kvm+libvirt虚拟机快照浅析（一）——QCOW2 backing files 与 overlays](http://www.virtclouds.com/129.html)

#### （5）搭建差分镜像的集群：
当前默认参数启动的vm中，在没有任何“-net”参数时，qemu-kvm默认使用的是“-net nic -net user”的参数，提供了一种用户模式（user-mode）的网络模拟。
所谓用户模式就是：虚拟机可以使用网络服务，但局域网中其他机器包括宿主机无法连接它。比如，它可以浏览网页，但外部机器不能访问架设在它里面的web服务器。
默认的，虚拟机得到的ip空间为10.0.2.0/24，主机ip为10.0.2.2供虚拟机访问。可以ssh到主机(10.0.2.2)，用scp来拷贝文件。

#### （6）命令总结：

qemu-img create -f qcow2 centos1.img 50G

qemu-system-x86_64 -cpu kvm64 -enable-kvm -m 2048 centos1.img -cdrom ~/Downloads/CentOS-7-x86_64-Minimal-1611.iso

change net-config to Bridge-mode and set static IP address:
```
DEVICE=br0
TYPE=Bridge
BOOTPROTO=static
DEFROUTE=yes
PEERDNS=yes
PEERROUTES=yes
IPV4_FAILURE_FATAL=no
IPV6INIT=yes
IPV6_AUTOCONF=yes
IPV6_DEFROUTE=yes
IPV6_PEERDNS=yes
IPV6_PEERROUTES=yes
IPV6_FAILURE_FATAL=no
IPV6_ADDR_GEN_MODE=stable-privacy
NAME=ens3
UUID=539bf701-51d3-4a87-92b9-43ed77e2ca3c
DEVICE=ens3
ONBOOT=yes
IPADDR=172.168.1.13
NETMASK=255.255.255.0
GATEWAY=172.168.1.1
```

qemu-system-x86_64 --enable-kvm  -cpu kvm64 -m 2048 /home/ubuntu/kvm_dev/centos/centos_1.img

virt-install -n centostst2 --ram 2048 --disk /home/ubuntu/kvm_dev/centos_cluster/centos_2.img --import --network bridge=virbr0

首先启动说上述三个镜像，然后修改网络地址：
需要注意的是，当前的kvm使用的网卡为virbr0。

## kvm中的网络设置：
网络是非常重要的一个内容，需要认证仔细的进行组网才能保证整个集群运行稳定。这方面自己很欠缺，还是要多多学习。
KVM提供了两种组网方式：NAT模式和Bridge模式。

### NAT方式：
我们为了保证每个虚拟机都有固定的IP，修改网络配置文件：
nano /etc/sysconfig/network-scripts/ifcfg-ens3
然后添加内容为：
IPADDR=10.0.2.11
GATEWAY=10.0.2.2
NETMASK=255.255.255.0
DNS1=10.0.2.2
DNS2=8.8.8.8
保存后，重启网络：
systemctl restart networking.service
然后重新查看当前IP内容为：
ifconfig

### Bridge方式：
和其他同事沟通了一下，如果内部vm组网，最好的方式就是通过bridge的方式进行网络规划，这样每个节点都有自己的独立IP。

安装KVM完毕后，使用iconfig检查就会发现多处一个virtbr0。
KVM 会自己创建一个 virbr0 的桥接网络，但是这个是一个 NAT 的网络，没有办法跟局域网内的其他主机进行通信。

首先需要在host上，新建一个桥接：
nano /etc/network/interface
在文件后面添加内容：
```shell
# The bridged network interface
auto br0
iface br0 inet static
    address 1.2.3.4
    netmask 255.255.255.0
    gateway 1.2.3.1
    dns-nameservers 1.2.3.1
    bridge_ports enp9s0
    bridge_stop off
    bridge_fd 0
    bridge_maxwait 0
```
保存后重启网络：
sudo /etc/init.d/networking restart
或者
sudo systemctl restart networking.service
重启完毕后使用ifconfig就可以看到br0这个桥接生效了：
```shell
ubuntu@ubuntu:/etc/network$ ifconfig
br0       Link encap:Ethernet  HWaddr 3e:2a:4f:6b:0c:be  
          inet addr:1.2.3.4  Bcast:1.2.3.255  Mask:255.255.255.0
          inet6 addr: fe80::3c2a:4fff:fe6b:cbe/64 Scope:Link
          UP BROADCAST RUNNING MULTICAST  MTU:1500  Metric:1
          RX packets:0 errors:0 dropped:0 overruns:0 frame:0
          TX packets:60 errors:0 dropped:0 overruns:0 carrier:0
          collisions:0 txqueuelen:1000 
          RX bytes:0 (0.0 B)  TX bytes:6246 (6.2 KB)

enp0s25   Link encap:Ethernet  HWaddr 54:ee:75:1e:6d:dd  
          UP BROADCAST MULTICAST  MTU:1500  Metric:1
          RX packets:0 errors:0 dropped:0 overruns:0 frame:0
          TX packets:0 errors:0 dropped:0 overruns:0 carrier:0
          collisions:0 txqueuelen:1000 
          RX bytes:0 (0.0 B)  TX bytes:0 (0.0 B)
          Interrupt:20 Memory:f0500000-f0520000 

lo        Link encap:Local Loopback  
          inet addr:127.0.0.1  Mask:255.0.0.0
          inet6 addr: ::1/128 Scope:Host
          UP LOOPBACK RUNNING  MTU:65536  Metric:1
          RX packets:478 errors:0 dropped:0 overruns:0 frame:0
          TX packets:478 errors:0 dropped:0 overruns:0 carrier:0
          collisions:0 txqueuelen:1000 
          RX bytes:35248 (35.2 KB)  TX bytes:35248 (35.2 KB)

tap0      Link encap:Ethernet  HWaddr c6:11:76:03:e9:03  
          UP BROADCAST MULTICAST  MTU:1500  Metric:1
          RX packets:0 errors:0 dropped:0 overruns:0 frame:0
          TX packets:0 errors:0 dropped:0 overruns:0 carrier:0
          collisions:0 txqueuelen:1000 
          RX bytes:0 (0.0 B)  TX bytes:0 (0.0 B)

virbr0    Link encap:Ethernet  HWaddr 00:00:00:00:00:00  
          inet addr:192.168.122.1  Bcast:192.168.122.255  Mask:255.255.255.0
          UP BROADCAST MULTICAST  MTU:1500  Metric:1
          RX packets:0 errors:0 dropped:0 overruns:0 frame:0
          TX packets:0 errors:0 dropped:0 overruns:0 carrier:0
          collisions:0 txqueuelen:1000 
          RX bytes:0 (0.0 B)  TX bytes:0 (0.0 B)

wlp3s0    Link encap:Ethernet  HWaddr 28:b2:bd:08:4d:96  
          inet addr:30.27.84.212  Bcast:30.27.87.255  Mask:255.255.252.0
          inet6 addr: fe80::58bf:8c91:6af5:32ce/64 Scope:Link
          UP BROADCAST RUNNING MULTICAST  MTU:1500  Metric:1
          RX packets:8886 errors:0 dropped:0 overruns:0 frame:0
          TX packets:1561 errors:0 dropped:0 overruns:0 carrier:0
          collisions:0 txqueuelen:1000 
          RX bytes:1230738 (1.2 MB)  TX bytes:189619 (189.6 KB)
```
然后验证一下桥接状态：
sudo brctl show
也能够看到这个网桥了：
```shell
ubuntu@ubuntu:/etc/network$ sudo brctl show
bridge name	bridge id		STP enabled	interfaces
br0		8000.000000000000	no		
virbr0		8000.000000000000	yes	
```

参考文档：
> - [KVM/QEMU桥接网络设置及kvm资料](http://blog.csdn.net/cd520yy/article/details/10003343)
> - [在 Ubuntu 的 KVM 中安装 Windows 系统](https://tommy.net.cn/2017/01/06/install-windows-under-ubuntu-and-kvm/)


## tun虚拟化网卡：
如果需要在一台物理机上进行多个vm的组网，我们的一个物理网卡就不够用了，需要使用虚拟网卡来辅助组网。

### tun原理：
Tun/Tap都是虚拟网卡，没有直接映射到物理网卡，是一种纯软件的实现。
Tun是三层虚拟设备，能够处理三层即IP包，Tap是二层设备，能处理链路层网络包如以太网包。使用虚拟网络设备，可以实现隧道，如OpenVPN的实现。

tun/tap驱动程序实现了虚拟网卡的功能，tun表示虚拟的是点对点设备，tap表示虚拟的是以太网设备，这两种设备针对网络包实施不同的封装。利用tun/tap驱动，可以将tcp/ip协议栈处理好的网络分包传给任何一个使用tun/tap驱动的进程，由进程重新处理后再发到物理链路中。

linux中使用uml-utilities工具提供tunctl,用来生成tun接口(三层)；bridge_utils提供brctl工具，用来生成tap接口(二层)。

参考文档：
[Linux下Tun/Tap设备通信原理](http://blog.csdn.net/flyforfreedom2008/article/details/46038853)
[虚拟网卡 TUN/TAP 驱动程序设计原理](https://www.ibm.com/developerworks/cn/linux/l-tuntap/)
[tun／tap工作原理分析](http://itoedr.blog.163.com/blog/static/12028429720143561235469/)


### 启用tun内核支持：
对于虚拟网卡，可以使用：
sudo apt-get install uml-utilities
完成安装，然后检查内核模块：
lsmod | grep tun
是没有的，但是查看设备驱动是已经存在：
ll /dev/net/tun
然后加载一下tun模块：
modprobe tun
lsmod | grep tun
还是没有，找一下tun.ko模块是否存在：
/lib/modules/xxx/kernel/drivers/net/tun.ko
发现虽然安装了uml-utilities，但是实际上并没有提供这个模块，造成这种情况的原因就是内核编译的时候，tun模块没有被打开，需要重新编译内核打开这个模块。

根据tun/tap的原理，uml-utilities这个工具只是提供了tunctl进行管理，对于内核模块是没有办法提供支持的。所以上述步骤中，安装前就应该检查是否存在这个内核模块。


（1）下载对应当前内核版本的源代码：
首先，检查当前使用的内核版本：
uname -a
然后下载当前内核版本对应的linux源代码：
sudo apt-get install linux-source-4.4.0
源码一般会被安装到/usr/src/linux-source-x.x.x/目录下,x.x.x是版本号。
然后在这个目录下就能找到刚刚下载的源代码tar包了。
然后解压缩到工作目录（不建议在这个文件夹下解压？）
tar -xvjf linux-source-4.8.0.tar.bz2 -C /x/x/x

（2）编译：
首先安装依赖：
apt-get install libncurses5-dev libssl-dev
然后配置需要编译的模块:
make menuconfig
找到Device Drivers -->,回车选择:
继续找到Network Device Support -->,回车选择:
找到Universal TUN/TAP device driver support,看到前面是<*>，键盘输入M，变成<M>，退出并保存,回到终端:

> 注：[*],<*>表示编译进内核，<M>表示编译成模块，如果不知道某选项为何时，且有模块可选时，那么就可以直接选择为模块,如果有疑惑,可以去翻鸟哥的linux私房菜基础篇这本书.

开始执行命令:
make -j4 modules

（3）复制加载模块
编译完成后,可以想内核中加载模块了:
cp /xxx/linux-source-4.8.0/drivers/net/tun.ko  /lib/modules/xxx/kernel/net/tun.ko
将编译过后的tun.ko复制到/lib/modules/xxx/kernel/net/目录下去,xxx是一个和你当前内核版本号相关的目录,一般而言,这个目录下也就xxx这么一个目录.
然后执行安装：
modprope tun
检查内核日志：
dmesg | tail
显示：
tun: no symbol version for module_layout
原因是内核的版本不匹配,因为我之前使用的内核版本和我编译的内核版本版本存在差异。

这个时候modinfo tun也是没有任何信息的，但是如果指定tun.ko的具体路径，可以看到：
ubuntu@ubuntu:~$ modinfo /lib/modules/4.4.0-89-generic/kernel/net/tun.ko 
filename:       /lib/modules/4.4.0-89-generic/kernel/net/tun.ko
alias:          devname:net/tun
alias:          char-major-10-200
license:        GPL
author:         (C) 1999-2004 Max Krasnyansky <maxk@qualcomm.com>
description:    Universal TUN/TAP device driver
srcversion:     8E2E080DACF43BA28D7EFB5
depends:        
intree:         Y
vermagic:       4.4.76 SMP mod_unload modversions 

（4）最终解决方法，重新编译内核：
上述方式都不行，还是只能重新编译内核，为了方便区分，我选择了4.11版本的内核来进行编译：
sudo apt-get install linxu-source-4.11.0
然后解压，添加基本依赖：
tar -zjvf /usr/src/linux-source-4.11.0//usr/src/linux-source-4.11.0.tar.bz2 -C /home/ubuntu/kvm_dev/origin/
sudo apt-get install bc
复制旧版本的配置：
cp /boot/config-3.19.0-81-generic /home/ubuntu/kvm_dev/origin/linux-source-4.11.0/.config 
使用配置：
make oldconfig
或者图形界面的配置：
make menuconfig
设置，将tun作为模块进行编译。

然后编译：
make -j4
安装：
make modules_install
make install
完成安装后，检查/boot下面对应的内核镜像是否正确安装，然后重启。
登入后检查，发现tun模块以及存在了：
ubuntu@ubuntu:~/kvm_dev/origin/linux-source-4.11.0$ modinfo tun
filename:       /lib/modules/4.11.12/kernel/drivers/net/tun.ko
alias:          devname:net/tun
alias:          char-major-10-200
license:        GPL
author:         (C) 1999-2004 Max Krasnyansky <maxk@qualcomm.com>
description:    Universal TUN/TAP device driver
srcversion:     04567545119E104B8EEC972
depends:        
intree:         Y
vermagic:       4.11.12 SMP mod_unload modversions 


（5）反思：
如果使用tun来做多网卡的模拟，那么就对底层linux发布版本有要求，不然还需要重新编译内核来打开tun，这点对于用户来将是非常不方便的。
是否采用其他全软件模拟的方式来完成多网卡的支持，而不需要通过内核支持？


参考文档：
[Ubuntu 16.04虚拟网络设备tun安装](http://blog.csdn.net/lishuhuakai/article/details/70305543)
[Ubuntu 16.04虚拟网络设备tun安装](http://www.linuxdiyf.com/linux/30181.html)
[linux内核扩展模块编译tun.ko](http://www.360doc.com/content/14/0124/13/14451193_347548588.shtml)


### 使用tun创建多个网卡：
确认打开tun之后，使用tun来做多网卡的模拟：

创建一个虚拟网卡：
sudo tunctl -t tap0 -u xxx
其中的xxx表示用户，默认是root用户，这儿可以指定为当前用户。还可以使用下面的命令创建默认虚拟网卡：
sudo tunctl -b

激活创建的tap：
sudo ip link set tap0 up
这个时候就可以在ifconfig中看到具体的网卡信息了：

将tap0虚拟网卡添加到指定网桥上：
brctl addif br0 tap0

给网桥配制ip地址：
ifconfig br0 169.254.251.4 up


## KVM基本原理学习：
熟悉了基本的使用流程，然后深入一点原理，帮助最大效率的使用kvm。


## Libvirt学习：
libvirt是目前使用最为广泛的对KVM虚拟机进行管理的工具和API。Libvirtd是一个daemon进程，可以被本地的virsh调用，也可以被远程的virsh调用，Libvirtd调用qemu-kvm操作虚拟机。

OpenStack 为了跨 VM 性，所以不会直接控制 QEMU-KVM，而是通过 libvit 的库去间接控制 QEMU-KVM 。
libvirt是一组管理虚拟机的工具集，支持各种虚拟化平台，例如KVM，Xen等。上述的libvirt其实是libvirt for qemu/kvm，通过这个库可以提供对虚拟环境的统一调用平台。
OpenStack就是通过这个接口来做调用的，Nova对于KVM的管理就是通过libvirt，作为计算资源。
那么我们也可以基于这个接口做调用封装。

Libvirt是一种实现虚拟化平台能力交互的工具集，它为所支持的Hypervisor提供了一种通用的API接口套件，上层管理平台（如Nova）通过Libvirt来实现对虚拟机的生命周期管理。Libvirt当前支持以下底层虚拟化平台：
    KVM：Linux平台仿真器；
    QEMU：面向各种架构的平台仿真器；
    Xen：面向IA-32、IA-64和PowerPC970架构的虚拟机监控程序；
    LXC：用于操作系统虚拟化的linux（轻量级）容器；
    OpenVZ：基于Linux内核的操作系统级虚拟化；
    User Mode Linux：面向各种架构的Linux平台仿真器；
    VirtualBox：x86虚拟化虚拟机监控程序；
    ESX、GSX：VMW爱热企业级虚拟化平台；
    VMWare Workstation、VMWare Player：VMWare用户级虚拟化平台；
    Hyper-V：Microsoft虚拟化平台。
    另外，Libvirt支持以Bridging、NAT、VEPA和VN-LINK方式构建虚拟网络，以及支持基于不同制式的IDE/SCSI/USB disks、FibreChannel、LVM、iSCSI、NFS和filesystems存储。因此，Libvirt在功能性、兼容性以及管理等方面的优势十分明显。
libvirt API 的实现是在各个 Hypervisor driver 和 Storage dirver 内。所以libvirt更像是一种规范接口，然后各个虚拟化层提供这个接口的实现。

建议从官方文档：[KVM/QEMU hypervisor driver](https://libvirt.org/drvqemu.html) 开始熟悉整体调用流程。
我们的目的就是：在虚拟化平台上，建立和物理机相同的访问接口，然后在这个接口上搭建运行环境。


参考文档：
[KVM 介绍（5）：libvirt 介绍 [ Libvrit for KVM/QEMU ]](http://www.cnblogs.com/sammyliu/p/4558638.html)
[Libvirt学习总结](http://blog.csdn.net/gaoxingnengjisuan/article/details/9674315)

### libvirt的python API调用封装：
从官方网站：[libvirt-python](https://github.com/libvirt/libvirt-python) 可以看到最新的发布情况。
我们的开发也是从这一层开始。
并且对于这个库的封装使用，还可以参考：[virt-manager](https://github.com/virt-manager/virt-manager) 中的实现。
例如virt-install这个工具。



## Virtio学习：
根据 [Virtio](https://wiki.libvirt.org/page/Virtio) 上的介绍：
```text
Virtio is a virtualization standard for network and disk device drivers where just the guest's device driver "knows" it is running in a virtual environment, and cooperates with the hypervisor. This enables guests to get high performance network and disk operations, and gives most of the performance benefits of paravirtualization.
```
也就是说：virtio是针对半虚拟化做的IO优化，即virtio是半虚拟化hypervisor中位于设备之上的抽象层。
所谓的半虚拟化，就是Guest OS知道它运行在hypervisor之上，并包含了对这个hypervisor特化的驱动程序。而hypervisor也为特定的设备模拟实现后端驱动程序。
virtio就体现在这两者中，通过在这些前端和后端驱动程序中的virtio，为开发模拟设备提供标准化接口，从而增加代码的跨平台重用率并提高效率。

那么现在需要确认一个问题：alios是否支持半虚拟化，以及网络相关的支持如何？

参考教程：
[Virtio：针对 Linux 的 I/O 虚拟化框架](https://www.ibm.com/developerworks/cn/linux/l-virtio/index.html)










