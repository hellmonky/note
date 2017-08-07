<!-- TOC -->

- [KVM原理和学习](#kvm原理和学习)
    - [基本概念：](#基本概念)
    - [基本环境搭建：](#基本环境搭建)
        - [环境检测：](#环境检测)
        - [打开虚拟化支持：](#打开虚拟化支持)
        - [安装kvm运行环境：](#安装kvm运行环境)
            - [ubuntu安装基本运行环境：](#ubuntu安装基本运行环境)
            - [centos7环境下安装运行环境：](#centos7环境下安装运行环境)
            - [安装kvm-qemu：](#安装kvm-qemu)
        - [kvm基本使用流程：](#kvm基本使用流程)
            - [（1）首先创建一个虚拟机镜像，类型为qcow2，文件名为centos.img，大小为50G：](#1首先创建一个虚拟机镜像类型为qcow2文件名为centosimg大小为50g)
            - [（2）然后使用IOS启动虚拟机：](#2然后使用ios启动虚拟机)
            - [（3）安装虚拟机，然后做基本设置，保证基础镜像准确：](#3安装虚拟机然后做基本设置保证基础镜像准确)
            - [（4）从基础镜像做差分镜像，减少空间用量：](#4从基础镜像做差分镜像减少空间用量)
            - [（5）搭建差分镜像的集群：](#5搭建差分镜像的集群)
                - [NAT方式：](#nat方式)
                - [Bridge方式：](#bridge方式)
    - [Libvirt学习：](#libvirt学习)
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

### 环境检测： 
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


### 打开虚拟化支持：
要使用kvm，必须首先确保当前的处理器有虚拟化支持，并且需要在BIOS中打开。

### 安装kvm运行环境：
#### ubuntu安装基本运行环境：
需要注意的是：安装qemu-kvm和libvirt-bin将自动创建用户组：
https://linux.cn/article-7060-1.html
然后就可以使用界面管理了，否则会出现没有权限访问的错误。


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


### kvm基本使用流程：
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

##### NAT方式：
我们为了保证每个虚拟机都有固定的IP，修改网络配置文件：
nano /etc/sysconfig/network-scripts/ifcfg-ens3
然后添加内容为：
IPADDR=10.0.2.11
GATEWAY=10.0.2.2
NETMASK=255.255.255.0
DNS1=10.0.2.2
DNS2=8.8.8.8
保存后，重启网络：
systemctl restart network.service
然后重新查看当前IP内容为：
ifconfig

##### Bridge方式：
和其他同事沟通了一下，如果内部vm组网，最好的方式就是通过bridge的方式进行网络规划，这样每个节点都有自己的独立IP。


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

参考：
[KVM/QEMU桥接网络设置及kvm资料](http://blog.csdn.net/cd520yy/article/details/10003343)



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










