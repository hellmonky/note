# android framework学习之内核驱动以及HAL学习：

android虽然作为基于linux的一种发行版，但是其设计之初就和linux的开源设定不同，作为一个开放的商业应用环境，android从linux中获取了所有构想，除去驱动框架，所以这部分主要学习和分析一下android最为不同的部分。
本文主要分为两个大的部分：android内核的驱动模块，android的HAL层抽象分析。从这两个部分来结合linux的驱动规范一起看看两者的差别和细节。

## 1 android内核的驱动模块：
和GUN/linux不同，android虽然使用了标准linux内核，但是再次基础上做出了非常多的修改。其中最终让android从linux树上除去的部分就在于驱动模块。
android一方面使用了linux内核中大量关于内核驱动的代码，又通过修改内核，实现了HAL层，让自己的内核驱动在用户空间实现闭源，然后用HAL层在内核执行这些闭源驱动来保护硬件厂商的利益。
这样android就支持了两级硬件驱动：GUN/Linux的内核硬件驱动模式（内核源代码），android的HAL硬件驱动模式（用户空间）。
但是厂家（特指IC原厂）如果考虑到驱动移植问题（或者旧有驱动port的问题），针对linux的驱动最好还是放置在内核代码中，不然就只能在android中使用。但是有的厂家不这么干，例如高通，博通等。
如果厂家要将自己的硬件驱动代码放在用户空间中，那么他需要这么做：
<1>首先，在android的内核驱动中新增一个硬件驱动程序；
<2>然后，在用户空间中为该硬件添加一个硬件抽象层模块；
<3>接着，在应用程序框架中添加一个硬件访问服务；
<4>最后，开发一个应用程序来访问<3>中的硬件访问服务。
其中：
<1>的作用在于为在Linux中只保留硬件与所需要寄存器的操作代码（一般而言为寄存器的基本读写函数），然后硬件和这些寄存器之间的数据传递就通过HAL进行，隔绝了硬件操作寄存器的详细代码实现；
<2>的目的是进行硬件设备对<1>中资源的操作的实现，也就是厂商不愿意开源的硬件操作细节代码，最终这部分代码被编译为二进制文件供外部调用；
<3>编写好了硬件驱动操作模块，还需要在framework中添加硬件访问接口，最后每一个硬件都有一个独立的硬件访问模块（对应于一个.so文件）；
<4>实现一个典型的硬件访问应用，例如屏幕绘制，CPU跑分等的程序开发，这部分主要依赖于framework中的硬件接口，和底层实现完全分离。
所以最关键的就是<2>的HAL层的实现了，他起到一个隔离作用，并且方便厂商开发私有驱动。
总结如下：
HAL架构比较简单，其基本原理就是在Android系统中使用用户空间的程序库（.so文件）调用位于内核空间的Linux 驱动（一般通过设备文件访问，位于/dev目录下）实现完整的驱动功能。然后给Android应用程序（APK 文件）提供统一的HAL硬件访问接口，供APK调用硬件实现功能。

更为详细具体来说，legency驱动方法可以不需要Android源代码单独进行编译，完成一个基本legency驱动需要：
<1>写LINUX驱动
<1>写LINUX应用测试程序
<1>写HAL驱动代码，调用linux native接口控制硬件。
<1>写JNI接口，用来包装第二步写的应用（要用NDK来编译）生成一个.SO文件，相当于CE下的DLL
<1>写JAVA程序,专门写一个类包含.SO文件，然后在JAVA里调用.SO里的函数。例子，可以看NDK里面的Sample文件夹，里面有一些例子
对于使用硬件服务方式编写驱动，好处是比较灵活给上层调用，硬件隔离，需要Android源码配合编译，一般编写方式如下：
<1>linux 字符设备驱动的编写(kernel/drivers/char/chr_dev/)
<1>linux 字符设备驱动的验证程序(openplatform/android/externl/chr_dev/)
<1>硬件抽象层(hal)子程序编写(android/hardware/libhardware/moudles/chr_dev/ 及android/hardware/libhardware/include/)
<1>jni 程序编写(android/framework/base/services/jni/)
<1>aidl 编写(android/framework/base/core/java/android/os/)
<1>aidl 接口具体实现(android/framework/base/services/java/com/android/server/)
<1>字符设备文件权限设置 (android/system/core/rootdir/ueventd.rc)
<1>字符设备ko文件，自动加载设置(android/openplatform/project/common/kk_4.4_overlay/rootfs/sbin/init.rc)


## 2 关于android的HAL层分析：
Android Hal层（即 Hardware Abstraction Layer）是Google开发的Android系统里上层应用对底层硬件操作屏蔽的一个软件层次，说直白点，就是上层应用不必关心底层硬件具体是如何工作的，只需要调用底层提供的统一接口即可，这种设计思想广泛的存在于当前的软件的架构设计里。
通过这个层次给整个android framework提供了一个统一的java访问硬件的接口，这个接口底层通过JNI调用本地so文件的形式来实现实际硬件的操作。
<1>Android Hal架构分为两种：
①旧的架构module：
Module架构
     Android用户应用程序或者框架层代码由JAVA实现，Java运行在Dalvik虚拟机中，没有办法直接访问底层硬件，只能通过调用so本地库代码实现，在so本地代码里有对底层硬件操作的代码。应用层或者框架层Java代码，通过JNI技术调用C或C++写的so库代码，在so库代码中调用底层驱动，从而实现上层应用操作底层硬件的目的。实现硬件操作的so库为module。
     这种设计架构虽然满足了Java应用访问硬件的需要，但是，使得我们的代码上下层次间的耦合太高，用户程序或者框架代码必须要去加载module库，如果底层硬件有变化，module要从新编译，上层也要做相应变化，另外，如果多个应用程序同时访问硬件，都去加载module，同一module被多个进程映射多次，会有代码的重入问题。
②新的架构module stub：
     新的代码架构使用的是module stub方式.Stub是存根或者桩的意思，其实说白了，就是指一个对象代表的意思。上层应用层或者框架层代码加载so库代码，so库代码我们称之为module，在Hal层注册了每个硬件对象的存根stub，当上层需要访问硬件的时候，就从当前注册的硬件对象stub里查找，找到之后stub会向上层module提供该硬件对象的operations interface（操作接口），该操作接口就保存在module中，上层应用或框架层再通过这个module操作接口来访问硬件。
这两种结构的对比：
    在Module架构中，本地代码由so库实现，上层直接将so库映射到进程空间，会有代码重入及设备多次打开的问题。新的Stub框架虽然也要加载module库，但是这个module已经不包含操作底层硬件驱动的功能了，它里面保存的只是底层stub提供的操作接口，底层stub扮演了“接口提供者”的角色，当stub第一次被使用时加载到内存，后续再使用时仅返回硬件对象操作接口，不会存在设备多次打开的问题，并且由于多进程访问时返回的只是函数指针，代码并没有重入。
<2>module stub代码分析：
Hal Stub的框架比较简单，三个结构体、两个常量、一个函数，简称321架构，它的定义在：
../hardware/libhardware/include/hardware/hardware.h
../hardware/libhardware/hardware.c
这两个文件中。
三个重要的结构体，其包含在hardware.h中，具体代码为：
```c
/**
 *每一个硬件都通过hw_module_t来描述，我们称之为一个硬件对象。你可以去"继承"这个hw_module_t
 *然后扩展自己的属性，硬件对象必须定义为一个固定的名字HMI,即:Hardware Module Information的简写
 *每个硬件对象里都封装了一个函数指针open用于打开硬件，我们理解为硬件对象的open方法，open调用后
 *返回这个硬件对应的操作接口集合
**/
typedef struct hw_module_t

/**
 *硬件对象的open方法描述结构体，它里面只有一个元素:open函数指针
**/
typedef struct hw_module_methods_t

/**
 *硬件对象hw_module_t的open方法返回该硬件的Operation interface，它由hw_device_t结构体来描述
 *我们称之为该硬件的操作接口
**/
typedef struct hw_device_t
```
上述三个结构体之间关系紧密，每个硬件对象都由hw_module_t来描述，只要我们拿到了这个硬件对象，就可以调用它的open方法，返回这个硬件对象的硬件操作接口，然后就可以通过这些硬件操作接口来间接操作硬件。只不过，open方法被hw_module_methods_t结构封装了一次，硬件操作接口被hw_device_t封装了一次而已。
