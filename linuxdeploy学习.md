<!-- TOC -->

- [linuxdeploy学习](#linuxdeploy学习)
    - [chroot切换工作环境：](#chroot切换工作环境)
        - [流程分析：](#流程分析)
        - [依赖工具集：](#依赖工具集)
            - [工具类中的一些用法记录：](#工具类中的一些用法记录)
        - [细节分析：](#细节分析)
            - [配置文件操作:](#配置文件操作)
            - [创建容器操作：](#创建容器操作)
            - [启动容器操作：](#启动容器操作)
        - [搭建开发环境：](#搭建开发环境)

<!-- /TOC -->

# linuxdeploy学习


## chroot切换工作环境：
当前deploy的工作开展依赖于busybox提供的功能，这是因为A默认的工具集中不包含创建rootfs需要的功能。
busybox的执行需要root权限。
对于一个陌生的环境，一定要从这个环境需要解决的问题角度来进行把握，这样才能快速适应，而不是陷入到庞大的细节中。

### 流程分析：
对于A的流程分析，要结合GUI操作的流程来进行梳理，APP的交互操作通过AndroidManifest.xml进行绑定，具体的细节回头在补充，总之，启动后的主要操作在MainActivity.java文件中，我们结合GUI看看整体的操作流程：
（1）从左侧选择一个配置项Repository：
默认有一个linux的配置项，可以修改，用户可以自己新建一个，然后选中后完成对配置项的切换；
这个操作对应的入口为onNavigationItemSelected；
（2）完成选中后，点击右下角完成配置：
在配置中，可以对当前选中的配置项进行细节配置，这部分的操作入口为：onOptionsItemSelected
（3）创建容器：
完成配置后，需要生成空的img保证空间申请成功，然后创建rootfs：
（4）启动，完成容器的启动：
入口点还是在onOptionsItemSelected。

### 依赖工具集：
为了完成这些工作，需要一些工具类的支持，这些工具类为整个流程的执行提供基本功能支持，主要包含（从依赖关系由底层到上层）：
ParamUtils：这个是纯java相关的参数配置类，通过java.io.Closeable对流文件进行处理，完成了对配置文件和map的转换和操作；
SettingsStore：继承了ParamUtils，完成对配置文件获取和读写，这个属于当前的每一个repo的配置有的配置处理；
PropertiesStore：继承了ParamUtils，完成对属性值的读写；
PrefStore：通过对SettingsStore和PropertiesStore的调用，完成配置文件内容，初始化的环境搭建工具，用于文件目录结构的构造；

Logger：日志记录，纯java应用，除了添加了A的Context相关的上下文，通过PrefStore来获取具体的日志文件，因为需要对同一个日志文件写入，所以使用了同步锁；
这儿其实是可以修改的，对于不同的repo使用不同的log来记录更为合理。issue :)

ExecService：对A默认的Service的重载；
EnvUtils：最核心的工具类，用于结合当前上下文，组织上述各个工具类完成任务，并且提供telnetd和web端的访问接口；

#### 工具类中的一些用法记录：
[how-to-use-getsharedpreferences-in-android](https://stackoverflow.com/questions/5950043/how-to-use-getsharedpreferences-in-android)
[探究java IO之AutoCloseable,Closeable和Flushable接口](https://my.oschina.net/fhd/blog/344961)
[Java线程同步：synchronized锁住的是代码还是对象](http://blog.csdn.net/xiao__gui/article/details/8188833)
[反射技术在android中的应用](http://blog.csdn.net/tiefeng0606/article/details/51700866)
[android使用Intent传递数据 2 种方式(Intent和Bundle)](http://blog.csdn.net/neu_yousei/article/details/21953995)
[Difference between getContext() , getApplicationContext() , getBaseContext() and “this”](https://stackoverflow.com/questions/10641144/difference-between-getcontext-getapplicationcontext-getbasecontext-and)
[Android SharedPreferences使用以及原理详解](http://blog.csdn.net/wxyyxc1992/article/details/17222841)
[Java TreeMap工作原理及实现](http://yikun.github.io/2015/04/06/Java-TreeMap%E5%B7%A5%E4%BD%9C%E5%8E%9F%E7%90%86%E5%8F%8A%E5%AE%9E%E7%8E%B0/)
[Telnet是什么？](http://www.jianshu.com/p/4bf3c19aace0)



SharedPreferences是Android平台上一个轻量级的存储类，简单的说就是可以存储一些我们需要的变量信息。
首先通过getSharedPreferences对象获取内容，如果你想要编辑SharedPreferences中的内容就需要用到editor对象。

### 细节分析：
完成了基本工具的熟悉，可以从主窗口入手，分析整个调用流程了。
有一个问题：现在会默认开启telnet和httpd服务，不是很清楚这两个服务是用来干嘛？

#### 配置文件操作:


#### 创建容器操作：
#### 启动容器操作：


### 搭建开发环境：
有了kernel+rootfs就等于有了完整的linux运行环境，可以在这个环境上搭建自己的开发环境了。