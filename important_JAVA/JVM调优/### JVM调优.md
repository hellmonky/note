### JVM调优：
为什么需要JVM调优？在什么情况下需要调优？如何调优？调优的结果对比有没有，具体的数据是多少？
我们分析一个java源文件代码文件被编译执行的过程来说明为什么需要jvm调优：
首先，我们在一个已经建立的文件目录结构的前提下，编写一个简单的java代码来演示整个过程，文件为：Test.java：
```java
package com.iscas.hellmonky;
class Test{
    public static void main(String[] args) {
        String hello = "hello world";
        System.out.println(hello);
    }
}
```
简单说一下这个代码：
第一行：说明这个代码的包结构，这个代码放在当前工程的..\src\main\java\com\iscas\hellmonky这个目录下，java通过这种方式来唯一确定命名空间；
第二行：新建了一个类，这个类的名称和java源代码的文件名相同，也是java规定的写法，用于生成的文件和类一一对应，并且类的首字母大写；
第三行：在这个Test类中定义了一个总的入口，这一点和C系列的代码相同，通过这个入口，编译结构就可以运行了；
第四行：定义了一个String类型的变量；
第五行：使用系统调用将这个String变量输出到控制台。

然后在命令行中使用java编译这个源文件：javac Test.java；编译完毕会生成一个文件：Test.class；然后输入：java Test.class 执行这段代码。
这个代码的执行室友java调用jvm虚拟机对class文件做处理的。
[自己动手编译、运行Java程序](http://www.cnblogs.com/haolujun/archive/2013/03/02/2939698.html)

所以整个流程中可以优化的地方就在于：java源文件的编写，jvm虚拟机的调优。
这样就能明白为什么要jvm调优，因为当代码一定的情况下，需要针对使用场景对jvm进行设置才能充分利用性能。



[JVM调优总结（一）-- 一些概念](http://pengjiaheng.iteye.com/blog/518623)
[JVM调优总结（二）-一些概念](http://pengjiaheng.iteye.com/blog/519471)
[JVM调优总结（三）-基本垃圾回收算法](http://pengjiaheng.iteye.com/blog/520228#bc2382393)

[Java 代码编译和执行的整个过程](http://wiki.jikexueyuan.com/project/java-vm/java-debug.html)
[JVM 性能调优实战之：一次系统性能瓶颈的寻找过程](http://www.importnew.com/22434.html)
[深入理解JVM(六)——JVM性能调优实战](http://blog.csdn.net/u010425776/article/details/51232463)
[]()
[]()
[]()
[]()
[]()
[]()
[]()