### java语言特性的底层实现：
了解了JVM对于java的意义之后，可以深入讨论java的一些语言特性的实现了

### Java Bean：

#### 什么是java bean：
在java编程中，javabean总是被广泛的使用，我们现在来深究一下到底什么是java bean：
[JavaBeans](https://zh.wikipedia.org/wiki/JavaBeans):
> - JavaBeans是Java中一种特殊的类，可以将多个对象封装到一个对象（bean）中。特点是可序列化，提供无参构造器，提供getter方法和setter方法访问对象的属性。名称中的“Bean”是用于Java的可重用软件组件的惯用叫法。

[Java bean 是个什么概念？](https://www.zhihu.com/question/19773379)：
> - Java语言欠缺属性、事件、多重继承功能。所以，如果要在Java程序中实现一些面向对象编程的常见需求，只能手写大量胶水代码。Java Bean正是编写这套胶水代码的惯用模式或约定。这些约定包括getXxx、setXxx、isXxx、addXxxListener、XxxEvent等。遵守上述约定的类可以用于若干工具或库。

[面向对象编程的弊端是什么？](https://www.zhihu.com/question/20275578):
> - Java程序员对有main函数的class非常不满，觉得这侵犯了其他class的平等权，于是他们发明了JavaBean。从此以后他们再也不知道自己的程序究竟有多少个入口了。

[Java 帝国之Java bean (上）](http://mp.weixin.qq.com/s?__biz=MzAxOTc0NzExNg==&mid=2665513115&idx=1&sn=da30cf3d3f163d478748fcdf721b6414#rd):
> - 1. 这个类需要是public 的， 然后需要有个无参数的构造函数
> - 2. 这个类的属性应该是private 的， 通过setXXX()和getXXX()来访问
> - 3. 这个类需要能支持“事件”， 例如addXXXXListener(XXXEvent e),  事件可以是Click事件，Keyboard事件等等， 当然咱们也支持自定义的事件。 
> - 4. 我们得提供一个所谓的自省/反射机制， 这样能在运行时查看java bean 的各种信息“
> - 5. 这个类应该是可以序列化的， 即可以把bean的状态保存的硬盘上， 以便以后来恢复。 

#### javabean有什么用：
综上所述，Java Bean最开始是为了组件化设计提出的，例如要求的可序列化，表示了这个类可以被缓存到硬盘并且被重新保留状态的恢复。
~~同时无参构造函数保证了java的入口函数main函数可以被放在任何的Jave Bean中。~~

要理解为什么java bean中可以包含main入口，我们需要看看main函数是怎么被初始化调用的：


#### java的动态绑定比较学习：
java和C系列的语言特性的对比是非常有意义的，一个是编译为本地相关代码，一个是编译为中间代码，但是两者在特性的实现上还是有参照的。
OOP三大特征：数据抽象（封装）、继承和多态。
其中多态允许基类的指针或引用指向派生类的对象，而在具体访问时实现方法的动态绑定。
对于C++来说，多态机制的实现依赖于virtual关键字，让编译器及进行late binding（也称为dynamic binding，迟绑定或者动态绑定），否则编译器会默认使用early binding（也称为static binding，早绑定或者静态绑定）完成类型转换。
对于java来说，多态机制的实现依赖于method table（方法表），但通过类引用调用(invokevitual)和接口引用调用(invokeinterface)的实现则有所不同。
[Java技术——多态的实现原理](http://blog.csdn.net/seu_calvin/article/details/52191321)
类引用调用的大致过程为：Java编译器将Java源代码编译成class文件，在编译过程中，会根据静态类型将调用的符号引用写到class文件中。在执行时，JVM根据class文件找到调用方法的符号引用，然后在静态类型的方法表中找到偏移量，然后根据this指针确定对象的实际类型，使用实际类型的方法表，偏移量跟静态类型中方法表的偏移量一样，如果在实际类型的方法表中找到该方法，则直接调用，否则，认为没有重写父类该方法。按照继承关系从下往上搜索。 


[]()
[]()
[]()
[]()
[]()
[]()
[]()
[]()
[]()