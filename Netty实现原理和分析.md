# Netty实现原理和分析

现阶段的应用随着业务的复杂度，有慢慢转换为微服务的趋势，越拉越多的应用采用分布式部署的方式进行应用提供，那么这么过程中，传统的HTTP协议虽然可以做到通信，但是其效率低下，除了面向用户的总入口和出口，内部调用是不适合的，RPC框架被广泛的应用。例如HSF这个产品就是基于Netty作为基础开发的。那么之前做的thrift这个框架，还有google的Protobuf本质上都是基于RPC的框架，那么这些不同的产品之间有什么区别？
Protobuf不是分布式的，google在其基础上开发了gRPC框架来和Dubbo等框架进行竞争。
所以基础的技术是不会很快速的过时的，世上没有新鲜事，所有的新技术也是依赖于底层基础才能发挥出作用的。
就以上述这几个技术而言，底层都是依赖于Java的NIO框架Netty来实现的，本文就这个关键技术框架做一个原理分析和梳理。

## Netty的I/O模型：

### Unix系统的I/O模型：
在《UNIX网络编程卷1》第6章中，总结了五种I/O模型：
> - 1. 阻塞I/O（blocking I/O）
> - 2. 非阻塞I/O （nonblocking I/O）
> - 3. I/O复用(select 和poll) （I/O multiplexing）
> - 4. 信号驱动I/O （signal driven I/O (SIGIO)）
> - 5. 异步I/O （asynchronous I/O (the POSIX aio_functions)）
其中前四种都是同步模型，最后的正如其名，是异步模型。

### Netty采用的I/O模型：
Netty采用了Reactor模型结构，Reactor模式是处理并发I/O比较常见的一种模式，用于同步I/O，中心思想是将所有要处理的I/O事件注册到一个中心I/O多路复用器上，同时主线程/进程阻塞在多路复用器上；一旦有I/O事件到来或是准备就绪(文件描述符或socket可读、写)，多路复用器返回并将事先注册的相应I/O事件分发到对应的处理器中。
Reactor是一种事件驱动机制，和普通函数调用的不同之处在于：应用程序不是主动的调用某个API完成处理，而是恰恰相反，Reactor逆置了事件处理流程，应用程序需要提供相应的接口并注册到Reactor上，如果相应的事件发生，Reactor将主动调用应用程序注册的接口，这些接口又称为“回调函数”。用“好莱坞原则”来形容Reactor再合适不过了：不要打电话给我们，我们会打电话通知你。

关于java中采用的Reactor的好处，Doug Lea的文档：[Scalable IO in Java](http://gee.cs.oswego.edu/dl/cpjslides/nio.pdf) 有着明确的解释。


参考文档：
> - [两种高效的服务器设计模型：Reactor和Proactor模型](http://blog.csdn.net/u013074465/article/details/46276967)
> - [Reactor模式详解](http://www.blogjava.net/DLevin/archive/2015/09/02/427045.html)