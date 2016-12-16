### NIO框架：
NIO框架，是指在专门针对于块设备的IO框架。
做java只会http，那人家要你写一个网络传输的，高性能的，你又不知道如何是好了。
所以需要明白怎么样才能完成高并发的网络传输。

#### netty：
Netty是一款异步的事件驱动的网络应用框架和工具，用于快速开发可维护的高性能、高扩展性协议服务器和客户端。也就是说，Netty是一个NIO客户端/服务器框架，支持快速、简单地开发网络应用，如协议服务器和客户端。它极大简化了网络编程，如TCP和UDP套接字服务器。

#### Mina(Multipurpose Infrastructure for Network Applications) ：
是 Apache 组织一个较新的项目，它为开发高性能和高可用性的网络应用程序提供了非常便利的框架。当前发行的 Mina 版本2.04支持基于 Java NIO 技术的 TCP/UDP 应用程序开发、串口通讯程序，Mina 所支持的功能也在进一步的扩展中。目前，正在使用 Mina的应用包括：Apache Directory Project、AsyncWeb、AMQP（Advanced Message Queuing Protocol）、RED5 Server（Macromedia  Flash Media RTMP）、ObjectRADIUS、 Openfire等等。

#### Grizzly：
Grizzly是一种应用程序框架，专门解决编写成千上万用户访问服务器时候产生的各种问题。使用JAVA NIO作为基础，并隐藏其编程的复杂性。容易使用的高性能的API。带来非阻塞socketd到协议处理层。利用高性能的缓冲和缓冲管理使用高性能的线程池。

#### xSocket：
是一个轻量级的基于nio的服务器框架用于开发高性能、可扩展、多线程的服务器。该框架封装了线程处理、异步读/写等方面。（只是对Java的NIO做了最简单的封装，以便于开发使用。）

[]()
[]()
[]()
[]()
[]()
[]()
[]()
[]()
[]()
[]()