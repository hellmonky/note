<!-- TOC -->

- [关于跨语言远程过程调用框架（RPC）——thrift的学习](#关于跨语言远程过程调用框架rpcthrift的学习)
    - [安装和官方样例测试：](#安装和官方样例测试)
        - [windows环境下的编译器的安装：](#windows环境下的编译器的安装)
        - [编译并安装开发环境：](#编译并安装开发环境)
        - [官方测试：](#官方测试)
    - [相同语言的服务调用测试——java：](#相同语言的服务调用测试java)
        - [编写IDL文件来描述被访问的接口：](#编写idl文件来描述被访问的接口)
        - [从IDL生成java文件：](#从idl生成java文件)
        - [用java实现定义的接口：](#用java实现定义的接口)
        - [实现java的服务端代码：](#实现java的服务端代码)
        - [实现java的客户端代码：](#实现java的客户端代码)
        - [测试与服务端通信：](#测试与服务端通信)
    - [跨语言的服务调用测试——java和C++调用交互：](#跨语言的服务调用测试java和c调用交互)
        - [windows环境下thrift源代码的编译：](#windows环境下thrift源代码的编译)
            - [依赖文件清单：](#依赖文件清单)
            - [依赖关系说明：](#依赖关系说明)
            - [编译过程：](#编译过程)
        - [从IDL生成cpp源文件：](#从idl生成cpp源文件)
        - [编写C++的客户端：](#编写c的客户端)
            - [cpp的客户端开发中添加依赖于boost提供的文件：](#cpp的客户端开发中添加依赖于boost提供的文件)
            - [cpp的客户端开发中添加依赖于thrift提供的文件：](#cpp的客户端开发中添加依赖于thrift提供的文件)
            - [cpp的客户端开发中添加依赖于openssl提供的文件：](#cpp的客户端开发中添加依赖于openssl提供的文件)
            - [编译出现问题：](#编译出现问题)
            - [手动编译openssl：](#手动编译openssl)
        - [编写C++的服务端：](#编写c的服务端)
    - [使用thrift完成其他语言之间的相互调用：](#使用thrift完成其他语言之间的相互调用)
    - [thrift基本语法学习：](#thrift基本语法学习)
    - [使用thrift做JDBC开发：](#使用thrift做jdbc开发)
    - [与其他PRC框架对比：](#与其他prc框架对比)
    - [源代码分析：](#源代码分析)

<!-- /TOC -->

# 关于跨语言远程过程调用框架（RPC）——thrift的学习

因为最近项目需要将已经有的java服务提供给其他开发人员使用，那么跨语言的过程调用就显得非常重要，facebook开源了他的RPC框架thrift能够很好的解决不同语言之间的过程调用问题，现在学习记录如下。

## 安装和官方样例测试：

### windows环境下的编译器的安装：
访问官方网站的[下载链接](http://thrift.apache.org/download)，可以直接获windows下编译好的可执行文件，这个文件的作用是将IDL转换为不同语言的源代码，被称为Thrift compiler。
新建目录，C:\Program Files\thrift，把下载好的thrift-0.10.0.exe文件放在里面，然后修改文件名为thrift.exe，然后把C:\Program Files\thrift添加到windows下面的环境变量PATH中。
在cmd命令行中输入：
```shell
thrift -version
```
可以看到当前thrift编译器的版本信息，表示可以使用thrift编译器将IDL进行转换了，编译器安装成功。

### 编译并安装开发环境：
因为thrift可执行文件只是包含了转换，没有提供其他程序开发时的库支持，所以需要在每一个平台上具体的进行编译来获取支持开发的lib库，例如java的jar包，C++的静态库等。
可以参考[官方编译文档](http://thrift.apache.org/docs/install/)查看具体每一个平台上的编译和安装。
对于java开发而言，已经给出了编译好的maven源，可以直接使用；对于C++而言，因为没有完善的库管理方案，就只能自己手动编译了。

### 官方测试：
从官方网站的下载页，获取thrift的源代码包，目前最近为：thrift-0.10.0.tar.gz。解压后进入tutorial目录。
这个目录中包含官方提供的shared.thrift和tutorial.thrift这两个IDL（Interface Definition Language）文件所描述服务的被支持的各种语言的测试文件，可以选择我们需要进行测试的语言进行测试。
所谓的IDL，就是接口定义，thrift通过自己定义的语法来对接口进行描述，从而使用这种DSL来完成对不同程序开发语言接口实现的转换。
拷贝官方的IDL文件到一个空目录，然后执行：
```shell
thrift -r --gen cpp tutorial.thrift
```
从IDL生成其他语言的命令的一般格式为：
```shell
thrift --gen <language> <Thrift filename>
```

这个命令会生成一个gen-cpp文件夹，里面包含这两个IDL文件同名的文件夹来包含对应的C++的接口实现，我们需要做的就是补全这个接口实现中的客户端和服务端。
因为这个是官方的教程文件，所以已经在tutorial.thrift同级的目录下放置了cpp文件夹，其中就包含了我们原本需要自己实现的客户端和服务端代码。
我们只需要将这个文件夹下的源代码添加到工程中，并且引入生成的cpp文件到这个工程中，添加需要的boots库和thrift静态库等到工程中就可以进行编译测试了。


## 相同语言的服务调用测试——java：
在了解了上述基本流程后，我们实际编写一个供java客户端调用的java服务端。选择java是因为整体流程相对适中，不需要C++进行本地编译，也不想动态语言过于简单，能够很好的描述整个开发流程。

具体流程为：使用thrift定义的IDL作为中间接口，完成实现这个接口的java服务端，然后提供调用这个接口的java客户端。
> - 参考文档：[Apache Thrift - 可伸缩的跨语言服务开发框架](https://www.ibm.com/developerworks/cn/java/j-lo-apachethrift/)

### 编写IDL文件来描述被访问的接口：
新建一个空的hello.thrift文件，输入：
```thrift
namespace java service.demo 
service Hello{ 
    string helloString(1:string para) 
    i32 helloInt(1:i32 para) 
    bool helloBoolean(1:bool para) 
    void helloVoid() 
    string helloNull() 
}
```
这个文件就定义了一个服务Hello，并且这个服务中定义了五个方法，每个方法包含一个方法名，参数列表和返回类型。每个参数包括参数序号，参数类型以及参数名。
需要注意的是上述内容使用了thrift的IDL语言进行编写，具体细节后续进行学习。
可以将这个thrift文件放在java工程的resource文件夹下，供后续修改时重新编译使用。

### 从IDL生成java文件：
使用命令：
```shell
thrift --gen java hello.thrift
```
会在当前目录下生成文件：
```shell
│  hello.thrift
│
└─gen-java
    └─service
        └─demo
           └─Hello.java
```

正好对应于namespace中设置的包路径：service.demo。
这个Hello.java文件就是IDL中描述接口的java接口定义，我们需要将这个接口定义进行实现，从而完成客户端和服务端具体实现。

打开Hello.java可以看到主体代码为：
```java
public class Hello {

  public interface Iface {

    public java.lang.String helloString(java.lang.String para) throws org.apache.thrift.TException;

    public int helloInt(int para) throws org.apache.thrift.TException;

    public boolean helloBoolean(boolean para) throws org.apache.thrift.TException;

    public void helloVoid() throws org.apache.thrift.TException;

    public java.lang.String helloNull() throws org.apache.thrift.TException;

  }
  
  public static class Client extends org.apache.thrift.TServiceClient implements Iface {
    ...
  }

  public static class AsyncClient extends org.apache.thrift.async.TAsyncClient implements AsyncIface {
      ...
  }
  
  public void helloInt(int para, org.apache.thrift.async.AsyncMethodCallback<java.lang.Integer> resultHandler) throws org.apache.thrift.TException {
      ...
  }
  
  public static class helloInt_call extends org.apache.thrift.async.TAsyncMethodCall<java.lang.Integer> {
      ...
  }
  
  public void helloBoolean(boolean para, org.apache.thrift.async.AsyncMethodCallback<java.lang.Boolean> resultHandler) throws org.apache.thrift.TException {
      ...
  }

  public static class helloBoolean_call extends org.apache.thrift.async.TAsyncMethodCall<java.lang.Boolean> {
      ...
  }
  ...

  public static class Processor<I extends Iface> extends org.apache.thrift.TBaseProcessor<I> implements org.apache.thrift.TProcessor {
      ...
  }
  
  public static class AsyncProcessor<I extends AsyncIface> extends org.apache.thrift.TBaseAsyncProcessor<I> {
      ...
  }
  
  ...
}
```
可以看到这个文件包含了在 Hello.thrift 文件中描述的服务 Hello 的接口定义，即 Hello.Iface 接口，以及服务调用的底层通信细节，包括客户端的调用逻辑 Hello.Client 以及服务器端的处理逻辑 Hello.Processor，用于构建客户端和服务器端的功能。
具体的生成文件的细节需要后续继续学习。

### 用java实现定义的接口：
创建 HelloServiceImpl.java 文件并实现 Hello.java 文件中的 Hello.Iface 接口，内容为：
```java
package service.demo;

import org.apache.thrift.TException;

public class HelloServiceImpl implements Hello.Iface { 
    @Override 
    public boolean helloBoolean(boolean para) throws TException { 
        return para; 
    } 
    @Override 
    public int helloInt(int para) throws TException { 
        try { 
            Thread.sleep(20000); 
        } catch (InterruptedException e) { 
            e.printStackTrace(); 
        } 
        return para; 
    } 
    @Override 
    public String helloNull() throws TException { 
        return null; 
    } 
    @Override 
    public String helloString(String para) throws TException { 
        return para; 
    } 
    @Override 
    public void helloVoid() throws TException { 
        System.out.println("Hello World"); 
    } 
 }
```
这个代码完成了IDL中接口的具体实现细节，也就是完成了接口的功能实现。

### 实现java的服务端代码：
创建服务器端实现代码，将 HelloServiceImpl 作为具体的处理器传递给 Thrift 服务器，具体的代码为：
```java
package service.demo;

import org.apache.thrift.TProcessor;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TBinaryProtocol.Factory;
import org.apache.thrift.server.TServer;
import org.apache.thrift.server.TSimpleServer;
import org.apache.thrift.transport.TServerSocket;
import org.apache.thrift.transport.TTransportException;

public class HelloServiceServer {
    public static void main(String[] args) {
        try {
            TServerSocket serverTransport = new TServerSocket(7911);
            Factory proFactory = new TBinaryProtocol.Factory();
            TProcessor processor = new Hello.Processor(new HelloServiceImpl());
            TServer server = new TSimpleServer(new TServer.Args(serverTransport).processor(processor));
            //TServer server = new TThreadPoolServer(processor, serverTransport,  proFactory);
            System.out.println("Start server on port 7911...");
            server.serve();
        } catch (TTransportException e) {
            e.printStackTrace();
        }
    }
}
```
这段代码就完成了接口的java端服务器的实现，通过运行这个代码，就得到了IDL中描述接口的具体实现的服务端。

将上述使用IDL生成的Hello.java源文件，还有两个自己手动补充的源文件均放置在service.demo包路径下，使用IDEA新建gradle工程，然后编辑build.gradle文件内容，添加thrift支持：
```gradle
group 'hellmonky'
version '1.0-SNAPSHOT'
apply plugin: 'java'
sourceCompatibility = 1.8

repositories {
    mavenCentral()
}

dependencies {
    testCompile group: 'junit', name: 'junit', version: '4.11'

    compile("org.apache.thrift:libthrift:0.10.0")
}

task runnbaleJar(type: Jar) {
    from files(sourceSets.main.output.classesDir)
    from configurations.runtime.asFileTree.files.collect { zipTree(it) }
    manifest {
        attributes 'Main-Class': 'service.demo.HelloServiceServer'
    }
}
```
需要注意的是，其中的task runnbaleJar创建了一个完整的可执行的服务端jar包，否则会因为库的不完整报JNI错误。然后使用：
```java
java -jar hellmonky-1.0-SNAPSHOT.jar
```
回显：
```java
SLF4J: Failed to load class "org.slf4j.impl.StaticLoggerBinder".
SLF4J: Defaulting to no-operation (NOP) logger implementation
SLF4J: See http://www.slf4j.org/codes.html#StaticLoggerBinder for further details.
Start server on port 7911...
```
就表示已经正常的启动运行java的后台服务了。

### 实现java的客户端代码：
在完成了上述满足接口规范的java后端服务器开发后，就可以继续从IDL中出发来完成其他语言的客户端的开发，这儿为了简化理解，还是使用java来开发客户端。
在当前工程目录下新建包client，然后实现HelloServiceClient.java文件：
```java
package service.demo.client;

/**
 * Created by 文涛 on 2017/2/14.
 */
import org.apache.thrift.TException;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.TSocket;
import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TTransportException;
import service.demo.autogen.Hello;

public class HelloServiceClient {
    public static void main(String[] args) {
        try {
            TTransport transport = new TSocket("localhost", 7911);
            transport.open();
            TProtocol protocol = new TBinaryProtocol(transport);
            Hello.Client client = new Hello.Client(protocol);
            client.helloVoid();
            transport.close();
        } catch (TTransportException e) {
            e.printStackTrace();
        } catch (TException e) {
            e.printStackTrace();
        }
    }
}
```

### 测试与服务端通信：
将当前HelloServiceClient.java文件作为执行入口，可以看到执行结果返回为：
```shell
SLF4J: Failed to load class "org.slf4j.impl.StaticLoggerBinder".
SLF4J: Defaulting to no-operation (NOP) logger implementation
SLF4J: See http://www.slf4j.org/codes.html#StaticLoggerBinder for further details.
Received 1

Process finished with exit code 0
```

同时，在server的启动窗口可以看到执行返回：
```shell
SLF4J: Failed to load class "org.slf4j.impl.StaticLoggerBinder".
SLF4J: Defaulting to no-operation (NOP) logger implementation
SLF4J: See http://www.slf4j.org/codes.html#StaticLoggerBinder for further details.
Start server on port 7911...
Hello World
```
至此，一个使用thrift的IDL定义的接口的java服务端实现和客户端调用接口就可以正常运行了。


## 跨语言的服务调用测试——java和C++调用交互：
完成了上述相同语言的RPC调用后，可以思考一下跨语言的服务调用过程。既然是语言无关的，那么最好的方式就是通过web进行交互，因为web本身的设计就考虑到了多语言多运行环境的交互。

在实际工程开发中，往往是已经存在一个后台服务，需要面向其他语言开发者提供服务的访问，常见的方式就是通过JSON作为数据，然后使用HTTP或者Socket作为访问方式来进行语言无关的服务提供。之前就是使用RESTFul API作为服务，C++端使用curl访问HTTP进行交互。
但是有时候也需要首先定义交互接口，然后客户端和服务端同步开发来完成敏捷开发迭代。这个时候就需要首先确定接口规范，然后各自进行开发，使用thrift是一个不错的选择。

不论上述两种情况如何，通过thrift来描述中间的交互接口是一个非常不错的方式，方便前后端分离开发。现在我们还是以上述Hello服务为例，看看C++的客户端该如何实现，从而用java作为后台服务端，供C++客户端访问服务。

### windows环境下thrift源代码的编译：
> -  参考文档：
[Using Thrift with C++](https://thrift.apache.org/lib/cpp)
[Windows Setup](http://thrift.apache.org/docs/install/windows)
[Apache Thrift 在Windows下的安装与开发](http://blog.csdn.net/colouroo/article/details/38588297)
[Thrift在Windows及Linux平台下的安装和使用示例](http://cpper.info/2016/03/06/Thrift-Install-And-Example.html)
[thrift在windows的编译/安装--c++版](http://www.cnblogs.com/mumuxinfei/p/3715721.html)


因为要使用C++进行开发，所以需要针对thrift源代码进行编译来获取对C++的支持。
进入目录：
```shell
..\thrift-0.10.0\lib\cpp
```
可以看出，在windows下提供了vs的工程文件，也可以通过cmake来完成平台无关的thrift编译。最后可以编译生成thrift的lib库文件和头文件供开发人员使用。
现在就用vs工程文件打开的方式对thrift进行编译。

#### 依赖文件清单：
[thrift-0.10.0.tar.gz源码包]()
VS2010
boost库，根据thrift发布版本要求，使用的boost1.53以及上版本，这儿选择sourceforge上发布的预编译好的vs2010的32位安装包:[boost_1_63_0-msvc-10.0-32.exe](https://sourceforge.net/projects/boost/files/?source=navbar)
libevent库，这里用的[libevent-2.1.8-stable.tar.gz](http://libevent.org/)
openssl库，虽然官方给出的[下载链接](https://www.openssl.org/source/)中为了安全不包含二进制包，但是为了方便起见，从[Win32 OpenSSL](http://slproweb.com/products/Win32OpenSSL.html)项目页下载到已经编译好的开发包使用。

#### 依赖关系说明：
Thrift.sln，里面有libthrift和libthriftnb两个工程，其中libthrift工程是常规的阻塞型server端（单线程server，一个连接一个线程server，线程池server），libthriftnb工程是非阻塞（non-blocking）模式的服务server端，也只有编译libthriftnb时才需要依赖libevent库，否则可以不编译libevent库；
thrift使用ssl协议来保证网络传输的安全性。

#### 编译过程：
首先：
安装boost和openssl

然后：
从源代码编译libevent。对于libevent库的编译比较麻烦，因为官方没有给出cmake等工程管理文件，而是使用vs的控制台nmake进行编译，参考：[windows下编译及使用libevent](http://www.cnblogs.com/luxiaoxun/p/3603399.html)

最后：
打开thrift工程文件，添加boost、openssl和libevent库的头文件和静态链接库位置，完成编译。


### 从IDL生成cpp源文件：
和java程序相同，为了从IDL的描述中获取cpp源文件，也需要使用thrift来生成一次，进入hello.thrift所在目录，执行：
```shell
thrift --gen cpp hello.thrift
```
会生成gen-cpp文件夹，包含以下文件：
```shell
|─ghello.thrift
│
├─gen-cpp
│      Hello.cpp
│      Hello.h
│      hello_constants.cpp
│      hello_constants.h
│      Hello_server.skeleton.cpp
│      hello_types.cpp
│      hello_types.h
```

### 编写C++的客户端：
需要注意的是，上述生成的cpp文件，只是包含了IDL的服务端相关代码，并不包含客户端调用的代码，所以需要自己新建并编写客户端代码，然后删除生成的HelloService_server.skeleton.cpp，将上述其他的生成的代码一起编译才能正确获取客户端可执行文件。
这是因为客户端的服务代码编写基本上都是通过使用thrift在不同程序开发语言中提供的库来完成远程访问，基本上都非常直接，不需要涉及服务端实现细节的部分。

新建client.cpp文件：
```cpp
#include <stdio.h>
#include <string>

#include <boost/make_shared.hpp>

#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/transport/TSocket.h>
#include <thrift/transport/TTransportUtils.h>

#include "hello_types.h"
#include "Hello.h"

using namespace boost;
using namespace apache::thrift;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;

int main(int argc, char** argv){
    shared_ptr<TTransport> socket(new TSocket("192.168.15.1", 7911));
    shared_ptr<TTransport> transport(new TBufferedTransport(socket));
    shared_ptr<TProtocol> protocol(new TBinaryProtocol(transport));
    HelloClient client(protocol);
	try {
		transport->open();
		client.helloVoid();
		transport->close();
	}
	catch(TException& tx) {
		printf("ERROR:%s\n",tx.what());
	}
}
```
然后对当前项目添加外部依赖。

#### cpp的客户端开发中添加依赖于boost提供的文件：
```shell
#include <boost/make_shared.hpp>
```
需要使用和thrift的编译版依赖的boots库相同的版本。位于boost二进制安装路径下:
```shell
C:\boost_1_63_0\lib32-msvc-10.0\libboost_thread-vc100-mt-gd-1_63.lib
C:\boost_1_63_0\lib32-msvc-10.0\libboost_system-vc100-mt-gd-1_63.lib
C:\boost_1_63_0\lib32-msvc-10.0\libboost_atomic-vc100-mt-gd-1_63.lib
C:\boost_1_63_0\lib32-msvc-10.0\libboost_date_time-vc100-mt-gd-1_63.lib
C:\boost_1_63_0\lib32-msvc-10.0\libboost_chrono-vc100-mt-gd-1_63.lib
C:\boost_1_63_0\lib32-msvc-10.0\
C:\boost_1_63_0\lib32-msvc-10.0\
C:\boost_1_63_0\lib32-msvc-10.0\
C:\boost_1_63_0\lib32-msvc-10.0\
C:\boost_1_63_0\lib32-msvc-10.0\
```

#### cpp的客户端开发中添加依赖于thrift提供的文件：
```shell
#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/transport/TSocket.h>
#include <thrift/transport/TTransportUtils.h>
```
位于thrift源代码包的lib文件夹下：
```shell
..\thrift-0.10.0\lib\cpp\src
```
所以在工程中（cmake文件等工程描述文件中）将这个路径添加到项目的附加库头文件依赖路径。
然后添加thrift的静态库：
..\thrift-0.10.0\lib\cpp\Debug\libthrift.lib

#### cpp的客户端开发中添加依赖于openssl提供的文件：
```shell
C:\OpenSSL-Win32\lib\VC\static\libeay32MTd.lib
C:\OpenSSL-Win32\lib\VC\static\ssleay32MTd.lib
```

#### 编译出现问题：
按照上述步骤添加依赖之后，出现：
```shell
1>ssleay32MTd.lib(t1_lib.obj) : error LNK2001: 无法解析的外部符号 ___report_rangecheckfailure
1>libeay32MTd.lib(b_print.obj) : error LNK2019: 无法解析的外部符号 ___report_rangecheckfailure，该符号在函数 _fmtfp 中被引用
1>libeay32MTd.lib(obj_dat.obj) : error LNK2001: 无法解析的外部符号 ___report_rangecheckfailure
1>libeay32MTd.lib(b_dump.obj) : error LNK2001: 无法解析的外部符号 ___report_rangecheckfailure
1>libeay32MTd.lib(pem_lib.obj) : error LNK2001: 无法解析的外部符号 ___report_rangecheckfailure
```
[网上搜索](https://github.com/pyca/cryptography/issues/2024)发现为Win32 OpenSSL使用vs2012编译导致了错误，所以最好的方法就是更新编译器到2012，或者自己编译对应版本的openssl。

#### 手动编译openssl：
既然问题出在openssl上，那么就采用自己手动编译的方式来解决，打开vs的控制台，然后按照如下步骤执行：
```shell
perl Configure VC-WIN32 no-asm --prefix=C:\openssl_lib

ms\do_ms.bat

nmake -f ms\ntdll.mak
nmake -f ms\nt.mak

nmake -f ms\ntdll.mak test
nmake -f ms\nt.mak test

nmake -f ms\ntdll.mak install
nmake -f ms\nt.mak install
```
如果要编译64位，首先需要打开vs的64位控制台，然后修改perl的配置为：
```shell
perl Configure VC-WIN64A no-asm --prefix=C:\openssl_lib
```
还要注意要关闭asm，否则会编译失败，或者使用masm进行预编译处理。

然后使用这个openssl来对thrift进行重新编译。

> - 参考文档：
[编译openssl出错](http://bbs.csdn.net/topics/390986380)
[VS2010编译OpenSSL](http://blog.csdn.net/zhangmiaoping23/article/details/52815974)
[在Windows系统上安装OpenSSL及在VS2010中使用OpenSSL](http://blog.csdn.net/iw1210/article/details/50947654)


### 编写C++的服务端：
上述java教程中实现了相同语言的服务端和客户端，上一节又直接使用thrift来使用了C++开发了客户端，这一节使用cpp来开发IDL对应的服务端，然后使用java进行调用。
PS:由于thrift的windows环境的C++编译比较麻烦，暂时不进行实际测试。

## 使用thrift完成其他语言之间的相互调用：
[使用thrift做c++,java和python的相互调用](http://jinghong.iteye.com/blog/1222713)

## thrift基本语法学习：


## 使用thrift做JDBC开发：


## 与其他PRC框架对比：
[](http://colobu.com/2016/09/05/benchmarks-of-popular-rpc-frameworks/)
[RPC框架性能基本比较测试](http://www.useopen.net/blog/2015/rpc-performance.html)

## 源代码分析：
[Thrift源码剖析](http://yanyiwu.com/work/2014/10/17/thrift-source-code-illustration.html)
[Thrift源码分析](https://www.kancloud.cn/digest/thrift/118984)