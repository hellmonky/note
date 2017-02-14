<!-- TOC -->

- [关于跨语言远程过程调用框架（RPC）——thrift的学习](#关于跨语言远程过程调用框架rpcthrift的学习)
    - [安装和官方样例测试：](#安装和官方样例测试)
        - [windows环境下的安装：](#windows环境下的安装)
        - [编译安装：](#编译安装)
        - [官方测试：](#官方测试)
    - [相同语言的服务调用测试——java：](#相同语言的服务调用测试java)
        - [编写IDL文件来描述被访问的接口：](#编写idl文件来描述被访问的接口)
        - [生成java文件：](#生成java文件)
        - [用java实现定义的接口：](#用java实现定义的接口)
        - [实现java的服务端代码：](#实现java的服务端代码)
        - [实现java的客户端代码：](#实现java的客户端代码)
    - [跨语言的服务调用测试——java服务端和C++客户端：](#跨语言的服务调用测试java服务端和c客户端)
    - [thrift基本语法学习：](#thrift基本语法学习)
    - [使用thrift做JDBC开发：](#使用thrift做jdbc开发)
    - [与其他PRC框架对比：](#与其他prc框架对比)

<!-- /TOC -->

# 关于跨语言远程过程调用框架（RPC）——thrift的学习

因为最近项目需要将已经有的java服务提供给其他开发人员使用，那么跨语言的过程调用就显得非常重要，facebook开源了他的RPC框架thrift能够很好的解决不同语言之间的过程调用问题，现在学习记录如下。

## 安装和官方样例测试：

### windows环境下的安装：
访问官方网站的[下载链接](http://thrift.apache.org/download)，可以直接获windows下编译好的可执行文件。
新建目录，C:\Program Files\thrift，把下载好的thrift-0.10.0.exe文件放在里面，然后修改文件名为thrift.exe，然后把C:\Program Files\thrift添加到windows下面的环境变量PATH中。
在cmd命令行中输入：
```shell
thrift -version
```
可以看到当前thrift的版本信息，表示可以使用thrift了，安装成功。

### 编译安装：
可以参考[官方编译文档](http://thrift.apache.org/docs/install/)查看具体每一个平台上的编译和安装。

### 官方测试：
从官方网站的下载页，获取thrift的源代码包，目前最近为：thrift-0.10.0.tar.gz。解压后进入tutorial目录。
这个目录中包含官方提供的shared.thrift和tutorial.thrift这两个IDL（Interface Definition Language）文件所描述服务的被支持的各种语言的测试文件，可以选择我们需要进行测试的语言进行测试。
所谓的IDL，就是接口定义，thrift通过自己定义的语法来对接口进行描述，从而使用这种DSL来完成对不同程序开发语言接口实现的转换。
拷贝官方的IDL文件到一个空目录，然后执行：
```shell
thrift -r --gen cpp tutorial.thrift
```
会生成gen-java文件夹，里面包含这两个IDL文件同名的文件夹来包含对应的C++的接口实现，我们需要做的就是补全这个接口实现中的客户端和服务端。

命令的一般格式为：
```shell
thrift --gen <language> <Thrift filename>
```

## 相同语言的服务调用测试——java：
在了解了上述基本流程后，我们实际编写一个供java客户端调用的java服务端。
使用thrift定义的IDL作为中间接口，完成实现这个接口的java服务端，然后提供调用这个接口的java客户端。

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

### 生成java文件：
使用命令：
```shell
thrift --gen java hello.thrift
```
会在当前目录下生成：..\gen-java\service\demo\Hello.java 这个文件。正好对应于namespace中设置的包路径：service.demo。
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
需要注意的是，其中的task创建了一个完整的可执行的服务端，否则会因为库的不完整报JNI错误。然后使用：
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
在完成了上述满足接口规范的java后端服务器开发后，就可以


## 跨语言的服务调用测试——java服务端和C++客户端：
完成了上述相同语言的RPC调用后，可以思考一下跨语言的服务调用过程。既然是语言无关的，那么最好的方式就是通过web进行交互，因为web本身的设计就考虑到了多语言多运行环境的交互。

在实际工程开发中，往往是已经存在一个后台服务，需要面向其他语言开发者提供服务的访问，常见的方式就是通过JSON作为数据，然后使用HTTP或者Socket作为访问方式来进行语言无关的服务提供。之前就是使用RESTFul API作为服务，C++端使用curl访问HTTP进行交互。
但是有时候也需要首先定义交互接口，然后客户端和服务端同步开发来完成敏捷开发迭代。这个时候就需要首先确定接口规范，然后各自进行开发，使用thrift是一个不错的选择。


这儿用java作为后台服务端，供C++客户端访问服务。


## thrift基本语法学习：


## 使用thrift做JDBC开发：


## 与其他PRC框架对比：