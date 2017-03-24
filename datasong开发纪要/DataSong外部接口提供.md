<!-- TOC -->

- [DataSong外部调用接口测试：](#datasong外部调用接口测试)
    - [测试部分接口：](#测试部分接口)
        - [协商外部交互的调用接口：](#协商外部交互的调用接口)
        - [编写thrift的IDL文件：](#编写thrift的idl文件)
        - [从IDL文件生成对应语言的代码：](#从idl文件生成对应语言的代码)
        - [java服务端代码的合并：](#java服务端代码的合并)
        - [C++客户端编译环境搭建：](#c客户端编译环境搭建)
            - [vs2017环境下的thrift开发：](#vs2017环境下的thrift开发)
                - [编译Boost：](#编译boost)
                - [编译openssl：](#编译openssl)
        - [C++客户端代码的编写：](#c客户端代码的编写)
        - [datasong服务端补充：](#datasong服务端补充)
        - [中文乱码的处理：](#中文乱码的处理)
    - [实际工程应用：](#实际工程应用)

<!-- /TOC -->


# DataSong外部调用接口测试：

根据之前thrift的相关学习到的知识，将之前通过RestFullAPI方式提供的DataSong外部调用接口进行封装，替换之前自己编写的socket服务包含的所有接口。

## 测试部分接口：
这部分和上次的内容类似，是使用java作为服务端，然后提供C++的客户端访问。整个过程还是一样的，唯一不同的就是和已经存在的代码进行合并。
整个过程需要的步骤为：
1 协商外部接口要求，形成文档；
2 根据文档编写thrift的IDL文件，描述所有的接口规范；
3 通过IDL文件生成java和C++端代码文件；
4 在java服务端对生成的java代码进行合并，并且实现接口规范中的函数（通过继承接口，重载实现其中定义的空函数完成，需要调用当前已有代码中的相关业务逻辑代码）；
5 编写C++端调用代码，合并生成的C++代码，完成测试；

### 协商外部交互的调用接口：
根据当前java服务端提供的RestFullAPI和用户讨论协商应该提供的服务内容，从而规划整个服务的调用接口范围，最终明确具体的调用参数和返回值类型。
最后，和用户协商完毕后形成外部调用的接口文档，作为后续开发的基本文件进行参考。

### 编写thrift的IDL文件：
```thrift
/*
 * DataSong requestor interface defination
 * version 1.0
 */

// 使用和java中相同的命名空间用于隔离
namespace java com.iscas.datasong.service.thriftService
namespace cpp datasong


// 查询操作返回类型：
struct GetDataResponse{
	1: required i32 status,
	2: required string DBName,
	3: required string tableName,
	4: required string info,
	5: required string dataId,
	6: required i64 version,
	7: required string source
}

// 删除操作返回类型：
struct DeleteDataResponse{
	1: required i32 status,
	2: required string DBName,
	3: required string tableName,
	4: required string info,
	5: required string dataId,
	6: required string version
}

// PUT操作返回类型：
struct PutDataResponse{
	1: required i32 status,
	2: required string DBName,
	3: required string tableName,
	4: required string info,
	5: required string dataId,
	6: required string version
}

// 全文检索返回类型：
struct DataItem{
	1: required string dbName,
	2: required string tableName,
	3: required string id,
	4: required double score,
	5: required i64 version,
	6: required string source
}

// POST表单提交返回类型：
struct PostDataResponse{
	1: required i32 status,
	2: required string DBName,
	3: required string tableName,
	4: required string info,
	5: required i64 tookInMillis,
	6: required i64 totalHit,
	7: required list<DataItem> items
}

// 批量操作返回类型：
struct BatchPutDataResponse{
	1: required i32 status,
	2: required string DBName,
	3: required string tableName,
	4: required string info,
	5: required list<string> dataIds,
	6: required list<string> errorIds
}

// 测试请求结构体：
struct queryDataRequest{
	1: required string dbName,
	2: required string tableName,
	3: required string id
}

// 开始定义服务：
service requestorService{
  GetDataResponse getQueryResponseData(1: queryDataRequest thing)
}
```
然后保存为：datasong.thrift。

需要注意的是：C++代码中需要注意namespace的使用，和java中不同！！！

### 从IDL文件生成对应语言的代码：
这一步通过thrift官方提供的翻译程序完成，可以通过编写脚本方便的完成：
```bat
thrift-0.10.0.exe --gen java datasong.thrift
thrift-0.10.0.exe --gen cpp datasong.thrift
```
这样就会在当前的datasong.thrift文件夹下生成对应的gen-java和gen-cpp文件夹，里面按照IDL文件中的namespace进行安排。

### java服务端代码的合并：
将datasong的源代码进行拷贝，在这个副本中进行测试。
首先在对应的namespace下新建package，然后将生成的java代码拷贝进去。然后再新建一个package，名称为：requestorServiceImpl，将IDL中的接口函数进行实现，具体代码为：
```java
package com.iscas.datasong.service.thriftService.requestorServiceImpl;

import com.iscas.datasong.service.thriftService.GetDataResponse;
import com.iscas.datasong.service.thriftService.queryDataRequest;
import com.iscas.datasong.service.thriftService.requestorService;
import com.iscas.datasong.web.DataServiceController;
import org.apache.thrift.TException;

/**
 * Created by home on 2017/3/7.
 */
public class requestorServiceImpl implements requestorService.Iface {

    @Override
    public GetDataResponse getQueryResponseData(queryDataRequest thing) throws TException {
        // 准备参数：
        GetDataResponse returnBean = new GetDataResponse();

        // 调用内部对象
        DataServiceController controller = new DataServiceController();

        // 确定调用参数：
        String dbName = thing.getDbName();
        String tableName = thing.getTableName();
        String id = thing.getId();

        // 获取调用结果：
        com.iscas.datasong.lib.response.data.GetDataResponse result = controller.getData(dbName,tableName,id);

        /*
        returnBean.setStatus(result.getStatus());
        returnBean.setDBName(result.getDbName());
        returnBean.setTableName(result.getTableName());
        returnBean.setInfo(result.getInfo());
        returnBean.setDataId(result.getDataId());
        returnBean.setVersion(result.getVersion());
        returnBean.setSource(result.getSource());
        */


        // 转换参数：
        returnBean.setStatus(404);
        returnBean.setDBName("testDBName");
        returnBean.setTableName("testTableName");
        returnBean.setInfo("testInfo");
        returnBean.setDataId("testID");
        returnBean.setVersion(1234567);
        returnBean.setSource("testSource");

        // 返回获取结果：
        return returnBean;
    }
}
```
接下来最重要的就是修改build.gradle，添加thrift的java库支持：
```gradle
compile("org.apache.thrift:libthrift:0.10.0")
```
刷新过后就可以完成thrift的包支持了。
还需要做的就是添加thrift的服务端启动代码，这样才能生成一个持续接受socket请求的入口，在datasong的根目录下添加一个启动服务ThriftServiceStartup：
```java
package com.iscas.datasong;

import com.iscas.datasong.service.thriftService.requestorService;
import com.iscas.datasong.service.thriftService.requestorServiceImpl.requestorServiceImpl;
import org.springframework.boot.context.event.ApplicationStartedEvent;
import org.springframework.context.ApplicationListener;

import org.apache.thrift.TProcessor;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TBinaryProtocol.Factory;
import org.apache.thrift.server.TServer;
import org.apache.thrift.server.TSimpleServer;
import org.apache.thrift.transport.TServerSocket;
import org.apache.thrift.transport.TTransportException;

/**
 * Created by home on 2017/3/7.
 *
 * Thrift服务入口
 *
 */
public class ThriftServiceStartup implements ApplicationListener<ApplicationStartedEvent> {

    @Override
    public void onApplicationEvent(ApplicationStartedEvent event) {
        try {
            TServerSocket serverTransport = new TServerSocket(7911);
            Factory proFactory = new TBinaryProtocol.Factory();
            TProcessor processor = new requestorService.Processor(new requestorServiceImpl());
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
通过这个服务，就可以在datasong启动的时候开启socket端口的监听了。

### C++客户端编译环境搭建：
因为C++客户端运行在windows7和VS2013 update5环境下，所以需要在相同的环境下对thrift进行编译来支持客户端库的生成。
根据上一篇文档，我们需要安装一些库来支持thrift的编译：
https://excellmedia.dl.sourceforge.net/project/boost/boost-binaries/1.63.0/boost_1_63_0-msvc-12.0-32.exe
http://slproweb.com/download/Win32OpenSSL-1_1_0e.exe
安装完毕后就可以对thrift的源代码进行编译获取C++的支持库了。
添加内容为boost和openssl的头文件路径和库文件路径。

#### vs2017环境下的thrift开发：
##### 编译Boost：
由于boost还没有支持vs2017的选项，所以需要修改boost库运行bootstrap.bat生成的project-config.jam文件来指定cl.exe文件的路径来完成编译：
```shell
C:\Program Files (x86)\Microsoft Visual Studio\2017\Professional\VC\Tools\MSVC\14.10.25017\bin\HostX64\x64\cl.exe
C:\Program Files (x86)\Microsoft Visual Studio\2017\Professional\VC\Tools\MSVC\14.10.25017\bin\HostX86\x86\cl.exe
```
首先，打开vs的命令行，输入bootstrap.bat生成相应的文件，然后修改project-config.jam文件为：
修改结果为：
```shell
import option ; 
 
using msvc : 14.0 : "C:\Program Files (x86)\Microsoft Visual Studio\2017\Professional\VC\Tools\MSVC\14.10.25017\bin\HostX86\x86\cl.exe"; 
 
option.set keep-going : false ; 
```
保存后，然后继续在vs的命令行中执行：
```shell
b2.exe toolset=msvc-14.0
```
等待完成编译了。

参考文档：[Build boost with msvc 14.1 ( VS2017 RC)](http://stackoverflow.com/questions/41464356/build-boost-with-msvc-14-1-vs2017-rc)

##### 编译openssl：
openssl中使用了perl进行字符串处理，所以需要安装activePerl环境，并且还需要NASM作为汇编代码的编译器，所以还需要安装NASM，然后将他添加到系统环境变量中。
完成上述基本环境的安装后，就可以对源代码进行处理：
首先安装perl的package：
ppm install dmake
然后开始预处理编译：
perl Configure VC-WIN32 --prefix=D:\thrift\openssl
最后使用vs进行编译：

参考文档：[在 Windows下用 Visual Studio 编译 OpenSSL 1.1.0](http://www.cnblogs.com/chinalantian/p/5819105.html)

### C++客户端代码的编写：
删除skeleton.cpp这个文件，然后添加main.cpp文件，编写如下内容：
```c++
#include <stdio.h>
#include <string>

#include <boost/make_shared.hpp>

#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/transport/TSocket.h>
#include <thrift/transport/TTransportUtils.h>

#include "datasong_types.h"
#include "requestorService.h"

using namespace boost;
using namespace apache::thrift;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;
using namespace datasong;

int main(int argc, char** argv){
	shared_ptr<TTransport> socket(new TSocket("192.168.15.1", 7911));
	shared_ptr<TTransport> transport(new TBufferedTransport(socket));
	shared_ptr<TProtocol> protocol(new TBinaryProtocol(transport));
	requestorServiceClient client(protocol);
	try {
		transport->open();
		queryDataRequest requestBean;
		requestBean.__set_dbName("testDBName");
		requestBean.__set_tableName("testTableName");
		requestBean.__set_id("testID");
		GetDataResponse resultBean;
		client.getQueryResponseData(resultBean, requestBean);
		transport->close();
	}
	catch (TException& tx) {
		printf("ERROR:%s\n", tx.what());
	}
}
```
还是出现了之前的链接错误，无法解析的外部符号：
```shell
1>main.obj : error LNK2001: 无法解析的外部符号 "public: virtual char const * __thiscall apache::thrift::transport::TTransportException::what(void)const " (?what@TTransportException@transport@thrift@apache@@UBEPBDXZ)
1>main.obj : error LNK2019: 无法解析的外部符号 "public: virtual __thiscall apache::thrift::protocol::TProtocol::~TProtocol(void)" (??1TProtocol@protocol@thrift@apache@@UAE@XZ)，该符号在函数 "public: virtual __thiscall apache::thrift::protocol::TProtocolDefaults::~TProtocolDefaults(void)" (??1TProtocolDefaults@protocol@thrift@apache@@UAE@XZ) 中被引用
1>main.obj : error LNK2001: 无法解析的外部符号 "public: virtual unsigned int __thiscall apache::thrift::protocol::TProtocol::skip_virt(enum apache::thrift::protocol::TType)" (?skip_virt@TProtocol@protocol@thrift@apache@@UAEIW4TType@234@@Z)
1>main.obj : error LNK2019: 无法解析的外部符号 "public: __thiscall apache::thrift::transport::TSocket::TSocket(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,int)" (??0TSocket@transport@thrift@apache@@QAE@ABV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@H@Z)，该符号在函数 _main 中被引用
1>main.obj : error LNK2001: 无法解析的外部符号 "public: virtual unsigned int __thiscall apache::thrift::transport::TBufferedTransport::readSlow(unsigned char *,unsigned int)" (?readSlow@TBufferedTransport@transport@thrift@apache@@UAEIPAEI@Z)
1>main.obj : error LNK2001: 无法解析的外部符号 "public: virtual void __thiscall apache::thrift::transport::TBufferedTransport::writeSlow(unsigned char const *,unsigned int)" (?writeSlow@TBufferedTransport@transport@thrift@apache@@UAEXPBEI@Z)
1>main.obj : error LNK2001: 无法解析的外部符号 "public: virtual void __thiscall apache::thrift::transport::TBufferedTransport::flush(void)" (?flush@TBufferedTransport@transport@thrift@apache@@UAEXXZ)
1>main.obj : error LNK2001: 无法解析的外部符号 "public: virtual unsigned char const * __thiscall apache::thrift::transport::TBufferedTransport::borrowSlow(unsigned char *,unsigned int *)" (?borrowSlow@TBufferedTransport@transport@thrift@apache@@UAEPBEPAEPAI@Z)
1>requestorService.obj : error LNK2019: 无法解析的外部符号 "public: void __cdecl apache::thrift::TOutput::printf(char const *,...)" (?printf@TOutput@thrift@apache@@QAAXPBDZZ)，该符号在函数 "public: virtual bool __thiscall apache::thrift::TDispatchProcessor::process(class boost::shared_ptr<class apache::thrift::protocol::TProtocol>,class boost::shared_ptr<class apache::thrift::protocol::TProtocol>,void *)" (?process@TDispatchProcessor@thrift@apache@@UAE_NV?$shared_ptr@VTProtocol@protocol@thrift@apache@@@boost@@0PAX@Z) 中被引用
1>requestorService.obj : error LNK2019: 无法解析的外部符号 "public: __thiscall apache::thrift::async::TConcurrentSendSentry::TConcurrentSendSentry(class apache::thrift::async::TConcurrentClientSyncInfo *)" (??0TConcurrentSendSentry@async@thrift@apache@@QAE@PAVTConcurrentClientSyncInfo@123@@Z)，该符号在函数 "public: int __thiscall datasong::requestorServiceConcurrentClient::send_getQueryResponseData(class datasong::queryDataRequest const &)" (?send_getQueryResponseData@requestorServiceConcurrentClient@datasong@@QAEHABVqueryDataRequest@2@@Z) 中被引用
1>requestorService.obj : error LNK2019: 无法解析的外部符号 "public: __thiscall apache::thrift::async::TConcurrentSendSentry::~TConcurrentSendSentry(void)" (??1TConcurrentSendSentry@async@thrift@apache@@QAE@XZ)，该符号在函数 "public: int __thiscall datasong::requestorServiceConcurrentClient::send_getQueryResponseData(class datasong::queryDataRequest const &)" (?send_getQueryResponseData@requestorServiceConcurrentClient@datasong@@QAEHABVqueryDataRequest@2@@Z) 中被引用
1>requestorService.obj : error LNK2019: 无法解析的外部符号 "public: void __thiscall apache::thrift::async::TConcurrentSendSentry::commit(void)" (?commit@TConcurrentSendSentry@async@thrift@apache@@QAEXXZ)，该符号在函数 "public: int __thiscall datasong::requestorServiceConcurrentClient::send_getQueryResponseData(class datasong::queryDataRequest const &)" (?send_getQueryResponseData@requestorServiceConcurrentClient@datasong@@QAEHABVqueryDataRequest@2@@Z) 中被引用
1>requestorService.obj : error LNK2019: 无法解析的外部符号 "public: __thiscall apache::thrift::async::TConcurrentRecvSentry::TConcurrentRecvSentry(class apache::thrift::async::TConcurrentClientSyncInfo *,int)" (??0TConcurrentRecvSentry@async@thrift@apache@@QAE@PAVTConcurrentClientSyncInfo@123@H@Z)，该符号在函数 "public: void __thiscall datasong::requestorServiceConcurrentClient::recv_getQueryResponseData(class datasong::GetDataResponse &,int)" (?recv_getQueryResponseData@requestorServiceConcurrentClient@datasong@@QAEXAAVGetDataResponse@2@H@Z) 中被引用
1>requestorService.obj : error LNK2019: 无法解析的外部符号 "public: __thiscall apache::thrift::async::TConcurrentRecvSentry::~TConcurrentRecvSentry(void)" (??1TConcurrentRecvSentry@async@thrift@apache@@QAE@XZ)，该符号在函数 "public: void __thiscall datasong::requestorServiceConcurrentClient::recv_getQueryResponseData(class datasong::GetDataResponse &,int)" (?recv_getQueryResponseData@requestorServiceConcurrentClient@datasong@@QAEXAAVGetDataResponse@2@H@Z) 中被引用
1>requestorService.obj : error LNK2019: 无法解析的外部符号 "public: void __thiscall apache::thrift::async::TConcurrentRecvSentry::commit(void)" (?commit@TConcurrentRecvSentry@async@thrift@apache@@QAEXXZ)，该符号在函数 "public: void __thiscall datasong::requestorServiceConcurrentClient::recv_getQueryResponseData(class datasong::GetDataResponse &,int)" (?recv_getQueryResponseData@requestorServiceConcurrentClient@datasong@@QAEXAAVGetDataResponse@2@H@Z) 中被引用
1>requestorService.obj : error LNK2019: 无法解析的外部符号 "public: int __thiscall apache::thrift::async::TConcurrentClientSyncInfo::generateSeqId(void)" (?generateSeqId@TConcurrentClientSyncInfo@async@thrift@apache@@QAEHXZ)，该符号在函数 "public: int __thiscall datasong::requestorServiceConcurrentClient::send_getQueryResponseData(class datasong::queryDataRequest const &)" (?send_getQueryResponseData@requestorServiceConcurrentClient@datasong@@QAEHABVqueryDataRequest@2@@Z) 中被引用
1>requestorService.obj : error LNK2019: 无法解析的外部符号 "public: bool __thiscall apache::thrift::async::TConcurrentClientSyncInfo::getPending(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > &,enum apache::thrift::protocol::TMessageType &,int &)" (?getPending@TConcurrentClientSyncInfo@async@thrift@apache@@QAE_NAAV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@AAW4TMessageType@protocol@34@AAH@Z)，该符号在函数 "public: void __thiscall datasong::requestorServiceConcurrentClient::recv_getQueryResponseData(class datasong::GetDataResponse &,int)" (?recv_getQueryResponseData@requestorServiceConcurrentClient@datasong@@QAEXAAVGetDataResponse@2@H@Z) 中被引用
1>requestorService.obj : error LNK2019: 无法解析的外部符号 "public: void __thiscall apache::thrift::async::TConcurrentClientSyncInfo::updatePending(class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,enum apache::thrift::protocol::TMessageType,int)" (?updatePending@TConcurrentClientSyncInfo@async@thrift@apache@@QAEXABV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@W4TMessageType@protocol@34@H@Z)，该符号在函数 "public: void __thiscall datasong::requestorServiceConcurrentClient::recv_getQueryResponseData(class datasong::GetDataResponse &,int)" (?recv_getQueryResponseData@requestorServiceConcurrentClient@datasong@@QAEXAAVGetDataResponse@2@H@Z) 中被引用
1>requestorService.obj : error LNK2019: 无法解析的外部符号 "public: void __thiscall apache::thrift::async::TConcurrentClientSyncInfo::waitForWork(int)" (?waitForWork@TConcurrentClientSyncInfo@async@thrift@apache@@QAEXH@Z)，该符号在函数 "public: void __thiscall datasong::requestorServiceConcurrentClient::recv_getQueryResponseData(class datasong::GetDataResponse &,int)" (?recv_getQueryResponseData@requestorServiceConcurrentClient@datasong@@QAEXAAVGetDataResponse@2@H@Z) 中被引用
1>requestorService.obj : error LNK2019: 无法解析的外部符号 "public: unsigned int __thiscall apache::thrift::TApplicationException::read(class apache::thrift::protocol::TProtocol *)" (?read@TApplicationException@thrift@apache@@QAEIPAVTProtocol@protocol@23@@Z)，该符号在函数 "public: void __thiscall datasong::requestorServiceClient::recv_getQueryResponseData(class datasong::GetDataResponse &)" (?recv_getQueryResponseData@requestorServiceClient@datasong@@QAEXAAVGetDataResponse@2@@Z) 中被引用
1>requestorService.obj : error LNK2019: 无法解析的外部符号 "public: unsigned int __thiscall apache::thrift::TApplicationException::write(class apache::thrift::protocol::TProtocol *)const " (?write@TApplicationException@thrift@apache@@QBEIPAVTProtocol@protocol@23@@Z)，该符号在函数 "protected: virtual bool __thiscall datasong::requestorServiceProcessor::dispatchCall(class apache::thrift::protocol::TProtocol *,class apache::thrift::protocol::TProtocol *,class std::basic_string<char,struct std::char_traits<char>,class std::allocator<char> > const &,int,void *)" (?dispatchCall@requestorServiceProcessor@datasong@@MAE_NPAVTProtocol@protocol@thrift@apache@@0ABV?$basic_string@DU?$char_traits@D@std@@V?$allocator@D@2@@std@@HPAX@Z) 中被引用
1>requestorService.obj : error LNK2001: 无法解析的外部符号 "class apache::thrift::TOutput apache::thrift::GlobalOutput" (?GlobalOutput@thrift@apache@@3VTOutput@12@A)
1>C:\workspace\datasongClient\Debug\datasongClient.exe : fatal error LNK1120: 22 个无法解析的外部命令
========== 全部重新生成:  成功 0 个，失败 1 个，跳过 0 个 ==========
```

综上所述，windows下的thrift客户端一直都是一个问题，目前猜测应该是调用方法的问题，需要thrift的异步库。

从stackoverflow上找到了相关的解决方法：[linking with openssl lib statically](http://stackoverflow.com/questions/37522654/linking-with-openssl-lib-statically)
需要添加windows的crypt32库链接就可以了。

修改后的代码为：
```c++
#include <stdio.h>
#include <string>
#include <iostream>

#include <boost/make_shared.hpp>

#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/transport/TSocket.h>
#include <thrift/transport/TTransportUtils.h>

#include "datasong_types.h"
#include "requestorService.h"

#pragma comment (lib, "crypt32")

using namespace boost;
using namespace apache::thrift;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;
using namespace datasong;

int main(int argc, char** argv){
	shared_ptr<TTransport> socket(new TSocket("192.168.184.1", 7911));
	shared_ptr<TTransport> transport(new TBufferedTransport(socket));
	shared_ptr<TProtocol> protocol(new TBinaryProtocol(transport));
	requestorServiceClient client(protocol);
	try {
		transport->open();
		queryDataRequest requestBean;
		requestBean.__set_dbName("testDBName");
		requestBean.__set_tableName("testTableName");
		requestBean.__set_id("testID");
		GetDataResponse resultBean;
		client.getQueryResponseData(resultBean, requestBean);
		std::cout << resultBean.info << std::endl;
		transport->close();
	}
	catch (TException& tx) {
		printf("ERROR:%s\n", tx.what());
	}
}
```

### datasong服务端补充：
上面java的服务端会启动一个阻塞线程，所以需要使用datasong中的线程池来完成服务的后台启动，修改后的ThriftServiceStartup.java代码为：
```java
package com.iscas.datasong;

import com.iscas.datasong.service.threadPoolService.ThreadPoolContainer;
import com.iscas.datasong.service.thriftService.requestorService;
import com.iscas.datasong.service.thriftService.requestorServiceImpl.requestorServiceImpl;
import org.springframework.boot.context.event.ApplicationStartedEvent;
import org.springframework.context.ApplicationListener;

import org.apache.thrift.TProcessor;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TBinaryProtocol.Factory;
import org.apache.thrift.server.TServer;
import org.apache.thrift.server.TSimpleServer;
import org.apache.thrift.transport.TServerSocket;
import org.apache.thrift.transport.TTransportException;

/**
 * Created by home on 2017/3/7.
 *
 * Thrift服务入口
 *
 */
public class ThriftServiceStartup implements ApplicationListener<ApplicationStartedEvent> {

    // thrift服务端入口：
    public void thriftServerEntery(){
        try {
            TServerSocket serverTransport = new TServerSocket(7911);
            //Factory proFactory = new TBinaryProtocol.Factory();
            TProcessor processor = new requestorService.Processor(new requestorServiceImpl());
            TServer server = new TSimpleServer(new TServer.Args(serverTransport).processor(processor));
            //TServer server = new TThreadPoolServer(processor, serverTransport,  proFactory);
            System.out.println("Start server on port 7911...");
            // 这儿会挂起来，导致后续进程失败，需要使用线程池来单开
            server.serve();
        } catch (TTransportException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void onApplicationEvent(ApplicationStartedEvent event) {
        ThreadPoolContainer globalThreadPool = ThreadPoolContainer.getInstance();
        // 添加到线程池中运行：
        Thread thriftMainThread = new Thread(){
            public void run() {
                thriftServerEntery();
            }
        };
        globalThreadPool.execute(thriftMainThread);
    }
}
```
然后就可以正常运行了，搞定。

### 中文乱码的处理：
因为vs中出现的中文都会按照GBK进行编码，所以需要对GBK的部分按照UTF编码处理，然后进行发送，那么可以在客户端发送数据之前进行格式转换，修改后的main.cpp内容为：
```c++
#include <stdio.h>
#include <string>
#include <iostream>

#include <boost/make_shared.hpp>

#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/transport/TSocket.h>
#include <thrift/transport/TTransportUtils.h>

#include "datasong_types.h"
#include "requestorService.h"

#pragma comment (lib, "crypt32")

using namespace boost;
using namespace apache::thrift;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;
using namespace datasong;

std::string GBKToUTF8(const std::string & strGBK) {
	std::string strOutUTF8 = "";
	WCHAR * str1;
	int n = MultiByteToWideChar(CP_ACP, 0, strGBK.c_str(), -1, NULL, 0);
	str1 = new WCHAR[n];
	MultiByteToWideChar(CP_ACP, 0, strGBK.c_str(), -1, str1, n);
	n = WideCharToMultiByte(CP_UTF8, 0, str1, -1, NULL, 0, NULL, NULL);
	char * str2 = new char[n];
	WideCharToMultiByte(CP_UTF8, 0, str1, -1, str2, n, NULL, NULL);
	strOutUTF8 = str2;
	delete[]str1;
	str1 = NULL;
	delete[]str2;
	str2 = NULL;
	return strOutUTF8;
}

std::string UTF8ToGBK(const std::string & strUTF8) {
	int len = MultiByteToWideChar(CP_UTF8, 0, strUTF8.c_str(), -1, NULL, 0);
	wchar_t* wszGBK = new wchar_t[len + 1];
	memset(wszGBK, 0, len * 2 + 2);
	MultiByteToWideChar(CP_UTF8, 0, strUTF8.c_str(), -1, wszGBK, len);
	len = WideCharToMultiByte(CP_ACP, 0, wszGBK, -1, NULL, 0, NULL, NULL);
	char* szGBK = new char[len + 1];
	memset(szGBK, 0, len + 1);
	WideCharToMultiByte(CP_ACP, 0, wszGBK, -1, szGBK, len, NULL, NULL);
	std::string strTemp(szGBK);
	if (wszGBK) delete[] wszGBK;
	if (szGBK) delete[] szGBK;
	return strTemp;
}

int main(int argc, char** argv){
	shared_ptr<TTransport> socket(new TSocket("192.168.184.1", 7911));
	shared_ptr<TTransport> transport(new TBufferedTransport(socket));
	shared_ptr<TProtocol> protocol(new TBinaryProtocol(transport));
	requestorServiceClient client(protocol);
	try {
		transport->open();
		queryDataRequest requestBean;
		std::string dbStr = GBKToUTF8("test数据库Name");
		std::string tableStr = GBKToUTF8("test数据表Name");
		std::string dataStr = GBKToUTF8("test唯一标示");
		requestBean.__set_dbName(dbStr);
		requestBean.__set_tableName(tableStr);
		requestBean.__set_id(dataStr);
		GetDataResponse resultBean;
		client.getQueryResponseData(resultBean, requestBean);
		std::cout << resultBean.info << std::endl;
		transport->close();
	}
	catch (TException& tx) {
		printf("ERROR:%s\n", tx.what());
	}
}
```
还可以通过修改thrift的源代码来完成底层的自动转换，主要修改对象为：..\thrift-0.10.0\lib\cpp\src\thrift\protocol\TBinaryProtocol.tcc 这个文件，将其中readString和writeString函数中添加从GBK编码到UTF8编码的转换函数，涉及到的内容为：
```c++
// add function start
std::string mb_to_utf8(const std::string & strGBK) {
	std::string strOutUTF8 = "";
	WCHAR * str1;
	int n = MultiByteToWideChar(CP_ACP, 0, strGBK.c_str(), -1, NULL, 0);
	str1 = new WCHAR[n];
	MultiByteToWideChar(CP_ACP, 0, strGBK.c_str(), -1, str1, n);
	n = WideCharToMultiByte(CP_UTF8, 0, str1, -1, NULL, 0, NULL, NULL);
	char * str2 = new char[n];
	WideCharToMultiByte(CP_UTF8, 0, str1, -1, str2, n, NULL, NULL);
	strOutUTF8 = str2;
	delete[]str1;
	str1 = NULL;
	delete[]str2;
	str2 = NULL;
	return strOutUTF8;
}

std::string utf8_to_mb(const std::string & strUTF8) {
	int len = MultiByteToWideChar(CP_UTF8, 0, strUTF8.c_str(), -1, NULL, 0);
	wchar_t* wszGBK = new wchar_t[len + 1];
	memset(wszGBK, 0, len * 2 + 2);
	MultiByteToWideChar(CP_UTF8, 0, strUTF8.c_str(), -1, wszGBK, len);
	len = WideCharToMultiByte(CP_ACP, 0, wszGBK, -1, NULL, 0, NULL, NULL);
	char* szGBK = new char[len + 1];
	memset(szGBK, 0, len + 1);
	WideCharToMultiByte(CP_ACP, 0, wszGBK, -1, szGBK, len, NULL, NULL);
	std::string strTemp(szGBK);
	if (wszGBK) delete[] wszGBK;
	if (szGBK) delete[] szGBK;
	return strTemp;
}
// add function end

template <class Transport_, class ByteOrder_>
template <typename StrType>
uint32_t TBinaryProtocolT<Transport_, ByteOrder_>::writeString(const StrType& str) {

  // modified content start
  StrType theStr = mb_to_utf8(str);
  
  if (theStr.size() > static_cast<size_t>((std::numeric_limits<int32_t>::max)()))
    throw TProtocolException(TProtocolException::SIZE_LIMIT);
  uint32_t size = static_cast<uint32_t>(theStr.size());
  uint32_t result = writeI32((int32_t)size);
  if (size > 0) {
    this->trans_->write((uint8_t*)theStr.data(), size);
  }
  // modified content end
  
  return result + size;
}

template <class Transport_, class ByteOrder_>
template <typename StrType>
uint32_t TBinaryProtocolT<Transport_, ByteOrder_>::readString(StrType& str) {
  uint32_t result;
  int32_t size;
  result = readI32(size);
  
  // modified content start
  //return result + readStringBody(str, size);
  result += result + readStringBody(str, size);
  str = utf8_to_mb(str);
  return result;
  // modified content end
}
```
重新编译thrift的cpp库文件之后就可以正常的使用中文进行传输了。

## 实际工程应用：
经过上述测试用例，基本满足了当前工程开发的需求，所以这部分将记录当前工程应用的具体过程。


虚拟环境中使用vs2013编译boost1.63、openSSL和thrift：
在vs2013命令行中执行：
bootstrap.bat
然后使用生成的bjam进行编译：
bjam.exe stage --toolset=msvc-12.0 link=static runtime-link=shared threading=multi debug release
参考：[vs2013编译boost1.55.0 32/64位](http://www.cnblogs.com/run220/p/3551134.html)
继续编译openssl：
安装ActivePerl和NASM，然后将NASM添加到系统环境变量中，进入vs的命令行窗口，使用ppm安装perl的组件：
ppm install dmake
然后开始使用perl预处理：
perl configure VC-WIN32
修改makefile和configdata.pm文件，与CRT静态绑定，避免了在目标机器上安装 VC 再发行包等等操作：
搜索 “/MD” 字符串， 替换成 “/MT”
然后使用nmake开始编译：
nmake
nmake test
nmake install
完成上述步骤之后，就可以对thrift的C++客户端进行编译了。进入thrift\lib\cpp文件夹下，使用vs打开sln工程，添加boost和openSSL的头文件路径和静态库文件路径：

在vs2010中编译64位的环境：
（1）boost：
在vs2010命令行中执行：
bootstrap.bat
然后使用生成的bjam进行编译：
bjam.exe stage --toolset=msvc-10.0 link=static runtime-link=shared address-model=64 threading=multi debug release
（2）openssl：
进入vs的命令行窗口，使用ppm安装perl的组件：
ppm install dmake
然后开始使用perl预处理：
perl configure VC-WIN64A --prefix=C:/workspace/thrift/openssl_install
修改makefile和configdata.pm文件，与CRT静态绑定，避免了在目标机器上安装 VC 再发行包等等操作：
搜索 “/MD” 字符串， 替换成 “/MT”
然后使用nmake开始编译：
nmake
nmake test
nmake install
（3）thrift：
打开../thrift-1.0.0/lib/cpp/文件夹下的sln工程文件，然后选择为64位编译环境，添加相关的库和头文件，完成编译。