# C++调用RESTFul API实现功能

在第二次浪潮（服务的网络化）中，为了结合本地处理的性能和外部服务的广泛性，采用C++调用web service是一个非常不错的选择，本文就目前环境下从跨平台的角度来考虑C++调用java实现的RESTFul风格的web service做出一个尝试。

## 一 C++调用RESTFul风格的web service的步骤：
web service是基于http协议的访问接口，所以C++需要一个http协议支持的库来作为web service调用的基础，并且web service接口访问中处理的数据是Json格式的，所以C++还需要一个用来转换Json格式数据的库来完成数据转换，那么根据上述的分析，完整的交互流程为：
> C++应用本地处理模块 <=======> C++数据转换模块 <=======> C++网络访问模块 <=======> RESTFul接口


## 二 C++框架和库的选型：
在完成上述步骤梳理之后，还需要考虑跨平台问题，编写底层系统无关的程序来增强该软件的适用性在当前的工程项目中非常重要。
而跨平台特性也会影响到第三方库的选择，所以按照上述步骤，对每个阶段可选的库进行列表分析。

### 1 C++数据转换模块第三方库：
主要完成C++数据和Json格式数据的转换，供外部进行调用来处理。

#### 1.1 JSON转换相关：
* [Boost.PropertyTree](http://www.boost.org/doc/libs/1_55_0/doc/html/property_tree.html) - A property tree parser/generator that can be used to parse XML/JSON/INI/Info files. [Boost]
* [frozen](https://github.com/cesanta/frozen) - JSON parser and generator for C/C++. [GPL & GPL2]
* [Jansson](https://github.com/akheron/jansson) - C library for encoding, decoding and manipulating JSON data. [MIT]
* [jbson](https://github.com/chrismanning/jbson) - jbson is a library for building & iterating BSON data, and JSON documents in C++14. [Boost]
* [JeayeSON](https://github.com/jeaye/jeayeson) - A very sane (header only) C++ JSON library. [BSD]
* [json](https://github.com/nlohmann/json) :zap: - JSON for Modern C++. [MIT]
* [JSON++](https://github.com/hjiang/jsonxx) - A JSON parser in C++. [MIT]
* [JsonCpp](https://github.com/open-source-parsers/jsoncpp) - A C++ library for interacting with JSON. [MIT]
* [json-parser](https://github.com/udp/json-parser) - Very low footprint JSON parser written in portable ANSI C. [BSD]
* [json11](https://github.com/dropbox/json11) - A tiny JSON library for C++11. [MIT]
* [json-voorhees](https://github.com/tgockel/json-voorhees) - JSON library for C++. Support for C++11. No dependencies, fast and dev-friendly. [Apache2]
* [jute](https://github.com/amir-s/jute) - Very simple C++ JSON Parser. [PublicDomain]
* [libjson](https://github.com/vincenthz/libjson) - A JSON parser and printer library in C. easy to integrate with any model. [LGPL]
* [libjson](http://sourceforge.net/projects/libjson/) - Lightweight JSON library. [?]
* [LIBUCL](https://github.com/vstakhov/libucl) :zap: - Universal configuration library parser [?]
* [PicoJSON](https://github.com/kazuho/picojson) - A header-file-only, JSON parser serializer in C++. [BSD]
* [qt-json](https://github.com/gaudecker/qt-json) - A simple class for parsing JSON data into a QVariant hierarchy and vice versa. [GPLv3]
* [QJson](https://github.com/flavio/qjson) - A qt-based library that maps JSON data to QVariant objects. [LGPL2]
* [RapidJSON](https://github.com/miloyip/rapidjson) :zap: - A fast JSON parser/generator for C++ with both SAX/DOM style API. [MIT]
* [ujson](https://bitbucket.org/awangk/ujson) - µjson is a a small, C++11, UTF-8, JSON library. [MIT]
* [YAJL](https://github.com/lloyd/yajl) - A fast streaming JSON parsing library in C. [ISC]

> 库的选型和对比：
首选RapidJSON进行JSON格式的解析和转换，备选libjson。

主要参考文档有：
[libjson文档翻译](http://yaocoder.blog.51cto.com/2668309/1154617)



#### 1.2 XML转换相关：
* [Boost.PropertyTree](https://www.boost.org/doc/libs/1_55_0/doc/html/property_tree.html): A property tree parser/generator that can be used to parse XML/JSON/INI/Info files. [Boost]
* [Expat](https://www.libexpat.org/): An XML parser library written in C. [MIT]
* [Libxml2](https://xmlsoft.org/): The XML C parser and toolkit of Gnome. [MIT]
* [libxml++](https://libxmlplusplus.sourceforge.net/): An XML Parser for C++. [LGPL2]
* [PugiXML](https://pugixml.org/): A light-weight, simple and fast XML parser for C++ with XPath support. [MIT]
* [rapidxml](http://rapidxml.sourceforge.net/): An attempt to create the fastest XML parser possible, while retaining useability, portability and reasonable W3C compatibility. [Boost]
* [TinyXML](https://sourceforge.net/projects/tinyxml/): A simple, small, minimal, C++ XML parser that can be easily integrating into other programs. [zlib] 
* [TinyXML2](https://github.com/leethomason/tinyxml2): A simple, small, efficient, C++ XML parser that can be easily integrating into other programs. [zlib]
* [TinyXML++](https://code.google.com/p/ticpp/): A completely new interface to TinyXML that uses MANY of the C++ strengths. Templates, exceptions, and much better error handling. [MIT]
* [Xerces-C++](https://xerces.apache.org/xerces-c/): A validating XML parser written in a portable subset of C++. [Apache2]

> 库的选型和对比：
建议使用rapidxml或者tinyXML进行xml文件的转换和处理。

主要参考文档有：
[TinyXML：一个优秀的C++ XML解析器](http://www.cnblogs.com/phinecos/archive/2008/03/11/1100912.html)
[]()


### 2 C++网络访问模块第三方库：
C++的网络模块有很多种，本节聚焦于http协议的网络模块。因为web service是基于http协议的，所以只要能够进行http协议访问，就可以实现对restful的接口的访问。

#### 2.1 网络访问相关：
* [ACE](http://www.cs.wustl.edu/~schmidt/ACE.html) - An OO Network Programming Toolkit in C++. [?MIT?]
* [Boost.Asio](http://think-async.com/) :zap: - A cross-platform C++ library for network and low-level I/O programming. [Boost]
* [C++ REST SDK](https://github.com/Microsoft/cpprestsdk) - C++ REST SDK (previously named Casablanca). [Apache2]
* [Restbed](https://github.com/corvusoft/restbed) - C++11 Asynchronous RESTful framework. [AGPL]
* [cpp-netlib](http://cpp-netlib.org/) - A collection of open-source libraries for high level network programming. [Boost]
* [cpr](https://github.com/whoshuu/cpr) - A modern C++ HTTP requests library with a simple but powerful interface. Modeled after the Python Requests module. [MIT] [website](https://whoshuu.github.io/cpr/)
* [Dyad.c](https://github.com/rxi/dyad) - Asynchronous networking for C. [MIT]
* [libcurl](http://curl.haxx.se/libcurl/) - Multiprotocol file transfer library. [MIT/X derivate license]
* [libjingle](https://code.google.com/p/libjingle/) - Google talk voice and P2P interoperability library. [BSD]
* [Mongoose](https://github.com/cesanta/mongoose) - Extremely lightweight webserver. [GPL2]
* [Muduo](https://github.com/chenshuo/muduo) - A C++ non-blocking network library for multi-threaded server in Linux. [BSD]
* [net_skeleton](https://github.com/cesanta/fossa) - TCP client/server library for C/C++. [GPL2]
* [nope.c](https://github.com/riolet/nope.c) - A C language-based ultra-light software platform for scalable server-side and networking applications. Think node.js for C programmers. [GPL2]
* [Onion](https://github.com/davidmoreno/onion) - HTTP server library in C designed to be lightweight and easy to use. [Apache2/GPL2]
* [POCO](https://github.com/pocoproject) :zap: - C++ class libraries and frameworks for building network- and internet-based applications that run on desktop, server, mobile and embedded systems. [Boost] [website](http://pocoproject.org/)
* [Proxygen](https://github.com/facebook/proxygen) - Facebook's collection of C++ HTTP libraries including an easy to use HTTP server. [BSD]
* [RakNet](https://github.com/OculusVR/RakNet) - A cross platform, open source, C++ networking engine for game programmers. [BSD]
* [Silicon](http://siliconframework.org) - A high performance, middleware oriented C++14 http web framework. [MIT]
* [Tufão](https://github.com/vinipsmaker/tufao) - An asynchronous web framework for C++ built on top of Qt. [LGPL2]
* [WebSocket++](https://github.com/zaphoyd/websocketpp) - C++/Boost Asio based websocket client/server library. [BSD]

> 库的选型和对比：
建议使用libcurl进行http访问的开发。或者使用POCO来进行http访问实现对restfulAPI的访问。

主要参考文档有：
> - [REST实战——调用百度语音的云服务](http://www.voidcn.com/blog/u011000290/article/p-5782799.html)
> - [Call Rest Web Services from C++](http://itcompiles.blogspot.jp/2013/09/call-rest-web-services-from-c.html)

*Rest webservice are based on http all we have to do is make http requests. *


#### 2.2 完整的Web Services相关：
> - [gsoap2](http://sourceforge.net/projects/gsoap2): Development toolkit for Web Services and XML data bindings for C & C++
> - [cpprestsdk](https://github.com/Microsoft/cpprestsdk): The C++ REST SDK is a Microsoft project for cloud-based client-server communication in native code using a modern asynchronous C++ API design. This project aims to help C++ developers connect to and interact with services.

库的选型和对比：
初步选取cpprestsdk进行RESTFul接口的访问，但是这个库由于过于重量级，暂时不予考虑。

> 参考文档：
> - [gsoap2官方文档](https://www.genivia.com/dev.html)
> - [c/c++使用gsoap发布和调用webservice](http://blog.csdn.net/wenzi49312/article/details/8963345)
> - [使用微软的 C++ REST SDK](http://blog.jobbole.com/53642/)
> - [cpprestsdk官方示例代码](https://github.com/Microsoft/cpprestsdk/wiki/Samples)


## 三 环境设置：
在上述基本步骤和第三方库选型完毕之后就需要尝试在windows和linux系统下进行实际的测试和开发，本节关注与工程实践的部署环节和开发环节。
个人现在从工程实践的角度来进行考虑，还是希望开发越简单，部署越容易好，从这个角度出发，偏向于使用功能完善的http库来提升开发效率。偏向于使用http库进行开发。

### 1 基于libcurl的web service访问：
这一节尝试基于jsoncpp库和libcurl库完成开发。其中JSON库可以使用其他C库进行替换。

#### 1.1 windows下编译安装需要的库：
基础编译环境：
vs2015 update 3
cmake-3.7.0-rc1-win64-x64.msi
需要下载的库版本：
- [openssl-1.1.0b.tar.gz](http://www.openssl.org/source/openssl-1.1.0b.tar.gz)
- [zlib-1.2.8.tar.gz](http://zlib.net/zlib-1.2.8.tar.gz)
- [curl-7.50.3.zip](https://github.com/curl/curl/archive/curl-7_50_3.zip)
- [jsoncpp-1.7.7.zip](https://codeload.github.com/open-source-parsers/jsoncpp/zip/1.7.7)

#### 1.2 编译基础库：
##### （1）编译zlib：
解压zlib-1.2.8.tar.gz到文件夹中，然后使用cmake默认生成vs工程文件。进入后设置安装路径就可以正常编译。因为当前的web service没有使用https协议，而且openssl需要perl进行编译，故本文没有对openssl进行编译。
##### （2）编译curl：
解压curl-7.50.3.zip到文件夹中，然后使用cmake生成，打开CURL_ZLIB选项，然后点击Add Entry，将zlib生成的文件指定：
```shell
ZLIB_INCLUDE_DIR		X:/XXXXX/zlib/include/
ZLIB_LIBRARIES			X:/XXXXX/zlib/lib
```
然后点击生成完成配置，使用vs进行编译安装。
##### （3）编译jsoncpp：
解压jsoncpp-1.7.7.zip到文件夹中，然后使用cmake默认生成，使用vs编译即可。

> 参考文档：
> - [在Windows上编译最新的CURL，含有zlib，openssl](http://blog.csdn.net/hujkay/article/details/18986153)
> - [在 Windows 上使用 Visual Studio 编译 CURL](https://yq.aliyun.com/articles/8502)

#### 1.3 编写测试代码：
打开vs，新建win32命令行工程，选择模板为空，不需要预编译选项。然后新建如下main.cpp文件：
```C++
#include <string>
#include <iostream>
#include <fstream>

#include "curl/curl.h"
#include "curl/easy.h"

using namespace std;

// 获取指定URL的内容到屏幕中：
void getURLPrint(string URLAddress) {
	CURL *curl;             //定义CURL类型的指针
	CURLcode res;           //定义CURLcode类型的变量，保存返回状态码

	// 修改协议头
	struct curl_slist *useragent = NULL;
	useragent = curl_slist_append(useragent, "Mozilla/5.0 (Windows; U; Windows NT 5.1; rv:1.7.3) Gecko/20041001 Firefox/0.10.1");


	curl = curl_easy_init();        //初始化一个CURL类型的指针
	if (curl != NULL)
	{
		//设置curl选项. 其中CURLOPT_URL是让用户指 定url. argv[1]中存放的命令行传进来的网址
		curl_easy_setopt(curl, CURLOPT_URL, URLAddress);
		//curl_easy_setopt(curl, CURLOPT_USERAGENT, "Mozilla/5.0 (Windows; U; Windows NT 5.1; rv:1.7.3) Gecko/20041001 Firefox/0.10.1", URLAddress);
		curl_easy_setopt(curl, CURLOPT_USERAGENT, useragent, URLAddress);
		//curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, URLAddress);

		//调用curl_easy_perform 执行我们的设置.并进行相关的操作. 在这 里只在屏幕上显示出来.
		res = curl_easy_perform(curl);
		//清除curl操作.
		curl_easy_cleanup(curl);
	}
}

// 测试入口
int main() {
	string wwwRequestURL = "http://www.baidu.com";
	getURLPrint(wwwRequestURL);
	return 0;
}
```

然后配置当前工程的附加库：
```shell
1. 添加编译所需要（依赖）的 lib 文件
     在“项目->属性->配置属性->连接器->输入->附加依赖项”里填写“winsock.lib”，多个 lib 以空格隔开。等同于“#pragma comment(lib, "*.lib") ”语句。

2. 添加库（libs）文件目录
     方法 1：项目->属性->配置属性->连接器->常规->附加库目录”
     方法 2：[菜单]“工具->选项->项目和解决方案->c++ 目录”，选择对应平台，然后添加所需“库文件”目录
     这个设置类似于设置环境变量，主要是为程序设置搜索的库目录，真正进行库加载还需要进行第一种设置！

3. 添加包含（include）文件目录
     方法 1：“项目->属性->配置属性->c/c++->常规->附加包含目录”
     方法 2：[菜单]“工具->选项->项目和解决方案->c++ 目录”，添加所需“包括文件”目录
     方法2类似于设置环境变量。
4. 导入库（import）
    在“项目->属性->配置属性->连接器->高级->导入库”填写需要生成的导入库
```

根据上述完成对libcurl和jsoncpp库的添加。

因为上述代码使用了C的运行时库，vs会报安全错误，为了关闭，执行以下操作：
```shell
右击工程 - 属性 - 配置属性 - C/C++  - 命令行
命令行增加		/D _CRT_SECURE_NO_WARNINGS
```

完成上述步骤后执行编译，如果没有错误就表示程序编译通过，然后就需要进行单步调试了。
在调试和运行前，需要将jsoncpp库和libcurl库的相应的.dll或者.a和.so库跟代码文件放在一个目录下保证动态链接库能被正常的访问。

> 关于vs设置的参考文件：
> - [带你玩转Visual Studio——带你跳出坑爹的Runtime Library坑](http://blog.csdn.net/luoweifu/article/details/49055933)


#### 1.4 libcurl编程框架：
因为使用libcurl来进行http协议下的网络访问，所以需要熟悉libcurl提供的编程框架来完成自己的代码。
libcurl的基本编程流程为：
* 1. 调用curl_global_init()初始化libcurl
* 2. 调用curl_easy_init()函数得到 easy interface型指针
* 3. 调用curl_easy_setopt()设置传输选项
* 4. 根据curl_easy_setopt()设置的传输选项，实现回调函数以完成用户特定任务
* 5. 调用curl_easy_perform()函数完成传输任务
* 6. 调用curl_easy_cleanup()释放内存

上述步骤中，除去初始化和结束回收之外，最关键的步骤中使用的函数就是：*curl_easy_setopt()*，这个函数通过不同的参数来进行行为的设置。

在完成上述框架的了解之后，可以使用提供的基本API进行编写代码来测试了。具体的代码片段为：
```C++
```

> 具体的libcurl的C编程教程可以参考：
> - [C++ 用libcurl库进行http通讯网络编程](http://www.cnblogs.com/moodlxs/archive/2012/10/15/2724318.html)
> - [Calling SOAP webservice from C++ using libcurl](https://curl.haxx.se/mail/lib-2011-10/0212.html)
> - [Save cURL content result into a string in C++](http://stackoverflow.com/questions/9786150/save-curl-content-result-into-a-string-in-c)
> - [c++ libcurl json rest](http://stackoverflow.com/questions/5707957/c-libcurl-json-rest)



> 关于断点续传：
> - [coco2dx c++ 断点续传实现](http://blog.csdn.net/vpingchangxin/article/details/22309067)
> - [使用libcurl库进行HTTP的下载](http://blog.csdn.net/gjy1606/article/details/5644712)
> - [Libcurl实现文件下载](http://blog.sina.com.cn/s/blog_a6fb6cc90101ffn4.html)
> - [使用libcurl提交POST请求](http://finux.iteye.com/blog/715247)
> - [libcurl post／get上传下载文件 以及断点下载](http://www.xuebuyuan.com/1254589.html)
> - [Libcurl实现断点续传](http://www.cnblogs.com/chang290/archive/2012/08/12/2634858.html)
> - [libcurl 撸记](http://ftxtool.org/index.php/tag/duan_dian_xu_chuan/)

#### 1.5 JSon格式数据的解析：
根据性能测评和移植性测评，最终选定RapidJSON来进行JSON格式的解析和转换。对比jsoncpp的编译和链接，可以方便的通过头文件引入的方式来集成，方便整个开发。
首先从[官方网站](https://github.com/miloyip/rapidjson)同步代码：
```shell
git clone https://github.com/miloyip/rapidjson.git
```
然后，新建一个C++类来完成测试：
```C++
```

#### 1.6 JSON和C++ struct之间的转化：
完成JSON格式的解析之后，为了通用性，需要将JSON字符串和对应的struct结构体进行转换才可以方便的进行处理。
然后C++没有提供java自带的反射机制，这一步骤无法通过语言提供的原生语法实现，需要借助于第三方库来进行处理。

> 参考：
> - [C结构体与 JSON 快速互转库](https://www.zybuluo.com/armink/note/189711)
> - [C结构体与Json字符串自动转换](http://xphhhh.blog.51cto.com/7540829/1573856)


> 关于一些开源的实现和思路：
> - [cobj](https://github.com/xphh/cobj)
> - [ajson](https://github.com/lordoffox/ajson)
> - [C++ JSON Serialization](http://stackoverflow.com/questions/17549906/c-json-serialization)
> - [Codeless JSON C/C++ Object Serialization](http://jbvsblog.blogspot.jp/2013/12/codeless-json-c-cpp-object-serialization.html)
> - [ThorSerializer](https://github.com/Loki-Astari/ThorsSerializer)
> - [JSON for Modern C++](https://github.com/nlohmann/json)
> - [protobuf-c](https://github.com/protobuf-c/protobuf-c)


#### 1.7 使用libcurl进行form表单的multipart/form-data的设置：
在使用libcurl进行form表单数据提交的时候遇到了问题，因为server端使用了spring的multipartFile参数，使用libcurl的设置就发生了变化，参考[官方的例程1:postit2](https://curl.haxx.se/libcurl/c/postit2.html)和[官方的例程2:multi-post](https://curl.haxx.se/libcurl/c/multi-post.html)没有帮助，只能从网上查找相关的资料，主要有如下：
> - [使用libcurl进行文件上传](http://www.cnblogs.com/lidabo/p/4159377.html)
> - [使用libcurl POST数据和上传文件](http://www.cnblogs.com/lidabo/p/4159592.html)
> - [使用libcurl Post 含有照片的HTTP form表单](http://inspire365.blog.163.com/blog/static/196187838201368112120658/)
> - [[实践OK]C语言 HTTP上传文件-利用libcurl库上传文件, curl连接超时的问题 特别是获取返回头及内容的c写法。](http://justwinit.cn/post/7626/)
> - [linux c libcurl的简单使用](https://my.oschina.net/u/136923/blog/93448)
> - [上传文件multipart form-data boundary 说明](http://www.cnblogs.com/yydcdut/p/3736667.html)
> - [libcurl上传文件](http://www.cnblogs.com/meteoric_cry/p/4285881.html)
> - [libcURL POST multipart upload (with buffered image) returning HTTP 400](http://stackoverflow.com/questions/37082651/libcurl-post-multipart-upload-with-buffered-image-returning-http-400)


整个form表单设置中最主要就是[curl_formadd](https://curl.haxx.se/libcurl/c/curl_formadd.html)函数对整个form表单的结构进行了规定。

最终发现问题所在：因为libcurl是C库，所以所有接受参数都必须为char*数组指针，而不是string，这就是造成明明传输了参数，但是却无法获取的原因。


#### 1.8 使用cmake来管理当前的整个工程文件：
在上述步骤完成之后，基本的测试和验证系统完成了开发，为了后续联合开发的方便，需要对整个工程做跨平台处理，这儿借助于cmake来帮助完成。
然后重新整理上述完成的代码，后续的开发和编译工作都从cmake进行组织和管理。

##### 1.8.1 梳理代码组织形式：
首先，需要梳理当前的源代码组织形式，然后再根据这个源代码的组织形式来编写cmake脚本，帮助完成配置和编译工作。
现在初步规划的组织形式为：
```shell
WebServicewithCurl_cmake
│
│─CMakeLists.txt
│
├─include
│  ├─curl
│  │      curl.h
│  │      curlbuild.h
│  │      curlrules.h
│  │      curlver.h
│  │      easy.h
│  │      mprintf.h
│  │      multi.h
│  │      stdcheaders.h
│  │      typecheck-gcc.h
│  │
│  ├─json
│  │      allocator.h
│  │      assertions.h
│  │      autolink.h
│  │      config.h
│  │      features.h
│  │      forwards.h
│  │      json.h
│  │      reader.h
│  │      value.h
│  │      version.h
│  │      writer.h
│  │
│  └─rapidjson
│      │  allocators.h
│      │  document.h
│      │  encodedstream.h
│      │  encodings.h
│      │  filereadstream.h
│      │  filewritestream.h
│      │  fwd.h
│      │  istreamwrapper.h
│      │  memorybuffer.h
│      │  memorystream.h
│      │  ostreamwrapper.h
│      │  pointer.h
│      │  prettywriter.h
│      │  rapidjson.h
│      │  reader.h
│      │  schema.h
│      │  stream.h
│      │  stringbuffer.h
│      │  writer.h
│      │
│      ├─error
│      │      en.h
│      │      error.h
│      │
│      ├─internal
│      │      biginteger.h
│      │      diyfp.h
│      │      dtoa.h
│      │      ieee754.h
│      │      itoa.h
│      │      meta.h
│      │      pow10.h
│      │      regex.h
│      │      stack.h
│      │      strfunc.h
│      │      strtod.h
│      │      swap.h
│      │
│      └─msinttypes
│              inttypes.h
│              stdint.h
│
├─lib
│      jsoncpp.lib
│      libcurl_imp.lib
│
├─libtest
│      CMakeLists.txt
│      main.cpp
│
└─src
    │  CMakeLists.txt
    │  RESTFulRequestor.cpp
    │  RESTFulRequestor.h
    │
    └─JsonUtil
            JsonCppUtil.cpp
            JsonCppUtil.h
            JsonInterface.h
            RapidJsonUtil.cpp
            RapidJsonUtil.h
```
其中：
- src为库的主要实现文件夹，包含自己定义的类和要生成库的主要实现文件和头文件；
- libtest是为测试库的测试应用程序文件夹；
- lib为使用的第三方静态库的文件夹，主要是为了方便开发放在了一起，可以通过cmake进行配置路径进行设置；
- include文件夹存放的是第三方库的头文件，下面按照使用的三个库：libcurl、jsoncpp和rapidjson分别放在不同的文件夹下；

##### 1.8.2 编写cmake脚本：
经过上述的初步组织，需要三个cmake脚本来帮助完成编译工作：
- 顶层cmake脚本：设置当前整个工程的属性和基本依赖项；
- src层cmake脚本：组织自己编写的代码模块，生成需要的静态库和动态库；
- libtest层cmake脚本：使用src生成的静态库链接生成可执行文件来进行测试；

###### （1）顶层cmake脚本内容为：
```cmake
# 设置最小的构建版本要求
CMAKE_MINIMUM_REQUIRED(VERSION 2.8)

# 设置当前的工程名称和版本号
PROJECT(WebServicewithCurl CXX)
SET(LIB_MAJOR_VERSION "1")
SET(LIB_MINOR_VERSION "1")
SET(LIB_PATCH_VERSION "0")
SET(LIB_VERSION_STRING "${LIB_MAJOR_VERSION}.${LIB_MINOR_VERSION}.${LIB_PATCH_VERSION}")

# 设置外部依赖的头文件路径：
FIND_PATH(MUDUO_INCLUDE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/include)

# 设置外部依赖的第三方库路径：
FIND_LIBRARY(LIB_CURL_PATH ${CMAKE_CURRENT_SOURCE_DIR}/lib/libcurl_imp.lib)
FIND_LIBRARY(LIB_JSONCPP_PATH ${CMAKE_CURRENT_SOURCE_DIR}/lib/jsoncpp.lib)

# 添加库的子文件夹：
ADD_SUBDIRECTORY(src)
# 添加测试库子文件夹：
ADD_SUBDIRECTORY(libtest)
```
###### （2）src文件夹cmake脚本内容为：
```cmake
# 设置当前文件夹构建需要的源文件：
SET(LIBRESTFULREQUESTOR_SRC RESTFulRequestor.cpp)

# 使用上述设置的源文件构建动态链接库：
ADD_LIBRARY(RESTFulRequestor SHARED ${LIBRESTFULREQUESTOR_SRC})
# 使用上述设置的源文件，构建静态库文件：
ADD_LIBRARY(RESTFulRequestorlib STATIC ${LIBRESTFULREQUESTOR_SRC})

# 添加库的版本
SET_TARGET_PROPERTIES(RESTFulRequestor PROPERTIES VERSION 1.0 SOVERSION 1)

# 设置当前编译需要的自定义模块头文件：
#INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR}/)
# 设置需要的附加头文件：
INCLUDE_DIRECTORIES(${MUDUO_INCLUDE_PATH})
# 设置需要的附加库文件：
TARGET_LINK_LIBRARIES(RESTFulRequestor ${LIB_CURL_PATH})

# 设置库编译安装的位置：
INSTALL(TARGETS RESTFulRequestor RUNTIME DESTINATION lib)
INSTALL(TARGETS RESTFulRequestorlib ARCHIVE DESTINATION libstatic)
INSTALL(FILES RESTFulRequestor.h DESTINATION include/RESTFulRequestor)
```

###### （3）libtest文件夹cmake脚本内容为：
```cmake
# 设置测试工程名称
SET(TARGET_LABEL_PREFIX "Test ")

# 设置当前文件夹编译依赖文件
SET(LIBRESTFULREQUESTORTEST_SRC main.cpp)

# 使用上述设置的源文件构建应用程序：
ADD_EXECUTABLE(RESTFulRequestorTest ${LIBRESTFULREQUESTORTEST_SRC})

# 添加库的版本
SET_TARGET_PROPERTIES(RESTFulRequestorTest PROPERTIES VERSION 1.0 SOVERSION 1)

# 依赖于本工程自己的头文件路径
INCLUDE_DIRECTORIES(${PROJECT_SOURCE_DIR}/src)
# 依赖的第三方库头文件路径：
INCLUDE_DIRECTORIES(${MUDUO_INCLUDE_PATH})

# 设置对当前工程文件生成库的依赖：直接使用其他子文件夹下ADD_LIBRARY中定义的文件
TARGET_LINK_LIBRARIES(RESTFulRequestorTest RESTFulRequestorlib)

# 设置需要的附加库文件：
TARGET_LINK_LIBRARIES(RESTFulRequestorTest ${LIB_CURL_PATH})
TARGET_LINK_LIBRARIES(RESTFulRequestorTest ${LIB_JSONCPP_PATH})

# 设置库编译安装的位置：
INSTALL(TARGETS RESTFulRequestorTest RUNTIME DESTINATION test)
```

完成上述文件的编写之后，就可以使用cmake构建工程了，最后生成需要的头文件、动态库文件和静态库文件。


参考资料：
> 入门渐进式资料，用于从头开始编写cmake文件：
> - [使用CMake构建项目的简明示例（1）](http://blog.csdn.net/lzx1104/article/details/6038007)
> - [使用CMake构建项目的简明示例（2）](http://blog.csdn.net/lzx1104/article/details/6046131)
> - [CMake 实例学习（0）开始](http://blog.chinaunix.net/uid-25696269-id-603825.html)
> - [CMake 实例学习（1）内外之分](http://blog.chinaunix.net/uid-25696269-id-603961.html)
> - [CMake 实例学习（2）构建共享库](http://blog.chinaunix.net/uid-25696269-id-761383.html)
> - [CMake 实例学习（3）构建静态库](http://blog.chinaunix.net/uid-25696269-id-1435094.html)
> - [CMake 实例学习（4）动/静态库共存](http://blog.chinaunix.net/uid-25696269-id-1564981.html)
> 汇总资料，用于查询对应的用法：
> - [CMake整理](http://pengbotao.cn/linux-cmake.html)
> - [cmake使用示例与整理总结](http://blog.csdn.net/wzzfeitian/article/details/40963457)
> - [利用CMake生成动态或静态链接库工程](http://www.cnblogs.com/springbarley/p/3359624.html)
> - [为什么使用CMake](http://linghutf.github.io/2016/06/16/cmake/)


#### 1.9 关于调用中的文件编码问题：



## 四 C++的反射实现：
在上述章节中，由于C++原生不支持反射，所以类似于java的反序列化在C++中难以只通过语言层面的机制实现，所以考虑由库的形式进行支持来帮助完成JSON库反序列化到DOM对象，然后生成指定的C++结构体或者类实例的过程。
根据网上查找的资料，[Kapok](https://github.com/qicosmos/Kapok)提供了预想中的例程。所以准备使用这个库来完成当前的任务。

### 1 依赖的库：
这个库为了完成对应的结构体自动转换，使用了如下的第三方库：
> - [boost](http://www.boost.org/users/download/)
> - [fmt](https://github.com/fmtlib/fmt)
> - [RapidJSON]()

#### boost库的使用：
因为boost库太庞大，所以采用官方提供的预编译库：*[Prebuilt windows binaries](https://sourceforge.net/projects/boost/files/boost-binaries/)* 来完成编译和链接。
修改默认安装位置为：C:/boost_1_62_0
等待一段时间后完成释放安装。

#### fmtlib库的使用：
对于fmtlib还是采用自己编译的方式来完成：
```shell
git clone https://github.com/fmtlib/fmt.git
cmake ..
make -j4
make install
```
然后使用安装完毕的头文件和静态库来完成开发。

#### RapidJSON库的使用：
Kapok是建立在RapidJSON基础上完成JSON序列化和反序列化的，所以也需要这个库来完成开发。具体内容参考之前的章节。

### 2 编译Kapok生成静态库：
根据顶层cmake脚本，需要手工在cmake-gui中添加变量：
```shell
BOOST_ROOT			C:/boost_1_62_0
```
然后就可以正确的生成了。
但是由于这个库作者的失误，在cmake-gui中添加的BOOST_ROOT并没有被正确的添加到工程文件中，所以还需要自己手动进行添加。
添加fmtlib是为了Kapok自带的测试输出更方便查看，而不是库完成功能本身的需求。
完成依赖库的添加之后就可以进行编译了。

> 后续考虑将这个工程集成到自己的工程中，方便使用和配置。

### 3 封装Kapok库提供接口，并且测试：
根据官方文档，然后结合自己的需求，对接口进行封装。
AutoParser.h头文件内容：
```C++
// 使用Kapok作为序列化和反序列化的支持
#pragma once

#ifndef AUTOPARSER_H
#define AUTOPARSER_H

// standar library
#include <string>
#include <iostream>
#include <fstream>

// 3rd library
# include "kapok\Kapok.hpp"

using namespace std;

template<class T> class AutoParser {
public:
	AutoParser();
	~AutoParser();
	bool deserialize(T& instance, string jsonStr);
	string serialize(T a);
private:
	Serializer sr;
	DeSerializer dr;
};

// for template link
//# include "AutoParser.cpp"
#endif
```

AutoParser.cpp实现内容：
```C++
#include "AutoParser.h"

template<class T>
AutoParser<T>::AutoParser() {
	cout << "welcome to auto parser to JSON-Struct" << endl;
}

template<class T>
AutoParser<T>::~AutoParser() {
	cout << "goodbye" << endl;
}

template<class T>
bool AutoParser<T>::deserialize(T& instance, string jsonStr) {
	this->dr.Parse(jsonStr);
	this->dr.Deserialize(instance);
	return true;
}

template<class T>
string AutoParser<T>::serialize(T a) {
	this->sr.Serialize(a);
	return this->sr.GetString();
}
```

最后在测试文件中添加如下代码：
```C++
// JSON转换接口
#include "AutoParser.cpp"
int main(int argc, char * argv[]) {

	// 测试结构体的序列化和反序列化
	struct Person
	{
		int age;
		string name;
		string city;

		// import : to add meteinfo to autoparser
		META(age, name, city)
	};
	Person p = { 18, "bb", "aa" };

	// 准备序列化和反序列化
	AutoParser<Person> parser;
	string serializitionResult = parser.serialize(p);
	cout << "转换后的JSON结果为：" << serializitionResult << endl;
	Person deserializitionResult;
	bool result = parser.deserialize(deserializitionResult, serializitionResult);
	if (result) {
		cout << "反序列化成功！" << endl;
		cout << deserializitionResult.age << endl;
		cout << deserializitionResult.name << endl;
		cout << deserializitionResult.city << endl;
		//cout << &deserializitionResult.Meta << endl;
	}

	return 0;
}
```
编译后，完成测试。

* 目前来看，通过Kapok是可以满足当前工程的基本需求的，但是作者的具体实现思路还不清楚，为了后续集成，RTFSC。 *

> 参考文档：
> - [Kapok wiki](https://github.com/qicosmos/Kapok/wiki)
> - [判断是否为TUPLE类型](http://purecpp.org/?p=30)
> - [从一个例子看现代C++的威力](http://purecpp.org/?p=694)
> - [更好的C++序列化/反序列化库–KAPOK](http://purecpp.org/?p=17)
> - [KAPOK发布1.0版本了](http://purecpp.org/?p=893)


## 四 关于C++模板导出为动态库：
C++的模板属于编译期特性，那么如何将自己定义的模板库导出为指定编译器的动态库让他人使用？这样可以将我们编写的代码和用户使用完全隔离开来，即使需要考虑编译器兼容性也是能够接受的（针对不同的编译器提供预编译动态库，然后供用户下载使用）。



> 参考文档：
> - [动态链接库中导出模板函数](http://blog.csdn.net/liyuanbhu/article/details/50363670)
> - [在dll中导出模板类的方法 ](http://blog.sina.com.cn/s/blog_4298002e01018y5s.html)