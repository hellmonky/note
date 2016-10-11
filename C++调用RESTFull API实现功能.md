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
主要用于让C++可以访问http协议的RESTFul接口的封装。

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
建议使用libcurl进行http访问的开发。

主要参考文档有：
[REST实战——调用百度语音的云服务](http://www.voidcn.com/blog/u011000290/article/p-5782799.html)


#### 2.2 Web Services相关：
* [gsoap2](http://sourceforge.net/projects/gsoap2): Development toolkit for Web Services and XML data bindings for C & C++
* [cpprestsdk](https://github.com/Microsoft/cpprestsdk): The C++ REST SDK is a Microsoft project for cloud-based client-server communication in native code using a modern asynchronous C++ API design. This project aims to help C++ developers connect to and interact with services.
* [](): 
* [](): 
* [](): 
* [](): 

> 库的选型和对比：
建议使用cpprestsdk进行RESTFul接口的访问。

主要参考文档有：
[gsoap2官方文档](https://www.genivia.com/dev.html)
[c/c++使用gsoap发布和调用webservice](http://blog.csdn.net/wenzi49312/article/details/8963345)
[使用微软的 C++ REST SDK](http://blog.jobbole.com/53642/)
[cpprestsdk官方示例代码](https://github.com/Microsoft/cpprestsdk/wiki/Samples)



## 三 环境设置：
在上述基本步骤和第三方库选型完毕之后就需要尝试在windows和linux系统下进行实际的测试和开发，本机关注与工程实践的部署环节和开发环节。