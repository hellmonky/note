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
[REST实战——调用百度语音的云服务](http://www.voidcn.com/blog/u011000290/article/p-5782799.html)
[Call Rest Web Services from C++](http://itcompiles.blogspot.jp/2013/09/call-rest-web-services-from-c.html)
> Rest webservice are based on http all we have to do is make http requests. 


#### 2.2 完整的Web Services相关：
* [gsoap2](http://sourceforge.net/projects/gsoap2): Development toolkit for Web Services and XML data bindings for C & C++
* [cpprestsdk](https://github.com/Microsoft/cpprestsdk): The C++ REST SDK is a Microsoft project for cloud-based client-server communication in native code using a modern asynchronous C++ API design. This project aims to help C++ developers connect to and interact with services.

> 库的选型和对比：
建议使用cpprestsdk进行RESTFul接口的访问。

主要参考文档有：
[gsoap2官方文档](https://www.genivia.com/dev.html)
[c/c++使用gsoap发布和调用webservice](http://blog.csdn.net/wenzi49312/article/details/8963345)
[使用微软的 C++ REST SDK](http://blog.jobbole.com/53642/)
[cpprestsdk官方示例代码](https://github.com/Microsoft/cpprestsdk/wiki/Samples)



## 三 环境设置：
在上述基本步骤和第三方库选型完毕之后就需要尝试在windows和linux系统下进行实际的测试和开发，本机关注与工程实践的部署环节和开发环节。
个人现在从工程实践的角度来进行考虑，还是希望开发越简单，部署越容易好，从这个角度出发，偏向于使用功能完善的http库来提升开发效率。偏向于使用http库进行开发。

### 基于libcurl的web service访问：
基于jsoncpp库和libcurl库完成开发。其中JSON库可以使用其他C库进行替换。

```C++
#include <cstdio>
#include <cstring>
#include <stdlib.h>
#include "curl/curl.h"

#include "curl/easy.h"
#include "json/json.h"
#define MAX_BUFFER_SIZE 512
#define MAX_BODY_SIZE 1000000

using namespace std;

static const std::string base64_chars =
"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
"abcdefghijklmnopqrstuvwxyz"
"0123456789+/";

static inline bool is_base64(unsigned char c) 
{
	return (isalnum(c) || (c == '+') || (c == '/'));
}

string base64_encode(unsigned char const* bytes_to_encode, unsigned int in_len) 
{
	std::string ret;
	int i = 0;
	int j = 0;
	unsigned char char_array_3[3];
	unsigned char char_array_4[4];

	while (in_len--) 
	{
		char_array_3[i++] = *(bytes_to_encode++);
		if (i == 3) 
		{
			char_array_4[0] = (char_array_3[0] & 0xfc) >> 2;
			char_array_4[1] = ((char_array_3[0] & 0x03) << 4) + ((char_array_3[1] & 0xf0) >> 4);
			char_array_4[2] = ((char_array_3[1] & 0x0f) << 2) + ((char_array_3[2] & 0xc0) >> 6);
			char_array_4[3] = char_array_3[2] & 0x3f;

			for (i = 0; (i <4); i++)
				ret += base64_chars[char_array_4[i]];
			i = 0;
		}
	}

	if (i)
	{
		for (j = i; j < 3; j++)
			char_array_3[j] = '\0';

		char_array_4[0] = (char_array_3[0] & 0xfc) >> 2;
		char_array_4[1] = ((char_array_3[0] & 0x03) << 4) + ((char_array_3[1] & 0xf0) >> 4);
		char_array_4[2] = ((char_array_3[1] & 0x0f) << 2) + ((char_array_3[2] & 0xc0) >> 6);
		char_array_4[3] = char_array_3[2] & 0x3f;

		for (j = 0; (j < i + 1); j++)
			ret += base64_chars[char_array_4[j]];

		while ((i++ < 3))
			ret += '=';
	}
	return ret;
}

string base64_decode(std::string const& encoded_string) 
{
	int in_len = encoded_string.size();
	int i = 0;
	int j = 0;
	int in_ = 0;
	unsigned char char_array_4[4], char_array_3[3];
	std::string ret;

	while (in_len-- && (encoded_string[in_] != '=') && is_base64(encoded_string[in_])) 
	{
		char_array_4[i++] = encoded_string[in_]; in_++;
		if (i == 4) 
		{
			for (i = 0; i <4; i++)
				char_array_4[i] = base64_chars.find(char_array_4[i]);

			char_array_3[0] = (char_array_4[0] << 2) + ((char_array_4[1] & 0x30) >> 4);
			char_array_3[1] = ((char_array_4[1] & 0xf) << 4) + ((char_array_4[2] & 0x3c) >> 2);
			char_array_3[2] = ((char_array_4[2] & 0x3) << 6) + char_array_4[3];

			for (i = 0; (i < 3); i++)
				ret += char_array_3[i];
			i = 0;
		}
	}

	if (i)
	{
		for (j = i; j <4; j++)
			char_array_4[j] = 0;


		for (j = 0; j <4; j++)
			char_array_4[j] = base64_chars.find(char_array_4[j]);

		char_array_3[0] = (char_array_4[0] << 2) + ((char_array_4[1] & 0x30) >> 4);
		char_array_3[1] = ((char_array_4[1] & 0xf) << 4) + ((char_array_4[2] & 0x3c) >> 2);
		char_array_3[2] = ((char_array_4[2] & 0x3) << 6) + char_array_4[3];

		for (j = 0; (j < i - 1); j++) ret += char_array_3[j];
	}
	return ret;
}

//回调函数
static size_t writefunc(void *ptr, size_t size, size_t nmemb, char **result)
{
	size_t result_len = size * nmemb;
	*result = (char *)realloc(*result, result_len + 1);
	if (*result == NULL)
	{
		printf("realloc failure!\n");
		return 1;
	}
	memcpy(*result, ptr, result_len);
	(*result)[result_len] = '\0';
	cout<<百度服务器返回的json数据："<<*result<<endl;
	/*Json::Reader reader;
	Json::Value root;
	if(reader.parse(result,root))
	{
		string res = root["result"].asString();
		cout <<"解析的结果: "<< res << endl;
	}*/
	return result_len;
}

int main()
{
    // 将整个结果写入到文本文件中作为测试查看
	freopen("out.txt", "w", stdout);
	int json_file_size;
	FILE *pFile = NULL;
	char *audio_data;
	pFile = fopen("test.pcm", "r");
	if (pFile == NULL)
	{
		perror("Open file error!\n");
	}
	else
	{
		fseek(pFile, 0, SEEK_END);
		int file_size = ftell(pFile);
		cout << "file size: " << file_size << " bytes" << endl;
		fseek(pFile, 0, SEEK_SET);
		audio_data = (char *)malloc(sizeof(char)*file_size);
		fread(audio_data, file_size, sizeof(char), pFile);

		//机器的mac地址
		char *cuid = "56:84:7a:fe:97:99";
		char *api_key = "6yFhYifMjXc8QmubiICXBQgi";
		char *secret_key = "nZn45o3X0LGx42qovumYy2mjpOiOup2E";

		char host[MAX_BUFFER_SIZE];
        // 最主要的调用URL构造
		snprintf(host, sizeof(host),
			"https://openapi.baidu.com/oauth/2.0/token?grant_type=client_credentials&client_id=%s&client_secret=%s",
			api_key, secret_key);
		cout << "curl -s命令的host: " << host << endl;

		FILE *p = NULL;
		char cmd[MAX_BUFFER_SIZE];
		//curl -s命令的返回结果
		char *result = (char*)malloc(MAX_BUFFER_SIZE);
		char *curl_cmd = "curl -s ";
		char *yinhao = "\"";

		strcpy(cmd, curl_cmd);
		strcat(cmd, yinhao);
		strcat(cmd, host);
		strcat(cmd, yinhao);


		p = popen(cmd, "r");
		fgets(result, MAX_BUFFER_SIZE, p);
		cout << "curl -s 响应结果: " << result << endl;
		pclose(p);

		string access_token;
		//解析服务器返回的Json数据,获取access_token
		if (result != NULL)
		{
			Json::Reader reader;
			Json::Value root;
			if (reader.parse(result, root, false))
			{
				access_token = root.get("access_token", "").asString();
			}
			cout << "access_token: " << access_token << endl;
		}

		//采取隐式发送的方式给服务器发送json格式的数据
		char body[MAX_BODY_SIZE];
		memset(body, 0, sizeof(body));
		string decode_data = base64_encode((const unsigned char *)audio_data, file_size);
		if (0 == decode_data.length())
		{
			cout << "Error!base64 encoded data is empty!";
			return 1;
		}
		else
		{
			Json::Value buffer;
			Json::FastWriter buf_writer;
			buffer["format"] = "pcm";
			buffer["rate"] = 8000;
			buffer["channel"] = 1;
			buffer["token"] = access_token.c_str();
			buffer["cuid"] = cuid;
			buffer["speech"] = decode_data;
			buffer["len"] = file_size;
			//实际json格式数据的长度
			json_file_size = buf_writer.write(buffer).length();
			cout << "Json file size:" << json_file_size << " bytes" << endl;
			memcpy(body, buf_writer.write(buffer).c_str(), json_file_size);

			CURL *curl;
			CURLcode res;//服务器的响应结果
			char *result_buffer = NULL;
			struct curl_slist *http_header = NULL;
			char temp[MAX_BUFFER_SIZE];
			memset(temp, 0, sizeof(temp));
			snprintf(temp, sizeof(temp), "%s", "Content-Type: application/json; charset=utf-8");
			http_header = curl_slist_append(http_header, temp);
			snprintf(temp, sizeof(temp), "Content-Length: %d", json_file_size);
			http_header = curl_slist_append(http_header, temp);


			memset(host, 0, sizeof(host));
			snprintf(host, sizeof(host), "%s", "http://vop.baidu.com/server_api");
			cout << "server host: " << host << endl;
			curl = curl_easy_init();
			curl_easy_setopt(curl, CURLOPT_URL, host);//设置访问的URL
			curl_easy_setopt(curl, CURLOPT_POST, 1);//1表示常规的http post请求
			curl_easy_setopt(curl, CURLOPT_TIMEOUT, 30);//设置延时
			curl_easy_setopt(curl, CURLOPT_HTTPHEADER, http_header);
			curl_easy_setopt(curl, CURLOPT_POSTFIELDS, body);
			curl_easy_setopt(curl, CURLOPT_POSTFIELDSIZE, json_file_size);
			curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, writefunc);//设置回调函数
			curl_easy_setopt(curl, CURLOPT_WRITEDATA, &result_buffer);
			res = curl_easy_perform(curl);
			if (res != CURLE_OK)
			{
				printf("perform curl error:%d.\n", res);
				return 1;
			}
			curl_slist_free_all(http_header);
			curl_easy_cleanup(curl);
			
			free(audio_data);
		}
	}

	fclose(pFile);
	return 0;

}
```

运行前，将jsoncpp库的头文件在json文件夹下和libcurl库的头文件在curl文件夹下，以及相应的.a和.so库跟代码文件放在一个目录下。运行可执行文件，源码中将标准输出写入到out.txt文件中。
具体的libcurl的C编程教程可以参考：
[C++ 用libcurl库进行http通讯网络编程](http://www.cnblogs.com/moodlxs/archive/2012/10/15/2724318.html)
