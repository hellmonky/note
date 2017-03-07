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
namespace cpp com.iscas.datasong.service.thriftService


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
还需要做的就是添加thrift的服务端启动代码，这样才能生成一个持续接受socket请求的入口，在datasong的根目录下添加一个启动服务：
```java

```