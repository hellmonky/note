# java项目中使用fastDFS库作为小文件存储平台

当前使用hbase的基础上，因为效率问题，考虑切换小文件存储到fastDFS这个开源框架上，但是目前这个框架的维护状态不稳定，主要体现在：
- [官方网站](http://www.csource.org/)目前无法访问；
- 没有具体的官方文档；
- 没有具体的设计说明。

但是考虑到效率问题，暂时考虑进行集成测试，看当前的效果如何。

## 0 fastDFS作者和相关资料：
余庆，根据InfoQ对他的采访：[易到用车的消息推送平台重构和服务化框架](http://www.infoq.com/cn/author/%E4%BD%99%E5%BA%86)上的整理资料可知：
> 余庆,15年互联网开发经验，开源分布式文件系统FastDFS作者；参与过Apache Traffic Server核心代码改造； 先后在新浪、中国雅虎、淘宝和阿里云工作，参与过搜索引擎、CDN等平台的设计和研发工作，对分布式和高性能计算有着比较深入的研究，现在易到用车担任架构师。

但是目前在linkin上没有找到相关的资料。

在fastDFS被广泛使用之后，《程序员》杂志社针对fastDFS作者进行了采访，并且给出了一个采访稿：[分布式文件系统FastDFS架构剖析](http://history.programmer.com.cn/4380/)

作者自己提供了一个同名的PPT进行了讲解：[分布式文件系统FastDFS架构剖析](http://www.slideshare.net/billowqiu/fast-dfs-24907184)


## 1 关于fastDFS的代码：
fastDFS最早在2008年7月11日（初始化代码开始时间）在[google code](https://code.google.com/archive/p/fastdfs)上进行了托管，然后又于2008年07月27在[sourceforge](https://sourceforge.net/projects/fastdfs/)上进行托管，最后于2013年7月4日在[github](https://github.com/happyfish100/fastdfs)上进行了托管。
fastDFS这个项目目前已经基本停止功能更新，最新的更新提交为：[fix php extention memory leak in PHP 7](https://github.com/happyfish100/fastdfs/commit/87659981148a362812912b6d4752d281ac05f0b6)。
然后在2014年5月29日，作者基于FastDFS V5.02，启动了libfastcommon的开发，继续托管在[github](https://github.com/happyfish100/libfastcommon)上进行更新维护。
并且作者针对其他语言调用实现了wrapper，针对java相关的项目为：[fastdfs-client-java](https://github.com/happyfish100/fastdfs-client-java)。

因为fastDFS是使用C语言编写，并且基于类Unix系统的系统调用实现，目前没有针对windows的版本。
fastDFS的2.x版本绑定了libevent作为http服务器，故如果采用这个版本时需要系统安装这个库来提供支持。并且在3.03之后移除了对libevent的依赖，但是后续5.03版本的时候使用了libfastcommon中的文件，最终在5.04版本开始需要libfastcommon进行支持。


## 2 fastDFS的编译和java api的编译：
在明确上述前提的情况下，我们进行基于centos7_x64的编译和安装。需要系统安装了基本的系统编译工具链。

### 2.1 编译安装libfastcommon：
编译安装过程为：
```shell
git clone https://github.com/happyfish100/libfastcommon.git
cd libfastcommon
./make.sh
./make.sh install
```
默认安装回显为：
```shell
mkdir -p /usr/lib64
mkdir -p /usr/lib
install -m 755 libfastcommon.so /usr/lib64
install -m 755 libfastcommon.so /usr/lib
mkdir -p /usr/include/fastcommon
install -m 644 common_define.h hash.h chain.h logger.h base64.h shared_func.h pthread_func.h ini_file_reader.h _os_define.h sockopt.h sched_thread.h http_func.h md5.h local_ip_func.h avl_tree.h ioevent.h ioevent_loop.h fast_task_queue.h fast_timer.h process_ctrl.h fast_mblock.h connection_pool.h fast_mpool.h fast_allocator.h fast_buffer.h skiplist.h multi_skiplist.h flat_skiplist.h skiplist_common.h system_info.h fast_blocked_queue.h php7_ext_wrapper.h id_generator.h /usr/include/fastcommon
```
将生成的libfastcommon.so文件拷贝到/usr/lib和/usr/lib64文件夹下，然后将需要使用的头文件放在/usr/include/fastcommon/文件夹下。

### 2.2 编译安装fastDFS：
根据官方的README文档，fastDFS的编译非常简单：
```shell
git clone https://github.com/happyfish100/fastdfs.git
cd fastdfs
./make.sh
./make.sh install
```
安装的详情为：
```shell
mkdir -p /usr/bin
mkdir -p /etc/fdfs
cp -f fdfs_trackerd /usr/bin
if [ ! -f /etc/fdfs/tracker.conf.sample ]; then cp -f ../conf/tracker.conf /etc/fdfs/tracker.conf.sample; fi
mkdir -p /usr/bin
mkdir -p /etc/fdfs
cp -f fdfs_storaged  /usr/bin
if [ ! -f /etc/fdfs/storage.conf.sample ]; then cp -f ../conf/storage.conf /etc/fdfs/storage.conf.sample; fi
mkdir -p /usr/bin
mkdir -p /etc/fdfs
mkdir -p /usr/lib64
mkdir -p /usr/lib
cp -f fdfs_monitor fdfs_test fdfs_test1 fdfs_crc32 fdfs_upload_file fdfs_download_file fdfs_delete_file fdfs_file_info fdfs_appender_test fdfs_appender_test1 fdfs_append_file fdfs_upload_appender /usr/bin
if [ 0 -eq 1 ]; then cp -f libfdfsclient.a /usr/lib64; cp -f libfdfsclient.a /usr/lib/;fi
if [ 1 -eq 1 ]; then cp -f libfdfsclient.so /usr/lib64; cp -f libfdfsclient.so /usr/lib/;fi
mkdir -p /usr/include/fastdfs
cp -f ../common/fdfs_define.h ../common/fdfs_global.h ../common/mime_file_parser.h ../common/fdfs_http_shared.h ../tracker/tracker_types.h ../tracker/tracker_proto.h ../tracker/fdfs_shared_func.h ../storage/trunk_mgr/trunk_shared.h tracker_client.h storage_client.h storage_client1.h client_func.h client_global.h fdfs_client.h /usr/include/fastdfs
if [ ! -f /etc/fdfs/client.conf.sample ]; then cp -f ../conf/client.conf /etc/fdfs/client.conf.sample; fi
```
可以看到这个安装：
- 将生成的fdfs_trackerd和fdfs_storaged这两个主程序拷贝到/usr/bin目录下；
- 将默认的配置文件放在/etc/fdfs/目录下，包含：tracker.conf.sample，storage.conf.sample和client.conf.sample，这三个配置文件。
- 将生成的测试程序拷贝到/usr/bin/目录下：
    > fdfs_monitor fdfs_test fdfs_test1 fdfs_crc32 fdfs_upload_file fdfs_download_file fdfs_delete_file fdfs_file_info fdfs_appender_test fdfs_appender_test1 fdfs_append_file fdfs_upload_appender
- 最后将生成的动态库libfdfsclient.a和libfdfsclient.so拷贝到/usr/lib64和/usr/lib文件夹下。

然后对默认的tracker和storage的配置文件进行修改：
```shell
vim /etc/fdfs/tracker.conf
```
修改为：
```shell

```
启动tracker服务：
```shell
/usr/bin/fdfs_trackerd /etc/fdfs/tracker.conf restart
```
接着运行storage的存储服务：

