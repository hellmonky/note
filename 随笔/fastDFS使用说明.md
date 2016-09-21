<!-- TOC -->

- [0 fastDFS作者和相关资料：](#0-fastdfs作者和相关资料)
- [1 关于fastDFS的代码：](#1-关于fastdfs的代码)
- [2 fastDFS的编译和配置安装：](#2-fastdfs的编译和配置安装)
    - [2.1 编译安装libfastcommon：](#21-编译安装libfastcommon)
    - [2.2 编译安装fastDFS：](#22-编译安装fastdfs)
    - [2.3 配置启动fastDFS服务：](#23-配置启动fastdfs服务)
        - [2.3.1 配置启动tracker：](#231-配置启动tracker)
        - [2.3.2 配置启动storage：](#232-配置启动storage)
        - [2.3.3 配置client：](#233-配置client)
        - [2.3.4 运行测试程序：](#234-运行测试程序)
- [3 实现fastDFS的java调用：](#3-实现fastdfs的java调用)
    - [3.1 开启tracker和storage的端口可以被外部访问：](#31-开启tracker和storage的端口可以被外部访问)
    - [3.2 编译fastDFS的java调用接口：](#32-编译fastdfs的java调用接口)
        - [3.2.1 fastdfs-client-java的编译：](#321-fastdfs-client-java的编译)
        - [3.2.2 gradle工程中引入这个本地的jar包来进行开发集成：](#322-gradle工程中引入这个本地的jar包来进行开发集成)
    - [3.3 编写java程序进行外部文件上传测试：](#33-编写java程序进行外部文件上传测试)
- [4 对fastDFS的部署思考：](#4-对fastdfs的部署思考)
    - [4.1 centos7下自动启动fastDFS服务：](#41-centos7下自动启动fastdfs服务)
    - [4.2 脚本实现fastDFS的分发和部署：](#42-脚本实现fastdfs的分发和部署)

<!-- /TOC -->

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


## 2 fastDFS的编译和配置安装：
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

### 2.3 配置启动fastDFS服务：
完成上述安装之后，就可以根据默认的tracker和storage的配置文件进行修改来启动服务了。
#### 2.3.1 配置启动tracker：
```shell
vim /etc/fdfs/tracker.conf
```
这个配置文件的说明为：
```shell
# 这个配置文件是否无效，false表示有效
# is this config file disabled
# false for enabled
# true for disabled
disabled=false

# 是否绑定IP
# bind_addr= 后面为绑定的IP地址 (常用于服务器有多个IP但只希望一个IP提供服务)。如果不填则表示所有的(一般不填就OK)
# bind an address of this host
# empty for bind all addresses of this host
bind_addr=

# 提供服务的端口
# the tracker server port
port=22122

# 连接超时时间，针对socket套接字函数connect
# connect timeout in seconds
# default value is 30s
connect_timeout=30

# tracker server的网络超时，单位为秒。发送或接收数据时，如果在超时时间后还不能发送或接收数据，则本次网络通信失败
# network timeout in seconds
# default value is 30s
network_timeout=60

# the base path to store data and log files
base_path=/home/yuqing/fastdfs

# base_path 目录地址(根目录必须存在,子目录会自动创建)
# 附目录说明: 
#  tracker server目录及文件结构：
#  ${base_path}
#    |__data
#    |     |__storage_groups.dat：存储分组信息
#    |     |__storage_servers.dat：存储服务器列表
#    |__logs
#          |__trackerd.log：tracker server日志文件

#数据文件storage_groups.dat和storage_servers.dat中的记录之间以换行符（\n）分隔，字段之间以西文逗号（,）分隔。
#storage_groups.dat中的字段依次为：
#  1. group_name：组名
#  2. storage_port：storage server端口号

#storage_servers.dat中记录storage server相关信息，字段依次为：
#  1. group_name：所属组名
#  2. ip_addr：ip地址
#  3. status：状态
#  4. sync_src_ip_addr：向该storage server同步已有数据文件的源服务器
#  5. sync_until_timestamp：同步已有数据文件的截至时间（UNIX时间戳）
#  6. stat.total_upload_count：上传文件次数
#  7. stat.success_upload_count：成功上传文件次数
#  8. stat.total_set_meta_count：更改meta data次数
#  9. stat.success_set_meta_count：成功更改meta data次数
#  10. stat.total_delete_count：删除文件次数
#  11. stat.success_delete_count：成功删除文件次数
#  12. stat.total_download_count：下载文件次数
#  13. stat.success_download_count：成功下载文件次数
#  14. stat.total_get_meta_count：获取meta data次数
#  15. stat.success_get_meta_count：成功获取meta data次数
#  16. stat.last_source_update：最近一次源头更新时间（更新操作来自客户端）
#  17. stat.last_sync_update：最近一次同步更新时间（更新操作来自其他storage server的同步）

# 系统提供服务时的最大连接数。对于V1.x，因一个连接由一个线程服务，也就是工作线程数。
# 对于V2.x，最大连接数和工作线程数没有任何关系
# max concurrent connections this server supported
max_connections=256

# work thread count, should <= max_connections
# default value is 4
# since V2.00
# V2.0引入的这个参数，工作线程数，通常设置为CPU数
work_threads=4

# 上传组(卷) 的方式 0:轮询方式 1: 指定组 2: 平衡负载(选择最大剩余空间的组(卷)上传)
# 这里如果在应用层指定了上传到一个固定组,那么这个参数被绕过
# the method of selecting group to upload files
# 0: round robin
# 1: specify group
# 2: load balance, select the max free space group to upload file
store_lookup=2

# 当上一个参数设定为1 时 (store_lookup=1，即指定组名时)，必须设置本参数为系统中存在的一个组名。如果选择其他的上传方式，这个参数就没有效了
# which group to upload file
# when store_lookup set to 1, must set store_group to the group name
store_group=group2

# 选择哪个storage server 进行上传操作(一个文件被上传后，这个storage server就相当于这个文件的storage server源，会对同组的storage server推送这个文件达到同步效果)
# 0: 轮询方式 
# 1: 根据ip 地址进行排序选择第一个服务器（IP地址最小者）
# 2: 根据优先级进行排序（上传优先级由storage server来设置，参数名为upload_priority）
# which storage server to upload file
# 0: round robin (default)
# 1: the first server order by ip address
# 2: the first server order by priority (the minimal)
store_server=0

# 选择storage server 中的哪个目录进行上传。storage server可以有多个存放文件的base path（可以理解为多个磁盘）。
# 0: 轮流方式，多个目录依次存放文件
# 2: 选择剩余空间最大的目录存放文件（注意：剩余磁盘空间是动态的，因此存储到的目录或磁盘可能也是变化的）
# which path(means disk or mount point) of the storage server to upload file
# 0: round robin
# 2: load balance, select the max free space path to upload file
store_path=0

# 选择哪个 storage server 作为下载服务器 
# 0: 轮询方式，可以下载当前文件的任一storage server
# 1: 哪个为源storage server 就用哪一个 (前面说过了这个storage server源 是怎样产生的) 就是之前上传到哪个storage server服务器就是哪个了
# which storage server to download file
# 0: round robin (default)
# 1: the source storage server which the current file uploaded to
download_server=0

# storage server 上保留的空间，保证系统或其他应用需求空间。可以用绝对值或者百分比（V4开始支持百分比方式）。
#(指出 如果同组的服务器的硬盘大小一样,以最小的为准,也就是只要同组中有一台服务器达到这个标准了,这个标准就生效,原因就是因为他们进行备份)
# reserved storage space for system or other applications.
# if the free(available) space of any stoarge server in 
# a group <= reserved_storage_space, 
# no file can be uploaded to this group.
# bytes unit can be one of follows:
### G or g for gigabyte(GB)
### M or m for megabyte(MB)
### K or k for kilobyte(KB)
### no unit for byte(B)
### XX.XX% as ratio such as reserved_storage_space = 10%
reserved_storage_space = 10%

# 选择日志级别
#standard log level as syslog, case insensitive, value list:
### emerg for emergency
### alert
### crit for critical
### error
### warn for warning
### notice
### info
### debug
log_level=info

# 操作系统运行FastDFS的用户组 (不填 就是当前用户组,哪个启动进程就是哪个)
#unix group name to run this program, 
#not set (empty) means run by the group of current user
run_by_group=

# 操作系统运行FastDFS的用户 (不填 就是当前用户,哪个启动进程就是哪个)
#unix username to run this program,
#not set (empty) means run by current user
run_by_user=

# 可以连接到此 tracker server 的ip范围（对所有类型的连接都有影响，包括客户端，storage server）
# allow_hosts can ocur more than once, host can be hostname or ip address,
# "*" means match all ip addresses, can use range like this: 10.0.1.[1-15,20] or
# host[01-08,20-25].domain.com, for example:
# allow_hosts=10.0.1.[1-15,20]
# allow_hosts=host[01-08,20-25].domain.com
allow_hosts=*

# 同步或刷新日志信息到硬盘的时间间隔，单位为秒
# 注意：tracker server 的日志不是时时写硬盘的，而是先写内存
# sync log buff to disk every interval seconds
# default value is 10 seconds
sync_log_buff_interval = 10

# 检测 storage server 存活的时间隔，单位为秒。
# storage server定期向tracker server 发心跳，如果tracker server在一个check_active_interval内还没有收到storage server的一次心跳，那边将认为该storage server已经下线。所以本参数值必须大于storage server配置的心跳时间间隔。通常配置为storage server心跳时间间隔的2倍或3倍
# check storage server alive interval seconds
check_active_interval = 120

# 线程栈的大小。FastDFS server端采用了线程方式。tracker server线程栈不应小于64KB
# 线程栈越大，一个线程占用的系统资源就越多。如果要启动更多的线程（V1.x对应的参数为max_connections，
V2.0为work_threads），可以适当降低本参数值
# thread stack size, should >= 64KB
# default value is 64KB
thread_stack_size = 64KB

# 这个参数控制当storage server IP地址改变时，集群是否自动调整。注：只有在storage server进程重启时才完成自动调整
# auto adjust when the ip address of the storage server changed
# default value is true
storage_ip_changed_auto_adjust = true

# V2.0引入的参数。存储服务器之间同步文件的最大延迟时间，缺省为1天。根据实际情况进行调整
# 注：本参数并不影响文件同步过程。本参数仅在下载文件时，判断文件是否已经被同步完成的一个阀值（经验值）
# storage sync file max delay seconds
# default value is 86400 seconds (one day)
# since V2.00
storage_sync_file_max_delay = 86400

# V2.0引入的参数。存储服务器同步一个文件需要消耗的最大时间，缺省为300s，即5分钟。
# 注：本参数并不影响文件同步过程。本参数仅在下载文件时，作为判断当前文件是否被同步完成的一个阀值（经验值）
# the max time of storage sync a file
# default value is 300 seconds
# since V2.00
storage_sync_file_max_time = 300

# V3.0引入的参数。是否使用小文件合并存储特性，缺省是关闭的
# if use a trunk file to store several small files
# default value is false
# since V3.00
use_trunk_file = false 

# V3.0引入的参数。
# trunk file分配的最小字节数。比如文件只有16个字节，系统也会分配slot_min_size个字节
# the min slot size, should <= 4KB
# default value is 256 bytes
# since V3.00
slot_min_size = 256

# V3.0引入的参数。
# 只有文件大小<=这个参数值的文件，才会合并存储。如果一个文件的大小大于这个参数值，将直接保存到一个文件中（即不采用合并存储方式）。
# the max slot size, should > slot_min_size
# store the upload file to trunk file when it's size <=  this value
# default value is 16MB
# since V3.00
slot_max_size = 16MB

# V3.0引入的参数。
# 合并存储的trunk file大小，至少4MB，缺省值是64MB。不建议设置得过大
# the trunk file size, should >= 4MB
# default value is 64MB
# since V3.00
trunk_file_size = 64MB

# 是否提前创建trunk file。只有当这个参数为true，下面3个以trunk_create_file_打头的参数才有效
# if create trunk file advancely
# default value is false
# since V3.06
trunk_create_file_advance = false

# 提前创建trunk file的起始时间点（基准时间），02:00表示第一次创建的时间点是凌晨2点
# the time base to create trunk file
# the time format: HH:MM
# default value is 02:00
# since V3.06
trunk_create_file_time_base = 02:00

# 创建trunk file的时间间隔，单位为秒。如果每天只提前创建一次，则设置为86400
# the interval of create trunk file, unit: second
# default value is 38400 (one day)
# since V3.06
trunk_create_file_interval = 86400

# 提前创建trunk file时，需要达到的空闲trunk大小
# 比如本参数为20G，而当前空闲trunk为4GB，那么只需要创建16GB的trunk file即可
# the threshold to create trunk file
# when the free trunk file size less than the threshold, will create 
# the trunk files
# default value is 0
# since V3.06
trunk_create_file_space_threshold = 20G

# trunk初始化时，是否检查可用空间是否被占用
# if check trunk space occupying when loading trunk free spaces
# the occupied spaces will be ignored
# default value is false
# since V3.09
# NOTICE: set this parameter to true will slow the loading of trunk spaces 
# when startup. you should set this parameter to true when neccessary.
trunk_init_check_occupying = false

# 是否无条件从trunk binlog中加载trunk可用空间信息
# FastDFS缺省是从快照文件storage_trunk.dat中加载trunk可用空间，
# 该文件的第一行记录的是trunk binlog的offset，然后从binlog的offset开始加载
# if ignore storage_trunk.dat, reload from trunk binlog
# default value is false
# since V3.10
# set to true once for version upgrade when your version less than V3.10
trunk_init_reload_from_binlog = false

# 是否使用server ID作为storage server标识
# if use storage ID instead of IP address
# default value is false
# since V4.00
use_storage_id = false

# use_storage_id 设置为true，才需要设置本参数
# 在文件中设置组名、server ID和对应的IP地址，参见源码目录下的配置示例：conf/storage_ids.conf
# specify storage ids filename, can use relative or absolute path
# since V4.00
storage_ids_filename = storage_ids.conf

#文件名中的id类型，有ip和id两种，只有当use_storage_id设置为true时该参数才有效
# id type of the storage server in the filename, values are:
## ip: the ip address of the storage server
## id: the server id of the storage server
# this paramter is valid only when use_storage_id set to true
# default value is ip
# since V4.03
id_type_in_filename = ip

# 存储从文件是否采用symbol link（符号链接）方式
# 如果设置为true，一个从文件将占用两个文件：原始文件及指向它的符号链接
# if store slave file use symbol link
# default value is false
# since V4.01
store_slave_file_use_link = false

# 是否定期轮转error log，目前仅支持一天轮转一次
# if rotate the error log every day
# default value is false
# since V4.02
rotate_error_log = false

# error log定期轮转的时间点，只有当rotate_error_log设置为true时有效
# rotate error log time base, time format: Hour:Minute
# Hour from 0 to 23, Minute from 0 to 59
# default value is 00:00
# since V4.02
error_log_rotate_time=00:00

# error log按大小轮转
# 设置为0表示不按文件大小轮转，否则当error log达到该大小，就会轮转到新文件中
# rotate error log when the log file exceeds this size
# 0 means never rotates log file by log file size
# default value is 0
# since V4.02
rotate_error_log_size = 0

# 是否使用连接池
# if use connection pool
# default value is false
# since V4.05
use_connection_pool = false

# 如果一个连接的空闲时间超过这个值将会被自动关闭
# connections whose the idle time exceeds this time will be closed
# unit: second
# default value is 3600
# since V4.05
connection_pool_max_idle_time = 3600

# 用于提供http服务的端口
# HTTP port on this tracker server
http.server_port=8080

# 检查http server是否还在工作的时间间隔，如果该值小于0则永远不检查
# check storage HTTP server alive interval seconds
# <= 0 for never check
# default value is 30
http.check_alive_interval=30

# 检查http server是否存活的类型，有tcp和http两种
# tcp方式只有http端口被连接
# http方式检查必须返回状态值200
# check storage HTTP server alive type, values are:
#   tcp : connect to the storge server with HTTP port only, 
#        do not request and get response
#   http: storage check alive url must return http status 200
# default value is tcp
http.check_alive_type=tcp

# check storage HTTP server alive uri/url
# NOTE: storage embed HTTP server support uri: /status.html
http.check_alive_uri=/status.html
```
根据上述配置文件的详细说明，主要需要修改的内容为：
```shell
bind_addr=192.168.45.135
base_path=/home/fastdfs/tracker
```
然后创建这个文件夹：
```shell
mkdir -p /home/fastdfs/tracker
```
其他的配置项可以在实际使用中逐步根据需求进行修改。
确定修改后就启动tracker服务了：
```shell
/usr/bin/fdfs_trackerd /etc/fdfs/tracker.conf restart
```
如果添加了服务，可以从服务进行重启：
```shell
/sbin/service fdfs_trackerd start
```
然后检查当前的tracker路径下是否存在log文件，来检查是否启动成功：
```shell
cat /home/fastdfs/tracker/logs/trackerd.log
```
回显结果为：
```shell
[2016-09-21 11:01:56] INFO - FastDFS v5.08, base_path=/home/fastdfs/tracker, run_by_group=, run_by_user=, connect_timeout=30s, network_timeout=60s, port=22122, bind_addr=192.168.45.135, max_connections=256, accept_threads=1, work_threads=4, store_lookup=2, store_group=, store_server=0, store_path=0, reserved_storage_space=10.00%, download_server=0, allow_ip_count=-1, sync_log_buff_interval=10s, check_active_interval=120s, thread_stack_size=64 KB, storage_ip_changed_auto_adjust=1, storage_sync_file_max_delay=86400s, storage_sync_file_max_time=300s, use_trunk_file=0, slot_min_size=256, slot_max_size=16 MB, trunk_file_size=64 MB, trunk_create_file_advance=0, trunk_create_file_time_base=02:00, trunk_create_file_interval=86400, trunk_create_file_space_threshold=20 GB, trunk_init_check_occupying=0, trunk_init_reload_from_binlog=0, trunk_compress_binlog_min_interval=0, use_storage_id=0, id_type_in_filename=ip, storage_id_count=0, rotate_error_log=0, error_log_rotate_time=00:00, rotate_error_log_size=0, log_file_keep_days=0, store_slave_file_use_link=0, use_connection_pool=0, g_connection_pool_max_idle_time=3600s
```
表示当前的tracker已经启动成功。

#### 2.3.2 配置启动storage：
编辑/etc/fsfd下的storage配置项，基本配置说明为：
```shell
# 这个配置文件是否无效，false表示有效
# is this config file disabled
# false for enabled
# true for disabled
disabled=false

# 指定 此 storage server 所在组(卷)
# the name of the group this storage server belongs to
group_name=group1

# 是否绑定IP
# bind_addr= 后面为绑定的IP地址 (常用于服务器有多个IP但只希望一个IP提供服务)。如果不填则表示所有的(一般不填就OK)
# bind an address of this host
# empty for bind all addresses of this host
bind_addr=

# bind_addr通常是针对server的。当指定bind_addr时，本参数才有效。
# 本storage server作为client连接其他服务器（如tracker server、其他storage server），是否绑定bind_addr。
# if bind an address of this host when connect to other servers 
# (this storage server as a client)
# true for binding the address configed by above parameter: "bind_addr"
# false for binding any address of this host
client_bind=true

# storage server服务端口
# the storage server port
port=23000

# 连接超时时间，针对socket套接字函数connect
# connect timeout in seconds
# default value is 30s
connect_timeout=30

# storage server 网络超时时间，单位为秒。发送或接收数据时，如果在超时时间后还不能发送或接收数据，则本次网络通信失败。
# network timeout in seconds
# default value is 30s
network_timeout=60

# 心跳间隔时间，单位为秒 (这里是指主动向tracker server 发送心跳)
# heart beat interval in seconds
heart_beat_interval=30

# storage server向tracker server报告磁盘剩余空间的时间间隔，单位为秒
# disk usage report interval in seconds
stat_report_interval=60

# base_path 目录地址,根目录必须存在  子目录会自动生成 (注 :这里不是上传的文件存放的地址,之前是的,在某个版本后更改了)
# the base path to store data and log files
base_path=/home/yuqing/fastdfs

# 系统提供服务时的最大连接数
# max concurrent connections the server supported
# default value is 256
# more max_connections means more memory will be used
max_connections=256

# V2.0引入本参数。设置队列结点的buffer大小。工作队列消耗的内存大小 = buff_size * max_connections
# 设置得大一些，系统整体性能会有所提升。
# 消耗的内存请不要超过系统物理内存大小。另外，对于32位系统，请注意使用到的内存不要超过3GB
# the buff size to recv / send data
# this parameter must more than 8KB
# default value is 64KB
# since V2.00
buff_size = 256KB

# 工作线程的数量，工作线程用于处理网络IO，应当小于max_connections的值
# work thread count, should <= max_connections
# work thread deal network io
# default value is 4
# since V2.00
work_threads=4

# V2.0引入本参数。磁盘IO读写是否分离，缺省是分离的
# if disk read / write separated
##  false for mixed read and write
##  true for separated read and write
# default value is true
# since V2.00
disk_rw_separated = true

# V2.0引入本参数。针对单个存储路径的读线程数，缺省值为1。
# 读写分离时，系统中的读线程数 = disk_reader_threads * store_path_count
# 读写混合时，系统中的读写线程数 = (disk_reader_threads + disk_writer_threads) * store_path_count
# disk reader thread count per store base path
# for mixed read / write, this parameter can be 0
# default value is 1
# since V2.00
disk_reader_threads = 1

# V2.0引入本参数。针对单个存储路径的写线程数，缺省值为1。
# 读写分离时，系统中的写线程数 = disk_writer_threads * store_path_count
# 读写混合时，系统中的读写线程数 = (disk_reader_threads + disk_writer_threads) * store_path_count
# disk writer thread count per store base path
# for mixed read / write, this parameter can be 0
# default value is 1
# since V2.00
disk_writer_threads = 1

# 同步文件时，如果从binlog中没有读到要同步的文件，休眠N毫秒后重新读取。0表示不休眠，立即再次尝试读取。
# 出于CPU消耗考虑，不建议设置为0。如何希望同步尽可能快一些，可以将本参数设置得小一些，比如设置为10ms
# when no entry to sync, try read binlog again after X milliseconds
# must > 0, default value is 200ms
sync_wait_msec=50

# 同步上一个文件后，再同步下一个文件的时间间隔，单位为毫秒，0表示不休眠，直接同步下一个文件。
# after sync a file, usleep milliseconds
# 0 for sync successively (never call usleep)
sync_interval=0

# 下面二个一起解释。允许系统同步的时间段 (默认是全天) 。一般用于避免高峰同步产生一些问题而设定，相信sa都会明白
# storage sync start time of a day, time format: Hour:Minute
# Hour from 0 to 23, Minute from 0 to 59
sync_start_time=00:00

# storage sync end time of a day, time format: Hour:Minute
# Hour from 0 to 23, Minute from 0 to 59
sync_end_time=23:59

# 同步完N个文件后，把storage的mark文件同步到磁盘
# 注：如果mark文件内容没有变化，则不会同步
# write to the mark file after sync N files
# default value is 500
write_mark_file_freq=500

# 存放文件时storage server支持多个路径（例如磁盘）。这里配置存放文件的基路径数目，通常只配一个目录。
# path(disk or mount point) count, default value is 1
store_path_count=1

# 逐一配置store_path个路径，索引号基于0。注意配置方法后面有0,1,2 ......，需要配置0到store_path - 1。
# 如果不配置base_path0，那边它就和base_path对应的路径一样。
# store_path#, based 0, if store_path0 not exists, it's value is base_path
# the paths must be exist
store_path0=/home/yuqing/fastdfs
#store_path1=/home/yuqing/fastdfs2

# FastDFS存储文件时，采用了两级目录。这里配置存放文件的目录个数 (系统的存储机制,大家看看文件存储的目录就知道了)
# 如果本参数只为N（如：256），那么storage server在初次运行时，会自动创建 N * N 个存放文件的子目录。
# subdir_count  * subdir_count directories will be auto created under each 
# store_path (disk), value can be 1 to 256, default value is 256
subdir_count_per_path=256

# tracker_server 的列表 要写端口的哦 (再次提醒是主动连接tracker_server )
# 有多个tracker server时，每个tracker server写一行
# tracker_server can ocur more than once, and tracker_server format is
#  "host:port", host can be hostname or ip address
tracker_server=192.168.209.121:22122

# 日志级别
#standard log level as syslog, case insensitive, value list:
### emerg for emergency
### alert
### crit for critical
### error
### warn for warning
### notice
### info
### debug
log_level=info

# 操作系统运行FastDFS的用户组 (不填 就是当前用户组,哪个启动进程就是哪个)
#unix group name to run this program, 
#not set (empty) means run by the group of current user
run_by_group=

# 操作系统运行FastDFS的用户 (不填 就是当前用户,哪个启动进程就是哪个)
#unix username to run this program,
#not set (empty) means run by current user
run_by_user=

# 允许连接本storage server的IP地址列表 （不包括自带HTTP服务的所有连接）
# 可以配置多行，每行都会起作用
# allow_hosts can ocur more than once, host can be hostname or ip address,
# "*" means match all ip addresses, can use range like this: 10.0.1.[1-15,20] or
# host[01-08,20-25].domain.com, for example:
# allow_hosts=10.0.1.[1-15,20]
# allow_hosts=host[01-08,20-25].domain.com
allow_hosts=*

#  文件在data目录下分散存储策略。
# 0: 轮流存放，在一个目录下存储设置的文件数后（参数file_distribute_rotate_count中设置文件数），使用下一个目录进行存储。
# 1: 随机存储，根据文件名对应的hash code来分散存储。
# the mode of the files distributed to the data path
# 0: round robin(default)
# 1: random, distributted by hash code
file_distribute_path_mode=0

# 当上面的参数file_distribute_path_mode配置为0（轮流存放方式）时，本参数有效。
# 当一个目录下的文件存放的文件数达到本参数值时，后续上传的文件存储到下一个目录中。
# valid when file_distribute_to_path is set to 0 (round robin), 
# when the written file count reaches this number, then rotate to next path
# default value is 100
file_distribute_rotate_count=100

# 当写入大文件时，每写入N个字节，调用一次系统函数fsync将内容强行同步到硬盘。0表示从不调用fsync 
# call fsync to disk when write big file
# 0: never call fsync
# other: call fsync when written bytes >= this bytes
# default value is 0 (never call fsync)
fsync_after_written_bytes=0

# 同步或刷新日志信息到硬盘的时间间隔，单位为秒
# 注意：storage server 的日志信息不是时时写硬盘的，而是先写内存。
# sync log buff to disk every interval seconds
# must > 0, default value is 10 seconds
sync_log_buff_interval=10

# 同步binglog（更新操作日志）到硬盘的时间间隔，单位为秒
# 本参数会影响新上传文件同步延迟时间
# sync binlog buff / cache to disk every interval seconds
# default value is 60 seconds
sync_binlog_buff_interval=10

# 把storage的stat文件同步到磁盘的时间间隔，单位为秒。
# 注：如果stat文件内容没有变化，不会进行同步
# sync storage stat info to disk every interval seconds
# default value is 300 seconds
sync_stat_file_interval=300

# 线程栈的大小。FastDFS server端采用了线程方式。
# 对于V1.x，storage server线程栈不应小于512KB；对于V2.0，线程栈大于等于128KB即可。
# 线程栈越大，一个线程占用的系统资源就越多。
# 对于V1.x，如果要启动更多的线程（max_connections），可以适当降低本参数值。
# thread stack size, should >= 512KB
# default value is 512KB
thread_stack_size=512KB

# 本storage server作为源服务器，上传文件的优先级，可以为负数。值越小，优先级越高。这里就和 tracker.conf 中store_server= 2时的配置相对应了 
# the priority as a source server for uploading file.
# the lower this value, the higher its uploading priority.
# default value is 10
upload_priority=10

# 网卡别名前缀，就像Linux中的eth，可以使用ifconfig -a命令来查看
# 多个别名之间使用逗号分隔，如果不设置这个值表示自动的被系统类型设置
# the NIC alias prefix, such as eth in Linux, you can see it by ifconfig -a
# multi aliases split by comma. empty value means auto set by OS type
# default values is empty
if_alias_prefix=

# 是否检测上传文件已经存在。如果已经存在，则不存在文件内容，建立一个符号链接以节省磁盘空间。 
# 这个应用要配合FastDHT 使用，所以打开前要先安装FastDHT 
# 1或yes 是检测，0或no 是不检测
# if check file duplicate, when set to true, use FastDHT to store file indexes
# 1 or yes: need check
# 0 or no: do not check
# default value is 0
check_file_duplicate=0

# 文件去重时，文件内容的签名方式：
# hash： 4个hash code
# md5：MD5
# file signature method for check file duplicate
## hash: four 32 bits hash code
## md5: MD5 signature
# default value is hash
# since V4.01
file_signature_method=hash

# 当上个参数设定为1 或 yes时 (true/on也是可以的) ， 在FastDHT中的命名空间
# namespace for storing file indexes (key-value pairs)
# this item must be set when check_file_duplicate is true / on
key_namespace=FastDFS

# 与FastDHT servers 的连接方式 (是否为持久连接) ，默认是0（短连接方式）。可以考虑使用长连接，这要看FastDHT server的连接数是否够用。
# set keep_alive to 1 to enable persistent connection with FastDHT servers
# default value is 0 (short connection)
keep_alive=0

# 下面是关于FastDHT servers 的设定 需要对FastDHT servers 有所了解,这里只说字面意思了
# 可以通过 #include filename 方式来加载 FastDHT servers  的配置，装上FastDHT就知道该如何配置啦。
# 同样要求 check_file_duplicate=1 时才有用，不然系统会忽略
# fdht_servers.conf 记载的是 FastDHT servers 列表 
# you can use "#include filename" (not include double quotes) directive to 
# load FastDHT server list, when the filename is a relative path such as 
# pure filename, the base path is the base path of current/this config file.
# must set FastDHT server list when check_file_duplicate is true / on
# please see INSTALL of FastDHT for detail
##include /home/yuqing/fastdht/conf/fdht_servers.conf

# 是否将文件操作记录到access log
# if log to access log
# default value is false
# since V4.00
use_access_log = false

# 是否定期轮转access log，目前仅支持一天轮转一次
# if rotate the access log every day
# default value is false
# since V4.00
rotate_access_log = false

# access log定期轮转的时间点，只有当rotate_access_log设置为true时有效
# rotate access log time base, time format: Hour:Minute
# Hour from 0 to 23, Minute from 0 to 59
# default value is 00:00
# since V4.00
access_log_rotate_time=00:00

# 是否定期轮转error log，目前仅支持一天轮转一次
# if rotate the error log every day
# default value is false
# since V4.02
rotate_error_log = false

# error log定期轮转的时间点，只有当rotate_error_log设置为true时有效
# rotate error log time base, time format: Hour:Minute
# Hour from 0 to 23, Minute from 0 to 59
# default value is 00:00
# since V4.02
error_log_rotate_time=00:00

# access log按文件大小轮转
# 设置为0表示不按文件大小轮转，否则当access log达到该大小，就会轮转到新文件中
# rotate access log when the log file exceeds this size
# 0 means never rotates log file by log file size
# default value is 0
# since V4.02
rotate_access_log_size = 0

# error log按文件大小轮转
# 设置为0表示不按文件大小轮转，否则当error log达到该大小，就会轮转到新文件中
# rotate error log when the log file exceeds this size
# 0 means never rotates log file by log file size
# default value is 0
# since V4.02
rotate_error_log_size = 0

# 文件同步的时候，是否忽略无效的binlog记录
# if skip the invalid record when sync file
# default value is false
# since V4.02
file_sync_skip_invalid_record=false

# 是否使用连接池
# if use connection pool
# default value is false
# since V4.05
use_connection_pool = false

# 如果一个连接的空闲时间超过这个值将会被自动关闭
# connections whose the idle time exceeds this time will be closed
# unit: second
# default value is 3600
# since V4.05
connection_pool_max_idle_time = 3600

# storage server上web server域名，通常仅针对单独部署的web server。这样URL中就可以通过域名方式来访问storage server上的文件了，
# 这个参数为空就是IP地址的方式。
# use the ip address of this storage server if domain_name is empty,
# else this domain name will ocur in the url redirected by the tracker server
http.domain_name=

# web server的端口
# the port of the web server on this storage server
http.server_port=8888
```
可以看出，上述配置中，主要需要修改的内容为：
```shell
group_name=group1
bind_addr=192.168.45.135
base_path=/home/fastdfs/fdfs_data/storage       #存放日志路径
store_path0==/home/fastdfs/fdfs_data/sdata      #上传文件路径
tracker_server=192.168.45.135:22122             #tracker服务器地址
```
然后创建对应的目录：
```shell
mkdir -p /home/fastdfs/fdfs_data/storage
mkdir -p /home/fastdfs/fdfs_data/sdata
```
确定修改后，接着运行storage的存储服务：
```shell
/usr/bin/fdfs_storaged /etc/fdfs/storage.conf restart
```
或者按照服务进行启动：
```shell
/sbin/service fdfs_storaged start
```
然后还是通过检查当前storage的日志来检查是否正确的启动：
```shell
cat /home/fastdfs/fdfs_data/storage/logs/storaged.log
```
回显为：
```shell
[2016-09-21 11:20:33] INFO - FastDFS v5.08, base_path=/home/fastdfs/fdfs_data/storage, store_path_count=1, subdir_count_per_path=256, group_name=group1, run_by_group=, run_by_user=, connect_timeout=30s, network_timeout=60s, port=23000, bind_addr=192.168.45.135, client_bind=1, max_connections=256, accept_threads=1, work_threads=4, disk_rw_separated=1, disk_reader_threads=1, disk_writer_threads=1, buff_size=256KB, heart_beat_interval=30s, stat_report_interval=60s, tracker_server_count=1, sync_wait_msec=50ms, sync_interval=0ms, sync_start_time=00:00, sync_end_time=23:59, write_mark_file_freq=500, allow_ip_count=-1, file_distribute_path_mode=0, file_distribute_rotate_count=100, fsync_after_written_bytes=0, sync_log_buff_interval=10s, sync_binlog_buff_interval=10s, sync_stat_file_interval=300s, thread_stack_size=512 KB, upload_priority=10, if_alias_prefix=, check_file_duplicate=0, file_signature_method=hash, FDHT group count=0, FDHT server count=0, FDHT key_namespace=, FDHT keep_alive=0, HTTP server port=8888, domain name=, use_access_log=0, rotate_access_log=0, access_log_rotate_time=00:00, rotate_error_log=0, error_log_rotate_time=00:00, rotate_access_log_size=0, rotate_error_log_size=0, log_file_keep_days=0, file_sync_skip_invalid_record=0, use_connection_pool=0, g_connection_pool_max_idle_time=3600s
data path: /home/fastdfs/fdfs_data/sdata/data, mkdir sub dir...
mkdir data path: 00 ...
mkdir data path: 01 ...
mkdir data path: 02 ...
mkdir data path: 03 ...
mkdir data path: 04 ...
mkdir data path: 05 ...
mkdir data path: 06 ...
mkdir data path: 07 ...
mkdir data path: 08 ...
mkdir data path: 09 ...
mkdir data path: 0A ...
mkdir data path: 0B ...
mkdir data path: 0C ...
mkdir data path: 0D ...
mkdir data path: 0E ...
mkdir data path: 0F ...
mkdir data path: 10 ...
mkdir data path: 11 ...
mkdir data path: 12 ...
mkdir data path: 13 ...
mkdir data path: 14 ...
mkdir data path: 15 ...
mkdir data path: 16 ...
mkdir data path: 17 ...
mkdir data path: 18 ...
mkdir data path: 19 ...
mkdir data path: 1A ...
mkdir data path: 1B ...
mkdir data path: 1C ...
mkdir data path: 1D ...
mkdir data path: 1E ...
mkdir data path: 1F ...
mkdir data path: 20 ...
mkdir data path: 21 ...
mkdir data path: 22 ...
mkdir data path: 23 ...
mkdir data path: 24 ...
mkdir data path: 25 ...
mkdir data path: 26 ...
mkdir data path: 27 ...
mkdir data path: 28 ...
mkdir data path: 29 ...
mkdir data path: 2A ...
mkdir data path: 2B ...
mkdir data path: 2C ...
mkdir data path: 2D ...
mkdir data path: 2E ...
mkdir data path: 2F ...
mkdir data path: 30 ...
mkdir data path: 31 ...
mkdir data path: 32 ...
mkdir data path: 33 ...
mkdir data path: 34 ...
mkdir data path: 35 ...
mkdir data path: 36 ...
mkdir data path: 37 ...
mkdir data path: 38 ...
mkdir data path: 39 ...
mkdir data path: 3A ...
mkdir data path: 3B ...
mkdir data path: 3C ...
mkdir data path: 3D ...
mkdir data path: 3E ...
mkdir data path: 3F ...
mkdir data path: 40 ...
mkdir data path: 41 ...
mkdir data path: 42 ...
mkdir data path: 43 ...
mkdir data path: 44 ...
mkdir data path: 45 ...
mkdir data path: 46 ...
mkdir data path: 47 ...
mkdir data path: 48 ...
mkdir data path: 49 ...
mkdir data path: 4A ...
mkdir data path: 4B ...
mkdir data path: 4C ...
mkdir data path: 4D ...
mkdir data path: 4E ...
mkdir data path: 4F ...
mkdir data path: 50 ...
mkdir data path: 51 ...
mkdir data path: 52 ...
mkdir data path: 53 ...
mkdir data path: 54 ...
mkdir data path: 55 ...
mkdir data path: 56 ...
mkdir data path: 57 ...
mkdir data path: 58 ...
mkdir data path: 59 ...
mkdir data path: 5A ...
mkdir data path: 5B ...
mkdir data path: 5C ...
mkdir data path: 5D ...
mkdir data path: 5E ...
mkdir data path: 5F ...
mkdir data path: 60 ...
mkdir data path: 61 ...
mkdir data path: 62 ...
mkdir data path: 63 ...
mkdir data path: 64 ...
mkdir data path: 65 ...
mkdir data path: 66 ...
mkdir data path: 67 ...
mkdir data path: 68 ...
mkdir data path: 69 ...
mkdir data path: 6A ...
mkdir data path: 6B ...
mkdir data path: 6C ...
mkdir data path: 6D ...
mkdir data path: 6E ...
mkdir data path: 6F ...
mkdir data path: 70 ...
mkdir data path: 71 ...
mkdir data path: 72 ...
mkdir data path: 73 ...
mkdir data path: 74 ...
mkdir data path: 75 ...
mkdir data path: 76 ...
mkdir data path: 77 ...
mkdir data path: 78 ...
mkdir data path: 79 ...
mkdir data path: 7A ...
mkdir data path: 7B ...
mkdir data path: 7C ...
mkdir data path: 7D ...
mkdir data path: 7E ...
mkdir data path: 7F ...
mkdir data path: 80 ...
mkdir data path: 81 ...
mkdir data path: 82 ...
mkdir data path: 83 ...
mkdir data path: 84 ...
mkdir data path: 85 ...
mkdir data path: 86 ...
mkdir data path: 87 ...
mkdir data path: 88 ...
mkdir data path: 89 ...
mkdir data path: 8A ...
mkdir data path: 8B ...
mkdir data path: 8C ...
mkdir data path: 8D ...
mkdir data path: 8E ...
mkdir data path: 8F ...
mkdir data path: 90 ...
mkdir data path: 91 ...
mkdir data path: 92 ...
mkdir data path: 93 ...
mkdir data path: 94 ...
mkdir data path: 95 ...
mkdir data path: 96 ...
mkdir data path: 97 ...
mkdir data path: 98 ...
mkdir data path: 99 ...
mkdir data path: 9A ...
mkdir data path: 9B ...
mkdir data path: 9C ...
mkdir data path: 9D ...
mkdir data path: 9E ...
mkdir data path: 9F ...
mkdir data path: A0 ...
mkdir data path: A1 ...
mkdir data path: A2 ...
mkdir data path: A3 ...
mkdir data path: A4 ...
mkdir data path: A5 ...
mkdir data path: A6 ...
mkdir data path: A7 ...
mkdir data path: A8 ...
mkdir data path: A9 ...
mkdir data path: AA ...
mkdir data path: AB ...
mkdir data path: AC ...
mkdir data path: AD ...
mkdir data path: AE ...
mkdir data path: AF ...
mkdir data path: B0 ...
mkdir data path: B1 ...
mkdir data path: B2 ...
mkdir data path: B3 ...
mkdir data path: B4 ...
mkdir data path: B5 ...
mkdir data path: B6 ...
mkdir data path: B7 ...
mkdir data path: B8 ...
mkdir data path: B9 ...
mkdir data path: BA ...
mkdir data path: BB ...
mkdir data path: BC ...
mkdir data path: BD ...
mkdir data path: BE ...
mkdir data path: BF ...
mkdir data path: C0 ...
mkdir data path: C1 ...
mkdir data path: C2 ...
mkdir data path: C3 ...
mkdir data path: C4 ...
mkdir data path: C5 ...
mkdir data path: C6 ...
mkdir data path: C7 ...
mkdir data path: C8 ...
mkdir data path: C9 ...
mkdir data path: CA ...
mkdir data path: CB ...
mkdir data path: CC ...
mkdir data path: CD ...
mkdir data path: CE ...
mkdir data path: CF ...
mkdir data path: D0 ...
mkdir data path: D1 ...
mkdir data path: D2 ...
mkdir data path: D3 ...
mkdir data path: D4 ...
mkdir data path: D5 ...
mkdir data path: D6 ...
mkdir data path: D7 ...
mkdir data path: D8 ...
mkdir data path: D9 ...
mkdir data path: DA ...
mkdir data path: DB ...
mkdir data path: DC ...
mkdir data path: DD ...
mkdir data path: DE ...
mkdir data path: DF ...
mkdir data path: E0 ...
mkdir data path: E1 ...
mkdir data path: E2 ...
mkdir data path: E3 ...
mkdir data path: E4 ...
mkdir data path: E5 ...
mkdir data path: E6 ...
mkdir data path: E7 ...
mkdir data path: E8 ...
mkdir data path: E9 ...
mkdir data path: EA ...
mkdir data path: EB ...
mkdir data path: EC ...
mkdir data path: ED ...
mkdir data path: EE ...
mkdir data path: EF ...
mkdir data path: F0 ...
mkdir data path: F1 ...
mkdir data path: F2 ...
mkdir data path: F3 ...
mkdir data path: F4 ...
mkdir data path: F5 ...
mkdir data path: F6 ...
mkdir data path: F7 ...
mkdir data path: F8 ...
mkdir data path: F9 ...
mkdir data path: FA ...
mkdir data path: FB ...
mkdir data path: FC ...
mkdir data path: FD ...
mkdir data path: FE ...
mkdir data path: FF ...
data path: /home/fastdfs/fdfs_data/sdata/data, mkdir sub dir done.
[2016-09-21 11:20:36] INFO - file: storage_param_getter.c, line: 191, use_storage_id=0, id_type_in_filename=ip, storage_ip_changed_auto_adjust=1, store_path=0, reserved_storage_space=10.00%, use_trunk_file=0, slot_min_size=256, slot_max_size=16 MB, trunk_file_size=64 MB, trunk_create_file_advance=0, trunk_create_file_time_base=02:00, trunk_create_file_interval=86400, trunk_create_file_space_threshold=20 GB, trunk_init_check_occupying=0, trunk_init_reload_from_binlog=0, trunk_compress_binlog_min_interval=0, store_slave_file_use_link=0
[2016-09-21 11:20:36] INFO - file: storage_func.c, line: 254, tracker_client_ip: 192.168.45.135, my_server_id_str: 192.168.45.135, g_server_id_in_filename: -2027050816
[2016-09-21 11:20:37] INFO - file: tracker_client_thread.c, line: 310, successfully connect to tracker server 192.168.45.135:22122, as a tracker client, my ip is 192.168.45.135
[2016-09-21 11:21:07] INFO - file: tracker_client_thread.c, line: 1235, tracker server 192.168.45.135:22122, set tracker leader: 192.168.45.135:22122
```
可以看到当前storage启动的时候初始化了存储目录。

#### 2.3.3 配置client：
默认的配置文件项的解释为：
```shell
# 连接超时时间，针对socket套接字函数connect
# connect timeout in seconds
# default value is 30s
connect_timeout=30

# client的网络超时，单位为秒。发送或接收数据时，如果在超时时间后还不能发送或接收数据，则本次网络通信失败
# network timeout in seconds
# default value is 30s
network_timeout=60

# 存储日志的根目录
# the base path to store log files
base_path=/home/yuqing/fastdfs

# tracker_server 的列表 要写端口
# tracker_server can ocur more than once, and tracker_server format is
#  "host:port", host can be hostname or ip address
tracker_server=192.168.0.197:22122

# 日志的级别
#standard log level as syslog, case insensitive, value list:
### emerg for emergency
### alert
### crit for critical
### error
### warn for warning
### notice
### info
### debug
log_level=info

# 是否使用连接池
# if use connection pool
# default value is false
# since V4.05
use_connection_pool = false

# 如果一个连接的空闲时间超过这个值将会被自动关闭
# connections whose the idle time exceeds this time will be closed
# unit: second
# default value is 3600
# since V4.05
connection_pool_max_idle_time = 3600

# 是否从FastDFS的tracker server加载参数
# if load FastDFS parameters from tracker server
# since V4.05
# default value is false
load_fdfs_parameters_from_tracker=false

是否使用storage ID 代替IP，只有当load_fdfs_parameters_from_tracker为false时才有效
# if use storage ID instead of IP address
# same as tracker.conf
# valid only when load_fdfs_parameters_from_tracker is false
# default value is false
# since V4.05
use_storage_id = false

# 指定storage_ids的路径，可以使用绝对路径和相对路径，只有当load_fdfs_parameters_from_tracker为false时才有效
# specify storage ids filename, can use relative or absolute path
# same as tracker.conf
# valid only when load_fdfs_parameters_from_tracker is false
# since V4.05
storage_ids_filename = storage_ids.conf


#tracker server的http端口
#HTTP settings
http.tracker_server_port=8080

#use "#include" directive to include HTTP other settiongs
##include http.conf
```
根据上述内容，主要修改的内容为：
```shell
base_path=/home/fastdfs/client
tracker_server=192.168.45.135:22122
```
然后创建对应的文件夹：
```shell
mkdir -p /home/fastdfs/client
```

#### 2.3.4 运行测试程序：
上述服务启动完毕之后，运行测试程序看是否正确的运行。
按照如下的格式进行启动：
```shell
/usr/bin/fdfs_test <client_conf_filename> <operation>
/usr/bin/fdfs_test1 <client_conf_filename> <operation>
```
例如可以使用如下命令进行测试（）：
```shell
/usr/bin/fdfs_test /etc/fdfs/client.conf upload /usr/include/stdlib.h
```
回显结果为：
```shell
This is FastDFS client test program v5.08

Copyright (C) 2008, Happy Fish / YuQing

FastDFS may be copied only under the terms of the GNU General
Public License V3, which may be found in the FastDFS source kit.
Please visit the FastDFS Home Page http://www.csource.org/ 
for more detail.

[2016-09-21 11:41:13] DEBUG - base_path=/home/fastdfs/client, connect_timeout=30, network_timeout=60, tracker_server_count=1, anti_steal_token=0, anti_steal_secret_key length=0, use_connection_pool=0, g_connection_pool_max_idle_time=3600s, use_storage_id=0, storage server id count: 0

tracker_query_storage_store_list_without_group: 
	server 1. group_name=, ip_addr=192.168.45.135, port=23000

group_name=group1, ip_addr=192.168.45.135, port=23000
storage_upload_by_filename
group_name=group1, remote_filename=M00/00/00/wKgth1fiAVmAJ89XAACE7ieMB5464804.h
source ip address: 192.168.45.135
file timestamp=2016-09-21 11:41:13
file size=34030
file crc32=663488414
example file url: http://192.168.45.135/group1/M00/00/00/wKgth1fiAVmAJ89XAACE7ieMB5464804.h
storage_upload_slave_by_filename
group_name=group1, remote_filename=M00/00/00/wKgth1fiAVmAJ89XAACE7ieMB5464804_big.h
source ip address: 192.168.45.135
file timestamp=2016-09-21 11:41:13
file size=34030
file crc32=663488414
example file url: http://192.168.45.135/group1/M00/00/00/wKgth1fiAVmAJ89XAACE7ieMB5464804_big.h
```
然后就可以在目录：
```shell
/home/fastdfs/fdfs_data/sdata/data/00/00
```
下面找到文件：
```shell
wKgth1fiAVmAJ89XAACE7ieMB5464804_big.h
wKgth1fiAVmAJ89XAACE7ieMB5464804_big.h-m
wKgth1fiAVmAJ89XAACE7ieMB5464804.h
wKgth1fiAVmAJ89XAACE7ieMB5464804.h-m
```
然后打开一个对比stdlib.h文件：
```shell
diff wKgth1fiAVmAJ89XAACE7ieMB5464804_big.h /usr/include/stdlib.h
diff wKgth1fiAVmAJ89XAACE7ieMB5464804.h /usr/include/stdlib.h
```
没有输出任何差异。表示上传测试成功。其中后缀为-m结尾的文件用于crc效验。


## 3 实现fastDFS的java调用：
因为项目的原因，我们目前采用java做整个系统的开发，所以需要在java环境中对fastDFS进行集成。具体的操作如下章节。

### 3.1 开启tracker和storage的端口可以被外部访问：
完成上述测试之后，在centos7中需要手动将tracker和storage的服务端口打开，否则无法正确的在外部访问。
首先，开放tracker监听端口访问，供外部服务调用：
```shell
firewall-cmd --zone=public --add-port=22122/tcp --permanent
firewall-cmd --reload
```
回显success之后表示开启端口成功。
然后，继续开放storage监听端口访问：
```shell
firewall-cmd --zone=public --add-port=23000/tcp --permanent
firewall-cmd --reload
```
回显success之后表示开启端口成功。

### 3.2 编译fastDFS的java调用接口：
因为fastDFS是基于C开发的程序，如果使用java调用，需要使用作者提供的java wrapper来进行操作。根据官方提供的[wrapper](https://github.com/happyfish100/fastdfs-client-java)，我们需要将这个wrapper编译为jar包来引入自己的java工程来进行开发集成。
#### 3.2.1 fastdfs-client-java的编译：
首先下载最新的wrapper代码：
```shell
git clone https://github.com/happyfish100/fastdfs-client-java.git
```
然后使用IDEA导入。
导入完毕后，选择maven项目的install，会自动下载编译相关的依赖库，然后在target目录下生成对应的jar包，并且会将这个jar包拷贝到本机的maven库目录下：
```shell
Installing D:\workspace\project\java_scripts\fastdfs-client-java\target\fastdfs-client-java-1.25.jar to C:\Users\wenta\.m2\repository\org\csource\fastdfs-client-java\1.25\fastdfs-client-java-1.25.jar
```
#### 3.2.2 gradle工程中引入这个本地的jar包来进行开发集成：
gradle中使用命令：
```shell
dependencies {
    compile files('D:\\workspace\\project\\java_scripts\\fastdfs-client-java\\target\\fastdfs-client-java-1.25.jar')
}
```
将本地路径的fastdfs-client-java-1.25.jar引入到当前自己的工程中。
> 也可以在自建的gradle库中添加这个jar包，但是感觉没有直接使用本地路径方便。

然后刷新gradle，就可以在自己的工程文件的外部依赖（External Libraries）中看到这个jar包了，并且可以打开这个jar包看到具体的代码。

### 3.3 编写java程序进行外部文件上传测试：
根据fastdfs-client-java的[官方示例](https://github.com/happyfish100/fastdfs-client-java/blob/master/src/org/csource/fastdfs/test/Test.java)，提取出主要的上传文件的ava操作代码为：
```java
// 读取配置文件：
String configFilePath = "/resources/main/config/fdfs_client.conf";

// 根据路径加载配置文件
try {
    ClientGlobal.init(configFilePath);
}catch(Exception e){
    e.printStackTrace();
}

// 设置本地要上传的文件路径：
String localFilePath = "D://test.jpg":
// 设置上传后的文件名称：
String uploadFileName = "test.jpg";

// 然后将本地文件转换为文件流
long fileLength = file.length();//获取文件的字节长度
byte[] buffer = new byte[256 * 1024];
byte[] fileBuffer = new byte[(int) fileLength];
int count = 0;
int length = 0;
while ((length = inStream.read(buffer)) != -1) {
    for (int i = 0; i < length; ++i) {
        fileBuffer[count + i] = buffer[i];
    }
    count += length;
}

// 检查当前的文件名是否合法：
String[] files = null;
String fileExtName = "";
if (uploadFileName.contains(".")) {
    fileExtName = uploadFileName.substring(uploadFileName.lastIndexOf(".") + 1);
} else {
    System.out.println("Fail to upload file, because the format of filename is illegal.");
    return null;
}

// 和fastDFS建立连接：
TrackerClient tracker = new TrackerClient();
TrackerServer trackerServer = tracker.getConnection();
StorageServer storageServer = null;
StorageClient client = new StorageClient(trackerServer, storageServer);//通过tracker服务器获取存储服务器

// 设置元信息
NameValuePair[] metaList = new NameValuePair[3];
metaList[0] = new NameValuePair("fileName", uploadFileName);
metaList[1] = new NameValuePair("fileExtName", fileExtName);
metaList[2] = new NameValuePair("fileLength", String.valueOf(fileLength));
        

// 然后将文件流交给fastDFS进行上传：
try {
    files = client.upload_file(fileBuff, fileExtName, metaList);
} catch (Exception e) {
    System.out.println("Upload file \"" + uploadFileName + "\"fails");
}

// 最后关闭tracker连接，结束操作
trackerServer.close();
return files;// 返回上传完毕的文件url路径
```
然后自己根据这个代码建立实际的测试工程代码，在IDEA中新建FastDFSTest类，然后填写一下代码：
```java
/**
 * Created by wentao on 2016/9/20.
 *
 * fastDFS的测试需要在当前测试环境部署对应的服务才能正确进行
 */

import org.csource.common.NameValuePair;
import org.csource.fastdfs.*;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;

public class FastDFSTest {

    /**
     * 上传文件
     */
    public static String[] uploadFile(File file, String uploadFileName, long fileLength) throws IOException {
        byte[] fileBuff = getFileBuffer(new FileInputStream(file), fileLength);
        String[] files = null;
        String fileExtName = "";
        if (uploadFileName.contains(".")) {
            fileExtName = uploadFileName.substring(uploadFileName.lastIndexOf(".") + 1);
        } else {
            System.out.println("Fail to upload file, because the format of filename is illegal.");
            return null;
        }

        // 建立连接
        TrackerClient tracker = new TrackerClient();
        TrackerServer trackerServer = tracker.getConnection();
        StorageServer storageServer = null;
        StorageClient client = new StorageClient(trackerServer, storageServer);

        // 设置元信息
        NameValuePair[] metaList = new NameValuePair[3];
        metaList[0] = new NameValuePair("fileName", uploadFileName);
        metaList[1] = new NameValuePair("fileExtName", fileExtName);
        metaList[2] = new NameValuePair("fileLength", String.valueOf(fileLength));

        // 上传文件
        try {
            files = client.upload_file(fileBuff, fileExtName, metaList);
        } catch (Exception e) {
            System.out.println("Upload file \"" + uploadFileName + "\"fails");
        }
        trackerServer.close();
        return files;
    }
    private static byte[] getFileBuffer(InputStream inStream, long fileLength) throws IOException {

        byte[] buffer = new byte[256 * 1024];
        byte[] fileBuffer = new byte[(int) fileLength];

        int count = 0;
        int length = 0;

        while ((length = inStream.read(buffer)) != -1) {
            for (int i = 0; i < length; ++i) {
                fileBuffer[count + i] = buffer[i];
            }
            count += length;
        }
        return fileBuffer;
    }


    public static void main(String[] args) throws Exception {
        // 获取配置文件的路径
        File cfg_root = new File(new File(new File(FastDFSTest.class.getResource("/").getPath()).getParent()).
                getParent()+ "/resources/main/config/fdfs_client.conf");

        // 根据路径加载配置文件
        try {
            ClientGlobal.init(cfg_root.getAbsolutePath());
        }catch(Exception e){
            e.printStackTrace();
        }

        // 测试文件：不能超过500M
        File file = new File("D:/CountingStars.mp4");
        String uploadFileName = "CountingStars.mp4";
        String[] files =  uploadFile(file, uploadFileName, file.length());
        System.out.println(Arrays.asList(files));//返回储存路径:group1 M00/00/00/wKhuW1Vmj6KAZ09pAAC9przUxEk788.jpg
    }
}
```
调试该程序，得到执行结果为：
```shell
[group1, M00/00/00/wKgth1fiJ-yAWqX2CAvnNEL5tsI595.mp4]
```
可以得到这个文件现在存储在storage标号为group1的卷上，存储位置为：
```shell
M00/00/00/wKgth1fiJ-yAWqX2CAvnNEL5tsI595.mp4
```
可以在:
```shell
/home/fastdfs/fdfs_data/sdata/data/00/00
```
路径下找到这个wKgth1fiJ-yAWqX2CAvnNEL5tsI595.mp4文件，表示上传成功。


*至此，基本的fastDFS环境搭建、测试和java程序开发的流程就结束了。*


## 4 对fastDFS的部署思考：

### 4.1 centos7下自动启动fastDFS服务：
将当前的fastDFS应用编写服务，在命令行下运行：
```shell
bash -c 'cat > /usr/lib/systemd/system/fdfs_storaged.service << EOF
[Unit]
Description=fastdfs storage server
After=network.target

[Service]
Type=forking
PIDFile=/data/fastdfs/data/fdfs_storaged.pid
ExecStart=/usr/bin/fdfs_storaged /etc/fdfs/storage.conf
ExecReload=/usr/bin/fdfs_storaged /etc/fdfs/storage.conf restart
ExecStop=/usr/bin/fdfs_storaged /etc/fdfs/storage.conf stop

[Install]
WantedBy=multi-user.target
EOF'
systemctl enable fdfs_storaged.service
systemctl start fdfs_storaged.service
```
这个脚本编写了一个服务启动描述，并且立即将服务启动。

### 4.2 脚本实现fastDFS的分发和部署：
根据第二章节编译部署fastDFS的总结发下，可以将编译好的文件包使用脚本进行分发，并且动态配置相关的配置文件内容。
目前希望通过运行在主机上的脚本来进行软件的分发和部署。