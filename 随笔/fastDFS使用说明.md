<!-- TOC -->

- [0 fastDFS作者和相关资料：](#0-fastdfs作者和相关资料)
- [1 关于fastDFS的代码：](#1-关于fastdfs的代码)
- [2 fastDFS的编译和java api的编译：](#2-fastdfs的编译和java-api的编译)
    - [2.1 编译安装libfastcommon：](#21-编译安装libfastcommon)
    - [2.2 编译安装fastDFS：](#22-编译安装fastdfs)
    - [2.3 配置启动fastDFS服务：](#23-配置启动fastdfs服务)
        - [2.3.1 配置启动tracker：](#231-配置启动tracker)
        - [2.3.2 配置启动storage：](#232-配置启动storage)
        - [2.3.3 配置client：](#233-配置client)
        - [2.3.4 运行测试程序：](#234-运行测试程序)

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
base_path=/home/fastdfs
```
然后创建这个文件夹：
```shell
mkdir /home/fastdfs
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
base_path=/home/fastdfs
store_path0=/home/fastdfs
tracker_server=192.168.209.121:22122
```
确定修改后，接着运行storage的存储服务：
```shell
/usr/bin/fdfs_storaged /etc/fdfs/storage.conf restart
```
或者按照服务进行启动：
```shell
/sbin/service fdfs_storaged start
```

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
base_path=/home/yuqing/fastdfs
tracker_server=192.168.0.197:22122
```

#### 2.3.4 运行测试程序：
上述服务启动完毕之后，运行测试程序看是否正确的运行。按照如下的格式进行启动：
```shell
/usr/bin/fdfs_test <client_conf_filename> <operation>
/usr/bin/fdfs_test1 <client_conf_filename> <operation>
```
例如可以使用如下命令进行测试（）：
```shell
/usr/bin/fdfs_test conf/client.conf upload /usr/include/stdlib.h

```
