<!-- TOC -->

- [Spark环境的搭建和测试：](#spark环境的搭建和测试)
    - [一 在Centos7集群中搭建Spark集群：](#一-在centos7集群中搭建spark集群)
        - [基本虚拟机镜像的制作过程：](#基本虚拟机镜像的制作过程)
            - [最小化安装完的组件安装：](#最小化安装完的组件安装)
            - [下载需要的官方安装包：](#下载需要的官方安装包)
            - [修改这个虚拟机的IP和hostname：](#修改这个虚拟机的ip和hostname)
            - [关闭防火墙：](#关闭防火墙)
            - [关闭SELINUX：](#关闭selinux)
        - [三个虚拟机的节点设置：](#三个虚拟机的节点设置)
            - [所有节点的SSH免密登陆：](#所有节点的ssh免密登陆)
            - [设置NTP服务：](#设置ntp服务)
        - [集群安装：](#集群安装)
            - [安装Hadoop集群：](#安装hadoop集群)
            - [安装Spark集群：](#安装spark集群)
    - [二 Spark集群的测试：](#二-spark集群的测试)
    - [三 MapReduce算法的实现和测试：](#三-mapreduce算法的实现和测试)

<!-- /TOC -->

# Spark环境的搭建和测试：
目前在开发中，越来越多的需求指向计算，也就是说建立在存储基础上的分布式计算任务也被新增的需求迫切的需要，基于当前这个状况和之前的规划（上层计算模块），决定使用Spark作为计算的核心平台，来扩展当前datasong的存储组件。

## 一 在Centos7集群中搭建Spark集群：
考虑到实际测试环境的便利性，采用vmware虚拟机中使用centos7来搭建整个Hadoop和Spark集群进行测试。

### 基本虚拟机镜像的制作过程：

#### 最小化安装完的组件安装：
yum -y install nano net-tools ntp
需要安装编辑文本的编辑器（nano），网络管理工具（net-tools）和NTP服务。

#### 下载需要的官方安装包：
首先，搭建整个集群需要的官方下载为：
[Centos7官方下载](https://www.centos.org/download/)
[Hadoop2.6官方下载](https://archive.apache.org/dist/hadoop/core/hadoop-2.6.0/)
[SCALA 2.10.5官方下载](https://www.scala-lang.org/download/2.10.5.html)
[JDK 8U121 官方下载](http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html)
[Spark官方下载](http://spark.apache.org/downloads.html)

在虚拟机中使用最小化安装完系统之后，安装JDK和Scala的rpm包，然后将Hadoop和Spark的tar包统一放在一个目录下（例如：/home/tools/ 目录下）。

需要注意的是，使用了rpm包虽然可以完成java的安装，但是没有添加到系统环境变量中，所以还是需要编辑/etc/profile文件：
```shell
export JAVA_HOME=/usr/java/jdk1.8.0_121
export JRE_HOME=/usr/java/jdk1.8.0_121/jre
export PATH=$JAVA_HOME/bin:$JRE_HOME/bin:$PATH
export CLASSPATH=.:$JAVA_HOME/lib/dt.jar:$JAVA_HOME/lib/tools.jar:$JRE_HOME/lib
```
同样的，也需要添加scala的环境变量，使用rpm包安装后的位置默认为：/usr/share/scala/bin/scala，然后编辑/etc/profile文件内容：
```shell

```
设置完毕后，更新profile：
```shell
source /etc/profile
```

> - 这样看来不如直接使用tar包进行安装，还更“绿色”一点。

#### 修改这个虚拟机的IP和hostname：
修改为静态IP：
```shell
nano /etc/sysconfig/network-script/ifcfg-ens33
```
然后编辑内容为：
```shell
TYPE=Ethernet
BOOTPROTO=static
DEFROUTE=yes
IPV4_FAILURE_FATAL=no
IPV6INIT=yes
IPV6_AUTOCONF=yes
IPV6_DEFROUTE=yes
IPV6_FAILURE_FATAL=no
IPV6_ADDR_GEN_MODE=stable-privacy
NAME=ens33
UUID=1123f1dc-0535-4f57-853b-82c642315aa4
DEVICE=ens33
ONBOOT=yes
PEERDNS=yes
PEERROUTES=yes
IPV6_PEERDNS=yes
IPV6_PEERROUTES=yes
IPADDR=192.168.184.129
NETMASK=255.255.255.0
GATEWAY=192.168.184.2
DNS1=192.168.184.2
```
然后修改host文件：
```shell
nano /etc/hosts
```
添加如下内容：
```shell
192.168.184.129	master
192.168.184.130	node1
192.168.184.131	node2
```

#### 关闭防火墙：
systemctl stop firewalld.service #停止firewall
systemctl disable firewalld.service #禁止firewall开机启动

参考文档：[Centos7 关闭防火墙](http://www.libenfu.com/index.php/archives/131/)

#### 关闭SELINUX：
使用命令：
getenforce
查看当前SELINUX的状态为：Enforcing
然后编辑 /etc/selinux/config 文档，修改内容为：
SELINUX=enforcing
改为：
SELINUX=disabled
保存后退出，重启电脑。

参考文档：[CentOS 7.x初始化](http://www.chenshake.com/centos-7-x-class/)

### 三个虚拟机的节点设置：
完成上述步骤后，对虚拟机进行克隆，复制出两个节点，总共搭建三个节点。这样可以减少虚拟机对空间的占用率，只存储被复制节点的差异空间。
将上述三个虚拟机的IP分别进行设置，然后修改其hostname，确保和hosts文件一一对应。然后就可以开始集群安装的前期设置了。

#### 所有节点的SSH免密登陆：
首先，在所有节点上修改SSH的默认配置，编辑 /etc/ssh/sshd_config 文件，找到注释配置：
```shell
#RSAAuthentication yes,
#PubkeyAuthentication yes
```
把前面的“#"号去掉。

然后，在当前节点上，以root进行登陆，然后创建ssh的key：
```shell
ssh-keygen -t rsa
```
回车三次，完成私钥id_rsa和公钥id_rsa.pub的生成。对其他节点也同样完成上述操作。

接着，从master拷贝node1和node2的公钥：
```shell
scp root@node1:~/.ssh/id_rsa.pub node1.pub
scp root@node2:~/.ssh/id_rsa.pub node2.pub
```
将所有节点的公钥写入到./ssh/authorized_keys文件中：
```shell
cat id_rsa.pub >> authorized_keys
cat node1.pub >> authorized_keys
cat node2.pub >> authorized_keys
```

最后，将这个authorized_keys再分发到node1和node2中：
```shell
scp authorized_keys  root@node1:~/.ssh/authorized_keys
scp authorized_keys  root@node2:~/.ssh/authorized_keys
```
第一次远程登录需要是输入yes，后续可以直接登录。

> - PS：也可以在虚拟机镜像中完成上述步骤，这样所有节点的公钥和私钥都是相同的，可以直接免密码进行SSH链接。

参考文档：[centos 7 免密登录](http://www.cnblogs.com/hobinly/p/6039844.html)

#### 设置NTP服务：
借助于installHadoop.sh脚本中的函数完成设置，编写setup_ntp.sh脚本：
```shell
function ntp_master_config(){
    file=$1
    sed -i '8 c restrict default nomodify' ${file}
    sed -i '/# Please consider joining the pool (http:\/\/www.pool.ntp.org\/join.html)./{N;N;N;N;s/.*/server 127.127.1.0\nfudge  127.127.1.0 stratum 10/}' ${file}
    ntpd -c /etc/ntp.conf -p /tmp/ntpd.pid
    chkconfig ntpd on
    echo "完成NTP服务器设置"
}
function ntp_slaver_config(){
    file=$1
    NTP_MASTER_IP=$2
    croncontent="*/1 * * * * /usr/sbin/ntpdate $NTP_MASTER_IP"
    cronfile="/var/spool/cron/root"
    sed -i '/# Please consider joining the pool (http:\/\/www.pool.ntp.org\/join.html)./{N;s/.*/server $NTP_MASTER_IP/}' ${file}
    # 创建定时任务
    touch ${cronfile}
    echo "${croncontent}" >> ${cronfile}
    service crond restart
    echo "完成NTP子节点设置"
}

ntp_config_file="/etc/ntp.conf"
ntp_master_ip="192.168.184.129"

function entery(){
    echo "NTP服务器设置，请输入：master"
    echo "NTP子节点设置，请输入：node"
    read -p "输入要进行的选项：" option

    # 根据用户输入选择要执行的操作是安装还是启动
    if [ ${option} == "master" ]
    then
        echo "设置NTP服务器主节点"
        ntp_master_config ${ntp_config_file}
    elif [ ${option} == "node" ]
    then
        echo "设置NTP服务器子节点"
        ntp_slaver_config  ${ntp_config_file} ${ntp_master_ip}
    fi
}

entery
```
然后分发到各个子节点上：
```shell
scp setup_ntp.sh root@node1:/home/tools/setup_ntp.sh
scp setup_ntp.sh root@node2:/home/tools/setup_ntp.sh
```
在master节点上执行：
```shell
./setup_ntp.sh
master
```
在其他node节点上执行：
```shell
./setup_ntp.sh
node
```
这样就完成了整个集群的NTP服务器搭建。


### 集群安装：
Spark作为原生Hadoop的MapReduce组件扩展，需要依赖Hadoop的环境，所以整个集群的搭建分为两步：Hadoop集群的搭建和Spark集群的搭建。

#### 安装Hadoop集群：
在所有节点上解压缩Hadoop的包到/usr/local目录下：
```shell
tar -zxvf hadoop-2.6.0.tar.gz -C /usr/local
```
然后就可以查看Hadoop的版本信息了：
```shell
./usr/local/hadoop-2.6.0/bin/hadoop version
```






#### 安装Spark集群：
参考文档：
[linux下spark集群搭建](http://smallx.me/2016/06/07/linux%E4%B8%8Bspark%E9%9B%86%E7%BE%A4%E6%90%AD%E5%BB%BA/)

## 二 Spark集群的测试：

## 三 MapReduce算法的实现和测试：



