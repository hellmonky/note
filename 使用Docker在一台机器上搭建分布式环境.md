<!-- TOC -->

- [使用Docker在一台机器上搭建分布式环境](#使用docker在一台机器上搭建分布式环境)
    - [Linux开发环境设置：](#linux开发环境设置)
    - [安装基础包：](#安装基础包)
        - [安装Oracle JDK和java开发环境：](#安装oracle-jdk和java开发环境)
        - [安装golang和开发环境：](#安装golang和开发环境)
        - [安装Docker运行环境：](#安装docker运行环境)
    - [搭建集群：](#搭建集群)
        - [建立基础镜像：](#建立基础镜像)

<!-- /TOC -->

# 使用Docker在一台机器上搭建分布式环境
在开发分布式应用的时候，非常有必要搭建集群进行测试，但是鉴于集群的购买成本高，那么可以使用虚拟机的方式进行集群搭建，但是虚拟机带来的性能损耗非常严重，是否有比较轻量级的实现方式来完成这个功能？
本文借鉴了如下文章，使用Docker来完成隔离，方便在测试机上搭建分布式应用：
[用Docker在一台笔记本电脑上搭建一个具有10个节点7种角色的Hadoop集群（上）-快速上手Docker](http://www.cnblogs.com/chengyujia/p/6855436.html)
[用Docker在一台笔记本电脑上搭建一个具有10个节点7种角色的Hadoop集群（下）-搭建Hadoop集群](http://www.cnblogs.com/chengyujia/p/6860551.html)

## Linux开发环境设置：
参考《DataSong部署环境搭建说明.md》

## 安装基础包：
进行环境搭建，需要准备一些基础环境依赖包的安装，下面各个章节就需要的基础环境安装进行说明。

### 安装Oracle JDK和java开发环境：
需要安装Oracle JDK来替换OpenJDK，所以需要从Oracle官网上下载相关的JDK来进行替换：
```shell
curl -L "http://download.oracle.com/otn-pub/java/jdk/8u101-b13/jdk-8u101-linux-i586.tar.gz" -H "Cookie: oraclelicense=accept-securebackup-cookie"  -H "Connection: keep-alive" -O
```
因为图形界面可以进行确认，但是在命令行下只能通过Curl进行参数来下载。
需要指定JDK的完整路径，这个还是需要查看当前提供的版本，或者记录需要的版本进行的。

### 安装golang和开发环境：

### 安装Docker运行环境：
因为目前docker的版本发生了变化，所以安装方式和之前的有所不同，按照上述教程，需要执行的操作为：
```shell
# 安装yum工具包：
yum -y install yum-utils

# 添加Docker CE的yum源：
yum-config-manager --add-repo https://download.docker.com/linux/centos/docker-ce.repo

# 更新yum包索引
yum makecache fast

# 安装Docker CE：
yum install docker-ce

# 启动Docker CE：
systemctl start docker

# 检查Docker CE版本：
docker --verison

# 将docker用户组添加到root，不用sudo命令
sudo usermod -aG docker root

```
关于yum-utils是什么，可以参考这篇文章：
[如何安装和使用’yum-utils’来维护Yum并提高其性能](https://www.howtoing.com/linux-yum-package-management-with-yum-utils/)
[yum-utils](http://blog.csdn.net/xiaoxiao_22/article/details/7044583)

## 搭建集群：
使用Docker搭建集群，一定需要注意Docker镜像中的可修改资源和不可修改资源的问题。
使用Dockerfile来构建对应的img，然后设置不同的网络地址。

### 建立基础镜像：


