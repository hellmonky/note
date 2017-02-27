# tensorflow学习：

人工智能作为下一个技术发展增长点，值得个人关注，并且这种趋势越来越明显，也就要求自己越来越快的进入到这个行业中去。

## 初识tensorflow：
tensorflow是google发布的开源机器学习工具框架，基于python完成。
为了比较直观的了解tensorflow，我们需要搭建一个测试环境来跑一下。

### 搭建centos7下的docker测试环境：
选用centos7虚拟机来完成环境的设置。因为centos7默认安装了python2.7版本，使用命令确认下：
```shell
python --version
```
官方给出了很多种安装方式，有：
> - 直接采用当前系统环境进行安装部署；
> - 采用python的virtualenv来完成python运行环境隔离安装；
> - 采用docker方式以容器的形式进行隔离；

而tensorflow需要依赖于python3才能正常运行，而centos7中很多组件是依赖于默认安装的python2.7版本的，为了解决不同版本的依赖问题，参考之后选定使用docker的方式进行安装最为隔离，并且可以通过使用docker来更加的熟悉整个运行环境的部署开发。

首先需要在当前系统环境中安装docker，建议使用：
```shell
curl -fsSL https://get.docker.com/ | sh
```
这个官方命令脚本来完成安装，会默认安装当前最新的release版本。完成了基本docker运行环境的安装之后，我们启动docker：
```shell
systemctl start docker.service
```
然后来安装tensorflow对应的环境：
```shell
docker run -it b.gcr.io/tensorflow/tensorflow-full
```
问题就来了，这个是googlecode上的地址，已经被和谐了，在虚拟机centos7中无法访问，阿西吧，好吧，只能在虚拟机centos7中再搭建一个ss客户端了。
但是测试，搭建ss之后，docker pull命令并无法使用socket5和http协议完成下载，最终docker run获取镜像也是失败，只能通过修改/etc/hosts文件：
```shell
61.91.161.217 b.gcr.io
```
才能正确访问，看来是因为docker获取镜像不通过http协议，ping也不通过http协议。

#### centos7环境下搭建shadowsocket客户端：
通过源代码编译的方式进行安装部署。
首先，获取shadowsocket的一个版本，这个选择libev版本，安装编译所需要的基本工具包：
```shell
yum install curl-devel zlib-devel openssl-devel expat-devel perl-devel asciidoc xmlto
```
然后根据[官方安装教程](https://github.com/shadowsocks/shadowsocks-libev)，获取最新的代码：
```shell
git clone https://github.com/shadowsocks/shadowsocks-libev.git
cd shadowsocks-libev
git submodule update --init --recursive
```
开始对源代码进行编译：
```shell
./autogen.sh
./configure --prefix=/usr/local/shadowsocks-libev
```
但是最新的代码总是无法满足所有的依赖。

放弃，使用epel源来完成预编译的安装：
```shell
sudo wget https://copr.fedoraproject.org/coprs/librehat/shadowsocks/repo/epel-7/librehat-shadowsocks-epel-7.repo -o /etc/yum.repo.d/librehat-shadowsocks-epel-7.repo
sudo yum update and sudo yum install shadowsocks-libev
```
完成安装后进行配置：
```shell
nano /etc/shadowsocks-libev/config.json
```
后台启动服务：
```shell
nohup ss-local -c /etc/shadowsocks-libev/config.json /dev/null 2>&1 &
```
检查是否启动成功：
```shell
netstat -lnp|grep 1080
ps aux | grep ss-local | grep -v "grep"
```
最后就是设置整个系统都使用ss作为代理，也就是将全局http代理转发到socks5代理：
```shell
yum install epel-release
yum update
rm /etc/yum.repos.d/epel-testing.repo
yum install privoxy
nano /etc/privoxy/config
```
在文件尾部添加以下内容：
```shell
forward-socks5 / 127.0.0.1:1080 .
```
保存后重启服务：
```shell
systemctl status privoxy
systemctl start privoxy
```
在当前终端中添加环境变量：
```shell
nano ~/.bashrc
export http_proxy=http://127.0.0.1:8118  
export https_proxy=http://127.0.0.1:8118  
export ftp_proxy=http://127.0.0.1:8118
```
进行测试：
```shell
curl ip.gs
```
确定可以通过ss访问http了，但是：
```shell
ping www.google.com
```
无法访问。因为ping协议不通过http和socket协议。

参考文档：
[Install and Configure Shadowsocks under Centos 7](https://amito.me/2016/02/Install-and-Configure-Shadowsocks-under-Centos-7/)
[librehat / shadowsocks](https://copr.fedorainfracloud.org/coprs/librehat/shadowsocks/)
[Centos7 64bit安装shadowsocks 如何使用sslocal配置本地代理](http://itgeeker.net/64bit-centos7-install-sslocal-how-to-use-the-shadowsocks-configuration-of-the-local-agent/)
[如何在centos7上使用shadowsocks配置代理](http://qindongliang.iteye.com/blog/2338383)
[Shadowsocks+Privoxy打造Linux / Mac socks和http代理](http://ian.wang/196.htm)
[使用Privoxy将socks5代理转为http代理](https://blog.phpgao.com/privoxy-shadowsocks.html)
[Linux CentOS 7 Shadowsocks 终端代理配置](https://www.zhb127.com/archives/linux-centos-7-shadowsocks-proxy-of-terminal.html)
[CentOS命令行下使用代理：Shadowsocks+privoxy+redsocks实现全局代理](https://laowang.me/centos-global-privoxy.html)
[使用 Shadowsocks 自建翻墙服务器，实现全平台 100% 翻墙无障碍](https://www.loyalsoldier.me/fuck-the-gfw-with-my-own-shadowsocks-server/)
[ss-install.md](https://gist.github.com/aa65535/ea090063496b0d3a1748)
[使用kubernetes创建容器一直处于ContainerCreating状态的原因查找与解决](http://www.itdadao.com/articles/c15a1120843p0.html)
[ping 原理与ICMP协议](http://blog.csdn.net/inject2006/article/details/2139149)


#### 使用国内docker源进行下载：
通过国内提供的镜像管理源直接安装：
```shell
docker run -it daocloud.io/daocloud/tensorflow:1.0.0-rc1-devel
```

> - 参考文档：
[Tensorflow中文-下载与安装](https://github.com/jikexueyuanwiki/tensorflow-zh/blob/master/SOURCE/get_started/os_setup.md)

### 搭建windows环境下的docker测试环境：
因为docker提供了windows下的DockerToolbox来支持容器的运行，我们也可以在windows环境下使用docker容器来跑tensorflow了。
同样的，在windows环境也是因为墙的问题，无法从google官方获取tensorflow的docker镜像。
解决方法也是通过hosts文件和使用国内源解决。


### tensorflow的基本运行测试：
在docker环境中安装完毕，我们首先来测试下是否可以正常运行起来。
首先检查当前docker镜像中的python版本：
```shell
python --verison
Python 2.7.6
```
然后输入python，进入终端中进行测试：
```shell
import tensorflow as tf
hello = tf.constant('Hello, TensorFlow!')
sess = tf.Session()
print sess.run(hello)
>>> Hello, TensorFlow!
a = tf.constant(10)
b = tf.constant(32)
print sess.run(a+b)
>>> 42
```
如果上述运行正确，基本上tensorflow的运行环境就表示成功了。



