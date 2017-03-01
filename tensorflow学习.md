<!-- TOC -->

- [tensorflow学习：](#tensorflow学习)
    - [初识tensorflow：](#初识tensorflow)
        - [搭建centos7下的docker测试环境：](#搭建centos7下的docker测试环境)
            - [使用国内docker源进行下载：](#使用国内docker源进行下载)
        - [搭建windows环境下的docker测试环境：](#搭建windows环境下的docker测试环境)
        - [tensorflow的基本运行测试：](#tensorflow的基本运行测试)
            - [TensorFlow中的基本概念：](#tensorflow中的基本概念)
    - [深入学习tensorflow：](#深入学习tensorflow)
        - [tensorflow的python开发：](#tensorflow的python开发)
        - [tensorflow的golang开发：](#tensorflow的golang开发)
    - [tensorflow和当前大数据生态系统的集成：](#tensorflow和当前大数据生态系统的集成)
        - [tensorflow使用HDFS获取数据：](#tensorflow使用hdfs获取数据)
        - [tenosrflow使用Spark完成计算：](#tenosrflow使用spark完成计算)
    - [附录：](#附录)
        - [centos7环境下搭建shadowsocket客户端：](#centos7环境下搭建shadowsocket客户端)

<!-- /TOC -->

# tensorflow学习：

人工智能作为下一个技术发展增长点，值得个人关注，并且这种趋势越来越明显，也就要求自己越来越快的进入到这个行业中去。

## 初识tensorflow：
TensorFlow是Google在2015年11月份开源的人工智能系统，目前托管在[Github](https://github.com/tensorflow/tensorflow)，是之前所开发的深度学习基础架构DistBelief的改进版本，该系统可以被用于语音识别、图片识别等多个领域。
官方对TensorFlow的介绍是，一个使用数据流图(data flow graphs)技术来进行数值计算的开源软件库。所谓的数据流图，就是描述有向图中的数值计算过程。有向图中的节点通常代表数学运算，但也可以表示数据的输入、输出和读写等操作；有向图中的边表示节点之间的某种联系，它负责传输多维数据(Tensors)。图中这些tensors的flow也就是TensorFlow的命名来源。
节点可以被分配到多个计算设备上，可以异步和并行地执行操作。因为是有向图，所以只有等到之前的入度节点们的计算状态完成后，当前节点才能执行操作。

TensorFlow不是一个严格的神经网络工具包，只要你可以使用数据流图来描述你的计算过程，你可以使用TensorFlow做任何事情。你还可以方便地根据需要来构建数据流图，用简单的Python语言来实现高层次的功能。

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

#### TensorFlow中的基本概念：
TensorFlow是一种将计算表示为图的编程系统。图中的节点称为ops(operation的简称)。一个ops使用0个或以上的Tensors，通过执行某些运算，产生0个或以上的Tensors。一个Tensor是一个多维数组，例如，你可以将一批图像表示为一个四维的数组[batch, height, width, channels]，数组中的值均为浮点数。

所以使用tensorflow完成计算，需要执行以下步骤：
> - 将计算流程表示成图；
> - 通过Sessions来执行图计算；
> - 将数据表示为tensors；
> - 使用Variables来保持状态信息；
> - 分别使用feeds和fetches来填充数据和抓取任意的操作结果；

TensorFlow中的图描述了计算过程，图通过Session的运行而执行计算。Session将图的节点们(即ops)放置到计算设备(如CPUs和GPUs)上，然后通过方法执行它们；这些方法执行完成后，将返回tensors。在Python中的tensor的形式是numpy ndarray对象，而在C/C++中则是tensorflow::Tensor。



> - 参考文档：
[TensorFlow上手](https://oncemore2020.github.io/blog/tensorflow-getstarted/)
[TensorFlow学习笔记1：入门](http://www.jeyzhang.com/tensorflow-learning-notes.html)


## 深入学习tensorflow：

### tensorflow的python开发：

### tensorflow的golang开发：


## tensorflow和当前大数据生态系统的集成：
AI从来都是来源于大样本的训练，只有通过大样本的训练才可以满足当前对AI程度的需求，所以只有和传统大数据平台结合才能真正发挥出机器学习的能力。
传统的大数据平台经过了Hadoop单一生态，到Spark专注计算辅助的多方面发展，将大量的样本数据的分析运算进行了拓展，同时2016年从新兴起的AR、VR等也都受益于大数据训练的DL和AI相关成果。
例如：Google Assistant和Apple Siri等语音识别都基于广泛的机器学习成果。
而Tensorflow作为一个DL框架，也就必然的需要和大数据平台结合才能进一步释放能力。通过训练的模型和云端数据，物联网、自动驾驶等才能真的运行起来。
作为从业者，自己需要跨域两个问题：从一般领域进入数据领域，然后从数据领域进入分析领域。22世纪一定是基于数据分析进行产业调整的一个过程，并且会更加的刺激计算能力提升，期待超越硅基电路物理限制的计算器的诞生。

### tensorflow使用HDFS获取数据：

### tenosrflow使用Spark完成计算：


## 附录：

### centos7环境下搭建shadowsocket客户端：
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