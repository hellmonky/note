<!-- TOC -->

- [DockerDocker](#dockerdocker)
    - [docker使用回顾：](#docker使用回顾)
        - [基本环境搭建：](#基本环境搭建)
            - [脚本安装：](#脚本安装)
            - [手动离线包安装：](#手动离线包安装)
            - [添加Docker服务开机自动启动：](#添加docker服务开机自动启动)
        - [hello-world：](#hello-world)
        - [基本的镜像命令使用：](#基本的镜像命令使用)
            - [将当前的容器导出：](#将当前的容器导出)
            - [从tgz恢复为镜像：](#从tgz恢复为镜像)
            - [修改已有镜像的名称：](#修改已有镜像的名称)
            - [通过Dockerfile生成基础镜像：](#通过dockerfile生成基础镜像)
        - [基本的容器命令使用：](#基本的容器命令使用)
            - [创建并且运行一个后台容器：](#创建并且运行一个后台容器)
            - [进入正在运行的容器：](#进入正在运行的容器)
            - [退出容器而不关闭：](#退出容器而不关闭)
            - [创建而不运行容器：](#创建而不运行容器)
            - [容器与host进行文件传输交互：](#容器与host进行文件传输交互)
        - [基本的容器和镜像转换操作：](#基本的容器和镜像转换操作)
            - [对运行中的docker镜像修改后保存：](#对运行中的docker镜像修改后保存)
        - [数据卷：](#数据卷)
        - [数据卷容器：](#数据卷容器)
            - [创建数据卷容器：](#创建数据卷容器)
            - [数据卷容器中的数据进行备份：](#数据卷容器中的数据进行备份)
            - [数据卷容器中的数据进行恢复：](#数据卷容器中的数据进行恢复)
            - [应用：](#应用)
            - [总结：](#总结)
    - [深入使用Docker：](#深入使用docker)
        - [image和container的关系：](#image和container的关系)
        - [application container 还是 machine container：](#application-container-还是-machine-container)
        - [systemd：](#systemd)
        - [镜像的选择：](#镜像的选择)
        - [GUI应用：](#gui应用)
            - [Linux的图形子系统简介：](#linux的图形子系统简介)
            - [本地GUI：](#本地gui)
                - [第一个实际例子：](#第一个实际例子)
                - [安装IDEA社区版本：](#安装idea社区版本)
                    - [# 需要注意的问题（1）](#-需要注意的问题1)
                    - [# 需要注意的问题（2）](#-需要注意的问题2)
                    - [# 需要注意的问题（3）](#-需要注意的问题3)
                    - [# 需要注意的问题（4）](#-需要注意的问题4)
                - [镜像拆分：](#镜像拆分)
            - [远程GUI：](#远程gui)
        - [存储的方式：](#存储的方式)
        - [共享kernel带来的风险：](#共享kernel带来的风险)
        - [Docker ulimit：](#docker-ulimit)
    - [Dockerfile语法：](#dockerfile语法)
        - [使用Alpine基础镜像和Dockerfile构建tomcat运行环境：](#使用alpine基础镜像和dockerfile构建tomcat运行环境)
            - [制作alpine基础镜像:](#制作alpine基础镜像)
            - [从官方网站下载oracle jdk和tomcat7:](#从官方网站下载oracle-jdk和tomcat7)
            - [构建tomcat工程：](#构建tomcat工程)
            - [最精简的运行环境：](#最精简的运行环境)
            - [镜像分层：](#镜像分层)
            - [部署测试：](#部署测试)
            - [其他文档：](#其他文档)
        - [Dockerfile的语法细节：](#dockerfile的语法细节)
    - [Docker应用实践：](#docker应用实践)
        - [基于alpine搭建IDEA的运行环境：](#基于alpine搭建idea的运行环境)
        - [基于alpine搭建chrome浏览器：](#基于alpine搭建chrome浏览器)
        - [基于ubuntu搭建openjdk8编译环境：](#基于ubuntu搭建openjdk8编译环境)
        - [基于Alpine的最小化java8运行环境：](#基于alpine的最小化java8运行环境)
            - [OpenJDK8](#openjdk8)
            - [OracleJDK8](#oraclejdk8)
            - [容量对比：](#容量对比)
    - [Docker仓库管理：Docker registry](#docker仓库管理docker-registry)
        - [初识Docker Hub：](#初识docker-hub)
    - [Docker实现的Linux基础：](#docker实现的linux基础)
        - [在A上的实现可能性：](#在a上的实现可能性)
        - [实现的特性要求：](#实现的特性要求)
            - [Namespace资源隔离：](#namespace资源隔离)
            - [cgroups资源限制：](#cgroups资源限制)
            - [docker graph driver：](#docker-graph-driver)
        - [手动实现一个简易Docker：](#手动实现一个简易docker)
            - [graphdriver:](#graphdriver)
            - [namespace的调用：](#namespace的调用)
            - [cgroups的调用：](#cgroups的调用)
    - [Docker的实现原理：](#docker的实现原理)
        - [多层容器镜像存储结构：](#多层容器镜像存储结构)
    - [容器性能调优：](#容器性能调优)
        - [文件存储的性能调整：](#文件存储的性能调整)
    - [容器集群：](#容器集群)

<!-- /TOC -->

# DockerDocker
Docker的轻量化能够做到一些不需要全虚拟化方案来解决的问题，将整个资源的隔离性需求细分了，所以未来必然是多种层次虚拟化方式并存的。
轻量级的就是Docker，重量级的就是KVM。

## docker使用回顾：
回顾之前学习docker的基本概念和操作。
基础操作系统为：Ubuntu 16.04.3 LTS

### 基本环境搭建：
现在有三种方式进行环境的搭建：源代码编译安装，rpm包安装，使用脚本安装。对于安装需要的人工介入需求依次减弱。

#### 脚本安装：
```shell
curl -sSL https://get.docker.com/ | sh
```
这个脚本会判断当前的linux发布版本，然后安装。
具体的流程可以看这个sh代码，但是个人建议能不谢shell脚本就不写，尽量使用python等其他规范脚本语言来提高可维护性。

#### 手动离线包安装：
分析 https://get.docker.com/ 返回的脚本，找到下载安装的deb包的位置为：
https://apt.dockerproject.org/repo/pool/main/d/docker-engine/

#### 添加Docker服务开机自动启动：
如果需要将Docker服务添加到开机自动启动，在systemd环境下可以执行：
systemctl enable docker.service
将会自动对当前的服务创建连接：
Created symlink from /etc/systemd/system/multi-user.target.wants/docker.service to /usr/lib/systemd/system/docker.service.


参考文档：
[Get Docker CE for Ubuntu](https://docs.docker.com/engine/installation/linux/docker-ce/ubuntu/)
[ubuntu server 16.04离线安装docker 1.12.3的探索](http://blog.csdn.net/wn527518/article/details/53160116)

### hello-world：
安装完毕之后，尝试跑hello-world来进行测试：
```shell
docker run hello-world
```
回显：
```shell
Hello from Docker!
This message shows that your installation appears to be working correctly.

To generate this message, Docker took the following steps:
 1. The Docker client contacted the Docker daemon.
 2. The Docker daemon pulled the "hello-world" image from the Docker Hub.
 3. The Docker daemon created a new container from that image which runs the
    executable that produces the output you are currently reading.
 4. The Docker daemon streamed that output to the Docker client, which sent it
    to your terminal.

To try something more ambitious, you can run an Ubuntu container with:
 $ docker run -it ubuntu bash

Share images, automate workflows, and more with a free Docker ID:
 https://cloud.docker.com/

For more examples and ideas, visit:
 https://docs.docker.com/engine/userguide/
```
这样就表示当前的docker环境安装，并且可以正确执行了。

### 基本的镜像命令使用：

#### 将当前的容器导出：
我们在一个容器上的很多snap会形成一个历史，如果需要将当前的完整历史信息保存，方便后续导入，我们可以通过：
```shell
sudo docker save -o /home/fengzheng/dockerImages/mmm.tar ubuntu:12.04
```
将这个容器保存下来。
需要注意的是，这个保存的内容就是镜像的分层目录结构。
如果需要将save的镜像恢复，只需要：
docker load -i XXX.tar

#### 从tgz恢复为镜像：
可以将自己制作的rootfs导入到docker中作为镜像运行。
以标准centos7的rootfs上安装了工具的系统制作备份：
```shell
tar --one-file-system --exclude './proc' --exclude './sys' -pzcvf ./centos7_rootfs.tgz ./*
```
然后将这个tgz导入测试：
```shell
sudo docker import /centos7_rootfs.tgz test_centos7
```
正好也可以和现在已经存在的centos7的镜像做一个大小的对比：
```shell
wentao@wentao-ThinkPad-X1-Carbon-2nd:/var/lib/docker$ docker images
REPOSITORY          TAG                 IMAGE ID            CREATED             SIZE
test_centos7        latest              4d348d4232ae        4 seconds ago       567MB
dev/centos7         latest              6756ab8575c6        4 minutes ago       589MB
centos7_base        latest              3643493a3c62        24 hours ago        193MB
hello-world         latest              1815c82652c0        2 months ago        1.84kB
```
可以看出大小差别不是很大，但是层次上的差异就很大了。使用命令docker inspect可以看到每一个镜像的详细内容：
docker inspect dev/centos7
```shell
[
    {
        "Id": "sha256:6756ab8575c6bc67248fd4e895cc315fa47692ec83de58cc083949356eb95f82",
        "RepoTags": [
            "dev/centos7:latest"
        ],
        "RepoDigests": [],
        "Parent": "sha256:3643493a3c62eb762f520531821352d496e08c0ea87963f508ff53ffbd1e86b0",
        "Comment": "yum groupinstall Development Tools;yum install net-tools",
        "Created": "2017-08-23T11:51:37.942668061Z",
        "Container": "ae5487f6c6e17501ab01c4438a77616aec56b6586d885157ab4a866c527c7c60",
        "ContainerConfig": {
            "Hostname": "ae5487f6c6e1",
            "Domainname": "",
            "User": "",
            "AttachStdin": true,
            "AttachStdout": true,
            "AttachStderr": true,
            "Tty": true,
            "OpenStdin": true,
            "StdinOnce": true,
            "Env": [
                "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
            ],
            "Cmd": [
                "/bin/bash"
            ],
            "Image": "centos7_base",
            "Volumes": null,
            "WorkingDir": "",
            "Entrypoint": null,
            "OnBuild": null,
            "Labels": {
                "build-date": "20170801",
                "license": "GPLv2",
                "name": "CentOS Base Image",
                "vendor": "CentOS"
            }
        },
        "DockerVersion": "17.06.1-ce",
        "Author": "wentao",
        "Config": {
            "Hostname": "",
            "Domainname": "",
            "User": "",
            "AttachStdin": false,
            "AttachStdout": false,
            "AttachStderr": false,
            "Tty": false,
            "OpenStdin": false,
            "StdinOnce": false,
            "Env": [
                "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
            ],
            "Cmd": [
                "/bin/bash"
            ],
            "Image": "",
            "Volumes": null,
            "WorkingDir": "",
            "Entrypoint": null,
            "OnBuild": null,
            "Labels": {
                "build-date": "20170801",
                "license": "GPLv2",
                "name": "CentOS Base Image",
                "vendor": "CentOS"
            }
        },
        "Architecture": "amd64",
        "Os": "linux",
        "Size": 588866218,
        "VirtualSize": 588866218,
        "GraphDriver": {
            "Data": null,
            "Name": "aufs"
        },
        "RootFS": {
            "Type": "layers",
            "Layers": [
                "sha256:e315e40a6a4f35a8f57d6e52fe4beb03cb72f0a459f1d8c02cfc62b9f59842e8",
                "sha256:99edd2ff49bd00775a8a11b6127f091d355ac4024168ac8f0055d52e090076c6"
            ]
        }
    }
]
```
docker inspect test_centos7
```shell
[
    {
        "Id": "sha256:4d348d4232aef8ca4f673954d6f8b88ef5600279d1c5950c81b730083d16a748",
        "RepoTags": [
            "test_centos7:latest"
        ],
        "RepoDigests": [],
        "Parent": "",
        "Comment": "Imported from -",
        "Created": "2017-08-23T11:55:36.913333105Z",
        "Container": "",
        "ContainerConfig": {
            "Hostname": "",
            "Domainname": "",
            "User": "",
            "AttachStdin": false,
            "AttachStdout": false,
            "AttachStderr": false,
            "Tty": false,
            "OpenStdin": false,
            "StdinOnce": false,
            "Env": null,
            "Cmd": null,
            "Image": "",
            "Volumes": null,
            "WorkingDir": "",
            "Entrypoint": null,
            "OnBuild": null,
            "Labels": null
        },
        "DockerVersion": "17.06.1-ce",
        "Author": "",
        "Config": {
            "Hostname": "",
            "Domainname": "",
            "User": "",
            "AttachStdin": false,
            "AttachStdout": false,
            "AttachStderr": false,
            "Tty": false,
            "OpenStdin": false,
            "StdinOnce": false,
            "Env": null,
            "Cmd": null,
            "Image": "",
            "Volumes": null,
            "WorkingDir": "",
            "Entrypoint": null,
            "OnBuild": null,
            "Labels": null
        },
        "Architecture": "amd64",
        "Os": "linux",
        "Size": 566706623,
        "VirtualSize": 566706623,
        "GraphDriver": {
            "Data": null,
            "Name": "aufs"
        },
        "RootFS": {
            "Type": "layers",
            "Layers": [
                "sha256:b42ecde8677da9946d3a469c9e7057451efd030f09b1a78e76409ed40729d535"
            ]
        }
    }
]
```
通过对比可以看出，layer少了一层。
docker inspect会输出所有的信息，如果想查看其中的一项，只需要使用-f {{}} 指定即可：
```shell
docker inspect -f{{.ContainerConfig.Cmd}} centos7_base
```
回显：
```shell
[/bin/sh -c #(nop)  CMD ["/bin/bash"]]
```
并且和docker save出来的分层的压缩包不同，一般rootfs打包出来的空间占用会小一些。

#### 修改已有镜像的名称：
有时候在保存一个镜像的时候起的名称存在问题，可以对镜像的名称进行修改，命令为：
docker tag 原镜像名 自定义名
再使用docker images查看会多出来一行，该行的image id和刚刚的自定义名称是一样的。
实际上是对现在的这个镜像新增了一个tag，并没有修改名称。

#### 通过Dockerfile生成基础镜像：
总体的流程为：编写Dockerfile，然后准备文件，最后编译生成镜像。
例如，从0开始搭建一个centos的基础镜像，首先准备centos的rootfs，然后使用当前host的kernel就可以了。
编写Dockerfile内容为：
```shell
FROM scratch
ADD centos-7-docker.tar.xz /

LABEL name="CentOS Base Image" \
    vendor="CentOS" \
    license="GPLv2" \
    build-date="20170801"

CMD ["/bin/bash"]
```
然后准备centos7的rootfs，可以从官方下载：
https://github.com/CentOS/sig-cloud-instance-images/tree/CentOS-7.3.1611/docker
需要指定branch来获取对应的rootfs。

使用docker对当前这个目录下的Dockerfile和文件进行编译，生成指定名称的镜像：
```shell
docker build -t centos7_base .
```
命令中第一个参数 -t testimage 指定创建的新镜像的名字，第二个参数是一个点 . 指定从当前目录查找 Dockerfile 文件。

编译完毕后，就可以通过：
```shell
docker images
```
查看当前编译的镜像。
最终，使用：
```shell
docker run -ti centos7_base /bin/bash
```
启动运行并且进入到docker容器中。

关于centos的rootfs生成方法可以看到在[Build](https://github.com/CentOS/sig-cloud-instance-build/tree/master/docker)中的表述:
```shell
We use lorax's livemedia-creator to create the rootfs tarball, but you'll need a boot.iso start the process.
```
可以作为自己制作rootfs的一个参考。

参考文档：
[CentOS/sig-cloud-instance-build](https://github.com/CentOS/sig-cloud-instance-build)
[每天5分钟玩转 Docker 容器技术](https://www.ibm.com/developerworks/community/blogs/132cfa78-44b0-4376-85d0-d3096cd30d3f?lang=en)
[](https://wanglu.info/1159.html)

ubuntu提供了官方的Docker镜像基础支持：
可以从：
https://partner-images.canonical.com/core/xenial/current/
获取最新的tgz包，对应的Dockerfile工程位于：
https://hub.docker.com/_/ubuntu/
```shell
FROM scratch
ADD ubuntu-xenial-core-cloudimg-arm64-root.tar.gz /

LABEL name="ubuntu 16.04.3 Base Image" \
    vendor="Ubuntu" \
    license="GPLv2" \
    build-date="20170903"

CMD ["/bin/bash"]
```

### 基本的容器命令使用：

#### 创建并且运行一个后台容器：
docker run -d -ti --name test busybox /bin/busybox sh
这个命令将会从远程拉取busybox的基础镜像，然后运行基于这个镜像的容器。
建议在运行docker中设置name，这样就不会自动生成name，方便后续的识别。

**注意：这个后台运行的容器，使用exec进入后exit不会关闭这个容器**

#### 进入正在运行的容器：
首先通过：
docker ps
查看正在后台运行中的容器，然后使用attach来进入：
docker attach NAMES

#### 退出容器而不关闭：
如果使用run来启动一个容器并且进入，需要退出容器但是不关闭运行，一定不要用ctrl+c，那样就是让docker容器停止了。
要用如下快捷键：
先按，ctrl+p
再按，ctrl+q
这个时候就会重新返回host的终端，使用docker ps还是可以看到在后台运行的容器的。

#### 创建而不运行容器：
docker run命令会创建并运行容器，如果仅想创建但不运行容器，可以使用docker create命令：
sudo docker create ubuntu 
返回当前创建容器的id：
7d31bae2b5fa86675de61ad3473bf05c95ce28c26b915a434991efa966042b07

后续创建数据卷容器的时候，会有特殊的参数-v来表示当前创建的容器是用于存储的数据卷容器，会将该容器中的数据存储在对应的layer中，保证持久化。


docker run -e RequestedIP=192.168.5.230 --privileged=true -d reg.docker.alibaba-inc.com/alios-el6u2-base:1.0
docker run -e --privileged=true -d busybox /bin/busybox sh

#### 容器与host进行文件传输交互：
docker cp的方法实现也是把文件推到容器里的aufs文件系统里，具体命令为：
docker cp [OPTIONS] CONTAINER:SRC_PATH DEST_PATH|-
docker cp [OPTIONS] SRC_PATH|- CONTAINER:DEST_PATH

具体的步骤为：
（1）使用docker ps查看当前运行容器的ID；
（2）使用命令传输host文件到docker容器的目的地址；
（3）使用docker exec进入容器，进行操作。


### 基本的容器和镜像转换操作：
#### 对运行中的docker镜像修改后保存：
因为centos7的官方镜像中不包含网络工具，为了方便后续开发，进行安装：
```shell
yum install net-tools
```
如果这个时候exit退出，那么这个新添加的工具再下次启动后并不存在，为了将这个修正保存，我们需要在exit之前进行保存，新开一个终端，然后使用docker ps -l确认容器状态后，使用：
```shell
# -a:修改者信息 -m:注释、说明  紧跟着当前操作的容器id   最后是要生成的新的镜像名称
sudo docker commit -a "wentao" -m "add net-tools" ae2a59b86dd9 net/centos7_base
```
这个时候可以看到会多出一个新的镜像。
需要注意的是，上面命令中的ae2a59b86dd9，是通过：
```shell
docker ps -l
```
得到的运行中的容器的ID，而不是对应的镜像的ID。

然后使用：
docker history IMG_ID
来查看当前保存的镜像的构建过程。

如果只有一个终端，例如访问的是远程服务器，可以使用组合键（大写字母）：
```shell
Ctrl+P+Q
```
回车即可跳出docker容器且不关闭，然后执行对当前容器的固化，完成后使用：
```shell
docker exec -ti 1d2503e7ffdb /bin/bash
```
再次进入运行中的docker容器，其中1d2503e7ffdb就是当前运行中的容器ID。

再次进入后，使用exit是无法关闭容器的，只能从当前的exec状态退出，如果要关闭容器，应该使用：
```shell
docker stop --time=20 container_id
```
通过输入当前运行的容器id来停止。

参考文档：
[保存对容器的修改](http://www.docker.org.cn/book/docker/docer-save-changes-10.html)
[如何进入Docker容器](http://blog.csdn.net/u010397369/article/details/41045251)
[优雅的终止docker容器](https://xiaozhou.net/stop-docker-container-gracefully-2016-09-08.html)





### 数据卷：
从上述的实践看到，除非是将正在运行的容器固化为镜像，否则一切修改并不会被保存。
那么容器是否只能作为运行的无状态环境？但是如果我们的应用需要记录日志等，就一定会涉及到持久化，那么这部分可以修改的内容被作为数据卷引入。
Docker中的数据可以存储在类似于虚拟机磁盘的介质中，在Docker中称为数据卷（Data Volume）。数据卷可以用来存储Docker应用的数据，也可以用来在Docker容器间进行数据共享。
数据卷呈现给Docker容器的形式就是一个目录，支持多个容器间共享，修改也不会影响镜像。使用Docker的数据卷，类似在系统中使用 mount 挂载一个文件系统。
使用方式为：
```shell
docker run -ti -v /home/wentao/workspace/docker/data_volume:/remote_data centos7_base /bin/bash
```
命令中 -v 参数可以使用多次，并挂在多个数据卷到容器中。后面的参数信息中冒号前面是宿主机的本地目录，冒号后面是容器中的挂载目录。
这样就将本地的/home/wentao/workspace/docker/data_volume作为docker中的/remote_data挂载，docker中，所有在这个目录下的文件操作就会直接被写入到host的这个目录下而，不会重启后丢失数据。

挂载的数据卷默认为可读写权限，除非外部文件系统做了特殊限制，在 docker run的时候也可以执行为只读权限：
```shell
docker run -ti -v /home/wentao/workspace/docker/data_volume:/remote_data:ro centos7_base /bin/bash
```
通过制定权限，ro表示 readonly，挂载后的数据卷就是只读权限了。

注意：文件也可以作为数据卷被挂载到容器中，挂载的方式是相同的。

### 数据卷容器：
如果需要在多个容器间共享数据，并希望永久保存这些数据，最好的方式是使用数据卷容器，类似于一个提供网络文件共享服务的NFS服务器。

#### 创建数据卷容器：
数据卷容器创建方法跟普通容器一样，只需要指定宿主机的一个文件夹作为数据卷即可，使用docker create命令创建但不启动数据卷容器：
docker create -v /data --name docker_data ubuntu /bin/true
上述命令，将基于ubuntu:latest，创建了一个数据卷容器docker_data，并在其中创建一个和/data同名的数据卷，并且挂载到host的/data位置。

其他使用该数据卷容器的容器创建时候需要使用–volumes-from参数，指定该容器名称或ID，来挂载docker_data容器中的数据卷。
例如创建db1和db2两个容器，并从docker_data容器挂载数据卷：
$ docker run -it --volumes-from docker_data --name db1 ubuntu
$ docker run -it --volumes-from docker_data --name db2 ubuntu
此时，容器db1和db2都挂载同一个数据卷到相同的/data目录。三个容器任何一方在该目录下的写入，其他容器都可以看到。
值得注意的是不管docker_data是否运行，它都会起作用。只要有容器连接Volume，docker_data就不会被删除。
在使用 --volumes-from 参数所挂载数据卷的容器自己并不需要保持在运行状态。

可以通过：
docker ps -a
查看当前的所有的容器运行记录，然后使用：
docker inspect docker_data
查看创建的这个容器的详情，其中：
/var/lib/docker/volumes/dd457f68f8b0bd335f7e93b8131f620512f0de40bad15f4b63e583eaaaaf1b69/_data
就是/data所在的实际路径。

所以，在创建容器时指定 volume 选项。数据卷位于主机系统的特定位置下，例如 /var/lib/docker/volumes/_container_name_/_data 
具体的位置可用 docker inspect 命令查看，升级容器时不会改动这些数据。


一般而言，数据卷容器的基础镜像都要小，避免不必要的浪费，例如使用：busybox 或者 alpine ：
docker create -v /data --name docker_data busybox echo "Data-only container"
这样就能保证最小的镜像占用，并且提供基础的命令支持，保证后续的操作可以进行（备份恢复等）。
运行：
docker run -ti busybox /bin/busybox sh
可以看到这个镜像本身的rootfs是非常简单的，而且没有所谓的/data目录。
docker run -it --volumes-from docker_data --name db1 busybox
**需要注意的是，这样的数据卷容器被使用的时候，只能用于相同模板的其他容器，否则会出现错误。**


如果删除了挂载数据卷的容器，数据卷不会被自动删除.如果需要删除一个数据卷，必须在删除最后一个挂载该数据卷的容器时显示的使用docker -rm -v 命令来同时删除关联的容器：
docer -rm -v docker_data
然后再进入volume目录下，就发现被删除了。


#### 数据卷容器中的数据进行备份：
要对数据卷容器中的数据进行备份的时候，可以通过：
1. 创建一个新的容器
2. 挂载数据卷容器
3. 挂载宿主机本地目录作为数据卷
4. 将数据卷容器的内容备份到宿主机本地目录挂载的数据卷中
5. 完成备份操作后容器销毁

首先在host上创建一个目录：
mkdir /tmp/backup
然后将数据卷容器中的/data目录打包为tar，备份到上面创建的目录中，然后删除这个数据容器：
docker run --rm --volumes-from docker_data -v /tmp/backup:/backup ubuntu tar cvf /backup/docker_data.tar /data
也可以新建一个容器，然后使用上述备份恢复数据：
docker run --rm --volumes-from docker_data -v /tmp/backup:/backup ubuntu bash -c "cd /data && tar -zxvf /backup/backup.tar"

#### 数据卷容器中的数据进行恢复：


#### 应用：
作为数据卷的一个应用就是MySQL服务：

#### 总结：
Docker的数据卷挂载方式：
[Manage data in Docker](https://docs.docker.com/engine/admin/volumes/#more-details-about-mount-types)
总体上存在三种方式：
Volumes: 
Bind mounts:
tmpfs mounts: 


## 深入使用Docker：
在实际使用中更为深入的理解和学习docker。

### image和container的关系：
container就是在image上面添加了一层可读写层的运行状态，运行中的container在commit之后就将这个可读写层固化到现在的image的分层文件结构上。

### application container 还是 machine container：
在启动一个容器之后，我们以为通过kernel+rootfs，然后借由kernel提供的隔离机制就构成了一个完整的运行环境，但是实际上真的如此？
例如，我们需要修改网络，然后重新服务：
systemctl status network.service
然后就会出现：
```shell
root@40bff9878c3e:/# systemctl status networking.service
Failed to connect to bus: No such file or directory
```
或者我们在当前的docker中开启ssh服务：
systemctl start sshd.service
亦或者需要安装包的时候使用apt-get命令，都会出现上述错误。

按理来讲，当前的rootfs中是包含了所有的基础文件的，也就是包含了systemd的（可以查看tgz包中是否存在），然后使用了host的内核，这个操作应该可以执行啊？

其实这是没有理解docker运行的本质。我们看看：
docker run -ti --name dev_ubuntu ubuntu /sbin/init
docker run -ti --name dev_ubuntu ubuntu /bin/bash
同样的启动，这两个docker实际的运行状态是不同。
使用ps查看这两个容器中的运行线程。
```shell
An "application container" just runs a specific process (or group of processes); essentially it just runs what you need.
A "machine container" will run a full init, possibly with ssh, syslog, etc.
If you start an application container, it doesn't have a real init process, and since systemctl talks to the init process
```
其中的machine container也就是Operating System Containers，是一种：
```shell
OS containers are virtual environments that share the kernel of the host operating system but provide user space isolation.
```
启动时加上特权参数：
--privileged 或者 --privileged=true
docker run -d -ti --privileged --name=contname image-name 
也就是：
docker run -d -ti --privileged --name ubuntu_test ubuntu /sbin/init
这样结合/sbin/init启动参数，容器在启动时有权限启动systemd，然后常驻后台运行。
如果没有使用-d后台启动，当前终端会卡住，然后使用Ctrl+p+q的方式让容器进入后台运行，然后查看当前的运行状态：
docker ps
然后使用exec来进入这个容器：
docker exec -ti XXXX /bin/bash
进入，使用完毕之后exit退出即可，也不会终止容器。
如果需要明确的终止容器，需要使用：
docker stop XXXX

但是，从整个docker的应用场景来看，这种应用方式是存在问题的，而且由于--privileged参数还会带来安全性问题。
正确的方式就通过Dockerfile，然后安装部署启动相关的服务，然后使用Docker守护进程来控制一切，而不是交给systemd来进行管理。 

参考文档：
[Docker针对systemd启动的问题](http://wangyaohua.cn/wordpress/?p=611)
[Operating System Containers vs. Application Containers](https://blog.risingstack.com/operating-system-containers-vs-application-containers/)
[docker容器里安装ssh](http://6226001001.blog.51cto.com/9243584/1953310)
[Systemd与Docker的爱恨情仇](http://dockone.io/article/1093)

### systemd：
只是为你提供特定的文件系统层和进程隔离，它给你一个VM的感觉却并不是VM，所以你可能偶尔会想要像在物理机那样使用systemctl start|status|stop来管理服务进程，然后你通常会看到
Failed to get D-Bus connection: Operation not permitted
这个错误。

原因很简单：
你需要启动systemd进程
你需要特权

### 镜像的选择：
一般而言，我们都会选择主流的Linux发布版本对应的Docker镜像作为自己的基础镜像来进行使用，却没有仔细考虑过除了rootfs之外还有Linux启动服务等其他的因素影响。
但是在实际使用中，我们发现在Docker中运行的进程其实和在物理机或者虚拟机中的Linux发布版本非常不同，会遇到上述提到的各种问题。

并且从最小化基础镜像的角度来进行取舍，还会采用Alpine Linux发行版的Docker镜像，并且Alpine提供了交互友好的包管理器，避免在安装包时候需要自己解决依赖关系，确实非常好用。
但是更深层次的，由于Alpine Linux发行版使用了musl，和其他Linux发行版使用的glibc实现有所不同。这就是除了rootfs以外所带来的问题。
这种基础运行时库的差异会带来运行时的问题。而且由于Docker的分层文件存储结构，基础镜像只需要保存一次就可以，那么体积带来的问题其实并不存在，从稳定业务的角度考虑，统一的环境更为重要。

由此可见，对于基础镜像的选择也是非常关键的。
关于最小化基础镜像的问题，在Hack news上有激烈的讨论：[Super small Docker image based on Alpine Linux (github.com)](https://news.ycombinator.com/item?id=10782897)

并且推荐了一个基于ubuntu_16.04的修改版本镜像，来对因为docker化带来的问题做出来修正：
http://phusion.github.io/baseimage-docker/
个人看了也觉得很不错，推荐在稳定生产环境中使用：
docker pull phusion/baseimage


### GUI应用：
有时候我们需要部署带有GUI的应用在docker中，例如IDEA（好吧，这个的确是本地GUI），或者远程服务器上的gui应用。

#### Linux的图形子系统简介：
而Linux系统下的GUI都是通过X Window System实现的：
X Window System 是一个 【Client-Server 结构】的图形接口系统。X Client 指的便是各式各样的应用程序 (文书处理、数据库应用、网络软件等)，而 X Server 主要在处理来自 X Client applications 的 请求，在屏幕上绘制图形 (所以又称为 Display Server)；并且负责管理可用的字形与屏幕可用的颜色等系统资源，读取使用者的输入 (来自键盘或鼠标)，传送键盘、鼠标事件与窗口状态信息给 X Client (也就是应用程序)。
在 X Window System 中，X Server 与 X Client 都是透过 X Protocol 来交换讯息，X Protocol 定义了 X Server 与 X Client 间的讯息交换格式。
在 X Window System 中比较特殊的一点，就是 X Protocol 允许 X Server 与 X Client 能跨网络来交换讯息，这意味着 X Server 与 X Client 不限制都得在同一台机器上执行，可在不同的计算机上同时执行。也叫做网络透明性。

许多人一直将 KDE 或 Gnome 当成是 Window Manager，这种说法可以算对，也可以说只对了四分之一。正确的说，KDE 与 Gnome 应该算是 Desktop Environment，而不只是 Window Manager，因为 Window Manager 只是它们的一部份组件而已 (之前读者跑过的 kwm 便是 KDE 内建的 Window Manager)。 
所谓的 Desktop Environment ，指的是「提供完整的 X 应用程序、设定工具、容易使用的桌面、开始菜单以及内建 Window Manager 的一整套程序」。最早有 Sun MicroSystems 的 CDE (Common Desktop Environment)，而目前 Linux/FreeBSD 系统上普遍流行的 Desktop Environment 则为 KDE (The K Desktop Environment) 与 Gnome (GNU Network Object Model Environment) 这两套。 

将容器中的图形界面展示到外部的一般性思路：
目前Unix/Linux比较主流的图形界面服务是X11，而X11服务的图形显示方式实际上是一种Client/Server模式，在服务端和客户端之间，X11通过『DISPLAY』环境变量来指定将图形显示到何处。如下面的流程所示，请注意服务端与客户端的位置，服务端是用于提供显示信息的。
[应用程序]->[X11客户端]->[X11服务端]->[显示屏幕]

DISPLAY的格式是『unix:端口』或『主机名:端口』，前一种格式表示使用本地的unix套接字，后一种表示使用tcp套接字。

默认情况下，X11的服务端会监听本地的『unit:0』端口，而DISPLAY的默认值为『:0』，这实际上是『unit:0』的简写。因此如果在Linux的控制台启动一个图形程序，它就会出现在当前主机的显示屏幕中。

基于这个原理，将Docker中的GUI程序显示到外面，就是通过某种方式把X11的客户端的内容从容器里面传递出来。基本的思路无非有两种：
> - 通过SSH连接或远程控制软件，最终通过tcp套接字将数据发送出来
> - 让容器和主机共享X11的unix套接字，直接将数据发送出来

从应用场景上划分，又可以分成两类情况：『运行本地的GUI程序』和『运行远程服务器上的GUI程序』。这两类情况在操作上很相似，但前者可以使用unix套接字，而后者必然要使用tcp套接字转发，原理上有很大差别。

明确了这些内容，就可以知道对于X Window System的显示可以不用依赖于具体的操作系统。我们还可以通过Xming在windows环境下显示远程的Linux GUI程序。
当然还有其他的windows环境下的xserver可以做到这个功能。

当然X Window System已经存在很多年了，各种窗口管理器都是在他的基础上实现，从而给Linux带来了可视交互界面。
但是作为替代 X 下一代的简单“显示服务器”的Wayland项目，图形效率更快，目前GNOME、KDE等桌面环境已经在做移植到Wayland上来的工作。

参考文档：
[linux图形系统框架](http://blog.csdn.net/kjfureone/article/details/52848550)
[从程序员的角度看xwindows](http://linux.ximizi.com/linux/linux7124.htm)
[Xming —— 从 Windows 系统管理图像化的 Linux 应用程序](https://wiki.centos.org/zh/HowTos/Xming)
[下一代X Server：揭开Wayland的面纱](http://os.51cto.com/art/201011/233447_all.htm)
[揭开Wayland的面纱（一）：X Window的前生今世](https://imtx.me/archives/1573.html)
[Wayland与Weston简介](http://blog.csdn.net/jinzhuojun/article/details/47290707)
[鸟哥的私房菜：23.1 什么是 X Window System](https://wizardforcel.gitbooks.io/vbird-linux-basic-4e/content/202.html)
[如何在 Windows Subsystem for Linux (WSL) 上运行 Linux GUI 软件](http://www.yuan-ji.me/%E5%A6%82%E4%BD%95%E5%9C%A8Windows-Subsystem-for-Linux-(WSL)-%E4%B8%8A%E8%BF%90%E8%A1%8CLinux-GUI-%E8%BD%AF%E4%BB%B6/)

#### 本地GUI：
如果每个应用程序可以通过docker做到类似于独立沙盒中运行的方式，就可以避免了应用程序将配置文件和运行过程中生成的临时文件散乱的丢在系统各种目录中的问题。而且可以做到多个版本的应用程序同时存在的方式。

##### 第一个实际例子：
Dockerfile：
```shell
FROM scratch

# add basic rootfs
ADD ubuntu-xenial-core-cloudimg-amd64-root.tar.gz /

# replace apt-get source 
RUN echo "deb http://mirrors.aliyun.com/ubuntu/ xenial main restricted universe multiverse" >> /etc/apt/sources.list \
    echo "deb http://mirrors.aliyun.com/ubuntu/ xenial-security main restricted universe multiverse" >> /etc/apt/sources.list \
    echo "deb http://mirrors.aliyun.com/ubuntu/ xenial-updates main restricted universe multiverse" >> /etc/apt/sources.list \
    echo "deb http://mirrors.aliyun.com/ubuntu/ xenial-proposed main restricted universe multiverse" >> /etc/apt/sources.list \
    echo "deb http://mirrors.aliyun.com/ubuntu/ xenial-backports main restricted universe multiverse" >> /etc/apt/sources.list \
    echo "deb-src http://mirrors.aliyun.com/ubuntu/ xenial main restricted universe multiverse" >> /etc/apt/sources.list \
    echo "deb-src http://mirrors.aliyun.com/ubuntu/ xenial-security main restricted universe multiverse" >> /etc/apt/sources.list \
    echo "deb-src http://mirrors.aliyun.com/ubuntu/ xenial-updates main restricted universe multiverse" >> /etc/apt/sources.list \
    echo "deb-src http://mirrors.aliyun.com/ubuntu/ xenial-proposed main restricted universe multiverse" >> /etc/apt/sources.list \
    echo "deb-src http://mirrors.aliyun.com/ubuntu/ xenial-backports main restricted universe multiverse" >> /etc/apt/sources.list

# install GUI compents and eclipse
RUN apt-get update && apt-get install -y libgtk2.0-0 libcanberra-gtk-module sudo
RUN apt-get install -y eclipse

# Replace 1000 with your user / group id
RUN export uid=1000 gid=1000 && \
    mkdir -p /home/developer && \
    echo "developer:x:${uid}:${gid}:Developer,,,:/home/developer:/bin/bash" >> /etc/passwd && \
    echo "developer:x:${uid}:" >> /etc/group && \
    echo "developer ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/developer && \
    chmod 0440 /etc/sudoers.d/developer && \
    chown ${uid}:${gid} -R /home/developer

# setup env
USER developer
ENV HOME /home/developer
CMD /usr/bin/eclipse
```

然后构建：
docker build -t eclipse .

完成后启动：
docker run -ti --rm \
       -e DISPLAY=$DISPLAY \
       -v /tmp/.X11-unix:/tmp/.X11-unix \
       eclipse

关键参数：-v /tmp/.X11-unix:/tmp/.X11-unix
这个参数就是将主机上X11的unix套接字共享到了容器里面。因为每个unix套接字实际上就是系统/tmp/.X11-unix目录下面依据套接字编号命名的一个特殊文件。

注意：
从scratch安装的时候，如果没有install sudo，就会出现：
cannot create /etc/sudoers.d/openwrt: Directory nonexistent
这个错误。

然后结合数据卷来替换将/home/developer进行挂载，就可以完成开发环境的隔离和代码的持久化了。

参考文档：
[Docker搭建开发环境](http://blog.csdn.net/zhaodedong/article/details/47396963)
[github version: /bin/sh: 1: cannot create /etc/sudoers.d/openwrt: Directory nonexistent #4](https://github.com/noonien/docker-openwrt-buildroot/issues/4)
[林帆：Docker运行GUI软件的方法](http://www.csdn.net/article/2015-07-30/2825340)
[Dockercraft](https://github.com/docker/dockercraft)

##### 安装IDEA社区版本：
挺好玩的，再试试IDEA，并且使用数据卷来进行代码持久化。
这儿使用IDEA without jdk的版本，然后安装oracle的jdk，否则还是会存在当前系统环境变量中找不到JDK的问题，无法完成编译。

Dockerfile：
```shell
FROM scratch

# add basic rootfs
ADD ubuntu-xenial-core-cloudimg-amd64-root.tar.gz /

# replace apt-get source 
RUN echo "deb http://mirrors.aliyun.com/ubuntu/ xenial main restricted universe multiverse" >> /etc/apt/sources.list \
    echo "deb http://mirrors.aliyun.com/ubuntu/ xenial-security main restricted universe multiverse" >> /etc/apt/sources.list \
    echo "deb http://mirrors.aliyun.com/ubuntu/ xenial-updates main restricted universe multiverse" >> /etc/apt/sources.list \
    echo "deb http://mirrors.aliyun.com/ubuntu/ xenial-proposed main restricted universe multiverse" >> /etc/apt/sources.list \
    echo "deb http://mirrors.aliyun.com/ubuntu/ xenial-backports main restricted universe multiverse" >> /etc/apt/sources.list \
    echo "deb-src http://mirrors.aliyun.com/ubuntu/ xenial main restricted universe multiverse" >> /etc/apt/sources.list \
    echo "deb-src http://mirrors.aliyun.com/ubuntu/ xenial-security main restricted universe multiverse" >> /etc/apt/sources.list \
    echo "deb-src http://mirrors.aliyun.com/ubuntu/ xenial-updates main restricted universe multiverse" >> /etc/apt/sources.list \
    echo "deb-src http://mirrors.aliyun.com/ubuntu/ xenial-proposed main restricted universe multiverse" >> /etc/apt/sources.list \
    echo "deb-src http://mirrors.aliyun.com/ubuntu/ xenial-backports main restricted universe multiverse" >> /etc/apt/sources.list

# install gui-basic-compent, git and sudo
RUN apt-get update && apt-get install -y libxtst-dev libxrender-dev sudo

# add idea package and oracle jdk
ADD ideaIC-2017.2.3-no-jdk.tar.gz /usr/local/bin/
ADD jdk-8u144-linux-x64.tar.gz /usr/local/bin/

# add jdk to /etc/profile
#RUN echo "export JAVA_HOME=/usr/local/bin/jdk1.8.0_144" >> /etc/profile && \
#    echo "export PATH=$JAVA_HOME/bin:$PATH" >> /etc/profile && \
#    echo "export CLASSPATH=.:$JAVA_HOME/lib/dt.jar:$JAVA_HOME/lib/tools.jar" >> /etc/profile

# must setup JAVA_HOME and IDEA need it
ENV JAVA_HOME /usr/local/bin/jdk1.8.0_144


# Replace 1000 with your user / group id
RUN export uid=1000 gid=1000 && \
    mkdir -p /home/developer && \
    echo "developer:x:${uid}:${gid}:Developer,,,:/home/developer:/bin/bash" >> /etc/passwd && \
    echo "developer:x:${uid}:" >> /etc/group && \
    echo "developer ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/developer && \
    chmod 0440 /etc/sudoers.d/developer && \
    chown ${uid}:${gid} -R /home/developer

# 设置环境变量，将IDEA的主目录设置为/home/developer
USER developer
ENV HOME /home/developer

# cleanup 删除不必要的软件和Apt缓存包列表
RUN sudo apt-get autoclean && \
    sudo apt-get autoremove && \
    sudo rm -rf /var/lib/apt/lists/*

# 启动时执行的指令，需要在启动时export变量:
CMD /usr/local/bin/idea-IC-172.3968.16/bin/idea.sh
```
然后构建：
docker build -t idea .

完成后启动：
docker run -ti --rm \
       -e DISPLAY=$DISPLAY \
       -v /tmp/.X11-unix:/tmp/.X11-unix \
       idea
       
问题的关键就在于需要补全GUI程序所有的依赖，IDEA使用java的awt编写，所以还需要awt的依赖库。
并且要充分的使用Docker的分层存储特性，不要每一次都从scratch开始，浪费空间。

####### 需要注意的问题（1）
上述构建和调用方式都是建立在当前host存在桌面环境的情况下的，如果当前的host没有GUI界面，那么需要在Docker中添加窗口管理器来再server上进行GUI操作，就需要安装窗口管理器，一个比较精简的管理器是：fluxbox
看到上述安装中，并没有安装libgtk2.0-0和libcanberra-gtk-module，这是因为当前的rootfs中已经包含了X11的客户端，GUI程序可以将显示渲染的内容交给X11的client来进行处理，而不用关心究竟是在哪里显示出来的。
所以上述运行代码中的：
-v /tmp/.X11-unix:/tmp/.X11-unix
就是将host的x11挂载到docker中rootfs的x11了，等于替换了docker中的显示接口，然后docker中的进程调用x11来显示的时候就等于调用host的x11进行显示。果然是一切皆文件。
本质上就是使用了UNIX域套接字，用于在同一台机器上运行的进程之间的通信，将docker进程中的GUI指令在host中渲染。
所以从这个角度来看，Docker中的GUI程序的界面渲染和本地的GUI应用，除了进程间通信的效率损失以外，没有任何的区别。
####### 需要注意的问题（2）
在安装IDEA中选择了no-jdk的版本，也就是IDEA的运行需要依赖外部的java环境，也就是需要在环境变量中通过JAVA_HOME来获取java的根路径。
如果和正常的系统一样，在/etc/profile中添加：
```shell
export JAVA_HOME=/usr/local/bin/jdk1.8.0_144
export PATH=$JAVA_HOME/bin:$PATH
export CLASSPATH=.:$JAVA_HOME/lib/dt.jar:$JAVA_HOME/lib/tools.jar
```
然后写命令：
RUN sudo source /etc/profile
在启动IDEA的时候，会发现找不到JAVA_HOME，也就说明这种设置并不能生效。
这是因为Docker运行的时候就通过创建进程实现的，不会存在系统启用初始化加载的方式，就算是使用/sbin/init也是无法做到加载/etc/profile中的变量的。
这个时候就需要使用Dockerfile中的ENV来设置环境变量，从而正确启动IDEA。
####### 需要注意的问题（3）
运行成功之后，为了方便保存，可以通过将当前系统备份的方式进行处理，这样就能够得到完整的系统rootfs，并且空间占用比save出来要小：
首先推出GUI运行状态，然后进入维护运行状态，也就是：
docker run -ti -d idea_2017.2.3 /bin/bash
docker ps
docker commit -a "wentao" -m "init setting" XXX idea_2017.2.3
docker run -ti -v /home/alibaba/:/data idea_2017.2.3 /bin/bash
sudo tar --one-file-system --exclude './proc' --exclude './sys'  --exclude './data' -pzcvf ./data/test.tgz ./*
这样一个独立的GUI运行环境就制作完成了。
但是在启动的时候就一定要待上环境参数和启动参数了，否则因为之前Dockerfile中的ENV和CMD命令缺失，无法启动：
docker run -ti --rm \
       -e DISPLAY=$DISPLAY \
       -e "JAVA_HOME=/usr/local/bin/jdk1.8.0_144" \
       -v /tmp/.X11-unix:/tmp/.X11-unix \
       idea \
       /usr/local/bin/idea-IC-172.3968.16/bin/idea.sh
并且需要注意当前的运行环境中包含$DISPLAY这个变量，否则无法完成和x11的client交互，导致无法启动GUI。
但是就算是这样加入了还是不行：
```shell
Start Failed: Failed to initialize graphics environment

java.awt.AWTError: Can't connect to X11 window server using 'unix' as the value of the DISPLAY variable.
        at sun.awt.X11GraphicsEnvironment.initDisplay(Native Method)
        at sun.awt.X11GraphicsEnvironment.access$200(X11GraphicsEnvironment.java:65)
        at sun.awt.X11GraphicsEnvironment$1.run(X11GraphicsEnvironment.java:115)
        at java.security.AccessController.doPrivileged(Native Method)
        at sun.awt.X11GraphicsEnvironment.<clinit>(X11GraphicsEnvironment.java:74)
        at java.lang.Class.forName0(Native Method)
        at java.lang.Class.forName(Class.java:264)
        at java.awt.GraphicsEnvironment.createGE(GraphicsEnvironment.java:103)
        at java.awt.GraphicsEnvironment.getLocalGraphicsEnvironment(GraphicsEnvironment.java:82)
        at sun.awt.X11.XToolkit.<clinit>(XToolkit.java:126)
        at java.lang.Class.forName0(Native Method)
        at java.lang.Class.forName(Class.java:264)
        at java.awt.Toolkit$2.run(Toolkit.java:860)
        at java.awt.Toolkit$2.run(Toolkit.java:855)
        at java.security.AccessController.doPrivileged(Native Method)
        at java.awt.Toolkit.getDefaultToolkit(Toolkit.java:854)
        at java.awt.Toolkit.getEventQueue(Toolkit.java:1734)
        at java.awt.EventQueue.isDispatchThread(EventQueue.java:1043)
        at javax.swing.SwingUtilities.isEventDispatchThread(SwingUtilities.java:1361)
        at javax.swing.text.StyleContext.reclaim(StyleContext.java:454)
        at javax.swing.text.StyleContext.addAttribute(StyleContext.java:311)
        at javax.swing.text.html.StyleSheet.addAttribute(StyleSheet.java:578)
        at javax.swing.text.StyleContext$NamedStyle.addAttribute(StyleContext.java:1501)
        at javax.swing.text.StyleContext$NamedStyle.setName(StyleContext.java:1312)
        at javax.swing.text.StyleContext$NamedStyle.<init>(StyleContext.java:1259)
        at javax.swing.text.StyleContext.addStyle(StyleContext.java:107)
        at javax.swing.text.StyleContext.<init>(StyleContext.java:87)
        at javax.swing.text.html.StyleSheet.<init>(StyleSheet.java:166)
        at javax.swing.text.html.HTMLEditorKit.getStyleSheet(HTMLEditorKit.java:391)
        at com.intellij.util.ui.UIUtil.<clinit>(UIUtil.java:105)
        at com.intellij.ide.plugins.PluginManager.start(PluginManager.java:77)
        at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
        at sun.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:62)
        at sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:43)
        at java.lang.reflect.Method.invoke(Method.java:498)
        at com.intellij.ide.Bootstrap.main(Bootstrap.java:39)
        at com.intellij.idea.Main.main(Main.java:84)
```
使用:
docker run -ti idea /bin/bash
进入，然后查看：
ps -ef | egrep X11
可以看到X11正常运行。所以这个问题还是没有解决！！！！！！！！！！！！！
####### 需要注意的问题（4）
在使用中还可以通过数据卷挂载的方式来对设置工作目录。挂载host的路径到docker中，从而将工作目录持久化：
docker run -ti --rm \
        -e DISPLAY=$DISPLAY \
        -v /tmp/.X11-unix:/tmp/.X11-unix \
        -v /home/alibaba/java_work:/home/developer/java_work \
        idea
启动后就可以看到这个目录了。

参考文档：
> - [Dockerfile中的ADD和COPY的区别](http://milo.leanote.com/post/Dockerfile%E4%B8%AD%E7%9A%84ADD%E5%92%8CCOPY%E7%9A%84%E5%8C%BA%E5%88%AB)
> - [ADD 更高级的复制文件](https://yeasy.gitbooks.io/docker_practice/content/image/dockerfile/add.html)
> - [高级进程间通信之UNIX域套接字](http://www.cnblogs.com/nufangrensheng/p/3569416.html)
> - [你可以在Docker容器中运行GUI应用程序吗？](https://gxnotes.com/article/23395.html)
> - [Docker run 命令的使用方法](http://dockone.io/article/152)


##### 镜像拆分：
因为有很多的GUI应用，如果每一个都从头开始做起是不实际的，所以我们需要按照层次来创建image。
首先构建基础的镜像：
```shell
FROM scratch

# add basic rootfs
ADD ubuntu-xenial-core-cloudimg-amd64-root.tar.gz /

# replace apt-get source
RUN echo "deb http://mirrors.aliyun.com/ubuntu/ xenial main restricted universe multiverse" >> /etc/apt/sources.list \
    echo "deb http://mirrors.aliyun.com/ubuntu/ xenial-security main restricted universe multiverse" >> /etc/apt/sources.list \
    echo "deb http://mirrors.aliyun.com/ubuntu/ xenial-updates main restricted universe multiverse" >> /etc/apt/sources.list \
    echo "deb http://mirrors.aliyun.com/ubuntu/ xenial-proposed main restricted universe multiverse" >> /etc/apt/sources.list \
    echo "deb http://mirrors.aliyun.com/ubuntu/ xenial-backports main restricted universe multiverse" >> /etc/apt/sources.list \
    echo "deb-src http://mirrors.aliyun.com/ubuntu/ xenial main restricted universe multiverse" >> /etc/apt/sources.list \
    echo "deb-src http://mirrors.aliyun.com/ubuntu/ xenial-security main restricted universe multiverse" >> /etc/apt/sources.list \
    echo "deb-src http://mirrors.aliyun.com/ubuntu/ xenial-updates main restricted universe multiverse" >> /etc/apt/sources.list \
    echo "deb-src http://mirrors.aliyun.com/ubuntu/ xenial-proposed main restricted universe multiverse" >> /etc/apt/sources.list \
    echo "deb-src http://mirrors.aliyun.com/ubuntu/ xenial-backports main restricted universe multiverse" >> /etc/apt/sources.list

# install gui-basic-compent, git and sudo
RUN apt-get update && apt-get -y upgrade

# cleanup
RUN sudo apt-get autoclean && \
    sudo apt-get autoremove && \
    sudo rm -rf /var/lib/apt/lists/*
```
docker build -t ubuntu .

然后安装需要的 X window 支持：
```shell
FROM ui_ubuntu

# add idea package and oracle jdk
ADD ideaIC-2017.2.3-no-jdk.tar.gz /usr/local/bin/
ADD jdk-8u144-linux-x64.tar.gz /usr/local/bin/

# add jdk to /etc/profile
#RUN echo "export JAVA_HOME=/usr/local/bin/jdk1.8.0_144" >> /etc/profile && \
#    echo "export PATH=$JAVA_HOME/bin:$PATH" >> /etc/profile && \
#    echo "export CLASSPATH=.:$JAVA_HOME/lib/dt.jar:$JAVA_HOME/lib/tools.jar" >> /etc/profile

# must setup JAVA_HOME and IDEA need it
ENV JAVA_HOME /usr/local/bin/jdk1.8.0_144


# Replace 1000 with your user / group id
RUN export uid=1000 gid=1000 && \
    mkdir -p /home/developer && \
    echo "developer:x:${uid}:${gid}:Developer,,,:/home/developer:/bin/bash" >> /etc/passwd && \
    echo "developer:x:${uid}:" >> /etc/group && \
    echo "developer ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/developer && \
    chmod 0440 /etc/sudoers.d/developer && \
    chown ${uid}:${gid} -R /home/developer

# 设置环境变量，将IDEA的主目录设置为/home/developer
USER developer
ENV HOME /home/developer

# cleanup 删除不必要的软件和Apt缓存包列表
RUN sudo apt-get autoclean && \
    sudo apt-get autoremove && \
    sudo rm -rf /var/lib/apt/lists/*

# 启动时执行的指令，需要在启动时export变量:
CMD /usr/local/bin/idea-IC-172.3968.16/bin/idea.sh
```

然后可以安装chrome了。



#### 远程GUI：
和上述本地GUI不同，远程GUI程序将显示渲染传递给自己的X11客户端之后，如果需要在远程显示，就需要X11的client和server进行远程通信，而不是本地的进程间通信了，而远程通信一般都采用TCP套接字进行转发。

远程GUI应用的情况多出现在将Docker作为产品测试环境使用的场景。利用Docker用后既消除的特点，能够快速的为每次测试提供干净的上下文环境。有时为了在非Linux系统中使用Linux的图形化软件，也可以通过远程Docker运行的方法实现。
此时，整个数据连接实际就变成了这样的：
[应用程序]->[X11客户端]->[SSH服务端]->[SSH客户端]->[X11服务端]->[显示屏幕]



### 存储的方式：





[Manage data in Docker](https://docs.docker.com/engine/admin/volumes/#more-details-about-mount-types)
[Docker解析：数据卷(Data Volume)的实现](http://hustcat.github.io/docker-data-volume/)


### 共享kernel带来的风险：
Docker container和host之间的隔离性并没有那么的强，可以参考：
[a process in the container has managed to escape into the kernel space of the host](https://nvd.nist.gov/vuln/detail/CVE-2014-9357)

### Docker ulimit：
在ubuntu16.04环境下，systemd的配置默认路径为：
/lib/systemd/system
进入查看docker.service：
```shell
[Unit]
Description=Docker Application Container Engine
Documentation=https://docs.docker.com
After=network-online.target docker.socket firewalld.service
Wants=network-online.target
Requires=docker.socket

[Service]
Type=notify
# the default is not to use systemd for cgroups because the delegate issues still
# exists and systemd currently does not support the cgroup feature set required
# for containers run by docker
ExecStart=/usr/bin/dockerd -H fd://
ExecReload=/bin/kill -s HUP $MAINPID
LimitNOFILE=1048576
# Having non-zero Limit*s causes performance problems due to accounting overhead
# in the kernel. We recommend using cgroups to do container-local accounting.
LimitNPROC=infinity
LimitCORE=infinity
# Uncomment TasksMax if your systemd version supports it.
# Only systemd 226 and above support this version.
TasksMax=infinity
TimeoutStartSec=0
# set delegate yes so that systemd does not reset the cgroups of docker containers
Delegate=yes
# kill only the docker process, not all processes in the cgroup
KillMode=process
# restart the docker process if it exits prematurely
Restart=on-failure
StartLimitBurst=3
StartLimitInterval=60s

[Install]
WantedBy=multi-user.target
```

## Dockerfile语法：
通过Dockerfile可以对镜像编程，所以这是一个非常关键的镜像生成方式。
根据官方文档：[Dockerfile reference](https://docs.docker.com/engine/reference/builder/) 来学习具体的语法规范。

我们通过一个构建镜像的例子来熟悉一下具体的语法和构建流程，然后再深入学习。

### 使用Alpine基础镜像和Dockerfile构建tomcat运行环境：
为了展示一个完成的流程，并且加快测试中等待的时间，我们使用Alpine作为基础镜像，然后使用oracle JDK作为基础java运行环境，然后在tomcat7下进行测试。

#### 制作alpine基础镜像:
wget http://dl-cdn.alpinelinux.org/alpine/v3.6/releases/x86_64/alpine-minirootfs-3.6.2-x86_64.tar.gz
Dockerfile内容为：
```shell
FROM scratch
ADD alpine-minirootfs-3.6.2-x86_64.tar.gz /
CMD ["/bin/sh"]
```
docker build -t alpine .
就会生成名称为alpine的基础镜像了。

需要注意的是，在alpine中需要设置时区，才能保证时钟的正常：
alpine 下修改UTC时间为CST时间 (测试通过)
```shell
apk add tzdata 
ln -sf /usr/share/zoneinfo/Asia/Shanghai /etc/localtime 
echo "Asia/Shanghai" > /etc/timezone
```
然后更换alpine的源为国内的：
http://mirrors.ustc.edu.cn/alpine/v3.6/main
http://mirrors.ustc.edu.cn/alpine/v3.6/community
最后综合的结果为：
```shell
FROM scratch
ADD alpine-minirootfs-3.6.2-x86_64.tar.gz /
RUN rm -f /etc/apk/repositories && \
    echo "http://mirrors.ustc.edu.cn/alpine/v3.6/main" >> /etc/apk/repositories && \
    echo "http://mirrors.ustc.edu.cn/alpine/v3.6/community" >> /etc/apk/repositories
RUN apk update --update && apk add tzdata && \
    ln -sf /usr/share/zoneinfo/Asia/Shanghai /etc/localtime && \
    echo "Asia/Shanghai" > /etc/timezone
CMD ["/bin/sh"]
```

#### 从官方网站下载oracle jdk和tomcat7:
首先现在tomcat7的当前最新版本：
wget http://mirror.bit.edu.cn/apache/tomcat/tomcat-7/v7.0.81/bin/apache-tomcat-7.0.81.tar.gz

然后下载当前oracle jdk的最新版本：
export JAVA_VERSION_MAJOR=8 \
    JAVA_VERSION_MINOR=144 \
    JAVA_VERSION_BUILD=01 \
    JAVA_PACKAGE=jdk
wget --no-check-certificate -c --header "Cookie: oraclelicense=accept-securebackup-cookie" http://download.oracle.com/otn-pub/java/jdk/${JAVA_VERSION_MAJOR}u${JAVA_VERSION_MINOR}-b${JAVA_VERSION_BUILD}/${JAVA_PACKAGE}-${JAVA_VERSION_MAJOR}u${JAVA_VERSION_MINOR}-linux-x64.tar.gz -o /tmp/java.tar.gz

#### 构建tomcat工程：
通过设置JAVA_HOME和tomcat的基本运行环境搭建Dockerfile：
```shell
FROM alpine

# 释放基本文件：
ADD jdk-8u144-linux-x64.tar.gz /usr/local/bin
ADD apache-tomcat-7.0.81.tar.gz /usr/local/bin
ADD startup.sh startup.sh

# 更改系统的时区设置
RUN apk update && apk add curl bash tree tzdata \
    && cp -r -f /usr/share/zoneinfo/Hongkong /etc/localtime

# 设置jdk的环境变量
ENV JAVA_HOME /usr/local/bin/jdk1.8.0_144 \
    CLASSPATH $JAVA_HOME/lib/dt.jar:$JAVA_HOME/lib/tools.jar

# 设置tomcat的环境变量
ENV CATALINA_HOME /usr/local/bin/apache-tomcat-7.0.81 \
    CATALINA_BASE /usr/local/bin/apache-tomcat-7.0.81

# 设置系统环境变量
ENV PATH $PATH:$JAVA_HOME/bin:$CATALINA_HOME/lib:$CATALINA_HOME/bin


# 暴露tomcat接口，默认为8080
EXPOSE 8080

# 设置自启动时候执行的命令
CMD ["sh", "startup.sh"]
```
其中startup.sh
```shell
#!/bin/sh

/bin/sh /usr/local/bin/apache-tomcat-7.0.81/bin/startup.sh
```
生成镜像：
docker build -t tomcat .
然后就可以启动了：
docker run -it -d -p 5000:8080 tomcat 
但是这样在显示tomcat started之后立即退出了。


所以为了避免退出，需要使用：
```shell
FROM alpine

# 释放基本文件：
ADD jdk-8u144-linux-x64.tar.gz /usr/local/bin
ADD apache-tomcat-7.0.81.tar.gz /usr/local/bin
ADD startup.sh startup.sh

# 更改系统的时区设置
RUN apk update && apk add curl bash tree tzdata \
    && cp -r -f /usr/share/zoneinfo/Hongkong /etc/localtime

# 设置jdk的环境变量
ENV JAVA_HOME /usr/local/bin/jdk1.8.0_144 \
    CLASSPATH $JAVA_HOME/lib/dt.jar:$JAVA_HOME/lib/tools.jar

# 设置tomcat的环境变量
ENV CATALINA_HOME /usr/local/bin/apache-tomcat-7.0.81 \
    CATALINA_BASE /usr/local/bin/apache-tomcat-7.0.81

# 设置系统环境变量
ENV PATH $PATH:$JAVA_HOME/bin:$CATALINA_HOME/lib:$CATALINA_HOME/bin


# 暴露tomcat接口，默认为8080
EXPOSE 8080

# 入口 
ENTRYPOINT /usr/local/bin/apache-tomcat-7.0.81/bin/startup.sh && tail -F /usr/local/bin/apache-tomcat-7.0.81/logs/catalina.out
```
来持续获取当前的tomcat输出。
docker run -d -p 8090:8080 tomcat
就可以了。
但是执行后还是自动退出，使用sh启动当前的镜像：
docker run -ti tomcat /bin/sh
然后检查输出日志：/usr/local/bin/apache-tomcat-7.0.81/logs/catalina.out
发现：
eval: line 1: /usr/local/bin/jdk1.8.0_144/bin/java: not found
最终确认问题是在于alpine不支持当前的jdk，因为apline是基于musl libc和busybox的，而oracle jdk是需要glibc运行时库的。

关于Linux下的不同C运行时库的实现可以参考：
http://www.etalabs.net/compare_libcs.html

如果还是要用alpine作为基础镜像，就需要对当前的alpine linux添加glibc支持，然后安装JDK进行测试。
需要从：
https://github.com/sgerrand/alpine-pkg-glibc
获取对应的三个apk文件，然后进行安装。
最终的版本Dockerfile文件内容为：
```shell
FROM alpine

# 添加基本文件
ADD jdk-8u144-linux-x64.tar.gz /usr/local/bin/
ADD apache-tomcat-7.0.81.tar.gz /usr/local/bin
COPY *.apk /tmp/

# 安装glibc支持
RUN apk upgrade --update && \
    apk add bash tree && \
    apk add --allow-untrusted /tmp/*.apk && \
    rm -v /tmp/*.apk && \
    ( /usr/glibc-compat/bin/localedef --force --inputfile POSIX --charmap UTF-8 C.UTF-8 || true ) && \
    echo "export LANG=C.UTF-8" > /etc/profile.d/locale.sh && \
    /usr/glibc-compat/sbin/ldconfig /lib /usr/glibc-compat/lib

# 设置jdk的环境变量
ENV JAVA_HOME=/usr/local/bin/jdk1.8.0_144 \
    CLASSPATH=$JAVA_HOME/lib/dt.jar:$JAVA_HOME/lib/tools.jar

# 设置tomcat的环境变量
ENV CATALINA_HOME=/usr/local/bin/apache-tomcat-7.0.81 \
    CATALINA_BASE=/usr/local/bin/apache-tomcat-7.0.81

# 设置系统环境变量
ENV PATH=$PATH:$JAVA_HOME/bin:$CATALINA_HOME/lib:$CATALINA_HOME/bin

# 暴露tomcat接口，默认为8080
EXPOSE 8080

# 入口 
ENTRYPOINT /usr/local/bin/apache-tomcat-7.0.81/bin/startup.sh && tail -F /usr/local/bin/apache-tomcat-7.0.81/logs/catalina.out
```
启动容器：
docker run -d -p 8090:8080 tomcat
然后在host上访问：
http://localhost:8090
就可以看到tomcat正常的启动页面了。

#### 最精简的运行环境：
上述环境虽然可以运行，但是在JDK中包含了很多的不必要文件，可以通过删除这些文件，减少文件体积。
REPOSITORY          TAG                 IMAGE ID            CREATED             SIZE
tomcat              latest              c1ec98774feb        41 minutes ago      432MB
alpine              latest              558174b6522c        2 hours ago         6.38MB
idea                latest              2ffe3048c6ad        3 days ago          1.4GB
centos_dev          latest              8de4b2603d68        3 days ago          567MB
ubuntu              latest              9a402b69afd5        3 days ago          120MB
busybox             latest              79211bdafeec        3 days ago          4.39MB


```shell
FROM alpine

# 添加基本文件
ADD jdk-8u144-linux-x64.tar.gz /usr/local/bin/
ADD apache-tomcat-7.0.81.tar.gz /usr/local/bin
COPY *.apk /tmp/

# 安装glibc支持
RUN apk upgrade --update && \
    apk add bash tree && \
    apk add --allow-untrusted /tmp/*.apk && \
    rm -v /tmp/*.apk && \
    ( /usr/glibc-compat/bin/localedef --force --inputfile POSIX --charmap UTF-8 C.UTF-8 || true ) && \
    echo "export LANG=C.UTF-8" > /etc/profile.d/locale.sh && \
    /usr/glibc-compat/sbin/ldconfig /lib /usr/glibc-compat/lib && \
    mv /usr/local/bin/jdk1.8.0_144/jre /jre && \
    rm /jre/bin/jjs && \
    rm /jre/bin/keytool && \
    rm /jre/bin/orbd && \
    rm /jre/bin/pack200 && \
    rm /jre/bin/policytool && \
    rm /jre/bin/rmid && \
    rm /jre/bin/rmiregistry && \
    rm /jre/bin/servertool && \
    rm /jre/bin/tnameserv && \
    rm /jre/bin/unpack200 && \
    rm /jre/lib/ext/nashorn.jar && \
    rm /jre/lib/jfr.jar && \
    rm -rf /jre/lib/jfr && \
    rm -rf /jre/lib/oblique-fonts && \
    rm -rf /tmp/* /var/cache/apk/* && \
    echo 'hosts: files mdns4_minimal [NOTFOUND=return] dns mdns4' >> /etc/nsswitch.conf

# 设置jdk的环境变量
ENV JAVA_HOME=/usr/local/bin/jdk1.8.0_144 \
    CLASSPATH=$JAVA_HOME/lib/dt.jar:$JAVA_HOME/lib/tools.jar

# 设置tomcat的环境变量
ENV CATALINA_HOME=/usr/local/bin/apache-tomcat-7.0.81 \
    CATALINA_BASE=/usr/local/bin/apache-tomcat-7.0.81

# 设置系统环境变量
ENV PATH=$PATH:$JAVA_HOME/bin:$CATALINA_HOME/lib:$CATALINA_HOME/bin

# 暴露tomcat接口，默认为8080
EXPOSE 8080

# 入口 
ENTRYPOINT /usr/local/bin/apache-tomcat-7.0.81/bin/startup.sh && tail -F /usr/local/bin/apache-tomcat-7.0.81/logs/catalina.out
```
可以看到少了100M空间。

其实如果是为了运行时库，而不是开发，可以选用oracle的Server JRE：
http://www.oracle.com/technetwork/java/javase/downloads/server-jre8-downloads-2133154.html
其他都不用变化，只是将其中ADD的部分的包进行替换就好。
可以看到体积变化：
REPOSITORY          TAG                 IMAGE ID            CREATED             SIZE
toto                latest              b51ed8d5b2a0        32 seconds ago      207MB
jenkins/alpine      latest              618b763bb1bb        13 hours ago        703MB
tomcat              latest              c1ec98774feb        15 hours ago        432MB
alpine              latest              558174b6522c        16 hours ago        6.38MB
idea                latest              2ffe3048c6ad        3 days ago          1.4GB
centos_dev          latest              8de4b2603d68        3 days ago          567MB
ubuntu              latest              9a402b69afd5        3 days ago          120MB
busybox             latest              79211bdafeec        3 days ago          4.39MB

只有207M，而且是集成了tomcat的，已经非常小了。

参考文档：
http://dev.dafan.info/detail/221368?p=32-4
https://hub.docker.com/r/anapsix/alpine-java/~/dockerfile/

#### 镜像分层：
回顾上述过程，我们可以将整个流程拆分，然后使用不同的Dockerfile来做层次分离，方便后续不同应用的集成。
在docker build的时候，通过-f来添加文件支持：
docker build -f /path/to/a/Dockerfile -t tagName .

首先，看看我们最终得到的一个Dockerfile：
```shell
# alpine:3.6.2 glibc:2.25 oracle_jre:1.8.144
FROM scratch

# 添加基本文件支持
ADD alpine-minirootfs-3.6.2-x86_64.tar.gz /
ADD server-jre-8u144-linux-x64.tar.gz /usr/local/bin
ADD apache-tomcat-7.0.81.tar.gz /usr/local/bin
COPY *.apk /tmp/

# 安装系统基础包，并且设置
RUN rm -f /etc/apk/repositories && \
    echo "http://mirrors.ustc.edu.cn/alpine/v3.6/main" >> /etc/apk/repositories && \
    echo "http://mirrors.ustc.edu.cn/alpine/v3.6/community" >> /etc/apk/repositories && \
    apk update --update && apk add bash tree tzdata && \
    ln -sf /usr/share/zoneinfo/Asia/Shanghai /etc/localtime && \
    echo "Asia/Shanghai" > /etc/timezone && \
    apk add --allow-untrusted /tmp/*.apk && \
    rm -v /tmp/*.apk && \
    ( /usr/glibc-compat/bin/localedef --force --inputfile POSIX --charmap UTF-8 C.UTF-8 || true ) && \
    echo "export LANG=C.UTF-8" > /etc/profile.d/locale.sh && \
    /usr/glibc-compat/sbin/ldconfig /lib /usr/glibc-compat/lib

# 设置环境变量
ENV JAVA_HOME=/usr/local/bin/jdk1.8.0_144 \
    CLASSPATH=$JAVA_HOME/lib/dt.jar:$JAVA_HOME/lib/tools.jar \
    CATALINA_HOME=/usr/local/bin/apache-tomcat-7.0.81 \
    CATALINA_BASE=/usr/local/bin/apache-tomcat-7.0.81 \
    PATH=$PATH:$JAVA_HOME/bin:$CATALINA_HOME/lib:$CATALINA_HOME/bin

# 暴露tomcat接口，默认为8080
EXPOSE 8080

# 入口 
ENTRYPOINT /usr/local/bin/apache-tomcat-7.0.81/bin/startup.sh && tail -F /usr/local/bin/apache-tomcat-7.0.81/logs/catalina.out
```
docker run -d -p 8090:8080 tagName
启动后台运行。

确认了这些，就可以对上述过程进行拆分，然后通过不同的dockerfile进行组织：
```基础alpine设置源和时区镜像
FROM scratch
ADD alpine-minirootfs-3.6.2-x86_64.tar.gz /
RUN rm -f /etc/apk/repositories && \
    echo "http://mirrors.ustc.edu.cn/alpine/v3.6/main" >> /etc/apk/repositories && \
    echo "http://mirrors.ustc.edu.cn/alpine/v3.6/community" >> /etc/apk/repositories
RUN apk update --update && apk add bash tzdata && \
    ln -sf /usr/share/zoneinfo/Asia/Shanghai /etc/localtime && \
    echo "Asia/Shanghai" > /etc/timezone && \
    rm -rf /var/cache/apk/*
CMD ["/bin/bash"]
```

```基础alpine包含glibc镜像
FROM alpine.base

# 添加glibc的安装包
COPY *.apk /tmp/

# 安装glibc等支持
RUN apk upgrade --update && \
    apk add --allow-untrusted /tmp/*.apk && \
    rm -v /tmp/*.apk && \
    ( /usr/glibc-compat/bin/localedef --force --inputfile POSIX --charmap UTF-8 C.UTF-8 || true ) && \
    echo "export LANG=C.UTF-8" > /etc/profile.d/locale.sh && \
    /usr/glibc-compat/sbin/ldconfig /lib /usr/glibc-compat/lib && \
    rm -rf /tmp/* /var/cache/apk/* && \
    echo 'hosts: files mdns4_minimal [NOTFOUND=return] dns mdns4' >> /etc/nsswitch.conf && \
    rm -rf /var/cache/apk/*
```

```oracle java运行环境
FROM alpine.glibc

# 添加基本文件
ADD server-jre-8u144-linux-x64.tar.gz /usr/local/bin/

# 设置jdk的环境变量
ENV JAVA_HOME=/usr/local/bin/jdk1.8.0_144 \
    CLASSPATH=$JAVA_HOME/lib/dt.jar:$JAVA_HOME/lib/tools.jar

# 设置系统环境变量
ENV PATH=$PATH:$JAVA_HOME/bin
```

```tomca支持
FROM alpine.oracle_jre

ADD apache-tomcat-7.0.81.tar.gz /usr/local/bin

# 设置tomcat的环境变量
ENV CATALINA_HOME=/usr/local/bin/apache-tomcat-7.0.81 \
    CATALINA_BASE=/usr/local/bin/apache-tomcat-7.0.81

# 设置系统环境变量
ENV PATH=$PATH:$CATALINA_HOME/lib:$CATALINA_HOME/bin

# 暴露tomcat接口，默认为8080
EXPOSE 8080

# 入口 
ENTRYPOINT /usr/local/bin/apache-tomcat-7.0.81/bin/startup.sh && tail -F /usr/local/bin/apache-tomcat-7.0.81/logs/catalina.out
```

然后创建生成脚本:
docker build -f base.dockerfile -t alpine.base .
docker build -f glibc.dockerfile -t alpine.glibc .
docker build -f oraclejre.dockerfile -t alpine.oracle_jre .
docker build -f tomcat.dockerfile -t alpine.tomcat .
创建删除脚本：
docker rmi -f alpine.tomcat
docker rmi -f alpine.oracle_jre
docker rmi -f alpine.glibc
docker rmi -f alpine.base
最后，启动tomcat：
docker run -d -p 8090:8080 alpine.tomcat

#### 部署测试：
完成了tomcat部署之后，测试war部署。
首先拷贝war到当前的容器中：
docker cp 本地文件路径 容器ID:容器路径
对应的命令为：
docker cp jenkins.war 5bbeb992d703:/usr/local/bin/apache-tomcat-7.0.81/webapps/
然后修改tomcat的tomcat-user.xml，添加图形界面的管理员：
```xml
<role rolename="manager-gui"/>
<user username="tomcat" password="s3cret" roles="manager-gui"/>
```
然后重启tomcat服务，就可以启动jenkins服务了。

#### 其他文档：
或者更换其他支持glibc的发行版，通过搜索，确认tiny core linux比较合适：
https://hub.docker.com/r/tatsushid/tinycore/
http://tinycorelinux.net/downloads.html
http://tinycorelinux.net/8.x/x86_64/release/

https://github.com/gliderlabs/docker-alpine/issues/11
https://stackoverflow.com/questions/45147371/docker-alpine-oracle-java-cannot-find-java
https://wiki.alpinelinux.org/wiki/Running_glibc_programs
适配：
https://github.com/sgerrand/alpine-pkg-glibc
https://github.com/frol/docker-alpine-oraclejdk8
https://hub.docker.com/r/anapsix/alpine-java/
或者其他的dist：
https://superuser.com/questions/307087/linux-distro-with-just-busybox-and-bash


### Dockerfile的语法细节：

## Docker应用实践：
在熟悉了关于Docker的基本概念和Dockerfile的基本编写内容后，我们就可以使用Docker来做一些有趣的事情了。

### 基于alpine搭建IDEA的运行环境：
之前我们基于完整的ubuntu和oracle jdk搭建过，现在可以轻量级使用alpine来重新搭建，看看能节省多少空间？
```shell
FROM alpine.oracle_jre

ADD ideaIC-2017.2.3-no-jdk.tar.gz /usr/local/bin/

# install gui-basic-compent, git and sudo
RUN apk update --update && \
    apk add libxtst-dev libxrender-dev sudo && \
    export uid=1000 gid=1000 && \
    mkdir -p /home/developer && \
    echo "developer:x:${uid}:${gid}:Developer,,,:/home/developer:/bin/bash" >> /etc/passwd && \
    echo "developer:x:${uid}:" >> /etc/group && \
    echo "developer ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/developer && \
    chmod 0440 /etc/sudoers.d/developer && \
    chown ${uid}:${gid} -R /home/developer && \
    sudo rm -rf /var/cache/apk/*
    

# 设置环境变量，将IDEA的主目录设置为/home/developer
USER developer
ENV HOME /home/developer

# 启动时执行的指令，需要在启动时export变量:
CMD /usr/local/bin/idea-IC-172.3968.16/bin/idea.sh
```
docker build -t idea_alpine .
然后启动：
docker run -ti --rm \
       -e DISPLAY=$DISPLAY \
       -v /tmp/.X11-unix:/tmp/.X11-unix \
       idea_alpine

然后查看大小：
REPOSITORY          TAG                 IMAGE ID            CREATED             SIZE
alpine.idea         latest              c9df0cfda12f        6 seconds ago       860MB
idea                latest              2ffe3048c6ad        3 days ago          1.4GB
比较基于ubuntu的还是小了不少。

### 基于alpine搭建chrome浏览器：

### 基于ubuntu搭建openjdk8编译环境：
有时候我们需要进行软件测试，而测试环境需要安装一堆的依赖包，不同的测试版本依赖不同。这个时候使用docker可以非常方便的解决这个问题。
现在我们基于最新的ubuntu镜像，我们搭建openjdk8的编译环境。
首先，搭建最基础的ubuntu的docker镜像，使用如下Dockerfile：
```dockerfile
FROM scratch
#ADD ubuntu-xenial-core-cloudimg-amd64-root.tar.gz /
ADD https://partner-images.canonical.com/core/xenial/current/ubuntu-xenial-core-cloudimg-amd64-root.tar.gz /

# a few minor docker-specific tweaks
# see https://github.com/docker/docker/blob/9a9fc01af8fb5d98b8eec0740716226fadb3735c/contrib/mkimage/debootstrap
RUN set -xe \
        \
# https://github.com/docker/docker/blob/9a9fc01af8fb5d98b8eec0740716226fadb3735c/contrib/mkimage/debootstrap#L40-L48
        && echo '#!/bin/sh' > /usr/sbin/policy-rc.d \
        && echo 'exit 101' >> /usr/sbin/policy-rc.d \
        && chmod +x /usr/sbin/policy-rc.d \
        \
# https://github.com/docker/docker/blob/9a9fc01af8fb5d98b8eec0740716226fadb3735c/contrib/mkimage/debootstrap#L54-L56
        && dpkg-divert --local --rename --add /sbin/initctl \
        && cp -a /usr/sbin/policy-rc.d /sbin/initctl \
        && sed -i 's/^exit.*/exit 0/' /sbin/initctl \
        \
# https://github.com/docker/docker/blob/9a9fc01af8fb5d98b8eec0740716226fadb3735c/contrib/mkimage/debootstrap#L71-L78
        && echo 'force-unsafe-io' > /etc/dpkg/dpkg.cfg.d/docker-apt-speedup \
        \
# https://github.com/docker/docker/blob/9a9fc01af8fb5d98b8eec0740716226fadb3735c/contrib/mkimage/debootstrap#L85-L105
        && echo 'DPkg::Post-Invoke { "rm -f /var/cache/apt/archives/*.deb /var/cache/apt/archives/partial/*.deb /var/cache/apt/*.bin || true"; };' > /etc/apt/apt.conf.d/docker-clean \
        && echo 'APT::Update::Post-Invoke { "rm -f /var/cache/apt/archives/*.deb /var/cache/apt/archives/partial/*.deb /var/cache/apt/*.bin || true"; };' >> /etc/apt/apt.conf.d/docker-clean \
        && echo 'Dir::Cache::pkgcache ""; Dir::Cache::srcpkgcache "";' >> /etc/apt/apt.conf.d/docker-clean \
        \
# https://github.com/docker/docker/blob/9a9fc01af8fb5d98b8eec0740716226fadb3735c/contrib/mkimage/debootstrap#L109-L115
        && echo 'Acquire::Languages "none";' > /etc/apt/apt.conf.d/docker-no-languages \
        \
# https://github.com/docker/docker/blob/9a9fc01af8fb5d98b8eec0740716226fadb3735c/contrib/mkimage/debootstrap#L118-L130
        && echo 'Acquire::GzipIndexes "true"; Acquire::CompressionTypes::Order:: "gz";' > /etc/apt/apt.conf.d/docker-gzip-indexes \
        \
# https://github.com/docker/docker/blob/9a9fc01af8fb5d98b8eec0740716226fadb3735c/contrib/mkimage/debootstrap#L134-L151
        && echo 'Apt::AutoRemove::SuggestsImportant "false";' > /etc/apt/apt.conf.d/docker-autoremove-suggests

# delete all the apt list files since they're big and get stale quickly
RUN rm -rf /var/lib/apt/lists/*
# this forces "apt-get update" in dependent images, which is also good

# enable the universe
RUN sed -i 's/^#\s*\(deb.*universe\)$/\1/g' /etc/apt/sources.list

# make systemd-detect-virt return "docker"
# See: https://github.com/systemd/systemd/blob/aa0c34279ee40bce2f9681b496922dedbadfca19/src/basic/virt.c#L434
RUN mkdir -p /run/systemd && echo 'docker' > /run/systemd/container

# overwrite this with 'CMD []' in a dependent Dockerfile
CMD ["/bin/bash"]
```
docker build -f base.dockerfile -t ubuntu .

然后在这个基础镜像上完成编译环境的搭建：
```dockerfile
FROM ubuntu

# 安装必要的工具
RUN apt-get update && \
    apt-get install -y mercurial build-essential \
    libx11-dev libxext-dev libxrender-dev libxtst-dev libxt-dev \
    libcups2-dev libfreetype6-dev \
    libasound2-dev ccache gawk m4 unzip zip libasound2-dev \
    libxrender-dev xorg-dev xutils-dev binutils libmotif-dev ant

# 安装openjdk7作为bootstrap编译器
RUN apt-get install software-properties-common python-software-properties && \
    add-apt-repository ppa:openjdk-r/ppa && \
    apt-get update && \
    apt-get install openjdk-7-jdk
```
可以看到安装了非常多的依赖包，这里也可以通过将安装的依赖包分离，从而得到不同层次的编译环境。
docker buidl -f openjdk7.dockerfile -t openjdk7/ubuntu .

然后就可以获取openjdk8的代码，然后执行编译。
docker run -ti -v /home/workspcce/:/share_data/ openjdk7/ubuntu /bin/bash
然后下载源代码到数据卷中：
hg clone http://hg.openjdk.java.net/jdk8u jdk8u
chmod 755 get_source.sh
./get_source.sh
./configure --prefix=/usr/local/bin/ >> configure.log
make all ZIP_DEBUGINFO_FILES=0 DISABLE_HOTSPOT_OS_VERSION_CHECK=OK >> make.log
完成后可以在build目录下找到当前编译环境相关的文件夹：
/share_data/jdk8u/build/linux-x86_64-normal-server-release
然后进入jdk/bin，就可以看到编译好的java可执行文件了：
./java -version
返回结果为：
```shell
openjdk version "1.8.0-internal"
OpenJDK Runtime Environment (build 1.8.0-internal-_2017_09_13_06_20-b00)
OpenJDK 64-Bit Server VM (build 25.71-b00, mixed mode)
```
然后执行安装：
make install
测试编译好的jdk，就可以在prefix中设置的路径下对java进行设置了。

参考文档：
[在docker上编译openjdk8](http://blog.csdn.net/boling_cavalry/article/details/70243954)
[openjdk8最新源码编译及使用(ubuntu16.04)](http://blog.csdn.net/love254443233/article/details/76378002)

### 基于Alpine的最小化java8运行环境：
有时候我们只需要一个最小化的java运行环境来给自己的应用提供基础环境。
既然是最小化环境，那么就是在保证可用的前提下越小越好。我们可以采用alpine来作为基础镜像来做到这一点：
#### OpenJDK8
我们在base的基础上直接安装jre：
```dockerfile
FROM alpine.base

# 安装openjdk_8.131.11-r2
RUN apk upgrade --update-cache && \
    apk add openjdk8-jre && \
    rm -rf /tmp/* /var/cache/apk/*
```
这样一个最小化的OpenJDK8-JRE环境就搭建好了。

#### OracleJDK8
OracleJDK是基于glibc运行时库的，所以使用base镜像是无法满足的，必须使用glibc镜像。
并且选择使用Oracle的JRE发布版来减少容量。
```dockerfile
FROM alpine.glibc

# 添加基本文件
ADD server-jre-8u144-linux-x64.tar.gz /usr/local/bin/

# 设置jdk的环境变量
ENV JAVA_HOME=/usr/local/bin/jdk1.8.0_144 \
    CLASSPATH=$JAVA_HOME/lib/dt.jar:$JAVA_HOME/lib/tools.jar

# 设置系统环境变量
ENV PATH=$PATH:$JAVA_HOME/bin
```

#### 容量对比：
```shell
alpine.openjdk8_jre   latest              08ea990eaa19        16 minutes ago      87.5MB
alpine.openjdk8       latest              b7b620d5b9b4        19 hours ago        105MB
alpine.oracle_jre     latest              a0bc990e287d        9 days ago          192MB
```
可以看出来使用openjdk8-jre能够做到最小的镜像。

其他文档：
[Smaller Java images with Alpine Linux](https://developer.atlassian.com/blog/2015/08/minimal-java-docker-containers/)

## Docker仓库管理：Docker registry
到现在为止，我们建立并且运行了很多的docker容器，现在希望把所有的这些容器打包给别人使用，难道只能通过save等方式导出然后分享吗？
我们可以借助于Docker仓库来给别人提供服务，对方只要能够SSL到我们这边，就可以通过docker pull来获取镜像了。

http://dockone.io/article/747
v1已经被废弃，现在使用的都是v2，但是开发还在缓慢进行中：
https://github.com/docker/distribution

### 初识Docker Hub：
[Docker Hub](https://hub.docker.com)是一个由Docker公司负责维护的公共注册中心，它包含了超过15,000个可用来下载和构建容器的镜像，并且还提供认证、工作组结构、工作流工具（比如webhooks）、构建触发器以及私有工具（比如私有仓库可用于存储你并不想公开分享的镜像）。
使用Docker Hub主要的命令为四个：
login
pull
push
search





## Docker实现的Linux基础：
Docker最早是在linux上的一种轻量级虚拟化，虽然现在windows上也有支持，但是也是需要在内核上修改才可以的。
### 在A上的实现可能性：
可以通过get.docker.com脚本安装的，但是无法执行，会出现cgroup错误。
根据[running-docker-on-android](https://stackoverflow.com/questions/19315168/running-docker-on-android)和[LXC 1.0: Some more advanced container usage [4/10]](https://stgraber.org/2013/12/23/lxc-1-0-some-more-advanced-container-usage/)中的表述，运行docker就是需要特定的内核特性支持的。
如果内核被裁剪，无法满足基本的运行需求，那么是没有方法的。

参考文档：
[](https://stackoverflow.com/questions/25817461/using-docker-on-android)

### 实现的特性要求：
如果要自己实现一个资源隔离的容器，应该从哪些方面下手呢？也许你第一反应可能就是chroot命令，这条命令给用户最直观的感觉就是使用后根目录/的挂载点切换了，即文件系统被隔离了。然后，为了在分布式的环境下进行通信和定位，容器必然需要一个独立的IP、端口、路由等等，自然就想到了网络的隔离。同时，你的容器还需要一个独立的主机名以便在网络中标识自己。想到网络，顺其自然就想到通信，也就想到了进程间通信的隔离。可能你也想到了权限的问题，对用户和用户组的隔离就实现了用户权限的隔离。最后，运行在容器中的应用需要有自己的PID,自然也需要与宿主机中的PID进行隔离。

总体上，容器技术使用了Cgroups、Namespaces、Union FS等一系列机制来保证隔离。

#### Namespace资源隔离：

参考文档：
[Docker背后的内核知识——Namespace资源隔离](http://www.infoq.com/cn/articles/docker-kernel-knowledge-namespace-resource-isolation?utm_source=infoq&utm_campaign=user_page&utm_medium=link)

#### cgroups资源限制：
cgroups是内核附加在程序上的一系列钩子（hooks），通过程序运行时对资源的调度触发相应的钩子以达到资源追踪和限制的目的。
这个和xposed框架是否有类似的地方？

参考文档：
[](http://www.infoq.com/cn/articles/docker-kernel-knowledge-cgroups-resource-isolation)

#### docker graph driver：



### 手动实现一个简易Docker：
熟悉了Docker的依赖特性，我们可以通过这些特性来仿造一个轮子，然后再进入后续的Docker原理来分析。

#### graphdriver:
容器其实就是一个特殊的进程。相比于普通进程，这个进程（容器）之所以特殊，
是因为它有自己独立的：
> - UTS(Unix Time-sharing System)：可以拥有独立的主机名和域名
> - PID：可以拥有独立的进程ID
> - NET: 可以拥有独立的网络设备
> - IPC：可以拥有独立的进程间通讯
> - NS：可以拥有独立的文件系统(rootfs)

从这个角度来看，docker的出现和之前的LXC和Warden有什么差异？都是基于内核提供的特性来做了上层的封装而已。
Docker最大的卖点在于类似git一样的镜像版本管理和发布，而这部分实现的基础在于graphdriver。


Docker在启动容器的时候，需要创建文件系统，为rootfs提供挂载点。Docker在内部通过graphdriver机制这种可扩展的方式来实现对不同文件系统的支持。参考：[Supported Filesystems](http://www.projectatomic.io/docs/filesystems/)：
```shell
A core part of the Docker model is the efficient use of layered images and containers based on images. To implement this Docker relies heavily on various filesystem features in the kernel. 
```
所以为了更有效率的使用分层的镜像结构，就一定和文件系统的实现密切结合。

Docker容器是建立在Aufs基础上的，Aufs是一种Union FS，简单来说就是支持将不同的目录挂载到同一个虚拟文件系统下，并实现一种layer的概念。Aufs将挂载到同一虚拟文件系统下的多个目录分别设置成read-only，read-write以及whiteout-able权限，对read-only目录只能读，而写操作只能实施在read-write目录中。重点在于，写操作是在read-only上的一种增量操作，不影响read-only目录。当挂载目录的时候要严格按照各目录之间的这种增量关系，将被增量操作的目录优先于在它基础上增量操作的目录挂载，待所有目录挂载结束了，继续挂载一个read-write目录，如此便形成了一种层次结构。

参考文档：
[深入理解docker graph driver – DeviceMapper](http://www.adocker.cn/archives/979)

#### namespace的调用：

#### cgroups的调用：


参考文档：
[动手创建一个容器](http://ybin.cc/linux/let-us-create-a-container/)
[自己动手写docker](https://github.com/xianlubird/mydocker)

## Docker的实现原理：
[Docker特性与原理解析](http://www.cnblogs.com/Bozh/p/3958469.html)


### 多层容器镜像存储结构：
因为docker镜像根据设定，是按照层次结构来进行组织的。在当前的aufs上，具体的结构为：
通过
参考文档：[](http://dockone.io/question/70)


## 容器性能调优：
容器的性能限制可以拆分为几个方面来考虑。

### 文件存储的性能调整：
因为Docker使用了AUFS管理image，通过docker的原理可以知道，容器的构造层次越多，容器的磁盘IO效率就越低。
最好的方式就是将已经完成的很多修改合并到rootfs中，然后再通过这个image来提供服务。


## 容器集群：
当需要服务越来越庞大，独立部署的容器也就越来越多，那么对这些容器的调度就是一个非常重要的问题。
容器集群有点类似于IAAS和PAAS的综合体。
目前github已经将所有的web服务切换到了容器。

Kubernetes是Google开源的容器集群管理系统，其提供应用部署、维护、 扩展机制等功能，利用Kubernetes能方便地管理跨机器运行容器化的应用，其主要功能如下：
> - 1) 使用Docker对应用程序包装(package)、实例化(instantiate)、运行(run)。
> - 2) 以集群的方式运行、管理跨机器的容器。
> - 3) 解决Docker跨机器容器之间的通讯问题。
> - 4) Kubernetes的自我修复机制使得容器集群总是运行在用户期望的状态。



