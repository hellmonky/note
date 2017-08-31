<!-- TOC -->

- [Docker实现的基础](#docker实现的基础)
    - [docker使用回顾：](#docker使用回顾)
        - [基本环境搭建：](#基本环境搭建)
        - [hello-world：](#hello-world)
        - [基本的容器运行命令：](#基本的容器运行命令)
            - [创建并且运行一个后台容器：](#创建并且运行一个后台容器)
            - [进入正在运行的容器：](#进入正在运行的容器)
            - [退出容器而不关闭：](#退出容器而不关闭)
            - [创建而不运行容器：](#创建而不运行容器)
        - [通过Dockerfile生成基础镜像：](#通过dockerfile生成基础镜像)
        - [对运行中的docker镜像修改后保存：](#对运行中的docker镜像修改后保存)
        - [将当前的容器导出：](#将当前的容器导出)
        - [从tgz恢复为镜像：](#从tgz恢复为镜像)
        - [数据卷：](#数据卷)
            - [数据卷容器：](#数据卷容器)
                - [创建数据卷容器：](#创建数据卷容器)
                - [数据卷容器中的数据进行备份：](#数据卷容器中的数据进行备份)
                - [数据卷容器中的数据进行恢复：](#数据卷容器中的数据进行恢复)
                - [应用：](#应用)
                - [总结：](#总结)
    - [深入使用Docker：](#深入使用docker)
        - [application container 还是 machine container：](#application-container-还是-machine-container)
        - [存储的方式：](#存储的方式)
        - [共享kernel带来的风险：](#共享kernel带来的风险)
        - [Docker ulimit：](#docker-ulimit)
    - [Docker实现的Linux基础：](#docker实现的linux基础)
        - [在A上的实现可能性：](#在a上的实现可能性)
        - [实现的特性要求：](#实现的特性要求)
            - [Namespace资源隔离：](#namespace资源隔离)
            - [cgroups资源限制：](#cgroups资源限制)
            - [文件系统支持：](#文件系统支持)
    - [Docker的实现原理：](#docker的实现原理)
        - [多层容器镜像存储结构：](#多层容器镜像存储结构)
    - [容器性能调优：](#容器性能调优)
        - [文件存储的性能调整：](#文件存储的性能调整)
    - [容器集群：](#容器集群)
    - [Docker管理：Docker registry](#docker管理docker-registry)

<!-- /TOC -->

# Docker实现的基础
Docker的轻量化能够做到一些不需要全虚拟化方案来解决的问题，将整个资源的隔离性需求细分了，所以未来必然是多种层次虚拟化方式并存的。
轻量级的就是Docker，重量级的就是KVM。

## docker使用回顾：
回顾之前学习docker的基本概念和操作。

### 基本环境搭建：
现在有三种方式进行环境的搭建：源代码编译安装，rpm包安装，使用脚本安装。
最方便的就是使用脚本安装：
```shell
curl -sSL https://get.docker.com/ | sh
```
这个脚本会判断当前的linux发布版本，然后安装。

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

### 基本的容器运行命令：

#### 创建并且运行一个后台容器：
docker run -d -ti --name test busybox /bin/busybox sh
这个命令将会从远程拉取busybox的基础镜像，然后运行基于这个镜像的容器。
建议在运行docker中设置name，这样就不会自动生成name，方便后续的识别。

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


### 通过Dockerfile生成基础镜像：
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

### 对运行中的docker镜像修改后保存：
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


### 将当前的容器导出：
我们在一个容器上的很多snap会形成一个历史，如果需要将当前的完整历史信息保存，方便后续导入，我们可以通过：
```shell
sudo docker save -o /home/fengzheng/dockerImages/mmm.tar ubuntu:12.04
```
将这个容器保存下来。
需要注意的是，这个保存的内容就是

### 从tgz恢复为镜像：
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

#### 数据卷容器：
如果需要在多个容器间共享数据，并希望永久保存这些数据，最好的方式是使用数据卷容器，类似于一个提供网络文件共享服务的NFS服务器。

##### 创建数据卷容器：
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


##### 数据卷容器中的数据进行备份：
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

##### 数据卷容器中的数据进行恢复：


##### 应用：
作为数据卷的一个应用就是MySQL服务：

##### 总结：
Docker的数据卷挂载方式：
[Manage data in Docker](https://docs.docker.com/engine/admin/volumes/#more-details-about-mount-types)
总体上存在三种方式：
Volumes: 
Bind mounts:
tmpfs mounts: 


## 深入使用Docker：
在实际使用中更为深入的理解和学习docker。

###application container 还是 machine container：
问题出在使用：
systemctl status network.service
然后就会出现：
```shell
root@40bff9878c3e:/# systemctl status networking.service
Failed to connect to bus: No such file or directory
```
为什么会找不到？
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
docker run -d --privileged-true --name=contname image-name 
也就是：
docker run -ti --privileged=true --name dev_ubuntu ubuntu /sbin/init
这样结合/sbin/init启动参数，容器在启动时有权限启动systemd，然后常驻后台运行。
但是启动的终端会卡住，需要在其他终端查看：
docker ps
然后使用exec来进入这个容器：
docker exec -ti XXXX /bin/bash
进入，使用完毕之后exit退出即可，也不会终止容器。
如果需要明确的终止容器，需要使用：
docker stop XXXX

参考文档：
[Docker针对systemd启动的问题](http://wangyaohua.cn/wordpress/?p=611)
[Operating System Containers vs. Application Containers](https://blog.risingstack.com/operating-system-containers-vs-application-containers/)
[docker容器里安装ssh](http://6226001001.blog.51cto.com/9243584/1953310)

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

#### 文件系统支持：
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



## Docker的实现原理：


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



## Docker管理：Docker registry
http://dockone.io/article/747
v1已经被废弃，现在使用的都是v2，但是开发还在缓慢进行中：
https://github.com/docker/distribution