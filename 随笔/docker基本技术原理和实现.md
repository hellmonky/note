#docker基本技术原理和实现

学习笔记，来源广泛 hellmonky

## 第一节 docker的基本概念
### 1 docker的两个基础技术：namespace和cgroup。
（1）cgroup主要作资源的限制隔离，它可以限制一组进程中能使用的最大资源使用量，相对比较好理解；
（2）namespace同样可以实现资源隔离，不同的是它是通过使PID,IPC,Network等系统资源不再是全局性的，而是属于特定的Namespace实现的。每个Namespace里面的资源对其他Namespace都是透明的，这个概念有点类似于linux的多用户机制。

### 2 Docker是CS架构，主要由下面三部分组成：
（1）Docker daemon: 运行在宿主机上，Docker守护进程，用户通过Docker client(Docker命令)与Docker daemon交互；
（2）Docker client: Docker 命令行工具，是用户使用Docker的主要方式，Docker client与Docker daemon通信并将结果返回给用户，Docker client也可以通过socket或者RESTful api访问远程的Docker daemon；
（3）Docker hub/registry: 共享和管理Docker镜像，用户可以上传或者下载上面的镜像，[官方地址](https://registry.hub.docker.com/)，也可以搭建自己私有的Docker registry。

也就是说通常使用的docker命令属于DockerClient部分；docker.service属于DockerDaemon部分来完成主要的执行状况；DockerHub类似于github一样，对docker镜像进行托管。

### 3 Docker的两个主要概念：
（1）Docker image：
镜像是只读的，镜像中包含有需要运行的文件。镜像用来创建container，一个镜像可以运行多个container；镜像可以通过Dockerfile创建，也可以从Docker hub/registry上下载。
（2）Docker container：
容器是Docker的运行组件，启动一个镜像就是一个容器，容器是一个隔离环境，多个容器之间不会相互影响，保证容器中的程序运行在一个相对安全的环境中。

## 第二节 docker的文件系统
Docker 的很多特性都表现在它所使用的文件系统上，比如大家都知道docker的文件系统是分层的，所以它可以快速迭代，可以回滚。并且新用户第一次接触docker也是从镜像和容器开始的，所以我们先看看docker的文件系统原理。

### 1 linux文件系统
典型的Linux文件系统由bootfs和rootfs两部分组成，bootfs(boot file system)主要包含 bootloader和kernel，bootloader主要是引导加载kernel，当kernel被加载到内存中后 bootfs就被umount了。 rootfs (root file system) 包含的就是典型 Linux 系统中的/dev，/proc，/bin，/etc等标准目录和文件。

### 2 AUFS文件系统初识：
Docker容器是建立在Aufs基础上的，最早的docker也只支持Aufs文件系统，后来因为扩展的需求，重构了代码来支持不同的文件系统。作为经典的实现过程，有必要理解一下Aufs文件系统。
Aufs(advanced multi layered unification filesystem)是一种Union FS， 简单来说就是支持将不同的目录挂载到同一个虚拟文件系统下，并实现一种layer的概念。Aufs将挂载到同一虚拟文件系统下的多个目录分别设置成read-only，read-write以及whiteout-able权限，对read-only目录只能读，而写操作只能实施在read-write目录中。重点在于，写操作是在read-only上的一种增量操作，不影响read-only目录。当挂载目录的时候要严格按照各目录之间的这种增量关系，将被增量操作的目录优先于在它基础上增量操作的目录挂载，待所有目录挂载结束了，继续挂载一个read-write目录，如此便形成了一种层次结构。
直观的看看这个AUFS支持将不同的目录挂载到同一个虚拟文件系统下:
```shell
mount -t aufs -o br=/tmp/dir1=ro:/tmp/dir2=rw none /tmp/newfs
```
其中参数：
```shell
-o 指定mount传递给文件系统的参数
br 指定需要挂载的文件夹，这里包括dir1和dir2
ro/rw 指定文件的权限只读和可读写
none 这里没有设备，用none表示
```
这个结果是什么样子的呢。 就是把/tmp/dir1 t和/tmp/dir2  合并之后挂载到/tmp/newfs ，如果这时在/tmp/dir1 下创建一个文件a  /tmp/dir2下创建一个文件b 则  在/tmp/newfs 会看到a,b 这两个文件，并且a 是只读的， 如果有相同的文件则以先挂载的为准，后面挂载的操作会被忽略掉。

### Device Mapper：
Device mapper 是 Linux 2.6 内核中提供的一种从逻辑设备到物理设备的映射框架机制，在该机制下，用户可以很方便的根据自己的需要制定实现存储资源的管理策略，可以在[这儿](http://www.ibm.com/developerworks/cn/linux/l-devmapper/index.html) 查看详细介绍。
Device mapper driver 会创建一个100G的简单文件包含你的镜像和容器。每一个容器被限制在10G大小的卷内。（如果想要调整，参考:http://jpetazzo.github.io/2014/01/29/docker-device-mapper-resize/ 。中文译文: http://zhumeng8337797.blog.163.com/blog/static/100768914201452405120107/ ）
你可以在启动docker daemon时用参数-s 指定driver:  docker -d -s devicemapper ;


### 3 docker image tarball结构分析：
docker可以将当前的镜像导出为tar包进行备份和传输，tar包中的内容基本是和。
这儿用centos7官方的docker镜像来作为分析对象：
首先查看当前生成的镜像历史：
再使用：
```shell
docker history centos7_base
```
查看结果为：
```shell
IMAGE               CREATED             CREATED BY                                      SIZE                COMMENT
ded5d38233c0        16 minutes ago      /bin/sh -c #(nop) CMD ["/bin/bash"]             0 B                 
2eea88e40ecf        16 minutes ago      /bin/sh -c #(nop) LABEL name=CentOS Base Imag   0 B                 
4e8f709f2cf1        16 minutes ago      /bin/sh -c #(nop) ADD file:892891d20280e59b45   196.8 MB            
329afbcdc6b4        16 minutes ago      /bin/sh -c #(nop) MAINTAINER https://github.c   0 B   
```
然后将这个镜像导出为tar包，然后解压查看：
```shell
mkdir /home/wentao/docker/imageAnalysis
cd /home/wentao/docker/centos7_base/
tar xvf centos7_base.tar -C /home/wentao/docker/imageAnalysis/
tree .
```
查看当前的tar包结果为：
```shell
.
├── 1dc8088f1c85a445fa66de54f3d7a2352cf811db392415a610573c3a2f14b9e5
│   ├── json
│   ├── layer.tar
│   └── VERSION
├── ded5d38233c09a3628cc803071d60d1c449416d60986c6ead4d11b4a22c132a2.json
├── manifest.json
└── repositories
```
看到有一个目录和三个与这个目录平级的文件。查看de开头的顶层json文件内容为（格式化之后）：
```shell
{
    "architecture": "amd64", 
    "author": "https://github.com/CentOS/sig-cloud-instance-images", 
    "config": {
        "Hostname": "dac7902ade73", 
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
        "Image": "sha256:2eea88e40ecf0c1aa5abc6e9e63d112cbfdd7ba779d9317a6f08eb221b6e64fb", 
        "Volumes": null, 
        "WorkingDir": "", 
        "Entrypoint": null, 
        "OnBuild": null, 
        "Labels": {
            "build-date": "20160701", 
            "license": "GPLv2", 
            "name": "CentOS Base Image", 
            "vendor": "CentOS"
        }
    }, 
    "container": "c76ac64e706f2941562383b40b4de773db905cf3f920ff7c60407e24d60d424b", 
    "container_config": {
        "Hostname": "dac7902ade73", 
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
            "/bin/sh", 
            "-c", 
            "#(nop) CMD [\"/bin/bash\"]"
        ], 
        "Image": "sha256:2eea88e40ecf0c1aa5abc6e9e63d112cbfdd7ba779d9317a6f08eb221b6e64fb", 
        "Volumes": null, 
        "WorkingDir": "", 
        "Entrypoint": null, 
        "OnBuild": null, 
        "Labels": {
            "build-date": "20160701", 
            "license": "GPLv2", 
            "name": "CentOS Base Image", 
            "vendor": "CentOS"
        }
    }, 
    "created": "2016-07-20T16:59:17.855636307Z", 
    "docker_version": "1.11.2", 
    "history": [
        {
            "created": "2016-07-20T16:59:02.526315828Z", 
            "author": "https://github.com/CentOS/sig-cloud-instance-images", 
            "created_by": "/bin/sh -c #(nop) MAINTAINER https://github.com/CentOS/sig-cloud-instance-images", 
            "empty_layer": true
        }, 
        {
            "created": "2016-07-20T16:59:15.208340206Z", 
            "author": "https://github.com/CentOS/sig-cloud-instance-images", 
            "created_by": "/bin/sh -c #(nop) ADD file:892891d20280e59b4522cfb77e3c30ef777e2ddaf1635ed925bd91ad4ba06ade in /"
        }, 
        {
            "created": "2016-07-20T16:59:16.428144924Z", 
            "author": "https://github.com/CentOS/sig-cloud-instance-images", 
            "created_by": "/bin/sh -c #(nop) LABEL name=CentOS Base Image vendor=CentOS license=GPLv2 build-date=20160701", 
            "empty_layer": true
        }, 
        {
            "created": "2016-07-20T16:59:17.855636307Z", 
            "author": "https://github.com/CentOS/sig-cloud-instance-images", 
            "created_by": "/bin/sh -c #(nop) CMD [\"/bin/bash\"]", 
            "empty_layer": true
        }
    ], 
    "os": "linux", 
    "rootfs": {
        "type": "layers", 
        "diff_ids": [
            "sha256:02f2e4ddf954c6d094ce78049af091017c5ba2cdd1fac7acfd9fafd6f7803d7e"
        ]
    }
}
```
这个文件中记录了当前dockerImage的总体内容，并且其中的history段记录了生成过程。
查看manifest.json文件内容为（格式化）：
```shell
[
    {
        "Config": "ded5d38233c09a3628cc803071d60d1c449416d60986c6ead4d11b4a22c132a2.json", 
        "RepoTags": [
            "centos7_base:latest"
        ], 
        "Layers": [
            "1dc8088f1c85a445fa66de54f3d7a2352cf811db392415a610573c3a2f14b9e5/layer.tar"
        ]
    }
]
```
查看repositories内容为：
```shell
{"centos7_base":{"latest":"1dc8088f1c85a445fa66de54f3d7a2352cf811db392415a610573c3a2f14b9e5"}}
```
这个文件记录了当前的docker镜像的最上层。

最上层的id和tar包中唯一的json文件的id是相同，并且history展示了当前这个镜像生成的过程文件，查看history知道只有第二步的ADD发生了文件的改变，其他操作都没有新增和删除文件，所以对应的size为0。
进入唯一的文件夹中，查看json文件内容（格式化之后）：
```shell
{
    "id": "1dc8088f1c85a445fa66de54f3d7a2352cf811db392415a610573c3a2f14b9e5", 
    "created": "2016-07-20T16:59:17.855636307Z", 
    "container": "c76ac64e706f2941562383b40b4de773db905cf3f920ff7c60407e24d60d424b", 
    "container_config": {
        "Hostname": "dac7902ade73", 
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
            "/bin/sh", 
            "-c", 
            "#(nop) CMD [\"/bin/bash\"]"
        ], 
        "Image": "sha256:2eea88e40ecf0c1aa5abc6e9e63d112cbfdd7ba779d9317a6f08eb221b6e64fb", 
        "Volumes": null, 
        "WorkingDir": "", 
        "Entrypoint": null, 
        "OnBuild": null, 
        "Labels": {
            "build-date": "20160701", 
            "license": "GPLv2", 
            "name": "CentOS Base Image", 
            "vendor": "CentOS"
        }
    }, 
    "docker_version": "1.11.2", 
    "author": "https://github.com/CentOS/sig-cloud-instance-images", 
    "config": {
        "Hostname": "dac7902ade73", 
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
        "Image": "sha256:2eea88e40ecf0c1aa5abc6e9e63d112cbfdd7ba779d9317a6f08eb221b6e64fb", 
        "Volumes": null, 
        "WorkingDir": "", 
        "Entrypoint": null, 
        "OnBuild": null, 
        "Labels": {
            "build-date": "20160701", 
            "license": "GPLv2", 
            "name": "CentOS Base Image", 
            "vendor": "CentOS"
        }
    }, 
    "architecture": "amd64", 
    "os": "linux"
}
```

继续看看tar包中唯一的文件夹中的layer.tar的内容：
```shell
mkdir layer
tar -xvf layer.tar -C layer
cd layer
tree -L 1
```
结果为：
```shell
[root@localhost 1dc8088f1c85a445fa66de54f3d7a2352cf811db392415a610573c3a2f14b9e5]# tree -L 1 layer
layer
├── anaconda-post.log
├── bin -> usr/bin
├── etc
├── home
├── lib -> usr/lib
├── lib64 -> usr/lib64
├── lost+found
├── media
├── mnt
├── opt
├── root
├── run
├── sbin -> usr/sbin
├── srv
├── tmp
├── usr
└── var

16 directories, 1 file
```
内容基本就是生成centos7_base镜像的boot.iso文件的内容，也就是rootfs。结合生成这个dockerImage的Dockerfile内容：
```shell
FROM scratch
MAINTAINER https://github.com/CentOS/sig-cloud-instance-images
ADD centos-7-docker.tar.xz /

LABEL name="CentOS Base Image" \
    vendor="CentOS" \
    license="GPLv2" \
    build-date="20160701"

CMD ["/bin/bash"]
```
可以看出对于文件系统的修改主要体现在ADD命令所添加的文件上，也就是整个镜像的基础文件系统内容的添加。修改这个Dockerfile文件，增加一个安装脚本：
```shell
FROM scratch
MAINTAINER https://github.com/CentOS/sig-cloud-instance-images
ADD centos-7-docker.tar.xz /

LABEL name="CentOS Base Image" \
    vendor="CentOS" \
    license="GPLv2" \
    build-date="20160701"

RUN yum -y install nano

CMD ["/bin/bash"]
```
继续按照上述方式分析。解压从这个image生成的tar包：
```shell
[root@localhost 2imageAnalysis]# ll
总用量 12
drwxr-xr-x. 2 root root   47 7月  21 07:10 034d3aafe3adf0c7e4a48693bc69034792274e824bcb7f5c5ca82b510697df56
drwxr-xr-x. 2 root root   47 7月  21 07:10 742434c0e47091563023a7702c7e4901ad04f9fc9bd3b200bb4f9ede7e3aea80
-rw-r--r--. 1 root root 2566 7月  21 07:10 83749061359cf0a1a356a8d2080a97921597b66f73a2a8bdd164cdd5f8bc05c1.json
-rw-r--r--. 1 root root  279 1月   1 1970 manifest.json
-rw-r--r--. 1 root root   89 1月   1 1970 repositories
```
发现多了一个文件，按照层级结构展开看：
```shell
[root@localhost 2imageAnalysis]# tree .
.
├── 034d3aafe3adf0c7e4a48693bc69034792274e824bcb7f5c5ca82b510697df56
│   ├── json
│   ├── layer.tar
│   └── VERSION
├── 742434c0e47091563023a7702c7e4901ad04f9fc9bd3b200bb4f9ede7e3aea80
│   ├── json
│   ├── layer.tar
│   └── VERSION
├── 83749061359cf0a1a356a8d2080a97921597b66f73a2a8bdd164cdd5f8bc05c1.json
├── manifest.json
└── repositories

2 directories, 9 files
```
比较之前的多了一个文件夹，继续查看顶层的.json文件内容：
```shell
{
    "architecture": "amd64", 
    "author": "https://github.com/CentOS/sig-cloud-instance-images", 
    "config": {
        "Hostname": "37ea39904090", 
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
        "ArgsEscaped": true, 
        "Image": "sha256:cdb8f5b67c5c3aa647cf354e8ecdd0e9806e91dbfccedecbe0d6d96e2b687671", 
        "Volumes": null, 
        "WorkingDir": "", 
        "Entrypoint": null, 
        "OnBuild": null, 
        "Labels": {
            "build-date": "20160701", 
            "license": "GPLv2", 
            "name": "CentOS Base Image", 
            "vendor": "CentOS"
        }
    }, 
    "container": "8e5c9fa558fbfe039cfef7604812351ae6680530d5bb821d46aa2d7428d48a62", 
    "container_config": {
        "Hostname": "37ea39904090", 
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
            "/bin/sh", 
            "-c", 
            "#(nop) CMD [\"/bin/bash\"]"
        ], 
        "ArgsEscaped": true, 
        "Image": "sha256:cdb8f5b67c5c3aa647cf354e8ecdd0e9806e91dbfccedecbe0d6d96e2b687671", 
        "Volumes": null, 
        "WorkingDir": "", 
        "Entrypoint": null, 
        "OnBuild": null, 
        "Labels": {
            "build-date": "20160701", 
            "license": "GPLv2", 
            "name": "CentOS Base Image", 
            "vendor": "CentOS"
        }
    }, 
    "created": "2016-07-20T23:10:05.42782255Z", 
    "docker_version": "1.11.2", 
    "history": [
        {
            "created": "2016-07-20T16:59:02.526315828Z", 
            "author": "https://github.com/CentOS/sig-cloud-instance-images", 
            "created_by": "/bin/sh -c #(nop) MAINTAINER https://github.com/CentOS/sig-cloud-instance-images", 
            "empty_layer": true
        }, 
        {
            "created": "2016-07-20T16:59:15.208340206Z", 
            "author": "https://github.com/CentOS/sig-cloud-instance-images", 
            "created_by": "/bin/sh -c #(nop) ADD file:892891d20280e59b4522cfb77e3c30ef777e2ddaf1635ed925bd91ad4ba06ade in /"
        }, 
        {
            "created": "2016-07-20T16:59:16.428144924Z", 
            "author": "https://github.com/CentOS/sig-cloud-instance-images", 
            "created_by": "/bin/sh -c #(nop) LABEL name=CentOS Base Image vendor=CentOS license=GPLv2 build-date=20160701", 
            "empty_layer": true
        }, 
        {
            "created": "2016-07-20T23:10:03.941170325Z", 
            "author": "https://github.com/CentOS/sig-cloud-instance-images", 
            "created_by": "/bin/sh -c yum -y install nano"
        }, 
        {
            "created": "2016-07-20T23:10:05.42782255Z", 
            "author": "https://github.com/CentOS/sig-cloud-instance-images", 
            "created_by": "/bin/sh -c #(nop) CMD [\"/bin/bash\"]", 
            "empty_layer": true
        }
    ], 
    "os": "linux", 
    "rootfs": {
        "type": "layers", 
        "diff_ids": [
            "sha256:02f2e4ddf954c6d094ce78049af091017c5ba2cdd1fac7acfd9fafd6f7803d7e", 
            "sha256:838487ddacc1b514dd54da1c70a9ae693e470836c0d7651f17a64a0e899e45fc"
        ]
    }
}
```
和上面的比较，差别在于history中多了一个记录，并且这个记录对应的层是非空的。顺便看看manifest.json的内容为：
```shell
[
    {
        "Config": "83749061359cf0a1a356a8d2080a97921597b66f73a2a8bdd164cdd5f8bc05c1.json", 
        "RepoTags": [
            "centos:latest"
        ], 
        "Layers": [
            "742434c0e47091563023a7702c7e4901ad04f9fc9bd3b200bb4f9ede7e3aea80/layer.tar", 
            "034d3aafe3adf0c7e4a48693bc69034792274e824bcb7f5c5ca82b510697df56/layer.tar"
        ]
    }
]
```
明显的多出来一个layer的描述。在看看repositories：
```shell
{"centos":{"latest":"034d3aafe3adf0c7e4a48693bc69034792274e824bcb7f5c5ca82b510697df56"}}
```
可见，repositories中只记录了一层内容。现在进入两个文件夹来查看具体的内容进行分析。
将两个文件夹中的tar包解压到各自路径下，然后进行对比。7开头的文件夹中tar包解压后的内容为：
```shell
[root@localhost 742434c0e47091563023a7702c7e4901ad04f9fc9bd3b200bb4f9ede7e3aea80]# tree -L 1 layer
layer
├── anaconda-post.log
├── bin -> usr/bin
├── etc
├── home
├── lib -> usr/lib
├── lib64 -> usr/lib64
├── lost+found
├── media
├── mnt
├── opt
├── root
├── run
├── sbin -> usr/sbin
├── srv
├── tmp
├── usr
└── var

16 directories, 1 file
```
0开头的文件夹中的tar包解压后的内容为：
```shell
[root@localhost 034d3aafe3adf0c7e4a48693bc69034792274e824bcb7f5c5ca82b510697df56]# tree -L 1 layer
layer
├── etc
├── usr
└── var

3 directories, 0 files
```
更为详细的内容为：
```shell
[root@localhost 034d3aafe3adf0c7e4a48693bc69034792274e824bcb7f5c5ca82b510697df56]# tree layer
layer
├── etc
│   └── nanorc
├── usr
│   ├── bin
│   │   ├── nano
│   │   └── rnano -> nano
│   └── share
│       └── nano
│           ├── asm.nanorc
│           ├── awk.nanorc
│           ├── cmake.nanorc
│           ├── c.nanorc
│           ├── css.nanorc
│           ├── debian.nanorc
│           ├── fortran.nanorc
│           ├── gentoo.nanorc
│           ├── groff.nanorc
│           ├── html.nanorc
│           ├── java.nanorc
│           ├── lua.nanorc
│           ├── makefile.nanorc
│           ├── man-html
│           │   ├── fr
│           │   │   ├── nano.1.html
│           │   │   ├── nanorc.5.html
│           │   │   └── rnano.1.html
│           │   ├── nano.1.html
│           │   ├── nanorc.5.html
│           │   └── rnano.1.html
│           ├── man.nanorc
│           ├── mgp.nanorc
│           ├── mutt.nanorc
│           ├── nanorc.nanorc
│           ├── objc.nanorc
│           ├── ocaml.nanorc
│           ├── patch.nanorc
│           ├── perl.nanorc
│           ├── php.nanorc
│           ├── pov.nanorc
│           ├── python.nanorc
│           ├── ruby.nanorc
│           ├── sh.nanorc
│           ├── spec.nanorc
│           ├── tcl.nanorc
│           ├── tex.nanorc
│           └── xml.nanorc
└── var
    ├── cache
    │   └── yum
    │       └── x86_64
    │           └── 7
    │               ├── base
    │               │   ├── 436345f4b666f0a461d479ccfabc2c22823d4f2173c2653e5250fea62f0afe98-c7-x86_64-comps.xml.gz
    │               │   ├── c6411f1cc8a000ed2b651b49134631d279abba1ec1f78e5dcca79a52d8c1eada-primary.sqlite.bz2
    │               │   ├── cachecookie
    │               │   ├── gen
    │               │   │   └── primary_db.sqlite
    │               │   ├── mirrorlist.txt
    │               │   ├── packages
    │               │   └── repomd.xml
    │               ├── extras
    │               │   ├── 8bbaece36ec6587426c9685a366f242c6a976e781db53be42c50bff51147cf39-primary.sqlite.bz2
    │               │   ├── cachecookie
    │               │   ├── gen
    │               │   │   └── primary_db.sqlite
    │               │   ├── mirrorlist.txt
    │               │   ├── packages
    │               │   └── repomd.xml
    │               ├── timedhosts
    │               ├── timedhosts.txt
    │               └── updates
    │                   ├── ad3c2808ae940c9bb62494c2ff9f0ac9fb1dd8a5380b9d056a3587ac04e17da1-primary.sqlite.bz2
    │                   ├── cachecookie
    │                   ├── gen
    │                   │   └── primary_db.sqlite
    │                   ├── mirrorlist.txt
    │                   ├── packages
    │                   └── repomd.xml
    ├── lib
    │   ├── rpm
    │   │   ├── Basenames
    │   │   ├── Conflictname
    │   │   ├── __db.001
    │   │   ├── __db.002
    │   │   ├── __db.003
    │   │   ├── Dirnames
    │   │   ├── Group
    │   │   ├── Installtid
    │   │   ├── Name
    │   │   ├── Packages
    │   │   ├── Providename
    │   │   ├── Requirename
    │   │   ├── Sha1header
    │   │   └── Sigmd5
    │   └── yum
    │       ├── history
    │       │   ├── 2016-07-01
    │       │   │   └── 3
    │       │   │       ├── config-main
    │       │   │       ├── config-repos
    │       │   │       └── saved_tx
    │       │   ├── history-2016-07-01.sqlite
    │       │   └── history-2016-07-01.sqlite-journal
    │       ├── rpmdb-indexes
    │       │   ├── conflicts
    │       │   ├── file-requires
    │       │   ├── obsoletes
    │       │   ├── pkgtups-checksums
    │       │   └── version
    │       └── yumdb
    │           └── n
    │               └── b4a8577b1e26738e571f0796bf4c1ddb726382dd-nano-2.3.1-10.el7-x86_64
    │                   ├── checksum_data
    │                   ├── checksum_type
    │                   ├── command_line
    │                   ├── from_repo
    │                   ├── from_repo_revision
    │                   ├── from_repo_timestamp
    │                   ├── installed_by
    │                   ├── origin_url
    │                   ├── reason
    │                   ├── releasever
    │                   ├── tsflag_nodocs
    │                   ├── ts_install_langs
    │                   ├── var_infra
    │                   └── var_uuid
    └── log
        └── yum.log

32 directories, 96 files
```
查看完了每一层tar包内容，那么怎么知道这两个层之间的依赖关系？分别进入0开头的文件夹，查看json文件内容：
```shell
[root@localhost 034d3aafe3adf0c7e4a48693bc69034792274e824bcb7f5c5ca82b510697df56]# cat json
{
    "id": "034d3aafe3adf0c7e4a48693bc69034792274e824bcb7f5c5ca82b510697df56", 
    "parent": "742434c0e47091563023a7702c7e4901ad04f9fc9bd3b200bb4f9ede7e3aea80", 
    "created": "2016-07-20T23:10:05.42782255Z", 
    "container": "8e5c9fa558fbfe039cfef7604812351ae6680530d5bb821d46aa2d7428d48a62", 
    "container_config": {
        "Hostname": "37ea39904090", 
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
            "/bin/sh", 
            "-c", 
            "#(nop) CMD [\"/bin/bash\"]"
        ], 
        "ArgsEscaped": true, 
        "Image": "sha256:cdb8f5b67c5c3aa647cf354e8ecdd0e9806e91dbfccedecbe0d6d96e2b687671", 
        "Volumes": null, 
        "WorkingDir": "", 
        "Entrypoint": null, 
        "OnBuild": null, 
        "Labels": {
            "build-date": "20160701", 
            "license": "GPLv2", 
            "name": "CentOS Base Image", 
            "vendor": "CentOS"
        }
    }, 
    "docker_version": "1.11.2", 
    "author": "https://github.com/CentOS/sig-cloud-instance-images", 
    "config": {
        "Hostname": "37ea39904090", 
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
        "ArgsEscaped": true, 
        "Image": "sha256:cdb8f5b67c5c3aa647cf354e8ecdd0e9806e91dbfccedecbe0d6d96e2b687671", 
        "Volumes": null, 
        "WorkingDir": "", 
        "Entrypoint": null, 
        "OnBuild": null, 
        "Labels": {
            "build-date": "20160701", 
            "license": "GPLv2", 
            "name": "CentOS Base Image", 
            "vendor": "CentOS"
        }
    }, 
    "architecture": "amd64", 
    "os": "linux"
}
```
7开头的文件夹，查看json内容：
```shell
{
    "id": "742434c0e47091563023a7702c7e4901ad04f9fc9bd3b200bb4f9ede7e3aea80", 
    "created": "0001-01-01T00:00:00Z", 
    "container_config": {
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
    }
}
```
对比这两个json文件就可以发现0开头的json中指明了字段：
```shell
"parent": "742434c0e47091563023a7702c7e4901ad04f9fc9bd3b200bb4f9ede7e3aea80"
```
来说明他的父层为7开头的文件夹。而7开头的json中没有指定任何parent，也就是说它就是base层，整个镜像从这个层次开始。正是json中的这个属性记录了继承结构，从而实现了layer的层次关系。每个文件夹下都有一个layer.tar的文件，这个压缩文件存放的正是rootfs的内容，不过是增量存放的而已。
对比可见，0开头的文件夹中的tar包只包含了安装nano的时候发生改变的文件夹和内容。整个镜像的加载是按照层次顺序进行的，后面的层中的文件系统依赖于之前所有层次的综合出来的文件系统的内容，这样构成了整个docker镜像的组成关系。
对比两个从同一个Dockerfile微小修改中生成从docker的镜像中生成的tar包，然后分析其结构，发现其中包含的不同文件夹表示了不同的层的内容，并且不同的层之间属于继承关系，进行之前镜像上的增加和删除。
```shell
An image tarball contains one directory per image layer (named using its long ID), each containing these files:

VERSION: currently 1.0 - the file format version
json: detailed layer information, similar to docker inspect layer_id
layer.tar: A tarfile containing the filesystem changes in this layer
```
来自官方的[image tarball format](https://docs.docker.com/engine/reference/api/docker_remote_api_v1.20/#image-tarball-format)解释。

### docker的存储模型
在Docker存储模型中，bootfs同宿主机，rootfs则是由多个镜像层和一个容器层构成，其中镜像层只读，容器层可读写，多个镜像层之间有父子关系，下层作为上层镜像的父镜像，最下面的镜像称为base images(基础镜像)。
上一节中描述了文件系统无关的镜像的save导出包tar的结构。这一节结合文件系统的相关内容进行分析。
在被广泛支持的文件系统下，docker的默认安装路径为：/var/lib/docker，在没有导入镜像的时候检查这个文件夹的所有内容：
```shell
[root@localhost docker]# tree .
.
├── containers
├── devicemapper
│   ├── devicemapper
│   │   ├── data
│   │   └── metadata
│   ├── metadata
│   │   ├── base
│   │   ├── deviceset-metadata
│   │   └── transaction-metadata
│   └── mnt
├── image
│   └── devicemapper
│       ├── distribution
│       ├── imagedb
│       │   ├── content
│       │   │   └── sha256
│       │   └── metadata
│       │       └── sha256
│       ├── layerdb
│       │   ├── mounts
│       │   ├── sha256
│       │   └── tmp
│       └── repositories.json
├── network
│   └── files
│       ├── ce60d5bd15b5fba588e93e7ea9b3b6f50010810b67229ba45edf7b8e0339b8ab.sock
│       └── local-kv.db
├── tmp
├── trust
└── volumes
    └── metadata.db

22 directories, 9 files
```
然后使用上面的Dockerfile脚本来生成镜像：
```shell

```


### 2 docker image存储结构
传统的Linux加载bootfs时会先将rootfs设为read-only，然后在系统自检之后将rootfs从read-only改为read-write，然后我们就可以在rootfs上进行写和读的操作了。但Docker的镜像却不是这样，它在bootfs自检完毕之后并不会把rootfs的read-only改为read-write。而是利用union mount（UnionFS的一种挂载机制）将一个或多个read-only的rootfs加载到之前的read-only的rootfs层之上。在加载了这么多层的rootfs之后，仍然让它看起来只像是一个文件系统，在Docker的体系里把union mount的这些read-only的rootfs叫做Docker的镜像。但是，此时的每一层rootfs都是read-only的，我们此时还不能对其进行操作。当我们创建一个容器，也就是将Docker镜像进行实例化，系统会在一层或是多层read-only的rootfs之上分配一层空的read-write的rootfs。

### 4 docker的容器存储结构
在初学docker的时候不理解所谓的镜像和容器，不知道怎么生成，怎么存储，怎么使用。和这些相关的就是容器和镜像的具体承载方式了。所以对这两个概念一定要有非常深入的认识才能掌握docker的核心。并且这两个概念需要和具体的文件系统结合来分析。
上一节中主要讨论了和文件系统基本无关的导出docker的镜像结构，这一节主要分析docker的容器，也就是container。
Docker文件系统因为从原来ubuntu上的Aufs没有能够加入到linux内核中给，为了更广泛的应用，docker团队将这部分作为后端可插拔的组建重购了docker的文件驱动部分。Docker在内部通过graphdriver机制这种可扩展的方式来实现对不同文件系统的支持。参考：[device-mapper-driver](https://docs.docker.com/engine/userguide/storagedriver/device-mapper-driver/)有更为详细的描述。
docker的镜像与容器都存储在 /var/lib/docker 下面，那么基于不同的系统又有不同的存储方式，在 ubuntu下面存储方式为AUFS；在Centos下面存储方式又是device mapper。我们以contos7_x64为例。
在当前docker启动没有安装任何镜像的时候，切换到/var/lib/docker目录下使用tree ./查看当前路径的内容：
```shell
[root@localhost docker]# tree ./
./
├── containers
├── devicemapper
│   ├── devicemapper
│   │   ├── data
│   │   └── metadata
│   └── metadata
│       ├── base
│       ├── deviceset-metadata
│       └── transaction-metadata
├── image
│   └── devicemapper
│       ├── distribution
│       ├── imagedb
│       │   ├── content
│       │   │   └── sha256
│       │   └── metadata
│       │       └── sha256
│       ├── layerdb
│       └── repositories.json
├── network
│   └── files
│       ├── 57e07176c24b76aac8161da78c652114df6f1b309a5dd4ec4d22ecea61e4c1d1.sock
│       ├── 98a99c576b31989805309cd45883b3fafd98405e2a3e0bc037620a3898b833fb.sock
│       └── local-kv.db
├── tmp
├── trust
└── volumes
    └── metadata.db

18 directories, 10 files
```
然后看看各自文件夹的默认大小：
```shell
[root@localhost docker]# tree -h ./
./
├── [   6]  containers
├── [  40]  devicemapper
│   ├── [  32]  devicemapper
│   │   ├── [100G]  data
│   │   └── [2.0G]  metadata
│   └── [  69]  metadata
│       ├── [  88]  base
│       ├── [ 105]  deviceset-metadata
│       └── [  56]  transaction-metadata
├── [  25]  image
│   └── [  77]  devicemapper
│       ├── [   6]  distribution
│       ├── [  35]  imagedb
│       │   ├── [  19]  content
│       │   │   └── [   6]  sha256
│       │   └── [  19]  metadata
│       │       └── [   6]  sha256
│       ├── [   6]  layerdb
│       └── [  19]  repositories.json
├── [  18]  network
│   └── [4.0K]  files
│       ├── [   0]  57e07176c24b76aac8161da78c652114df6f1b309a5dd4ec4d22ecea61e4c1d1.sock
│       ├── [   0]  98a99c576b31989805309cd45883b3fafd98405e2a3e0bc037620a3898b833fb.sock
│       └── [ 64K]  local-kv.db
├── [   6]  tmp
├── [   6]  trust
└── [  24]  volumes
    └── [ 32K]  metadata.db

```
可以看出centos下面docker使用devicemapper的存储方式，所以在/var/lib/docker下面出现了devicemapper目录。其中/image/devicemapper/devicemapper/目录下有两个文件：
```shell
devicemapper
│   │   ├── data
│   │   └── metadata
```
用来存储对应的存储池和相关的元数据。
/image/devicemapper/metadata/目录下有三个文件：
```shell
metadata
│       ├── base
│       ├── deviceset-metadata
│       └── transaction-metadata
```
用来存放前面元数据的ID，大小以及UUID等信息。

