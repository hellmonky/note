#docker基本技术原理和实现

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

## 第二节 docker的实现原理

### 1 docker的文件驱动

在初学docker的时候不理解所谓的镜像和容器，不知道怎么生成，怎么存储，怎么使用。和这些相关的就是容器和镜像的具体承载方式了。所以对这两个概念一定要有非常深入的认识才能掌握docker的核心。并且这两个概念需要和具体的文件系统结合来分析。
Docker文件系统因为从原来ubuntu上的Aufs没有能够加入到linux内核中给，为了更广泛的应用，docker团队将这部分作为后端可插拔的组建重购了docker的文件驱动部分。Docker在内部通过graphdriver机制这种可扩展的方式来实现对不同文件系统的支持。[device-mapper-driver](https://docs.docker.com/engine/userguide/storagedriver/device-mapper-driver/)。
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

