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

### 2 AUFS文件系统：
Docker容器是建立在Aufs基础上的，Aufs(advanced multi layered unification filesystem)是一种Union FS， 简单来说就是支持将不同的目录挂载到同一个虚拟文件系统下，并实现一种layer的概念。Aufs将挂载到同一虚拟文件系统下的多个目录分别设置成read-only，read-write以及whiteout-able权限，对read-only目录只能读，而写操作只能实施在read-write目录中。重点在于，写操作是在read-only上的一种增量操作，不影响read-only目录。当挂载目录的时候要严格按照各目录之间的这种增量关系，将被增量操作的目录优先于在它基础上增量操作的目录挂载，待所有目录挂载结束了，继续挂载一个read-write目录，如此便形成了一种层次结构。
直观的看看这个AUFS支持将不同的目录挂载到同一个虚拟文件系统下:
```shell
mount -t aufs -o br=/tmp/dir1=ro:/tmp/dir2=rw none /tmp/newfs
```



### 2 docker image存储结构
传统的Linux加载bootfs时会先将rootfs设为read-only，然后在系统自检之后将rootfs从read-only改为read-write，然后我们就可以在rootfs上进行写和读的操作了。但Docker的镜像却不是这样，它在bootfs自检完毕之后并不会把rootfs的read-only改为read-write。而是利用union mount（UnionFS的一种挂载机制）将一个或多个read-only的rootfs加载到之前的read-only的rootfs层之上。在加载了这么多层的rootfs之后，仍然让它看起来只像是一个文件系统，在Docker的体系里把union mount的这些read-only的rootfs叫做Docker的镜像。但是，此时的每一层rootfs都是read-only的，我们此时还不能对其进行操作。当我们创建一个容器，也就是将Docker镜像进行实例化，系统会在一层或是多层read-only的rootfs之上分配一层空的read-write的rootfs。




### 3 docker的容器存储结构
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

