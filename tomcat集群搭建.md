<!-- TOC -->

- [搭建tomcat集群](#搭建tomcat集群)
    - [集群搭建的准备：](#集群搭建的准备)
        - [相关软件的编译安装：](#相关软件的编译安装)
            - [apache的安装：](#apache的安装)
            - [Apache Tomcat Connector的编译：](#apache-tomcat-connector的编译)
            - [nginx的编译安装：](#nginx的编译安装)
        - [多个tomcat实例的配置修改：](#多个tomcat实例的配置修改)
    - [tomcat集群搭建：](#tomcat集群搭建)
        - [单机多实例tomcat集群的搭建：](#单机多实例tomcat集群的搭建)
        - [多机多实例tomcat集群的搭建：](#多机多实例tomcat集群的搭建)

<!-- /TOC -->

# 搭建tomcat集群
因为现在的web应用使用单一的tomcat服务器，响应能力有限，并且没有高可用，所以想通过单机多实例的方式提供高可用，然后通过多机多实例的方式提高并发。
tomcat集群搭建的关键是通过什么样的组织形式对当前的tomcat服务器进行配置。不同的配置和搭建方式，解决的问题也是不同的。
为了尽可能多的测试tomcat集群的搭建方式，我们将会尝试各种配置。

## 集群搭建的准备：
要完成tomcat集群环境的搭建，需要一些配置软件的安装才能做到，否则只是通过tomcat是无法完成这个任务的。

### 相关软件的编译安装：
当前环境为centos7_x64系统。

#### apache的安装：
apache2.4版本之后需要APR的支持，需要先提供环境才能编译，首先下载需要的包：
从[官网](http://apr.apache.org/)下载apr和apr-util，还需[pcre](http://pcre.org/)，完成下载后解压缩，然后开始编译。
需要注意的是，apr-util1.6版本存在bug，需要降一个版本使用，否则会因为对expa的链接没有主动链接造成编译错误。

APR:
```shell
./configure --prefix=/usr/local/bin/tomcatCluster/apr
make -j4
make install
```
APR-UTIL:
```shell
yum install expat-devel
./configure --prefix=/usr/local/bin/tomcatCluster/apr-util --with-apr=/usr/local/bin/tomcatCluster/apr
make -j4
make install
```
pcre:
```shell
yum install gcc-c++
./configure --prefix=/usr/local/bin/tomcatCluster/pcre
make -j4
make install
```
apache:
```shell
./configure --prefix=/usr/local/bin/tomcatCluster/apache24 --with-apr=/usr/local/bin/tomcatCluster/apr --with-apr-util=/usr/local/bin/tomcatCluster/apr-util --with-pcre=/usr/local/bin/tomcatCluster/pcre
make -j4
make install
```

然后输入：
apache24/bin/apachectl start
启动httpd服务，然后查看是否可运行；
curl localhost
返回结果为：
```shell
<html><body><h1>It works!</h1></body></html>
```

参考：
[Linux下编译安装Apache 2.4](http://www.cnblogs.com/freeweb/p/5177516.html)
[編譯安裝apache2.4.26出現的問題](http://www.503e.net/%E7%BC%96%E8%AF%91%E5%AE%89%E8%A3%85apache2-4-26%E5%87%BA%E7%8E%B0%E7%9A%84%E9%97%AE%E9%A2%98/)

#### Apache Tomcat Connector的编译：
从官方下载地址：
http://tomcat.apache.org/connectors-doc/webserver_howto/apache.html
获取最新的代码，然后编译安装：
```shell
yum install autoconf libtool
cd native
./buildconf.sh
./configure --with-apxs=/usr/local/bin/tomcatCluster/apache24/bin/apxs
make
make install
```
就会在/usr/local/bin/tomcatCluster/apache24/module/目录下看到mod_jk.so文件了，表示编译安装成功。

#### nginx的编译安装：
从nginx官方网站下载最新的稳定版本：
https://nginx.org/en/download.html
因为nginx编译依赖于zlib，openssl和pcre，我们之前已经安装了pcre，系统默认带了zlib，现在只需要编译安装openssl就可以了，从官方：
https://www.openssl.org/source/
下载1.1版本的openssl。

然后执行安装：
```shell
# openssl install
  ./config --prefix=/usr/local/bin/openssl --openssldir=/usr/local/bin/openssl/conf
make -j4
make install

cd nginx
yum install zlib-devel
./configure \
--user=www \
--group=www \
--prefix=/usr/local/bin/tomcatCluster/nginx \
--with-http_ssl_module \
--with-openssl=/home/tools/apache/openssl-1.1.0f \
--with-pcre=/home/tools/apache/pcre-8.41 \
--with-http_stub_status_module \
--with-threads
make -j4
make install

```
需要注意的是：nginx编译需要依赖pcre和openssl的源代码，而不是安装目录。
安装完毕，使用如下命令检查：
```shell
/usr/local/nginx/sbin/nginx -V
```
回显：
```shell
nginx version: nginx/1.12.1
built by gcc 4.8.5 20150623 (Red Hat 4.8.5-11) (GCC) 
built with OpenSSL 1.1.0f  25 May 2017
TLS SNI support enabled
configure arguments: --user=www --group=www --prefix=/usr/local/bin/tomcatCluster/nginx --with-http_ssl_module --with-openssl=/home/tools/apache/openssl-1.1.0f --with-pcre=/home/tools/apache/pcre-8.41 --with-http_stub_status_module --with-threads
```
表示当前的nginx编译成功。

### 多个tomcat实例的配置修改：
同时复制两个tomcat到目录下，修改server.xml文件，具体修改为：
可以通过命令查看：
diff -urNa server.xml /usr/local/bin/tomcatCluster/tomcatServer_2/conf/server.xml 
返回：
```shell
--- server.xml	2017-07-11 09:58:47.389073508 +0800
+++ /usr/local/bin/tomcatCluster/tomcatServer_2/conf/server.xml	2017-07-11 09:58:04.286905547 +0800
@@ -19,7 +19,7 @@
      define subcomponents such as "Valves" at this level.
      Documentation at /docs/config/server.html
  -->
-<Server port="8005" shutdown="SHUTDOWN">
+<Server port="9005" shutdown="SHUTDOWN">
   <Listener className="org.apache.catalina.startup.VersionLoggerListener" />
   <!-- Security listener. Documentation at /docs/config/listeners.html
   <Listener className="org.apache.catalina.security.SecurityListener" />
@@ -68,7 +68,7 @@
          APR (HTTP/AJP) Connector: /docs/apr.html
          Define a non-SSL HTTP/1.1 Connector on port 8080
     -->
-    <Connector port="8080" protocol="HTTP/1.1"
+    <Connector port="9080" protocol="HTTP/1.1"
                connectionTimeout="20000"
                redirectPort="8443" />
     <!-- A "Connector" using the shared thread pool-->
@@ -90,7 +90,7 @@
     -->
 
     <!-- Define an AJP 1.3 Connector on port 8009 -->
-    <Connector port="8009" protocol="AJP/1.3" redirectPort="8443" />
+    <Connector port="9009" protocol="AJP/1.3" redirectPort="8443" />
 
 
     <!-- An Engine represents the entry point (within Catalina) that processes
@@ -102,7 +102,7 @@
     <!-- You should set jvmRoute to support load-balancing via AJP ie :
     <Engine name="Catalina" defaultHost="localhost" jvmRoute="jvm1">
     -->
-    <Engine name="Catalina" defaultHost="localhost" jvmRoute="tomcatServer_1">
+    <Engine name="Catalina" defaultHost="localhost" jvmRoute="tomcatServer_2">
 
       <!--For clustering, please take a look at documentation at:
           /docs/cluster-howto.html  (simple how to)
```


## tomcat集群搭建：

### 单机多实例tomcat集群的搭建：
采用apache和tomcat来一起处理，可以达到目的是负载均衡，通过多个tomcat服务器来提高最高访问并发，充分使用当前服务器的性能。


### 多机多实例tomcat集群的搭建：
