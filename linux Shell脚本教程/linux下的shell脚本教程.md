# linux下的shell脚本教程

<!-- TOC -->

- [linux下的shell脚本教程](#linux%E4%B8%8B%E7%9A%84shell%E8%84%9A%E6%9C%AC%E6%95%99%E7%A8%8B)
    - [shell的基本概念：](#shell%E7%9A%84%E5%9F%BA%E6%9C%AC%E6%A6%82%E5%BF%B5)
    - [Bash Shell基本语法：](#bash-shell%E5%9F%BA%E6%9C%AC%E8%AF%AD%E6%B3%95)
        - [一个简单的Shell实例：](#%E4%B8%80%E4%B8%AA%E7%AE%80%E5%8D%95%E7%9A%84shell%E5%AE%9E%E4%BE%8B)
            - [作为可执行程序运行脚本：](#%E4%BD%9C%E4%B8%BA%E5%8F%AF%E6%89%A7%E8%A1%8C%E7%A8%8B%E5%BA%8F%E8%BF%90%E8%A1%8C%E8%84%9A%E6%9C%AC)
            - [作为解释器参数运行脚本：](#%E4%BD%9C%E4%B8%BA%E8%A7%A3%E9%87%8A%E5%99%A8%E5%8F%82%E6%95%B0%E8%BF%90%E8%A1%8C%E8%84%9A%E6%9C%AC)
        - [Shell变量：](#shell%E5%8F%98%E9%87%8F)
    - [看看别人是怎么用shell脚本来执行任务的：](#%E7%9C%8B%E7%9C%8B%E5%88%AB%E4%BA%BA%E6%98%AF%E6%80%8E%E4%B9%88%E7%94%A8shell%E8%84%9A%E6%9C%AC%E6%9D%A5%E6%89%A7%E8%A1%8C%E4%BB%BB%E5%8A%A1%E7%9A%84)
    - [使用shell完成一些基本任务：](#%E4%BD%BF%E7%94%A8shell%E5%AE%8C%E6%88%90%E4%B8%80%E4%BA%9B%E5%9F%BA%E6%9C%AC%E4%BB%BB%E5%8A%A1)

<!-- /TOC -->

因为项目使用centos7作为部署开发环境，所以需要使用shell脚本来做一些自动化的工作。
之所以选择脚本，出发点在于：
> - (1)shell可以不依赖特殊的运行时环境，例如python等。
> - (2)虽然Shell需要依赖其他程序才能完成大部分的工作，但简洁的脚本语言标记方式，而且比C语言编写的程序执行更快、更有效率。

Linux的Shell解释器种类众多，常见的有：
```content
Bourne Shell（/usr/bin/sh或/bin/sh）
Bourne Again Shell（/bin/bash）
C Shell（/usr/bin/csh）
K Shell（/usr/bin/ksh）
Shell for Root（/sbin/sh）
```
最终选择bash作为Shell的解释器，也就是 Bourne Again Shell，因为Bash是大多数Linux系统默认的Shell。

## shell的基本概念：
Shell本身是一个用C语言编写的程序，它是用户使用Unix/Linux的桥梁，用户的大部分工作都是通过Shell完成的。Shell既是一种命令语言，又是一种程序设计语言。作为命令语言，它交互式地解释和执行用户输入的命令；作为程序设计语言，它定义了各种变量和参数，并提供了许多在高级语言中才具有的控制结构，包括循环和分支。
它虽然不是Unix/Linux系统内核的一部分，但它调用了系统核心的大部分功能来执行程序、建立文件并以并行的方式协调各个程序的运行。因此，对于用户来说，Shell是最重要的实用程序，深入了解和熟练掌握Shell的特性极其使用方法，是用好Unix/Linux系统的关键。
因而，单独使用Shell是没有意义的，因为Shell的功能实现依赖于操作系统，所以必须要熟悉Unix/Linux系统才能发挥出shell的全部功能。

Shell有两种执行命令的方式：
> - 交互式（Interactive）：解释执行用户的命令，用户输入一条命令，Shell就解释执行一条。
> - 批处理（Batch）：用户事先写一个Shell脚本(Script)，其中有很多条命令，让Shell一次把这些命令执行完，而不必一条一条地敲命令。

Shell脚本和编程语言很相似，也有变量和流程控制语句，但Shell脚本是解释执行的，不需要编译，Shell程序从脚本中一行一行读取并执行这些命令，相当于一个用户把脚本中的命令一行一行敲到Shell提示符下执行。

因为shell脚本需要调用系统功能来执行任务，为了安全起见，一般不适用root权限来运行shell脚本。

## Bash Shell基本语法：

### 一个简单的Shell实例：
还是以Bash解释器的Shell脚本为例，首先试试第一个shell脚本：

```shell
#!/bin/bash
echo "Hello World !"
```
其中：#!” 是一个约定的标记，它告诉系统这个脚本需要什么解释器来执行，即使用哪一种Shell。echo命令用于向窗口输出文本。

编写完成后，需要运行这个Shell脚本，有两种方式：
> - 1. 作为可执行程序
> - 2. 作为解释器参数

#### 作为可执行程序运行脚本：
将上面的代码保存为test.sh，并 cd 到相应目录，就可以执行这个脚本了：

```shell
#执行脚本#使脚本具有执行权限
chmod +x ./test.sh  
#执行脚本
./test.sh  
```
就可以在terminal中看到输出了：Hello World !

#### 作为解释器参数运行脚本：



我们的第一个shell脚本完成了，现在来详细看看shell脚本的基本语法。

### Shell变量：




## 看看别人是怎么用shell脚本来执行任务的：

## 使用shell完成一些基本任务：