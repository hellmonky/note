# linux下的shell脚本教程

<!-- TOC -->

- [linux下的shell脚本教程](#linux%E4%B8%8B%E7%9A%84shell%E8%84%9A%E6%9C%AC%E6%95%99%E7%A8%8B)
    - [shell的基本概念：](#shell%E7%9A%84%E5%9F%BA%E6%9C%AC%E6%A6%82%E5%BF%B5)
    - [Bash Shell基本语法：](#bash-shell%E5%9F%BA%E6%9C%AC%E8%AF%AD%E6%B3%95)
        - [一个简单的Shell实例：](#%E4%B8%80%E4%B8%AA%E7%AE%80%E5%8D%95%E7%9A%84shell%E5%AE%9E%E4%BE%8B)
            - [作为可执行程序运行脚本：](#%E4%BD%9C%E4%B8%BA%E5%8F%AF%E6%89%A7%E8%A1%8C%E7%A8%8B%E5%BA%8F%E8%BF%90%E8%A1%8C%E8%84%9A%E6%9C%AC)
            - [作为解释器参数运行脚本：](#%E4%BD%9C%E4%B8%BA%E8%A7%A3%E9%87%8A%E5%99%A8%E5%8F%82%E6%95%B0%E8%BF%90%E8%A1%8C%E8%84%9A%E6%9C%AC)
        - [Shell语法：](#shell%E8%AF%AD%E6%B3%95)
            - [Shell变量：](#shell%E5%8F%98%E9%87%8F)
                - [定义变量：](#%E5%AE%9A%E4%B9%89%E5%8F%98%E9%87%8F)
                - [使用变量：](#%E4%BD%BF%E7%94%A8%E5%8F%98%E9%87%8F)
                - [重新定义变量：](#%E9%87%8D%E6%96%B0%E5%AE%9A%E4%B9%89%E5%8F%98%E9%87%8F)
                - [只读变量：](#%E5%8F%AA%E8%AF%BB%E5%8F%98%E9%87%8F)
                - [删除变量：](#%E5%88%A0%E9%99%A4%E5%8F%98%E9%87%8F)
                - [变量类型：](#%E5%8F%98%E9%87%8F%E7%B1%BB%E5%9E%8B)
                - [特殊变量：](#%E7%89%B9%E6%AE%8A%E5%8F%98%E9%87%8F)
                    - [命令行参数：](#%E5%91%BD%E4%BB%A4%E8%A1%8C%E5%8F%82%E6%95%B0)
                    - [$* 和 $@ 的区别：](#-%E5%92%8C--%E7%9A%84%E5%8C%BA%E5%88%AB)
                    - [退出状态：](#%E9%80%80%E5%87%BA%E7%8A%B6%E6%80%81)
            - [Shell 注释：](#shell-%E6%B3%A8%E9%87%8A)
            - [Shell 字符串：](#shell-%E5%AD%97%E7%AC%A6%E4%B8%B2)
                - [单引号字符串：](#%E5%8D%95%E5%BC%95%E5%8F%B7%E5%AD%97%E7%AC%A6%E4%B8%B2)
                - [双引号字符串：](#%E5%8F%8C%E5%BC%95%E5%8F%B7%E5%AD%97%E7%AC%A6%E4%B8%B2)
                - [字符串操作：](#%E5%AD%97%E7%AC%A6%E4%B8%B2%E6%93%8D%E4%BD%9C)
                    - [拼接字符串：](#%E6%8B%BC%E6%8E%A5%E5%AD%97%E7%AC%A6%E4%B8%B2)
                    - [获取字符串长度：](#%E8%8E%B7%E5%8F%96%E5%AD%97%E7%AC%A6%E4%B8%B2%E9%95%BF%E5%BA%A6)
                    - [提取子字符串：](#%E6%8F%90%E5%8F%96%E5%AD%90%E5%AD%97%E7%AC%A6%E4%B8%B2)
                    - [查找子字符串：](#%E6%9F%A5%E6%89%BE%E5%AD%90%E5%AD%97%E7%AC%A6%E4%B8%B2)
            - [Shell 数组：](#shell-%E6%95%B0%E7%BB%84)
                - [定义数组：](#%E5%AE%9A%E4%B9%89%E6%95%B0%E7%BB%84)
                - [读取数组：](#%E8%AF%BB%E5%8F%96%E6%95%B0%E7%BB%84)
                - [获取数组长度：](#%E8%8E%B7%E5%8F%96%E6%95%B0%E7%BB%84%E9%95%BF%E5%BA%A6)
            - [Shell](#shell)
    - [看看别人是怎么用shell脚本来执行任务的：](#%E7%9C%8B%E7%9C%8B%E5%88%AB%E4%BA%BA%E6%98%AF%E6%80%8E%E4%B9%88%E7%94%A8shell%E8%84%9A%E6%9C%AC%E6%9D%A5%E6%89%A7%E8%A1%8C%E4%BB%BB%E5%8A%A1%E7%9A%84)
    - [使用shell完成一些基本任务：](#%E4%BD%BF%E7%94%A8shell%E5%AE%8C%E6%88%90%E4%B8%80%E4%BA%9B%E5%9F%BA%E6%9C%AC%E4%BB%BB%E5%8A%A1)
        - [搭建局域网内的NTP服务器：](#%E6%90%AD%E5%BB%BA%E5%B1%80%E5%9F%9F%E7%BD%91%E5%86%85%E7%9A%84ntp%E6%9C%8D%E5%8A%A1%E5%99%A8)
            - [基本安装配置：](#%E5%9F%BA%E6%9C%AC%E5%AE%89%E8%A3%85%E9%85%8D%E7%BD%AE)
            - [实现步骤描述：](#%E5%AE%9E%E7%8E%B0%E6%AD%A5%E9%AA%A4%E6%8F%8F%E8%BF%B0)
            - [脚本实现：](#%E8%84%9A%E6%9C%AC%E5%AE%9E%E7%8E%B0)
                - [（1）获取本机当前的hostname和ip地址：](#1%E8%8E%B7%E5%8F%96%E6%9C%AC%E6%9C%BA%E5%BD%93%E5%89%8D%E7%9A%84hostname%E5%92%8Cip%E5%9C%B0%E5%9D%80)
                - [（2）判断当前的主机名对应的角色：](#2%E5%88%A4%E6%96%AD%E5%BD%93%E5%89%8D%E7%9A%84%E4%B8%BB%E6%9C%BA%E5%90%8D%E5%AF%B9%E5%BA%94%E7%9A%84%E8%A7%92%E8%89%B2)
                - [（3）使用sed进行文本替换和写入：](#3%E4%BD%BF%E7%94%A8sed%E8%BF%9B%E8%A1%8C%E6%96%87%E6%9C%AC%E6%9B%BF%E6%8D%A2%E5%92%8C%E5%86%99%E5%85%A5)
                    - [<1>如果是master：](#1%E5%A6%82%E6%9E%9C%E6%98%AFmaster)
                    - [<2>如果是ntp子节点：](#2%E5%A6%82%E6%9E%9C%E6%98%AFntp%E5%AD%90%E8%8A%82%E7%82%B9)
        - [局域网内的SSH通信检查：](#%E5%B1%80%E5%9F%9F%E7%BD%91%E5%86%85%E7%9A%84ssh%E9%80%9A%E4%BF%A1%E6%A3%80%E6%9F%A5)
            - [主要步骤:](#%E4%B8%BB%E8%A6%81%E6%AD%A5%E9%AA%A4)
            - [对应脚本:](#%E5%AF%B9%E5%BA%94%E8%84%9A%E6%9C%AC)
        - [kafka部署：](#kafka%E9%83%A8%E7%BD%B2)
        - [其他实例：](#%E5%85%B6%E4%BB%96%E5%AE%9E%E4%BE%8B)

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

注意，一定要写成./test.sh，而不是test.sh。运行其它二进制的程序也一样，直接写test.sh，linux系统会去PATH里寻找有没有叫test.sh的，而只有/bin, /sbin, /usr/bin，/usr/sbin等在PATH里，你的当前目录通常不在PATH里，所以写成test.sh是会找不到命令的，要用./test.sh告诉系统说，就在当前目录找。

通过这种方式运行bash脚本，第一行一定要写对，好让系统查找到正确的解释器。也就是/bin/bash。

#### 作为解释器参数运行脚本：
这种运行方式是，直接运行解释器，其参数就是shell脚本的文件名，例如：
```shell
/bin/sh test.sh
/bin/php test.php
```
这种方式运行的脚本，不需要在第一行指定解释器信息，写了也没用。


### Shell语法：
我们的第一个shell脚本完成了，现在来详细看看shell脚本的基本语法，通过这些语法可以组织复杂的任务。

#### Shell变量：
程序设计语言中，变量是非常关键的概念，用于表示可变的值，可以被作为参数来使用。

##### 定义变量：
变量的定义使用赋值的方式，等号左侧是变量名，等号右侧是变量值，并且等号两边不能有空格：
```shell
variableName="value"
```
并且变量名的命名需要遵循以下规则：
> - 首个字符必须为字母（a-z，A-Z）。
> - 中间不能有空格，可以使用下划线（_）。
> - 不能使用标点符号。
> - 不能使用bash里的关键字（可用help命令查看保留关键字）。

##### 使用变量：
使用一个定义过的变量，只要在变量名前面加美元符号（$）即可，例如：
```shell
your_name="mozhiyan"
echo $your_name
echo ${your_name}
```
变量名外面的花括号是可选的，加不加都行，加花括号是为了帮助解释器识别变量的边界，比如下面这种情况：
```shell
for skill in Ada Coffe Action Java 
do
    echo "I am good at ${skill}Script"
done
```
如果不给skill变量加花括号，写成echo "I am good at $skillScript"，解释器就会把$skillScript当成一个变量（其值为空），代码执行结果就不是我们期望的样子了。
所以推荐给所有变量加上花括号，这是个好的编程习惯。

##### 重新定义变量：
已定义的变量，可以被重新定义，如：
```shell
myUrl="http://see.xidian.edu.cn/cpp/linux/"
echo ${myUrl}
myUrl="http://see.xidian.edu.cn/cpp/shell/"
echo ${myUrl}
```
这样写是合法的，但注意，第二次赋值的时候不能写 $myUrl="http://see.xidian.edu.cn/cpp/shell/"，使用变量的时候才加美元符（$）。

##### 只读变量：
使用 readonly 命令可以将变量定义为只读变量，只读变量的值不能被改变。
下面的例子尝试更改只读变量，结果报错：
```shell
#!/bin/bash
myUrl="http://see.xidian.edu.cn/cpp/shell/"
readonly myUrl
myUrl="http://see.xidian.edu.cn/cpp/danpianji/"
```
运行脚本，结果如下：
```shell
/bin/sh: NAME: This variable is read only.
```

##### 删除变量：
使用 unset 命令可以删除变量。语法：
```shell
unset variable_name
```
变量被删除后不能再次使用；unset 命令不能删除只读变量。
例如：
```shell
#!/bin/sh
myUrl="http://see.xidian.edu.cn/cpp/u/xitong/"
unset myUrl
echo $myUrl
```
上面的脚本没有任何输出。

##### 变量类型：
运行shell时，会同时存在三种变量：
1) 局部变量
局部变量在脚本或命令中定义，仅在当前shell实例中有效，其他shell启动的程序不能访问局部变量。

2) 环境变量
所有的程序，包括shell启动的程序，都能访问环境变量，有些程序需要环境变量来保证其正常运行。必要的时候shell脚本也可以定义环境变量。

3) shell变量
shell变量是由shell程序设置的特殊变量。shell变量中有一部分是环境变量，有一部分是局部变量，这些变量保证了shell的正常运行

##### 特殊变量：
在变量定义中有命名规范，变量名只能包含数字、字母和下划线，因为某些包含其他字符的变量有特殊含义，这样的变量被称为特殊变量。
例如，$ 表示当前Shell进程的ID，即pid，看下面的代码：
```shell
echo $$
```
回显就是当前shell的PID。

对于特殊变量，总结为如下列表：
变量 | 含义
------- | -------
$0 | 当前脚本的文件名
$n | 传递给脚本或函数的参数。n 是一个数字，表示第几个参数。例如，第一个参数是$1，第二个参数是$2。
$# | 传递给脚本或函数的参数个数。
$* | 传递给脚本或函数的所有参数。
$@ | 传递给脚本或函数的所有参数。被双引号(" ")包含时，与 $* 稍有不同，下面将会讲到。
$? | 上个命令的退出状态，或函数的返回值。
$$ | 当前Shell进程ID。对于 Shell 脚本，就是这些脚本所在的进程ID。

我们详细看看这些特殊变量的具体含义：
###### 命令行参数：
运行脚本时传递给脚本的参数称为命令行参数。命令行参数用 $n 表示，例如，$1 表示第一个参数，$2 表示第二个参数，依次类推。
例如脚本：
```shell
#!/bin/bash
echo "File Name: $0"
echo "First Parameter : $1"
echo "First Parameter : $2"
echo "Quoted Values: $@"
echo "Quoted Values: $*"
echo "Total Number of Parameters : $#"
```
表示其中需要2个命令行参数，我们保存后输入：
```shell
./test.sh Zara Ali
```
输出结果为：
```shell
File Name : ./test.sh
First Parameter : Zara
Second Parameter : Ali
Quoted Values: Zara Ali
Quoted Values: Zara Ali
Total Number of Parameters : 2
```

###### $* 和 $@ 的区别：
$* 和 $@ 都表示传递给函数或脚本的所有参数，不被双引号(" ")包含时，都以"$1" "$2" … "$n" 的形式输出所有参数。
但是当它们被双引号(" ")包含时，"$*" 会将所有的参数作为一个整体，以"$1 $2 … $n"的形式输出所有参数；"$@" 会将各个参数分开，以"$1" "$2" … "$n" 的形式输出所有参数。

我们再来看一个例子：
```shell
#!/bin/bash
echo "\$*=" $*
echo "\"\$*\"=" "$*"
echo "\$@=" $@
echo "\"\$@\"=" "$@"
echo "print each param from \$*"
for var in $*
do
    echo "$var"
done
echo "print each param from \$@"
for var in $@
do
    echo "$var"
done
echo "print each param from \"\$*\""
for var in "$*"
do
    echo "$var"
done
echo "print each param from \"\$@\""
for var in "$@"
do
    echo "$var"
done
```
执行 ./test.sh "a" "b" "c" "d"，看到下面的结果：
```shell
$*=  a b c d
"$*"= a b c d
$@=  a b c d
"$@"= a b c d
print each param from $*
a
b
c
d
print each param from $@
a
b
c
d
print each param from "$*"
a b c d
print each param from "$@"
a
b
c
d
```

###### 退出状态：
$? 可以获取上一个命令的退出状态。所谓退出状态，就是上一个命令执行后的返回结果。
退出状态是一个数字，一般情况下，大部分命令执行成功会返回 0，失败返回 1。不过也有一些命令返回其他值，来表示不同的错误类型。
例如上面的例子执行完毕后，使用这个命令返回0，如果没有执行任何脚本，这个命令返回127，因为:[127 Return code from $?](http://stackoverflow.com/questions/1763156/127-return-code-from)。
$? 也可以表示函数的返回值，后续将会讲解。

#### Shell 注释：
以“#”开头的行就是注释，会被解释器忽略。
sh里没有多行注释，只能每一行加一个#号。就像这样：

```shell
#--------------------------------------------
# 这是一个自动打ipa的脚本，基于webfrogs的ipa-build书写：https://github.com/webfrogs/xcode_shell/blob/master/ipa-build

# 功能：自动为etao ios app打包，产出物为14个渠道的ipa包
# 特色：全自动打包，不需要输入任何参数
#--------------------------------------------

##### 用户配置区 开始 #####
#
#
# 项目根目录，推荐将此脚本放在项目的根目录，这里就不用改了
# 应用名，确保和Xcode里Product下的target_name.app名字一致
#
##### 用户配置区 结束  #####
```
如果在开发过程中，遇到大段的代码需要临时注释起来，过一会儿又取消注释，怎么办呢？每一行加个#符号太费力了，可以把这一段要注释的代码用一对花括号括起来，定义成一个函数，没有地方调用这个函数，这块代码就不会执行，达到了和注释一样的效果。


#### Shell 字符串：
字符串是shell编程中最常用最有用的数据类型（除了数字和字符串，也没啥其它类型好用了），字符串可以用单引号，也可以用双引号，也可以不用引号。单双引号的区别跟PHP类似。

##### 单引号字符串：
例如：
```shell
str='this is a string'
```
单引号字符串的限制：
> - 1. 单引号里的任何字符都会原样输出，单引号字符串中的变量是无效的；
> - 2. 单引号字串中不能出现单引号（对单引号使用转义符后也不行）。

##### 双引号字符串：
例如：
```shell
your_name="$1"
str="Hello, I know your are \"$your_name\"! \n"
```
双引号的优点：
> - 1. 双引号里可以有变量
> - 2. 双引号里可以出现转义字符

##### 字符串操作：
Shell内置了可以对字符串进行一些基本操作来完成功能，通常这些功能都有更强的外置命令来进行替换，而且在编写脚本中也往往采用外置程序来完成字符串操作功能。
所以下面的命令可能不会被经常用到，但是需要熟悉。
###### 拼接字符串：
例如：
```shell
your_name="$1"
echo ${your_name}
greeting="hello, ${your_name} !"
echo ${greeting}
```
使用参数调用这个脚本，这个脚本将输出拼接好的字符串。
###### 获取字符串长度：
```shell
testStr="abcd"
echo ${#testStr}
```
使用：echo ${#string}
这种格式来获取字符串变量的长度。
> 这种方式是Shell内置的方式，还可以使用其他命令来获取字符串长度，例如：wc -L命令、expr length命令、wk+length命令等。

###### 提取子字符串：
使用${string:1:4}的方式，通过下标来获取字串，并且下标从0开始。
```shell
string="alibaba is a great company"
echo ${string:1:4}
```
输出liba
###### 查找子字符串：
```shell
string="alibaba is a great company"
echo `expr index "$string" is`
```
这儿就使用了expr来做字符串的计算。
expr命令是一个手工命令行计数器，用于在UNIX/Linux下求表达式变量的值，一般用于整数值，也可用于字符串。
所以严格来讲，这个子字符串查找并不是Shell脚本引擎内置的功能，而是通过外部工具来完成的。


#### Shell 数组：
bash支持一维数组（不支持多维数组），并且没有限定数组的大小。类似与C语言，数组元素的下标由0开始编号。获取数组中的元素要利用下标，下标可以是整数或算术表达式，其值应大于或等于0。
##### 定义数组：
在Shell中，用括号来表示数组，数组元素用“空格”符号分割开。定义数组的一般形式为：
array_name=(value1 ... valuen)
例如：
```shell
array_name=(value0 value1 value2 value3)
```
或者：
```shell
array_name=(
value0
value1
value2
value3
)
```
通过换行符来对数组元素进行分割。
或者单独对数组的分量进行定义：
```shell
array_name[0]=value0
array_name[1]=value1
array_name[2]=value2
```
##### 读取数组：

##### 获取数组长度：


#### Shell 



 
## 看看别人是怎么用shell脚本来执行任务的：
```shell
```

## 使用shell完成一些基本任务：

### 搭建局域网内的NTP服务器：
NTP服务器在集群环境搭建中非常重要，时钟同步作为基准信号保证服务器集群的正常运行。
当前系统运行环境是内部局域网，无法和外部服务器连接，所以需要在内网搭建NTP服务器来对整个局域网的时间进行同步。

#### 基本安装配置：
独立安装NTP需要三个rpm包，使用命令：
```shell
rpm -ivh *.rpm 
```
完成安装。

#### 实现步骤描述：
需要两个配置文件：iplist.ini和role.ini

> - （1）首先根据本机的hosts文件确定当前主机的名称和IP
> - （2）然后，根据当前本机的host名称获取其对应的NTP角色；
> - （3）明确自己的角色之后，设置NTP服务配置：
<1>如果是NTP_master，就是服务端，那么就需要修改/etc/ntp.conf文件，修改：
```
# Permit time synchronization with our time source, but do not
# permit the source to query or modify the service on this system.
restrict default nomodify notrap nopeer noquery
```
为：
```
# Permit time synchronization with our time source, but do not
# permit the source to query or modify the service on this system.
# 允许任何IP的客户端进行时间同步
restrict default nomodify
```

然后设置使用本机时间作为标准，将：
```shell
# Use public servers from the pool.ntp.org project.
# Please consider joining the pool (http://www.pool.ntp.org/join.html).
server 0.centos.pool.ntp.org iburst
server 1.centos.pool.ntp.org iburst
server 2.centos.pool.ntp.org iburst
server 3.centos.pool.ntp.org iburst
```
修改为：
```shell
# local clock
server 127.127.1.0
fudge  127.127.1.0 stratum 10
```

然后以守护进程启动NTP服务器：
```shell
ntpd -c /etc/ntp.conf -p /tmp/ntpd.pid
```
检查是否启动成功：
```shell
[root@localhost bin]# ntpstat
synchronised to local net at stratum 11 
   time correct to within 7948 ms
   polling server every 64 s
```
表示成功启动。

然后设置开机启动：
```shell
chkconfig ntpd on
```
回显：
```shell
注意：正在将请求转发到“systemctl enable ntpd.service”。
Created symlink from /etc/systemd/system/multi-user.target.wants/ntpd.service to /usr/lib/systemd/system/ntpd.service.
```
表示创建开机启动脚本成功。

<2>如果是客户端：
首先修改/etc/ntp.conf文件，设置NTP服务器地址修改：
```shell
# Use public servers from the pool.ntp.org project.
# Please consider joining the pool (http://www.pool.ntp.org/join.html).
server 0.centos.pool.ntp.org iburst
server 1.centos.pool.ntp.org iburst
server 2.centos.pool.ntp.org iburst
server 3.centos.pool.ntp.org iburst
```
为：
```
# Use public servers from the pool.ntp.org project.
# Please consider joining the pool (http://www.pool.ntp.org/join.html).
server NTP_MASTER_IP
```
其中NTP_MASTER_IP就是上一步中的主机IP。

然后手动执行一次时间同步：
```shell
/usr/sbin/ntpdate NTP_MASTER_IP
```

然后，修改crond例行性程序的配置文件：
```shell
vi /var/spool/cron/root
```
此处是以root用户为例，如果是其他用户，替换为对应的用户文件名，即可。
在该配置文件中，添加一行：
```shell
*/1 * * * * /usr/sbin/ntpdate NTP_MASTER_IP
```
保证每隔一分钟，从ntp_server，同步一次时间。
或者：
```shell
13 5,9,14,19 * * * /usr/sbin/ntpdate NTP_MASTER_IP
```
在每天的5点13分、9点13分、14点13分、19点13分与时间同步服务器进行同步

最后保存脚本，重新启动crond服务: 
```shell
service crond restart
```
一分钟以后，局域网内的所有机器的时间就同步为NTP_MASTER_IP的时间了。

#### 脚本实现：
梳理清楚了局域网内部搭建NTP服务器的过程，现在使用shell脚本来完成整个流程的自动化。

##### （1）获取本机当前的hostname和ip地址：

```shell
function get_hostip(){
    local_host="`hostname --fqdn`"
    echo ${local_host}
    local_ip=`ip addr | grep 'state UP' -A2 | tail -n1 | awk '{print $2}' | cut -f1  -d'/'`
    echo ${local_ip}
}
get_hostip
```
##### （2）判断当前的主机名对应的角色：
通过在函数中echo一个变量值的方式作为这个函数的返回值：

```shell
# 从函数中的局部变量中获取结果：
function getCurrentHOSTRole(){
    currentHostRole="NTP_MASTER"
    echo ${currentHostRole}
}
currentHostRole=$(getCurrentHOSTRole)
echo ${currentHostRole}

# 然后判断，根据不同的角色来执行不同的函数：
if [ "${currentHostRole}" == "NTP_MASTER" ]
then
     echo "get success"
else
     echo "get fail"
fi
```
##### （3）使用sed进行文本替换和写入：
使用sed来完成文本的替换和写入就可以大部分搞定配置文件的定制了。
现在看看主要操作的ntp.conf这个文件：

```shell
# For more information about this file, see the man pages
# ntp.conf(5), ntp_acc(5), ntp_auth(5), ntp_clock(5), ntp_misc(5), ntp_mon(5).

driftfile /var/lib/ntp/drift

# Permit time synchronization with our time source, but do not
# permit the source to query or modify the service on this system.
restrict default nomodify notrap nopeer noquery

# Permit all access over the loopback interface.  This could
# be tightened as well, but to do so would effect some of
# the administrative functions.
restrict 127.0.0.1 
restrict ::1

# Hosts on local network are less restricted.
#restrict 192.168.1.0 mask 255.255.255.0 nomodify notrap

# Use public servers from the pool.ntp.org project.
# Please consider joining the pool (http://www.pool.ntp.org/join.html).
server 0.centos.pool.ntp.org iburst
server 1.centos.pool.ntp.org iburst
server 2.centos.pool.ntp.org iburst
server 3.centos.pool.ntp.org iburst

#broadcast 192.168.1.255 autokey	# broadcast server
#broadcastclient			# broadcast client
#broadcast 224.0.1.1 autokey		# multicast server
#multicastclient 224.0.1.1		# multicast client
#manycastserver 239.255.254.254		# manycast server
#manycastclient 239.255.254.254 autokey # manycast client

# Enable public key cryptography.
#crypto

includefile /etc/ntp/crypto/pw

# Key file containing the keys and key identifiers used when operating
# with symmetric key cryptography. 
keys /etc/ntp/keys

# Specify the key identifiers which are trusted.
#trustedkey 4 8 42

# Specify the key identifier to use with the ntpdc utility.
#requestkey 8

# Specify the key identifier to use with the ntpq utility.
#controlkey 8

# Enable writing of statistics records.
#statistics clockstats cryptostats loopstats peerstats

# Disable the monitoring facility to prevent amplification attacks using ntpdc
# monlist command when default restrict does not include the noquery flag. See
# CVE-2013-5211 for more details.
# Note: Monitoring will not be disabled with the limited restriction flag.
disable monitor
```
然后根据当前节点的角色来进行文本替换了。使用sed来完成这个功能。

###### <1>如果是master：
需要将第八行：
```shell
restrict default nomodify notrap nopeer noquery
```
替换为：
```shell
restrict default nomodify
```
对应的脚本为：
```shell
sed -i '8 c restrict default nomodify' ntp.conf
```
然后将21到24行替换
```shell
sed '/# Please consider joining the pool (http:\/\/www.pool.ntp.org\/join.html)./{N;N;N;N;s/.*/server 127.127.1.0\nfudge  127.127.1.0 stratum 10/}' ntp.conf
```
然后启动NTP服务，添加开机启动：
```shell
ntpd -c /etc/ntp.conf -p /tmp/ntpd.pid
chkconfig ntpd on
```

综上所述，master节点命令为：

```shell
function ntp_master_config(){
    file=$1
    sed -i '8 c restrict default nomodify' ${file}
    sed -i '/# Please consider joining the pool (http:\/\/www.pool.ntp.org\/join.html)./{N;N;N;N;s/.*/server 127.127.1.0\nfudge  127.127.1.0 stratum 10/}' ${file}
    ntpd -c /etc/ntp.conf -p /tmp/ntpd.pid
    chkconfig ntpd on
}
file="/home/wentao/shell/ntp.conf"
ntp_master_config ${file}
```
###### <2>如果是ntp子节点：
将21到24行替换为NTP服务器地址：
```shell
sed '/# Please consider joining the pool (http:\/\/www.pool.ntp.org\/join.html)./{N;s/.*/server NTP_MASTER_IP/}' ntp.conf
```

综上所述，脚本为：

```shell
function ntp_slaver_config(){
    file=$1
    NTP_MASTER_IP=$2
    croncontent="*/1 * * * * /usr/sbin/ntpdate $NTP_MASTER_IP"
    cronfile="/var/spool/cron/root"
    sed -i '/# Please consider joining the pool (http:\/\/www.pool.ntp.org\/join.html)./{N;s/.*/server $NTP_MASTER_IP/}' ${file}
    # 创建定时任务
    touch ${cronfile}
    echo "${croncontent}" >> ${cronfile}
    service crond restart
}
file=""
ntp_master_ip=""
ntp_slaver_config ${file} ${ntp_master_ip}
```

### 局域网内的SSH通信检查：
局域网内汪汪需要使用SSH链接来作为心跳检测，现在就开始集群内部的SSH互联测试脚本。

#### 主要步骤:
（1）在主节点上生成key：
ssh-keygen -t rsa
需要回车：
```shell
Generating public/private rsa key pair.
Enter file in which to save the key (/root/.ssh/id_rsa): 
Created directory '/root/.ssh'.
Enter passphrase (empty for no passphrase): 
Enter same passphrase again: 
Your identification has been saved in /root/.ssh/id_rsa.
Your public key has been saved in /root/.ssh/id_rsa.pub.
The key fingerprint is:
3b:42:b6:29:ed:18:0e:77:a1:75:d8:4b:25:57:d6:50 root@localhost.localdomain
The key's randomart image is:
+--[ RSA 2048]----+
|            ++E  |
|           o  .  |
|        . o      |
|       o +       |
|      * S        |
|     * * o       |
|  . = * +        |
|   + * . .       |
|    o .          |
+-----------------+
```
以root用户执行的时候，会在/root/.ssh/文件夹下生产私钥id_rsa，并且声称了一个公钥，id_rsa.pub。

（2）然后将公钥复制到其他节点：
ssh-copy-id -i /root/.ssh/id_rsa.pub root@192.168.15.133
需要输入yes，并且输入密码：
```shell
The authenticity of host '192.168.15.133 (192.168.15.133)' can't be established.
ECDSA key fingerprint is 15:94:bf:54:5c:de:fa:d7:17:75:a5:23:cc:a9:e7:76.
Are you sure you want to continue connecting (yes/no)? yes
/usr/bin/ssh-copy-id: INFO: attempting to log in with the new key(s), to filter out any that are already installed
/usr/bin/ssh-copy-id: INFO: 1 key(s) remain to be installed -- if you are prompted now it is to install the new keys
root@192.168.15.133's password: 

Number of key(s) added: 1

Now try logging into the machine, with:   "ssh 'root@192.168.15.133'"
and check to make sure that only the key(s) you wanted were added.
```

（3）登陆子节点测试：
ssh root@192.168.15.133

因为上述步骤需要密码和工具交互，故选择使用expect来代替bash shell完成。
参考：[附录 实现用Shell脚本自动登录其他机器并执行任务](https://chu888chu888.gitbooks.io/shellstudy/content/shellexample.html)


#### 对应脚本:

```shell
#!/bin/bash
function ssh_node(){
    USERNAME=$1
    HOSTS=$2
    SCRIPT="pwd; ls -a"
    for HOSTNAME in ${HOSTS} ; do
        ssh -l ${USERNAME} ${HOSTNAME} "${SCRIPT}"
    done
}
username='root'
host='192.168.15.133 192.168.15.134'
ssh_node ${username} "${host}"
```

### kafka部署：
Hadoop中默认不包含kafka，所以需要进行单独的安装。
安装方式也非常简单，解压缩之后修改配置即可。

```shell
!/bin/bash
# 设置每个节点的kafka配置：
function setup_kafka(){
    # 获取对应的主机名
    ZOOKEEPER_NODES=$1
    content=""
    for node in ${ZOOKEEPER_NODES} ; do
        content=${content}${node}":2181,"
    done
    length=`expr ${#content} - 1`
    content=${content:0:$length}
    # 修改文件命令，设置breaker的id为当前机器的hostname
    KAFKA_CONFIG=$2
    #sed -i "s/^zookeeper.connect=/zookeeper.connect=${content}/g" ${KAFKA_CONFIG}
    sed -i "/zookeeper.connect=localhost:2181/{s/.*/zookeeper.connect=${content}/}" ${KAFKA_CONFIG}
}
zookeeper_nodes='node1 node2 node3'
kafka_config='/usr/local/hadoop/kafka_2.11-0.9.0.1/config/server.properties'
setup_kafka "${zookeeper_nodes}" ${kafka_config}
```

> **遇到一个问题：shell命令传参给函数的时候如果参数中有空格，需要再次用双引号再包含一次。否则会导致参数被空格分隔，传参失败。**

zookeeper_nodes='node1 node2 node3'
content=""
for node in ${zookeeper_nodes} ; do
    echo $content
    content=${content}${node}":2181,"
done
echo $content

content="sdfsdfs"
echo $content
sed '/zookeeper.connect=localhost:2181/{s/.*/zookeeper.connect=nod1:2181,node2:2181/}' server.properties
sed "/zookeeper.connect=localhost:2181/{s/.*/zookeeper.connect=$content/}" server.properties

sed "/# root directory for all kafka znodes./{N;s/.*/$content/}" /home/wentao/shell/server.properties





content=‘node1:2181,node2:2181,node3:2181’
    echo $content
    sed -e "s/zookeeper.connect=localhost:2181/$content/g" $KAFKA_CONFIG






### 其他实例：

或者按照行进行替换：
```shell
ntp_server_open_access="restrict default nomodify"
sed -i "3c ${ntp_server_open_access}" /etc/ntp.conf
sed -i "s/`${ntp_server_access}`/`${ntp_server_open_access}`" /etc/ntp.conf
```


计算当前输入文本的行数：
关键点在于，在awk中使用外部函数调用的参数。
```shell
function calFileLines(){
    file=$1
    lines=`awk "BEGIN {i=0} {i++} END{print i}" $file`
    echo ${lines}
}
file="/home/wentao/shell/ntp.conf"
calFileLines ${file}
```

```shell
```
