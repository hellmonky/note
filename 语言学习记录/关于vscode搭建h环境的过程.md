# 在windows环境下，使用vscode搭建haskell开发环境

> 很久之前在研究生毕业前系统的学习过haskell，工作之后就很少接触了，但是随着不断的编写程序，越来越觉得理性的思想对程序设计带来的力量。
> 所以再次重启haskell的学习，这次将完整的记录整个流程：从环境的搭建，编辑器的选择，语法的熟悉到程序的编写，最后完成一个完整功能的软件。
> 通过这次的整理，希望自己的能力水平能得到质的提升。

## 1 基本开发环境：

根据[官方网页](https://www.haskell.org/downloads)，目前有三种方式来安装haskell工具链和平台相关的编译器：
> Minimal installers: Just GHC (the compiler) and Cabal (a package install and build tool) are installed globally on your system, using your system's package manager.

> Stack: Installs the stack command globally: a project-centric build tool to automatically download and manage Haskell dependencies on a project-by-project basis.

> Haskell Platform: Installs GHC, Cabal, and some other tools, along with a starter set of libraries in a global location on your system.

其中Minimal installers，根据[官方页面](https://github.com/fpco/minghc#using-the-installer)的声明，已经被废弃，并且建议使用stack来进行安装。

### 1.1 windows下Haskell Platform的安装：
找到windows下的[haskell platform安装包](https://www.haskell.org/platform/windows.html)，选择minimal安装包进行下载安装。
按照默认路径安装完毕之后，进行cabal的基本设置：
使用如下命令定位cabal的配置文件路径：
```shell
cabal user-config init
```
会回显如下内容：
```shell
cabal: C:\Users\wenta\AppData\Roaming\cabal\config already exists.
```
然后根据上述cabal的配置文件路径，用文本编辑器打开，输入以下命令进行基本的cabal设置：
```shell
extra-prog-path: C:\Program Files\Haskell Platform\8.0.1\msys\usr\bin
extra-lib-dirs: C:\Program Files\Haskell Platform\8.0.1\mingw\lib
extra-include-dirs: C:\Program Files\Haskell Platform\8.0.1\mingw\include
```
设置完毕之后就可以使用cabal来安装第三方库，使用GHCI和GHC来解释和编译hs文件了。

> 使用haskell platform完整安装包之后，cabal默认位置为：C:\Program Files\Haskell Platform\8.0.1\lib\extralibs\bin  
> 然后在命令行中可以使用cabal来完成第三方库的下载、编译和安装。

### 1.2 windows下stack的安装：
在使用Haskell Platform方式安装完毕之后，需要使用cabal进行第三方包的依赖管理和安装，但是由于第三方库之间复杂的依赖关系带来的版本问题不容易解决。所以使用stack来进行haskell环境的安装。

> Stack is a cross-platform build tool for Haskell that handles management of the toolchain (including the GHC compiler and MSYS2 on Windows), building and registering libraries, and more.

#### 1.2.1 stack基本安装：
根据给出的[下载页面](https://docs.haskellstack.org/en/stable/install_and_upgrade/#windows)，选择自己windows版本对应的应用程序下载安装。可以从官方维护的[github主页](https://github.com/commercialhaskell/stack)下载代码来编译安装最新的发布版本。
> 编译stack的内容不再介绍，本文更关注于使用haskell程序设计语言完成需要的功能开发

根据[官方文档](https://docs.haskellstack.org/en/stable/install_and_upgrade/#path)，最好将stack放在stack自己下载可执行文件所放置的路径下，这样stack就可以自己更新自己了：
```shell
%APPDATA%\local\bin
```
这个具体路径可以通过如下命令查看：
```shell
echo %APPDATA%
```
然后根据回显的路径，创建对应的完整路径，然后将stack的安装包解压到其中。我本机的路径为：
```shell
C:\Users\wenta\AppData\Roaming\local\bin
```
现在就可以在命令行中使用stack命令进行管理了。

#### 1.2.2 使用stack安装基本的haskell工具环境：

##### <1> 配置stack：
在使用stack之前，首先需要初始化stack的变量设置。
stack使用YAML格式的配置文件：YAML 是一个数据序列化语言，被设计成人类直接可写可读的。它是 JSON 的严格超集，增加了语法显著换行符和缩进，就像 Python。但和 Python 不一样， YAML 根本不容许文字制表符。
> 关于YAML可以在[官方网站](http://www.yaml.org/)获取最新的规范文档信息。

> 具体的语法可以参考教程：*[X分钟速成Y](https://learnxinyminutes.com/docs/zh-cn/yaml-cn/)* 和 *[YAML 简介](http://www.ibm.com/developerworks/cn/xml/x-cn-yamlintro/)*

stack的全局默认配置文件位于：~/.stack/config.yaml，在windows下默认路径为：C:\Users\wenta\AppData\Roaming\stack\config.yaml。
编辑这个文件，添加基本的用户信息：
```shell
# This file contains default non-project-specific settings for 'stack', used
# in all projects.  For more information about stack's configuration, see
# http://docs.haskellstack.org/en/stable/yaml_configuration/
#

templates:
    params:
        author-name: hellmonky
        author-email: hellmonky@qq.com
        category: personal project
        copyright: 'Copyright: (c) 2016 hellmonky'
        github-username: hellmonky

```
下次执行 *"stack new currentProject"* 的时候就会默认使用这个全局配置作为蓝本生成工程。在新生成的currentProject工程目录下的currentProject.cabal文件中就会出现如下内容：
```shell
name:                currentProject
version:             0.1.0.0
synopsis:            Initial project template from stack
description:         Please see README.md
homepage:            https://github.com/hellmonky/testProfile#readme
license:             BSD3
license-file:        LICENSE
author:              hellmonky
maintainer:          hellmonky@qq.com
copyright:           Copyright: (c) 2016 hellmonky
category:            personal project
build-type:          Simple
-- extra-source-files:
cabal-version:       >=1.10
```
表示当前的stack全局设置生效了。

##### <2> stack的基本命令：
我们通过stack安装基本的haskell解释和编译工具支持，基本的命令可以用如下命令查看：
```shell
stack --help
```
使用stack安装基本的haskell环境：
```shell
stack setup
```
stack会根据当前的系统环境下载安装合适的GHC来进行编译。这个命令会下载对应的GHC包、7z解压文件和msys压缩包到路径：
```shell
C:\Users\wenta\AppData\Local\Programs\stack\x86_64-windows
```
下面，然后会被解压之后放置在相同的路径中。
现在就可以在命令行中使用：
```shell
stack ghc
stack ghci
```
来调用haskell编译器和解释器了。
同样的为了方便设置，stack可以使用命令：*stack path* 来查看当前stack中涉及的所有环境变量和路径，一下是我本机的环境变量查看情况：
```shell
stack-root: C:\Users\wenta\AppData\Roaming\stack
project-root: C:\Users\wenta\AppData\Roaming\stack\global-project
config-location: C:\Users\wenta\AppData\Roaming\stack\global-project\stack.yaml
bin-path: C:\Users\wenta\AppData\Roaming\stack\snapshots\412194c7\bin;C:\Users\wenta\AppData\Local\Programs\stack\x86_64-windows\ghc-7.10.3\bin;C:\Users\wenta\AppData\Local\Programs\stack\x86_64-windows\ghc-7.10.3\mingw\bin;C:\Users\wenta\AppData\Local\Programs\stack\x86_64-windows\msys2-20150512\mingw64\bin;C:\Users\wenta\AppData\Local\Programs\stack\x86_64-windows\msys2-20150512\usr\bin;C:\Users\wenta\AppData\Local\Programs\stack\x86_64-windows\msys2-20150512\usr\local\bin;C:\Program Files\Haskell\bin;C:\Program Files\Haskell Platform\8.0.1\lib\extralibs\bin;C:\Program Files\Haskell Platform\8.0.1\bin;C:\ProgramData\Oracle\Java\javapath;C:\Windows\system32;C:\Windows;C:\Windows\System32\Wbem;C:\Windows\System32\WindowsPowerShell\v1.0\;C:\Program Files (x86)\Windows Kits\10\Windows Performance Toolkit\;C:\gradle-2.12/bin;C:\Program Files\Java\jdk1.8.0_102\bin;C:\Program Files\Java\jdk1.8.0_102\jre\bin;C:\Program Files\TortoiseSVN\bin;C:\Windows\System32;C:\putty;C:\Program Files\TortoiseGit\bin;C:\Program Files\Git\cmd;C:\Program Files\Intel\WiFi\bin\;C:\Program Files\Common Files\Intel\WirelessCommon\;C:\adb;C:\Program Files\Haskell Platform\8.0.1\mingw\bin;C:\Users\wenta\AppData\Roaming\cabal\bin;C:\Users\wenta\AppData\Roaming\local\bin;C:\Program Files (x86)\Microsoft VS Code\bin
programs: C:\Users\wenta\AppData\Local\Programs\stack\x86_64-windows
compiler-exe: C:\Users\wenta\AppData\Local\Programs\stack\x86_64-windows\ghc-7.10.3\bin\ghc.exe
compiler-bin: C:\Users\wenta\AppData\Local\Programs\stack\x86_64-windows\ghc-7.10.3\bin
local-bin: C:\Users\wenta\AppData\Roaming\local\bin
extra-include-dirs: C:\Users\wenta\AppData\Local\Programs\stack\x86_64-windows\msys2-20150512\mingw64\include
extra-library-dirs: C:\Users\wenta\AppData\Local\Programs\stack\x86_64-windows\msys2-20150512\mingw64\lib
snapshot-pkg-db: C:\Users\wenta\AppData\Roaming\stack\snapshots\412194c7\pkgdb
local-pkg-db: C:\Users\wenta\AppData\Roaming\stack\global-project\.stack-work\install\29d018e7\pkgdb
global-pkg-db: C:\Users\wenta\AppData\Local\Programs\stack\x86_64-windows\ghc-7.10.3\lib\package.conf.d
ghc-package-path: C:\Users\wenta\AppData\Roaming\stack\global-project\.stack-work\install\29d018e7\pkgdb;C:\Users\wenta\AppData\Roaming\stack\snapshots\412194c7\pkgdb;C:\Users\wenta\AppData\Local\Programs\stack\x86_64-windows\ghc-7.10.3\lib\package.conf.d
snapshot-install-root: C:\Users\wenta\AppData\Roaming\stack\snapshots\412194c7
local-install-root: C:\Users\wenta\AppData\Roaming\stack\global-project\.stack-work\install\29d018e7
snapshot-doc-root: C:\Users\wenta\AppData\Roaming\stack\snapshots\412194c7\doc
local-doc-root: C:\Users\wenta\AppData\Roaming\stack\global-project\.stack-work\install\29d018e7\doc
dist-dir: .stack-work\dist\2672c1f3
local-hpc-root: C:\Users\wenta\AppData\Roaming\stack\global-project\.stack-work\install\29d018e7\hpc
local-bin-path: C:\Users\wenta\AppData\Roaming\local\bin
ghc-paths: C:\Users\wenta\AppData\Local\Programs\stack\x86_64-windows
```
同时，可以方便的使用：
```shell
stack upgrade
```
命令来更新stack命令本身。这个命令会下载stack的源代码，然后编译安装到默认的路径下。

> 可以输入 stack --version命令查看当前stack的具体版本，并且会给出对应的git版本号，这样就可以通过这个版本号访问对应的源代码了，例如：
```shell
stack --version
Version 1.1.2, Git revision c6dac65e3174dea79df54ce6d56f3e98bc060ecc (3647 commits) x86_64 hpack-0.14.0
```
> 然后可以构造如下的url来查看当前这次提交的具体的内容：
> https://github.com/commercialhaskell/stack/commit/c6dac65e3174dea79df54ce6d56f3e98bc060ecc

> 可以访问这个提交下的完整的源代码树，进行下载对应代码的zip包：
> https://github.com/commercialhaskell/stack/tree/c6dac65e3174dea79df54ce6d56f3e98bc060ecc

> 也可以check具体的版本看源代码：git checkout c6dac65e3174dea79df54ce6d56f3e98bc060ecc

#### 1.2.3 最终选择：
关于这两个平台的选择，实质是不同构建思路的选择。haskell platform更为基础，并且支持提供cabal来管理第三方库；stack更类似于一个集成开发环境，一切开发活动都通过stack来进行代理处理。

关于stack和cabal的对比分析，以及stack的出现的分析，Mathieu Boespflug的这篇文章：[Why is stack not cabal?](https://www.fpcomplete.com/blog/2015/06/why-is-stack-not-cabal) 讲解的非常详细。
总体上说，围绕着一个程序开发语言构建的生态系统，随着逐步的发展都会遇到相似的问题，这个问题就是工具类之间的依赖问题。随着围绕这个程序设计语言开发的工具类的增多，工具类之间也行程了层级关系的相互依赖，但是各自开发带来的接口耦合，增加了整个生态系统的脆弱性，并且给用户使用带来了麻烦的依赖解决问题。而且库之间的依赖只是限制生态系统发展的最坏的一个方面而已。
C遇到过的，C++和java也遇到了，并且提供了解决方式。那么在一个语言中遇到这个问题的时候，一种通用的构建工具就显得非常必要。类似于java中从maven进化到gradle，haskell也从cabal进化到stack。
stack保证了依赖的一致性和运行的一致性，确保给用户提供了一个可依赖的，不变的开发环境。
在stack中，所有的依赖版本被完整明确的保存在每个项目的stack.yaml文件中，项目之间进行依赖的隔离。给定相同的stack.yaml和操作系统环境，stack总是可以执行相同的编译计划。正是这种一致性，给开发人员带来了稳定的开发环境，给用户带来了稳定的生产环境。
这种环境也同样给初学者提供了友好的开始过程，更能关注于语言的学习和开发，将依赖的问题交给专业的stack处理。

并且在实际使用中，通过安装hlint来支持代码提示的过程，结合对比[第一节](#### 1.2.1)和[第二节](#### 1.2.2)的安装过程，虽然haskell platform比较简单，但是需要自己使用cabal安装解决第三方包的依赖和运行环境的设置，反而stack比较方便，通过使用stack的命令可以方便的安装依赖、构建和编译工程。
最终选择stack的方式进行安装haskell开发环境。

> 相关的讨论也可以参考：知乎的邵成写的 *[Haskell工具链科普](https://zhuanlan.zhihu.com/p/21798264?refer=damotou)*

## 2 使用stack构造haskell工程
stack本身就是一个完整的haskll工程构建工具，我们开发haskell程序的时候可以使用stack构建基本工程结构，然后进行代码编写。本节主要介绍使用stack构建基本工程的步骤。

官方出了详细的 *[用户手册](https://docs.haskellstack.org/en/stable/GUIDE/#user-guide)* 来详细的说明如何使用stack来构建工程。

### 2.1 stack新建一个haskll工程：
首先，我们使用stack创建一个工程，走通整个流程，然后在后续小节中进行仔细的分析。
在命令行下，进入一个目录，然后使用如下命令新建一个hellworld工程目录：
```shell
stack new hellworld
```
回显如下内容：
```shell
The following parameters were needed by the template but not provided: author-email, author-name, category, copyright, github-username
You can provide them in C:\Users\wenta\AppData\Roaming\stack\config.yaml, like this:
templates:
  params:
    author-email: value
    author-name: value
    category: value
    copyright: value
    github-username: value
Or you can pass each one as parameters like this:
stack new hellworld new-template -p "author-email:value" -p "author-name:value" -p "category:value" -p "copyright:value" -p "github-username:value"

Looking for .cabal or package.yaml files to use to init the project.
Using cabal packages:
- hellworld\hellworld.cabal

Selecting the best among 8 snapshots...

Downloaded lts-6.17 build plan.
Did not find .cabal file for servant-yaml-0.1.0.0 with Git SHA of 71c0a55d0a877954d9590e285c0eb861ace2d8cc
Right Nothing
Did not find .cabal file for servant-lucid-0.7.1 with Git SHA of 8d46c38d33953d7fd1c387ea84cec9bbe0fe42cc
Right Nothing
* Matches lts-6.17

Selected resolver: lts-6.17
Initialising configuration using resolver: lts-6.17
Total number of user packages considered: 1
Writing configuration to file: hellworld\stack.yaml
All done.
```
这样就在当前路径下生成对应的文件夹，并且包含了基本的工程文件。并且提示当前的stack设置为空，可以配置文件：C:\Users\wenta\AppData\Roaming\stack\config.yaml来完成整体配置，也可以配置在当前生成的工程目录下的config.yaml文件。
生成的文件结构如下：
```shell
|
│  hellworld.cabal
│  LICENSE
│  Setup.hs
│  stack.yaml
│
├─app
│      Main.hs
│
├─src
│      Lib.hs
│
└─test
        Spec.hs
```
然后编辑app/Main.hs这个文件，添加如下内容：
```shell
module Main where

import System.Environment

main :: IO ()
main = do 
    args <- getArgs
    putStrLn ("Hello, " ++ args !! 0)
```
保存之后，在命令行下执行：
```shell
stack build
```
然后stack会进行编译，并且提示如下内容：
```shell
hellworld-0.1.0.0: configure
Configuring hellworld-0.1.0.0...
hellworld-0.1.0.0: build
Preprocessing library hellworld-0.1.0.0...
[1 of 1] Compiling Lib              ( src\Lib.hs, .stack-work\dist\2672c1f3\build\Lib.o )
In-place registering hellworld-0.1.0.0...
Preprocessing executable 'hellworld-exe' for hellworld-0.1.0.0...
[1 of 1] Compiling Main             ( app\Main.hs, .stack-work\dist\2672c1f3\build\hellworld-exe\hellworld-exe-tmp\Main.o )
Linking .stack-work\dist\2672c1f3\build\hellworld-exe\hellworld-exe.exe ...
hellworld-0.1.0.0: copy/register
Installing library in
D:\workspace\vsproject\hellworld\.stack-work\install\b70b48a6\lib\x86_64-windows-ghc-7.10.3\hellworld-0.1.0.0-DhJo3n53QxfF0ZT4o9DA6N
Installing executable(s) in
D:\workspace\vsproject\hellworld\.stack-work\install\b70b48a6\bin
Registering hellworld-0.1.0.0...
```
这个时候的文件结构如下：
```shell
│  hellworld.cabal
│  LICENSE
│  Setup.hs
│  stack.yaml
│
├─.stack-work
│  ├─dist
│  │  └─2672c1f3
│  │      │  setup-config
│  │      │  stack-build-cache
│  │      │  stack-cabal-mod
│  │      │  stack-config-cache
│  │      │
│  │      ├─build
│  │      │  │  HShellworld-0.1.0.0-DhJo3n53QxfF0ZT4o9DA6N.o
│  │      │  │  Lib.hi
│  │      │  │  Lib.o
│  │      │  │  libHShellworld-0.1.0.0-DhJo3n53QxfF0ZT4o9DA6N.a
│  │      │  │
│  │      │  ├─autogen
│  │      │  │      cabal_macros.h
│  │      │  │      Paths_hellworld.hs
│  │      │  │
│  │      │  ├─hellworld-exe
│  │      │  │  │  hellworld-exe.exe
│  │      │  │  │
│  │      │  │  └─hellworld-exe-tmp
│  │      │  │      │  Main.hi
│  │      │  │      │  Main.o
│  │      │  │      │
│  │      │  │      └─app
│  │      │  │              Main.dump-hi
│  │      │  │
│  │      │  └─src
│  │      │          Lib.dump-hi
│  │      │
│  │      └─package.conf.inplace
│  │              hellworld-0.1.0.0-inplace.conf
│  │              package.cache
│  │
│  └─install
│      └─b70b48a6
│          ├─bin
│          │      hellworld-exe.exe
│          │
│          ├─doc
│          │  └─hellworld-0.1.0.0
│          │          LICENSE
│          │
│          ├─flag-cache
│          │      hellworld-0.1.0.0-40673576c29908dc4bd69e51997136af
│          │
│          ├─lib
│          │  └─x86_64-windows-ghc-7.10.3
│          │      └─hellworld-0.1.0.0-DhJo3n53QxfF0ZT4o9DA6N
│          │              HShellworld-0.1.0.0-DhJo3n53QxfF0ZT4o9DA6N.o
│          │              Lib.hi
│          │              libHShellworld-0.1.0.0-DhJo3n53QxfF0ZT4o9DA6N.a
│          │
│          └─pkgdb
│                  hellworld-0.1.0.0-40673576c29908dc4bd69e51997136af.conf
│                  package.cache
│
├─app
│      Main.hs
│
├─src
│      Lib.hs
│
└─test
        Spec.hs
```
可以看见多出了.stack-work文件夹，并且在这个文件夹下生成了两个文件夹：dist和install。会生成lib文件和可执行的exe文件。
我们先检测生成的文件是否可以正确的执行，使用下面的命令：
```shell
stack exec helloworld-exe hellmonky
```
使用stack的exec命令来指定当前工程的可执行文件，这儿的名称为helloworld-exe，后面跟着这个程序运行所需要的参数。回车后如果程序执行正常，表示当前stack的编译完成正确。基本的使用stack构建haskell工程的步骤就完成了。

### 2.2 关于stack的进一步探索：
在[上一小节](###2.1)中，从整体流程上说明了使用stack创建新的工程的完整流程，这一节对上述内容的细节进行展开讨论。
首先，上一节的整体流程总结下来为：
```shell
stack new my-project
cd my-project
stack setup
stack build
stack exec my-project-exe
```
其中：
> stack new 命令会创建一个新的包含所有可以正常启动一个工程所需要的文件的目录；
> stack setup 命令会下载必要的haskell编译器在一个独立的目录中（默认为：~/.stack），不会与任何系统级的安装过程干涉，只能在命令行中执行stack命令进行运行；
> stack build 命令会按照最小的依赖构建当前工程；
> stack exec my-project-exe 命令会在当前命令行中运行当前工程所构建的应用程序；
> 如果你想使用stack安装当前的应用程序，可以执行stack install <package-name>命令。

在第一步骤 *"stack new hellworld"* 命令创建工程文件后，生成了如下的文件结构：
```shell
.
├── LICENSE
├── Setup.hs
├── app
│   └── Main.hs
├── hellworld.cabal
├── src
│   └── Lib.hs
├── stack.yaml
└── test
    └── Spec.hs

    3 directories, 7 files
```
其中src目录下主要包含创建library文件的源代码，而app目录下则主要包含可执行文件相关的源代码文件。
如果在编写源代码的过程中，需要调用其它的第三方库，例如text库，具体的步骤为：

- 1. 编辑当前工程目录的hellworld.cabal文件，在其中的build-depends段落中添加需要引入的text包名，否则在执行stack build的时候会报“Could not find module”错误；
- 2. 重新执行stack build命令一次，stack会索引hellworld.cabal文件，然后下载所引入的包；
- 3. 如果在执行stack build命令的时候遇到“your package isn't in the LTS”错误，尝试在stack.yaml文件的extra-deps段落中添加对应包的更新的版本说明，然后再次执行stack build命令，直到没有错误发生；
- 4. 执行stack build正确后，即生成对应的库文件和可执行文件。

其中hellworld.cabal文件中的library段落说明了当前工程依赖的库的具体信息。初始化之后的内容为：
```yaml
library
  hs-source-dirs:      src
  exposed-modules:     Lib
  build-depends:       base >= 4.7 && < 5
  default-language:    Haskell2010
```
如果我们修改src/Lib.hs文件，引入第三方包：
```haskell
module Lib
    ( someFunc
    ) where

import qualified Data.Text.IO as T

someFunc :: IO ()
someFunc = T.putStrLn "someFunc"
```
然后使用stack build编译，会出现如下结果：
```shell
hellworld-0.1.0.0: unregistering (local file changes: app\Main.hs src\Lib.hs)
hellworld-0.1.0.0: build
Preprocessing library hellworld-0.1.0.0...

D:\workspace\vsproject\hellworld\src\Lib.hs:5:18:
    Could not find module ‘Data.Text.IO’
    Use -v to see a list of the files searched for.

--  While building package hellworld-0.1.0.0 using:
      C:\Users\wenta\AppData\Roaming\stack\setup-exe-cache\x86_64-windows\setup-Simple-Cabal-1.22.5.0-ghc-7.10.3.exe --builddir=.stack-work\dist\2672c1f3 build lib:hellworld exe:hellworld-exe --ghc-options " -ddump-hi -ddump-to-file"
    Process exited with code: ExitFailure 1
```
所以我们需要在hellworld.cabal文件中的library段落中加入Data包：
```yaml
library
  hs-source-dirs:      src
  exposed-modules:     Lib
  build-depends:       base >= 4.7 && < 5
                      ,text
  default-language:    Haskell2010
```
然后执行 *"stack build"* 命令执行编译。

> PS：千万注意，在这个src/Lib.hs文件的最上方需要加入：{-# LANGUAGE OverloadedStrings #-} ，否则会报错。
> 具体的内容可以参考：Alexander Altman写的[Basic Syntax Extensions](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/basic-syntax-extensions#overloadedstrings)

从上述也可以看出，haskell的实际实现GHC，使用了很多扩展，包括OverloadedStrings在内，这个是对标准98和2010标准的haskell进行了扩展，也是现在常用的版本，如果需要继续学习haskell，最好还是熟悉一下这些基本扩展，方便理解。

在编辑了cabal文件之后，我们也可以使用stack来查看当前项目依赖的包的详情：
``shell
stack list-dependencies
```
看到目前的项目依赖包的具体内容为：
```shell
array 0.5.1.0
base 4.8.2.0
binary 0.7.5.0
bytestring 0.10.6.0
containers 0.5.6.2
deepseq 1.4.1.1
ghc-prim 0.4.0.0
hellworld 0.1.0.0
integer-gmp 1.0.0.0
text 1.2.2.1
```

既然引入了包机制，那就还有一个问题：__如果需要使用的包已经不再维护，或者需要指定版本的包才行，那么该如何进行配置？__
还记得我们在"stack new"新建一个工程的时候，stack默认使用lts-3.2进行工程的构建吗？lts-3.2定义了我们工程依赖的库的版本，上面我们使用text的时候也只是在depends里面加入了text说明，而没有指定版本，这是因为lts-3.2已经默认包含了text的支持版本，我们可以直接使用。那么如果lts中没有支持的package要被使用，该如何进行？
我们可以使用stack.yaml中的extra-deps段落来指定自己需要的没有被lts维护的包的具体信息（名称和版本）：
```yaml
extra-deps:
- acme-missiles-0.3 # not in lts-3.2
```
然后更新我们的代码进行测试，编辑src/Lib.hs文件：
```haskell
module Lib
    ( someFunc
    ) where

import Acme.Missiles

someFunc :: IO ()
someFunc = launchMissiles
```
然后在helloworld.cabal文件中也添加相关的包支持说明：
```shell
library
  hs-source-dirs:      src
  exposed-modules:     Lib
  build-depends:       base >= 4.7 && < 5
                       -- This next line is the new one
                     , text
                     , acme-missiles
  default-language:    Haskell2010
```
然后编译测试当前的修改"stack build"：
```shell
hellworld-0.1.0.0: unregistering (missing dependencies: acme-missiles)
stm-2.4.4.1: download
stm-2.4.4.1: configure
stm-2.4.4.1: build
stm-2.4.4.1: copy/register
acme-missiles-0.3: download
acme-missiles-0.3: configure
acme-missiles-0.3: build
acme-missiles-0.3: copy/register
hellworld-0.1.0.0: configure
Configuring hellworld-0.1.0.0...
hellworld-0.1.0.0: build
Preprocessing library hellworld-0.1.0.0...
[1 of 1] Compiling Lib              ( src\Lib.hs, .stack-work\dist\2672c1f3\build\Lib.o )
In-place registering hellworld-0.1.0.0...
Preprocessing executable 'hellworld-exe' for hellworld-0.1.0.0...
[1 of 1] Compiling Main             ( app\Main.hs, .stack-work\dist\2672c1f3\build\hellworld-exe\hellworld-exe-tmp\Main.o ) [Lib changed]
Linking .stack-work\dist\2672c1f3\build\hellworld-exe\hellworld-exe.exe ...
hellworld-0.1.0.0: copy/register
Installing library in
D:\workspace\vsproject\hellworld\.stack-work\install\b70b48a6\lib\x86_64-windows-ghc-7.10.3\hellworld-0.1.0.0-3s6tG2TYF7fIYayAZNLhNw
Installing executable(s) in
D:\workspace\vsproject\hellworld\.stack-work\install\b70b48a6\bin
Registering hellworld-0.1.0.0...
Completed 3 action(s).
```
可以看到顺利的下载编译了acme-missiles这个package。


> 更多的关于stack使用教程，可以参考官方文档，也可以查看Michael Snoyman的这篇文章[New in-depth guide to stack](https://www.fpcomplete.com/blog/2015/08/new-in-depth-guide-stack)比较完整的给出了stack的概略介绍。



## 3 使用vscode搭建haskell编译开发环境：
因为目前根据[windows下的haskell环境安装](#1)中提供的方式搭建好基本环境后，就可以使用解释器GHCI和编译器GHC来测试代码了。但是这种方式不方便进行大量的程序开发工作，而且haskell官方并没有给出和提供完善的IDE开发环境来帮助编写，而是推荐使用编辑器配合插件来完成同样的目的。
常见的推荐编辑器是emacs和vim，操作配置不是非常熟悉，为了集中注意力在程序开发本身选择其他编辑器。
vscode有着丰富的插件，并且打开的内存消耗小于Atom，windows友好，最终选择vscode作为当前haskell的开发编辑器。

### 3.1 vscode的基本概念：

vscode只能针对打开文件夹来设置工作空间，然后在这个工作空间中生成.vscode目录来保存三个配置文件：
```shell
├── .vscode
│   ├── launch.json
│   ├── settings.json
│   └── tasks.json
```

### 3.2 安装vscode的haskell插件：
为了将vscode打造为haskell开发编辑器，需要基本插件支持，vscode市场上被广泛使用的有两个：haskell-linter和Haskell ghc-mod，点击安装后重启vscode生效。

### 3.3 stack安装hlint来支持开发：
vscode的haskell-linter插件需要本机安装hlint来完成功能。使用stack安装如下：
```shell
stack install hlint
~~stack --resolver lts-6.16 --install-ghc --no-system-ghc install hlint~~
```
stack会自动完成依赖的查找和安装，并且最终将可执行的hlint文件放在stack所在的目录里，供windows程序可以直接访问。
关于hlint，[官方说明文档](http://community.haskell.org/~ndm/darcs/hlint/hlint.htm)给出了详细的说明，如果需要关注更多的动态，可以在作者[Neil Mitchell的blog](http://ndmitchell.com/)页面找到更多的资料。

### 3.4 配置vscode的haskell插件：
完成上述两个步骤之后，需要重新配置vscode的haskell插件来让自己编写代码更加简单。


## 4 haskell基本语法和程序组织形式：
学习一门语言最重要的包含两点：
- 熟练的掌握这门语言的基本语法，理解这些语法对应的语义，能够明确的将自己的逻辑想法转换为这门语言的表述；
- 明确该语言支持的程序的组织形式，明确从这些组织形式到实际被编译的可执行文件之间的关联；

掌握了上述内容，就基本可以使用该语言进行程序的开发了，本节的内容也是从这两点出发，进行一些学习。因为篇幅相关，本节不会非常的详细，但是一定会将必须的内容给予分析和展示。

### 4.1 haskell语言的基本语法特点：
haskell作为一个函数式程序设计语言，和之前自己熟悉的命令式程序设计语言非常不同。虽然本质上和scheme都是相同的，但是haskell更加的强调不变性，引入monod来处理可变性内特，将haskell作为一个“纯”的函数式程序设计语言而变得非常不同。

#### <1> 基本元素和基本表达式：
> 本节根据文章：[十分钟学会_Haskell](https://wiki.haskell.org/Cn/%E5%8D%81%E5%88%86%E9%92%9F%E5%AD%A6%E4%BC%9A_Haskell) 整理而成。


> 我们在这一节使用ghci来获取解释结果，这样可以方便直观的查看当前结果，使用stack调出ghci的命令为：
```shell
stack ghci
```
然后继续下面的内容。

##### 表达式：
大部份数学表达式都可以输入 ghci 直接解答：
```haskell
3 * 5
```
符串需要双引号引用，以 ++ 连接：
```haskell
"Hello" ++ ", Haskell"
```
调用函数时，参数紧接函数即可，其间无须添加括号：
```haskell
succ 5
round 6.59
sqrt 2
```
##### 调用 I/O actions 进行控制台输入和输出：
```haskell
putStrLn "Hello, Haskell"
print (5 + 4)
```
putStr 和 putStrLn 输出字符串到终端。print 输出任意类型的值。（如果 print 作用于字符串，输出将用引号引用。) 
复杂的 I/O acttions 需要 do 语句块，以分号间隔：
```haskell
do { putStr "2 + 2 = " ; print (2 + 2) }
```
通过 getLine（返回字符串）或 readLn（返回任意你需要的类型）获得输入。用 <- 符号给 I/O action 的结果命名：
```haskell
do { n <- readLn ; print (n^2) }
```
do 语句块的另一种方式，以缩进取代花括号和分号。虽然在 ghci 中未能获得完美支持，但是可以把它们塞进源文件（如 Test.hs）里然后编译：
```haskell
main = do putStrLn "What is 2 + 2?"
        x <- readLn
        if x == 4
            then putStrLn "You're right!"
            else putStrLn "You're wrong!"
```
这儿就需要注意缩进对于haskell的意义：
    *do 之后首个非空白字符，如上例 putStrLn 的 p，是特别的。每新起一行，行首与之对齐，即视为同一 do 块之新句；缩进较之多则继续前句；较之少则结束此 do 块。*

##### 类型：
关于类型声明，Haskell会自动进行推断，不必声明之。如果非要声明类型，可用 :: 符号明确指出：
```haskell
5 :: Int
5 :: Double
```
第一行将5指定类型为一个整型，第二行则将5指定为浮点型，两者输出的结果是不同的：
```haskell
5
5.0
```
*注意：类型 types（以及类型类 type classes，稍后提及）总是以大写开头。变量（variables）总是以小写开头。这是语言规则，而不只是命名习惯。*

上述的类型说明中，exp :: T 就是向 Haskell 表示， exp 的类型是 T ，而 :: T 就是表达式 exp 的类型签名。如果一个表达式没有显式地指名类型的话，那么它的类型就通过自动推导来决定，这个时候可以通过":type"来查看当前变量的类型：
```haskell
:type 'a'
```
返回字符a的类型说明：
```haskell
'a' :: Char
```
也就是说haskell针对字符a的自动类型推导，获取a为字符类型。对于数字查看其类型：
```haskell
:type 42
42 :: Num a => a

:type 42.0
42.0 :: Fractional a => a

:type gcd 15 20
gcd 15 20 :: Integral a => a
```
这些类型用到了 "类型类（type classes）" 含义如下：
- 42 可作为任意数字(numeric)类型。(这就是为什么我既可以把5声明为Int类型，也可以声明为Double类型的原因。)
- 42.0 可作为任意分数(fractional)类型，但不能是整数(integral)类型。
- gcd 15 20 (此为函数调用) 可作为任意整数(integral)类型，但不能是分数类型。

在Haskell "Prelude"(你不需要import任何东西就能使用的那部分库)中有五种数字(numeric)类型:
- Int 是一个至少30位(bit)精度的整数。
- Integer 是一个无限精度的整数。
- Float 是一个单精度浮点数。
- Double 是一个双精度浮点数。
- Rational 是一个没有舍入误差的分数/小数类型。

上面这5个都是Num类型的实例（instances）。其中前两个是Integral类型的实例，后面三种是Fractional类型的实例。

还有一个类型是()，念做"unit"。 它只有一个取值，也写作()并念做"unit"：
```haskell
:type ()
() :: ()
```
你可以把它看作类似C语言中的void关键字。在一个I/O动作中，如果你不想返回任何东西，你可以返回()。

##### 有结构的数据：
基本数据类型可以通过两种方式组合在一起：
- 列表：lists，使用方括号组合在一起，例如：[a, b]；
- 元组：tuples，使用圆括号组合在一起，例如：(a,b)。

列表可以用来存储多个相同类型的值，多个可以个数不固定，甚至是无限多个，例如：
```haskell
[1,2,3]
[True,False,False]
```
haskell中的字符串就是字符的list：
```haskell
['H','e','l','l','o']
```
ghci返回的结果为：Hello
使用冒号":"运算符，可以将一个项（item）添加到列表的开始处，相当于lisp中的cons函数：
```haskell
'C':['H','e','l','l','o']
```
将返回：CHello

元组用来存储固定个数，但是类型可以不同的值：
```haskell
(1, True)
```
这个元组将整型和布尔型组合在一起了。
可以使用zip函数将两个列表组合为一个元组的列表：
```haskell
zip [1 .. 5] ['a' .. 'e']
```
返回的结果为：
```haskell
[(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e')]
```
他将第一个列表的第一个元素和第二个列表的第一个元素组合为一个元组，形成新的元组类型，然后依次这样处理，最后形成这种元组类型的列表。
这样就可以保证列表中的每一个元素的类型都是一样的，然后通过元组的可以容纳不同类型的元素来合并之前两个列表的每一项的内容。可以查看这个新的列表的类型：
```haskell
:type [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e')]
```
返回结果为：
```haskell
[(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e')] :: Num t => [(t, Char)]
```
这个返回结果表示ghci对当前输入内容的类型推导，两个冒号之前的是当前的输入，冒号之后的是这个输入的类型说明。
这个类型说明为：当前输入的值的类型为一个类类型的变量t（也称为类型变量），这个类型变量t是类型[(t, Char)]的一个实例。

*Haskell中经常使用小写字母，比如a，f，m等代表类型变量，并且形成了一套类型体系机制，英文名称是：Algebraic Data Types，翻译过来就是：代数数据类型。*

Algebra Data Type之所以叫这个名字是因为它可以像代数一样做运算，更为详细的内容可以参考下面三篇文章：
- [The Algebra of Algebraic Data Types, Part 1](http://chris-taylor.github.io/blog/2013/02/10/the-algebra-of-algebraic-data-types/)
- [The Algebra of Algebraic Data Types, Part 2](http://chris-taylor.github.io/blog/2013/02/11/the-algebra-of-algebraic-data-types-part-ii/)
- [The Algebra of Algebraic Data Types, Part 3](http://chris-taylor.github.io/blog/2013/02/13/the-algebra-of-algebraic-data-types-part-iii/)


##### 列表上的函数：
haskell中经常使用列表来进行计算，所以针对列表提供了大量的函数来方便使用。这些函数一般都为高阶函数，因为都需要接受函数操作参数来对列表进行处理：

map：将第一个参数指定的操作，作用在第二参数上，其中第二个参数为列表，作用方式为对列表中的每一项进行，最后返回被第一个指定的操作参数处理过的列表。定义为：
```haskell
map' :: (a -> b) -> [a] -> [b]

map' f [] = []
map' f (x:xs) =f x : map' f xs
```

filter：和map相同，但是返回第一个参数操作后为真的列表元素，是一个新的列表。定义为：
```haskell
filter' :: (a -> Bool) -> [a] -> [a]

filter' f [] = []
filter' f (x:xs)
     | f x       = x : filter' f xs
     | otherwise = filter' f xs
```

fold：它对一个列表中的所有元素做某种处理，并且一边处理元素，一边更新累积器，最后在处理完整个列表之后，返回累积器的值。根据操作执行的顺序，分别定义了左侧和右侧的fold函数：
```haskell
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f s [] = s
foldl' f s (x:xs) = foldl' f (f s x) xs

foldr' :: (a -> b -> a) -> a -> [b] -> a
foldr' f s [] = s
foldr' f s (x:xs) = f (foldr' f s xs) x
```
foldl’函数接收一个函数，一个a类型的值，一个b类型的列表，返回值为a类型的值。这个函数接收一个a类型的值，b类型的值，并返回一个a类型值。并且这个fold函数是递归的，他在实现中调用了自己。
fold函数的递归特性可以用来构造自己，例如可以使用左fold函数构造右fold函数：
```haskell
foldr2 f s [] = s
foldr2 f s (x:xs) = f s (foldl' f x xs)
```
这个右fold函数foldr2，被foldl'函数实现。
还可以使用fold函数来实现map和filter。可以使用左fold函数实现：
```haskell
map2 :: (a -> b) -> [a] -> [b]
map2 f xs =foldl' (\s x -> s ++ [f x]) [] xs

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 f [] = []
filter2 f (x:xs) = foldl' (\s x -> if f x then s ++ [x] else s ) [] xs
```
也可以使用右fold函数实现：
```haskell
map3 :: (a -> b) -> [a] -> [b
map3 f xs =foldr' (\s x -> f x : s) [] xs

filter3 :: (a -> Bool) -> [a] -> [a]
filter3 f [] = []
filter3 f (x:xs) = foldr' (\s x -> if f x then x : s else s) [] xs
```
> 注意：由于++效率没有:高，所以生成结果为list的时候最好使用右fold。

最后需要说明的是，haskell的列表其实也是一个函数，我们可以自己来实现。

> 参考文档：[Haskell函数式编程之四-List操作](http://www.cnblogs.com/huang0925/archive/2013/04/05/3000675.html)


##### 函数定义：




```haskell

```




















