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
根据给出的[下载页面](https://docs.haskellstack.org/en/stable/install_and_upgrade/#windows)，选择自己windows版本对应的应用程序下载安装。可以从官方维护的[github主页](https://github.com/commercialhaskell/stack)下载安装最新的发布版本。
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
接下来，我们通过stack安装基本的haskell解释和编译工具支持，基本的命令可以用如下命令查看：
```shell
stack --help
```
在使用stack之前，首先需要初始化stack的变量设置：


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
```
来调用haskell编译器了。
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

#### 1.2.3 最终选择：
对比[第一节](#### 1.2.1)和[第二节](#### 1.2.2)的安装过程，虽然haskell platform比较简单，但是需要自己使用cabal安装解决第三方包的依赖和运行环境的设置，反而stack比较方便，通过使用stack的命令可以方便的安装依赖、构建和编译工程。
最终选择stack的方式进行安装haskell开发环境。


## 2 使用stack构造haskell工程
stack本身就是一个完整的haskll工程构建工具，我们开发haskell程序的时候可以使用stack构建基本工程结构，然后进行代码编写。本节主要介绍使用stack构建基本工程的步骤。

### 2.1 stack新建一个haskll工程：
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
stack exec helloworld-exe
```



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

### 3.4 配置vscode的haskell插件：
完成上述两个步骤之后，需要重新配置vscode的haskell插件来让自己编写代码更加简单。





















