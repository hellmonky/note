# windows环境下搭建Go开发环境

by hellmonky
<!-- TOC -->

- [windows环境下搭建Go开发环境](#windows环境下搭建go开发环境)
    - [windows环境下安装Go开发环境：](#windows环境下安装go开发环境)
        - [安装和环境搭建：](#安装和环境搭建)
        - [编译环境设置：](#编译环境设置)
    - [Go语言基本语法学习：](#go语言基本语法学习)
        - [关于继承机制的讨论：](#关于继承机制的讨论)

<!-- /TOC -->

## windows环境下安装Go开发环境：

### 安装和环境搭建：
从官网下载windows下的安装包：
[Downloads](https://golang.org/dl/)
选择windows下的msi安装包，然后执行默认安装。

安装完毕后需要设置一些基本的环境变量来保证正常运行：
```shell
GOROOT：Go的安装目录
GOPATH：用于存放Go语言Package的目录，这个目录不能在Go的安装目录中
GOBIN：Go二进制文件存放目录，写成%GOROOT%\bin就好
GOOS：操作系统
GOARCH：指定系统环境，i386表示x86，amd64表示x64
PATH：需要将%GOBIN%加在PATH变量的最后，方便在命令行下运行Go
```
设置环境变量，添加如下内容：
GOROOT = C:/Go/
GOPATH = C:/Go_package/
GOBIN = %GOROOT%bin
PATH中添加GOBIN

然后在cmd中使用命令：
```shell
go env
```
来查看当前设置好的环境变量。

当环境变量都配置正常之后，Go就已经安装完毕了。现在打开命令行，运行go：
```shell
Go is a tool for managing Go source code.

Usage:

        go command [arguments]

The commands are:

        build       compile packages and dependencies
        clean       remove object files
        doc         show documentation for package or symbol
        env         print Go environment information
        fix         run go tool fix on packages
        fmt         run gofmt on package sources
        generate    generate Go files by processing source
        get         download and install packages and dependencies
        install     compile and install packages and dependencies
        list        list packages
        run         compile and run Go program
        test        test packages
        tool        run specified go tool
        version     print Go version
        vet         run go tool vet on packages

Use "go help [command]" for more information about a command.

Additional help topics:

        c           calling between Go and C
        buildmode   description of build modes
        filetype    file types
        gopath      GOPATH environment variable
        environment environment variables
        importpath  import path syntax
        packages    description of package lists
        testflag    description of testing flags
        testfunc    description of testing functions

Use "go help [topic]" for more information about that topic.
```
现在就可以使用go了。

新建一个go代码：
```go
package main
import "fmt"
func main() {
    fmt.Println("Hello, World!")
}
```
保存为hello.go，然后再cmd中执行：
```shell
go run hello.go
```
就可以看到输出了。

到此为止，基本的go语言环境已经搭建完毕。

### 编译环境设置：
基本环境设置完毕之后，为了更为方便的编辑和组织go工程，需要一个比较方便的IDE来帮助我们。
这儿使用vscode来作为过渡。

在vscode中安装插件：
Go - Version :0.6.51 
它支持以下功能：
```shell
彩色高亮Colorization
自动完成列表 (using gocode)
方法和类的签名帮助信息 (using godoc)
代码片段
快速信息 (using godef)
查找定义 (using godef)
查找引用 (using guru)
文件大纲 (using go-outline)
工作区符号搜索 (using go-symbols)
重命名 (using gorename)
保存时编译 (using go build and go test)
格式化 (using goreturns or goimports or gofmt)
增加导入 (using gopkgs)
调试 [部分实现] (using delve)
```
它的调试功能值得称赞，Go总算也有一个方便的调试功能呢，加上断点后可以断点所在的堆栈信息，变量以及监控自定义的表达式。(atom + godebug也可以实现这个功能，它也通过delve进行调试)
所以我们可以通过安装了插件的vscode来作为Go的IDE来进行开发。

然后打开vscode的用户自定义设置，添加以下内容：
```TypeScript
"go.buildOnSave": true,
"go.lintOnSave": true,
"go.vetOnSave": true,
"go.buildTags": "",
"go.buildFlags": [],
"go.lintTool": "golint",
"go.lintFlags": [],
"go.vetFlags": [],
"go.coverOnSave": false,
"go.useCodeSnippetsOnFunctionSuggest": false,
"go.formatOnSave": true, 
"go.formatTool": "goreturns",
"go.formatFlags": [],
"go.goroot": "C:/Go/",
"go.gopath": "C:/Go_package",
"go.gocodeAutoBuild": false
```
需要注意其中的go相关的系统路径的添加。

然后使用vscode打开刚刚新建的hello.go文件，这个时候在右下角你会看到 "Analysis Tools Missing"的提示，点击它就会自动安装这些所需的工具。
当然也可以在cmd中使用手动的方式来安装，这些安装的文件就会放在之前设置的GOPATH指定的路径下，也就是C:/Go_package：
```shell
go get -u -v github.com/nsf/gocode
go get -u -v github.com/rogpeppe/godef
go get -u -v github.com/golang/lint/golint
go get -u -v github.com/lukehoban/go-outline
go get -u -v sourcegraph.com/sqs/goreturns
go get -u -v golang.org/x/tools/cmd/gorename
go get -u -v github.com/tpng/gopkgs
go get -u -v github.com/newhook/go-symbols
go get -u -v golang.org/x/tools/cmd/guru
```
共计十个插件。


Failed to continue: "Cannot find Delve debugger. Ensure it is in your `GOPATH/bin` or `PATH`."

## Go语言基本语法学习：

### 关于继承机制的讨论：
和传统的java等命令式语言不同，golang并没有提供一个基于类的继承机制，而是使用接口（interface）来实现继承。但是这种对接口的继承是非侵入式（No-intrusive）的，这点也和java与c++中不同。
编译时对“引用”的类和接口定义的依赖，我们称之为“侵入性”的；任何显式的“接口”、“基类”都是侵入性的，不可避免的带来编译期依赖；即使这些依赖很小，但依然有办法而且应该尽可能消除。
“侵入式设计，就是设计者将框架功能“推”给客户端，而非侵入式设计，则是设计者将客户端的功能“拿”到框架中用。”

在java非侵入式的设计依赖方法就是反射，但是使用反射会导致效率低，并不具有类型安全性，因此，除非独立性和灵活性需求大于效率和类型安全需求的场合，一般不要使用反射。例如Spring框架这样的通用性框架也是依赖于反射来完成非侵入式的设计。非侵入式设计的概念是比IoC或者AOP更宽泛的，不要将这两些概念等同。
在C++中，通过语言机制－－模板来实现非侵入式的设计。模板是类型安全并且效率无损；它不需要你做任何继承操作，只需要满足模板参数的概念约束，提供“语法兼容” 的调用即可；注意是“语法兼容”即可，这意味着你的函数可以是“static”的，也可以是“virtual”的，也可以什么都不是，只要签名一致就可以； 比如在扩充STL时，你不需要include任何STL头文件。

当然模块系统和非侵入性在解决依赖问题上是正交的；作为降低编译期依赖的有效机制，以模板参数的概念约束形成的模块接口，应得到更多应用

> - 参考文档：
[]()
[No-intrusive, 非侵入式接口设计](http://www.cnblogs.com/so-what/archive/2012/11/21/2780057.html)
[Coding(1): 侵入式接口、反向控制、依赖注入](https://github.com/clarkehe/Android/wiki/Coding(1):-%E4%BE%B5%E5%85%A5%E5%BC%8F%E6%8E%A5%E5%8F%A3%E3%80%81%E5%8F%8D%E5%90%91%E6%8E%A7%E5%88%B6%E3%80%81%E4%BE%9D%E8%B5%96%E6%B3%A8%E5%85%A5)