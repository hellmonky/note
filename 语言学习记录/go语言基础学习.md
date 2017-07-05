# Go语言基础学习

author:hellmonky@qq.com

## 初识：
### golang的25个关键词：
```golang
break    default      func    interface    select
case     defer        go      map          struct
chan     else         goto    package      switch
const    fallthrough  if      range        type
continue for          import  return       var
```
然后整个go程序就是通过这25个关键词进行完成了语法的组织。

### HelloWorld：
一切都从HelloWorld，新建HelloWorld.go文件，内容为：
```golang
package main

import "fmt"

func main() {
    fmt.Printf("Hello, world or 你好，世界 or καλημ ́ρα κóσμ or こんにちはせかい\n")
}
```
使用：
go run HelloWorld.go
运行文件，会在命令行中输出：
```shell
Hello, world or 你好，世界 or καλημ ́ρα κóσμ or こんにちはせかい
```
一个基本的可执行文件就完成了编译。

### 程序组织形式：
Go程序是通过package来组织的。
```golang
package <pkgName>
```
告诉我们当前文件属于哪个包，如果包名是main则告诉我们它是一个可独立运行的包，它在编译后会产生可执行文件。除了main包之外，其它的包最后都会生成*.a文件（也就是包文件）并放置在$GOPATH/pkg/$GOOS_$GOARCH中（windows环境下使用LiteIDE就是：$GOPATH/pkg/windows_amd64）。
**每一个可独立运行的Go程序，必定包含一个package main，在这个main包中必定包含一个入口函数main，而这个函数既没有参数，也没有返回值。**
通过package可以满足程序开发的：模块化（能够把你的程序分成多个模块)和可重用性（每个模块都能被其它应用程序反复使用）。
调用了fmt包里面定义的函数Printf。大家可以看到，这个函数是通过<pkgName>.<funcName>的方式调用的。

总结：Go使用package（和Python的模块类似）来组织代码。main.main()函数(这个函数位于主包）是每一个独立的可运行程序的入口点。Go使用UTF-8字符串和标识符(因为UTF-8的发明者也就是Go的发明者之一)，所以它天生支持多语言。

### 变量定义：
Go语言里面定义变量有多种方式。
使用var关键字是Go最基本的定义变量方式，与C语言不同的是Go把变量类型放在变量名后面：
```golang
//定义一个名称为“variableName”，类型为"type"的变量
var variableName type
```
定义多个变量：
```golang
//定义三个类型都是“type”的变量
var vname1, vname2, vname3 type
```
定义变量并初始化值：
```golang
//初始化“variableName”的变量为“value”值，类型是“type”
var variableName type = value
```
同时初始化多个变量：
```golang
/*
    定义三个类型都是"type"的变量,并且分别初始化为相应的值
    vname1为v1，vname2为v2，vname3为v3
*/
var vname1, vname2, vname3 type= v1, v2, v3
```
可以直接忽略类型声明，那么上面的代码变成这样了：
```golang
/*
    定义三个变量，它们分别初始化为相应的值
    vname1为v1，vname2为v2，vname3为v3
    然后Go会根据其相应值的类型来帮你初始化它们
*/
var vname1, vname2, vname3 = v1, v2, v3
```
继续简化：
```golang
/*
    定义三个变量，它们分别初始化为相应的值
    vname1为v1，vname2为v2，vname3为v3
    编译器会根据初始化的值自动推导出相应的类型
*/
vname1, vname2, vname3 := v1, v2, v3
```
:=这个符号直接取代了var和type,这种形式叫做简短声明。不过它有一个限制，那就是它只能用在函数内部；在函数外部使用则会无法编译通过，所以一般用var方式来定义全局变量。

_（下划线）是个特殊的变量名，任何赋予它的值都会被丢弃。
```golang
_, b := 34, 35
```
在这个例子中，我们将值35赋予b，并同时丢弃34。

Go对于已声明但未使用的变量会在编译阶段报错，比如下面的代码就会产生一个错误：声明了i但未使用。
```golang
package main

func main() {
    var i int
}
```

```golang

```