# GoLang学习

** 通过docker源代码阅读完成对Go语言的初步学习
** RTFSC：在实战中学习才能学到真的东西
> - 希望自己能越过自己思维中的沟壑，看到更大的世界:)

<!-- TOC -->

- [GoLang学习](#golang学习)
    - [从编写程序的角度来学习Go的基础：](#从编写程序的角度来学习go的基础)
        - [那么开始动手的时候，需要什么基本组件：](#那么开始动手的时候需要什么基本组件)
            - [首先，从函数入手：](#首先从函数入手)
                - [1 看看关于函数的一些BNF定义：](#1-看看关于函数的一些bnf定义)
                    - [函数声明：](#函数声明)
                    - [函数类型：](#函数类型)
                    - [方法：](#方法)
                - [2 实际看看一个函数：](#2-实际看看一个函数)
            - [其次，我们看看一个可编译go文件的结构：](#其次我们看看一个可编译go文件的结构)
                - [1 go环境搭建：](#1-go环境搭建)
                - [2 构建第一个可编译执行的文件：](#2-构建第一个可编译执行的文件)
                - [3 分析这个源代码的结构：](#3-分析这个源代码的结构)
            - [然后，看看一个类是怎么实现的：](#然后看看一个类是怎么实现的)
            - [最后，整个程序是怎么组合不同的类完成功能的：](#最后整个程序是怎么组合不同的类完成功能的)
        - [总是要比较，那么就比比看：](#总是要比较那么就比比看)
        - [实际工作使用的效果如何：](#实际工作使用的效果如何)
        - [关于多种语言之间的协作开发：](#关于多种语言之间的协作开发)
    - [进入Docker：](#进入docker)
    - [后记：](#后记)

<!-- /TOC -->

## 从编写程序的角度来学习Go的基础：
新学习一门语言，最好的方法就是这样入手：我现在需要实现一个思路，那么用这个语言怎么才能快速的实现出来。
这个过程中就需要对选择的语言有一定的要求了，这个要求是建立在之前编写程序的已有思路基础上的，例如：
> - C背景的就在想用不同的函数来组织整个流程；
> - C++背景的就在考虑基本类的构造和继承链的生成过程，跨平台如何？是否需要使用模板编程等；
> - Java背景的就在想用基本类来通过什么样的设计模式完成整体的规划，并且考虑引入什么开源第三方库来简化整个工作；
> - shell背景的就会想用该调用什么系统组件来一句话完成任务；

总体上，都会用以往的编程经验来对现在遇到的问题的解决方案提出要求，一般来讲：
> - 有的语言特性需求也是当前这个语言提供的，那么就可以用当前语言的表达方式来使用，这个时候上手会非常快；
> - 有的语言特性需求是当前语言没有提供的，但是可以通过这个语言的其他基本特性搭建出来，这个时候网上一般都会有针对这个问题的解决方法的详细讨论，以及引入这两种语言优劣的对比言论；
> - 有的语言特性需求是当前语言没有提供的，但是给出了不同的解决方案，这就是这门语言设计的精髓之处了，因为这往往意味这门语言提供了不同的看待问题的方式和思路，最为明显的就是Lisp系列的语言和命令式语言的对比；

上述三种状况是新接触一门语言最为常见的状态。例如学习了C，然后学习C++虽然有很多兼容的地方，但是C++的特性太多，很多人都无法到第二种状态，学习新的特性已经耗费了很多的精力，所以在学习一门语言的时候一定要有的放矢，如果只是为了临时使用，就不要想面面俱到，先让整个工程完成，精通的时候才需要梳理各个特性细节，进行对比，这个能力也是一种语言能力（和语文是一样的，但是内核还是逻辑关系的讨论和整理）。
所以，我现在也是从阅读docker的源代码来进行学习go相关的知识点，希望看看这个语言在实际工程中能够到什么程度。

### 那么开始动手的时候，需要什么基本组件：
因为之前使用了C/C++和Java相关，在我看来，最基础的是：函数、类和包。
其中：
> - 组成函数需要：变量、控制流程结构和数据容器；
> - 组成类需要：类的结构、隔离声明和实例化构造与析构方式；
> - 组成包需要：程序片段（函数在文件中，或者一个单独的类文件）之间的相互组成关系，以及需要依赖的关键字等；

通过函数和类这个中间层次的抽象来看整个语言的开发逻辑关系，然后理清语法细节。

PS:其实从这个角度看程序，虽然可以快速的完成开发，但是总是缺少一些能够从总体看待程序设计语言功能的能力。
例如：
1 缺少从实际机器执行的角度看待程序的方式：这样总是会陷入程序提供的特性中来对实际执行的情况进行猜测，在C语言中这一点可以通过阅读汇编得到的代码进行补充，java系的则需要熟练的掌握JVM的特性来进行了解，也就是说越是提供了高级抽象能力的语言特性，其实际执行就越是复杂，也不容易简单获取。
2 缺少从作为软件为整体的角度看待独立功能的模块组成用户可实际使用的软件的能力：虽然可以通过拆分功能需求到实现方式，但是很难从实现方式的组合得到完整的功能需求的概念。
这些能力的建立是需要技术上的时间和努力的积累，也需要在不断的尝试理解不同的程序设计语言的设计理念的过程中不断升华的。学会跳出自己的舒适区，多想想为什么，现在是什么样，应该怎么样改进。

#### 首先，从函数入手：
我们先实现一个小目标：编写一个完整功能的函数，然后从函数入手来进行其他相关内容的学习。

##### 1 看看关于函数的一些BNF定义：

###### 函数声明：
为了快速准确的入手，我们先从官方文档看看[函数声明](https://golang.org/ref/spec#Function_declarations)的定义：
A function declaration binds an identifier, the function name, to a function.
一个函数声明，将函数名和函数绑定在一起。也就是和变量的声明类似，都是将一个函数过程和具体的函数名进行绑定，方便通过函数名来对这个函数过程进行调用。

还有BNF语法：
```BNF
FunctionDecl = "func" FunctionName ( Function | Signature ) .
FunctionName = identifier .
Function     = Signature FunctionBody .
FunctionBody = Block .
```
初看和C类的命令式语言中的函数的定义差不多，但是：
If the function's signature declares result parameters, the function body's statement list must end in a terminating statement.
这儿使用了signature，对比C中的函数定义的BNF：
```BNF
<function-definition> ::= {<declaration-specifier>}* <declarator> {<declaration>}* <compound-statement>
<declaration-specifier> ::= <storage-class-specifier>
                          | <type-specifier>
                          | <type-qualifier>
```
可以看到function中使用了Signature来作为函数的返回，这个是在语法层面和C中不一样的地方，也就是编写Go的语言的结构发生了变化。并且这个返回的内容可以带多个参数回来。

###### 函数类型：
除此之外，Go中还多了一个Function types：
A function type denotes the set of all functions with the same parameter and result types. The value of an uninitialized variable of function type is nil.
```BNF
FunctionType   = "func" Signature .
Signature      = Parameters [ Result ] .
Result         = Parameters | Type .
Parameters     = "(" [ ParameterList [ "," ] ] ")" .
ParameterList  = ParameterDecl { "," ParameterDecl } .
ParameterDecl  = [ IdentifierList ] [ "..." ] Type .
```
function type通过参数和返回值类型将所有函数进行分类，作为每一个函数的类型。并且，在Go中函数类型也是一种变量，我们通过type定义这种变量的类型，如果这个函数类型变量没有初始化，那么默认值为nil。在Go中，通过将函数作为一种类型的变量，我们可以将这种类型的函数作为值传递。
这一点是在C系中没有明确出现的概念，C系中常常使用函数指针来对一个函数进行变量绑定，本质上函数指针其实就是指向一个函数地址的指针。也就是说，这个指针指向的地址被解析为函数的入口来跳转，而不是作为一个变量类型被解析。函数指针需要和返回一个指针的指针函数进行区别，其格式一般为：类型标识符 *函数名(参数表) 。
> 关于C中的函数指针，参考：[C/C++ 函数指针 总结](http://www.cnblogs.com/pengdonglin137/p/3241900.html)

###### 方法：
在Go中还将有接受返回参数的函数叫做方法（Method）：
A method is a function with a receiver. A method declaration binds an identifier, the method name, to a method, and associates the method with the receiver's base type.
```BNF
MethodDecl   = "func" Receiver MethodName ( Function | Signature ) .
Receiver     = Parameters .
```

##### 2 实际看看一个函数：
了解了上述基本语法声明和定义，我们来具体看看代码，然后做更为详细的学习。
```golang
type functinTyoe func(int) bool // 声明了一个函数类型

func isOdd(integer int) bool {
	if integer%2 == 0 {
		return false
	}
	return true
}

func isEven(integer int) bool {
	if integer%2 == 0 {
		return true
	}
	return false
}
```
这段代码分析：
首先声明了一个函数类型变量functinTyoe，表示所有接受int类型参数并且返回bool类型参数的函数；
然后，定义了一个函数isOdd，实现了检查当前输入参数是否为奇数的检查，同样的，又定义了一个偶数检查函数isEven。

```golang
func filter(slice []int, f functinTyoe) []int {
	var result []int
	for _, value := range slice {
		if f(value) {
			result = append(result, value)
		}
	}
	return result
｝	
func test(){
    slice := []int {1, 2, 3, 4, 5, 7}
    fmt.Println("slice = ", slice)
    odd := filter(slice, isOdd)    // 函数当做值来传递了
    fmt.Println("Odd elements of slice are: ", odd)
    even := filter(slice, isEven)  // 函数当做值来传递了
    fmt.Println("Even elements of slice are: ", even)
}
```
这段代码中，函数filter使用了一个函数类型的变量f，这意味在go中可以将一个函数作为参数使用，特有的函数式编程的味道出来了。
并且在这个函数中，使用了for循环来进行结果的筛选，其中比较特殊的是for循环条件的编写方式：
```golang
_, value := range slice
```
其中下划线为blank identifier，根据官方文档[The blank identifier](https://golang.org/doc/effective_go.html#blank)所述，这个下划线操作符被广泛的在go中使用，尤其是[for range loops](https://golang.org/doc/effective_go.html#for)和[maps](https://golang.org/doc/effective_go.html#maps)中：
The blank identifier can be assigned or declared with any value of any type, with the value discarded harmlessly.It's a bit like writing to the Unix /dev/null file: it represents a write-only value to be used as a place-holder where a variable is needed but the actual value is irrelevant. It has uses beyond those we've seen already.
也就是说，"_"(下划线)，可以简单理解为赋值但以后不再使用，并且用UNIX设备中的/dev/null来进行类比。
在上述这个for循环中，使用了range clause结构来获取slice数组的值，根据for的官方文档中对range clause的说明：
If you're looping over an array, slice, string, or map, or reading from a channel, a range clause can manage the loop.
参考[聊聊Go中的Range关键字](https://xiaozhou.net/something-about-range-of-go-2016-04-10.html)中的例子说明。我们可以确定使用for-range获取的返回值有两个：一个是index，一个是value。因为这儿只需要值，所以使用下划线来表示获取index，但是不使用。然后for循环中的使用的函数类型变量来对值进行处理，将获取的结果作为判断结果来填充返回结果。
> - 参考：[What is “_,” in a Golang declaration?](http://stackoverflow.com/questions/27764421/what-is-in-a-golang-declaration)


#### 其次，我们看看一个可编译go文件的结构：
我们得到了希望的一个用go实现的函数，那么我们如何将这个函数补全为一个可编译的

##### 1 go环境搭建：
既然要编译go源代码，就需要在当前操作系统安装go支持。现在以windows为例进行安装。
安装还是非常简单的，从[官网下载页](https://golang.org/dl/)下载对应操作系统的安装包，然后进行默认安装。
在windows下，默认安装位置为c:\Go 目录下，然后在系统环境变量中添加：
```shell
GOROOT C:\Go\
GOPATH C:\Go_pacakge
GOBIN %GOROOT%bin
```
具体内容可以参考文档：GoLang学习_windows开发环境搭建.md

##### 2 构建第一个可编译执行的文件：
说了这么多，现在试试将上述代码合成一个可编译执行的go源代码了。
首先，还是从经典的C语言helloworld开始：
```golang
package main  
import  "fmt" //引入fmt库  
func main() {  
    fmt.Println("Hello World!")  
}
```
将这个文件保存为hello.go。
然后再命令行下输入：
```shell
go build hello.go
```
就会生成一个hello的可执行文件。运行这个可执行文件，就会得到"Hello World!"输出。

##### 3 分析这个源代码的结构：
逐行分析这段程序： 
第一行是必须的。所有Go语言编写的文件都以package <*>开头，对于独立运行的执行文件必须是 package main； 
第二行表示将fmt包加入main。一般非main的其他package（包）都被称为库， 
第三行就是程序中的主函数。Go程序执行时候，首先调用的函数就是main函数。这个是从C中继承过来的。这里是main函数的函数定义。 
第四行调用fmt包的函数打印字符串到屏幕。字符串由“”包裹，并且可以包含非ASCII的字符。






#### 然后，看看一个类是怎么实现的：

#### 最后，整个程序是怎么组合不同的类完成功能的：

### 总是要比较，那么就比比看：
在了解了上述go基础的概念上，看看和C等语言的显著区别，并做一个总结。
通过思维的关联交叉才能将新知识稳固下来。

### 实际工作使用的效果如何：

### 关于多种语言之间的协作开发：

## 进入Docker：
在了解了上述基本概念和代码实践后，可以开始正式入手阅读Docker的源代码了。



## 后记：

