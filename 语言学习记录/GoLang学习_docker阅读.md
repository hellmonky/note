# GoLang学习

**通过docker源代码阅读完成对Go语言的初步学习**
**RTFSC：在实战中学习才能学到真的东西**
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
                - [4 一个可编译执行的源文件：](#4-一个可编译执行的源文件)
                - [5 初步了解源代码的组织形式：](#5-初步了解源代码的组织形式)
                    - [5.1 包的定义和使用初学：](#51-包的定义和使用初学)
                    - [5.2 包内函数的可见性：](#52-包内函数的可见性)
                    - [5.3 多个源文件构成的同一个包：](#53-多个源文件构成的同一个包)
            - [接着，从顶到底梳理golang的代码组织关系：](#接着从顶到底梳理golang的代码组织关系)
                - [1 golang的workspace标准目录结构：](#1-golang的workspace标准目录结构)
                - [2 golang的package管理逻辑：](#2-golang的package管理逻辑)
                - [3 golang中直接使用github远程代码：](#3-golang中直接使用github远程代码)
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
我们得到了希望的一个用go实现的函数，那么我们如何将这个函数补全为一个可编译执行的go文件？然后不同的go文件之间应该如何协作？

##### 1 go环境搭建：
既然要编译go源代码，就需要在当前操作系统安装go支持。现在以windows为例进行安装。
安装还是非常简单的，从[官网下载页](https://golang.org/dl/)下载对应操作系统的安装包，然后进行默认安装。
在windows下，默认安装位置为c:\Go 目录下，然后在系统环境变量中添加：
```shell
GOROOT C:\Go\
GOPATH C:\Go_pacakge
GOBIN %GOROOT%bin
```
具体内容可以参考文档：[GoLang学习_windows开发环境搭建]()

关于Linux下安装go最新稳定版的方式参考：
> - [CentOS 6、CentOS 7 安装 Golang](http://www.linux-mac.com/archives/656)
> - [如何在CentOS / RHEL 7/6/5安装Go 1.7](http://www.howtoing.com/install-go-on-centos/)

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
将这个文件保存为hello.go。并且，Go源码文件需要以UTF-8编码存储。
然后再命令行下输入：
```shell
go build hello.go
```
就会生成一个hello的可执行文件。运行这个可执行文件，就会得到"Hello World!"输出。

##### 3 分析这个源代码的结构：
逐行分析上面这段程序： 
第一行是必须的。所有Go语言编写的文件都以package <*>开头，对于独立运行的执行文件必须是 package main； 
第二行表示将fmt包加入main。一般非main的其他package（包）都被称为库， 
第三行就是程序中的主函数。Go程序执行时候，首先调用的函数就是main函数。这个是从C中继承过来的。这里是main函数的函数定义。 
第四行调用fmt包的函数打印字符串到屏幕。字符串由“”包裹，并且可以包含非ASCII的字符。

##### 4 一个可编译执行的源文件：
Go保持了与C家族语言一致的风格：即目标为可执行程序的Go源码中务必要有一个名为main的函数，该函数即为可执行程序的入口点。除此之外 Go还增加了一个约束：作为入口点的main函数必须在名为main的package中。
所以，一个独立的可执行的golang程序，package main是必须出现，紧跟在是引入的各种库，然后是各个函数，这里必须要有一个main函数。main函数是整个程序的入口。
也就是说上面的hello.go是因为包含了main，并且依赖了go默认提供的运行时库fmt才能够被正确的编译和独立的运行。并且这个hello.go构成了一个自己编写是单独的可编译执行的源文件。

##### 5 初步了解源代码的组织形式：
通过上面的例子，我们知道了一个独立的可编译的go文件的组成，但是这个源文件也是因为依赖了go的运行时库提供的包才能正确的运行输出。
所以真正意义上的单独可运行的单独的go文件是没有意义的，程序总是通过相互协作来才能完成功能。那么多个go文件该如何相互组合来形成大的可执行的文件？
方式就和hello.go中引入官方提供的运行时库的形式一样，通过package来组织不同的go源文件。

Golang使用包（package）这种语法元素来组织源码，包是函数和数据的集合。所有语法可见性均定义在package这个级别，与Java 、python等语言相比，这算不上什么创新，但与C传统的include相比，则是显得“先进”了许多。
上面的程序被放在叫做hello.go的文件中，这个文件通过package关键字被定义为一个叫做main的package，表示这个package可以被独立的编译为可执行程序，并且程序执行的入口为package中的main函数。

###### 5.1 包的定义和使用初学：
Golang中包的定义和使用看起来十分简单，通过package关键字定义包，文件名不需要与包名一致。包名是小写的一个单词；不应当有下划线或混合大小写：
```golang
package xxx
```
使用import关键字，导入要使用的标准库包或第三方依赖包。
```golang
import "a/b/c"
import "fmt"

c.Func1()
fmt.Println("Hello, World")
```

###### 5.2 包内函数的可见性：
如果我们定义了一个包，其中包含一些函数只想在这个包中被使用，也就是类似于private的函数，还有一些想作为外部的函数服务给其他包或者用户使用，就类似于public的函数。那么在go中应该如何表达？
在Go中，当函数的首字母大写的时候，函数会被从包中导出（在包外部可见，或者说公有的）。所以，私有函数的名字以小写字母开头。
这个规则同样适用于定义在包中的其他名字（新类型、全局变量）。注意，“大写” 的含义并不仅限于 US ASCII，它被扩展到了所有大小写字母表（拉丁文、希腊文、斯拉夫文、亚美尼亚文和埃及古文）。

这是一个初学的时候感觉很奇怪的地方，因为明确的可见性说明符更直接，使用大小写进行区分就有点隐晦了。
> - 带着原有的概念学习，一定要注意对比和反思，不要被旧的思维方式限制，而是在不同中不断思考为什么会有这些差异，以及造成这些差异的原因是什么。

###### 5.3 多个源文件构成的同一个包：
每个Go文件都属于且仅属于一个包。一个包可以由许多以.go为扩展名的源文件组成，因此文件名和包名一般来说都是不相同的。也就是说，一个Go的包可以由多个文件组成，但是使用相同的package进行指明所属的包。
那么就可以将同一个包中的不同功能的代码拆分到不同的go文件中，只需要保证package声明是相同的就行。组成一个package的多个文件，编译后实际上和一个文件类似，组成包的不同文件相互之间可以直接引用变量和函数，不论是否导出。
> - 如果相同的package的不同文件，放在不同的文件夹下，应该怎么进行组织？

每个子目录中只能存在一个package，否则编译时会报错。所以上述问题是不存在的，虽然可以将一个package的内容拆分，但是这些拆分的子文件都应该在同一个目录下面。

go不要求package的名称和所在目录名相同，但是你最好保持相同，否则容易引起歧义。**因为引入包的时候，go会使用子目录名作为包的路径，而你在代码中真正使用时，却要使用你package的名称。**


#### 接着，从顶到底梳理golang的代码组织关系：
根据上述基本的演练，对于整个go的开发过程有了初步的了解，我们现在从总体上从顶层到底层再次来梳理一下golang的代码结构和开发流程。

总体上，根据官方文档：[How to Write Go Code](https://golang.org/doc/code.html) 中对于go的代码结构的描述：
```text
* Go programmers typically keep all their Go code in a single workspace.
* A workspace contains many version control repositories (managed by Git, for example).
* Each repository contains one or more packages.
* Each package consists of one or more Go source files in a single directory.
* The path to a package's directory determines its import path.
```
这个是属于go语言开发中源代码组织的基本准则。
总体上说明了一下几点：
> - 整个go的工作目录（workspace）在逻辑上是统一的，不同的目录相当于平级的放在一起；
> - 工作目录下用不同的目录来区分不同的repositories，这些repositories可以是用git管理的远程代码仓库，也可以是用户自己的代码仓库；
> - 每一个代码仓库下面通过一个或者多个package来进行组织管理，并且package是golang的语法可见性最高级别（也就是语法层面，最高只能看到package，而无法看到package组成的代码仓库结构）；
> - 一个package就是包含一个或者多个go源文件的，并且包含在$GOPATH/src路径下的目录。同一个package中的多个go源文件被编译器看作同一个文件，所以同一个包下的不同源文件中不能出现相同的全局变量和函数（除了init函数），那么同一个package中的不同go源文件可以相互直接引用其他源文件中的数据；
> - 包名和所在的目录名是相同的，所以go中的包只有一级（不同于java中的点分割的多级包结构），但是可以通过目录的包含关系来完成多级的package结构组成（例如标准库中的net包下包含http包等其他包）；package之间的相互引用以$GOPATH/src下的路径为起点，所以引用包时需要以$GOPATH/src目录为相对根目录，依次输入下面的各级目录名。

##### 1 golang的workspace标准目录结构：
一个Golang的workspace是至少包含下面三个子目录的层次结构的目录：
> - src：目录用来放置代码源文件，在进行import时，是使用这个位置作为根目录的。自己编写的代码也应该放在这下面。
> - pkg：用来放置安装的包的链接对象(Object)的。这个概念有点类似于链接库，Go 会将编译出的可连接库放在这里， 方便编译时链接。不同的系统和处理器架构的对象会在pkg存放在不同的文件夹中；
> - bin：目录用来放置编译好的可执行文件，为了使得这里的可执行文件可以方便的运行， 可以将这个目录添加到PATH变量中。

一般，bin和pkg目录可以不创建，go命令会自动根据编译src的结果进行创建。

需要注意的是go语言中所有的代码都在同一个workspace中，这个workspace的指定是通过GOPATH这个环境变量来进行设置的，GOPATH中包含的目录就是当前整个系统中go所认定的唯一workspace位置。那么就可以通过这个方式来将不同的源代码路径都作为同一个路径。

例如：我们新建一个目录来存放第三方库，使用go get命令将第三方包的源代码放到这个目录下，然后将这个路径添加到GOPATH中，就可以在自己的代码中通过import来引入这些第三方库进行编码。
并且自己的代码不应该直接放置在go的src目录下，或者是在GOPATH中设置的第三方库所在的目录下，所以应该为自己的工作项目建立对应的项目文件夹，并且类似于java的包名一样进行命名，进行区分。
因此一般推荐设置两个GOPATH，比如在linux下的：
```shell
export GOPATH="/usr/local/share/go:$HOME/codes/go"
```
在windows下的：
```shell
GOPATH=C:\Go_package;$HOME/codes/go
```
这样第三方包就会默认放置在第一个路径中，而你可以在第二个路径下编写自己的代码。

关于第三方库的管理可以参考文档：
> - [Go 语言的包依赖管理](https://io-meter.com/2014/07/30/go's-package-management/)

##### 2 golang的package管理逻辑：
go中代码复用的级别有两层：函数和包。package作为语法可见性的最顶层结构，提供了最高层的代码复用方式。
> - [Packages](https://www.golang-book.com/books/intro/11)

与java等语言一样，go的包导入关键字为import。
java语言的import只能导入文件（导入路径以文件名结束，或者以“*”号结束），而go语言只能导入包名。代码包的导入使用代码包导入路径，导入路径就是工作区下的src目录下的相对路径。
例如go源码文件所在路径为$GOPATH/src/hello/log/logging.go，则此源码文件的导入路径为hello/log
当导入多个代码包时，即可以对每个代码包都使用import关键字，也可以所有需要导入的代码包共用一个import关键字，后一种需要用圆括号把包名括起来，并且每个包名也是独占一行
调用导入包中的变量或者函数方法：<导入包名中最后一个目录名称>.<变量名/函数名>

很多Golang初学者看到上面代码，都会想当然的将import后面的"c"、"fmt"当成包名，将其与c.Func1()和 fmt.Println()中的c和fmt认作为同一个语法元素：包名。但在深入Golang后，很多人便会发现事实上并非如此。比如在使用实时分布式消 息平台nsq提供的go client api时：

我们导入的路径如下：


在[Introducing workspaces](https://talks.golang.org/2014/organizeio.slide#9)中提到:
```txt
The Go tool understands the layout of a workspace. 
You don't need a Makefile. The file layout is everything.
Change the file layout, change the build.
```
也就是说，golang使用了这种固定的目录结构作为整个项目的编译结构，从而不再需要makefile之类的组织管理工具来对源代码的逻辑关系进行描述。这种特点也是和其他编程语言差异比较大的地方。
这样就可以通过当前的目录结构来方便的理解当前的整体代码中模块的组织关系。


##### 3 golang中直接使用github远程代码：
很多第三方库是托管在github等远程服务器上的，go中不再需要将这些代码手动同步到本地来完成库的引入，而是可以直接在自己的代码中import







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
一门语言的生命力该怎么样去衡量是一个非常关键的问题，活着和有活力的活着是不同的状态。
都是图灵完备的语言之间，为什么会有不同的特性？程序设计语言作为思维的工具，总是有着不同的视角和考虑，为什么不能尽可能多的学着从不同的角度看？

[build-web-application-with-golang](https://github.com/astaxie/build-web-application-with-golang)

