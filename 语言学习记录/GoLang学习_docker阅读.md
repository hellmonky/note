# GoLang学习

** 通过docker源代码阅读完成对Go语言的初步学习
** RTFSC：在实战中学习才能学到真的东西

<!-- TOC -->

- [GoLang学习](#golang%E5%AD%A6%E4%B9%A0)
    - [从编写程序的角度来学习Go的基础：](#%E4%BB%8E%E7%BC%96%E5%86%99%E7%A8%8B%E5%BA%8F%E7%9A%84%E8%A7%92%E5%BA%A6%E6%9D%A5%E5%AD%A6%E4%B9%A0go%E7%9A%84%E5%9F%BA%E7%A1%80)
        - [那么开始动手的时候，需要什么基本组件：](#%E9%82%A3%E4%B9%88%E5%BC%80%E5%A7%8B%E5%8A%A8%E6%89%8B%E7%9A%84%E6%97%B6%E5%80%99%E9%9C%80%E8%A6%81%E4%BB%80%E4%B9%88%E5%9F%BA%E6%9C%AC%E7%BB%84%E4%BB%B6)
            - [首先，从函数入手：](#%E9%A6%96%E5%85%88%E4%BB%8E%E5%87%BD%E6%95%B0%E5%85%A5%E6%89%8B)
                - [1 看看关于函数的一些BNF定义：](#1-%E7%9C%8B%E7%9C%8B%E5%85%B3%E4%BA%8E%E5%87%BD%E6%95%B0%E7%9A%84%E4%B8%80%E4%BA%9Bbnf%E5%AE%9A%E4%B9%89)
                    - [函数声明：](#%E5%87%BD%E6%95%B0%E5%A3%B0%E6%98%8E)
                    - [函数类型：](#%E5%87%BD%E6%95%B0%E7%B1%BB%E5%9E%8B)
                    - [方法：](#%E6%96%B9%E6%B3%95)
                - [2 实际看看一个函数：](#2-%E5%AE%9E%E9%99%85%E7%9C%8B%E7%9C%8B%E4%B8%80%E4%B8%AA%E5%87%BD%E6%95%B0)
            - [然后，看看一个类是怎么实现的：](#%E7%84%B6%E5%90%8E%E7%9C%8B%E7%9C%8B%E4%B8%80%E4%B8%AA%E7%B1%BB%E6%98%AF%E6%80%8E%E4%B9%88%E5%AE%9E%E7%8E%B0%E7%9A%84)
            - [最后，整个程序是怎么组合不同的类完成功能的：](#%E6%9C%80%E5%90%8E%E6%95%B4%E4%B8%AA%E7%A8%8B%E5%BA%8F%E6%98%AF%E6%80%8E%E4%B9%88%E7%BB%84%E5%90%88%E4%B8%8D%E5%90%8C%E7%9A%84%E7%B1%BB%E5%AE%8C%E6%88%90%E5%8A%9F%E8%83%BD%E7%9A%84)
    - [总是要比较，那么就比比看：](#%E6%80%BB%E6%98%AF%E8%A6%81%E6%AF%94%E8%BE%83%E9%82%A3%E4%B9%88%E5%B0%B1%E6%AF%94%E6%AF%94%E7%9C%8B)
    - [实际工作使用的效果如何：](#%E5%AE%9E%E9%99%85%E5%B7%A5%E4%BD%9C%E4%BD%BF%E7%94%A8%E7%9A%84%E6%95%88%E6%9E%9C%E5%A6%82%E4%BD%95)
    - [关于多种语言之间的协作开发：](#%E5%85%B3%E4%BA%8E%E5%A4%9A%E7%A7%8D%E8%AF%AD%E8%A8%80%E4%B9%8B%E9%97%B4%E7%9A%84%E5%8D%8F%E4%BD%9C%E5%BC%80%E5%8F%91)
    - [后记：](#%E5%90%8E%E8%AE%B0)

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
可以看到function中使用了Signature来作为函数的返回，这个是在语法层面和C中不一样的地方，也就是编写Go的语言的结构发生了变化。
这个返回的内容可以带多个参数回来：

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




#### 然后，看看一个类是怎么实现的：

#### 最后，整个程序是怎么组合不同的类完成功能的：


## 总是要比较，那么就比比看：

## 实际工作使用的效果如何：

## 关于多种语言之间的协作开发：

## 后记：


参考网页：
[手把手教你构建 C 语言编译器（9）- 总结](http://lotabout.me/2016/write-a-C-interpreter-9/)
