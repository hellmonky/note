# golang的类型系统

类型系统是一门程序设计语言语法中最为核心的特性之一，因为类型作为语言提供的用户可以使用的最基本的抽象手段，为用户提供了自定义数据的构造。
程序设计语言还通过提供组织原则以及控制结构，完成逻辑块和代码块的实现以及组织和隔离关系。整体上程序设计语言通过这三种方式给用户提供了多维度的抽象方式和组织管理方式，供用户实现自己需要的业务内容。

我们可以将golang中25个关键词：
```text
break	case	chan	const	continue
default	defer	else	fallthrough	for
func	go	goto	if	import
interface	map	package	range	return
select	struct	switch	type	var
```
其中：
var和const ：变量和常量的声明
var varName type  或者 varName : = value
package and import: 导入
func： 用于定义函数和方法
return ：用于从函数返回
defer someCode ：在函数退出之前执行
go : 用于并行
select 用于选择不同类型的通讯
interface 用于定义接口
struct 用于定义抽象数据类型
break、case、continue、for、fallthrough、else、if、switch、goto、default 流程控制
chan用于channel通讯
type用于声明自定义类型
map用于声明map类型数据，和函数式程序设计语言的基本数据类型对照；
range用于读取slice、map、channel数据

根据这三个维度的特性进行分类：
类型抽象：const, func, interface, map, struct, type, var
控制结构：break, case, chan, continue, default, defer, else, fallthrough, for, go, goto, if, range, return, select, switch, 
组织管理：import, package

在golang中，type和struct还有interface绑定使用：
通过关键字type和struct，实现了用户定义具体类型的构造；
通过type和interface，实现了对象行为的定义。

本文就程序设计语言中的类型抽象入手，深入学习golang中提供的类型抽象手段和用法。

## 类型是什么
在程序设计语言中，类型是什么？
因为程序设计语言链接了开发者和计算机，所以程序设计语言的类型就代表了一个人所理解的逻辑概念对应的物理计算机中的内存空间使用和编码方法。
通过将一个逻辑概念映射为在内存中的一段数据，和处理这个数据需要的编码方式，完成了对信息的转换。其中编码就是一种实体之间的映射关系。
不论是人所理解的逻辑概念，还是计算机中处理的内存数据，都是对信息的一种表示。既然是信息，那么就可以用香农三定律进行描述。
在计算机中对信息的处理，就表现为对具体类型的操作，也就是对具体内存中的数据按照规定编码进行处理的方法，通过在每一种类型上所定义的操作完成类型之间的交互，也就是CPU中的基本机器指令（和汇编命令一一对应）。
一个用具体的程序设计语言表达的软件或者应用，就是建立在人所理解的逻辑概念上的整体组织管理，最终运行在计算机上的多种类型的实例之间的交互时序。
计算机将这些操作进行抽象，使用基本的操作来完成，图灵机的五元组序列，从逻辑概念到计算，计算机作为思维最后的载体被广泛的使用。
计算机是逻辑概念和数学概念的一个实现，通过布尔逻辑完成了开关电路和数学运算的等价关系转换，也明确了建立在布尔逻辑上的不可计算限制。


Type provides two pieces of information that both the compiler and you need to perform the same exercise we just went through.

1. The amount of memory, in bytes, to look at
2. The representation of those bytes

参考文档：
[Understanding Type in Go](https://www.goinggo.net/2013/07/understanding-type-in-go.html)

## golang关键字type：
Go语言里可以使用type关键字来把一个类型来转换成另外一个类型而保持数据的本质不变：
```golang
type Height     int
type Age        int
type Grade      int
```
这儿通过type定义了三个类型，这三个类型本质上都是int类型，但是因为适用场景不同，所以用三种不同的类型进行描述。
这儿使用type定义了一系列互不相干的行为特征：通过这些互不相干的行为特征，本质上同一的事物表现出不同事物的特征:整数还是整数，但年龄却不是高度也不是分数。
然后我们可以给不同的类型指定不同的行为：
```golang
// 超过50岁算老年
func (a Age) Old() bool {
    return a > 50
}

// 高于120cm需要买票
func (l Length) NeedTicket() bool {
    return l > 120
}

// 60分及格
func (g Grade) Pass() bool {
    return g >= 60
}
```
通过将数据和对这个数据的操作解耦，保证了对数据存储的和对这个数据的处理的相互独立，能够很灵活的实现业务。

例如，在游戏中，一个队伍可以包括若干玩家，这个队伍可以有加入退出等一系列游戏逻辑相关的行为，也可以按照玩家的等级进行排序。
分析如下：
1.队伍在本质上可以是一个slice
2.加入、退出队伍和队伍排序是游戏逻辑相关的
3.为了排序，队伍需要实现sort.Interface接口，这个是游戏逻辑无关的，它的存在只是为了满足按等级排序这个后台统计的要求。
实现代码为：
```golang
import "sort"

// 定义一个玩家类型
type Player struct {
	name  string
	level int
}

// 定义队伍是玩家的slice
type Team []*Player

// 对于队伍这个类型，有一系列的行为：
func (t *Team) Join(p *Player) {
	*t = append(*t, p)
}
func (t *Team) Quit(p *Player) {
	for i, v := range *t {
		if v.name == p.name {
			copy((*t)[i:], (*t)[i+1:])
			*t = (*t)[:len(*t)-1]
			return
		}
	}
}
func (t *Team) Sort() {
	sort.Sort(sortTeam(*t))
}

// 定义对队伍的排序类型，本质上还是一个队伍
type sortTeam Team

func (t sortTeam) Len() int {
	return len(t)
}
func (t sortTeam) Swap(i, j int) {
	t[i], t[j] = t[j], t[i]
}
func (t sortTeam) Less(i, j int) bool {
	return t[i].level < t[j].level
}
```
一个slice，可以是玩家队伍，也可以是sort.Interface接口的实现。
数据仍然还是同一个slice，但是不同的type对应的行为特征却迥然不同。这很好地反映了现实世界里对同一个数据的不同解读，从代码维护的角度而言，也因为不同类别的逻辑相互分离从而使得代码更加清晰易读和容易维护。

这种方式带来了编程的弹性，例如现在有新的统计需求，需要根据用户名称进行排序，那么我们只需要新增一个按名称排序的类型，然后在这个类型上定义操作就可以了：
```golang
// 名字排序
type sortName Team

func (t sortName) Len() int {
	return len(t)
}
func (t sortName) Swap(i, j int) {
	t[i], t[j] = t[j], t[i]
}
func (t sortName) Less(i, j int) bool {
	return t[i].name < t[j].name
}
```
还是从Team生成一个新的类型sortName，然后这个类型有对应的方法进行处理。

Go语言通过type关键字很好地揭示了事物的本质，就是同一个事物可以有完全不同的一系列行为特征以至于它们表现像是不同的事物；而事物表象上的一系列行为特征的独立，对写出清晰易读易维护的代码提供了很好的帮助。Go语言这种对事物本质的透彻理解和把握，在C/C++语言里是看不到的。




func的定义为：
```golang
func (p myType ) funcName ( a, b int , c string ) ( r , s int ) {
    return
}
```
还需要区分出goalng的方法和函数。他们之间虽然都是通过func定义的，但还是有区别的。
为特定类型定义函数，即为类型对象定义方法：
在Go中通过给函数标明所属类型，来给该类型定义方法，上面的 p myType 即表示给myType声明了一个方法， p myType 不是必须的。如果没有，则纯粹是一个函数，通过包名称访问。



## golang关键字interface：
Golang 中没有 class 的概念，而是通过 interface 类型转换支持在动态类型语言中常见的 鸭子类型 达到运行时多态的效果。
官方对于interface的定义为：
```text
An interface type specifies a method set called its interface. A variable of interface type can store a value of any type with a method set that is any superset of the interface. Such a type is said to implement the interface. The value of an uninitialized variable of interface type is nil.
```
一个 interface 类型定义了一个方法集，称为接口。




参考文档：
[Understanding Golang Type System](https://thenewstack.io/understanding-golang-type-system/)
[Golang 语言基础之八： interface](http://xhrwang.me/2014/12/29/golang-fundamentals-8-interface.html)
[关于golang struct interface的理解使用](http://xiaorui.cc/2016/03/11/%E5%85%B3%E4%BA%8Egolang-struct-interface%E7%9A%84%E7%90%86%E8%A7%A3%E4%BD%BF%E7%94%A8/)
[一人千面：谈谈Go语言中的type](http://blog.sina.com.cn/s/blog_9be3b8f10101ccpu.html)

[]()
[]()
[]()
[]()