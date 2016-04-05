#关于Rust的基本语法和语义学习：

##1. 变量绑定（variable-bindings）:
使用let来声明一个绑定，将一些值绑定在一些变量上，例如：
```rust
fn main(){
    let x = 5i;
    println!("the number is:{}",x);
}
```
##2. 模式（patterns）：
模式是从函数式编程语言中引入过来的概念，但是和函数式编程并没有什么关系，只是因为最早出现在函数式程序设计语言中而已。
所谓的模式匹配需要拆分为模式和匹配，其中的：
（1）模式是指数据结构上的，用模式描述这个数据结构的组成。模式可以当作对某个类型，其内部数据在结构上抽象出来的表达式。模式也不只是表示某种结构，还可以表示常量或者类型。
（2）匹配就是建立在上述模式上的检查，表示匹配变量是否符合这种模式。
所以按照上述理解，正则表达式中的模式匹配用来检查特定的字符类型是否满足要求，lisp中的模式匹配用来检查列表结构是否满足要求，都是一种具体的模式匹配方式。

[理解模式匹配需要分为三个部分](http://stackoverflow.com/questions/2502354/what-is-pattern-matching-in-functional-languages)，现在自己翻译如下：

* (1) 代数数据类型;
* (2) 什么是模式匹配;
* (3) 为什么模式匹配非常好用;

### （1）简单介绍代数数据类型：
ML类型的函数式程序设计语言中，运行用户定义叫做“disjoint unions”或者“algebraic data types”简单的数据类型。这些数据结构是简单的容器，并且可以递归定义。例如：

```ML
type 'a list =
    | Nil
    | Cons of 'a * 'a list
```
同样的使用C#定义一个堆栈式（stack-like）的数据结构为：

```C#
public abstract class List<T>
{
    public class Nil : List<T> { }
    public class Cons : List<T>
    {
        public readonly T Item1;
        public readonly List<T> Item2;
        public Cons(T item1, List<T> item2)
        {
            this.Item1 = item1;
            this.Item2 = item2;
        }
    }
}
```
其中的Nil和Cons标识符定义了简单的数据类型；类似于x*y*z*...的形式定义了一种构造函数或者数据类型，这个构造函数的参数并没有给出，而是通过参数的位置和数据类型来决定。

根据上述ML和C#我们可以构造一个list的实例为：

```ML
let x = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
```

```C#
Stack<int> x = new Cons(1, new Cons(2, new Cons(3, new Cons(4, new Nil()))));
```

### （2）简单介绍模式匹配：
模式匹配就像是一种类型测试。我们就用上述ML和C#创建的list对象为例，实现以下peek和pop方法来说明模式匹配。

```ML
let peek s =
    match s with
    | Cons(hd, tl) -> hd
    | Nil -> failwith "Empty stack"

let pop s =
    match s with
    | Cons(hd, tl) -> tl
    | Nil -> failwith "Empty stack"
```
同样的功能，在stack-like对象的C#上实现如下：	

```C#
public static T Peek<T>(Stack<T> s)
{
    if (s is Stack<T>.Cons)
    {
        T hd = ((Stack<T>.Cons)s).Item1;
        Stack<T> tl = ((Stack<T>.Cons)s).Item2;
        return hd;
    }
    else if (s is Stack<T>.Nil)
        throw new Exception("Empty stack");
    else
        throw new MatchFailureException();
}

public static Stack<T> Pop<T>(Stack<T> s)
{
    if (s is Stack<T>.Cons)
    {
        T hd = ((Stack<T>.Cons)s).Item1;
        Stack<T> tl = ((Stack<T>.Cons)s).Item2;
        return tl;
    }
    else if (s is Stack<T>.Nil)
        throw new Exception("Empty stack");
    else
        throw new MatchFailureException();
}
```
