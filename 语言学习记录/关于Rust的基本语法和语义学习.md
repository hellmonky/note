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
ML实现为：
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

根据上述ML和C#我们可以构造一个list的实例为，ML实现为：
```ML
let x = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
```
C#实现为：
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
ML语言实现模式匹配并没有运行时的类型检查和类型转换，而C#的代码有，所以看起来有些复杂。

### （3）数据结构分解：
首先看上述代码中peek方法：
```ML
let peek s =
    match s with
    | Cons(hd, tl) -> hd
    | Nil -> failwith "Empty stack"
```
关键就在于理解其中的hd和tl描述符是变量（其实，他们不可以改变，所以并不是真正的变量，更像是值）。如果s具有Cons类型，那么我们就可以从结构中拉取出值，并且将他们绑定到名为hd和tl的变量上。
模式匹配的用途就在于它可以让我们通过结构来分解一个数据结构，而不需要依赖于它的内容。
考虑我们定义一个二叉树：
```ML
type 'a tree =
    | Node of 'a tree * 'a * 'a tree
    | Nil
```
那么我们可以在这个二叉树基础上定义一些树的旋转操作：
```ML
let rotateLeft = function
    | Node(a, p, Node(b, q, c)) -> Node(Node(a, p, b), q, c)
    | x -> x

let rotateRight = function
    | Node(Node(a, p, b), q, c) -> Node(a, p, Node(b, q, c))
    | x -> x
```
其中的：
```ML
let rotateRight = function
```
结构是:
```ML
let rotateRight s = match s with ...
```
的语法糖。

所以利用模式匹配，我们不仅可以绑定结构到变量上，还可以深入到数据结构之中。例如，我们有一个节点x：
```ML
let x = Node(Nil, 1, Nil)
```
如果我们调用rotateLeft x，程序的执行如下：
（1）首先用第一个模式对x进行测试，这个测试会因为右边的子节点类型为Nil而不是Node从而导致错误；
（2）然后继续下一个模式的测试，x->x，这个模式会匹配任何的输入，然后无修改的返回作为输出；

为了进行比较，上述函数用C#重写如下：
```C#
public abstract class Tree<T>
{
    public abstract U Match<U>(Func<U> nilFunc, Func<Tree<T>, T, Tree<T>, U> nodeFunc);

    public class Nil : Tree<T>
    {
        public override U Match<U>(Func<U> nilFunc, Func<Tree<T>, T, Tree<T>, U> nodeFunc)
        {
            return nilFunc();
        }
    }

    public class Node : Tree<T>
    {
        readonly Tree<T> Left;
        readonly T Value;
        readonly Tree<T> Right;

        public Node(Tree<T> left, T value, Tree<T> right)
        {
            this.Left = left;
            this.Value = value;
            this.Right = right;
        }

        public override U Match<U>(Func<U> nilFunc, Func<Tree<T>, T, Tree<T>, U> nodeFunc)
        {
            return nodeFunc(Left, Value, Right);
        }
    }

    public static Tree<T> RotateLeft(Tree<T> t)
    {
        return t.Match(
            () => t,
            (l, x, r) => r.Match(
                () => t,
                (rl, rx, rr) => new Node(new Node(l, x, rl), rx, rr))));
    }

    public static Tree<T> RotateRight(Tree<T> t)
    {
        return t.Match(
            () => t,
            (l, x, r) => l.Match(
                () => t,
                (ll, lx, lr) => new Node(ll, lx, new Node(lr, x, r))));
    }
}	
```

### （3）为什么模式匹配非常好用：
在C#中可以用[visitor pattern](http://stackoverflow.com/questions/694651/what-task-is-best-done-in-a-functional-programming-style/694822#694822)来实现和模式匹配一样的效果，但是这种实现并不是非常的灵活，因为无法有效的解析复杂的数据结构。并且使用模式匹配可以得到编译器的检查和支持，例如会告诉你左边的条件非法等。
所以模式匹配可以非常方便的帮助你解析和引导数据结构，并且提供了统一的语法和编译器的逻辑检查，这就是模式匹配为什么好用的原因。

## 类型注解（type annotations）：
Rust是一个静态类型语言，需要确定变量的类型才能通过编译，而不是像Lisp或者javascript一样是动态类型的。
但是Rust有类型推导的功能，如果他能够确定当前变量的类型就不需要明确的写入，当然也可以明确的加上类型，语法为：
```Rust
let x: i32 = 5;
```
其中类型写在变量后面，用冒号表示出来。上述代码的意思是：“x被绑定为i32类型，它的值是5”。

