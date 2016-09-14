# 关于java中回调机制和注册的实现：

在实现多种接口的具体实现和interface的接口调用的过程中，随着代码的编写，想实现类似于Spring中注册机制，Spring基于的IOC是其核心。
本文分析java的接口回调机制细节，然后结合Spring的IOC原理进行思考。

## 一 java中的接口回调机制

### 1.1 回调的概念：
模块之间都存在一定的调用关系，从调用方式上看，可以分为三类：同步调用、异步调用和回调。
- 同步调用是一种阻塞式调用，即在函数A的函数体里通过书写函数B的函数名来调用之，使内存中对应函数B的代码得以执行。
- 异步调用是一种类似消息或事件的机制解决了同步阻塞的问题，例如 A通知 B后，他们各走各的路，互不影响，不用像同步调用那样， A通知 B后，非得等到 B走完后， A才继续走 。
- 回调是一种双向的调用模式，也就是说，被调用的接口被调用时也会调用对方的接口，例如A要调用B，B在执行完又要调用A。

回调也包含为同步回调和异步回调：
- 异步回调一般使用多线程来进行，被回调的部分包含在线程中，在后台执行；
- 同步回调会一直等待被调用的返回结果，获取返回值之后再进行当前的程序。

### 1.2 回调的用处：
回调一般用于层间协作，上层将本层函数安装在下层，这个函数就是回调，而下层在一定条件下触发回调。例如作为一个驱动，是一个底层，他在收到一个数据时，除了完成本层的处理工作外，还将进行回调，将这个数据交给上层应用层来做进一步处理，这在分层的数据通信中很普遍。

### 1.3 java中的接口回调：
在C/C++中，要实现回调函数，被调用函数要告诉调用者自己的指针地址。但是Java没有指针地址，不能传递方法的地址，一般采用接口回调的方法来实现：
- 把实现某一接口的类创建的对象的引用赋给该接口声明的接口变量，那么该接口变量就可以调用被调用类实现的接口的方法。
> 原理：首先创建一个回调对象，然后再创建一个控制器对象，将回调对象需要被调用的方法告诉控制器对象，控制器对象负责检查某个场景是否出现或某个条件是否满足，当满足时，自动调用回调对象的方法。

也就是说回调的双方需要协商好一个交互的接口，然后通过这个接口来实现相互之间的调用：
- 调用方：需要一个可以处理这个接口的函数；
- 被调用方：需要继承这个接口，然后在接口中实现处理方法。

### 1.4 基本回调的java实现样例代码：
首先定义一个接口：
```java
public interface CallBack {
    public void doEvent();
}
 ```
然后定义调用方，也就是所谓的控制类，主导什么时候进行回调：
```java
public class Master {
    CallBack callBack;
    public Master(CallBack callBack) {
        this.callBack=callBack;
    }
    public void process() {
        System.out.println("主程序执行中");
        callBack.doEvent();
    }
}
```
接着定义被回调的类，用于响应回调发生的处理事件：
```java
public class Slave implements CallBack {
    @Override
    public void doEvent() {
        System.out.println("告知已经完成工作了");
    }
}
```
这个被调用的类继承了协商好的接口，然后在其中实现被调用的方法，也就是接口中的doEvent方法。
最后就是完成整个回调的调用流程：
```java
public static void main(String[] args) {
    // 创建回调对象实例
    Slave slave = new Slave(); 
    //创建控制器对象，将提供给他的回调对象传入
    Master master = new Master(slave);
    //启动控制器对象运行
    master.process();
}
```

### 1.5 异步回调的java实现样例代码：
在[上一节](### 1.4)中展示了基本的java中的接口回调方式，但是上述调用是同步的，控制器中需要等待回调对象中处理函数的返回。
但是在实际使用中，往往需要进行异步调用来继续执行控制器中的函数，直到回调对象返回处理结果通知控制器。本节主要展示异步的java代码。
主要修改控制类代码完成异步调用：
```java
public class Master {
    CallBack callBack;
    public Master(CallBack callBack) {
        this.callBack=callBack;
    }
    
    // 使用thread方式来完成异步调用封装，然后继续调用处理函数
    public void asynchronousCall(){
        // 开始请求异步调用
        System.out.println("准备开始异步调用");

        //这里用一个线程就是异步，
        new Thread(new Runnable() {
            @Override
            public void run() {
                callBack.doEvent();
            }
        }).start();

        // 然后继续执行当前主程序，等待回调通知
        System.out.println("完成异步调用，继续执行当前控制类代码");
        process();
    }

    // 异步调用之后的控制程序需要继续执行的代码
    public void process() {
        System.out.println("控制类继续执行中");
    }
}
```
其他的接口和实现都不变。这儿主要使用了new Thread来启动一个线程来进行异步调用，也可以使用其他方式来进行。
为了将整个过程模拟，改变被回调的类的定义，加入延时：
```java
public class Slave implements CallBack {
    @Override
    public void doEvent() {
        // 进入回调处理
        System.out.println("进入回调调用");

        // 模拟雕用处理需要很长时间
        for(int i=0; i<10000;i++){
            System.out.println("slave process : "+i);
            //int j = i+1;
        }
        // 然后返回结果
        System.out.println("回调执行完毕");
    }
}
```
然后在主程序中执行调用：
```java
public static void main(String[] args) {
    // 创建回调对象实例
    Slave slave = new Slave();
    //创建控制器对象，将提供给他的回调对象传入
    Master master = new Master(slave);
    //启动控制器对象运行
    master.asynchronousCall();
}
```
会返回如下执行结果：
```shell
准备开始异步调用
进入回调调用
slave process : 0
完成异步调用，继续执行当前控制类代码
slave process : 1
slave process : 2
控制类继续执行中
slave process : 3
slave process : 4
slave process : 5
slave process : 6
slave process : 7
...
slave process : 9999
回调执行完毕
```
可见当前的执行顺序为master在执行异步调用后，继续执行后续的process程序，然后异步处理的doEvent在后台进行处理，然后返回结果。



## 二 Spring的IOC机制：
