### Spring框架基本原理和使用解析：
Spring作为javaEE的开发框架被广泛的使用，那么作为一个java开发者，如果使用Spring，最好彻底的理解这个框架的基本原理，这样才能方便自己根据实际需求定制开发。

<!-- TOC -->

- [Spring框架基本原理和使用解析：](#spring%E6%A1%86%E6%9E%B6%E5%9F%BA%E6%9C%AC%E5%8E%9F%E7%90%86%E5%92%8C%E4%BD%BF%E7%94%A8%E8%A7%A3%E6%9E%90)
    - [IoC：](#ioc)
    - [AOP：](#aop)
        - [通过实例理解AOP：](#%E9%80%9A%E8%BF%87%E5%AE%9E%E4%BE%8B%E7%90%86%E8%A7%A3aop)
            - [（1）一个坏的例子：](#1%E4%B8%80%E4%B8%AA%E5%9D%8F%E7%9A%84%E4%BE%8B%E5%AD%90)
            - [（2）方案一：静态代理](#2%E6%96%B9%E6%A1%88%E4%B8%80%E9%9D%99%E6%80%81%E4%BB%A3%E7%90%86)
            - [（3）方案二：动态代理](#3%E6%96%B9%E6%A1%88%E4%BA%8C%E5%8A%A8%E6%80%81%E4%BB%A3%E7%90%86)
            - [（4）方案三：使用cglib动态代理](#4%E6%96%B9%E6%A1%88%E4%B8%89%E4%BD%BF%E7%94%A8cglib%E5%8A%A8%E6%80%81%E4%BB%A3%E7%90%86)
        - [Spring中的AOP应用：](#spring%E4%B8%AD%E7%9A%84aop%E5%BA%94%E7%94%A8)
            - [（1）前置增强、后置增强和环绕增强的基本用法样例：](#1%E5%89%8D%E7%BD%AE%E5%A2%9E%E5%BC%BA%E5%90%8E%E7%BD%AE%E5%A2%9E%E5%BC%BA%E5%92%8C%E7%8E%AF%E7%BB%95%E5%A2%9E%E5%BC%BA%E7%9A%84%E5%9F%BA%E6%9C%AC%E7%94%A8%E6%B3%95%E6%A0%B7%E4%BE%8B)
            - [（2）使用Spring配置文件来完成增强：](#2%E4%BD%BF%E7%94%A8spring%E9%85%8D%E7%BD%AE%E6%96%87%E4%BB%B6%E6%9D%A5%E5%AE%8C%E6%88%90%E5%A2%9E%E5%BC%BA)
            - [（3）Spring AOP的应用：抛出增强](#3spring-aop%E7%9A%84%E5%BA%94%E7%94%A8%E6%8A%9B%E5%87%BA%E5%A2%9E%E5%BC%BA)
            - [（4）Spring AOP的应用：对类的增强](#4spring-aop%E7%9A%84%E5%BA%94%E7%94%A8%E5%AF%B9%E7%B1%BB%E7%9A%84%E5%A2%9E%E5%BC%BA)
        - [SpringBoot中的AOP应用：](#springboot%E4%B8%AD%E7%9A%84aop%E5%BA%94%E7%94%A8)
            - [（1）引入SpringBoot工程的AOP支持组件：](#1%E5%BC%95%E5%85%A5springboot%E5%B7%A5%E7%A8%8B%E7%9A%84aop%E6%94%AF%E6%8C%81%E7%BB%84%E4%BB%B6)
            - [（2）实现Web层的日志切面测试：](#2%E5%AE%9E%E7%8E%B0web%E5%B1%82%E7%9A%84%E6%97%A5%E5%BF%97%E5%88%87%E9%9D%A2%E6%B5%8B%E8%AF%95)
            - [（3）关于Spring中使用AOP的一些优化：](#3%E5%85%B3%E4%BA%8Espring%E4%B8%AD%E4%BD%BF%E7%94%A8aop%E7%9A%84%E4%B8%80%E4%BA%9B%E4%BC%98%E5%8C%96)
            - [（4）Spring中多个切面的合并：](#4spring%E4%B8%AD%E5%A4%9A%E4%B8%AA%E5%88%87%E9%9D%A2%E7%9A%84%E5%90%88%E5%B9%B6)
            - [（5）切面中获取目标方法的信息：](#5%E5%88%87%E9%9D%A2%E4%B8%AD%E8%8E%B7%E5%8F%96%E7%9B%AE%E6%A0%87%E6%96%B9%E6%B3%95%E7%9A%84%E4%BF%A1%E6%81%AF)
        - [Spring中使用AOP的注解和语法汇总说明：](#spring%E4%B8%AD%E4%BD%BF%E7%94%A8aop%E7%9A%84%E6%B3%A8%E8%A7%A3%E5%92%8C%E8%AF%AD%E6%B3%95%E6%B1%87%E6%80%BB%E8%AF%B4%E6%98%8E)
            - [（1）涉及到的注解：](#1%E6%B6%89%E5%8F%8A%E5%88%B0%E7%9A%84%E6%B3%A8%E8%A7%A3)
            - [（2）重点注解的用法：](#2%E9%87%8D%E7%82%B9%E6%B3%A8%E8%A7%A3%E7%9A%84%E7%94%A8%E6%B3%95)
            - [（3）通知参数：](#3%E9%80%9A%E7%9F%A5%E5%8F%82%E6%95%B0)
            - [（4）切入点定义：](#4%E5%88%87%E5%85%A5%E7%82%B9%E5%AE%9A%E4%B9%89)

<!-- /TOC -->

#### IoC：


#### AOP：
AOP（Aspect Oriented Programming），也就是面向方面编程，作为面向对象编程的一种补充，专门用于处理系统中分布于各个模块（不同方法）中的交叉关注点的问题。
面向切面编程作为对面向对象思想的一种补充，原因在于：
[什么是面向切面编程AOP？](https://www.zhihu.com/question/24863332)
```text
面向对象的特点是继承、多态和封装。而封装就要求将功能分散到不同的对象中去，这在软件设计中往往称为职责分配。实际上也就是说，让不同的类设计不同的方法。这样代码就分散到一个个的类中去了。这样做的好处是降低了代码的复杂程度，使类可重用。
但是人们也发现，在分散代码的同时，也增加了代码的重复性。什么意思呢？比如说，我们在两个类中，可能都需要在每个方法中做日志。按面向对象的设计方法，我们就必须在两个类的方法中都加入日志的内容。也许他们是完全相同的，但就是因为面向对象的设计让类与类之间无法联系，而不能将这些重复的代码统一起来。
也许有人会说，那好办啊，我们可以将这段代码写在一个独立的类独立的方法里，然后再在这两个类中调用。但是，这样一来，这两个类跟我们上面提到的独立的类就有耦合了，它的改变会影响这两个类。那么，有没有什么办法，能让我们在需要的时候，随意地加入代码呢？**这种在运行时，动态地将代码切入到类的指定方法、指定位置上的编程思想就是面向切面的编程。** 
一般而言，我们管切入到指定类指定方法的代码片段称为切面，而切入到哪些类、哪些方法则叫切入点。有了AOP，我们就可以把几个类共有的代码，抽取到一个切片中，等到需要时再切入对象中去，从而改变其原有的行为。
这样看来，AOP其实只是OOP的补充而已。OOP从横向上区分出一个个的类来，而AOP则从纵向上向对象中加入特定的代码。有了AOP，OOP变得立体了。如果加上时间维度，AOP使OOP由原来的二维变为三维了，由平面变成立体了。从技术上来说，AOP基本上是通过代理机制实现的。 
AOP在编程历史上可以说是里程碑式的，对OOP编程是一种十分有益的补充。
```

整章参考：
> - [AOP那点事儿：面向切面编程](http://developer.51cto.com/art/201309/410861_all.htm)
> - [AOP 那点事儿](https://my.oschina.net/huangyong/blog/161338)
> - [AOP 那点事儿（续集）](https://my.oschina.net/huangyong/blog/161402)

##### 通过实例理解AOP：
我们可以看一些实际的例子来帮助理解为什么要使用AOP：
###### （1）一个坏的例子：
先定义一个接口，用于描述要实现的功能的特性：

```java
public interface Greeting {  
 
    void sayHello(String name);  
} 
```
然后实现这个接口，完成具体的功能：

```java
public class GreetingImpl implements Greeting {  
 
    @Override 
    public void sayHello(String name) {  
        before();  
        System.out.println("Hello! " + name);  
        after();  
    }  
 
    private void before() {  
        System.out.println("Before");  
    }  
 
    private void after() {  
        System.out.println("After");  
    }  
} 
```
在实现的时候，为了完成接口定义的sayHello的功能，需要内部的两个方法来帮助实现。但是before() 与 after() 方法写死在 sayHello() 方法体中了，这样的代码的味道非常不好。
这种场景是非常常见的，例如：
我们要统计每个方法的执行时间，以对性能作出评估，那是不是要在每个方法的一头一尾都做点手脚呢？
我们要写一个 JDBC 程序，那是不是也要在方法的开头去连接数据库，方法的末尾去关闭数据库连接呢？
如果都按照这个样子去进行编写，工作量会非常大，并且代码的维护需要对分散在各个实现中的类来进行。那么如何对这种情况给出比较良好的方案实现去耦合？

###### （2）方案一：静态代理
最简单的解决方案就是使用静态代理模式了，我们单独为 GreetingImpl 这个类写一个代理类，这个代理也实现接口：

```java
public class GreetingProxy implements Greeting {  
 
    private GreetingImpl greetingImpl;  
 
    public GreetingProxy(GreetingImpl greetingImpl) {  
        this.greetingImpl = greetingImpl;  
    }  
 
    @Override 
    public void sayHello(String name) {  
        before();  
        greetingImpl.sayHello(name);  
        after();  
    }  
 
    private void before() {  
        System.out.println("Before");  
    }  
 
    private void after() {  
        System.out.println("After");  
    }  
} 
```
然后完成一个接口的实现，这个实现只关心接口定义的具体功能的填充：

```java
public class GreetingImpl implements Greeting {  
    @Override 
    public void sayHello(String name) {  
        System.out.println("Hello! " + name);  
    }  
} 
```
最后，通过组合具体实现的方式来减少实现中的代码，客户端调用方法为：

```java
public class Client {  
 
    public static void main(String[] args) {  
        Greeting greetingProxy = new GreetingProxy(new GreetingImpl());  
        greetingProxy.sayHello("Jack");  
    }  
} 
```
这样就可以将一个接口的实现，通过对应的代理完成环境的设置了，这个接口关注于具体的业务代码。
但是这种方式还是存在问题的：XxxProxy 这样的类会越来越多，而且，所有的代理操作除了调用的方法不一样之外，其他的操作都一样，则此时肯定是重复代码。
如何才能将这些代理类尽可能减少呢？
最好只有一个代理类。借用JDK提供的动态代理来完成。

###### （3）方案二：动态代理
与静态代理类对照的是动态代理类，动态代理类的字节码在程序运行时由Java反射机制动态生成，无需程序员手工编写它的源代码。动态代理类不仅简化了编程工作，而且提高了软件系统的可扩展性，因为Java 反射机制可以生成任意类型的动态代理类。java.lang.reflect 包中的Proxy类和InvocationHandler 接口提供了生成动态代理类的能力。 
JDK动态代理中包含一个类和一个接口： 
<1> InvocationHandler接口： 

```java
public interface InvocationHandler { 
    public Object invoke(Object proxy,Method method,Object[] args) throws Throwable; 
} 
```
参数说明： 
> - Object proxy：指被代理的对象。 
> - Method method：要调用的方法 
> - Object[] args：方法调用时所需要的参数 

可以将InvocationHandler接口的子类想象成一个代理的最终操作类，替换掉ProxySubject。 

<2> Proxy类： Proxy类是专门完成代理的操作类，可以通过此类为一个或多个接口动态地生成实现类，此类提供了如下的操作方法： 

```java
public static Object newProxyInstance(ClassLoader loader, Class<?>[] interfaces,  InvocationHandler h)  throws IllegalArgumentException 
```
参数说明： 
> - ClassLoader loader：类加载器 
> - Class<?>[] interfaces：得到全部的接口 
> - InvocationHandler h：得到InvocationHandler接口的子类实例 


还是通过实现Greeting接口的实例进行演示：

```java
public class GreetingImpl implements Greeting {  
    @Override 
    public void sayHello(String name) {  
        System.out.println("Hello! " + name);  
    }  
} 
```
然后使用JDK的动态代理来改写上一个方案中的静态代理：

```java
import java.lang.reflect.InvocationHandler;

public class JDKDynamicProxy implements InvocationHandler {  
 
    private Object target;  
 
    public JDKDynamicProxy(Object target) {  
        this.target = target;  
    }  
 
    @SuppressWarnings("unchecked")  
    public <T> T getProxy() {  
        return (T) Proxy.newProxyInstance(  
            target.getClass().getClassLoader(),  
            target.getClass().getInterfaces(),  //这里要绑定接口(这是一个缺陷，cglib弥补了这一缺陷)  
            this 
        );  
    }  
 
    @Override 
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {  
        before();  
        Object result = method.invoke(target, args);  
        after();  
        return result;  
    }  
 
    private void before() {  
        System.out.println("Before");  
    }  
 
    private void after() {  
        System.out.println("After");  
    }  
} 
```
然后客户端的调用方法为：

```java
public class Client {  
    
    public static void main(String[] args) {  
        JDKDynamicProxy proxy = new JDKDynamicProxy(new GreetingImpl());
        Greeting greeting = (Greeting) proxy.getProxy();
        greeting.sayHello("Jack");  
    }  
} 
```
通过这个JDKDynamicProxy，所有的代理类都合并到动态代理类中了，只需要将不同的接口实现传入就能生成对应的代理。
但这样做仍然存在一个问题：JDK 给我们提供的动态代理只能代理接口，而不能代理没有接口的类。有什么方法可以解决呢？

###### （4）方案三：使用cglib动态代理
JDK的动态代理机制只能代理实现了接口的类，而不能实现接口的类就不能实现JDK的动态代理，cglib是针对类来实现代理的，他的原理是对指定的目标类生成一个子类，并覆盖其中方法实现增强，但因为采用的是继承，所以不能对final修饰的类进行代理。 
还是继续实现Greeting接口：

```java
import java.lang.reflect.Method;  
  
import net.sf.cglib.proxy.Enhancer;  
import net.sf.cglib.proxy.MethodInterceptor;  
import net.sf.cglib.proxy.MethodProxy;  

public class CGLibDynamicProxy implements MethodInterceptor {  
 
    private static CGLibDynamicProxy instance = new CGLibDynamicProxy();  
 
    private CGLibDynamicProxy() {  
    }  
 
    public static CGLibDynamicProxy getInstance() {  
        return instance;  
    }  
 
    @SuppressWarnings("unchecked")  
    public <T> T getProxy(Class<T> cls) {  
        return (T) Enhancer.create(cls, this);  
    }  
 
    // 回调用方法
    @Override 
    public Object intercept(Object target, Method method, Object[] args, MethodProxy proxy) throws Throwable {  
        before();  
        Object result = proxy.invokeSuper(target, args);  
        after();  
        return result;  
    }  
 
    private void before() {  
        System.out.println("Before");  
    }  
 
    private void after() {  
        System.out.println("After");  
    }  
} 
```
上述代码就需要引入cglib包来实现，并且是一个单例模式，所以客户端的调用为：

```java
public class Client {  
    public static void main(String[] args) { 
        CGLibDynamicProxy cglibProxy = CGLibDynamicProxy.getInstance();
        Greeting greeting = cglibProxy.getProxy(GreetingImpl.class);  
        greeting.sayHello("Jack");  
    }  
} 
```
其中的GreetingImpl还是和方案二中的实现一样，只关注于Greeting接口的业务逻辑实现。
到目前位置，问题似乎全部都解决了。但是真的是这样吗？

##### Spring中的AOP应用：
Spring的基石即使IoC和AOP，所以让我们看看Spring中的AOP到底是为了解决什么问题提出的，为什么不直接使用cglib来完成动态代理？
上面例子中提到的 before() 方法，在 Spring AOP 里就叫 Before Advice（前置增强）。像 after() 这样的方法就叫 After Advice（后置增强），因为它放在后面来增强代码的功能。如果能把 before() 与 after() 合并在一起，那就叫 Around Advice（环绕增强）。
回顾上面的Greeting接口，在代理中，我们需要编写具体的before和after函数来完成环境的设置，那么既然这个是环境，那么就不应该固化在代码中完成硬编码，而是需要通过外部环境来动态确定，那么我们也通过组合来将这些容易变化的部分抽取出来。通过Spring来看看怎么编写相关的代码：

###### （1）前置增强、后置增强和环绕增强的基本用法样例：
还是用之前的Greeting的例子，但是使用Spring的AOP包来进行处理。
先来一个前置增强类吧：

```java
import java.lang.reflect.Method;    
import org.springframework.aop.MethodBeforeAdvice;  

public class GreetingBeforeAdvice implements MethodBeforeAdvice {  
    @Override 
    public void before(Method method, Object[] args, Object target) throws Throwable {  
        System.out.println("Before");  
    }  
} 
```
这个类完成了对before函数的剥离。

再来一个后置增强类吧：

```java
import java.lang.reflect.Method;    
import org.springframework.aop.AfterReturningAdvice;

public class GreetingAfterAdvice implements AfterReturningAdvice {  
 
    @Override 
    public void afterReturning(Object result, Method method, Object[] args, Object target) throws Throwable {  
        System.out.println("After");  
    }  
} 
```
这个类完成对after函数的剥离。

现在看看客户调用应该是什么样子：

```java
import org.springframework.aop.framework.ProxyFactory;

public class Client {  
 
    public static void main(String[] args) {  
        ProxyFactory proxyFactory = new ProxyFactory();     // 利用spring的API,创建代理工厂 
        proxyFactory.setTarget(new GreetingImpl());         // 射入目标类对象  
        proxyFactory.addAdvice(new GreetingBeforeAdvice()); // 添加前置增强  
        proxyFactory.addAdvice(new GreetingAfterAdvice());  // 添加后置增强   
 
        Greeting greeting = (Greeting) proxyFactory.getProxy(); // 从代理工厂中获取代理  
        greeting.sayHello("Jack");                              // 调用代理的方法  
    }  
} 
```
其中的ProxyFactory就是Spring的AOP组件提供的代理工厂。

当然，我们完全可以只定义一个增强类，让它同时实现 MethodBeforeAdvice 与 AfterReturningAdvice 这两个接口，如下：

```java
import java.lang.reflect.Method;    
import org.springframework.aop.MethodBeforeAdvice;
import org.springframework.aop.AfterReturningAdvice;  

public class GreetingBeforeAndAfterAdvice implements MethodBeforeAdvice, AfterReturningAdvice {  
 
    @Override 
    public void before(Method method, Object[] args, Object target) throws Throwable {  
        System.out.println("Before");  
    }  
 
    @Override 
    public void afterReturning(Object result, Method method, Object[] args, Object target) throws Throwable {  
        System.out.println("After");  
    }  
} 
```
这样我们只需要使用一行代码，同时就可以添加前置与后置增强：

```java
import org.springframework.aop.framework.ProxyFactory;

public class Client {  
 
    public static void main(String[] args) {  
        ProxyFactory proxyFactory = new ProxyFactory();             // 利用spring的API,创建代理工厂 
        proxyFactory.setTarget(new GreetingImpl());                 // 射入目标类对象  
        proxyFactory.addAdvice(new GreetingBeforeAndAfterAdvice()); // 添加合并的增强 
 
        Greeting greeting = (Greeting) proxyFactory.getProxy();     // 从代理工厂中获取代理  
        greeting.sayHello("Jack");                                  // 调用代理的方法  
    }  
} 
```
刚才有提到“环绕增强”，其实这个东西可以把“前置增强”与“后置增强”的功能给合并起来，无需让我们同时实现以上两个接口：

```java
import java.lang.reflect.Method;    
import org.aopalliance.intercept.MethodInterceptor;

public class GreetingAroundAdvice implements MethodInterceptor {  
 
    @Override 
    public Object invoke(MethodInvocation invocation) throws Throwable {  
        before();  
        Object result = invocation.proceed();  
        after();  
        return result;  
    }  
 
    private void before() {  
        System.out.println("Before");  
    }  
 
    private void after() {  
        System.out.println("After");  
    }  
} 
```
环绕增强类需要实现 org.aopalliance.intercept.MethodInterceptor 接口。注意，这个接口不是 Spring 提供的，它是 AOP 联盟写的，Spring 只是借用了它。
这个时候的客户端还是一样的调用方式。

###### （2）使用Spring配置文件来完成增强：
刚刚的用法只是一个简单的库的使用方法举例，并不是Spring AOP应该有的用法。Spring强调通过配置来完成自动化，通过xml配置文件来解耦代码。
看一下一个具体的配置：

```xml
<?xml version="1.0" encoding="UTF-8"?> 
<beans xmlns="http://www.springframework.org/schema/beans" 
       xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
       xmlns:context="http://www.springframework.org/schema/context" 
       xsi:schemaLocation="http://www.springframework.org/schema/beans  
       http://www.springframework.org/schema/beans/spring-beans.xsd  
       http://www.springframework.org/schema/context  
       http://www.springframework.org/schema/context/spring-context.xsd"> 
 
    <!-- 扫描指定包（将 @Component 注解的类自动定义为 Spring Bean），这儿是aop.demo包下 --> 
    <context:component-scan base-package="aop.demo"/> 
 
    <!-- 配置一个代理 --> 
    <bean id="greetingProxy" class="org.springframework.aop.framework.ProxyFactoryBean"> 
        <property name="interfaces" value="aop.Greeting"/> <!-- 需要代理的接口 --> 
        <property name="target" ref="greetingImpl"/>       <!-- 接口实现类 --> 
        <property name="interceptorNames">                 <!-- 拦截器名称（也就是增强类名称，Spring Bean 的 id） --> 
            <list> 
                <value>greetingAroundAdvice</value> 
            </list> 
        </property> 
    </bean> 
 
</beans> 
```
使用 ProxyFactoryBean 就可以取代前面的 ProxyFactory，其实它们俩就一回事儿。其中 interceptorNames 应该改名为 adviceNames 或许会更容易让人理解，作用都是这个属性里面添加增强类。
此外，如果只有一个增强类，可以使用以下方法来简化：

```xml
    <bean id="greetingProxy" class="org.springframework.aop.framework.ProxyFactoryBean"> 
        <property name="interfaces" value="aop.Greeting"/> 
        <property name="target" ref="greetingImpl"/> 
        <property name="interceptorNames" value="greetingAroundAdvice"/> <!-- 变为：name-value 这样的键值对来表明只有一个 --> 
    </bean> 
```
这里使用了 Spring 2.5+ 的特性“Bean 扫描”，这样我们就无需在 Spring 配置文件里不断地定义 <bean id="xxx" class="xxx"/> 了，从而简化了代码编写的工作量。
通过上述配置，现在的代码就可以简化写了。

```java
import org.springframework.stereotype.Component;

@Component 
public class GreetingImpl implements Greeting {  
    @Override 
    public void sayHello(String name) {  
        System.out.println("Hello! " + name);  
    }  
}
```
表明这个类被自动添加到扫描中。
然后环绕增强的代码也添加到自动扫描中：

```java
import org.springframework.stereotype.Component;
import java.lang.reflect.Method;    
import org.aopalliance.intercept.MethodInterceptor;

@Component 
public class GreetingAroundAdvice implements MethodInterceptor {  
 
    @Override 
    public Object invoke(MethodInvocation invocation) throws Throwable {  
        before();  
        Object result = invocation.proceed();  
        after();  
        return result;  
    }  
 
    private void before() {  
        System.out.println("Before");  
    }  
 
    private void after() {  
        System.out.println("After");  
    }  
} 
```
最后客户端的调用方法为：

```java
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

public class Client {  
 
    public static void main(String[] args) {  
        ApplicationContext context = new ClassPathXmlApplicationContext("aop/demo/spring.xml"); // 获取 Spring Context  
        Greeting greeting = (Greeting) context.getBean("greetingProxy");                        // 从 Context 中根据 id 获取 Bean 对象（其实就是一个代理）  
        greeting.sayHello("Jack");                                                              // 调用代理的方法  
    }  
} 
```
其中：
"aop/demo/spring.xml"，就是上述Spring的配置文件相对于当前工程的完整路径；
greetingProxy就是配置文件中设置的代理的id；
通过ApplicationContext获取配置文件中设定的上下文环境，然后从context中拿到自动扫描添加的bean结构，也就是Greeting的一个实例，然后通过这个接口的实例来调用其抽象方法完成功能。
具体的实现类greetingImpl被xml隐藏了，从代码编写角度去除了耦合。

最后得到的效果就是：
代码只关注于业务逻辑，将配置性的代码放入配置文件，这样去除代码中耦合的同时也有助于后期维护。

###### （3）Spring AOP的应用：抛出增强
除了上述的前置增强、后置增强和环绕增强，还有一种增强用于目标方法抛出异常后实施增强，叫做抛出增强。
程序报错，抛出异常了，一般的做法是打印到控制台或日志文件中，这样很多地方都得去处理，有没有一个一劳永逸的方法呢？那就是 Throws Advice（抛出增强）。
改写Greeting接口的实现，增加一个异常抛出：

```java
import org.springframework.stereotype.Component;

@Component 
public class GreetingImpl implements Greeting {  
 
    @Override 
    public void sayHello(String name) {  
        System.out.println("Hello! " + name);  
 
        throw new RuntimeException("Error"); // 故意抛出一个异常，看看异常信息能否被拦截到  
    }  
} 
```
然后实现一个抛出增强：

```java
import org.springframework.stereotype.Component;
import org.springframework.aop.ThrowsAdvice;

@Component 
public class GreetingThrowAdvice implements ThrowsAdvice {  
 
    public void afterThrowing(Method method, Object[] args, Object target, Exception e) {  
        System.out.println("---------- Throw Exception ----------");  
        System.out.println("Target Class: " + target.getClass().getName());  
        System.out.println("Method Name: " + method.getName());  
        System.out.println("Exception Message: " + e.getMessage());  
        System.out.println("-------------------------------------");  
    }  
} 
```
抛出增强类需要实现 org.springframework.aop.ThrowsAdvice 接口，在接口方法中可获取方法、参数、目标对象、异常对象等信息。我们可以把这些信息统一写入到日志中，当然也可以持久化到数据库中。

需要注意的是需要在xml配置中添加这个增强：
```xml
    <bean id="greetingThrowAdviceProxy" class="org.springframework.aop.framework.ProxyFactoryBean"> 
        <property name="interfaces" value="aop.Greeting"/> 
        <property name="target" ref="greetingImpl"/> 
        <property name="interceptorNames" value="GreetingThrowAdvice"/> <!-- 这儿设置抛出增强的实例 -->
    </bean> 
```
这样客户端调用的时候和上述实例是一样的：

```java
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

public class Client {  
 
    public static void main(String[] args) {  
        ApplicationContext context = new ClassPathXmlApplicationContext("aop/demo/spring.xml"); // 获取 Spring Context  
        Greeting greeting = (Greeting) context.getBean("greetingThrowAdviceProxy");             // 从 Context 中根据 id 获取 Bean 对象（其实就是一个代理）  
        greeting.sayHello("Jack");                                                              // 调用代理的方法  
    }  
} 
```
还是通过xml配置文件中关联的增强器来进行调用。

###### （4）Spring AOP的应用：对类的增强
上述应用都是针对类的方法进行增强的，那么能不能对一个类进行增强？根据AOP的实现原理可以知道，这个没有什么问题的，完全可以。
现在我们就来看看如何对一个类进行增强。
用 AOP 的行话来讲，对方法的增强叫做 Weaving（织入），而对类的增强叫做 Introduction（引入）。而 Introduction Advice（引入增强）就是对类的功能增强，它也是 Spring AOP 提供的最后一种增强。

现在来设想一种场景：
因为业务需要，我们定义了一个新接口 Apology（道歉）：

```java
public interface Apology {   
    void saySorry(String name);  
} 
```
然后要求所有的实现要同时满足这个接口和之前定义的Greeting接口，并且之前只实现Greeting接口的也需要按照这个规范进行整改。
也就是说：我们需要一个实现Greeting和Apology版本的GreetingImpl，并且之前的GreetingImpl需要被覆盖掉，现在该怎么办？
最直接的方法就是：改写GreetingImpl这个类来补全新增接口的实现。

关键是我不想改它，或许在真实场景中，这个类有1万行代码，或者我拿不到源代码等原因，无法通过重写GreetingImpl的方式完成。
于是，我需要借助 Spring 的引入增强，在程序运行的时候动态地在GreetingImpl这个类中实现这个新的Apology接口：

```java
import org.springframework.stereotype.Component;
import org.springframework.aop.support.DelegatingIntroductionInterceptor;

@Component 
public class GreetingIntroAdvice extends DelegatingIntroductionInterceptor implements Apology {  
    
    @Override 
    public Object invoke(MethodInvocation invocation) throws Throwable {  
        return super.invoke(invocation);  
    }  
 
    @Override 
    public void saySorry(String name) {  
        System.out.println("Sorry! " + name);  
    }  
} 
```
以上定义了一个引入增强类GreetingIntroAdvice，继承了 org.springframework.aop.support.DelegatingIntroductionInterceptor 类，同时也实现了新定义的 Apology 接口。在类中首先覆盖了父类DelegatingIntroductionInterceptor的 invoke() 方法，然后实现了 Apology 接口的saySorry方法。
通过这个增强类去丰富 GreetingImpl 类的功能，那么这个 GreetingImpl 类无需直接实现 Apology 接口，就可以在程序运行的时候调用 Apology 接口的方法了。

现在看看怎么用这个增强类来完成这个功能。
首先还是通过配置文件来进行关联配置：

```xml
<?xml version="1.0" encoding="UTF-8"?> 
<beans xmlns="http://www.springframework.org/schema/beans" 
       xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
       xmlns:context="http://www.springframework.org/schema/context" 
       xsi:schemaLocation="http://www.springframework.org/schema/beans  
       http://www.springframework.org/schema/beans/spring-beans.xsd  
       http://www.springframework.org/schema/context  
       http://www.springframework.org/schema/context/spring-context.xsd"> 
 
    <context:component-scan base-package="aop.demo"/> 
 
    <bean id="greetingClassProxy" class="org.springframework.aop.framework.ProxyFactoryBean"> 
        <property name="interfaces" value="aop.demo.Apology"/>          <!-- 需要动态实现的接口 --> 
        <property name="target" ref="greetingImpl"/>                    <!-- 目标类 --> 
        <property name="interceptorNames" value="greetingIntroAdvice"/> <!-- 引入增强 --> 
        <property name="proxyTargetClass" value="true"/>                <!-- 代理目标类（默认为 false，只能代理接口） --> 
    </bean> 
 
</beans> 
```
需要注意 proxyTargetClass 属性，它表明是否代理目标类，默认为 false，也就是代理接口了，此时 Spring 就用 JDK 动态代理。如果为 true，那么 Spring 就用 CGLib 动态代理。

最后是客户端代码调用：

```java
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

public class Client {  
 
    public static void main(String[] args) {  
        ApplicationContext context = new ClassPathXmlApplicationContext("aop/demo/spring.xml");  
        GreetingImpl greetingImpl = (GreetingImpl) context.getBean("greetingClassProxy"); // 注意：转型为目标类，而并非它的 Greeting 接口  
        greetingImpl.sayHello("Jack");  
 
        Apology apology = (Apology) greetingImpl; // 将目标类强制向上转型为 Apology 接口（这是引入增强给我们带来的特性，也就是“接口动态实现”功能）  
        apology.saySorry("Jack");  
    }  
}
```
发现saySorry() 方法原来是可以被 greetingImpl 对象来直接调用的，只需将其强制转换为该接口即可。

##### SpringBoot中的AOP应用：
通过上述实例可以看到，在Spring中使用AOP编程需要进行xml配置的设置，SpringBoot设置的目标就是简化配置，那么现在看看在SpringBoot中怎么使用AOP编程。
我们以一个场景进行描述：一个是如何在Spring Boot中引入Aop功能，二是如何使用Aop做切面去统一处理Web请求的日志。

###### （1）引入SpringBoot工程的AOP支持组件：
本文主要参考：
> - [Spring Boot中使用AOP统一处理Web请求日志](http://didispace.com/springbootaoplog/)

在gradle构建脚本中加入：

```gradle
dependencies {
    compile("org.springframework.boot:spring-boot-starter-aop")
}
```
在完成了引入AOP依赖包后，一般来说并不需要去做其他配置。也许在Spring中使用过注解配置方式的人会问是否需要在程序主类中增加@EnableAspectJAutoProxy来启用，实际并不需要。
可以看下面关于AOP的默认配置属性，其中spring.aop.auto属性默认是开启的，也就是说只要引入了AOP依赖后，默认已经增加了@EnableAspectJAutoProxy。

```config
# AOP
spring.aop.auto=true # Add @EnableAspectJAutoProxy.
spring.aop.proxy-target-class=false # Whether subclass-based (CGLIB) proxies are to be created (true) as opposed to standard Java interface-based proxies (false).
```
而当我们需要使用CGLIB来实现AOP的时候，需要在当前工程的application.property中配置spring.aop.proxy-target-class=true，不然默认使用的是标准Java的实现。

###### （2）实现Web层的日志切面测试：
我们在一个标准的SpringBoot欢迎页面来进行AOP注入：

首先在当前工程中设置application.property文件，来指定访问地址和端口，以及主路径：
```application.property
# IDENTITY
spring.application.name=hellmonky
# EMBEDDED SERVER CONFIGURATION
#server.address=127.0.0.1
server.context-path=/hellmonky
server.port=8080
server.session.timeout=500
```

然后实现一个controller层：

```java
package cn.ac.iscas.controller;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

/**
 * Created by wentao on 2016/12/20.
 *
 * web访问的Welcome页面回显
 *
 */

@RestController
@RequestMapping("/")
public class Welcome {

    @RequestMapping(value = "/version", method = RequestMethod.GET)
    String version(){
        return "Welcome";
    }
}
```
实现AOP的切面主要有以下几个要素：
> - 使用@Aspect注解将一个java类定义为切面类
> - 使用@Pointcut定义一个切入点，可以是一个规则表达式，比如下例中某个package下的所有函数，也可以是一个注解等。

根据需要在切入点不同位置的切入内容“
> - 使用@Before在切入点开始处切入内容
> - 使用@After在切入点结尾处切入内容
> - 使用@AfterReturning在切入点return内容之后切入内容（可以用来对处理返回值做一些加工处理）
> - 使用@Around在切入点前后切入内容，并自己控制何时执行切入点自身的内容
> - 使用@AfterThrowing用来处理当切入内容部分抛出异常之后的处理逻辑

根据上述这些基本概念的了解，现在来进行注入。
我们新建一个类，名字为WelcomeAspectService，表示用于给上面的Welcome进行切面注入服务：

```java
package cn.ac.iscas.aspect;

import cn.ac.iscas.bean.logBean.InfoLogBean;
import cn.ac.iscas.services.loggerService.impl.LogService;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.annotation.Pointcut;

/**
 * Created by wentao on 2016/12/20.
 *
 * Welcome请求AOP处理
 *
 */

@Aspect
@Component
public class WelcomeAspectService {

    private static LogService logService = LogService.getInstance();

    public WelcomeAspectService(){}

    @Pointcut("execution(public * cn.ac.iscas.controller.Welcome..*.*(..))")
    public void welcomeLog(){}

    @Before("welcomeLog()")
    public void doBefore(){
        InfoLogBean bean = new InfoLogBean();
        bean.setLogContent("doBefore log service");
        this.logService.info(bean);
    }

    @AfterReturning(returning = "ret", pointcut = "welcomeLog()")
    public void doAfterReturning(Object ret){
        InfoLogBean bean = new InfoLogBean();
        bean.setLogContent("doAfterReturning log service:"+ret);
        this.logService.info(bean);
    }
}
```
在上面的代码中，我们通过@Pointcut定义的切入点为cn.ac.iscas.controller.Welcome类下的所有函数。（这个切入点设置存在问题，没有正确进入）
也可以指定为包路径来对对web层所有请求处理做切入点：

```java
@Pointcut("execution(public * cn.ac.iscas.controller..*.*(..))")
```
然后通过@Before实现，对请求内容的日志记录（本文只是说明过程，可以根据需要调整内容）。
最后通过@AfterReturning记录请求返回的对象。其中returning = "ret"这个表达式用于获取返回值形参名，增强处理定义的方法可以通过这个形参名来访问目标方法的返回值。

需要注意的是，必须在AOP类中加入：
```java
@Component
```
表示用Spring来进行初始化这个类，相当于Spring中的XML配置关系，否则因为无法让Spring扫描管理，无法实现Aspect编程。

现在重启服务，访问：
http://localhost:8080/hellmonky/version
查看日志是否正确输出。

###### （3）关于Spring中使用AOP的一些优化：
在WelcomeAOP切面中，分别通过doBefore和doAfterReturning两个独立函数实现了切点头部和切点返回后执行的内容，若我们想统计请求的处理时间，就需要在doBefore处记录时间，并在doAfterReturning处通过当前时间与开始处记录的时间计算得到请求处理的消耗时间。
那么我们是否可以在WebLogAspect切面中定义一个成员变量来给doBefore和doAfterReturning一起访问呢？是否会有同步问题呢？
的确，直接在这里定义基本类型会有同步问题，所以我们可以引入ThreadLocal对象，保证同步的正确性。
现在添加后的代码为：

```java
package cn.ac.iscas.aspect;

import cn.ac.iscas.bean.logBean.InfoLogBean;
import cn.ac.iscas.services.loggerService.impl.LogService;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.stereotype.Component;

/**
 * Created by wentao on 2016/12/20.
 *
 * Welcome请求AOP处理
 *
 */

@Aspect
@Component
public class WelcomeAspectService {

    // 日志记录调用
    private static LogService logService = LogService.getInstance();

    // 支持同步的计时器，表明是当前线程自有变量：
    ThreadLocal<Long> startTime = new ThreadLocal<>();

    public WelcomeAspectService(){}

    //@Pointcut("execution(public * cn.ac.iscas.controller..*.*(..))")
    @Pointcut("execution(public * cn.ac.iscas.controller.Welcome..*.*(..))")
    public void welcomeLog(){}

    @Before("welcomeLog()")
    public void doBefore(){
        startTime.set(System.currentTimeMillis());
        InfoLogBean bean = new InfoLogBean();
        bean.setLogContent("doBefore log service");
        this.logService.info(bean);
    }

    @AfterReturning(returning = "ret", pointcut = "welcomeLog()")
    public void doAfterReturning(Object ret){
        Long currentSpendTime = System.currentTimeMillis() - startTime.get();
        InfoLogBean bean = new InfoLogBean();
        bean.setLogContent("doAfterReturning log service:"+ret+" with time:"+currentSpendTime);
        this.logService.info(bean);
    }
}
```

由于通过AOP实现，程序得到了很好的解耦，但是也会带来一些问题，比如：我们可能会对Web层做多个切面，校验用户，校验头信息等等，这个时候经常会碰到切面的处理顺序问题。
Spring AOP采用和AspectJ一样的有限顺序来织入增强处理：在“进入”连接点时，最高优先级的增强处理将先被织入（所以给定的两个Before增强处理中，优先级高的那个会先执行）；在“退出”连接点时，最高优先级的增强处理会最后被织入（所以给定的两个After增强处理中，优先级高的那个会后执行）。当不同的切面中的多个增强处理需要在同一个连接点被织入时，Spring AOP将以随机的顺序来织入这些增强处理。如果应用需要指定不同切面类里的增强处理的优先级，Spring提供了如下两种解决方案：
> - 让切面类实现org.springframework.core.Ordered接口：实现该接口只需要实现一个int getOrder()方法，该方法返回值越小，优先级越高
> - 直接使用@Order注解来修饰一个切面类：使用这个注解时可以配置一个int类型的value属性，该属性值越小，优先级越高

在这个例子中，我们需使用@Order(i)注解来标识切面的优先级。i的值越小，优先级越高。
如果我们再写一个AOP处理，假定为：CheckNameAspect，用来对当前登陆用户进行效验，保证被授权的用户才能继续访问，那么这个AOP就需要被记录到日志中，我们设置其@Order(10)，然后设置WelcomeAOP为@Order(5)，那么执行的时候的顺序为：
> - 在@Before中优先执行@Order(5)的内容，再执行@Order(10)的内容；
> - 在@After和@AfterReturning中优先执行@Order(10)的内容，再执行@Order(5)的内容。

用于用户登录的AOP具体的代码为：

```java
package cn.ac.iscas.aspect;

import cn.ac.iscas.bean.logBean.InfoLogBean;
import cn.ac.iscas.services.loggerService.impl.LogService;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

/**
 * Created by wentao on 2016/12/20.
 *
 * 用户登入授权校验AOP
 *
 */


@Aspect
@Component
@Order(10)
public class UserLoginAspectService {

    // 日志记录调用
    private static LogService logService = LogService.getInstance();
    // 线程安全的计时器：
    private ThreadLocal<Long> timer = new ThreadLocal<>();

    public UserLoginAspectService(){}

    // 对所有controller层都进行检查：
    @Pointcut("execution(public * cn.ac.iscas.controller..*.*(..))")
    public void userloginCheck(){}

    @Before("userloginCheck()")
    public void doBefore(){
        timer.set(System.currentTimeMillis());
        InfoLogBean bean = new InfoLogBean();
        bean.setLogContent("user login check start");
        this.logService.info(bean);
    }

    @AfterReturning(returning = "ret", pointcut = "userloginCheck()")
    public void doAfterReturning(Object ret){
        Long currentSpendTime = System.currentTimeMillis() - timer.get();
        InfoLogBean bean = new InfoLogBean();
        bean.setLogContent("user login check success:"+ret+" with time:"+currentSpendTime);
        this.logService.info(bean);
    }
}
```

所以总结为：
> - 在切入点前的操作，按order的值由小到大执行
> - 在切入点后的操作，按order的值由大到小执行

###### （4）Spring中多个切面的合并：
上述例子中的用户登录和日志分开为两个切面进行，其实可以将不同的切面进行合并。
我们新建一个计时类来完成对http地址和controller层的方法分别进行计时统计的类ExecutionTimeLoggerService：

```java
package cn.ac.iscas.aspect;

import cn.ac.iscas.services.loggerService.impl.LogService;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.util.StopWatch;

/**
 * Created by wentao on 2016/12/22.
 */

@Aspect
@Component
public class ExecutionTimeLoggerService {

    private static LogService logService = LogService.getInstance();

    public ExecutionTimeLoggerService(){};


    @Pointcut("@annotation(org.springframework.web.bind.annotation.RequestMapping)")
    public void requestMapping() {}

    @Pointcut("execution(public * cn.ac.iscas.controller..*.*(..))")
    public void methodPointcut() {}

    @Around("requestMapping() && methodPointcut()")
    public Object profile(ProceedingJoinPoint pjp) throws Throwable {
        StopWatch sw = new StopWatch();
        String name = pjp.getSignature().getName();
        try {
            sw.start();
            return pjp.proceed();
        } finally {
            sw.stop();
            logService.info("STOPWATCH: " + sw.getTotalTimeMillis() + " millis, while calling: " + name);
        }
    }
}
```
在这个类中，requestMapping()表示了对HTTP访问的切面，methodPointcut()表示了对于controller层的所有方法的切面。
最终上述这两个切面都会被一个环绕增强来进行计时统计。
这个时候，最终的日志为：
```log
2016-12-22 10:03:24,406  INFO LogService:46 - doBefore log service
2016-12-22 10:03:31,286  INFO LogService:46 - user login check start
2016-12-22 10:03:39,325  INFO LogService:46 - STOPWATCH: 1484 millis, while calling: version
2016-12-22 10:03:43,489  INFO LogService:46 - user login check success:Welcome version 1.0 with time:12203
2016-12-22 10:03:45,309  INFO LogService:46 - doAfterReturning log service:Welcome version 1.0 with time:20903
```

###### （5）切面中获取目标方法的信息：
参考：
> - [Spring中的AOP（五）——在Advice方法中获取目标方法的参数](https://my.oschina.net/itblog/blog/211693)

上述例子中，我们的切面只是单纯的织入到连接点上，没有进行参数的交互。但是往往切面还需要根据连接点的参数才能完成功能。
例如，我们需要在Welcome类的Aspect中记录当前被访问的HTTP接口传入了什么参数，以这个例子来看怎么样获取参数。
首先对Welcome增加一个获取参数的方法：

```java
package cn.ac.iscas.controller;

import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

/**
 * Created by wentao on 2016/12/20.
 *
 * web访问的Welcome页面回显
 *
 */

@RestController
@RequestMapping("/")
public class Welcome {

    @RequestMapping(value = "/version", method = RequestMethod.GET)
    public String version(){
        return "Welcome version 1.0";
    }

    @RequestMapping(value = "/sayHello/{content}", method = RequestMethod.GET)
    public String sayHello(@PathVariable String content){
        return "hello "+ content;
    }
}
```
现在重启服务，访问：
http://localhost:8080/hellmonky/sayHello/test
查看服务返回结果。

我们新建一个类WelcomeSayHelloAspectService，表示针对Welcome中的sayHello方法的参数来进行获取：

```java
package cn.ac.iscas.aspect;

import cn.ac.iscas.services.loggerService.impl.LogService;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.stereotype.Component;

/**
 * Created by wentao on 2016/12/22.
 */

@Aspect
@Component
public class WelcomeSayHelloAspectService {

    // 日志记录调用
    private static LogService logService = LogService.getInstance();
    // 支持同步的计时器，表明是当前线程自有变量：
    ThreadLocal<Long> timer = new ThreadLocal<>();

    public WelcomeSayHelloAspectService(){}

    @Pointcut("execution(public * cn.ac.iscas.controller.Welcome.sayHello(..))")
    public void welcomeSayHelloLog(){}


    @Before("welcomeSayHelloLog()")
    public void doBefore(){
        timer.set(System.currentTimeMillis());
        this.logService.info(timer.get().toString());
    }

    @AfterReturning(pointcut = "welcomeSayHelloLog()")
    public void doAfterReturning(){
        Long currentSpendTime = System.currentTimeMillis() - timer.get();
        this.logService.info(currentSpendTime.toString());
    }
}
```
这个切面通过：
@Pointcut("execution(public * cn.ac.iscas.controller.Welcome.sayHello(..))")
明确的定位到Welcome的sayHello方法上进行织入。
重新运行可以发现成功的进行了织入。

<1> 使用JoinPoint参数获取目标方法所有信息：
访问目标方法最简单的做法是定义增强处理方法时，将第一个参数定义为JoinPoint类型，当该增强处理方法被调用时，该JoinPoint参数就代表了织入增强处理的连接点。JoinPoint里包含了如下几个常用的方法：
```content
Object[] getArgs：返回目标方法的参数
Signature getSignature：返回目标方法的签名
Object getTarget：返回被织入增强处理的目标对象
Object getThis：返回AOP框架为目标对象生成的代理对象
```
现在我们使用JoinPoint类型参数获取织入点的参数，修改上述doBefore函数为：

```java
    @Before("welcomeSayHelloLog()")
    public void doBefore(JoinPoint point){
        Object[] args = point.getArgs();
        if (args != null && args.length > 0) {
            for(Object tmp:args){
                if (tmp.getClass() == String.class) {
                    System.out.println(tmp);
                }
            }
        }

        timer.set(System.currentTimeMillis());
        this.logService.info(timer.get().toString());
    }
```
重启服务，访问页面，可以获取到来连接点的参数了。

JoinPoint参数不仅可以获取目标方法的参数信息，还可以获取其他信息。
我们还可以在Welcome的sayHello执行之前改变参数的参数来完成一些附加操作，新增一个环绕增强：

```java
    @Around("welcomeSayHelloLog()")
    public Object process(ProceedingJoinPoint point) throws Throwable {
        String changedeArgs = null;
        Object[] args = point.getArgs();
        if (args != null && args.length > 0) {
            for(Object tmp:args){
                if (tmp.getClass() == String.class) {
                    System.out.println(tmp);
                    // 这儿可以修改参数：
                    changedeArgs  = "processed success:" + tmp;
                    System.out.println(changedeArgs);
                }
            }
        }
        // 用改变后的参数来执行目标方法：
        args[0] = changedeArgs;
        Object returnValue = null;
        try{
            returnValue = point.proceed(args);
            System.out.println("@Around：被织入的目标对象为：" + point.getTarget());
        }catch (Throwable e){
            e.printStackTrace();
        }

        return returnValue;
    }
```
其中ProceedingJoinPoint是JoinPoint的子类，并且只能在@Around中使用，来完成对传入参数的更改的功能。
这个函数将获取sayHello的传入参数，修改后再交给其他增强切面函数和sayHello函数进行处理。
这三个增强函数的执行顺序为：
sayHello->process()的proceed执行返回->doBefore()->process()的return->doAfterReturning()

并且通过返回结果可以看出：
在任何一个织入的增强处理中，都可以获取目标方法的信息。

需要注意的是：
同一个切面类里的两个相同类型的增强处理在同一个连接点被织入时，Spring AOP将以随机的顺序来织入这两个增强处理，没有办法指定它们的织入顺序。如果确实需要保证它们以固有的顺序被织入，则可以考虑将多个增强处理压缩为一个增强处理；或者将不同增强处理重构到不同切面中，通过在切面级别上定义顺序。

<2> 使用获取目标方法的参数信息：
如果只要访问目标方法的参数，Spring还提供了一种更加简洁的方法：我们可以在程序中使用args来绑定目标方法的参数。如果在一个args表达式中指定了一个或多个参数，该切入点将只匹配具有对应形参的方法，且目标方法的参数值将被传入增强处理方法。
我们还是以sayHello为例来用args表达式的方式来获取参数，修改Before切面：

```java
    @Before("welcomeSayHelloLog() && args(content)")
    public void doBefore(String content){
        System.out.println(content);
        timer.set(System.currentTimeMillis());
        this.logService.info(timer.get().toString());
    }
```
上面的程序中，定义Before时，表达式中增加了args(content)部分，意味着可以在增强处理方法（doBefore方法）中定义的content形参的类型可以随意指定，但一旦指定了这个参数的类型，则这个形参类型将用于限制该切入点匹配参数类型为String的方法（方法参数个数和类型若有不同均不匹配）。
并且args表达式中可以增加多个参数。 
除此之外，使用args表达式时，还可以使用如下形式：args(param1, param2, ..)，注意args参数中后面的两个点，它表示可以匹配更多参数。在例子args(param1, param2, ..)中，表示目标方法只需匹配前面param1和param2的类型即可。

需要注意的是：
如果增强方法定义了returning表达式的时候，这个值（即上面的returning="returnValue"中的returnValue）作为增强处理方法的形参时，位置可以随意，但是一定要保证args表达式中获取参数的顺序不能改变，例如定义增强方法为：
```java
@Before("welcomeSayHelloLog() && args(time, name)",returning="returnValue")
```
那么可以的方法定义为：
```java
public void access(Date time, Object returnValue, String name)
public void access(Object returnValue, Date time, String name)
public void access(Date time, String name, Object returnValue)
```
只需要满足另外的参数名的顺序和pointcut中args(param1, param2)的顺序相同即可。

##### Spring中使用AOP的注解和语法汇总说明：
本节主要参考：
> - [【spring-boot】spring aop 面向切面编程初接触](http://www.cnblogs.com/lic309/p/4079194.html)

在实际使用测试了具体的代码之后，我们来对一些基本概念进行解释和梳理：

> - 切面（Aspect）：一个关注点的模块化，这个关注点可能会横切多个对象。事务管理是J2EE应用中一个关于横切关注点的很好的例子。在Spring AOP中，切面可以使用基于模式或者基于@Aspect注解的方式来实现。
> - 连接点（Joinpoint）：在程序执行过程中某个特定的点，比如某方法调用的时候或者处理异常的时候。在Spring AOP中，一个连接点总是表示一个方法的执行。
> - 通知（Advice）：在切面的某个特定的连接点上执行的动作。其中包括了“around”、“before”和“after”等不同类型的通知（通知的类型将在后面部分进行讨论）。许多AOP框架（包括Spring）都是以拦截器做通知模型，并维护一个以连接点为中心的拦截器链。
> - 切入点（Pointcut）：匹配连接点的断言。通知和一个切入点表达式关联，并在满足这个切入点的连接点上运行（例如，当执行某个特定名称的方法时）。切入点表达式如何和连接点匹配是AOP的核心：Spring缺省使用AspectJ切入点语法。
> - 引入（Introduction）：用来给一个类型声明额外的方法或属性（也被称为连接类型声明（inter-type declaration））。Spring允许引入新的接口（以及一个对应的实现）到任何被代理的对象。例如，你可以使用引入来使一个bean实现IsModified接口，以便简化缓存机制。
> - 目标对象（Target Object）：被一个或者多个切面所通知的对象。也被称做被通知（advised）对象。既然Spring AOP是通过运行时代理实现的，这个对象永远是一个被代理（proxied）对象。
> - AOP代理（AOP Proxy）：AOP框架创建的对象，用来实现切面契约（例如通知方法执行等等）。在Spring中，AOP代理可以是JDK动态代理或者CGLIB代理。
> - 织入（Weaving）：把切面连接到其它的应用程序类型或者对象上，并创建一个被通知的对象。这些可以在编译时（例如使用AspectJ编译器），类加载时和运行时完成。Spring和其他纯Java AOP框架一样，在运行时完成织入。

上述就是在使用Spring中必须要明确的概念。
> - 关于Spirng的Annotation，根据不同的版本查看官网文档最为合适：[]()

###### （1）涉及到的注解：
这些概念在SpringBoot中的体现为注解，现在我们详细看一下对应的注解方式。
<1> @Aspect:
定义在：aspectjweaver.jar\org\aspectj\lang\annotation\Aspect.java
用来描述一个切面类，定义切面类的时候需要打上这个注解。

<2> @Component:
将类标识为Java Bean。
Spring还提供有：@Repository、@Service、@Controller和@Component都可以将类标识为Java Bean，但是他们之间还是存在差异。例如：@Repository注解用于将数据访问层 (DAO 层 ) 的类标识为Spring Bean。

<3> @Configuration:
表示这个类可以使用Spring IoC容器作为bean定义的来源。

<4> @Pointcut:
声明一个切入点，切入点决定了连接点关注的内容，使得我们可以控制通知什么时候执行。

<5> @Before：
前置通知：在某连接点之前执行的通知，但这个通知不能阻止连接点之前的执行流程（除非它抛出一个异常）。

<6> @AfterReturning:
后置通知：在某连接点正常完成后执行的通知，通常在一个匹配的方法返回的时候执行。

<7> @AfterThrowing:
异常通知：在方法抛出异常退出时执行的通知。　　　　　　　

<8> @After:
最终通知：当某连接点退出的时候执行的通知（不论是正常返回还是异常退出）。

<9> @Around：
环绕通知：包围一个连接点的通知，如方法调用。这是最强大的一种通知类型。环绕通知可以在方法调用前后完成自定义的行为。它也会选择是否继续执行连接点或直接返回它自己的返回值或抛出异常来结束执行。
环绕通知是一个对方法的环绕，具体方法会通过代理传递到切面中去，切面中可选择执行方法与否，执行方法几次等。
环绕通知使用一个代理ProceedingJoinPoint类型的对象来管理目标对象，所以此通知的第一个参数必须是ProceedingJoinPoint类型，在通知体内，调用ProceedingJoinPoint的proceed()方法会导致后台的连接点方法执行。proceed 方法也可能会被调用并且传入一个Object[]对象-该数组中的值将被作为方法执行时的参数。
例如上述代码片段：

```java
@Around("requestMapping() && methodPointcut()")
    public Object profile(ProceedingJoinPoint pjp) throws Throwable {
        StopWatch sw = new StopWatch();
        String name = pjp.getSignature().getName();
        try {
            sw.start();
            return pjp.proceed();
        } finally {
            sw.stop();
            logService.info("STOPWATCH: " + sw.getTotalTimeMillis() + " millis, while calling: " + name);
        }
    }
```
其中ProceedingJoinPoint pjp就是代理参数。

###### （2）重点注解的用法：
我们在SpringBoot中已经用了相关的注解完成了Spring对AOP的配置支持，我们现在更为详细的看一下注解的用法。
<1> @Aspect注解：
当我们定义了一个切面类，一定要通过这个注解告诉Spring，让他完成对应的加载操作。

<2> @Pointcut注解：
Spring AOP只支持Spring bean的方法执行连接点。所以你可以把切入点看做是Spring bean上方法执行的匹配。一个切入点声明有两个部分：一个包含名字和任意参数的签名，还有一个切入点表达式，该表达式决定了我们关注那个方法的执行。
并且作为切入点签名的方法必须返回void 类型。例如上述代码中的：requestMapping()函数。

Spring AOP支持在切入点表达式中使用如下的切入点指示符：
```note
execution - 匹配方法执行的连接点，这是你将会用到的Spring的最主要的切入点指示符。
within - 限定匹配特定类型的连接点（在使用Spring AOP的时候，在匹配的类型中定义的方法的执行）。
this - 限定匹配特定的连接点（使用Spring AOP的时候方法的执行），其中bean reference（Spring AOP 代理）是指定类型的实例。
target - 限定匹配特定的连接点（使用Spring AOP的时候方法的执行），其中目标对象（被代理的应用对象）是指定类型的实例。
args - 限定匹配特定的连接点（使用Spring AOP的时候方法的执行），其中参数是指定类型的实例。
@target - 限定匹配特定的连接点（使用Spring AOP的时候方法的执行），其中正执行对象的类持有指定类型的注解。
@args - 限定匹配特定的连接点（使用Spring AOP的时候方法的执行），其中实际传入参数的运行时类型持有指定类型的注解。
@within - 限定匹配特定的连接点，其中连接点所在类型已指定注解（在使用Spring AOP的时候，所执行的方法所在类型已指定注解）。
@annotation - 限定匹配特定的连接点（使用Spring AOP的时候方法的执行），其中连接点的主题持有指定的注解。
```
其中execution使用最频繁，即某方法执行时进行切入。定义切入点中有一个重要的知识，即切入点表达式，我们一会在解释怎么写切入点表达式。
切入点意思就是在什么时候切入什么方法，定义一个切入点就相当于定义了一个“变量”，具体什么时间使用这个变量就需要一个通知。即将切面与目标对象连接起来。

###### （3）通知参数：
有时候我们定义切面的时候，切面中需要使用到目标对象的某个参数，要使切面能得到目标对象的参数就需要引入通知参数。
任何通知方法可以将第一个参数定义为org.aspectj.lang.JoinPoint类型（环绕通知需要定义第一个参数为ProceedingJoinPoint类型，它是 JoinPoint 的一个子类）。
JoinPoint接口提供了一系列有用的方法，比如 getArgs()（返回方法参数）、getThis()（返回代理对象）、getTarget()（返回目标）、getSignature()（返回正在被通知的方法相关信息）和 toString()（打印出正在被通知的方法的有用信息）。
然后在切面函数中就可以通过这个类提供的方法来完成对目标对象信息的访问。

###### （4）切入点定义：
在上述实例中，我们使用了一个入口函数作为切入点的代理，然后在不同的增强函数中使用这个切入点来进行增强说明。
所谓定义切入点，其实质就是为一个切入点表达式起一个名称，从而允许在多个增强处理中重用该名称。
切入点定义包含两个部分：
> - 一个切入点表达式：用于指定切入点和哪些方法进行匹配
> - 一个包含名字和任意参数的方法签名：将作为切入点的名称


上述切入点注解中，可以用切入点表达式来完成对要进行织入对象的定位，从而让Spring准确的找到需要进行织入的对象。
切入点表达式的格式：
```java
execution([可见性] 返回类型 [声明类型].方法名(参数) [异常])
``
其中：
```shell
[] : 表示其中的内容为可选的
*  : 表示可以匹配所有字符
.. : 一般用于匹配多个包，多个参数
+  : 表示类及其子类
```
运算符有：
```shell
&& : 表示与，表示两个条件都需要满足
|| : 表示或，两个条件满足一个即可
!  : 表示否定
```




##### Spring中使用AOP需要依赖的jar包说明：
虽然上述Spring和SpringBoot工程

要利用spring aop，至少需要添加以下jar包：
使用spring需要的jar：
spring.jar 、commons-logging.jar
使用切面编程（AOP）需要的jar：
aspectjrt.jar、aspectjweaver.jar、cglib-nodep-2.1_3.jar
使用JSR-250提供的注解,如@Resource，需要的jar：
common-annotations.jar