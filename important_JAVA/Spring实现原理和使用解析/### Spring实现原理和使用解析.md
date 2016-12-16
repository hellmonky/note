### Spring实现原理和使用解析：
Spring作为javaEE的开发框架被广泛的使用，那么作为一个java开发者，如果使用Spring，最好彻底的理解这个框架的基本原理，这样才能方便自己根据实际需求定制开发。
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
（1）一个坏的例子：
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

（2）方案一：静态代理
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

（3）方案二：动态代理
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

（4）方案三：使用cglib动态代理
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

（1）前置增强、后置增强和环绕增强的基本用法样例：
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

（2）使用Spring配置文件来完成增强：
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

（3）Spring AOP的应用：抛出增强
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

（4）Spring AOP的应用：对类的增强
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

