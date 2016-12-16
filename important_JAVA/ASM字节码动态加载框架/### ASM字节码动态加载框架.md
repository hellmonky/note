### ASM字节码动态加载框架：
关于asm模块的使用，可以通过几个典型的场景来进行描述和分析。

#### 热部署场景：
[深入探索 Java 热部署](https://www.ibm.com/developerworks/cn/java/j-lo-hotdeploy/)

##### 发生场景：热部署（hotswap），热部署是在不重启Java虚拟机的前提下，能自动侦测到class文件的变化，更新运行时class的行为。
##### 场景描述：当前部署的环境不能因为小的bug修复等原因重启当前服务，而需要动态加载修改后的class文件来进行执行环境的更新。
##### 遇到问题：默认的虚拟机行为只会在启动时加载类，如果后期有一个类需要更新的话，单纯替换编译的 class 文件，Java 虚拟机是不会更新正在运行的 class。
##### 尝试方式：
（1）修改虚拟机的源代码：改变classloader的加载行为，使虚拟机能监听class文件的更新，重新加载class文件，这样的行为破坏性很大，为后续的JVM升级埋下了一个大坑。
（2）创建自己的classloader：通过自定义的类加载器来加载需要监听的class，这样就能控制类加载的时机，从而实现热部署。
##### 选择方案：上述解决方式中，一般从通用性的角度考虑都不会使用修改JVM的方式，而是会选择第二种自定义类加载器的方式来实现热部署。
##### 原理探讨：在确定了方案之后，从一般性原来来分析下这个方案的实现方式。
目前的加载机制，称为双亲委派，系统在使用一个classloader来加载类时，会先询问当前classloader的父类是否有能力加载，如果父类无法实现加载操作，才会将任务下放到该classloader来加载。这种自上而下的加载方式的好处是，让每个classloader执行自己的加载任务，不会重复加载类。但是这种方式却使加载顺序非常难改变，让自定义classloader抢先加载需要监听改变的类成为了一个难题。
虽然无法抢先加载该类，但是仍然可以用自定义classloader创建一个功能相同的类，让每次实例化的对象都指向这个新的类。当这个类的class文件发生改变的时候，再次创建一个更新的类，之后如果系统再次发出实例化请求，创建的对象讲指向这个全新的类。
##### 实现步骤：
明确了类的加载机制之后，我们梳理一下整体的方案实现思路：
（1）创建自定义的classloader，加载需要监听改变的类，在class文件发生改变的时候，重新加载该类。
（2）改变创建对象的行为，使他们在创建时使用自定义classloader加载的class。
###### 自定义类加载器的实现：
自定义加载器仍然需要执行类加载的功能。这里却存在一个问题，同一个类加载器无法同时加载两个相同名称的类，由于不论类的结构如何发生变化，生成的类名不会变，而classloader只能在虚拟机停止前销毁已经加载的类，这样classloader 就无法加载更新后的类了。
这里有一个小技巧，让每次加载的类都保存成一个带有版本信息的class，比如加载 Test.class时，保存在内存中的类是Test_v1.class，当类发生改变时，重新加载的类名是Test_v2.class。但是真正执行加载class文件创建class的defineClass方法是一个JVM的native的方法，修改起来又变得很困难。这条路不通。
最后只有一个方法了：动态修改编译生成的class文件，这样就用到了asm，修改字节码框架包。CGLIB也是调用了asm包。
（1）使用ASM修改字节码文件的流程：
一个标准的class文件包含了以下几类信息：一个是类的基本信息，包含了访问权限信息，类名信息，父类信息，接口信息；第二个是类的变量信息；第三个是方法的信息。
ASM 会先加载一个class文件，然后严格顺序读取类的上述各项信息，用户可以按照自己的意愿定义增强组件修改这些信息，最后输出成一个新的class。示例代码：

```java
ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_MAXS); 
ClassReader cr = null;     
String enhancedClassName = classSource.getEnhancedName(); 
try {
    // 从class文件读入
    cr = new ClassReader(new FileInputStream(classSource.getFile())); 
} catch (IOException e) { 
    e.printStackTrace(); 
    return null; 
} 
ClassVisitor cv = new EnhancedModifier(cw, 
                className.replace(".", "/"), 
                enhancedClassName.replace(".", "/"));
cr.accept(cv, 0);
```
ASM修改字节码文件的流程是一个责任链模式，首先使用一个ClassReader读入字节码，然后利用ClassVisitor做个性化的修改，最后利用ClassWriter输出修改后的字节码。
（2）将要修改的class文件读入，修改为一个接口：

```java
public Class<?> redefineClass(String className){ 
    ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_MAXS); 
    ClassReader cr = null; 
    ClassSource cs = classFiles.get(className); 
    if(cs==null){ 
        return null; 
    } 
    try { 
        cr = new ClassReader(new FileInputStream(cs.getFile())); 
    } catch (IOException e) { 
        e.printStackTrace(); 
        return null; 
    } 
    ClassModifier cm = new ClassModifier(cw); 
    cr.accept(cm, 0); 
    byte[] code = cw.toByteArray(); 
    return defineClass(className, code, 0, code.length); 
 }
 ```
 首先 load 原始类的 class 文件，此处定义了一个增强组件 ClassModifier，作用是修改原始类的类型，将它转换成接口。原始类的所有方法逻辑都会被去掉。
 （3）生成实现这个接口的派生类：
生成的派生类都实现这个接口，即原始类，并且复制原始类中的所有方法逻辑。之后如果该类需要更新，会生成一个新的派生类，也会实现这个接口。这样做的目的是不论如何修改，同一个 class 的派生类都有一个共同的接口，他们之间的转换变得对外不透明。

```java
private Class<?> redefineClass(String className, ClassSource classSource) {
    ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_MAXS);
    ClassReader cr = null;
    classSource.update();
    String enhancedClassName = classSource.getEnhancedName();
    try {
        cr = new ClassReader(
                new FileInputStream(classSource.getFile()));
    } catch (IOException e) {
        e.printStackTrace();
        return null;
    }
    EnhancedModifier em = new EnhancedModifier(cw, className.replace(".", "/"),
            enhancedClassName.replace(".", "/"));
    ExtendModifier exm = new ExtendModifier(em, className.replace(".", "/"),
            enhancedClassName.replace(".", "/"));
    cr.accept(exm, 0);
    byte[] code = cw.toByteArray();
    classSource.setByteCopy(code);
    Class<?> clazz = defineClass(enhancedClassName, code, 0, code.length);
    classSource.setClassCopy(clazz);
    return clazz;
}
```
再次load原始类的class文件，此处定义了两个增强组件:
第一个是EnhancedModifier：这个增强组件的作用是改变原有的类名;
第二个是ExtendModifier：这个增强组件的作用是改变原有类的父类，让这个修改后的派生类能够实现同一个原始类（此时原始类已经转成接口了）。
（4）使用定时器扫描需要监控的class文件的变化：
在完成上述修改class文件的功能后，还需要在这个classloader中添加一个定时器来扫面需要动态加载的classs文件的变化，这部分内容不填代码了。

通过上述三个步骤，就完成对一个原始class的动态修改：监控class文件的变化，发生变化的时候从原始class文件生成这个class的抽象接口，然后从原始class读入内容继承这个接口，实现一个和class文件相同功能的派生类。

###### 拦截默认加载器的行为：
之前实现的类加载器已经解决了热部署所需要的功能，可是 JVM 启动时，并不会用自定义的加载器加载 classpath 下的所有 class 文件，取而代之的是通过应用加载器去加载。如果在其之后用自定义加载器重新加载已经加载的 class，有可能会出现 LinkageError 的 exception。所以必须在应用启动之前，重新替换已经加载的 class。
JDK5.0之后，JDK提供了在JVM启动之后，应用启动之前的短暂间隙里提供给用户自定义行为的接口JavaAgent。通过这个接口，可以提供空间给用户做一些特殊行为。我们就使用这个接口来完成对默认加载器的拦截。
比较常见的应用，是利用JavaAgent做面向方面的编程（AOP），在方法间加入监控日志等功能。
（1）JavaAgent应用示例：
JavaAgent的实现很容易，只要在一个类里面，定义一个premain的方法:

```java
public class ReloadAgent {
    public static void premain(String agentArgs, Instrumentation inst){
        GeneralTransformer trans = new GeneralTransformer();
        inst.addTransformer(trans);
    }
}
```
然后编写一个manifest文件，将Premain-Class属性设置成定义一个拥有premain方法的类名即可：
```manifest
manifest-Version: 1.0 
Premain-Class: com.example.ReloadAgent 
Can-Redefine-Classes: true
```
然后生成要给包含这个manifest文件的jar包。
最后需要在执行应用的参数中增加-javaagent参数 , 加入这个jar。这样在执行应用的之前，会优先执行premain方法中的逻辑，并且预解析需要加载的class。
（2）阻止原始字节码被Java虚拟机加载：
这里利用JavaAgent替换原始字节码，阻止原始字节码被Java虚拟机加载。只需要实现一个ClassFileTransformer的接口，利用这个实现类完成class替换的功能。

```java
@Override
public byte [] transform(ClassLoader paramClassLoader, String paramString,
                         Class<?> paramClass, ProtectionDomain paramProtectionDomain,
                         byte [] paramArrayOfByte) throws IllegalClassFormatException {
    String className = paramString.replace("/", ".");
    if(className.equals("com.example.Test")){
        MyClassLoader cl = MyClassLoader.getInstance();
        cl.defineReference(className, "com.example.Greeter");
        return cl.getByteCode(className);
    }else if(className.equals("com.example.Greeter")){
        MyClassLoader cl = MyClassLoader.getInstance();
        cl.redefineClass(className);
        return cl.getByteCode(className);
    }
    return null;
}
```
这样就完成了对class文件的热部署功能。

#### AOP场景：
[AOP 的利器：ASM 3.0 介绍](https://www.ibm.com/developerworks/cn/java/j-lo-asm30/)

##### 发生场景：


所谓的AOP，就是一种在运行时，动态地将代码切入到类的指定方法、指定位置上的编程思想。关键点就在于运行时和动态切入，这儿选择java的原因在于自带的reflection机制能够帮助我们轻松的获取运行时的信息，然后通过class的字节码来操作生成代码，所以比较能够方便和广泛的使用，同样的思想，在C++中也可以存在AOP编程，例如：[AspectC++](http://www.aspectc.org/)，这个框架为了达到运行时动态的效果，需要用到专门的编译器去编译，语法也不再是C++语法，相比java来讲实现比较复杂，所以从java入手可以比较容易的理解原理。



在AOP中需要关注代码的动态生成，那么面向AOP的编程也就需要依赖于字节码动态生成框架来进行开发了，在这个场景下，使用asm来作为对AOP的基础支持技术。

##### 场景描述：
##### 遇到问题：
##### 尝试方式：
##### 选择方案：
##### 原理探讨：
##### 实现步骤：



spring、hibernate等大多数框架（Nutz）的源码就引用了asm，看spring源码肯定需要懂这个，不然你怎么看得懂？源码都没看懂技术会好吗？
[关于java字节码框架ASM的学习](http://www.cnblogs.com/liuling/archive/2013/05/25/asm.html)
[asm官方下载](http://forge.ow2.org/project/showfiles.php?group_id=23&release_id=5803)

[Package org.springframework.asm](http://docs.spring.io/spring/docs/current/javadoc-api/org/springframework/asm/package-summary.html)
[Java深度历险（一）——Java字节代码的操纵](http://www.infoq.com/cn/articles/cf-java-byte-code)
[使用ASM操作Java字节码，实现AOP原理](https://yq.aliyun.com/articles/4798)
[使用 ASM 实现 Java 语言的“多重继承”](http://www.cnblogs.com/liuling/archive/2013/05/31/asmMutilExtends.html)
[Java之代理（jdk静态代理，jdk动态代理，cglib动态代理，aop，aspectj）](http://blog.csdn.net/centre10/article/details/6847828)
[通过Java字节码发现有趣的内幕之初始化篇（三）](https://my.oschina.net/imcf/blog/647602)
[]()