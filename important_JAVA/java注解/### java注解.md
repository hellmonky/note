### java注解：
Java注解又称Java标注，是Java语言5.0版本开始支持加入源代码的特殊语法元数据。 Java语言中的类、方法、变量、参数和包等都可以被标注。 Java标注和Javadoc不同，标注有自反性。 在编译器生成类文件时，标注可以被嵌入到字节码中，由Java虚拟机执行时获取到标注。
注解的功能类似于代码中的注释，所不同的是注解不是提供代码功能的说明，而是实现程序功能的重要组成部分。Java注解已经在很多框架中得到了广泛的使用，用来简化程序中的配置。
注解的定义位于：../jdk1.8.0_102/src.zip这个压缩包的java.lang.annotation包下。
注解按照功能也分为不同的类型，这些类型和它们所支持的类在java.lang.annotation包中可以找到。

元注解：元注解的作用就是负责注解其他注解。Java5.0定义了4个标准的meta-annotation类型，它们被用来提供对其它annotation类型作说明。这四个注解分别对应这个包下的四个java类。
也就是说元注解用于生成用户自定义注解，这个是各大框架所依赖的生成自定义注解的主要方式。

1. @Target：用于描述注解的使用范围（即：被描述的注解可以用在什么地方）。Annotation可被用于 packages、types（类、接口、枚举、Annotation类型）、类型成员（方法、构造方法、成员变量、枚举值）、方法参数和本地变量（如循环变量、catch参数）。在Annotation类型的声明中使用了target可更加明晰其修饰的目标。取值是一个枚举，定义在java.lang.annotation.ElementType.java中。
ElementType的取值有：
　　　　1.CONSTRUCTOR:用于描述构造器
　　　　2.FIELD:用于描述域
　　　　3.LOCAL_VARIABLE:用于描述局部变量
　　　　4.METHOD:用于描述方法
　　　　5.PACKAGE:用于描述包
　　　　6.PARAMETER:用于描述参数
　　　　7.TYPE:用于描述类、接口(包括注解类型) 或enum声明
实例如下：
```java
@Target(ElementType.TYPE)
public @interface Table {
    /**
     * 数据表名称注解，默认值为类名称
     * @return
     */
    public String tableName() default "className";
}

@Target(ElementType.FIELD)
public @interface NoDBColumn {
}
```
在 interface 前面的@符号表名这是一个注解，一旦你定义了一个注解之后你就可以将其应用到你的代码中。所以这里定义了两个注解：Table和NoDBColumn。
注解Table 可以用于注解类、接口(包括注解类型) 或enum声明,而注解NoDBColumn仅可用于注解类的成员变量。

> - 2. @Retention：
> - 3. @Documented：
> - 4. @Inherited：

[Java深度历险（六）——Java注解](http://www.infoq.com/cn/articles/cf-java-annotation)
[]()
[]()
[]()
[]()
[]()
[]()
[]()
[]()
[]()