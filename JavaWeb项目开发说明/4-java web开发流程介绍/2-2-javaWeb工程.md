### （2）java web工程 ###

>java web开发需要一些基本概念，这是自己现在没有的，需要进行学习和了解，本节内容就针对当前使用的java web开发基础知识进行学习。

#### <1> java本工程的基本概念 ####
java工程和java web工程有什么区别？为什么要分为javaee和javase这两种？而且很多工程也根据是否是javase和javaee来建立不同的工程。
其实这两种工程都是Java语言的应用，只是应用场合不同罢了，他们的本质区别在于：编译后路径。
虚拟机执行的是class文件而不是java文件，那么我们不管是何种项目都是写的java文件，怎么就不一样了呢？分成java和web两种了呢？

##### (a) .classpath文件的作用 #####
以eclipse的工程文件为例，从.classpath文件入手来看，这个文件在每个项目目录下都是存在的。这是一个XML文件，使用文本编辑器打开即可，示例如下：

```xml
<?xml version="1.0" encoding="UTF-8"?>
<classpath>
	<classpathentry excluding="com/sys/filter/" kind="src" path="src"/>
	<classpathentry kind="src" path="web"/>
	<classpathentry kind="src" path="database"/>
	<classpathentry kind="src" path="plugin"/>
	<classpathentry kind="src" path="filter"/>
	<classpathentry kind="con" path="org.eclipse.jst.server.core.container/org.eclipse.jst.server.tomcat.runtimeTarget/Apache Tomcat v7.0">
		<attributes>
			<attribute name="owner.project.facets" value="jst.web"/>
		</attributes>
	</classpathentry>
	<classpathentry kind="con" path="org.eclipse.jst.j2ee.internal.web.container"/>
	<classpathentry kind="con" path="org.eclipse.jst.j2ee.internal.module.container"/>
	<classpathentry kind="lib" path="WebContent/WEB-INF/lib/aopalliance-1.0.jar"/>
	<classpathentry kind="lib" path="WebContent/WEB-INF/lib/chardet.jar"/>
	<classpathentry kind="output" path="build/classes"/>
</classpath>
```
这个XML文档包含一个根元素classpath，它由子元素classpathentry的kind参数来设置不同的属性和类型信息：
>src：源码相对于当前工程文件的具体路径

>con：JRE容器的信息，表示当前工程需要的基本java执行环境

>lib：项目依赖的第三方类库的相对路径

>output：src设置的所有源代码编译后的输出位置，按照源代码的结构进行组织

总体上说.classpath文件就是eclipse配置整个工程的运行环境。
是否是web工程的要点就在于最后output的位置不同，按照javaee规范放置在web目录下的就是web工程，没有的就是一般的java工程。

##### (b) .project文件的作用 #####
当前工程文件的根目录下还有一个.project文件，他也是一个xml格式的文件，打开可以看见如下结果：

```xml
<?xml version="1.0" encoding="UTF-8"?>
<projectDescription>
	<name>sys</name>
	<comment></comment>
	<projects>
	</projects>
	
	<buildSpec>
		<buildCommand>
			<name>org.eclipse.jdt.core.javabuilder</name>
			<arguments>
			</arguments>
		</buildCommand>
	</buildSpec>
	
	<natures>
		<nature>org.eclipse.jdt.core.javanature</nature>
	</natures>
</projectDescription>
```
可以看出到这些配置主要是描述工程的基本信息：

```xml
<name></name>           工程名
<comment></comment>     工程注释描述
<buildSpec></buildSpec> 具体加载方式信息
<natures></natures>     运行时需要的额外Eclipse插件
```
也就是说.project是项目文件，项目的结构都在其中定义

##### (c) 其他配置文件相关 #####

**通过不同的配置文件设置class文件的输出位置就可以完成java不同项目的设置**

以Eclipse IDE for Java Developers这个基础IDE为例说明不同的java项目只在于组织结构不同：

>这个eclipse是最基本的用语开发java程序的IDE，对于开发web项目而言缺少一些自动生成工具来简化开发，所以如果要开发和管理web项目 需要手动安装WTP组件（Web Tools Platform）来进行管理。

在当前eclipse中按照如下网址安装WTP组件：
```url
http://download.eclipse.org/webtools/updates
```
安装完毕之后重启，然后新建一个web dynamic project就会在工程属性设置中找到“Deployment assembly”选项，然后在其中选择要部署的源代码位置，然后设置部署的路径保存。
这个选项将会在当前工程文件夹的根路径下生成”.setting“文件夹，并且在其中生成”org.eclipse.wst.common.component“配置文件。
这个文件是xml格式的配置文件，用语描述当前工程在部署的时候class文件的部署路径，用于生成war包或者tomcat调试的时候在临时文件夹下生成web工程目录结构。

```xml
<?xml version="1.0" encoding="UTF-8"?><project-modules id="moduleCoreId" project-version="1.5.0">
    <wb-module deploy-name="sys">
        <wb-resource deploy-path="/" source-path="/WebContent" tag="defaultRootSource"/>
        <wb-resource deploy-path="/WEB-INF/classes" source-path="/src"/>
        <wb-resource deploy-path="/WEB-INF/classes" source-path="/web"/>
        <wb-resource deploy-path="/WEB-INF/classes" source-path="/database"/>
        <wb-resource deploy-path="/WEB-INF/classes" source-path="/plugin"/>
        <wb-resource deploy-path="/WEB-INF/classes" source-path="/filter"/>
        <property name="context-root" value="sys"/>
        <property name="java-output-path" value="/sys/build/classes"/>
    </wb-module>
</project-modules>
```
其中的：
 deploy-path：表示部署路径
 source-path：表示源代码路径

这个内容和在IDE中设置的内容一一对应，所以插件帮助我么维护并且自动处理我们设置好的内容，简化我们开发。

综合起来.setting文件夹下存放的是eclipse的各种插件的配置文件，用于自动化处理。

##### (d) java web项目的基本结构 #####
javaee web项目的结构目录遵守javeee规范，要求必须的结构有：

```shell

```
在实际工程中除了这些必须的目录结构，还需要一些其他辅助的目录结构来帮助组织工程代码，举例如下：
```shell
web_project
------src (必须)
------src_resource(自建)
------WebRoot (必须)
-----------js (自建)
-----------css (自建)
-----------WEB-INF
----------------classes(必须)
----------------jsp(自建)
----------------lib(自建)
----------------web.xml(必须)
-----------META-INF
----------------MANIFEST.MF
-----------index.jsp
```

一般来说java的web开发工程都是按照这种目录结构组织的，然后通过war包等方式进行代码组织，最后部署到服务器容器中从而可以被访问。所以现在的代码结构也是按照这个形式进行处理的。

##### 综上所述 #####

eclipse工程通过.classpath和.project这两个文件进行基本描述，stackoverflow上有一个详细的说明[What's in an Eclipse .classpath/.project file?](http://stackoverflow.com/questions/7186676/whats-in-an-eclipse-classpath-project-file)：

```text
Eclipse is a runtime environment for plugins. Virtually everything you see in Eclipse is the result of plugins installed on Eclipse, rather than Eclipse itself.

The .project file is maintained by the core Eclipse platform, and its goal is to describe the project from a generic, plugin-independent Eclipse view. What's the project's name? what other projects in the workspace does it refer to? What are the builders that are used in order to build the project? (remember, the concept of "build" doesn't pertain specifically to Java projects, but also to other types of projects)

The .classpath file is maintained by Eclipse's JDT feature (feature = set of plugins). JDT holds multiple such "meta" files in the project (see the .settings directory inside the project); the .classpath file is just one of them. Specifically, the .classpath file contains information that the JDT feature needs in order to properly compile the project: the project's source folders (that is, what to compile); the output folders (where to compile to); and classpath entries (such as other projects in the workspace, arbitrary JAR files on the file system, and so forth).

Blindly copying such files from one machine to another may be risky. For example, if arbitrary JAR files are placed on the classpath (that is, JAR files that are located outside the workspace and are referred-to by absolute path naming), the .classpath file is rendered non-portable and must be modified in order to be portable. There are certain best practices that can be followed to guarantee .classpath file portability.
```

**从.java源代码到.class编译的VM执行程序这个过程不同的java工程没有任何区别，但是根据不同的协议来组织.class的结构形式产生了不同java项目。**

在学习java的时候一定要明白这个道理，才能掌握各种框架和工具的实现原理，把握住java开发中的设计思想和不同协议对项目架构带来的影响，才能抛开技术本身而关注与业务的逻辑和实现。

>**只有把握了技术的本质才能不受技术对思路的限制，否则拿着锤子看什么都是钉子。**

在目前工程开发的javaee项目的时候常用的还是已经配置了各种自动化工具的Eclipse IDE for Java EE Developers，只需要根据IDE提供的可视化插件来管理项目配置然后自动生成需要的文件就可以了，不用自己手动去组织代码的结构形式，方便和简化自己的开发。**但是一定不能因为有了自动化工具就忽视基本的组织原理，问题总是会出现，依赖工具并不能避免一切问题，一定要掌握原理。**

#### <2>java web相关的概念 ####
web开发是目前java的热点应用，这种现象的存是和java语言本身也有密切的关系的：

（1）Java是一种动态加载和运行的语言。也就是说当应用程序持有一个类的地址（CLASSPATH）和名称（包名和类名）的情况下，可以在程序运行期间任何时候加载这个类，并创建和使用该类的对象。

 （2）Java Servlet要求必须运行在Web服务器当中，与Web服务器之间属于分工和互补关系。确切的说，在实际运行的时候Java Servlet与Web服务器会融为一体，如同一个程序一样运行在同一个Java虚拟机（JVM）当中。
 
 （3）Servlet对每个请求都是单独启动一个线程，而不是进程。这种处理方式大幅度地降低了系统里的进程数量，提高了系统的并发处理能力。另外因为Java Servlet是运行在虚拟机之上的，也就解决了跨平台问题。
 
 （4）当Web容器接收到来自客户端的请求信息之后，会根据URL中的Web元件地址信息到Servlet队列中查找对应的Servlet对象，如果找到则直接使用，如果没有找到则加载对应的类，并创建对象。也就是说，Servlet对象是在第一次被使用的时候才创建的，并且一旦创建就会被反复使用，不再创建新的对象。所有创建出的Servlet对象会在Web服务器停止运行的时候统一进行垃圾回收。
 
 （5）为了解决客户端请求地址与Java Servlet之间对应关系问题，Web容器需要一个用来描述这种对应关系的文件，一般是web.xml文件。如果一个Web应用程序中存在很多个Servlet，那么web.xml会变得非常庞大。在Servlet 3.0规范推出之后，允许在Servlet代码中使用声明式语法来代替web.xml中的描述信息，这才让web.xml瘦身下来。下图是这个过程的一个示意图。

##### (a) web服务器 #####
Web服务器是可以向发出请求的浏览器提供文档的程序。web服务器作为互联网应用的基础平台，它实质上是一个网关，即介于多种协议之间的程序。
web服务器由两个部分组成：

>（1）HTTP守候程序（HTTPd），完成web服务器的基本功能，包括：和客户建立连接；接受客户提交的HTTP请求消息，将HTTP响应消息返回给客户，关闭连接等。狭义的web服务器就是指这个部分；
>（2）各种服务器端应用程序组成：这些应用程序作为HTTPd和其它外部系统之间的中介，完成服务器的扩展功能，平时所说的网关应用程序就是指这个部分。

也就是说：web服务器是一个只处理http协议，只给浏览器发送静态页面的应用程序；然后通过其他程序完成动态内容的处理，这些其他程序就是应用服务器。

在[Web 服务器与应用服务器的区别是什么？](http://www.zhihu.com/question/20096067)有一个清晰的说明：
>严格意义上Web服务器只负责处理HTTP协议，只能发送静态页面的内容。而JSP，ASP，PHP等动态内容需要通过CGI、FastCGI、ISAPI等接口交给其他程序去处理。这个其他程序就是应用服务器。
>比如Web服务器包括Nginx，Apache，IIS等。而应用服务器包括WebLogic，JBoss等。应用服务器一般也支持HTTP协议，因此界限没这么清晰。但是应用服务器的HTTP协议部分仅仅是支持，一般不会做特别优化，所以很少有见Tomcat直接暴露给外面，而是和Nginx、Apache等配合，只让Tomcat处理JSP和Servlet部分。

随着web技术的发展，web服务器和应用服务器之间的界限越来越小，这篇文章： [Web服务器的工作原理](http://www.importnew.com/15020.html) 就给出了很清晰的发展过程。

**注意：下文中所有的web应用服务器包含了基本web服务和应用服务，不再细分。**

##### (b) web容器 #####
web容器是web应用服务器中位于组件和平台之间的接口集合。
容器一般位于应用服务器之内，由应用服务器负责加载和维护。一个容器只能存在于一个应用服务器之内，一个应用服务器可以建立和维护多个容器。

>web容器为满足某种服务调用规范的框架，介于web应用服务器和web应用之间，他不一定是应用程序，也可以为一个进程。web容器为web应用提供了统一的访问接口，为web应用服务器提供了统一的管理接口，从而将web应用服务器和web程序解耦。web应用服务器通过创建不同的进程来实例化不同的web容器，一个web容器载入一个web应用，同一个web容器中使用线程来响应当前web程序的不同请求。

web应用服务器管理当前计算机中被指定的端口，所有按照一定协议访问这些端口的请求都会被web应用服务器接受处理，并且分发给不同的web应用，然后由不同的web应用程序返回请求结果，然后再有web应用服务器转换为对应的协议返回给访问者（一般而言是浏览器，当然也可以是应用程序，例如wget等）。

##### (c) java web容器 #####
在Java方面，web容器一般是指Servlet容器。Servlet容器是与Java Servlet交互的web容器的组件。
web容器负责管理Servlet的生命周期、把URL映射到特定的Servlet、确保URL请求拥有正确的访问权限和更多类似的服务。综合来看，Servlet容器就是用来运行你的Servlet和维护它的生命周期的运行环境。


##### (d) java servlet #####
明白了java开发中的web容器就等同于servlet容器，那么就需要理解java web开发中的核心概念java servlet。

Java Servlet就是你能够编写的根据请求动态生成内容的服务端组件。其主要功能在于交互式地浏览和修改数据，生成动态 Web 内容。
Java Servlet是一套规范，官方文档： [Java Servlet Technology](http://www.oracle.com/technetwork/java/index-jsp-135475.html) 给出了各个版本的规范要求（最新版本为3.0）。

狭义的 Servlet 是指Java语言根据servlet规范在javax.servlet包里定义的接口；
广义的 Servlet 是指任何根据servlet规范定义的接口的类；


java原生实现Servlet接口有3个实现类，FacesServlet、GenericServlet、HttpServlet：
 *FacesServlet类一般用于JSF的Servlet，很少使用。
 *GenericServlet是一个抽象类，有除了service()方法外的所有抽象方法的默认实现。
 *HttpServlet最常用，包含在javax.servlet.http.HttpServlet类中。

一般情况下，人们将 Servlet 理解为后者。比如Spring就使用了自己的DispatcherServlet来替换java原生的servlet从而完成更为复杂的管理。

java Servlet 运行于支持 Java 的应用服务器中，也就是说只要有JVM环境，java Servlet 就可以被支持。从实现上讲，Servlet 可以响应任何类型的请求，但绝大多数情况下 Servlet 只用来扩展基于 HTTP 协议的 Web 服务器。

java Servlet 是web容器最基本的组成单元，http请求是向web服务器请求一种信息资源，而servlet就充当了这种资源的最小单位，servlet可以无限扩展，使用java所有的类库资源，为用户返回文本、图片、音频的各类信息资源。

Java Servlet接口为Servlet的生命周期声明了三个基本方法——init()、service()和destroy()。
每个Servlet都要实现这些方法，并在它们的生命周期的特定时间由web容器来控制它的创建、初始化、提供服务、销毁等。它的各种行为方式通过web.xml文件中来配置。

由于Servlet是一个java接口，所以需要加载。这个加载动作有servlet容器完成，下面就结合上述的servlet容器和servlet来描述一下java web应用的整体行为。

##### (e) servlet容器的总体行为 #####
根据上述介绍可知，sevlet受控于另一个java应用程序，它就是web容器；sevlet没有main方法，说明它要被别的类web容器调用；servlet到web容器需要用xml文件注册，而xml的解析由web容器封装的方法完成。

整个java web应用启动过程为：
>**当Servlet容器启动时，它会部署并加载所有的web应用。当web应用被加载时，Servlet容器会一次性为每个应用创建Servlet上下文（ServletContext）并把它保存在内存里。Servlet容器会处理web应用的web.xml文件，并且一次性创建在web.xml里定义的Servlet、Filter和Listener，同样也会把它们保存在内存里。当Servlet容器关闭时，它会卸载所有的web应用和ServletContext，所有的Servlet、Filter和Listner实例都会被销毁。**

所以servlet容器管理整个Servlet的生命周期。整体上生命周期有4个阶段：加载、初始化、提供服务和销毁。

加载阶段是将请求的servlet类加载到java虚拟机中，这里需要通过公开的无参的构造方法来实例化，无没有则加载失败，也可以通过<load-on-startup>设置servlet在web容器启动时加载。当前web应用关闭的时候web容器会自动销毁当前所有的servlet。 上述这些过程都由web容器来控制，开发者关注最多的是初始化和提供服务两个阶段：
 *init()方法中，开发者可以获取配置在web.xml中的初始化参数；
 *service()方法会在Servlet请求时调用，处理业务逻辑。
通过这两个方法就完成了web应用的用户交互。

##### (f) java servlet容器配置 #####
在Servlet规范中定义了web.xml文件，它是Web应用的配置文件，Web.xml文件是和Web容器无关的。通过Web.xml文件可以配置Servlet类和url的映射、欢迎列表、过滤器以及安全约束条件等。
>xml文件的其他特点：对大小写敏感。

然后根据文档[web.xml Reference Guide for Tomcat](http://wiki.metawerx.net/wiki/Web.xml)说明：
```text
The web.xml Deployment Descriptor file describes how to deploy a web application in a servlet container such as Tomcat.
```
可以知道Web容器通过web.xml文件来配置web应用（但是这个web.xml文件并不是必须的）。web.xml这个文件是sun公司规范化格式的，用xml承载的web容器描述文件。

以工程中的实际配置为例，展示如下：
```xml
<?xml version="1.0" encoding="UTF-8"?>
<web-app version="2.5" xmlns="http://java.sun.com/xml/ns/javaee"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
	xsi:schemaLocation="http://java.sun.com/xml/ns/javaee http://java.sun.com/xml/ns/javaee/web-app_2_5.xsd">


    <!-- 上下文参数 -->
    <!-- the details for log4j in Spring can be found in Spring's API doc for Log4jConfigListener -->
    <context-param>
        <param-name>log4jConfigLocation</param-name>
        <param-value>/WEB-INF/config/log4j.properties</param-value>
    </context-param>
    <!-- if we set the log4jRefreshInterval, the system will re-read the log4j configuration regularly-->
    <context-param>
        <param-name>log4jRefreshInterval</param-name>
        <param-value>60000</param-value>
    </context-param>
    <!-- the manegementtool.root can be used in log4j configuration to point to this application. -->
    <context-param>
        <param-name>webAppRootKey</param-name>  
        <param-value>SpringWeb.root</param-value>  
    </context-param>
    <!-- The definition of the Root Spring Container shared by all Servlets and Filters -->
	<context-param>
		<param-name>contextConfigLocation</param-name>
		<param-value>/WEB-INF/spring/appContext.xml</param-value>
	</context-param>
    
	<!-- 设置监听器 -->
	<listener>
        <listener-class>org.springframework.web.util.Log4jConfigListener</listener-class>
    </listener>
 	<!-- Creates the Spring Container shared by all Servlets and Filters -->
	<listener>
		<listener-class>org.springframework.web.context.ContextLoaderListener</listener-class>
	</listener>
	
	<!-- 设置过滤器 -->
	<filter>
		<filter-name>sysAccessFilter</filter-name>
        <filter-class>
            org.springframework.web.filter.DelegatingFilterProxy
        </filter-class>
    </filter>
    <filter-mapping>
        <filter-name>sysAccessFilter</filter-name>
        <url-pattern>/*</url-pattern>
    </filter-mapping>
	
	 <!-- Spring servlet相关设置 -->
    <servlet>
		<servlet-name>appServlet</servlet-name>
		<servlet-class>org.springframework.web.servlet.DispatcherServlet</servlet-class>
		<init-param>
			<param-name>contextConfigLocation</param-name>
			<param-value>/WEB-INF/spring/servlet/servlet-context.xml</param-value>
		</init-param>
		<load-on-startup>2</load-on-startup>
	</servlet>
	<servlet-mapping>
		<servlet-name>appServlet</servlet-name>
		<url-pattern>/</url-pattern>
	</servlet-mapping>
    

	<!-- Processes application requests -->
    <session-config>
        <session-timeout>300</session-timeout>
    </session-config>
    <error-page>
        <exception-type>java.lang.Exception</exception-type>
        <location>/WEB-INF/views/error.jsp</location>
    </error-page>
    <error-page>
        <error-code>404</error-code>
        <location>/WEB-INF/views/error.jsp</location>
    </error-page>

</web-app>
```
首先我们先理解这个web.xml文档描述了那些内容，然后分析web容器怎么根据这个配置文件对整个web进行部署。

###### 1 web.xml文档内容分析 ######
（1）xml文件头：
因为web.xml也是xml标准格式，所以需要一个xml头来说明当前的xml承载格式的信息：
```xml
<?xml version="1.0" encoding="UTF-8"?>
```
指定了xml的版本号和所使用的编码。

（2）文件类型定义DTD：
DTD（Document Type Definition），即文档类型定义,是一种XML约束模式语言，是XML文件的验证机制，属于XML文件组成的一部分。
是一种保证XML文档格式正确的有效方法，可以通过比较XML文档和DTD文件来看文档是否符合规范，元素和标签使用是否正确。

一个典型的DTD文档包含：元素的定义规则，元素间关系的定义规则，元素可使用的属性，可使用的实体或符号规则。从而把要创作的XML文档的元素结构、属性类型、实体引用等预先进行规定。
用户既可以直接在XML文档中定义DTD，也可以通过URL引用外部的DTD。DTD为XML文档的编写者和处理者提供了共同遵循的原则，使得与文档相关的各种工作有了统一的标准。一般内容如下：
```xml
<!DOCTYPE
	web-app 
	PUBLIC
	"-//Sun Microsystems, Inc.//DTD Web Application 2.3//EN"
	http://java.sun.com/dtd/web-app_2_3.dtd >
```
其中包含四个元素，各自的含义如下：

>web-app定义该文档(部署描述符，不是DTD文件)的根元素

>PUBLIC意味着DTD文件可以被公开使用

>"-//Sun Microsystems, Inc.//DTD Web Application 2.3//EN"意味着DTD 由Sun Microsystems, Inc.维护。 该信息也表示它描述的文档类型是DTD Web Application 2.3，而且DTD是用英文书写的。

>URL"http://java.sun.com/dtd/web-app_2_3.dtd" 表示D文件的位置。

（3）文档类型定义XSD：
根据文档[web.xml - DTD and XSD](http://wiki.metawerx.net/wiki/Web.xmlDTDAndXSD)所述：
```text
There are two ways to specify the schema for a the web.xml file (Deployment Descriptor).
DTD - Document Type Definition
XSD - XML Schema Definition
```
自JSP 2.0 / Servlets 2.4规范之后，使用XSD（XML Schema Definition）的方式对当前xml文档进行规范描述，而不再使用DTD。

根据文档：[Why Use XML Schemas?](http://www.w3schools.com/schema/schema_why.asp)可知XSD也是为了描述了XML文档的结构。
XSD可以用一个指定的XML Schema来验证某个XML文档，以检查该XML文档是否符合其要求。文档设计者可以通过XML Schema指定一个XML文档所允许的结构和内容，并可据此检查一个XML文档是否是有效的。XML Schema本身是一个XML文档，它符合XML语法结构。可以用通用的XML解析器解析它。 

XSD是DTD替代者的原因：
**一是据将来的条件可扩展，二是比DTD丰富和有用，三是用XML书写，四是支持数据类型，五是支持命名空间。**

现在总结对比两者如下：
```text
DTD的缺点：
 1) DTD 是使用非 XML 语法编写的
 2) DTD 不可扩展,不支持命名空间,只提供非常有限的数据类型
相对于DTD，XSD的优点: 
 1) XML Schema基于XML,没有专门的语法 
 2) XML Schema可以象其他XML文件一样解析和处理 
 3) XML Schema比DTD提供了更丰富的数据类型
 4) XML Schema提供可扩充的数据模型
 5) XML Schema支持综合命名空间
 6) XML Schema支持属性组
```

根据上述内容介绍可知XSD可以通过读入不同的XSD配置来进行格式规范，给出一个 JSP 2.2 / Servlets 3.0 (Tomcat 7.0)规范的示例：

```xml
<web-app xmlns="http://java.sun.com/xml/ns/javaee"
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xsi:schemaLocation="http://java.sun.com/xml/ns/javaee http://java.sun.com/xml/ns/javaee/web-app_3_0.xsd"
      version="3.0">
```

本工程中通过引入web-app2.5的XSD规范来检查web.xml，所以具体的内容如下：

```xml
<web-app version="2.5" xmlns="http://java.sun.com/xml/ns/javaee"
	xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
	xsi:schemaLocation="http://java.sun.com/xml/ns/javaee http://java.sun.com/xml/ns/javaee/web-app_2_5.xsd">
```
>web-app_2_5.xsd这个规范，还是比较早的了（属于Java EE 5 Schema Resources）。
每个web.xml文件的根元素<web-app>中，都必须标明这个 web.xml使用的是哪个模式文件。所以XSD不再单独的放在web-app元素之前，而是作为web-app的一个配置属性存在了。

>参考文档：
[XML Schema 与 XML DTD的技术比较与分析](http://www.ibm.com/developerworks/cn/xml/x-sd/)
[XML中DTD,XSD的区别与应用](http://kim-miao.iteye.com/blog/1310963)
[Java EE: XML Schemas for Java EE Deployment Descriptors](http://www.oracle.com/webfolder/technetwork/jsc/xml/ns/javaee/index.html)
[web.xml - DTD and XSD](http://wiki.metawerx.net/wiki/Web.xmlDTDAndXSD)

（4）web-app元素：
部署描述符的根元素是web-app，其它的元素都放在<web-app></web-app>之中。所有和部署相关的配置都是作为web-app的子元素标签在web.xml中被描述。web-app作为他的子元素的统一声明存在。

（4）web-app的配置子元素：
现在将最常用的几个子元素进行分析。

1. <context-param>上下文参数：
context-param元素声明应用范围内的初始化参数。它用于向 ServletContext提供键值对，即应用程序上下文信息。
我们的listener,filter等在初始化时会用到这些上下文中的信息。在servlet里面可以通过getServletContext().getInitParameter("context/param")得到。
context-param元素含有一对参数名和参数值，用作应用的servlet上下文初始化参数。参数名在整个Web应用中必须是惟一的。
param-name子元素包含有参数名，而param-value子元素包含的是参数值。作为选择，可用description子元素来描述参数。
```xml
<context-param>
	<param-name>contextConfigLocation</param-name>
	<param-value>/WEB-INF/spring/appContext.xml</param-value>
</context-param>
```

2. <filter>过滤器：
filter元素用于指定Web容器中的过滤器。在请求和响应对象被servlet处理之前或之后，可以使用过滤器对这两个对象进行操作。
利用filter-mapping元素，过滤器被映射到一个servlet或一个URL模式。这个过滤器的filter元素和filter-mapping元素必须具有相同的名称。

```xml
<filter>
    <filter-name>sysAccessFilter</filter-name>
    <filter-class>
       org.springframework.web.filter.DelegatingFilterProxy
    </filter-class>
</filter>
```
**在请求和响应对象被servlet处理之前或之后，可以使用过滤器对这两个对象进行操作。**

3. <filter-mapping>关联元素：
filter-mapping元素用来声明Web应用中的过滤器映射。过滤器可被映射到一个servlet或一个URL模式。将过滤器映射到一个servlet中会造成过滤器作用于servlet上。将过滤器映射到一个URL模式中则可以将过滤器应用于任何资源，只要该资源的URL与URL模式匹配。过滤是按照部署描述符的filter-mapping元素出现的顺序执行的。

```xml
<filter-mapping>
    <filter-name>sysAccessFilter</filter-name>
    <url-pattern>/*</url-pattern>
</filter-mapping>
```
其中filter-name值必须对应filter元素中声明的其中一个过滤器名称。

4. <listener>监听器：
listener元素用来注册一个监听器类，可以在Web应用中包含该类。使用listener元素，可以收到事件什么时候发生以及用什么作为响应的通知。
也就是说，listener元素和servlet元素类似，用于在Web 应用启动时，启动某些后台程序，这些后台程序负责为系统运行提供支持。

使用Listener 只需要两个步骤:
 (1)创建Listener 实现类：
 创建Listener 类必须实现ServletContextListener 接口，该接口包含两个方法。
  • contextInitialized(ServletContextEvent sce): 启动Web 应用时，系统调用该Filter的方法。
  • contextDestroyed(ServletContextEvent sce): 关闭Web 应用时候，系统调用Filter的方法。
 (2)在web.xml 文件中配置Listener：
 **Listener 用于启动Web 应用的后台服务程序，但不负责处理及响应用户请求，因此无须配置URL。**
 若将Listener 配置在Web 容器中(如果Web 容器支持Listener)，则Listener 将随 Web 应用的启动而启动。配置Listener 时使用<listener/>元素，下面是配置Listener 的片段:
```xml
<listener>
    <listener-class>org.springframework.web.context.ContextLoaderListener</listener-class>
</listener>
```

5. <servlet>元素：
<servlet></servlet> 用来声明一个servlet的数据，主要有以下子元素：

```xml
<servlet-name></servlet-name>		指定servlet的名称
<servlet-class></servlet-class>		指定servlet的类名称
<init-param></init-param>			用来定义servlet的初始化参数，可有多个init-param。在servlet类中通过getInitParamenter(String name)方法访问初始化参数
<jsp-file></jsp-file>				指定web站台中的某个JSP网页的完整路径
<load-on-startup></load-on-startup>	指定当Web应用启动时，装载Servlet的次序。当值为正数或零时：Servlet容器先加载数值小的servlet，再依次加载其他数值大的servlet。当值为负或未定义：Servlet容器将在Web客户首次访问这个servlet时加载它。
<servlet-mapping></servlet-mapping>	用来定义servlet所对应的URL，包含两个子元素
<url-pattern></url-pattern>			指定servlet所对应的URL
```
servlet元素必须含有servlet-name元素和servlet-class元素，或者servlet-name元素和jsp-file元素。描述如下：
 ● servlet-name元素用来定义servlet的名称，该名称在整个应用中必须是惟 一的。
 ● servlet-class元素用来指定servlet的完全限定的名称。
 ● jsp-file元素用来指定应用中JSP文件的完整路径。这个完整路径必须由a/ 开始。

当启动Web容器时，用load-on-startup元素自动将servlet加入内存。加载servlet就意味着实例化这个servlet， 并调用 它的init方法。可以使用这个元素来避免第一个servlet请求的响应因为servlet载入内存所导致的任何延迟。如果load-on- startup元素存在，而且也指定了jsp-file元素，则JSP文件会被重新编译成servlet，同时产生的servlet也被载入内存。  load-on-startup元素的内容可以为空，或者是一个整数。这个值表示由Web容器载入内存的顺序。举个例子，如果有两个servlet元素都 含有load-on-startup子元素，则load-on-startup子元素值较小的servlet将先被加载。如果load-on- startup子元素值为空或负值，则由Web容器决定什么时候加载servlet(在该servlet被选择时才加载)。如果两个servlet的load-on-startup子元素值相同，则由Web容器决定先加载哪一个servlet。

servlet-mapping元素将URL模式映射到某个servlet。根据规范，**servlet元素必须出现在所有servlet-mapping元素之前。**

6. <session-config>会话超时配置：
单位为分钟。项目中的配置如下：
```xml
<session-config>
    <session-timeout>300</session-timeout>
</session-config>
```

7. <error-page>错误页面
主要用于处理请求发生错误的时候跳转页面的设置，示例如下：
```xml
<!-- 1、通过错误码来配置error-page。当系统发生×××错误时，跳转到错误处理页面。 -->
<error-page>
    <error-code>404</error-code>
    <location>/NotFound.jsp</location>
</error-page>
<!-- 2、通过异常的类型配置error-page。当系统发生java.lang.NullException（即空指针异常）时，跳转到错误处理页面。 -->
<error-page>
    <exception-type>java.lang.NullException</exception-type>
    <location>/error.jsp</location>
</error-page>
```

上述就是web-app的常用元素，更多详细内容参看[官方文档](http://docs.oracle.com/cd/E13222_01/wls/docs81/webapp/web_xml.html)。
>参考文档：
[web.xml文件中的web-app元素](http://blog.sina.com.cn/s/blog_490cc6450100h54p.html)
[web.xml文件详解](http://www.cnblogs.com/hellojava/archive/2012/12/28/2835730.html)
[tomcat web.xml配置](http://www.blogjava.net/baoyaer/articles/107428.html)
[web.xml详解——元素含义及加载顺序](http://wenku.baidu.com/view/4d155464f5335a8102d22039.html)
[web.xml Reference Guide for Tomcat](http://wiki.metawerx.net/wiki/Web.xml)

###### 2 web容器加载web.xml的顺序 ######
tomcat等web容器在初始化web应用的时候首先会加载web.xml文件进行这个web应用的初始化配置，那么就需要根据web.xml中的配置进行加载，具体的加重顺序为：
** context-param -> listener -> filter -> servlet **

也就是说上述元素在web.xml中的定义顺序和加载顺序是无关的。但是**凡是涉及到mapping的都会顺序相关**，例如listener，filter和servlet的加载顺序就是按照定义顺序执行的。

