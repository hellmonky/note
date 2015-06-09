# 深圳开发 #

**hellmonky**

## 1 环境搭建 ##

当前深圳的所有项目开发采用javaEE组件方式进行，代码在svn进行版本管理，并且使用了Spring和myBatis进行数据库封装。
当前已经根据omni项目完成了一个程序框架，这个也是本次出差的主要学习任务和目的，所以需要在最短的时间内掌握当前开发环境的搭建和部署，并且熟悉当前已经搭建好的框架进行后续开发。

### （1） 相关的环境下载和设置 ###
当前还是基于javaEE开发web项目，并且统一管理数据库，相关的软件如下：

>对于已经使用过的软件和框架没有给出具体下载地址

（1）JDK：1.7以上版本，基本开发环境；
（2）tomcat：web服务容器
（3）[Spring Framework](http://repo.spring.io/libs-release-local/org/springframework/spring/)：综合框架，负责业务模型的建立；
（4）[MyBatis](http://mybatis.github.io/)：ORM框架，轻量级数据库持久化框架（本项目中没有使用hibernate框架）；
（5）MySQL：数据库支持，使用开源的数据库完成基本数据存储；
（6）[STS](http://spring.io/tools/sts)：eclipse的Spring框架封装，方便快速开发；
（7）svn：当前的开发环境中使用svn管理不同版本的代码，所以需要安装svn管理工具。
（8）IDEA（可选）：另一个java IDE和eclipse的使用习惯不同，但是开发中的补全支持是很突出的。

当前使用上述的软件并且安装就可以搭建好几本的开发环境了。但是在开发中还需要一些其他第三方库的支持，会在后续的开发中逐步记录。

### （2） 基本环境设置 ###
上述环境安装的时候需要注意事项：
（1）tomcat的运行需要在环境变量中查找JAVA_HOME是否存在，该环境变量存储当前jdk的根目录，一般为：

```shell
C:\Program Files\Java\jdk1.8.0_45
```
完成这个环境变量的设置之后就可以启动tomcat的web服务支持了。

（2）IDEA中使用svn命令行工具：
因为IDEA只使用svn的命令行工具，所以在安装svn的时候一定要勾选svn的命令行支持。

（3）当前主要使用的框架为Spring和mybatis：
后台数据库的持久化和bean的转换使用mybatis来完成，后台业务层和前台交互使用Spring来完成封装，整体框架已经搭建起来了，有很多的配置是非常需要注意学习的。

（4）MySQL数据库的使用：
还是使用5.6版本的非安装MySQL数据库，然后使用navicat进行可视化管理，这儿需要注意的有：

#### <1> MySQL数据库的远程备份和恢复 ####
使用MySQL来进行数据库操作是非常方便的，并且navicat等管理工具都是建立在基本功能的封装上的，所以首先要学会基本工具的使用。例如为了数据安全需要将远程数据库的内容备份到本地进行测试，所有需要对使用命令行工具mysqldump来完成这个功能：

```shell
mysqldump -P [端口号] -h [IP地址] -u [用户名] -p [表名]>[导出数据库脚本的绝对路径]
```
这样操作的时候需要在命令行中输入密码，如果不考虑安全性因素，使用如下命令：

```shell
mysqldump -h[hosname] -u[user_name] -p[password] --default-character-set=n[char_set_name] [db_name] > [save_path]
```
使用如下的链接信息进行测试：
>IPAD:172.16.1.242
>PORT:3306
>USER:root
>PASS:P@55w0rd
>TABL:sys

实际测试代码为：

```shell
D:\dev\mysql-5.6.21-winx64\bin>mysqldump.exe -P 3306 -h 172.16.1.242 -u root -p
sys > d:\sys.sql

mysqldump -h172.16.1.242 -uroot -pP@55w0rd --default-character-set=utf8 sys --skip-lock-tables> d:\sys.sql
```

而且MySQL从本地导入到远程数据库的命令和上面的唯一区别就在于>符号变为了<符号，表示不同的数据流方向。

#### <2> SQL的记录 ####
因为底层查询使用的内容都是和业务相关的，所以需要根据业务逻辑将查询内容记录下来，这样在后续文档的生成和自己重构代码的时候才方便自己回顾和优化查询代码，这一点是非常重要的，所以后续内容一定要按照开发流程和逻辑整理SQL代码。

### （3） 基本框架了解 ###
因为当前是属于接手开发，所以现在的架构都是已经定型了的，需要做的就是快速的熟悉整体架构，并且理解当前架构处于什么样的业务模型的情况下来选择的，这样做的优势和劣势是什么，多一点整体形势的思考和分析。

## 2 整体架构分析 ##

整体框架搭建还是建立在MVC的基础上的，并且对后台部分按照层次来定义调用接口，这样完成协作和同步开发，并且将测试与实现隔离，方便代码检查。现在就当前已经有的代码架构做一个简单的分析。

总体上整个框架的构成为：
>分层设计->定义业务接口->分module进行设计和实现->细化实现

通过约定接口调用来约束不同层级之间的分离关系。从而分散整体工作，并行开发。
通过规定整个框架的基本代码组织关系：
优点：可以自动生成并且维护代码，代码层次结构固定统一，方便其他人合作开发；
缺点：一旦架构确定下来之后可变性受限，并且增加了学习成本，不容易让新的开发人员熟悉整个流程。

**现在通过从eclipse来作为基础搭建整个环境，熟悉和使用当前使用的各种框架，并且了解整体架构的特点所在。**

### （1）后台数据的持久化 ###

使用mybatis做为ORM框架来对数据库表和java对象之间去耦，并且通过配置xml文件来实现java对象的自动生成功能。
根据[官方文档](http://mybatis.github.io/mybatis-3/)给出的介绍
>MyBatis 是支持定制化 SQL、存储过程以及高级映射的优秀的持久层框架。

主要使用方式是通过配置xml文件来将数据库中的表映射为JOBO(Plain Old Java Objects)来完成去耦，对上层提供统一的操作方法。

#### <1> mybatis基本配置 ####
将下载到的库中的mybatis-x.x.x.jar 文件置于 classpath 中即可进行基本配置了。

##### (a) 配置基本generatorConfig.xml #####
generatorConfig.xml文件是mybatis的核心配置文件，包含获取数据库连接实例的数据源（DataSource）和决定事务范围和控制方式的事务管理器（TransactionManager）。
mybatis通过这个文件完成项目中的自动生成工作。这个文件的内容如下：

```xml
<?xml version="1.0" encoding="UTF-8" ?>

<!DOCTYPE configuration
  PUBLIC "-//mybatis.org//DTD Config 3.0//EN"
  "http://mybatis.org/dtd/mybatis-3-config.dtd">

<configuration>
  <environments default="development">
    <environment id="development">
      <transactionManager type="JDBC"/>
      <dataSource type="POOLED">
        <property name="driver" value="${driver}"/>
        <property name="url" value="${url}"/>
        <property name="username" value="${username}"/>
        <property name="password" value="${password}"/>
      </dataSource>
    </environment>
  </environments>
  
  <mappers>
    <mapper resource="org/mybatis/example/BlogMapper.xml"/>
  </mappers>
</configuration>
```
整体上分为两个个部分：xml文件头（用来验证xml文件的正确性）和整体配置信息（用来说明具体的配置细节）。
其中整体配置信息为configuration体，又包含多个具体的配置信息，包括：
> environment 元素体中包含了事务管理和连接池的配置。
> mappers 元素则是包含一组 mapper 映射器（这些 mapper 的 XML 文件包含了 SQL 代码和映射定义信息）。
>>更为详细的说明参看[官方文档配置说明部分](http://mybatis.github.io/mybatis-3/zh/configuration.html)。

##### (b) 配置mapper映射关系 #####
mapper映射关系描述了结果集ResultMap和SQL映射关系等内容，其中SQL映射关系包含有以下顶级元素：

```xml
cache –     给定命名空间的缓存配置。
cache-ref – 其他命名空间缓存配置的引用。
resultMap – 是最复杂也是最强大的元素，用来描述如何从数据库结果集中来加载对象。
sql –       可被其他语句引用的可重用语句块。
insert –    映射插入语句
update –    映射更新语句
delete –    映射删除语句
select –    映射查询语句
```

通过这些顶级标签元素就可以在xml中对标准SQL进行描写，从而完成后续动态SQL生成等高级功能，使得查询的调整在接口调用规定的基础上不再影响上层业务实现，从而去除耦合关联。
>更多的映射细节查看[官方文档](http://mybatis.github.io/mybatis-3/zh/sqlmap-xml.html#)

##### (c) 总体配置流程 #####

在上面的总体配置中，最为关键的就是mappers元素中说明的bean结构映射关系，这个文件中说明了基本的SQL语句的映射关系，将传统SQL转换为函数调用。
具体需要按照如下步骤来完成配置和关联：
（1）创建数据库表：根据业务需要完成数据库设计之后，在数据库中create table，完成这一步骤；
（2）创建java实体类：在工程中根据基本的table结构完成java bean的编写（类似C语言中的结构体，但是java中的一切都是对象，也就是class，所以这儿就是java bean了），并且添加基本的set和get函数；
（3）创建dao接口：在完成java实体类之后，因为这个类表示了数据库表的抽象，所以还需要添加基本的增删查改操作，也就是一般意义的dao层，也就是mybatis中所谓的Mapper，表示了对抽象数据库表的基本操作；
（4）创建dao的实现：和hibernate不同，mybatis使用一个xml文件来描述所谓的[映射语句](http://mybatis.github.io/mybatis-3/zh/sqlmap-xml.html)，通过这个xml文件中不同标签的设置，实现对数据库表的基本操作；
（5）装载映射文件：完成上述步骤之后，需要在mybatis的总体配置文件的mapper标签下添加（4）中实现的所有xml文件，只有这样才能将（1）和（2）分别代表的数据库表项实体和java程序中的基本对象这两个内容链接在一起，完成ORM框架中的去耦作用。

通过上述5个步骤，mybatis就完成了从数据库表实体到java对象的关联配置。可以在java程序中使用（2）中定义的实体类来根据在（4）中定义的同名xml中实现的基本操作来对据库表进行处理了。

#### <2> mybatis的java调用 ####
mybatis通过上述配置就可以完成java对象和数据库表的管理，接下来就需要通过java调用来穿件session完成数据库操作了。
但是因为本项目中使用Spring来进行session管理，所以现在这一部分内容暂时没有考虑，不过官方网站给出了很详细的例子，可以参考学习。
总体上来说还是通过sqlsession来对后台数据操作进行管理，具体代码示例如下：

```java
package com.forum.test;

import java.io.IOException;

import org.apache.ibatis.io.Resources;
import org.apache.ibatis.session.SqlSession;
import org.apache.ibatis.session.SqlSessionFactory;
import org.apache.ibatis.session.SqlSessionFactoryBuilder;

import com.forum.dao.UserMapper;
import com.forum.po.User;

/**
 * myBatis数据库连接测试
 * 
 * @author db2admin
 * 
 */
public class MyBatisTest {
	/**
	 * 获得MyBatis SqlSessionFactory  
	 * SqlSessionFactory负责创建SqlSession，一旦创建成功，就可以用SqlSession实例来执行映射语句，commit，rollback，close等方法。
	 * @return
	 */
	private static SqlSessionFactory getSessionFactory() {
		SqlSessionFactory sessionFactory = null;
		String resource = "configuration.xml";
		try {
			sessionFactory = new SqlSessionFactoryBuilder().build(Resources
					.getResourceAsReader(resource));
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return sessionFactory;
	}

	public static void main(String[] args) {
		SqlSession sqlSession = getSessionFactory().openSession();
		UserMapper userMapper = sqlSession.getMapper(UserMapper.class);
		User user = userMapper.findById("1");
		System.out.println(user.getName());

	}

}
```

#### <3> mybatis的自动生成 ####
根据上面的流程，对于数据库表的java对象表示而言，每一个表都要手动写xml描述和dao的xml描述，这个是非常麻烦的事情，所以mybatis提供了自动生成工具：[MyBatis Generator](http://mybatis.github.io/generator/)，简称为MBG。通过建立起这个工具需要的配置文件，我们就能自动根据数据库中的表来生成bean结构以及mapper配置文件。

##### (a)mybatis 自动生成的配置 #####

MBG需要一个xml格式的配置文件来描述，这个配置文件非常详细的给定了自动生成所需要的一切，在网上有一篇非常好：[MyBatis Generator 详解](http://blog.csdn.net/isea533/article/details/42102297)可以参看。
这是一个简单的配置示例：

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE generatorConfiguration
  PUBLIC "-//mybatis.org//DTD MyBatis Generator Configuration 1.0//EN"
  "http://mybatis.org/dtd/mybatis-generator-config_1_0.dtd">

<generatorConfiguration>
    <classPathEntry location="mysql-connector-java-5.1.6-bin.jar" />

    <context id="DB2Tables" targetRuntime="MyBatis3">

        <commentGenerator>
            <property name="suppressDate" value="true" />
        </commentGenerator>

        <jdbcConnection driverClass="com.mysql.jdbc.Driver"
            connectionURL="jdbc:mysql://localhost/test" userId="qgd" password="123456">
        </jdbcConnection>

        <javaTypeResolver>
            <property name="forceBigDecimals" value="false" />
        </javaTypeResolver>

        <javaModelGenerator targetPackage="test.model"
            targetProject="../src/main/java">
            <property name="enableSubPackages" value="true" />
            <property name="trimStrings" value="true" />
        </javaModelGenerator>

        <sqlMapGenerator targetPackage="test.dao"
            targetProject="../src/main/java">
            <property name="enableSubPackages" value="true" />
        </sqlMapGenerator>

        <javaClientGenerator type="XMLMAPPER"
            targetPackage="test.dao" targetProject="../src/main/java">
            <property name="enableSubPackages" value="true" />
        </javaClientGenerator>

        <table tableName="pet" domainObjectName="Pet">
        </table>

    </context>
</generatorConfiguration>
```
这个配置文件提供了 mybatis-generator所需要的参数信息：
  * 其中classPathEntry 是引用的jdbc的类路径，这里将jdbc jar和generator的jar包放在一起了；
  * commentGenerator 是用来除去时间信息的，这在配合类似subversion的代码管理工具时使用很有效，因为可以减少没有必要的注释迁入；
  * jdbcConnection是指定的jdbc的连接信息；
  * javaTypeResolver式类型转换的信息，这里并没有用到；
  * javaModelGenerator是模型的生成信息，这里将指定这些Java model类的生成路径；
  * sqlMapGenerator是mybatis 的sqlMapper XML文件的生成信息，包括生成路径等；
  * javaClientGenerator是应用接口的生成信息；
  * table是用户指定的被生成相关信息的表，它必须在指定的jdbc连接中已经被建立。


下面结合上面的例子和工程中实际用到的内容进行分析：

###### 1 配置头文件 ######
就是xml文件头，描述xml的格式情况。具体内容如下：

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE generatorConfiguration
        PUBLIC "-//mybatis.org//DTD MyBatis Generator Configuration 1.0//EN"
        "http://mybatis.org/dtd/mybatis-generator-config_1_0.dtd">
```

###### 2 generatorConfiguration根节点 ######
generatorConfiguration节点没有任何属性，直接写节点即可，如下：

```xml
<generatorConfiguration>
    <!-- 具体配置内容 -->
</generatorConfiguration>
```

###### 3 根节点下的配置元素 ######
从这段开始，就是配置的主要内容，这些配置都是generatorConfiguration元素的子元素。包含以下子元素（有严格的顺序）：
<properties> (0个或1个)
<classPathEntry> (0个或多个)
<context> (1个或多个)

####### 3.1 properties元素 #######
这个元素用来指定外部的属性元素，不是必须的元素。
这个元素用于指定一个需要在配置中解析使用的外部属性文件，引入属性文件后，可以在配置中使用 ${property}这种形式的引用，通过这种方式引用属性文件中的属性值。对于后面需要配置的**jdbc**信息和targetProject属性会很有用。
这个属性可以通过resource或者url来指定属性文件的位置，这两个属性只能使用其中一个来指定，同时出现会报错。
>resource：指定**classpath**下的属性文件，使用类似com/myproject/generatorConfig.properties这样的属性值。

>url：可以指定文件系统上的特定位置，例如file:///C:/myfolder/generatorConfig.properties

####### 3.2 classPathEntry元素 #######
这个元素的作用是将MBG运行时需要用到的jar包(或zip格式)添加到**classpath**下。
最常见的用法是，当**classpath**下面**没有**JDBC驱动的时候，我们通常通过这个属性指定驱动的路径，例如：
```xml
<classPathEntry location="E:\mysql\mysql-connector-java-5.1.29.jar"/>
```
如果需要用到其他的jar包，也可以这么配置，例如如果你开发了一个MBG的插件，你就可以通过这种方式加入到**classpath**
>这里注意上面重点强调的**没有**，一般在项目中使用的时候，**classpath**下面都有JDBC驱动，因此从项目中启动的时候不需要配置该项。
>>建议:由于该参数使用了绝对路径，因此不利用在不同电脑上通用，因此建议最好把需要的jar包放到项目的**classpath**下，避免每个人都得单独配置路径。

####### 3.3 context元素 #######
在MBG的配置中，至少需要有一个<context>元素。<context>元素用于指定生成一组对象的环境。例如指定要连接的数据库，要生成对象的类型和要处理的数据库中的表。运行MBG的时候还可以指定要运行的<context>。
该元素只有一个**必选属性**id，用来唯一确定一个<context>元素，该id属性可以在运行MBG的使用。
在这个元素下面还有一些非常必要的子元素来进行配置：
这些子元素（有严格的配置顺序）包括：
><property> (0个或多个)
><plugin> (0个或多个)
><commentGenerator> (0个或1个)
><jdbcConnection> (1个)
><javaTypeResolver> (0个或1个)
><javaModelGenerator> (1个)
><sqlMapGenerator> (0个或1个)
><javaClientGenerator> (0个或1个)
><table> (1个或多个)


##### (b) mybatis generator的自动生成 #####
完成上述基本配置之后，mybatis需要根据这个配置文件生成相应的代码，mybatis generator有三种方式可以完成自动生成：命令行，eclipse插件和maven插件。
>[官方文档](http://mybatis.github.io/generator/running/running.html)有详细的说明
下面就针对这三种方式进行讲解。

###### 1 命令行下的自动生成 ######
mybatis generator最简单的就是命令行的方式，只需要指定相应的配置文件的路径即可：

```shell
java -jar /path/to/mybatis-generator-core-xxx.jar -configfile /path/to/config.xml -overwrite
```
将上述命令中的mybatis-generator-core和config.xml的路径替换为实际路径就可以生成了。

###### 2 eclipse集成插件完成自动生成 ######
eclipse提供了一个插件，安装之后可以对xml文件直接生成代码。现在简述安装过程如下：
打开eclipse的menu
```menu
help->install new software
```
然后输入如下地址：
```address
http://mybatis.googlecode.com/svn/sub-projects/generator/trunk/eclipse/UpdateSite/
```
然后点击“Add”添加，安装插件就可以了。
重启之后右键点击xml配置文件就可以根据配置生成代码了。

>**注意：**eclipse中手动新建的目录和工程中添加的目录是不同的，因为工程中添加的目录会修改当前工程的.classpath文件，在其中进行文件夹的管理。
>自己手动新建的文件夹在mybatis generator自动生成的时候回找不到路径而报错。因为没有在工程中进行管理。

###### 3 maven插件安装 ######
maven作为包java的管理器是非常好的，但是自己在实际工程中使用不多，现在简述一下插件安装过程感受一下。
参考[利用mybatis-generator自动生成代码](http://www.cnblogs.com/yjmyzz/p/mybatis-generator-tutorial.html)这一篇博文来学习。



##### (c)使用mybatis自动生成的代码 #####
完成自动生成之后，最核心的问题就是如何使用mybatis生成的这些代码组织自己的工程了。
**工具只是手段，问题的解决才是核心**


### （2）java web框架 ###

>java的web开发需要一些基本概念，这是自己现在没有的，需要进行学习和了解，本节内容就针对当前使用的java web开发基础知识进行学习。

web开发是目前java的热点应用，这种现象的存是和java语言本身也有密切的关系的：
（1）Java是一种动态加载和运行的语言。也就是说当应用程序持有一个类的地址（CLASSPATH）和名称（包名和类名）的情况下，可以在程序运行期间任何时候加载这个类，并创建和使用该类的对象。
（2）Java Servlet要求必须运行在Web服务器当中，与Web服务器之间属于分工和互补关系。确切的说，在实际运行的时候Java Servlet与Web服务器会融为一体，如同一个程序一样运行在同一个Java虚拟机（JVM）当中。
（3）Servlet对每个请求都是单独启动一个线程，而不是进程。这种处理方式大幅度地降低了系统里的进程数量，提高了系统的并发处理能力。另外因为Java Servlet是运行在虚拟机之上的，也就解决了跨平台问题。
（4）当Web容器接收到来自客户端的请求信息之后，会根据URL中的Web元件地址信息到Servlet队列中查找对应的Servlet对象，如果找到则直接使用，如果没有找到则加载对应的类，并创建对象。也就是说，Servlet对象是在第一次被使用的时候才创建的，并且一旦创建就会被反复使用，不再创建新的对象。所有创建出的Servlet对象会在Web服务器停止运行的时候统一进行垃圾回收。
（5）为了解决客户端请求地址与Java Servlet之间对应关系问题，Web容器需要一个用来描述这种对应关系的文件，一般是web.xml文件。如果一个Web应用程序中存在很多个Servlet，那么web.xml会变得非常庞大。在Servlet 3.0规范推出之后，允许在Servlet代码中使用声明式语法来代替web.xml中的描述信息，这才让web.xml瘦身下来。下图是这个过程的一个示意图。

#### <1> java web基本概念 ####
java工程和java web工程有什么区别？为什么要分为javaee

##### (a)web服务器 #####
Web服务器是可以向发出请求的浏览器提供文档的程序。web服务器作为互联网应用的基础平台，它实质上是一个网关，即介于多种协议之间的程序。
web服务器由两个部分组成：

>HTTP守候程序（HTTPd），完成web服务器的基本功能，包括：和客户建立连接；接受客户提交的HTTP请求消息，将HTTP响应消息返回给客户，关闭连接等。狭义的web服务器就是指这个部分；

>各种服务器端应用程序组成：这些应用程序作为HTTPd和其它外部系统之间的中介，完成服务器的扩展功能，平时所说的网关应用程序就是指这个部分。

也就是说：web服务器是一个只处理http协议，只给浏览器发送静态页面的应用程序；然后通过其他程序完成动态内容的处理，这些其他程序就是应用服务器。

在[Web 服务器与应用服务器的区别是什么？](http://www.zhihu.com/question/20096067)有一个清晰的说明：
>严格意义上Web服务器只负责处理HTTP协议，只能发送静态页面的内容。而JSP，ASP，PHP等动态内容需要通过CGI、FastCGI、ISAPI等接口交给其他程序去处理。这个其他程序就是应用服务器。
>比如Web服务器包括Nginx，Apache，IIS等。而应用服务器包括WebLogic，JBoss等。应用服务器一般也支持HTTP协议，因此界限没这么清晰。但是应用服务器的HTTP协议部分仅仅是支持，一般不会做特别优化，所以很少有见Tomcat直接暴露给外面，而是和Nginx、Apache等配合，只让Tomcat处理JSP和Servlet部分。

##### (b) #####


##### (a)servlet是什么 #####
servlet是web容器最基本的组成单元，http请求是向web服务器请求一种信息资源，而servlet就充当了这种资源的最小单位，servlet可以无限扩展，使用java所有的类库资源，为用户返回文本、图片、音频的各类信息资源。 从程序员的角度看，servlet是一个java类，需要实现javax.servlet.Servlet接口的所有方法，提供一个公开的无参数的构造方法。由web容器来控制它的创建、初始化、提供服务、销毁等。它的各种行为方式通过web.xml文件中来配置。

Servlet接口有3个重要的方法，分别是init()，destroy()和service()，由于Servlet是一个java接口，所以需要加载。

Servlet生命周期分4个阶段：加载，初始化，提供服务和销毁。

加载阶段是将请求的servlet类加载到java虚拟机中，这里需要通过公开的无参的构造方法来实例化，无没有则加载失败，也可以通过<load-on-startup>设置servlet在web容器启动时加载。 这些过程都由web容器来控制，开发者关注最多的是初始化和提供服务两个阶段，在init()方法中，开发者可以获取配置在web.xml中的初始化参数，service()方法会在Servlet请求时调用，处理业务逻辑。

Servlet接口有3个实现类，FacesServlet、GenericServlet、HttpServlet。FacesServlet类一般用于JSF的Servlet，很少使用。GenericServlet是一个抽象类，有除了service()方法外的所有抽象方法的默认实现。HttpServlet最常用，包含在javax.servlet.http.HttpServlet类中。

#### <1> java web项目的基本结构 ####
java web开发中对项目的结构有一个基本要求，具体内容如下：

```shell
WEB项目结构

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

#### <2>web容器配置 ####
Web容器需要一个用来描述这种对应关系的文件，一般是web.xml文件。


### （3）Spring框架的使用 ###

#### <1> Spring框架的web.xml配置 ####
工程中使用了Spring MVC框架进行前后台交互的封装，就需要在上述java web工程的web.xml文件中进行配置。
并且使用Spring的组件对基本web.xml中的Filter、Listener、Servlet等进行设置。下面就针对使用Spring框架的时候应该如何设置web.xml进行说明和分析。

##### (a) 前端servlet配置 #####
SpringMVC是一个基于DispatcherServlet的MVC框架，每一个请求最先访问的都是DispatcherServlet，DispatcherServlet负责转发每一个Request请求给相应的Handler，Handler处理以后再返回相应的视图(View)和模型(Model)，返回的视图和模型都可以不指定，即可以只返回Model或只返回View或都不返回。
也就是说DispatcherServlet是Spring MVC的入口，所有进入Spring Web的Request都经过DispatcherServlet，所以首先需要在 web.xml 中注册 DispatcherServlet并且进行配置。
>这儿和不使用SpringMVC而只使用最基本servlet的原理都是一样的，但是因为使用了Spring框架，所以这儿使用DispatcherServlet作为中间层对servlet进行管理。

具体代码如下：

```xml
<servlet>
 <servlet-name>dispatherContext</servlet-name>
 <servlet-class>
  org.springframework.web.servlet.DispatcherServlet
 </servlet-class>
 <load-on-startup>1</load-on-startup>
</servlet>
```
加载 DispatcherServlet 时 Spring 会尝试读取初始配置文件，默认的初始配置文件位于 web.xml 相同的路径下文件名与注册的Servlet名有关 Servlet注册名跟上 -servlet.xml。当然这种读入方式不适合多种框架结合使用的情况，所以可以通过指定参数的方式来对配置文件路径进行设置，具体配置如下：

```xml
<init-param>
 <param-name>contextConfigLocation</param-name>
 <param-value>
  <!-- 配置文件名 -->
 </param-value>
</init-param>
```
实际工程中的配置内容为：

```xml
<init-param>
	<param-name>contextConfigLocation</param-name>
	<param-value>/WEB-INF/spring/servlet/servlet-context.xml</param-value>
</init-param>
```
这样当前DispatcherServlet的注册就完成了。

##### (b) servlet map配置 #####
注册 DispatcherServlet 后 还应指定有 Spring 处理的 url 模板。具体配置如下：
```xml
<servlet-mapping>
 <servlet-name>dispatherContextServlet</servlet-name>
 <url-pattern>*.do</url-pattern>
</servlet-mapping>
```
这样对当前网址的所有请求.do的处理就交给Spring了。在本项目中，Spring处理所有调用请求，将“.do”替换为“/”。

##### (c) 配置DispatcherServlet上下文配置文件加载路径 #####
配置文件读取器注册成功后还需要设定配置文件列表，通过设置全局参数contextConfigLocation来对配置文件列表进行说明：
>多个配置文件列表以逗号分隔，注意路径

```xml
<context-param>
	<param-name>contextConfigLocation</param-name>
	<param-value>
		/WEB-INF/dispatcherContext-servlet.xml,
		<!-- classpath*: 指定编译后的class目录 在ide中 与src根目录相同 -->
		classpath*:hibernateContext.xml
	</param-value>
</context-param>
```
也可以将多个配置文件列表方在另外的配置文件中，然后设置路径进行读入：
```xml
<context-param>
	<param-name>contextConfigLocation</param-name>
	<param-value>/WEB-INF/spring/appContext.xml</param-value>
</context-param>
```
在本工程中，文件列表为所有的Spring Bean的配置文件所在目录。也就是说appContext.xml中包含了所有Spring要管理的bean文件路径。
>实际工程中还包含了数据库的配置文件

##### (d) 监听器的配置 #####
当程序越来越大，配置文件中的 <bean> 越来越多，而且变得关系错综复杂难于维护，此时应该考虑将配置文件拆分成多个。为了让 Spring 能够读到这些配置文件并察觉到他们的变化，需要注册配置文件读取器。
对于Servlet 2.3以上标准且web容器支持监听器就可以在 web.xml 中注册监听。具体配置如下：

```xml
<listener>
	<listener-class>
		org.springframework.web.context.ContextLoaderListener
	</listener-class>
</listener>
```

>综上所述，通过四个基本配置，Spirng MVC框架的web容器配置就基本建立起来了。



### （4）mybatis和Spring的集成 ###
单独使用mybatis是有很多限制的（比如无法实现跨越多个session的事务），而且很多业务系统本来就是使用spring来管理的事务，因此mybatis最好与spring集成起来使用。
在实际工程中，使用Spring管理session并且与mybatis集成的配置代码如下：

```xml
<beans:bean id="sqlSessionFactory" class="org.mybatis.spring.SqlSessionFactoryBean">
	<beans:property name="dataSource" ref="dataSource" />
	<beans:property name="mapperLocations" value="classpath*:com/sys/db/**/*.xml" />
</beans:bean>

<beans:bean id="sqlSession" class="org.mybatis.spring.SqlSessionTemplate">
	<beans:constructor-arg index="0" ref="sqlSessionFactory" />
</beans:bean>
```

这儿使用了
