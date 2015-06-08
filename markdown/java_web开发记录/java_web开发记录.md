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




### （2）java web框架 ###

>java的web开发需要一些基本概念，这是自己现在没有的，需要进行学习和了解，本节内容就针对当前使用的java web开发基础知识进行学习。

web开发是目前java的热点应用，这种现象的存是和java语言本身也有密切的关系的：
（1）Java是一种动态加载和运行的语言。也就是说当应用程序持有一个类的地址（CLASSPATH）和名称（包名和类名）的情况下，可以在程序运行期间任何时候加载这个类，并创建和使用该类的对象。
（2）Java Servlet要求必须运行在Web服务器当中，与Web服务器之间属于分工和互补关系。确切的说，在实际运行的时候Java Servlet与Web服务器会融为一体，如同一个程序一样运行在同一个Java虚拟机（JVM）当中。
（3）Servlet对每个请求都是单独启动一个线程，而不是进程。这种处理方式大幅度地降低了系统里的进程数量，提高了系统的并发处理能力。另外因为Java Servlet是运行在虚拟机之上的，也就解决了跨平台问题。
（4）当Web容器接收到来自客户端的请求信息之后，会根据URL中的Web元件地址信息到Servlet队列中查找对应的Servlet对象，如果找到则直接使用，如果没有找到则加载对应的类，并创建对象。也就是说，Servlet对象是在第一次被使用的时候才创建的，并且一旦创建就会被反复使用，不再创建新的对象。所有创建出的Servlet对象会在Web服务器停止运行的时候统一进行垃圾回收。
（5）为了解决客户端请求地址与Java Servlet之间对应关系问题，Web容器需要一个用来描述这种对应关系的文件，一般是web.xml文件。如果一个Web应用程序中存在很多个Servlet，那么web.xml会变得非常庞大。在Servlet 3.0规范推出之后，允许在Servlet代码中使用声明式语法来代替web.xml中的描述信息，这才让web.xml瘦身下来。下图是这个过程的一个示意图。

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

#### <2>web容器入口 ####
Web容器需要一个用来描述这种对应关系的文件，一般是web.xml文件。


### （3）Spring框架的使用 ###

### （4）mybatis和Spring的集成 ###
单独使用mybatis是有很多限制的（比如无法实现跨越多个session的事务），而且很多业务系统本来就是使用spring来管理的事务，因此mybatis最好与spring集成起来使用。
