### JDBC规范和连接池实现：
在设计分布式存储框架的时候，为了方便用户使用和交互，可以通过实现自有连接池的方式来将直接的JDBC链接进行转换，然后调用自己的服务来实现接口。
比较出名的就是阿里巴巴的Druid。

#### Druid数据库连接池：
这个是阿里巴巴的开源数据库连接池项目，作者温绍锦，网名温少，[博客](http://wenshao.iteye.com/)。
我们将通过这个东西来搭建自己的JDBC。
同样我们也可以使用DataSource来封装JDBC访问，而不用实际上实现JDBC协议来完成开发：
[JDBC数据源(DataSource)的简单实现](http://lavasoft.blog.51cto.com/62575/265073)
参考这个帖子我完成了一个自定义的DataSource来仿造JDBC客户端编程接口。

[JDBC学习笔记](http://www.jianshu.com/p/8aba0d57d26e)
[使用 JAVA 中的动态代理实现数据库连接池](https://www.ibm.com/developerworks/cn/java/l-connpoolproxy/)
[实现自己的JDBC框架](http://www.cnblogs.com/pokid/p/5823695.html)
[JDBC中获取Connection的两种方式](http://yuanwhy.com/2016/10/03/jdbc-drivermanager-datasource-connection/)
[数据库连接池（DataSource）](http://www.flyne.org/article/599)
[java学习笔记—标准连接池的实现（27）](http://www.cnblogs.com/zhenghongxin/p/4399132.html)

更多关于Druid连接池的学习参考：
[Druid：一个用于大数据实时处理的开源分布式系统](http://www.infoq.com/cn/news/2015/04/druid-data)
[Druid OLAP架构设计](http://zqhxuyuan.github.io/2015/12/03/2015-12-03-Druid-Design/)
[阿里巴巴开源项目 Druid 负责人温少访谈](http://www.iteye.com/magazines/90)
[数据库连接池：Druid](http://www.cnblogs.com/windlaughing/p/3287501.html)
[]()

[JDBC中获取Connection的两种方式](http://yuanwhy.com/2016/10/03/jdbc-drivermanager-datasource-connection/)
[数据库连接池（DataSource）](http://www.flyne.org/article/599)
[JDBC数据源(DataSource)的简单实现](http://lavasoft.blog.51cto.com/62575/265073)
[java学习笔记—标准连接池的实现（27）](http://www.cnblogs.com/zhenghongxin/p/4399132.html)
[实现自己的JDBC框架](http://www.cnblogs.com/pokid/p/5823695.html)
[]()
[]()
[]()
[]()
[]()