### SpringBoot的使用：
传统的Spring依赖于太多的xml配置文件，所以SpringBoot就为了减少和去除对xml的依赖而开发，用于快速开发Spring应用。
关于SpringBoot和Spring之间的关系，需要做一个梳理，这样才能在熟悉Spring的基础上，知道如何合理的使用SpringBoot进行应用开发。
参考手册：[Spring Boot Reference Guide](http://docs.spring.io/spring-boot/docs/current/reference/htmlsingle/)作为官方给出的完整教程，具有最权威的性质。例如在使用log4j的SpringBoot包的时候，根据：
[使用 Spring Boot 快速构建 Spring 框架应用](https://www.ibm.com/developerworks/cn/java/j-lo-spring-boot/)
[Spring boot中使用log4j记录日志](http://didispace.com/springbootlog4j/)
这两个文章中提到的starter有：spring-boot-starter-log4j，但是自己在添加了这个包之后gradle无法找到。根据官方文档的Logging章节：
[Logging](https://docs.spring.io/spring-boot/docs/current/reference/html/howto-logging.html)
给出的starter应该是：spring-boot-starter-log4j2，在添加了这个包之后顺利解决。
通过这个例子可以看出，所有的技术文档，官方手册永远都是首选参考，然后再尝试其他比较有价值的文章作为辅助。

并且通过SpringBoot做一些默认提供的监控设置：
[Spring Boot应用的健康监控](http://www.jianshu.com/p/734519d3c383)
[]()
[]()
[]()
[]()