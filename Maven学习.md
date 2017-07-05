# Java构建工具Maven学习

## 引入：
之前的工程管理学习使用了Gradle，所以从Maven切换过来了，但是目前接手的项目还是使用Maven进行管理，自己之前接触的还是有点简单，不能适用于现在复杂的构建环境搭建，所以需要重新加强学习。
Apache Maven 是一套软件工程管理和整合工具。基于工程对象模型（POM）的概念，通过一个中央信息管理模块，Maven 能够管理项目的构建、报告和文档。

## 本地Maven环境的搭建：
虽然使用IDEA内嵌的Maven可以完成构建

参考文档：
[Maven - 环境配置](http://wiki.jikexueyuan.com/project/maven/environment-setup.html)

## 当前的Maven构建实例：
```xml
<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd">
    <modelVersion>4.0.0</modelVersion>

	<groupId>tech.hellmonky.www</groupId>
    <artifactId>JOJO</artifactId>
    <version>0.0.1-SNAPSHOT</version>
	<packaging>war</packaging>
    <name>JOJO</name>

	<properties>
        <appName>JOJO</appName>
        <java.version>1.8</java.version>
        <java.encoding>utf-8</java.encoding>
		<project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
		<maven.compiler.source>1.8</maven.compiler.source>
		<maven.compiler.target>1.8</maven.compiler.target>
    </properties>
    <build>
        <finalName>${appName}</finalName>
		<plugins>
			<plugin>
				<artifactId>maven-assembly-plugin</artifactId>
				<configuration>
					<archive>
						<manifest>
							<mainClass>fully.qualified.MainClass</mainClass>
						</manifest>
					</archive>
					<descriptorRefs>
						<descriptorRef>jar-with-dependencies</descriptorRef>
					</descriptorRefs>
				</configuration>
			</plugin>
		</plugins>
    </build>

    <dependencies>
        <!-- https://mvnrepository.com/artifact/org.glassfish.jersey.core/jersey-common -->
        <dependency>
            <groupId>org.glassfish.jersey.core</groupId>
            <artifactId>jersey-common</artifactId>
            <version>2.25.1</version>
        </dependency>

        <!-- https://mvnrepository.com/artifact/org.glassfish.jersey.core/jersey-server -->
        <dependency>
            <groupId>org.glassfish.jersey.core</groupId>
            <artifactId>jersey-server</artifactId>
            <version>2.25.1</version>
        </dependency>
		
		<!-- https://mvnrepository.com/artifact/org.glassfish.jersey.core/jersey-client -->
		<dependency>
			<groupId>org.glassfish.jersey.core</groupId>
			<artifactId>jersey-client</artifactId>
			<version>2.25.1</version>
		</dependency>
		
		<!-- https://mvnrepository.com/artifact/org.glassfish.jersey.media/jersey-media-multipart -->
		<dependency>
			<groupId>org.glassfish.jersey.media</groupId>
			<artifactId>jersey-media-multipart</artifactId>
			<version>2.25.1</version>
		</dependency>
		
		<!-- https://mvnrepository.com/artifact/org.glassfish.jersey.media/jersey-media-json-jackson -->
		<dependency>
			<groupId>org.glassfish.jersey.media</groupId>
			<artifactId>jersey-media-json-jackson</artifactId>
			<version>2.25.1</version>
		</dependency>
		
        <!-- https://mvnrepository.com/artifact/org.glassfish.jersey.containers/jersey-container-servlet -->
        <dependency>
            <groupId>org.glassfish.jersey.containers</groupId>
            <artifactId>jersey-container-servlet</artifactId>
            <version>2.25.1</version>
        </dependency>
	</dependencies>
</project>
```
上述管理文档描述了当前这个工程构建的所需信息，maven也是从当前目录的pom.xml开始进行递归构建的。

参考文档：

## 一些有用的设置：
[How can I create an executable JAR with dependencies using Maven?](https://stackoverflow.com/questions/574594/how-can-i-create-an-executable-jar-with-dependencies-using-maven)

[maven 下载 源码和javadoc命令](http://blog.csdn.net/topwqp/article/details/8902863)