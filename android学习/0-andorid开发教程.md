# android程序开发：

## 1 使用android studio进行开发：
as是google官方提供的android开发工具，建立在IDEA的基础上，比eclipse方便很多，是一个优秀的IDE。
对比之前的eclipse，IDEA对于工程的命名有了明显的区别，针对as的工程，组织和显示内容可以根据google官方文档：[Projects Overview](https://developer.android.com/studio/projects/index.html) 可以看到具体的细节。
本文所使用的就是as来进行android app的学习和开发。

## 2 android开发基本概念：
使用一门语言开发一个应用程序，首先需要对这门语言有一个充分的理解，然后还需要针对需要开发的应用程序的定位有一个清晰的定位。
针对android来讲，关键点就是java的熟练程度，已经在这个基础上对android程序开发的独特性的理解，和将要实现的功能目标的定位。
整体上来讲，android程序开发的角度来看，关键点就在于：资源文件、界面布局和代码逻辑实现。
通过这三个的组合完成整个app的整体呈现和用户交互。这里就需要更为详细的android开发的概念和api调用流程的学习了。

### 2.1 第一印象：
首先，我们需要一个可以正确编译运行的基本工程来帮助实例化理解这个开发流程，通过建立android app的第一印象来帮助组织思路，进行后续的开发和深入的理解。
启动as，默认创建一个空的工程，会自动生成一个可执行的hello world程序，从上述所说的三个关键点来看这个工程。

这个工程自动生成了一个默认的布局，并且用使用xml进行配置，文件位于：src\main\res\layout\activity_main.xml，打开后在as中可以看到预览，代码内容为：
```xml
<RelativeLayout xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:tools="http://schemas.android.com/tools"
    android:id="@+id/activity_main"
    android:layout_width="match_parent"
    android:layout_height="match_parent"
    android:paddingBottom="@dimen/activity_vertical_margin"
    android:paddingLeft="@dimen/activity_horizontal_margin"
    android:paddingRight="@dimen/activity_horizontal_margin"
    android:paddingTop="@dimen/activity_vertical_margin"
    tools:context="com.iscas.wentao.hellothisisme.MainActivity">

    <TextView
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:text="Hello World!" />
</RelativeLayout>
```
并且在其中有android开头的一些项目来说明独特的属性，包含资源文件的说明。这资源文件、界面布局和代码逻辑实现这三者之间的组织关系中，正是通过这个布局文件xml来关联逻辑代码和资源文件的。
例如xml中指定了当前这个布局文件关联的逻辑实现代码：
```xml
tools:context="com.iscas.wentao.hellothisisme.MainActivity"
```
并且还指定了需要的资源文件（这儿是直接写入的字符串）：
```xml
android:text="Hello World!"
```
然后逻辑代码就可以通过这个xml文件和资源文件生成的R.java来管理资源文件供代码部分获取和处理。基本逻辑实现代码位于生成的基本activity中，现在为位于：src/main/java/packagename/MainActivity.java，内容为：
```java
public class MainActivity extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
    }
}
```
其中R.layout.activity_main就指明了当前布局所采用的xml文件，也就是上述src\main\res\layout\activity_main.xml文件。

### 2.2 资源文件：
在应用程序中，资源文件是非常重要的数据来源，保证程序的正常运行。并且从国际化和适配的角度来讲，合理的分离资源文件能够简化整个程序开发的复杂性，并且方便后台代码开发和前台美工的分离，提升开发效率。
在android中使用资源的步骤，总体上总结为以下三步：
> - 1. 首先：确定需要外部化的应用资源，整理并归类；
> - 2. 然后：将各种资源放入项目 res/ 目录的特定子目录下，形成对应的组织结构，方便分类查找和引用；
> - 3. 最后：在布局xml文件中使用资源类型标识符来引入指定类型的资源；在逻辑代码中使用由aapt工具自动生成R.java来引用资源文件。

#### 2.2.1 资源文件的组成和结构：
在对整体工程的组成有了一个基本了解之后，首先看看外部化的资源文件。Android的资源文件，是由目录结构，Xml格式的文件，和纯数据文件构成。

#### 2.2.2 资源文件的访问：
对于资源文件的访问，在[提供资源](https://developer.android.com/guide/topics/resources/providing-resources.html)章节有明确的说明。主要分为xml中对资源的访问和逻辑代码中对资源的访问。

##### 2.2.2.1 资源文件在xml文件中的访问：
根据[访问资源-在 XML 中访问资源](https://developer.android.com/guide/topics/resources/accessing-resources.html#ResourcesFromXml)章节的介绍，在xml中使用特殊语法引入资源。具体语法为：
```xml
```

##### 2.2.2.2 资源文件在逻辑代码中的访问：
根据[访问资源-在代码中访问资源](https://developer.android.com/guide/topics/resources/accessing-resources.html#ResourcesFromCode)章节的介绍，

### 2.3 界面布局：

### 2.4 逻辑代码：


## 3 可运行的工程实践：
在完成上述章节的学习之后，对整个android工程开发有了一个较为明确的概念，接下来通过几个实际可编译运行的工程，增加对工程开发中一些常见需求的理解。
通过这些例子可以逐步的深入android开发的

### 3.1 使用opencv完成图像处理：
关键点就是使用外部模块来帮助开发，因为从0开始开发程序在实践中往往不可取，都会从现有的提供的成熟模块来逐步快速构建自己的项目。完成原型系统设计和测试后，再制定后续开发计划，这样快速的构建能够帮助开发人员

参考教程：
[在Android Studio上进行OpenCV 3.1开发](http://johnhany.net/2016/01/opencv-3-development-in-android-studio/)
[OpenCV4Android开发之旅(一)----OpenCV2.4简介及 app通过Java接口调用OpenCV的示例](http://blog.csdn.net/yanzi1225627/article/details/16917961)

关于xml布局：
```xml
<RelativeLayout xmlns:android="http://schemas.android.com/apk/res/android"
    xmlns:tools="http://schemas.android.com/tools"
    android:layout_width="match_parent"
    android:layout_height="match_parent"
    android:paddingBottom="@dimen/activity_vertical_margin"
    android:paddingLeft="@dimen/activity_horizontal_margin"
    android:paddingRight="@dimen/activity_horizontal_margin"
    android:paddingTop="@dimen/activity_vertical_margin"
    tools:context=".MainActivity" >
```