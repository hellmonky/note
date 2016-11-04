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
> - 2. 然后：将各种资源放入项目的特定子目录下，形成对应的组织结构，方便分类查找和引用；
> - 3. 最后：在布局xml文件中使用资源类型标识符来引入指定类型的资源；在逻辑代码中使用由aapt工具自动生成R.java来引用资源文件。

#### 2.2.1 资源文件的组成和结构：
在对整体工程的组成有了一个基本了解之后，首先看看外部化的资源文件。Android的资源文件，是由目录结构，Xml格式的文件，和纯数据文件构成。
Android应用程序资源可以分为两大类，分别是assets和res。
1 assets：assets类资源放在工程根目录的assets子目录下，它里面保存的是一些原始的文件，可以以任何方式来进行组织。这些文件最终会被原装不动地打包在apk文件中。如果我们要在程序中访问这些文件，那么就需要指定文件名来访问。例如，假设在assets目录下有一个名称为filename的文件，那么就可以使用以下代码来访问它：
```java
AssetManager am= getAssets();
InputStream is = assset.open("filename");
```
2 res：res类资源放在工程根目录的res子目录下，它里面保存的文件大多数都会被编译，并且都会被赋予资源ID。这样我们就可以在程序中通过ID来访问res类的资源。res类资源按照不同的用途可以进一步划分为以下9种子类型：

-   --animator。这类资源以XML文件保存在res/animator目录下，用来描述属性动画。属性动画通过改变对象的属性来实现动画效果，例如，通过不断地修改对象的坐标值来实现对象移动动画，又如，通过不断地修改对象的Alpha通道值来实现对象的渐变效果。

-   --anim。这类资源以XML文件保存在res/anim目录下，用来描述补间动画。补间动画和属性动画不同，它不是通过修改对象的属性来实现，而是在对象的原来形状或者位置的基础上实现一个变换来得到的，例如，对对象施加一个旋转变换，就可以获得一个旋转动画，又如，对对象实施一个缩放变换，就可以获得一个缩放动画。从数学上来讲，就是在对象的原来形状或者位置的基础上施加一个变换矩阵来实现动画效果。注意，在动画的执行过程中，对象的属性是始终保持不变的，我们看到的只不过是它的一个变形副本。

-   --color。这类资源以XML文件保存在res/color目录下，用描述对象颜色状态选择子。例如，我们可以定义一个选择子，规定一个对象在不同状态下显示不同的颜色。对象的状态可以划分为pressed、focused、selected、checkable、checked、enabled和window_focused等7种。

-   --drawable。这类资源以XML或者Bitmap文件保存在res/drawable目录下，用来描述可绘制对象。例如，我们可以在里面放置一些图片（.png, .9.png, .jpg, .gif），来作为程序界面视图的背景图。注意，保存在这个目录中的Bitmap文件在打包的过程中，可能会被优化的。例如，一个不需要多于256色的真彩色PNG文件可能会被转换成一个只有8位调色板的PNG面板，这样就可以无损地压缩图片，以减少图片所占用的内存资源。

-   --layout。这类资源以XML文件保存在res/layout目录下，用来描述应用程序界面布局。

-   --menu。这类资源以XML文件保存在res/menu目录下，用来描述应用程序菜单，例如，Options Menu、Context Menu和Sub Menu。

-   --raw。这类资源以任意格式的文件保存在res/raw目录下，它们和assets类资源一样，都是原装不动地打包在apk文件中的，不过它们会被赋予资源ID，这样我们就可以在程序中通过ID来访问它们。例如，假设在res/raw目录下有一个名称为filename的文件，并且它在编译的过程，被赋予的资源ID为R.raw.filename，那么就可以使用以下代码来访问它：
```java
Resources res = getResources();  
InputStream is = res .openRawResource(R.raw.filename);
```

-   --values。这类资源以XML文件保存在res/values目录下，用来描述一些简单值，例如，数组、颜色、尺寸、字符串和样式值等，一般来说，这六种不同的值分别保存在名称为arrays.xml、colors.xml、dimens.xml、strings.xml和styles.xml文件中。

-   --xml。这类资源以XML文件保存在res/xml目录下，一般就是用来描述应用程序的配置信息。

上述9种类型的资源文件，除了raw类型资源，以及Bitmap文件的drawable类型资源之外，其它的资源文件均为文本格式的XML文件，它们在打包的过程中，会被编译成二进制格式的XML文件。这些二进制格式的XML文件分别有一个字符串资源池，用来保存文件中引用到的每一个字符串，包括XML元素标签、属性名称、属性值，以及其它的一切文本值所使用到的字符串。这样原来在文本格式的XML文件中的每一个放置字符串的地方在二进制格式的XML文件中都被替换成一个索引到字符串资源池的整数值。这样做有两个好处：
> 1. 文件占用更小。例如，假设在原来的文本格式的XML文件中，有四个地方使用的都是同一个字符串，那么在最终编译出来的二进制格式的XML文件中，字符串资源池只有一份字符串值，而引用它的四个地方只占用一个整数值。
> 2. 解析速度更快。由于在二进制格式的XML文件中，所有的XML元素标签和属性等值都是使用整数来描述的，因此，在解析的过程中，就不再需要进行字符串解析，这样就可以提高解析速度。

每一个res资源在编译的打包完成之后，都会被分配一个资源ID，这些资源ID被终会被定义为Java常量值，保存在一个R.java文件中，与应用程序的其它源文件一起被编译到程序中，这样我们就可以在程序或者资源文件中通过这些ID常量来访问指定的资源。

关于android的资源管理，需要分析AssetManager模块来进行更为深入的理解。目前看来只需要知道：
> 1. 资源的分类区别；
> 2. 对应资源的放置位置都有明确的要求。
这两点就可以完成基本资源的设置和使用了。

参考文档：
[Android资源管理框架（Asset Manager）简要介绍和学习计划](http://blog.csdn.net/luoshengyang/article/details/8738877)
[官方文档-提供资源](https://developer.android.com/guide/topics/resources/providing-resources.html)
[Android源码分析-资源加载机制](http://blog.csdn.net/singwhatiwanna/article/details/24532419)
[Android应用程序资源的查找过程分析](http://blog.csdn.net/luoshengyang/article/details/8806798)
[记一次苦逼的资源逆向分析](http://zjutkz.net/2016/05/15/%E8%AE%B0%E4%B8%80%E6%AC%A1%E8%8B%A6%E9%80%BC%E7%9A%84%E8%B5%84%E6%BA%90%E9%80%86%E5%90%91%E5%88%86%E6%9E%90/)
[Android中资源查找过程分析](http://zjutkz.net/2016/06/19/Android%E4%B8%AD%E8%B5%84%E6%BA%90%E6%9F%A5%E6%89%BE%E8%BF%87%E7%A8%8B%E5%88%86%E6%9E%90/)

~~并且资源文件的放置也需要注意：**所有的资源文件都放在rec文件夹下，不同类别的资源，需要放置在不同的特定名称的子文件夹中，或者是写在特定文件名的文件中。**
也就是说，资源文件通过目录结构限定了资源类型，然后as会自动索引这些资源文件供布局文件和逻辑代码调用。~~

#### 2.2.2 资源文件的访问：
对于资源文件的访问，在[提供资源](https://developer.android.com/guide/topics/resources/providing-resources.html)章节有明确的说明。主要分为xml中对资源的访问和逻辑代码中对资源的访问。
xml中使用特殊标签来表示对资源的引用，逻辑代码则是通过aapt生成的R.java代码在activty中直接使用getResources函数获取资源。

##### 2.2.2.1 资源文件在xml文件中的访问：
根据[访问资源-在 XML 中访问资源](https://developer.android.com/guide/topics/resources/accessing-resources.html#ResourcesFromXml)章节的介绍，在xml中使用特殊语法引入资源。具体语法为：
```xml
@[<package_name>:]<resource_type>/<resource_name>
package_name：表示资源所在包的名称；
resource_type：表示资源类型在R中的子类；
resource_name：是不带扩展名的资源文件名，或 XML 元素中的 android:name 属性值（如果资源是简单值）；用于描述该资源被引用时的名称。
```
例如，我们使用一个简单资源作为测试，根据[分组资源类型](https://developer.android.com/guide/topics/resources/providing-resources.html#ResourceTypes)的说明，那么我们就应该在src\main\res\values目录下，新建一个test.xml，并且填入一下内容：
```xml
<?xml version="1.0" encoding="utf-8"?>
<resources>
   <color name="opaque_red">#f00</color>
   <string name="hello">Hello!</string>
</resources>
```
这样，我们就可以在主activity对应的样式文件activity_main.xml中直接引用上述xml中定义的资源了，代码为：
```xml
<EditText xmlns:android="http://schemas.android.com/apk/res/android"
        android:id="@+id/edit_text"
        android:layout_width="fill_parent"
        android:layout_height="wrap_content"
        android:textColor="@color/opaque_red"
        android:text="@string/hello" />
```
这段代码在主样式文件中添加了一个EditText类型的组件，并且新增了一个名称为edit_text的id，然后在其中通过@color表明引用name为opaque_red为颜色属性资源，@string表明引用name为hello为字符串属性资源。
然后重新编译当前工程，点击调试进行测试，可以看到多了一个红色的字符串在一个可编辑文本框中。这个文本框就是新添加在组件EditText，红色的hello就是通过资源文件获取的显示结果。


##### 2.2.2.2 资源文件在逻辑代码中的访问：
根据[访问资源-在代码中访问资源](https://developer.android.com/guide/topics/resources/accessing-resources.html#ResourcesFromCode)章节的介绍，我们可以在代码中通过以方法参数的形式传递资源 ID，在代码中使用资源。引用语法为：
```java
[<package_name>.]R.<resource_type>.<resource_name>
其中：
<package_name> 是资源所在包的名称（如果引用的资源来自您自己的资源包，则不需要）。
<resource_type> 是资源类型的 R 子类。
<resource_name> 是不带扩展名的资源文件名，或 XML 元素中的 android:name 属性值（如果资源是简单值）。
```
我们接着上面[资源文件在xml文件中的访问](##### 2.2.2.1)中增加的test.xml内容，使用xml中指定的ID获取对应的资源，然后转换为对应类型的java变量：
```java
String string = getString(R.string.hello);
```
这个id为hello，也就是xml中name对应的值。
然后新建一个函数来实现对资源的访问和修改：
```java
void changeStringView(){
    // 使用findViewById获取布局xml中指定ID的布局组件，这儿就是EditText，然后将他转换为EditText类型：
    EditText edit_text =(EditText)findViewById(R.id.edit_text);
    // 首先获取默认的文本值，也就是从资源文件中获取的id为hello的值：
    String original_str = edit_text.getText().toString();
    // 设置一个新的字符串，准备替换：
    String new_str = "https://developer.android.com/guide/topics/resources/available-resources.html";
    // 最后通过判断初始资源文件是否获取成功，如果成功就用程序动态修改显示内容：
    if(original_str.equals(getString(R.string.hello))){
        // 然后使用setText函数，在程序中动态修改EditText其中的值：
        edit_text.setText(new_str.toCharArray(), 0, new_str.length());
    }
}
```
将这个代码加入到主activity交互逻辑代码的onResume函数中，表示在启动的时候完成这些功能：
```java
protected void onResume() {
    super.onResume();

    // 初始化之后修改EditText中的string的内容：
    changeStringView();
}
```
重新编译运行，看看启动之后的程序界面中的文本编辑框中是否还是那个原来的Hello!了。

#### 2.2.3 小结：
通过上述增加资源文件，然后从布局配置xml中引入完成显示，最后通过代码对布局文件和资源文件的访问的整体流程，基本上完成了一个简单的交互过程。
在这个过程中主要体现android程序开发的搭建过程和编写思路，对很多的细节部分并不关心，这也是学习新内容的一个思路：
> 在练习中学习，在学习中加强练习
只有通过的不断的实际动手，才能完成学习和思考的交互，帮助充分的理解整个开发流程，对于其他方面也是一样。

### 2.3 界面布局：
根据上述简答的hello程序，我们理解了一个android程序的组成部分和相互的基本交互过程，但是我们的主界面布局文件怎么将这些资源文件组织显示还没有概念。
本节主要初步分析一下界面布局常用的组件特性和布局方式，保证自己的测试程序能够将需要的信息正确的显示出来，而不关心美观问题。
> 程序的美观程度和美工相关，也需要非常专业的知识才能完成良好的用户交互

#### 2.3.1 基本布局组件：

#### 2.3.2 基本布局方式：

#### 2.3.3 布局之间的切换：


### 2.4 逻辑代码：


## 3 可运行的工程实践：
在完成上述章节的学习之后，对整个android工程开发有了一个较为明确的概念，接下来通过几个实际可编译运行的工程，增加对工程开发中一些常见需求的理解。
通过这些例子可以逐步的深入android开发的

### 3.1 使用opencv完成图像处理：
关键点就是使用外部模块来帮助开发，因为从0开始开发程序在实践中往往不可取，都会从现有的提供的成熟模块来逐步快速构建自己的项目。
完成原型系统设计和测试后，再制定后续开发计划，这样快速的构建能够帮助开发人员重新定位该应用的实际使用状况，并且动态调整，在后续中通过更换或者自行编写外部引用的代码来进行有优化或者规避授权问题。
本节将使用开源机器视觉算法库openCV来进行图像处理的工程开发，并且逐步引入android app开发中的activity生命周期的概念。

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