# android程序开发：

默认最低版本为android 6.0，以nexus 5作为开发机，as2.1及以上IDE，并且不考虑向下兼容性。

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
> - 1. [Android资源管理框架（Asset Manager）简要介绍和学习计划](http://blog.csdn.net/luoshengyang/article/details/8738877)
> - 2. [官方文档-提供资源](https://developer.android.com/guide/topics/resources/providing-resources.html)
> - 3. [Android源码分析-资源加载机制](http://blog.csdn.net/singwhatiwanna/article/details/24532419)
> - 4. [Android应用程序资源的查找过程分析](http://blog.csdn.net/luoshengyang/article/details/8806798)
> - 5. [记一次苦逼的资源逆向分析](http://zjutkz.net/2016/05/15/%E8%AE%B0%E4%B8%80%E6%AC%A1%E8%8B%A6%E9%80%BC%E7%9A%84%E8%B5%84%E6%BA%90%E9%80%86%E5%90%91%E5%88%86%E6%9E%90/)
> - 6. [Android中资源查找过程分析](http://zjutkz.net/2016/06/19/Android%E4%B8%AD%E8%B5%84%E6%BA%90%E6%9F%A5%E6%89%BE%E8%BF%87%E7%A8%8B%E5%88%86%E6%9E%90/)

~~并且资源文件的放置也需要注意：**所有的资源文件都放在rec文件夹下，不同类别的资源，需要放置在不同的特定名称的子文件夹中，或者是写在特定文件名的文件中。**
也就是说，资源文件通过目录结构限定了资源类型，然后as会自动索引这些资源文件供布局文件和逻辑代码调用。~~

#### <a name="2.2.2">2.2.2 资源文件的访问：</a>
对于资源文件的访问，在[提供资源](https://developer.android.com/guide/topics/resources/providing-resources.html)章节有明确的说明。主要分为xml中对资源的访问和逻辑代码中对资源的访问。
xml中使用特殊标签来表示对资源的引用，逻辑代码则是通过aapt生成的R.java代码在activty中直接使用getResources函数获取资源。

##### <a name="2.2.2.1">2.2.2.1 资源文件在xml文件中的访问：</a>
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
我们接着上面[资源文件在xml文件中的访问](#2.2.2.1)中增加的test.xml内容，使用xml中指定的ID获取对应的资源，然后转换为对应类型的java变量：
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

### 2.3 界面和布局：
根据上述简答的hello程序，我们理解了一个android程序的组成部分和相互的基本交互过程，但是我们的主界面布局文件怎么将这些资源文件组织显示还没有概念。

因为之前学习过Qt，所以对于这种使用xml的方式来组织布局的特点还是印象深刻的，Qt提供了多种组织布局的方式，包含ui文件和C++代码形式，其中ui文件本质上就是Qt Designer生成的xml文件。
通过这种拆分，可以方便的将前台展示界面的调试和后台代码开发分离，这种思路和当前web开发的逻辑是一样的，也是一种更为合理有效的界面程序开发模式。

Android系统提供了两种方法来设置视图：第一种也是我们最常用的的使用XML文件来配置View的相关属性，然后在程序启动时系统根据配置文件来创建相应的View视图。第二种是我们在代码中直接使用相应的类来创建视图。
> 从更为广泛的意义上考虑，和Qt类似，将界面从代码中分离是一个非常不错的选择，用户交互和逻辑代码实现的隔离可以简化开发流程。

由于程序的美观程度和美工相关，也需要非常专业的知识才能完成良好的用户交互，本节只通过介绍基于xml的android布局，初步分析一下界面布局常用的组件特性和布局方式，保证自己的测试程序能够将需要的信息正确的显示出来，而不关心美观问题。

#### 2.3.1 View布局概述：
View对象是Android平台上表示用户界面的基本单元。View代表了用户界面组件的一块可绘制的空间块，它包含了用户交互和显示，和windows的窗口的概念非常类似。ViewGroup是View的子类，主要用于存放其他View（和ViewGroup）对象的布局容器。

通过ViewGroup构造的容器，将多个View构成了树形结构来完成用户界面的展示。所以，Android应用中的所有用户界面元素都是使用View和ViewGroup对象构建而成：
> - 1. View对象用于在屏幕上绘制可供用户交互的内容；
> - 2. ViewGroup对象用于储存其他View（和 ViewGroup）对象，以便定义界面的布局。

[View类](https://developer.android.com/reference/android/view/View.html)是所有android上可显示的基类。
[ViewGroup类](https://developer.android.com/reference/android/view/ViewGroup.html)也是从View类继承而来的：
```shell
java.lang.Object
   ↳	android.view.View
 	   ↳	android.view.ViewGroup
```
并且ViewGroup类是所有Layout的基类，也就是说，所有的布局方式都是由ViewGroup类派生出来的。

同时，andorid在[View类](https://developer.android.com/reference/android/view/View.html)中提出了widget的概念：which are used to create interactive UI components (buttons, text fields, etc.)，也就是说是一种“立即可用的UI组件”，例如：Button、ImageView和EditText等。
一定要和[App Widgets](https://developer.android.com/guide/topics/appwidgets/index.html)这个概念区分开来：an App Widget is a remote View hierarchy which is most commonly displayed on the user's home screen，更侧重于可展示和嵌入应用的完整交互界面而言。

参考文档：[difference between view and widget](http://stackoverflow.com/questions/5168549/difference-between-view-and-widget/21541275)

关于从xml布局文件到View的转换过程，就是android默认提供的转换机制来保证的，具体可以参考：
> - 1. [Android Inflate机制](https://developer.android.com/reference/android/view/LayoutInflater.html)
> - 2. [Fragment-Lifecycle-onAttach(Activity)](https://developer.android.com/reference/android/app/Fragment.html)
> - 3. [浅析 android 应用界面的展现流程（二）布局与视图的创建](http://3dobe.com/archives/119/)
> - 4. [管理 Activity 生命周期](https://developer.android.com/training/basics/activity-lifecycle/index.html)

从android生命周期的创建过程来综合理解。后续章节会有更为详细的介绍。


#### 2.3.2 七种基本布局的公共参数介绍：
从ViewGroup类派生出来了很多不同样式的布局来对整体界面的显示进行控制。
布局定义用户界面的视觉结构，根据[官方文档-布局](https://developer.android.com/guide/topics/ui/declaring-layout.html)介绍，android包含以下几种基本布局方式：
> - 1. LinearLayout：线性布局，所有的控件都是串在一条线上的；
> - 2. RelativeLayout：相对布局，所有的控件的位置，都是相对于父控件的；
> - 3. FrameLayout：单帧布局，FrameLayout布局中的控件都是一层一层的。帧布局每次添加的控件都显示在最上面，最后显示在界面上的是最后添加的一个控件；
> - 4. TableLayout：表格布局，表格布局可以实现的.一般 可以使用 线性布局实现；
> - 5. GridLayout：网格布局，是Android4.0增加的网格布局控件，与之前的TableLayout有些相似，它把整个容器划分为rows × columns个网格，每个网格可以放置一个组件；
> - 6. ListLayout：列表布局，
> - 6. AbsoluteLayout：绝对布局，已经是废弃的状态，因为分辨率等复杂设备要求，这个布局不再采用。

复杂的布局方式就是通过这些布局的不同组合来完成展示的。

为了对布局进行调整，ViewGroup类中有一个内部类[ViewGroup.LayoutParams](https://developer.android.com/reference/android/view/ViewGroup.LayoutParams.html)作为对Layout调整参数的基类。
然后根据不同的ViewGroup子类，使用对应的LayoutParams的子类来构造布局参数，来告诉他的父View，自己所需要的布局效果，从而完成对不同的布局进进行参数调整。

根据[ViewGroup Layout Attributes](https://developer.android.com/reference/android/R.styleable.html)中的列表，可以看到所有相关的参数说明。不同布局都从ViewGroup中派生而来，这些基本布局方式使用一些公共参数来确定具体布局方式，例如：width和height表示这个布局的长和高信息。更为详细的17个公共参数为：

| Attribute                     | Description                                                                                   |
| ----------------------------- |-----------------------------------------------------------------------------------------------|
| android:id                    | This is the ID which uniquely identifies the view.                                            |
| android:layout_width          | This is the width of the layout.                                                              |
| android:layout_height         | This is the height of the layout.                                                             |
| android:layout_marginTop      | This is the extra space on the top side of the layout.                                        |
| android:layout_marginBottom   | This is the extra space on the bottom side of the layout.                                     |
| android:layout_marginLeft     | This is the extra space on the left side of the layout.                                       |
| android:layout_marginRight    | This is the extra space on the right side of the layout.                                      |
| android:layout_gravity        | This specifies how child Views are positioned.                                                |
| android:layout_weight         | This specifies how much of the extra space in the layout should be allocated to the View.     |
| android:layout_x              | This specifies the x-coordinate of the layout.                                                |
| android:layout_y              | This specifies the y-coordinate of the layout.                                                |
| android:layout_width          | This is the width of the layout.                                                              |
| android:layout_width          | This is the width of the layout.                                                              |
| android:paddingLeft           | This is the left padding filled for the layout.                                               |
| android:paddingRight          | This is the right padding filled for the layout.                                              |
| android:paddingTop            | This is the top padding filled for the layout.                                                |
| android:paddingBottom         | This is the bottom padding filled for the layout.                                             |

可以看出基本布局方式的属性都是用android:开头的，然后后面接着描述属性的名称，然后用等号链接属性对应的值。
需要注意的是，在布局xml文件中，对已有资源的引用就是在：[资源文件在xml文件中的访问](#2.2.2.1)所介绍的使用的语法。

下面的小结中，将以：
```xml
<EditText xmlns:android="http://schemas.android.com/apk/res/android"
        android:id="@+id/edit_text"
        android:layout_width="fill_parent"
        android:layout_height="wrap_content"
        android:textColor="@color/opaque_red"
        android:text="@string/hello" />
```
这段xml代码为例，来简略介绍上述公共参数的含义。

##### 2.3.2.1 View的ID：
android中，任何视图对象都可能具有关联的整型ID，此ID用于在结构树中对View对象进行唯一标识。编译应用后，此ID将作为整型数引用，但在布局XML文件中，通常会在id属性中为该ID赋予字符串值。
这是所有View对象共用的XML属性（由View类定义）。XML标记内部的 ID 语法是：
```xml
android:id="@+id/my_button"
```
字符串开头处的 @ 符号指示 XML 解析程序应该解析并展开 ID 字符串的其余部分，并将其标识为 ID 资源。加号 (+) 表示这是一个新的资源名称，必须创建该名称并将其添加到我们的资源（在 R.java 文件中）内。
引用 Android 资源 ID 时，不需要加号，但必须添加 android 软件包命名空间，如下所示：
```xml
android:id="@android:id/empty"
```
@符号后的字符串的含义为：引用android软件包下的，类型为id的，标识符为empty的ID。例如在[资源文件在xml文件中的访问](#2.2.2.1)节中的：

上述代码就是用字符串“edit_text”对当前的布局组件EditText添加了一个ID。

##### 2.3.2.2 布局的长度和宽度参数：
对应的xml中的设置为：
```xml
android:layout_width="fill_parent"
android:layout_height="fill_parent"
```
这两个属性设置了对应View的基本长宽属性值，是每个View的必备属性。
表示这个布局的宽度特性值为"fill_parent"。这个属性值可选参数根据文档[layout_width](https://developer.android.com/reference/android/R.attr.html#layout_width)有：
```shell
fill_parent         -1	The view should be as big as its parent (minus padding). This constant is deprecated starting from API Level 8 and is replaced by match_parent.
match_parent        -1	The view should be as big as its parent (minus padding). Introduced in API Level 8.
wrap_content        -2	The view should be only big enough to enclose its content (plus padding).
dimension value     例如120dp，需要一个明确的大小值，一般不会这样设置，因为适配的屏幕大小不确定，这样带来外观的不可控。
```
除了设置为具体的像素大小之后，其他三种属性值的含义为：
> fill_parent将强制性地使构件扩展，以填充布局单元内尽可能多的空间。
> match_parent指的是将当前View的大小设置为其父View的大小相同。
> wrap_content指根据视图内部内容自动扩展以适应其大小，设置一个视图的尺寸为wrap_content将强制性地使视图扩展以显示全部内容。

##### 2.3.2.3 layout_gravity属性：
```xml
android:layout_gravity
```
android:layout_gravity是用来设置该view相对与父view 的位置。根据文档[layout_gravity](https://developer.android.com/reference/android/R.attr.html#layout_gravity)，现有的可选项有：

|Constant	        |Value	    |Description    |
| ------------------|-----------|---------------|
|top	            |0x30	    |Push object to the top of its container, not changing its size.|
|bottom	            |0x50	    |Push object to the bottom of its container, not changing its size.|
|left	            |0x03	    |Push object to the left of its container, not changing its size.|
|right	            |0x05	    |Push object to the right of its container, not changing its size.|
|center_vertical	|0x10	    |Place object in the vertical center of its container, not changing its size.|
|fill_vertical	    |0x70	    |Grow the vertical size of the object if needed so it completely fills its container.|
|center_horizontal	|0x01	    |Place object in the horizontal center of its container, not changing its size.|
|fill_horizontal	|0x07	    |Grow the horizontal size of the object if needed so it completely fills its container.|
|center	            |0x11	    |Place the object in the center of its container in both the vertical and horizontal axis, not changing its size.|
|fill	            |0x77	    |Grow the horizontal and vertical size of the object if needed so it completely fills its container.|
|clip_vertical	    |0x80	    |Additional option that can be set to have the top and/or bottom edges of the child clipped to its container's bounds. The clip will be based on the vertical gravity: a top gravity will clip the bottom edge, a bottom gravity will clip the top edge, and neither will clip both edges.|
|clip_horizontal	|0x08	    |Additional option that can be set to have the left and/or right edges of the child clipped to its container's bounds. The clip will be based on the horizontal gravity: a left gravity will clip the right edge, a right gravity will clip the left edge, and neither will clip both edges.|
|start	            |0x00800003	|Push object to the beginning of its container, not changing its size.|
|end	            |0x00800005	|Push object to the end of its container, not changing its size.|


需要注意的是还有一个非常类似的属性：
```xml
android:gravity
```
这个属性的作用是设置View中内容相对于View组件的对齐方式。他可选的属性值，根据文档[gravity](https://developer.android.com/reference/android/R.attr.html#gravity)，和上述layout_gravity属性一致。

也就是说：android:gravity用于设置View中内容相对于View组件的对齐方式，而android:layout_gravity用于设置View组件相对于Container的对齐方式。
参考文档：
[“android:gravity”和“android:layout_gravity”属性解释](http://www.jianshu.com/p/c8a00330a1b1)
[在程序中设置android:gravity 和 android:layout_Gravity属性](http://blog.csdn.net/feng88724/article/details/6333809)


##### 2.3.2.4 layout_weight属性：
```xml
android:layout_weight
```
根据官方文档[layout_weight](https://developer.android.com/reference/android/widget/LinearLayout.LayoutParams.html#attr_android:layout_weight)，也就是说：layout_weight属性用于分配LinearLayout中的的额外空间(extra space)。如果View不想拉伸的话，layout_weight值设置为0。否则的话这些像素会按比例分配到这些weight值大于0的所有View。





最终调整布局文件的样式为：
```xml
<?xml version="1.0" encoding="utf-8"?>
<LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
    android:layout_width="wrap_content"
    android:layout_height="wrap_content"
    android:orientation="vertical"
    android:id="@+id/layout"
    android:weightSum="0">

    <TextView
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:layout_alignParentTop="true"
        android:layout_centerHorizontal="true"
        android:layout_gravity="center"
        android:textColor="@color/colorPrimaryDark"
        android:text="OpenCV Test"
        android:id="@+id/textView" />

    <ImageView
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:id="@+id/img"
        android:layout_centerInParent="true"
        android:background="@drawable/xiaohui"
        android:layout_weight="0.68" />

    <Button
        android:id="@+id/btn"
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:layout_below="@id/img"
        android:layout_centerHorizontal="true"
        android:layout_gravity="center"
        android:textColor="@color/colorPrimaryDark"
        android:text="灰度化"/>"

    <EditText xmlns:android="http://schemas.android.com/apk/res/android"
        android:id="@+id/edit_text"
        android:layout_width="fill_parent"
        android:layout_height="wrap_content"
        android:textColor="@color/opaque_red"
        android:text="@string/hello" />

</LinearLayout>
```
并且在调整中可以看到不同的属性之间有相互的作用，都是从父节点到子节点逐步影响








参考文档：
> - 1. [Develop-API Guides-用户界面：布局](https://developer.android.com/guide/topics/ui/declaring-layout.html)
> - 2. [Android - UI Layouts](https://www.tutorialspoint.com/android/android_user_interface_layouts.htm)



#### 2.3.3 七种基本布局特性参数介绍：
熟悉了七种布局的共有属性之后，还需要针对每一种不同的布局的特性进行了解，这样才能方便的进行界面的调整。

##### 2.3.3.1 LinearLayout属性：
orientation：属性是指定线性布局的排列方向。
horizontal 水平。线性布局默认的朝向是水平的。
vertical 垂直


#### 2.3.4 布局组件：
在完成基本布局的理解上，再看看由View和ViewGroup派生的功能布局，这些布局就是android默认提供的，特殊功能的布局，可以实现不同的基本功能，也可以称为布局组件。
这些组件就像是可视化的功能积木，我们使用这些组件搭建界面功能模块。

##### 2.3.4.1 TextEditView：



#### 2.3.5 布局之间的切换：




### 2.4 Android应用程序代码框架：

#### 2.4.1 四大组件：

#### 2.4.2 应用程序的启动流程：

#### 2.4.3 应用程序的消息处理：

#### 2.4.4 




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




关于markdown的锚点，有两种方法可以实现：
> - 1. 使用html的span语法标签，手动指定需要的锚点，然后使用语法[test](#锚点ID)实现跳转；
> - 2. 根据markdown编译的html，找到自动生成的锚点ID，然后使用语法[test](#找到的锚点ID)实现跳转。

例如根据markdown生成的html找到的锚点例子：
[test](#user-content-2222-资源文件在逻辑代码中的访问)
手动添加锚点的代码：
```html
<a name="id"> id对应的内容 </a>
```
参考文档：[How to link to a named anchor in Multimarkdown?](http://stackoverflow.com/questions/6695439/how-to-link-to-a-named-anchor-in-multimarkdown)



## 4 优秀的开源android第三方库：
参考文档：
> - 1. [一个2年Android开发者的18条忠告](https://news.cnblogs.com/n/556548/)
> - 2. [awesome-android](https://snowdream.github.io/awesome-android/)
> - 3. [open-source-android-apps](https://github.com/pcqpcq/open-source-android-apps)