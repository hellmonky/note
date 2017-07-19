<!-- TOC -->

- [small tool by python](#small-tool-by-python)
    - [python同步任务：](#python同步任务)
        - [watchdog模块的使用：](#watchdog模块的使用)
        - [添加自定义文件变化监听事件处理：](#添加自定义文件变化监听事件处理)
        - [完成远程的文件变更自动更新：](#完成远程的文件变更自动更新)
        - [添加系统开机启动：](#添加系统开机启动)
            - [Linux系统的开机启动：](#linux系统的开机启动)
            - [windows添加为开机服务：](#windows添加为开机服务)
    - [python类和继承](#python类和继承)
        - [面向对象编程基础：](#面向对象编程基础)
        - [在python中的体现：](#在python中的体现)
            - [定义一个类：](#定义一个类)
            - [创建实例对象](#创建实例对象)
            - [访问属性](#访问属性)
            - [Python内置类属性](#python内置类属性)
        - [反思：](#反思)
    - [在线获取图像和视频：](#在线获取图像和视频)
        - [搭建scrapy框架：](#搭建scrapy框架)
    - [平台环境自动化安装：](#平台环境自动化安装)
        - [使用python自动安装软件：](#使用python自动安装软件)
        - [另外一种思路：监控每一个软件的安装过程](#另外一种思路监控每一个软件的安装过程)
    - [python实现RESTFul API接口：](#python实现restful-api接口)
    - [参考文档](#参考文档)

<!-- /TOC -->

# small tool by python

> - life is short, so i need python

对于糟糕的python2和3版本问题，实在无法吐槽，原则就是：
**尽量使用3，如果使用了2会明确的说明。**

## python同步任务：
主要参考了：https://github.com/iWoz/file_sync
我们现在来看看这个代码的主体框架：
```python
#!/usr/bin/python

import sys
import time
import ntpath
import os
import re
import platform

from subprocess import call
from shutil import copy
from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler

# git root path for files to push to remote
DIR_FOR_GIT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

# files to synchronize
SYNC_FILE_LIST = []
f = open(os.path.join(DIR_FOR_GIT, "file_list.txt"), "r")
try:
    SYNC_FILE_LIST = [line.strip() for line in f]
except Exception as e:
    raise e
finally:
    f.close()

# get filename without upper directory
def path_leaf(path):
    head, tail = ntpath.split(path)
    return tail or ntpath.basename(head)

class FileChangeHandler(FileSystemEventHandler):
    def on_modified(self, event):
        if event.src_path in SYNC_FILE_LIST:
            copy(event.src_path, DIR_FOR_GIT)
            cd_cmd = "cd "+DIR_FOR_GIT
            git_add_cmd = "git add -A"
            git_commit_cmd = "git commit -m " + re.escape("Update "+path_leaf(event.src_path))
            if platform.system() == "Windows":
                git_commit_cmd = "git commit -m " + re.escape("Update_"+path_leaf(event.src_path))
            git_pull_cmd = "git pull origin master"
            git_push_cmd = "git push origin master"
            call(
                cd_cmd + "&&" +
                git_add_cmd + "&&" +
                git_commit_cmd + "&&" +
                git_pull_cmd + "&&" +
                git_push_cmd,
                shell=True
            )

if __name__ == "__main__":
    observer = Observer()
    event_handler = FileChangeHandler()

    for file_path in SYNC_FILE_LIST:
        copy(file_path, DIR_FOR_GIT)
        observer.schedule(event_handler, path=os.path.dirname(os.path.realpath(file_path)), recursive=False)

    observer.start()

    try:
        while True:
            time.sleep(10)
    except KeyboardInterrupt:
        observer.stop()
    observer.join()
```
代码的结构很简单，主体就是上述思路，我们逐一看看细节。
### watchdog模块的使用：
watchdog用来监控指定目录/文件的变化，如添加删除文件或目录、修改文件内容、重命名文件或目录等，每种变化都会产生一个事件，且有一个特定的事件类与之对应，然后再通过事件处理类来处理对应的事件，怎么样处理事件完全可以自定义，只需继承事件处理类的基类并重写对应实例方法。
从官方的例子入手，访问[官方网站](https://github.com/gorakhargosh/watchdog)：
```python
import sys
import time
import logging
from watchdog.observers import Observer
from watchdog.events import LoggingEventHandler

if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO,
                        format='%(asctime)s - %(message)s',
                        datefmt='%Y-%m-%d %H:%M:%S')
    path = sys.argv[1] if len(sys.argv) > 1 else '.'
    event_handler = LoggingEventHandler()
    observer = Observer()
    observer.schedule(event_handler, path, recursive=True)
    observer.start()
    try:
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        observer.stop()
    observer.join()
```
使用LoggingEventHandler()事件处理器，然后使用Observer创建一个监听，并且添加监听的文件路径和创建好的事件处理器实例，然后打开监听。
这样对于文件的变化，这个脚本就可以感知，并且使用事件处理器处理了。这儿默认的事件处理是LoggingEventHandler，也就是将文件变化记录在日志中。
然后开启一个循环，间隔1秒停一次监听，如果在循环中有任何的键盘的终止命令，则关闭监听。
最后通过join来伴随当前线程结束而结束。

将上述代码保存为watchdog_example.py，然后运行：
```shell
python watchdog_example.py "D:\workspace\python_workspace\small"
```
然后在这个文件夹下创建和删除文件，会得到这样的反馈：
```shell
2017-07-18 11:03:39 - Created file: D:\workspace\python_workspace\small\新建文本文档.txt
2017-07-18 11:03:47 - Deleted file: D:\workspace\python_workspace\small\新建文本文档.txt
```
可以看到在这个文件夹下的变化被作为日志记录在当前的命令界面中了。

### 添加自定义文件变化监听事件处理：
上述的监听处理使用了watchdog的默认日志处理器，一般而言，实际使用中对于文件变化之后的操作才是最关键的，我们看看自定义的操作方法，还是在上述基础上修改：
```python
import sys
import time
import logging
from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler

# 将上述log处理抽取出来，创建一个自定义的事件处理，这儿继承了FileSystemEventHandler这个基类，并在子类中重写对应方法。
class MyHandler(FileSystemEventHandler):
    # 初始化函数，用于传递
    def __init__(self, comparePath):
        self.comparePath = comparePath
    # 对于文件和文件夹变化的操作
    def on_modified(self, event):
        print("file changed:",event.src_path)    

if __name__ == "__main__":
    obserPath = sys.argv[1] if len(sys.argv) > 1 else '.'
    comparePath = obserPath
    print(comparePath)
    event_handler = MyHandler(comparePath)
    observer = Observer()
    observer.schedule(event_handler, obserPath, recursive=False)
    observer.start()
    try:
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        observer.stop()
    observer.join()
```
现在存在一个问题，就是只能监控文件变化了，但是无法感知变化的内容，这部分的实现可以通过git托管来保证文件变化内容的可追踪性。

现在回过头看看实际代码中的实现例子的细节：
首先，获取要监听的文件列表记录文件，然后放在SYNC_FILE_LIST中；
然后，遍历这个列表中的各个文件，将这些文件复制到git同步目录中，然后对于每一个文件使用observer.schedule添加真实路径和处理事件；
最后，启动监听。
其中使用了自定义的文件修改变化事件处理，并且在处理中，判断了当前发生变化的文件是否是记录列表中的文件，保证在当前git同步文件夹下的文件一定要在列表中才会被操作。
最终，通过自定义的命令组合，通过call来进行调用，也就是说，当前系统中已经安装了git服务才行，并且git登陆授权也已经搞定了。

其中，通过：
```python
DIR_FOR_GIT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
```
返回了当前脚本所在的文件夹的上一级文件夹，将这个文件夹作为同步文件的git目录。
也就是说需要首先创建一个git目录，完成git授权，然后在这个目录下创建目录保存同步脚本，然后启动脚本对当前git目录下的文件进行同步。
所以可以自己定制将这个配置信息采用GUI的形式进行设置，减少这种特化的配置方式（目录结构、将要同步的文件拷贝到当前的git同步文件夹和使用git作为文件存储服务器等），更加灵活的实现功能。

### 完成远程的文件变更自动更新：
和git相同，如果要求双向都可以修改，那么必然会遇到冲突问题，解决的方式也是和git相同，那么这种双向的同步操作就很难自动化完成，因为冲突的解决需要人工决策。
换一个思路考虑：git服务器只保持最新的修改，所有的客户端都以git服务器上的内容为标准来修改自己本地的文件，只有这种方式才能完成自动化处理。

在上述基础上，我们需要使用python来操作Git。其实使用python操作Git有两种思路：
> - （1）使用cmd来调用本地git命令完成操作；
> - （2）使用python库来对git命令操作。

从实践角度来讲，使用python的Git客户端也是需要本地安装git的，例如[GitPython](https://github.com/gitpython-developers/GitPython)。
所以使用cmd直接调用git命令是最快速的方式，但是上述变更提交代码已经使用了cmd调用命令的方式，这儿就使用pygit来试试看。

再进一步思考，如何让github在得到更新的时候自动通知我们来拉取最新的代码？
（1）一般而言，这个过程类似于回调：提前设置好要通知的地址，在代码提交的时候，向这个预设的地址发送请求。这种方式性能高，时效性强，但是需要服务器提供，个人来讲无法要求，适用性优先。
（2）更通用的方式就是轮询，在客户端主动轮询远程服务器的状态，如果状态变化就主动执行命令。但是这种轮询对于服务器和本地都有很大的压力，常见的策略是不会后台主动查询，而是用户手动执行查询减少压力。

如果从实际实现的角度考虑，还是都会选择主动查询的方式来解决这个问题，如果一定要实现自动获取，那么查询的时长一般都会长一点的，因为代码提交并不是很频繁，只需要定时查询就好了。

目前确认的实现方式为：
（1）获取本地和远程的差异，得到对比状态信息：
> - 差异的获取非常重要，可以使用git时间戳和hash，因为本地时钟可能存在问题，所以用hash双重校验。
（2）根据对比状态，确定需要执行的操作：
> - 如果本地的超前，就提交直接覆盖github上的；
> - 如果本地落后，就拉取github上，直接覆盖本地的。

关于这部分的内容，涉及到git的原理，找到了篇不错的文章：[git-recipes](https://github.com/geeeeeeeeek/git-recipes/wiki)
可以对照学习一下git的实际操作流程。



### 添加系统开机启动：
可以将上述代码作为服务，在系统启动的时候自动开启，给用户提供后台服务，完成自动化操作。
对于不同的操作系统，开启启动的方式是不同的。
#### Linux系统的开机启动：
#### windows添加为开机服务：

## python类和继承
python作为脚本语言，还支持类和继承，所以是可以面向对象编程的。

### 面向对象编程基础：
首先看看python中面向对象编程的基本概念：
> - 类(Class): 用来描述具有相同的属性和方法的对象的集合。它定义了该集合中每个对象所共有的属性和方法。对象是类的实例。
> - 类变量：类变量在整个实例化的对象中是公用的。类变量定义在类中且在函数体之外。类变量通常不作为实例变量使用。
> - 数据成员：类变量或者实例变量用于处理类及其实例对象的相关的数据。
> - 方法重写：如果从父类继承的方法不能满足子类的需求，可以对其进行改写，这个过程叫方法的覆盖（override），也称为方法的重写。
> - 实例变量：定义在方法中的变量，只作用于当前实例的类。
> - 继承：即一个派生类（derived class）继承基类（base class）的字段和方法。继承也允许把一个派生类的对象作为一个基类对象对待。例如，有这样一个设计：一个Dog类型的对象派生自Animal类，这是模拟"是一个（is-a）"关系（例图，Dog是一个Animal）。
> - 实例化：创建一个类的实例，类的具体对象。
> - 方法：类中定义的函数。
> - 对象：通过类定义的数据结构实例。对象包括两个数据成员（类变量和实例变量）和方法。

### 在python中的体现：
面向对象在不同的程序设计语言中有不同的切入角度和体现，我们看看python中的具体体现。

#### 定义一个类：
使用class语句来创建一个新类，class之后为类的名称并以冒号结尾，如下实例:
```python
class ClassName:
   '类的帮助信息'   #类文档字符串
   class_suite  #类体
```
> - 类的帮助信息可以通过ClassName.__doc__查看。
> - class_suite 由类成员，方法，数据属性组成。

以下是一个简单的Python类实例:
```python
#!/usr/bin/python
# -*- coding: UTF-8 -*-
 
class Employee:
   '所有员工的基类'
   empCount = 0
 
   def __init__(self, name, salary):
      self.name = name
      self.salary = salary
      Employee.empCount += 1
   
   def displayCount(self):
     print("Total Employee %d", Employee.empCount)
 
   def displayEmployee(self):
      print("Name : ", self.name,  ", Salary: ", self.salary)
```
empCount 变量是一个类变量，它的值将在这个类的所有实例之间共享。你可以在内部类或外部类使用 Employee.empCount 访问；
第一种方法__init__()方法是一种特殊的方法，被称为类的构造函数或初始化方法，当创建了这个类的实例时就会调用该方法；
self 代表类的实例，self 在定义类的方法时是必须有的，虽然在调用时不必传入相应的参数。
self代表类的实例，而非类
类的方法与普通的函数只有一个特别的区别——它们必须有一个额外的第一个参数名称, 按照惯例它的名称是 self：
```python
class Test:
    def prt(self):
        print(self)
        print(self.__class__)
 
t = Test()
t.prt()
```
以上实例执行结果为：
```shell
<__main__.Test instance at 0x10d066878>
__main__.Test
```
从执行结果可以很明显的看出，self 代表的是类的实例，代表当前对象的地址，而 self.class 则指向类。
self 不是 python 关键字，我们把他换成 runoob 也是可以正常执行的:
```python
class Test:
    def prt(runoob):
        print(runoob)
        print(runoob.__class__)
 
t = Test()
t.prt()
```
以上实例执行结果为：
```shell
<__main__.Test instance at 0x10d066878>
__main__.Test
```
#### 创建实例对象
实例化类其他编程语言中一般用关键字 new，但是在 Python 中并没有这个关键字，类的实例化类似函数调用方式。
以下使用类的名称 Employee 来实例化，并通过 __init__ 方法接受参数。
```python
"创建 Employee 类的第一个对象"
emp1 = Employee("Zara", 2000)
"创建 Employee 类的第二个对象"
emp2 = Employee("Manni", 5000)
```
#### 访问属性
您可以使用点(.)来访问对象的属性。使用如下类的名称访问类变量:
```python
emp1.displayEmployee()
emp2.displayEmployee()
print("Total Employee %d", Employee.empCount)
```
完整实例：
```python
#!/usr/bin/python
# -*- coding: UTF-8 -*-
 
class Employee:
   '所有员工的基类'
   empCount = 0
 
   def __init__(self, name, salary):
      self.name = name
      self.salary = salary
      Employee.empCount += 1
   
   def displayCount(self):
     print("Total Employee %d", Employee.empCount)
 
   def displayEmployee(self):
      print("Name : ", self.name,  ", Salary: ", self.salary)
 
"创建 Employee 类的第一个对象"
emp1 = Employee("Zara", 2000)
"创建 Employee 类的第二个对象"
emp2 = Employee("Manni", 5000)
emp1.displayEmployee()
emp2.displayEmployee()
print("Total Employee %d", Employee.empCount)
```
执行以上代码输出结果如下：
```shell
Name :  Zara ,Salary:  2000
Name :  Manni ,Salary:  5000
Total Employee 2
```
你可以添加，删除，修改类的属性，如下所示：
```python
emp1.age = 7  # 添加一个 'age' 属性
emp1.age = 8  # 修改 'age' 属性
del emp1.age  # 删除 'age' 属性
```
你也可以使用以下函数的方式来访问属性：
```python
getattr(obj, name[, default]) : 访问对象的属性。
hasattr(obj,name) : 检查是否存在一个属性。
setattr(obj,name,value) : 设置一个属性。如果属性不存在，会创建一个新属性。
delattr(obj, name) : 删除属性。
hasattr(emp1, 'age')    # 如果存在 'age' 属性返回 True。
getattr(emp1, 'age')    # 返回 'age' 属性的值
setattr(emp1, 'age', 8) # 添加属性 'age' 值为 8
delattr(empl, 'age')    # 删除属性 'age'
```
#### Python内置类属性
```python
__dict__ : 类的属性（包含一个字典，由类的数据属性组成）
__doc__ :类的文档字符串
__name__: 类名
__module__: 类定义所在的模块（类的全名是'__main__.className'，如果类位于一个导入模块mymod中，那么className.__module__ 等于 mymod）
__bases__ : 类的所有父类构成元素（包含了一个由所有父类组成的元组）
```
Python内置类属性调用实例如下：
```python
#!/usr/bin/python
# -*- coding: UTF-8 -*-
 
class Employee:
   '所有员工的基类'
   empCount = 0
 
   def __init__(self, name, salary):
      self.name = name
      self.salary = salary
      Employee.empCount += 1
   
   def displayCount(self):
     print "Total Employee %d" % Employee.empCount
 
   def displayEmployee(self):
      print "Name : ", self.name,  ", Salary: ", self.salary
 
print "Employee.__doc__:", Employee.__doc__
print "Employee.__name__:", Employee.__name__
print "Employee.__module__:", Employee.__module__
print "Employee.__bases__:", Employee.__bases__
print "Employee.__dict__:", Employee.__dict__
```
执行以上代码输出结果如下：
```shell
Employee.__doc__: 所有员工的基类
Employee.__name__: Employee
Employee.__module__: __main__
Employee.__bases__: ()
Employee.__dict__: {'__module__': '__main__', 'displayCount': <function displayCount at 0x10a939c80>, 'empCount': 0, 'displayEmployee': <function displayEmployee at 0x10a93caa0>, '__doc__': '\xe6\x89\x80\xe6\x9c\x89\xe5\x91\x98\xe5\xb7\xa5\xe7\x9a\x84\xe5\x9f\xba\xe7\xb1\xbb', '__init__': <function __init__ at 0x10a939578>}
```

### 反思：
这种语法的罗列是没有意义的，个人感受还是通过实际的工程需求来刺激自己学习，这样主动的学习效果是要好于知识的罗列的，更好于灌输性的方式。
建议还是从实际的应用出发，看看有什么需求，然后这个需求是如何通过python来解决的。

## 在线获取图像和视频：
python最拿手的就是爬虫了，丰富功能的爬虫框架，可以帮助我们快速批量的从www上获取需要的资料，这个功能对于资源收集者来说是非常重要的。
现在我们借助于scrapy来对特定网站的音视频资料进行获取。

### 搭建scrapy框架：


## 平台环境自动化安装：
痛点：每一次重新安装操作系统之后，都需要下载安装基础软件，然后通过添加环境变量等方式完成环境搭建，然后将需要启动的软件作为服务添加启动；而且软件的升级也是需要手动执行的，需要用户自己去维护。
这种环境的维护也是需要成本的，例如：
（1）安装的软件冲突，需要回退，但是这个时候环境变量已经混乱了，无法正常通过卸载软件恢复（尤其是国产的全家桶系列）；
（2）如果想要将当前的系统环境保持，就需要定期的系统备份，问题在于操作系统本身也是在不断升级的，这种备份无法完成环境和系统的去耦合，而且备份的生成非常耗时，占用空间巨大。
上述问题，可以通过自动化脚本的形式给予处理。每次安装操作系统之后，运行脚本，结束后，个性化的定制环境就已经生效了，这个时候用户关注的就是按照原来的喜好继续使用环境，而不是花费很多的精力安装部署环境。

### 使用python自动安装软件：
将需要安装的安装包放在特定路径下，然后使用脚本完成定制安装。
这个内容在windows下有了Chocolatey，Mac下有Homebrew。
那么是否可以通过python调用这些工具来完成整个软件的安装和管理。
并且windows环境下的安装需要重新后生效，如果存在链式的依赖关系，那么自动化安装就无法顺利进行，这个时候[BoxStarter](http://boxstarter.org/)，一个封装了Chocolatey的能解决windows烦人的reboot问题命令行方式就出现了。
这个软件在github上开源了：https://github.com/mwrock/boxstarter
并且boxstarter还能将软件的配置通过git进行同步，能够满足现在的需求了。

> - 同时还有一个开发环境搭建的工具vagrant，Vagrant是一个软件，可以自动化虚拟机的安装和配置流程。这个软件可以帮助在非Linux系统环境下，对于基于Linux的开发环境进行管理。也就是说，只能对于虚拟机进行包管理，这个和当前在物理机层面的需求是类似的，但是实现方式是不同的。


具体的内容可以参考：
[路径（二）：更好的安装软件的方法（Windows：Chocolatey，Mac：Homebrew）](https://ninghao.net/blog/2071)
[Chocolatey - kind of like apt-get, but for Windows](https://chocolatey.org/docs)
[choco](https://github.com/chocolatey/choco)
[在Windows平台上实现云自动化](http://www.infoq.com/cn/articles/cloud-automation-windows)

### 另外一种思路：监控每一个软件的安装过程
将每一个软件在安装中涉及到的文件进行管理，然后对于这些文件变化监听和记录，这样就对于每一个软件做备案。
然后通过，这个记录来帮助自动化管理。
估计涉及到的有windows官方出的fileMonitor工具，现在合并到[Process Monitor](https://technet.microsoft.com/en-us/sysinternals/processmonitor)了，可以通过这个工具完成对文件变化的监听。
但是这种方式涉及到的文件会比较多，管理流程也是很麻烦的，而且不容易区分软件本身的资源文件和配置文件之间的差异，无法实现对软件配置项的备份。





## python实现RESTFul API接口：
现在需要对不同的IaaS厂商提供的接口做封装调用，并且对外提供RESTFul的接口，作为一个Proxy。
然后将python程序打包为可执行文件（包含windows和linux环境）。


参考：
[使用 Python 和 Flask 设计 RESTful API](http://www.pythondoc.com/flask-restful/first.html)
[eve - an open source Python REST API framework](https://github.com/pyeve/eve)


## 参考文档
> - [基于Git的文件自动同步的思考和实现](http://wuzhiwei.net/file_sync_git/)
> - [watchdog--监控文件系统变化](http://sapser.github.io/python/2014/07/25/watchdog)
> - [watchdog 0.8.2 documentation](http://pythonhosted.org/watchdog/api.html#event-classes)
> - [pygit: Just enough of a Git client to create a repo, commit, and push itself to GitHub](http://benhoyt.com/writings/pygit/)
[图解git 用图片分析学习git原理](http://huanglei.me/git-theory.html)
[小白进阶之Scrapy第一篇](http://cuiqingcai.com/3472.html)
