<!-- TOC -->

- [small tool by python](#small-tool-by-python)
    - [python同步任务：](#python同步任务)
        - [watchdog模块的使用：](#watchdog模块的使用)
        - [添加自定义文件变化监听事件处理：](#添加自定义文件变化监听事件处理)
        - [完成远程的文件变更自动更新：](#完成远程的文件变更自动更新)
        - [添加系统开机启动：](#添加系统开机启动)
        - [参考文档](#参考文档)

<!-- /TOC -->

# small tool by python

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

关于这部分的内容，涉及到git的原理，找到了篇不错的文章：[git-recipes](#参考1)



### 添加系统开机启动：
可以将上述代码作为服务，在系统启动的时候自动开启，给用户提供后台服务，完成自动化操作。


### 参考文档
> - [基于Git的文件自动同步的思考和实现](http://wuzhiwei.net/file_sync_git/)
> - [watchdog--监控文件系统变化](http://sapser.github.io/python/2014/07/25/watchdog)
> - [watchdog 0.8.2 documentation](http://pythonhosted.org/watchdog/api.html#event-classes)
> - [pygit: Just enough of a Git client to create a repo, commit, and push itself to GitHub](http://benhoyt.com/writings/pygit/)
> - <span id = "参考1">[git-recipes](https://github.com/geeeeeeeeek/git-recipes/wiki)</span>
