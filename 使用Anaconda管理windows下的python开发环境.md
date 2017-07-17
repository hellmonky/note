# 使用Anaconda管理windows下的python开发环境

Python由于2和3版本的过渡，在使用中存在很多的麻烦，而且在使用pip安装第三方库支持的时候也会遇到各种问题，所以习惯了maven或者gradle等管理工具的java开发工程师对于python的版本感觉就是一个字：乱。
既然每个人都会到这个问题，就会有专门的人去努力解决这个问题，我们现在使用的Anaconda就是一种解决方式，选择他的原因在于他对于python本身也提供了管理功能，就像对于JDK1.7和JDK1.8的切换一样，非常有用。

## Anaconda简介：
Anaconda是一个用于科学计算的Python发行版，支持 Linux, Mac, Windows系统，提供了包管理与环境管理的功能，可以很方便地解决多版本python并存、切换以及各种第三方包安装问题。Anaconda利用工具/命令conda来进行package和environment的管理，并且已经包含了Python和相关的配套工具。

### windows环境下的安装：
官方下载速度太慢，可以使用国内的镜像加速，这儿使用清华的镜像：
```shell
https://mirrors.tuna.tsinghua.edu.cn/anaconda/archive/
```
访问这个页面，然后按照Anaconda2和Anaconda3下载python2和python3为主版本的最新对应安装包。

### 使用国内镜像源：

参考：
[Anaconda 镜像使用帮助](https://mirrors.tuna.tsinghua.edu.cn/help/anaconda/)

## Conda使用：
对于Anaconda的使用，核心在于对conda的使用。Conda在github上进行了开源，根据 [官方地址](https://github.com/conda/conda) 上的介绍：
> - Conda is a cross-platform, language-agnostic binary package manager.

conda可以自我升级，添加完成国内的源之后，可以升级看看：
conda update conda



## 开发环境搭建：
我们搭建一个标准的python3.6主版本的运行环境：
首先创建一个环境：
conda create -n python36 python=3.6
然后看看当前所安装的环境有那些：
conda info -e
然后给这个环境完整的anaconda环境：
conda install -n python36 anaconda
这将会安装非常多的包，安装完毕，可以在路径：
C:\Users\admin\AppData\Local\conda\conda\envs\
这个目录下看到刚刚命名为python36的目录，整个环境约1.4G，果然是巨大啊。
如果觉得这个环境其实用不上，那就卸载安装好的anaconda包了：
conda remove -n python36 anaconda
我们还可以查看当前已经安装的包：
conda list

现在我们切换到初始环境下：
activate python27
deactivate python27

### 搭建TensorFlow开发环境：
因为在widnows环境下，TensorFlow只支持python3.6及以上版本，所以现在需要切换到python36环境下，然后使用包安装命令：
conda install -n python36 tensorflow


### 使用cmder完成搭建：
[将Cmder添加到系统右键菜单中](http://blog.csdn.net/hicoldcat/article/details/64904652)

IDEAServer:
cmd /k  "C:\Users\yuanlai.xwt\Desktop\idea.bat" -new_console:d:%USERPROFILE%


和pycharm搭配使用问题：
在pycharm中升级的源，和anaconda中添加的源不同，导致虽然现在可以在pycharm中进行升级，但是对于conda中的conda-meta文件夹下的包说明并没有修改，导致现在pycharm中查看到的版本和使用conda查看的库版本不同。
但是实际上现在这个库确实存放的位置还是一样的，只有一个最新版本。
现在的处理方式就是保证conda和pycharm使用了同一个源。而且，

