# emacs使用随记 #

*一边学习一边联系，在实战中学习*
by hellmonky

使用Emacs编辑可以方便的根据文档的层级进行处理，这个是在其他编辑器中支持很差的，并且让用户关注于文本本身，而不是各种点击操作。

## 1 Emacs的基本操作 ##
Emacs作为编辑器，他带有丰富的基本文本操作功能，但是因为使用中需要不断的切换模式，所以和一般的编辑器使用有比较大的差异，最核心的是两个主要按键的：

```shell
C键：代表Contorl的意思，用户来控制与语言无关的单位（字符，行）。键盘上为 ctrl 键。
M键：代表Meta的意思，用来控制与语言定义的单位（如词，句子，段落）。键盘上一般为 Alt 键。
```
使用中还出现了如下内容：

```shell
-        ：在Emacs的命令中，你经常会看到  -  这个符号，它代表按住前面一个字符，比如C – x  就表示按住Ctrl再按x。
命令的取消：C – g ,或者 按2次Esc.
Esc      ：单击一次相当于M – 
```

现在记录基本的操作功能如下。

### 1.1 buffer的创建，操作和保存 ###

buffer作为使用过程中一直需要关注的最基本内容，Emacs会定时给你的buffer文件进行auto save，并在同文件目录下命名一个#文件名#的文件来保存当前编辑状态。

一些最常用的处理方法记录如下：

#### （1）创建新的buffer ####
Emacs支持多个buffer的操作，就像是多个tab标签一样进行管理，不同的buffer可以处于不同的模式下面，所以使用buffer可以很方便的满足自己的记录需求，使用：

```shell
C-x b
```
会提示你要打开的buffer名称，如果存在这个buffer就会进行切换，如果不存在这个buffer就相当于创建了一个新的buffer。

同时还可以使用：

```shell
C-x C-f
```
要求输入文件名来打开一个文件的：
>1.如果这个文件不存在，就会新建一个buffer而不是文件，保存的时候会用输入的文件名来提示保存；
>2.如果这个文件存在，就将文件内容读入到buffer中。

#### （2）将当前编辑的buffer保存到文件中，使用如下：
```shell
C-c C-s
```
提示输入保存到的文件名，需要带上后缀来制定文件类型。

#### （3）查看当前的所有buffer ####
知道当前存在的所有buffer是非常重要的内容，因为Emacs没有对类似于windows下的tab表示不同的buffer，所以查看和在不同的buffer中切换是一定要给予支持的。
Emacs默认的情况下，使用如下命令来操作buffer：

```shell
C-x C-b  ：显示所有缓冲区
C-x b    ：选择一个name缓冲区，输入在缓冲区中的文件名，默认为上一个buffer
C-x left ：切换到上一个缓冲区
C-x right：切换到下一个缓冲区
```
因为之前参考了[icoderme](https://icoderme.wordpress.com/2011/02/02/ctab_el/)根据tabbar的基础上修改的buffer展示插件，现在将.emacs配置文件修改如下：

```shell
(add-to-list 'load-path  "~/.emacs.d/project/ctab")
(require 'ctab)
(ctab-mode t)
;; 如果需要让.h文件和.c/.cpp文件排在一起，则增加下面一行:
(setq ctab-smart t)
```
重启之后所有的buffer将按照tab来显示，并且使用：

```shell
C-c 左右按键
```
来切换不同的buffer了。

#### （4）文本操作按键 ####
在buffer中进行编辑时需要熟悉各种按键来加快编辑速度，现在记录常用的编辑按键如下：

##### 标记一段内容 #####
使用emacs时候需要针对一段内容进行编辑，首先就需要将这段内容标记选定。

Emacs默认标记文本区域起始位置的按键命令是C-Space，在中文系统下被输入切换热键冲掉了。默认的候补方案是M-@，其实就是Alt-Shift-2，非常难按。于是修改为了C-c m，编辑.emacs文件，添加如下代码：

```shell
(global-set-key "\C-cm" 'set-mark-command)
```
这样通过了C-c m就可以确定标记的起始位置，然后继续选择了。
  

##### 基本的复制，粘贴和剪切 #####
以为使用windows下的编辑功能较多，Emacs默认使用的复制粘贴按键需要多次，不方便使用，现在把C-c C-c设为复制到系统剪贴板，C-c C-v设为从系统剪贴板粘贴。
（1）首先开启Emacs和系统其它部分的复制粘贴交互：
```shell
(setq x-select-enable-clipboard t)
```
（2）然后添加复制粘贴支持：
```shell
(global-set-key "\C-z\C-c" 'clipboard-kill-ring-save)
(global-set-key "\C-z\C-v" 'clipboard-yank)
```
（3）剪切还是使用Emacs中默认的快捷键处理，将标记选定的文本剪切：
```shell
C-w
```


##### 基本的恢复 #####
如果当前操作出现问题，需要进行恢复上一次操作，那么就使用：
```shell
C-x u
```
表示undo。


##### 段落编辑处理 #####
对文本编辑的时候有时候需要将一个块的文字作为整体处理，常见的操作有：
（1）删除当前整行：
```shell
C-s-Backspace
```
就是按住Ctrl，然后按s和删除键，当前行就全部给删除了。
（2）


### 1.2 其他命令 ###
上述中的命令是自己在学习中使用的，还有一些常用命令用于备忘，可以逐渐学习。

光标的移动
在编辑文本时对光标的控制。

```shell
上：C-p(previous).

下：C-n(next).

左：C-b(back).

右：C-f(forward).

移动到文件的开始处：M-<.

移动到文件的结尾处：M->.

移动到行首：C-a.

移动到行末：C-e(end).

向前移动一个词：M-f.

向后移动一个词：M-b.

向前移动一个句子：M-a.

向后移动一个句子：M-e.

向下滚动一屏：C-v(view).

向上滚动一屏：M-v.

将当前行置于屏幕中间：C-l.  2次 C-l 置于屏幕首，3次 C-l 置于屏幕末。
```
 

文件的编辑

对文本的删除，复制，粘贴等。
```shell
删除光标前字符：backspace（回车键上面那个）.

删除当前字符：C-d(delete).

删除光标前的一个词：M-backspace.

删除光标后的一个词：M-d.

删除光标处到行末的字符：C-k(kill).

删除光标处到句末的字符：M-k.

撤销：C-x u(undo). 或者 C-/ 再或者 C-_ .推荐使用第一种,方便。

移除标记mark set:C-@，即标记从光标处开始的字符。

移除：C-w,移除mark set处到现光标处的字符。

召回上一次移除文字：C-y(yank).

召唤以前移除的文字：M-y.注意，在C-y使用之后使用。

复制:M-w,从上一次mark set处，到现在光标处的字符。使用C-y粘贴。

全选：C-x h.
```


文本的搜索
Emacs可以向前，向后搜索字符串，搜索命令是渐进的（incremental）的，就是搜索与输入同步，没输入一个字符，Emacs就已经开始搜索了。
```shell
向前搜索：C-r.

向后搜索：C-s.
```
注意：在搜索时候，可以按C-s/r 查看下/上一处，C-g取消搜索，回到初始搜索光标处；<Enter>结束搜索，光标留在搜索结果上。


多窗口
Emacs迷人之处很多，能在一个屏幕上同时显示多个文件就是其中之一。

```shell

添加窗口：C-x 2.新添加的窗口为当前文件。

关闭当前窗口外窗口：C-x 1.

滚动下方窗口（向下）：C-M-v

滚动下方窗口（向上）：C-M-Shift-v.

遍历窗口：C-x o.

在新窗口中打开文件：C-x 4 C-f.
```

## 2 Emacs文件管理
使用中需要对当前操作的各种文件进行管理，那么就需要对当前的文件树结构进行操作。
Emacs自带的文件处理可以很容易的进行基本的操作。

### 2.1 进入文件列表 ###
使用如下命令进入文件列表
```shel
C-x d
```
进入之后可以

## 3 emacs的markdown-mode使用
本文件使用emacs的markdown mode来进行编辑，参考了emacs的[官方](http://jblevins.org/projects/markdown-mode/)教程。并且使用gfm-mode作为默认模式，这样做可以很方便的在github下发布自己笔记。下面列举出自己在使用中用到的命令。

### 3.1 进入gfm-mode ###
新建一个buffer或者打开非markdown绑定的文件类型时候，如果需要切换到markdown模式下，输入：

```shell
M-x gfm-mode
```
回车后当前buffer进入Google风格的markdown模式，就可以开始编辑了。

### 3.2 快速插入代码
程序员在编辑笔记中最常用的就是插入代码，如果手动放置代码内容会很麻烦，使用：

```shell
C-c C-s P
```
就可以打开快速插入gfm风格代码的功能，如果P小写就是标准markdown的代码块，用tab来表示嵌入。

### 3.3 插入标题 ###
markdown中使用层级标题来进行排列，那么如何快速的插入制定的层级标题就是很重要的步骤，使用：

```shell
C-c C-t 层级数
```
输入上述快捷键之后，接着输入层级就可以快速插入对应的层级了，然后输入标题继续编辑。


## 4 org-mode使用 ##
org mode是Emacs中的核心模式，被广泛的使用来进行知识管理。从Emacs23版本之后被内置，可以称得上为神器中的神器，因为学习上比较类似于markdown，所以这一部分是最为核心的模式，一定要学习和掌握。
网上有非常多的教程，但是最好的方式还是参考[官方](http://orgmode.org/manual/)给出的教程来进行学习。

### 4.1 org自动换行 ###
org模式下默认没有自动换行的功能，我们在.emacs文件里面添加如下代码，实现自动换行：
```shell
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
```
添加完毕之后再org模式下编辑就可以自动换行了。

### 4.2 org文件导出###
使用org模式编辑文本之后，转换为其他格式的文件方便阅读和分享，最常见的是导出为html静态网页：

```shell
```shell
上：C-p(previous).

下：C-n(next).

左：C-b(back).

右：C-f(forward).

移动到文件的开始处：M-<.

移动到文件的结尾处：M->.

移动到行首：C-a.

移动到行末：C-e(end).

向前移动一个词：M-f.

向后移动一个词：M-b.

向前移动一个句子：M-a.

向后移动一个句子：M-e.

向下滚动一屏：C-v(view).

向上滚动一屏：M-v.

将当前行置于屏幕中间：C-l.  2次 C-l 置于屏幕首，3次 C-l 置于屏幕末。
```
 

文件的编辑

对文本的删除，复制，粘贴等。
```shell
删除光标前字符：backspace（回车键上面那个）.

删除当前字符：C-d(delete).

删除光标前的一个词：M-backspace.

删除光标后的一个词：M-d.

删除光标处到行末的字符：C-k(kill).

删除光标处到句末的字符：M-k.

撤销：C-x u(undo). 或者 C-/ 再或者 C-_ .推荐使用第一种,方便。

移除标记mark set:C-@，即标记从光标处开始的字符。

移除：C-w,移除mark set处到现光标处的字符。

召回上一次移除文字：C-y(yank).

召唤以前移除的文字：M-y.注意，在C-y使用之后使用。

复制:M-w,从上一次mark set处，到现在光标处的字符。使用C-y粘贴。

全选：C-x h.
```


文本的搜索
Emacs可以向前，向后搜索字符串，搜索命令是渐进的（incremental）的，就是搜索与输入同步，没输入一个字符，Emacs就已经开始搜索了。
```shell
向前搜索：C-r.

向后搜索：C-s.
```
注意：在搜索时候，可以按C-s/r 查看下/上一处，C-g取消搜索，回到初始搜索光标处；<Enter>结束搜索，光标留在搜索结果上。


多窗口
Emacs迷人之处很多，能在一个屏幕上同时显示多个文件就是其中之一。

```shell

添加窗口：C-x 2.新添加的窗口为当前文件。

关闭当前窗口外窗口：C-x 1.

滚动下方窗口（向下）：C-M-v

滚动下方窗口（向上）：C-M-Shift-v.

遍历窗口：C-x o.

在新窗口中打开文件：C-x 4 C-f.
```


## 2 Emacs文件管理
使用中需要对当前操作的各种文件进行管理，那么就需要对当前的文件树结构进行操作。
Emacs自带的文件处理可以很容易的进行基本的操作。

### 2.1 进入文件列表 ###
使用如下命令进入文件列表
```shel
C-x d
```
进入之后可以

## 3 emacs的markdown-mode使用
本文件使用emacs的markdown mode来进行编辑，参考了emacs的[官方](http://jblevins.org/projects/markdown-mode/)教程。并且使用gfm-mode作为默认模式，这样做可以很方便的在github下发布自己笔记。下面列举出自己在使用中用到的命令。

### 3.1 进入gfm-mode ###
新建一个buffer或者打开非markdown绑定的文件类型时候，如果需要切换到markdown模式下，输入：

```shell
M-x gfm-mode
```
回车后当前buffer进入Google风格的markdown模式，就可以开始编辑了。

### 3.2 快速插入代码
程序员在编辑笔记中最常用的就是插入代码，如果手动放置代码内容会很麻烦，使用：

```shell
C-c C-s P
```
就可以打开快速插入gfm风格代码的功能，如果P小写就是标准markdown的代码块，用tab来表示嵌入。

### 3.3 插入标题 ###
markdown中使用层级标题来进行排列，那么如何快速的插入制定的层级标题就是很重要的步骤，使用：

```shell
C-c C-t 层级数
```
输入上述快捷键之后，接着输入层级就可以快速插入对应的层级了，然后输入标题继续编辑。


## 4 org-mode使用 ##
org mode是Emacs中的核心模式，被广泛的使用来进行知识管理。从Emacs23版本之后被内置，可以称得上为神器中的神器，因为学习上比较类似于markdown，所以这一部分是最为核心的模式，一定要学习和掌握。
网上有非常多的教程，但是最好的方式还是参考[官方](http://orgmode.org/manual/)给出的教程来进行学习。

### 4.1 org自动换行设置 ###
org模式下默认没有自动换行的功能，我们在.emacs文件里面添加如下代码，实现自动换行：
```shell
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
```
添加完毕之后再org模式下编辑就可以自动换行了。

### 4.2 org模式下的编辑功能 ###
org支持各种的编辑状态，常用的有如下：

#### 使用easy-template插入代码 ####
Emacs24之后内置的org模式支持easy-template，只需要
```shell
1. 文本中输入 <s
2. 按一下 Tab 键
```
就会出现如下内容
```shell
#+begin_src | <---光标处,可方便书写语言种类

#+end_src
```
官方还给出了[其他模板](http://orgmode.org/manual/Easy-Templates.html)的插入功能。


### 4.3 org文件导出###
使用org模式编辑文本之后，转换为其他格式的文件方便阅读和分享，那么就需要导出为不同的格式，基本命令为：
```shell
C-c C-e
```
将会调出export的界面，然后用按键点击来进行选择导出的格式。

#### （1）导出为html静态网页格式 ####
Emacs的org模式内置了对html格式的导出功能，使用：
```shell
C-c C-e h h
```
就会在当前文件夹下导出为带有样式的静态html格式文件。


#### （2）导出为md格式 ####
markdown作为另一种轻量级格式，org内置了导出功能，但是默认是不打开的，根据[stackoverflow的资料](http://stackoverflow.com/questions/22988092/emacs-org-mode-export-markdown)所以需要在.emacs中添加：
```shell
(eval-after-load "org" '(require 'ox-md nil t))
```
来打开markdown格式导出组件，然后根据[官网手册](http://orgmode.org/manual/Markdown-export.html#Markdown-export)使用：
```shell
C-c C-e m m
```
导出为同名的md文件。
