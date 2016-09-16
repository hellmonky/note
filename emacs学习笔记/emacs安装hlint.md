# emacs中使用hlint开发haskell

emacs作为haskell官方指定的编辑器，提供了很多的插件来帮助emacser来方便的使用haskell进行程序编写，其中的hlint就是一个出色的工具，可以帮助用户在编写程序中方便的进行代码的优化和重构。

## 1 emacs中的hlint插件：
在haskell的官方网站提供了非常多的package的相关页面和说明，其中包含hlint package的*[官方网页](https://hackage.haskell.org/package/hlint)*。在这个页面中可以看到具体的开发者的信息和最新的维护版本信息，同样的，可以看到这个包的emacs插件内容，我们就按照hlint的官方教程进行emacs的集成。

### 1.1 在当前系统中确保hlint可以正常运行：
hlint作为一个可以被编译运行的可执行文件，在当前系统中必须可以被访问，用来提供hlint的功能支持。所以首要的目标就是检查当前系统是否已经安装了hlint，如果没有建议使用stack进行安装。

安装过之后可以在命令行中输入：
```shell
hlint --version
```
查看当前的hlint版本。如果显示正确就可以继续下一步了。

### 1.2 安装hlint的emacs插件：
从[这儿](https://raw.githubusercontent.com/ndmitchell/hlint/master/data/hs-lint.el)下载最新的el文件，然后放在emacs的插件目录下。
然后编辑.emacs文件，添加如下内容：
```shell
(add-to-list 'load-path  "~/.emacs.d/mode/hlint")
(require 'hs-lint)
(defun my-haskell-mode-hook ()
   (local-set-key "\C-cl" 'hs-lint))
   (add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
```
其中"add-to-list 'load-path"表示将后面的位置作为load路径添加到列表中，这样就可以让emacs找到刚刚下载的.el文件，来解析添加hlint支持。
保存.emacs文件之后，重启emacs，然后打开一个空白的文本，输入一下代码：
```haskell
module Main where
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    putStrLn ("Hello, " ++ args !! 0)
```
然后输入M+c l调出haskell的hlint模式，就可以看到如下的提示了：
```shell
-*- mode: hs-lint; default-directory: "d:/workspace/vsproject/hellworld/app/" -*-
HLint started at Fri Sep 16 22:27:59

hlint "d:/workspace/vsproject/hellworld/app/Main.hs"
d:/workspace/vsproject/hellworld/app/Main.hs:7:26: Suggestion: Use head
Found:
  args !! 0
Why not:
  head args

1 hint

HLint exited abnormally with code 1 at Fri Sep 16 22:27:59
```
这样表示hlint的emacs插件安装正常了。


## 2 关于使用emacs编辑文本：

最近新安装了vscode，对比emacs这个传统的编辑器，他的功能定制更为简单，使用typescript作为脚本，嵌入v8引擎来进行解析，在使用上和速度上对比emacs都还可以。
而且借着hlint的使用，vscode对于新手用户更为友好，可以直接在源代码上进行提示和修正。
而emacs则新开了一个buffer来显示对当前源代码执行一次hlint获取的结果，这种虽然很正常，但是缺少实时的处理，不是非常方便使用。

综合看来还是要根据自己的实际需求和实际的使用习惯来进行编辑器的选择。
