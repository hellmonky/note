;;随着配置文件的增加，发现一个最为明显的问题：不同模式下存在快捷键冲突
;;使用global-set-key为全局按键绑定
;;而在某种mode中，应该避免污染全局按键设置，使用local-set-key。
;;应该习惯于使用 mode-hook 来针对不同mode进行个性化的设置。
;;所以最好就是根据自己需求仔细使用定制，那么整个设置也应该做出调整。
;;来源：http://blog.jamespan.me/2015/04/05/the-return-of-the-emacs/


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;一 基本外观设置
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;行号显示
(setq column-number-mode t)
(global-linum-mode t)
;禁用工具栏
(tool-bar-mode 0)
;禁用菜单栏，F10 开启关闭菜单
(menu-bar-mode 0)
;禁用滚动栏，用鼠标滚轮代替
;;(scroll-bar-mode 0)
;禁用启动画面
(setq inhibit-startup-message t)
;尺寸：这儿设置宽度为90个字符，用于显示编码内容
(setq initial-frame-alist '( (width . 90) (height . 35)))
;时间设置
(display-time-mode 1);;启用时间显示设置，在minibuffer上面的那个杠上
(setq display-time-24hr-format t);;时间使用24小时制
(setq display-time-day-and-date t);;时间显示包括日期和具体时间
(setq display-time-interval 10);;时间的变化频率，单位多少来着？
;设置默认打开目录
(setq default-directory "E:\\MyGit\\note\\")
;改变 Emacs 固执的要你回答 yes 的行为。按 y 或空格键表示 yes，n 表示 no
(fset 'yes-or-no-p 'y-or-n-p)
;打开括号匹配显示模式
(setq show-paren-mode t)
;括号匹配时可以高亮显示另外一边的括号，但光标不会烦人的跳到另一个括号处
(setq show-paren-style 'parenthesis)
;光标靠近鼠标指针时，让鼠标指针自动让开，别挡住视线。
(setq mouse-avoidance-mode 'animate)
;在标题栏显示buffer的名字，而不是 emacs@wangyin.com 这样没用的提示
(setq frame-title-format "emacs@%b")
;允许emacs和外部其他程序的粘贴
(setq x-select-enable-clipboard t)
;设置有用的个人信息,这在很多地方有用。
(setq user-full-name "hellmonky")
(setq user-mail-address "hellmonky@163.com")
;; 备份文件目录
(setq backup-by-copying t) ; 自动备份
;;自动备份目录~/.emacs.d/backup
(setq backup-directory-alist '(("." . "~/backup"))) 
(setq delete-old-versions t) ; 自动删除旧的备份文件
(setq kept-new-versions 2) ; 保留最近的3个备份文件
(setq kept-old-versions 1) ; 保留最早的2个备份文件
(setq version-control t) ; 多次备份
;;鼠标设置
;; 鼠标滚轮，把默认滚动改为3行 
(defun up-slightly () (interactive) (scroll-up 3)) 
(defun down-slightly () (interactive) (scroll-down 3)) 
(global-set-key [mouse-4] 'down-slightly) 
(global-set-key [mouse-5] 'up-slightly)
;;;;调整滚轮的速度，实际测试效果不好
(defun smooth-scroll (increment)
  (scroll-up increment) (sit-for 0.05)
  (scroll-up increment) (sit-for 0.02)
  (scroll-up increment) (sit-for 0.02)
  (scroll-up increment) (sit-for 0.05)
  (scroll-up increment) (sit-for 0.06)
  (scroll-up increment))
(global-set-key [(mouse-5)] '(lambda () (interactive) (smooth-scroll 1)))
(global-set-key [(mouse-4)] '(lambda () (interactive) (smooth-scroll -1)))
;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;二 windows下使用emacs的一些调整：
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;来源：http://kidneyball.iteye.com/blog/1014537
;;<1>C-z默认是挂起emacs，跳回到shell中，这对文本型的shell很有用。
;;但在windows中，事实上变成了毫无实际意义的窗口最小化，浪费了C-z这么顺手的键。
;;可以用以下代码把C-z改为一个类似C-x的组合起始键。
;;这样就可以自定义使用C-z来完成类似于C-x的功能了。
(define-prefix-command 'ctl-z-map)
(global-set-key (kbd "C-z") 'ctl-z-map)
;;<2>实际使用中经常需要使用系统剪贴板（与其他编辑器或浏览器互相复制粘贴）
;;CUA模式对按键习惯影响太大，不想用。用鼠标中键可以粘贴，但太麻烦。
;;使用上述自定义功能组合按键来实现复制和粘贴功能
;;可以在.emacs中加入以下代码，把C-z C-c设为复制到系统剪贴板，C-z C-v设为从系统剪贴板粘贴。
(global-set-key "\C-z\C-c" 'clipboard-kill-ring-save)
(global-set-key "\C-z\C-v" 'clipboard-yank)
;;<3>使用emacs时经常需要管理多个buffer，C-x C-b的默认界面太过简陋。
;;emacs事实上已经提供了更好的buffer管理界面ibuffer，在配置文件中选用即可。
;;启用ibuffer支持，增强*buffer*  
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
;;<4>标记文本区域起始位置的按键命令是C-Space，在中文系统下被输入切换热键冲掉了。
;;默认的候补方案是M-@，其实就是Alt-Shift-2，非常难按。改为了C-c m。 
(global-set-key "\C-cm" 'set-mark-command)

;;来源：https://icoderme.wordpress.com/2011/02/02/ctab_el/
;;<4>使用ctab显示多个buffer：
;;自己修改了代码的Bind/unbind shortcut keys部分，使用C-c 然后左右键来切换，防止冲突
;;本来想改为M-x，但是不行（http://stackoverflow.com/questions/9462111/emacs-error-key-sequence-m-x-g-starts-with-non-prefix-key-m-x）
(add-to-list 'load-path  "~/.emacs.d/project/ctab")
(require 'ctab)
(ctab-mode t)
;; 如果需要让.h文件和.c/.cpp文件排在一起，则增加下面一行:
(setq ctab-smart t)
;;终于可以方便的查看当前buffer了，使用C-x k来关闭当前的buffer，泪流满面啊！

;;<5>设置默认编码:http://blog.waterlin.org/articles/set-emacs-default-coding-system.html
;;使用M-x describe-coding-system 查看当前buffer的编码类型
;;因为在使用orgmode的时候导出的md文件编码不是utf8的
;;设置默认写文件需要的编码
(setq default-buffer-file-coding-system 'utf-8)
;;设置默认读取文件的编码
(prefer-coding-system 'utf-8)
;;如果读取的文件还是不对，那么就用M-x revert-buffer-with-coding-system来重新设置读入的编码类型
;;碰到文件编码混乱的时候，最重要的一点，看到乱码的文件，不要随便保存。



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;三 自己添加的扩展
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;（1）获取所有正则表达式匹配的行放在buffer中：
;;来源：http://stackoverflow.com/questions/2289883/emacs-copy-matching-lines
;;首先定义获取正则表达式匹配结果的函数，然后放置在名为matching的buffer中
(defun copy-lines-matching-re (re)
  "find all lines matching the regexp RE in the current buffer
  putting the matching lines in a buffer named *matching*"
  (interactive "sRegexp to match: ")
  (let ((result-buffer (get-buffer-create "*matching*")))
    (with-current-buffer result-buffer 
      (erase-buffer))
    (save-match-data 
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward re nil t)
          (princ (buffer-substring-no-properties (line-beginning-position) 
                                                 (line-beginning-position 2))
                 result-buffer))))
    (pop-to-buffer result-buffer)))

(defun get_all_matched_results (re)
  ;;添加交互提示
  (interactive "Regexp to find match: ")
  ;;开始查找函数
  ;;创建一个新的名为“*matched_results*”的buffer，并且把它作为result-buffer来存储数据的结果
  (let ((result-buffer (get-buffer-create "*matched_results*")));;let表达式创建局部变量result-buffer
    ;;对当前新建的result-buffer进行擦除操作，保证没有内容
    (with-current-buffer result-buffer (erase-buffer))
    ;;然后将当前获取的结果写入到result-buffer中
    (save-match-data
      ;;保存当前缓存的内容，包括位置和标记
      (save-excursion
	;;（1）首先，将标记设置为当前buffer的开始位置处
	;;[This expression jumps the cursor to the minimum point in the buffer, that is, to the beginning of the buffer (or to the beginning of the accessible portion of the buffer if it is narrowed.)](http://www.gnu.org/software/emacs/manual/html_node/eintr/simplified_002dbeginning_002dof_002dbuffer.html)
	;;goto-char：Emacs内建函数，其参数是个（内嵌）函数调用。也就是说这个函数的参数也是函数
	;;point是Emacs内部名称，表示光标的当前位置。point-min返回当前buffer里第一个字符的位置值，几乎总是1
	;;goto-char调用point-min，就等于其参数值为1，效果等同于把point移至buffer起始处。（http://blog.donews.com/leal/archive/2005/10/17/591883.aspx）
	(goto-char (point-min))
	;;（2）然后开始遍历当前的所有buffer，开始查找正则匹配内容
	;;re-search-forward函数在当前buffer范围内搜索一个正则表达式。如果查找成功，它将point设置在匹配目标的最后一个字符的后面，并且返回成功
	;;与search-forward类似，它接收四个参数：
	;;第一个参数是要查找的正则表达式。表达式是一个被引号包括的字符串。
	;;第二个参数是可选参数，限制搜索的范围，如果它不是nil，那么它一定是当前缓冲区中的某个位置（point），用于设定搜索的上限。
	;;第三个可选参数指定搜索失败时如何处理：1 如果设置为nil，则导致函数在搜索失败时显示错误信息；2 如果设置为t，则什么也不做返回nil；3 如果设置为其它值将标记到设置的查询范围终点，并且返回nil。
	;;第四个可选参数，用于指定重复次数。负数表示重复的向后搜索。
	(while (re-search-forward re nil t);;这儿就是在当前buffer的所有范围内查找re匹配的结果，如果没有查询到就返回nil.
	  ;;princ：这个函数用于连接字符串
	  ;;buffer-substring-no-properties：接受两个参数start和end，用于拷贝两者之间的字符串，并且不带有property信息
	  ;;line-beginning-position：调用beginning-of-line函数，这个函数将标记移动到当前行的行首，如果带有非nil和1的正整型参数n，这个函数向前移动(n-1)行然后，标记该行的行首
	  ;;所以 (line-beginning-position)和 (line-beginning-position 2)之间的就是正则匹配的那一行了
	  (princ (buffer-substring-no-properties (line-beginning-position) (line-beginning-position 2))
		 result-buffer))))
      ;;最后，将获取的结果显示到创建的buffer中
      ;;pop-to-buffer函数将当前数据放在参数result-buffer指定的buffer中显示
    (pop-to-buffer result-buffer)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;四 定制设置
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; （1）主题和字体支持：
;;<1>字体显示设置：
;;来源：http://zhuoqiang.me/torture-emacs.html，作者还给出了自己的设置方案：https://bitbucket.org/zhuoqiang/emacs_configuration/overview
;;可能存在的修正情况：http://baohaojun.github.io/perfect-emacs-chinese-font.html
;;首先，我们要能判断某个字体在系统中是否存在：
(defun qiang-font-existsp (font)
  (if (null (x-list-fonts font))
      nil
    t))
;;其次，要到用户预定义的字体列表中找出首个存在的字体：
(defvar font-list '("Microsoft Yahei" "文泉驿等宽微米黑" "黑体" "新宋体" "宋体"))
(require 'cl) ;; find-if is in common list package
(find-if #'qiang-font-existsp font-list)
;;还要有个辅助函数，用来产生带上字体大小信息的字体描述文本：
(defun qiang-make-font-string (font-name font-size)
  (if (and (stringp font-size)
           (equal ":" (string (elt font-size 0))))
      (format "%s%s" font-name font-size)
    (format "%s %s" font-name font-size)))
;;自动设置字体函数
(defun qiang-set-font (english-fonts
                       english-font-size
                       chinese-fonts
                       &optional chinese-font-size)

  "english-font-size could be set to \":pixelsize=18\" or a integer.
If set/leave chinese-font-size to nil, it will follow english-font-size"
  (require 'cl) ; for find if
  (let ((en-font (qiang-make-font-string
                  (find-if #'qiang-font-existsp english-fonts)
                  english-font-size))
        (zh-font (font-spec :family (find-if #'qiang-font-existsp chinese-fonts)
                            :size chinese-font-size)))

    ;; Set the default English font
    ;;
    ;; The following 2 method cannot make the font settig work in new frames.
    ;; (set-default-font "Consolas:pixelsize=18")
    ;; (add-to-list 'default-frame-alist '(font . "Consolas:pixelsize=18"))
    ;; We have to use set-face-attribute
    (message "Set English Font to %s" en-font)
    (set-face-attribute 'default nil :font en-font)

    ;; Set Chinese font
    ;; Do not use 'unicode charset, it will cause the English font setting invalid
    (message "Set Chinese Font to %s" zh-font)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset zh-font))))
;;然后调用字体自动设置函数来实现字体加载
(qiang-set-font
 '("Consolas" "Monaco" "DejaVu Sans Mono" "Monospace" "Courier New") ":pixelsize=18"
 '("Microsoft Yahei" "文泉驿等宽微米黑" "黑体" "新宋体" "宋体"))
;;最后还要增加鼠标滑轮调整字体大小的方式
;; For Linux
(global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
(global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)
;; For Windows
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

;;<2>主题设置：所有主题相关的都统一放在color-theme文件夹下
(add-to-list 'load-path "~/.emacs.d/color-theme/color-theme-6.6.0")
(require 'color-theme)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 配色方案1：solarized模式
;;(color-theme-initialize)
;; 安装solarized主题
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/color-theme/emacs-color-theme-solarized/")
;; 载入主题
;;(load-theme 'solarized t)
;;(set-frame-parameter nil 'background-mode 'dark)
;;(set-terminal-parameter nil 'background-mode 'dark)
;;(enable-theme 'solarized)
;;;;调节整体的主题表现，块注释
;;(add-hook 'after-make-frame-functions
;;          (lambda (frame)
;;            (set-frame-parameter frame
;;                                 'background-mode
;;                                 (if (display-graphic-p frame) 'light 'dark))
;;            (enable-theme 'solarized)))
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 配色方案2：黑板模式
(load-file "~/.emacs.d/color-theme/blackboard/color-theme-blackboard.el")
(color-theme-blackboard)


;; （2）添加auto-complete插件，并且定制
(add-to-list 'load-path "~/.emacs.d/auto-complete/fuzzy-el")
(add-to-list 'load-path "~/.emacs.d/auto-complete/popup-el")
(add-to-list 'load-path "~/.emacs.d/auto-complete/auto-complete")
;; 添加auto-complete插件的增强补全
(add-to-list 'load-path "~/.emacs.d/auto-complete/pos-tip")
(require 'pos-tip)
(setq ac-quick-help-prefer-pos-tip t)
;; 添加auto-complete插件的默认字典
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             "~/.emacs.d/auto-complete/auto-complete/dict")
(ac-config-default)
;;全局使用自动补全模式
(global-auto-complete-mode t)
;;使用Ctrl+enter按键触发自动补全
(define-key ac-mode-map  [(control return)] 'auto-complete) 	
;;设置代码补全的提示帮助
(setq ac-use-quick-help t)
(setq ac-quick-help-delay 1.0)
;; 使用fuzzy来实现模糊匹配
(setq ac-fuzzy-enable t)
;;设置tab键的模式：当用C-n c-p 选中候选项时tab 表现为回车行为，即令其上屏
(setq ac-dwim t)
;;将backspace的删除后仍旧可以触发ac补全
(setq ac-trigger-commands
      (cons 'backward-delete-char-untabify ac-trigger-commands))

	  
;; （3）添加yasnippet插件
(add-to-list 'load-path "~/.emacs.d/auto-complete/yasnippet")
(require 'yasnippet)
(yas-global-mode t)
;; 然后将yasnippet和auto-complete结合起来完成自动补全过程
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")


;; （4）添加auto-complete-clang插件
(add-to-list 'load-path "~/.emacs.d/auto-complete/auto-complete-clang")

;; 设置环境变量完成对外部可执行程序的调用
(defun extra_bin (path) 
  (setenv "PATH" (concat (expand-file-name path)  
                         path-separator  
                         (getenv "PATH")))  
  (setq exec-path  
        (cons (expand-file-name path)  
              exec-path)))

;;同时更新PATH和exec-path
(mapc #'extra_bin  
      (reverse   
       '("~/extra_bin/unix-utils-bin"  
         "~/extra_bin/etc"  
         "~/extra_bin/PuTTY"  
         "~/extra_bin/clang"  
         )))

;; 加载clang  
(require 'auto-complete-clang)  
  
;; 添加c-mode和c++-mode的hook，开启auto-complete的clang扩展  
(defun clang-cc-mode-setup ()  
  (make-local-variable 'ac-auto-start)
  ;;关闭自动补全，不然很卡顿，不是所有的都需要补全
  (setq ac-auto-start nil)
  ;;设置ac-sources作为自动补全的来源，其中包含了clang和yasnippet这两个
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))

  ;;然后绑定C和C++的补全模式到自定义函数上
(add-hook 'c-mode-hook 'clang-cc-mode-setup)  
(add-hook 'c++-mode-hook 'clang-cc-mode-setup)  

;;添加系统头文件的目录，这点非常重要，clang就是通过ac-clang-flags这个变量中的设置，来正确找到所有的系统头文件的
(setq ac-clang-flags	(list
	;;添加VS的头文件到索引目录中
	"-IC:/Program Files (x86)/Microsoft Visual Studio 12.0/VC/include"
	;;添加MinGW的头文件到索引目录中
	"-IC:/MinGW/lib/gcc/mingw32/4.6.1/include"
	"-IC:/MinGW/lib/gcc/mingw32/4.6.1/include/c++"
	"-IC:/MinGW/lib/gcc/mingw32/4.6.1/include/c++/mingw32"
	"-D__MSVCRT__="))


;; （5）使用sr-speedbar管理窗口
(add-to-list 'load-path "~/.emacs.d/project/sr-speedbar")
(require 'sr-speedbar)
(setq sr-speedbar-right-side nil)
(setq sr-speedbar-width 18)
(setq dframe-update-speed t)
(global-set-key (kbd "<f5>")
	(lambda()
          (interactive)
          (sr-speedbar-toggle)))

		  

;; （6）安装ecb
(add-to-list 'load-path "~/.emacs.d/project/ecb/")
;;加载ecb
(require 'ecb)
;;默认打开ecb支持，在一般编辑的时候不会需要，如果需要的时候手动打开
;;(ecb-activate)
;;取消蛋痛的开始提示
(setq ecb-tip-of-the-day 0)
(setq ecb-help-info-start-file 0)
;;然后对ecb的布局进行处理
;;(setq ecb-layout-name "leftright2")
(setq ecb-layout-name "leftright2"
	  ecb-layout-window-sizes
	  '(("leftright2"
		 (ecb-directories-buffer-name 0.12 . 0.6428571428571429)
		 (ecb-sources-buffer-name 0.12 . 0.3392857142857143)
		 (ecb-methods-buffer-name 0.12 . 0.6428571428571429)
		 (ecb-history-buffer-name 0.12 . 0.3392857142857143))))
		 
;;然后总是显示当前的文件树关系
(setq ecb-show-sources-in-directories-buffer 'always)
;;然后显示编译结果
(setq ecb-compile-window-height 12)
;;自动启动ecb
;;(setq ecb-auto-activate t)
;;;自定义一键开关，ctrl+f1 快速打开ecb窗口的函数
(defun my-ecb-active-or-deactive ()
	(interactive)
	(if ecb-minor-mode
		(ecb-deactivate)
		(ecb-activate)))
(global-set-key (kbd "<C-f1>") 'my-ecb-active-or-deactive)


;; （7）单进程守护：在windows下只打开一个emacs，从而加快文本处理速度
(server-start)

;; （8）各种模式的添加：统一放置在mode文件夹下
;;<1>使用company-mode来替代auto-complete来处理自动补全的前端，安装简单
;;(add-to-list 'load-path "~/.emacs.d/mode/company-mode")
;;(autoload 'company-mode "company" 0 t)

;;<2>使用内置的python-mode：开启python支持，但是调用ipython还是存在问题
(require 'python)
;; 完成python相关设置
(setq
  python-shell-interpreter "ipython"
  python-shell-interpreter-args ""
  python-shell-prompt-regexp "In \\[[0-9]+\\]: "
  python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
  python-shell-completion-setup-code
    "from IPython.core.completerlib import module_completion"
  python-shell-completion-module-string-code
    "';'.join(module_completion('''%s'''))\n"
  python-shell-completion-string-code
    "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
	
;;<3>rust mode支持，来自官方：https://github.com/rust-lang/rust-mode
(add-to-list 'load-path "~/.emacs.d/mode/rust-mode")
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;;<4>haskell mode支持
;;注意：直接从github上下载的代码还需要编译生成其他文件才能够正常使用
;;因为在windows下无法编译安装，最后还是使用elpa来安装
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
;;然后添加当前安装的目录
(add-to-list 'load-path  "~/.emacs.d/elpa/haskell-mode-20150524.2347")
;;设置haskell的三个模式中的haskell-indentation-mode为有效，设置Enter和Tab的作用
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
;;增加tab支持
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;设置按键绑定的基本包
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;;设置基本变量
(custom-set-variables
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-log t))
;;Haskell-mode按键绑定
(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
(define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
;;cabal-mode按键绑定
(define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
;;设置repl是哪个（和scheme类似）
(custom-set-variables '(haskell-process-type 'ghci))
;;附上网上找到的配置资料：
;;http://bbs.chinaunix.net/thread-1270108-1-1.html
;;http://www.programgo.com/article/53751618345/
;;https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md#indentation-modes
;;http://sritchie.github.io/2011/09/25/haskell-in-emacs/
;;https://github.com/chrisdone/chrisdone-emacs/blob/master/config/haskell.el

;;添加代码格式化，根据[教程](https://github.com/haskell/haskell-mode/wiki/Autoformatting)
;;在windows平台下安装[minghc](https://github.com/fpco/minghc)
;;然后使用cabal管理包，在命令行中执行：
;;cabal update
;;cabal install stylish-haskell
(setq haskell-stylish-on-save t)




;;<5>markdown mode支持
;;来源：http://jblevins.org/projects/markdown-mode/
(add-to-list 'load-path  "~/.emacs.d/mode/markdown-mode")
;;官方给出的是标砖markdown模式的配置
;;(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
;;使用gfm-mode作为markdown的主要模式：http://superuser.com/questions/552888/auto-load-gfm-mode
(autoload 'gfm-mode "markdown-mode" "Major mode for editing Markdown files" t)
;;需要注意的是还有GitHub Flavored Markdown模式，应该进入不同的模式
(add-to-list 'auto-mode-alist '("README'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.text\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
;;标准markdown格式说明：http://maogm.com/blog/markdown-syntax.html
;;gfm-mode说明：https://help.github.com/articles/github-flavored-markdown/
;;然后还有一个对比说明：http://www.worldhello.net/gotgithub/appendix/markups.html


;;<6>scheme支持
;;来源：http://www.yinwang.org/blog-cn/2013/04/11/scheme-setup/
(add-to-list 'load-path  "~/.emacs.d/mode/scheme-mode")
;;自动加载paredit-mode
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)
;;添加配置
(require 'cmuscheme)
;;设置解释器
;;定义是否为windows环境判断
;;来源：http://kelvinh.github.io/blog/2013/07/21/scheme-configuration-in-emacs/
(defconst is-os-windows (string-equal system-type "windows-nt")
  "non-nil means it is windows operating system")
;;然后根据当前是否为windows环境选择对应的解释器
(if is-os-windows
	(setq scheme-program-name "Racket");;使windows下用的scheme解释器
	(setq scheme-program-name "guile"));;linux下使用的scheme解释器
;; bypass the interactive question and start the default interpreter
(defun scheme-proc ()
  "Return the current Scheme process, starting one if necessary."
  (unless (and scheme-buffer
               (get-buffer scheme-buffer)
               (comint-check-proc scheme-buffer))
    (save-window-excursion
      (run-scheme scheme-program-name)))
  (or (scheme-get-process)
      (error "No current process. See variable `scheme-buffer'")))
(defun scheme-split-window ()
  (cond
   ((= 1 (count-windows))
    (delete-other-windows)
    (split-window-vertically (floor (* 0.68 (window-height))))
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window 1))
   ((not (find "*scheme*"
               (mapcar (lambda (w) (buffer-name (window-buffer w)))
                       (window-list))
               :test 'equal))
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window -1))))
(defun scheme-send-last-sexp-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-last-sexp))
(defun scheme-send-definition-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-definition))
(add-hook 'scheme-mode-hook
  (lambda ()
    (paredit-mode 1)
	;;自己修改了快捷键，F11表示读入当前的s表达式
    (define-key scheme-mode-map (kbd "<f11>") 'scheme-send-last-sexp-split-window)
    (define-key scheme-mode-map (kbd "<f12>") 'scheme-send-definition-split-window)))


;;<7>org mode设置
;;<7.1>快速插入代码模版
;;Emacs Org 7 以上的版本已经开始支持 easy-template
;;只需要两部:
;;1. 文本中输入 <s
;;2. 按一下 Tab 键
;;就会出现如下内容
;;#+begin_src | <---光标处,可方便书写语言种类
;;#+end_src
;;官方给出的资料为：http://orgmode.org/manual/Easy-Templates.html
;;<7.2>自动换行
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
;;<7.3>添加markdown导出：http://stackoverflow.com/questions/22988092/emacs-org-mode-export-markdown
;;但是实际使用的时候发现，导出的md文件不是gfm格式，并且
(eval-after-load "org" '(require 'ox-md nil t))
