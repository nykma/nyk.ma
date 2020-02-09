---
title: "Emacs 自力求生指南 ── 前言"
author: ["Nyk Ma"]
date: 2020-02-08T16:47:00+08:00
lastmod: 2020-02-08T23:34:17+08:00
tags: ["emacs"]
categories: ["Tutorial"]
draft: true
---

> 建议你在阅读本文前先通读一遍 [Emacs 101 新手求生指南](https://github.com/emacs-tw/emacs-101-beginner-survival-guide) 。


## <span class="section-num">1</span> Emacs 是编辑器？ {#emacs-是编辑器}

呃……既是，也不是。就我理解， Emacs 是一个长得像文本编辑器的 [REPL](https://zh.wikipedia.org/wiki/REPL) ：

-   它是 `Elisp` —— 一个纯函数式语言 —— 的运行时。
-   每次你打开 Emacs ，它都会使用一个干净的[^fn:1]运行时跑一遍 `~/.emacs.d/init.el` 脚本。[^fn:2]
-   你所看到的窗口、文字、状态栏、光标等，均为上述脚本产生的 **副作用** 。


## <span class="section-num">2</span> 缺点？ {#缺点}

先说缺点吧，客观一些。


### <span class="section-num">2.1</span> 学习成本高 {#学习成本高}

一个黑 Emacs 比较上档次一点的角度[^fn:3]是「为了一个编辑器要学习一个新语言」。的确，想要随心所欲地使用 Emacs 的话，不可避免地需要用 Elisp 深度定制。

推荐参考书是 [ANSI Common Lisp 指南](http://acl.readthedocs.io/en/latest/index.html) 。

> 把 Emacs 当作 CL 的 REPL 基本没啥问题，只要 `(require 'cl)` 就可以用 Common Lisp 的关键字了。

<!--quoteend-->

> 这么想，你在用一个编辑器的时候顺便还能把 LISP 大家族 [Racket](https://racket-lang.org/)、[Clojure](https://clojure.org/) ([ClojureScript](https://clojurescript.org/index))、[Scheme](http://schemers.org/) 都入了个门，何乐而不为啊（（


### <span class="section-num">2.2</span> 有些操作会阻塞编辑器 {#有些操作会阻塞编辑器}

虽然 Emacs 26+ 的 async 已经实现得很好了，日常操作基本不会被阻塞。但你依然还是会被什么套件里的同步调用卡住。比如

-   有些网络 IO，比如 gnus 下载新闻
-   org-mode 里的 export to PDF
-   开一个大图片预览 Buffer 或打开一个 PDF

> 惊人的事实： Emacs 通过把 PDF 转成图片来预览。所以如果你对一个上百 M 的大 PDF 做缩放操作会十分酸爽……

没有一个工具是万能的，甚至 Emacs 也是。如果你觉得有个功能 Emacs 干得不够好，那就立刻换一个工具吧。时间宝贵。


### <span class="section-num">2.3</span> 不够轻量，导致默认装机量不够 {#不够轻量-导致默认装机量不够}

这个是真的没办法了… `vi` （不是 `vim` ）几乎是每个服务器 Linux 的标配，但 Emacs 的基础包实在太大[^fn:4]，甚至不少桌面版 Linux 都不会预装它。

不过 macOS 居然预装了它，难道帮主爱用？


## <span class="section-num">3</span> 长处？ {#长处}


### <span class="section-num">3.1</span> GUI 友好、鼠标友好、不反直觉 {#gui-友好-鼠标友好-不反直觉}

奇怪的是没几个 Emacs 介绍文提到这个的：Emacs 鼓励你使用它的 GUI 模式。

-   无参数启动 `emacs` 就是 GUI[^fn:5]
-   自带了可深度自定义的 Menu 和 Toolbar
    -   大部分常用功能都能在菜单栏里找到，甚至还能显示当前快捷键组合。前期我建议你不要关掉菜单栏，找个功能还是相当方便的……
-   鼠标的框选、滚轮、双击、右键菜单等操作和你的使用习惯一致
-   外观、字体的颗粒度极细。可以使用多套字体、所有桌面色彩和花哨的 window decoration
-   甚至 Emacs 本身有一个类似控制面板的 GUI 配置界面 `M-x customize` ，可以不写 Elisp 、不碰配置文件也能轻度定制编辑器行为
-   看图、浏览网页、刷 Telegram 、预览 Markdown 等场景几乎只有在 GUI 内才 make sense


### <span class="section-num">3.2</span> 天生支持 C/S 模式 {#天生支持-c-s-模式}

你肯定有过想在打开两个 vim 进程间互通剪贴板或光标互相跳转的场景，遗憾的是，不能。[^fn:6]

Emacs 能以 server 模式启动， expose 到端口或 socket 文件。client 能随时连接它[^fn:7]。


### <span class="section-num">3.3</span> 文档又多又全还易读 {#文档又多又全还易读}

`M-x info` 里的文档每一篇都可以拿来当小说读。


### <span class="section-num">3.4</span> 随时 Hack ，彻底 Hack {#随时-hack-彻底-hack}

你的所有可见元素和变化都是 **执行函数** 所带来的副作用，所以你对编辑器的改造几乎没有场所和功能限制。

来几个例子体验一下 Emacs 的可定制性吧。


#### <span class="section-num">3.4.1</span> 自己造一个简单的 Vim 按键模式 {#自己造一个简单的-vim-按键模式}

在 Emacs 中，「按 `j` ，一个字母 `j` 出现在屏幕上」也是 **函数调用带来的副作用** ！

按 `C-h k j` 可以看到调用的函数叫 `(self-insert-command)` 。

我们试着重新绑定 `j` 让它变成「跳到下一行」。

1.  我们只知道按方向键下可以跳到下一行。首先用 `C-h k (方向键下)` 来查询对应的函数调用。我们可以得到很多信息：
    -   函数调用是 `(next-line)`
    -   方向键下在 Emacs 里写作 `<down>`
    -   这个函数也被绑定到 `C-n` 上了。
2.  在 `*scratch*` Buffer 里试试看：

    ```elisp
    (global-set-key ;; 全局绑定设置
     (kbd "j")      ;; 按键 j
     'next-line)    ;; 调用 (next-line)
    ```
3.  把光标移到最后一个括号的后面，按 `C-x C-e` （ `(eval-last-sexp)` ），现在按键盘上的 `j` ，你会发现光标真的向下跑了。
4.  以此类推把 `hjkl` 都绑定了吧（

> 我们这个例子太糙，执行了这个之后你的 `j` 就再也打不出字来了，最方便的复原法就是重启 Emacs……
>
> 这个例子表明 Emacs 不仅能做 Vim 能做的任何事儿，而且甚至能做得更好。
>
> 事实上，Emacs 的 `evil-mode` 是我用过的最接近 vim 原生的 vim style 实现。


#### <span class="section-num">3.4.2</span> 自定义自己的副作用函数 {#自定义自己的副作用函数}

将光标向左移动一格，你会用 `C-b` 。我现在想写一个函数，让我能一次向前移动三格。

模拟击键 `C-b` 三次吗？不够鲁棒，万一有用户把它绑定到其它功能了怎么办。[^fn:8]

那么我怎么精确定义这个函数呢？

1.  使用 `C-h k C-b` 查询 `C-b` 究竟绑定了什么函数。结果是 `(backward-char)`
2.  试试看，在 `M-x` 里使用 `backward-char` ，发现光标真的回退了一格
3.  顺便在这个帮助文档里还看到了 `(backward-char)` 可以加一个参数用来表示回退几个字符
4.  可以动笔写了：

    ```elisp
    (defun my/backward-3-chars ()   ;; 该函数没有参数
      "Backward 3 chars."           ;; Docstring。以下是函数本体
      (interactive)                 ;; 该函数可被 M-x 调用或绑定快捷键
      (backward-char 3))
    ```
5.  在 `*scratch*` Buffer 里粘贴这一段[^fn:9]，把光标移到最后一个括号的后面，按 `C-x C-e` （ `(eval-last-sexp)` ），你会看到状态栏里出现了一个 `my/backward-3-chars` ，说明 defun 成功了[^fn:10]。
6.  试试在 `M-x` 里调用 `my/backward-3-chars` ，works as expected.
7.  不妨把它绑定到一个快捷键上？

    ```elisp
    (global-set-key (kbd "C-M-b") 'my/backward-3-chars) ;; Ctrl + Alt + b
    ```
8.  把这些代码放到我的配置里，就能每次打开 Emacs 自动生效啦。

[^fn:1]: 其实带了一些 Emacs 预设的默认值。比如没有 `~/.emacs.d/init.el` 文件时 Emacs 也依然能生成一个窗口来。
[^fn:2]: Emacs 28 之后变成 `$XDG_CONFIG_HOME/emacs/init.el` 了，一般是 `~/.config/emacs/init.el`
[^fn:3]: 「不上档次的角度」是「快捷键太多」、「打开速度慢」之类的。事实上，当 Emacs 工作在 `daemon` 模式时，打开 client 只会比冷启动一个同等重量级的 vim 快。当然，「同等重量级」是个伪命题，哈哈（
[^fn:4]: 如果精简太多就会让 Emacs 失去太多功能，最后变成跟 `nano` 差不多的存在……
[^fn:5]: 也有 CLI 模式： `emacs -nw`
[^fn:6]: neovim 在 C/S 模式上做了不少努力，その努力を認めよう。
[^fn:7]: 具体参考 `emacsclient --help` 和 `C-h i m Emacs server` 。简单地说，启动服务器是 `emacs --daemon` ，启动客户端是 `emacsclient`
[^fn:8]: 虽然不太可能，但因为 Emacs 什么都能做，所以如果真的有人这么干了，也请不要奇怪： Because he can.
[^fn:9]: 如果你用 Emacs 打开 org 格式的本文的话，你可以直接把光标放在 `BEGIN_SRC` 和 `END_SRC` 内按 `C-c C-c` ，这段会自动执行，并将结果追加到这个代码块后面。
[^fn:10]: 状态栏里的显示是 `(defun)` 函数的求值结果：一个名叫 `my/backward-3-chars` 的 Symbol。如果你在 `(+ 1 2)` 的后面按 `C-x C-e` ，你会看到求值结果 `3` 。
