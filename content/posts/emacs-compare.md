+++
title = "Emacs 自力求生指南 ── 外一则：与编辑器的对比"
author = ["Nyk Ma"]
date = 2020-03-03T03:18:00+08:00
lastmod = 2021-03-22T17:52:17+08:00
tags = ["emacs"]
categories = ["tutorial"]
draft = false
+++

定位的不同会让结果天差地别。为什么说 Emacs 不是编辑器，因为它的定位不是编辑器。

说说“配置文件”和“插件”这两个编辑器常见的概念吧。

比如 VSCode ，你的配置文件是个 JSON。编辑器启动时读取这个 JSON，
switch case 每个字段里的内容，然后改变自己（和插件）的行为，对吧？如果你写了个编辑器认不得的字段，会被忽略；如果你对某字段给了一个
invalid 的值，启动时会报错，之类的。

这思路对 95% 的场景足够用，但是一旦你踢到了 5% 的那个桌脚，就会非常非常疼：这套系统太“死”了。

“死”体现在很多方面，比如：

-   不能运行时动态决定一个配置

    我想两台电脑共享一个配置。有个插件调用了一个二进制，二进制的路径需要写在配置里 ，但这两台电脑里的路径不同，怎么办…

    > 如果配置里能用 `os.find_executable("my_binary")` 之类的就好了…

-   不能自定义行为

    你配置文件里 valid 的字段和值，都是编辑器（和插件）提供好了的，跳不出这个框。

    要改一个插件没暴露到配置文件里的行为？请 fork 它。[我朋友](https://twitter.com/xream)不知道 fork 了多少个
    Atom 的插件了…

-   不能 pick 一个插件提供的功能

    有个插件提供了 ABC 三个功能，我只想要 A 和 C ，因为 B 没有另一个插件做得好。

    还是只能 fork 么……

-   甚至连插件也不是自由的

    我作为一个插件作者，明明我插件和 VSCode 的主业务都是 JS，偏偏就有他们能用我不能用的 API，还得去提 feature request 等 API 开放（或者根本等不到）…

---

以上痛点归根结底是一个问题：“编辑器”、“插件”和“配置”太泾渭分明了。编辑器对插件藏着掖着一点 API ，是为第一道防线；插件对用户又藏着掖着一点接口，是为第二道防线。

{{< admonition note "代码编辑器和其它软件产品是不一样的！" >}}
编辑器作者、插件作者和用户都是程序员，大家都是朋友，为什么要互相使绊子呢？为什么还要用做产品的思路做代码编辑器呢？为什么编辑器要用配置文件这种有限暴露自己接口的方式来“防”着自己的用户？为什么编辑器要用 API 这种有限暴露自己接口的方式来“防”着插件作者？如果你踢过上文的桌脚，你会理解我在说什么。
{{< /admonition >}}

那怎么解决呢？几十年前就解决了，叫软件工程：你把上文的“插件”换成“依赖”，“配置”换成“程序”，一切都不是问题了。

在 Emacs 里，你安装的不是低于编辑器一等的“插件”，而是你的项目所必需的“依赖”；你写的不是低人一等的“配置”，是和其它“依赖”（甚至大多数
Emacs 内部代码）地位齐平的“程序”。

所有 Elisp 代码都运行在同一个运行时里，不分出处，互相完全透明。有人说 Emacs 是一个能“自己修改自己”的编辑器。

{{< admonition note "Emacs 怎么处理这些问题的？" >}}
-   运行时动态生成值

    你写的“配置”实际上为 Elisp 程序，比如

    ```elisp
    ;; ~/.emacs.d/init.el
    ;; setq: 设置全局变量的值
    (setq lsp-java-java-path
      (executable-find "java"))
    ```

-   自定义行为

    Elisp 可以给任意函数“包”一个洋葱 proxy ，这个功能叫 advice，它可以让你在不拆开或者重定义原函数的情况下修改一个函数的行为，而原函数完全无感。举个例子：

    ```elisp
    (defun nema-add-number (a b)
      "把 A 和 B 相加，很简单的函数"
      (+ a b))

    ;; 函数名无所谓，它比原函数多一个参数 orig-fn 表示原函数
    (defun advice@nema-add-number (orig-fn a b)
      (let* ((result (funcall orig-fn (+ 1 a) (* 2 b)))) ;; 修改传入原始函数的参数再 call
        (message (format "A, B, result: %i, %i, %i" a b result))
        result))

    ;; 把新函数包在原函数外面
    (advice-add 'nema-add-number :around 'advice@nema-add-number)

    ;; 现在 call 原函数会让 advice 生效

    (nema-add-number 1 2)
    ;; 上面的调用返回变成了 6
    ;; 并且 *Messages* 里多了 log ： A, B, result: 1, 2, 6
    ```

-   pick 一个插件提供的功能

    `(require)` 一个插件一般只是引入一批函数，要想用它改变环境还得调用它。比如 [elixir-mode](https://github.com/elixir-editors/emacs-elixir) 提供了一个格式化代码的方法
    `(elixir-format)` ，但它何时被调用完全取决于你。官方有一个
    sample ，每次保存文件时都 format：

    ```elisp
    ;; ~/.emacs.d/init.el
    ;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
    ;; 用 C-h f 和 C-h v 查看各个函数及变量的意义
    (add-hook 'elixir-mode-hook
              (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
    ```

    在此基础上，如果精细点，只针对以 `.model.ex` 结尾的文件开启自动
     format 呢？

    ```elisp
    ;; ~/.emacs.d/init.el
    ;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
    (add-hook 'elixir-mode-hook
              (lambda ()
                (if (string-match "\\.model\\.ex$" (buffer-name))
                    (add-hook 'before-save-hook 'elixir-format nil t))))
    ```

    或者不用任何自动流程，我偶尔手动调用一下就够了： `M-x elixir-format RET`

-   三方平等

    所有 Elisp 代码都是平等的。Emacs 主要代码 [80% 是 Elisp](https://github.com/emacs-mirror/emacs)&nbsp;[^fn:1]，插件和你的配置也都是 Elisp。所有 Elisp 在同一个运行时下，互相完全透明[^fn:2]。你可以任意组合 Emacs 自带函数、插件 A 的函数、插件 B 的函数和你自己写的函数。
{{< /admonition >}}

呼，铺垫完了，终于可以进入正题了。

等等，似乎也没几个编辑器值得提一提了？


## <span class="section-num">1</span> Sublime Text {#sublime-text}

我先说它，因为我觉得这才是最接近 Emacs 思想的一个编辑器。它能现场跑一段 python 来修改自己以及调用系统 API……唯一可惜的是它现在用户少了、它的历史不够长（导致没有沉淀出好插件）、以及闭源……

其实和闭源相比，收费不算是个事儿的。闭源意味着你不能从心所欲不逾矩地写插件，只能 API 文档喂啥你吃啥……


## <span class="section-num">2</span> Vim {#vim}

整个上文其实是用来说明 Vim 和 Emacs 的“定位不同”的。

Vimscript 一开始根本不能算是一个 script ，只是一种配置文件 DSL。要不是 Vim 用户对定制化的需求日益强烈，这配置文件格式根本不需要“进化”成为现在这种四不像“语言”。上一个硬着头皮“进化”的例子是 php。

我就说一个，Vimscript 直到 08 年的 7.2 版才支持浮点数。浮点数连语言 feature 都算不上吧…？

所以 `.vimrc` 不要超过 20 行。超过了说明你要换工具。


## <span class="section-num">3</span> VSCode {#vscode}

就是那两道防线的体现。用户是用户，插件开发者是插件开发者，编辑器作者是编辑器作者，三方都放不开，三方都不舒服…

Atom 并没有并 VSCode 更加开放，性能还比 Code 差，直接抬出场外。


## <span class="section-num">4</span> Studio / XCode / VS {#studio-xcode-vs}

这个甚至不是编辑器的比拼，拼的是工具链。UI Designer / instrument /
可视化 debugger 之类的，没得选，反而不用纠结。

[^fn:1]: C 和 elisp 的比例大概是 1:4
[^fn:2]: Elisp 没有 private / public 一说。只要不是在 `(let)` 闭包里定义的东西，都是 public 的。插件里的“内部函数”你也可以直接调用或 advice，只要你清楚你在做什么…
