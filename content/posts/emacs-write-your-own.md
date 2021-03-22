+++
title = "Emacs 自力求生指南 ── 来写自己的配置吧"
author = ["Nyk Ma"]
date = 2020-10-03T15:41:00+08:00
lastmod = 2021-03-22T17:52:19+08:00
tags = ["emacs"]
categories = ["tutorial"]
draft = false
+++

## <span class="section-num">1</span> 我要不要使用别人的配置 {#我要不要使用别人的配置}

现在 Emacs 社区已经有了相当多的「明星配置包」，比如 [Spacemacs](https://github.com/syl20bnr/spacemacs)、
[Purcell](https://github.com/purcell/emacs.d)、[Doom Emacs](https://github.com/hlissner/doom-emacs) 等。

乍一看这些框架都十分全面且易用，但一旦你想改动点什么（which 在
Emacs 使用期间是不可能没有的），就会变得很痛苦。因为你既要知道「这个软件包在原生 Emacs 里如何引入和配置」，又得熟悉「框架对原生
Emacs 做了什么二次封装」。

一般的软件包会提供前者，所以你要自己学「如何定制你的框架」。现在阅读本文的你很可能 Lisp 底子还不够扎实，而 Lisp 又是一个可以把 DSL
玩得很花的语言（那些框架都是这么干的），所以你会很快迷失自我……

> 一个软件包，一行 `(require 'xxx)` 加一行 `(xxx-mode 1)` 就能配置完成的事儿，你可以去看看 Spacemacs 里要[怎么给它写 Layer](https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org) ……
>
> …反正我不写 σ\`∀′) ﾟ∀ﾟ)σ
>
> 更不用说 debug 了，用了框架之后，不仅要对 Emacs 做 debug，说不定还要对框架做 debug ……

<!--quoteend-->

> 从这个角度看，这些配置包和 Emacs 的关系有点像「Linux 发行版之于
> Linux 内核」的关系。
>
> 不过好在从零配置 Emacs 比从零编译 Linux 简单得多……


## <span class="section-num">2</span> 项目结构 {#项目结构}

如前文所说，Emacs 的「配置」实际上是一个软件 Project，它的「主文件」是

1.  `~/.emacs` （一个 Emacs lisp 文本文件），或者
2.  `~/.emacs.d/init.el` ，或者
3.  `$XDG_CONFIG_HOME/emacs/init.el` （仅 Emacs 28+）

一般选择方案二：把所有文件都放在 `~/.emacs.d` 里，并把它作为一个 git repo。


### <span class="section-num">2.1</span> 引用子文件 {#引用子文件}

在堆积配置的过程中，你的 `init.el` 会急剧膨胀，不利于模块化管理。分出子文件是迟早的。初期阶段你只需要知道两种引入方法就够了。至于如何分，你可以在使用期间慢慢烦恼，毕竟 Emacs 用户最大的乐趣就是重构自己的配置文件……


#### <span class="section-num">2.1.1</span> `(load "subfile.el")` {#load-subfile-dot-el}

执行一遍目标文件。考虑以下情况

```elisp
;; ~/.emacs.d/init.el
(load "subfile.el")
(load "subfile.el")
(load "subfile.el")

;; ~/.emacs.d/subfile.el
(message "subfile.el loaded.")
```

此时 `*Messages*` buffer 里会打印三次 `subfile.el loaded.` ，也就是该文件被执行了三遍。

这是本方法的特性：可以重复执行同一个文件。


#### <span class="section-num">2.1.2</span> `(provide)` 和 `(require)` {#provide--和--require}

这个就比较类似 `#ifndef ... #define ... #endif` 了：

```elisp
;; ~/.emacs.d/init.el
(add-to-list 'load-path "~/.emacs.d/my")
(require 'subfile)
(require 'subfile)
(require 'subfile)

;; ~/.emacs.d/my/subfile.el
(message "subfile.el loaded.")
(provide 'subfile)
```

此时 `*Messages*` 只会打印一次。

Emacs 的 `load-path` 变量保存了一个文件夹 List，当有一个 Symbol
被 `(require)` 时， Emacs 会在这些 `load-path` 里寻找是否有文件
`(provide)` 了这个 symbol。因此，上例中我们需要把自己的文件夹
`~/.emacs.d/my` 也加入这个变量。

几乎所有的软件包都会把自己 `(provide)` 出来。


### <span class="section-num">2.2</span> 安装软件包 {#安装软件包}


#### <span class="section-num">2.2.1</span> 手动安装软件包 {#手动安装软件包}

这是最 old school 、最万金油、魔法最少的方法。

```sh
cd ~/.emacs.d
# 既然 ~/.emacs.d 是一个 git repo，这里你也可以用 submodule 的方式来管理它
git clone https://github.com/joaotavora/sly.git
```

```elisp
;; ~/.emacs.d/init.el
;; 把这个软件包加入查找文件夹
(add-to-list 'load-path "~/.emacs.d/sly")
;; 这样就能 require 它了
(require 'sly-autoloads)
;; 此时可以说是「安装」成功了。接下来可以额外配置这个软件包了：
(setq inferior-lisp-program "/opt/sbcl/bin/sbcl")
```


#### <span class="section-num">2.2.2</span> `package.el` ── 下载、安装、 `load-path` 管理 {#package-dot-el-下载-安装-load-path-管理}

2012年的 Emacs 24 引入了 `package.el` 软件包管理器和 ELPA 软件源，使得 Emacs 的软件包管理如同 `apt-get` 一样简单。

```elisp
;; ~/.emacs.d/init.el
;; 这两段一定要在 init.el 的最上方
(require 'package)
;; 初始化包管理器
(package-initialize)

;; 设置软件源
;; 默认软件源里只有 ELPA，也就是 GNU Emacs 官方的软件源
;; 我们引入以下几个最常用的软件源：

;; MELPA：软件包比 ELPA 多（软件进入 MELPA 比 ELPA 手续更简单）、新
;; （nightly 级别的更新速度，以时间作为版本号）
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; 稳定版 MELPA （非 nightly，有版本号）
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;; org-mode 专用软件源。它几乎只服务于 org-plus-contrib 这一个包
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)


;; 可以使用了。我们先更新本地缓存，相当于 apt-get update
(package-refresh-contents)
;; 然后安装一个软件包
;; 会被安装在 ~/.emacs.d/elpa 下
(package-install 'better-defaults)
;; 你不用关心 'load-path 怎么改。这里你可以直接 require 它：
(require 'better-defaults)
```

上面这个例子很糙，比如

-   每次打开 Emacs 都会触发一次 `package-refresh-contents` ，偏偏这个动作又是阻塞的，所以在网络请求完成前啥都不能做……
-   `(package-install)` 会爆出很多诸如 `‘better-defaults’ is already
            installed` 之类的信息在 `*Messages*` 里，所以需要判断该包是不是已安装
-   `(require)` 还是没省掉


#### <span class="section-num">2.2.3</span> 一站式解决： `use-package` 宏 {#一站式解决-use-package-宏}

[use-package](https://github.com/jwiegley/use-package) 可以一举解决以上所有问题，同时能让软件包的配置更加有组织，强烈推荐使用。

```elisp
;; ~/.emacs.d/init.el
;; 以下用来 bootstrap use-package 自己。在上文设置好软件源后，

;; 如果 use-package 没安装
(unless (package-installed-p 'use-package)
  ;; 更新本地缓存
  (package-refresh-contents)
  ;; 之后安装它。use-package 应该是你配置中唯一一个需要这样安装的包。
  (package-install 'use-package))

(require 'use-package)
;; 让 use-package 永远按需安装软件包
(setq use-package-always-ensure t)

;; 之后就可以使用它了。
;; 比如上文安装并 require better-defaults 的过程就可以简化为这一行：
(use-package better-defaults)
;; 1. 它会判断是否已安装。没有时才会更新 package 缓存并安装它
;; 2. 它会自动 (require)
;; 3. 它有很多配置项能让你控制每个环节，从而做到把和这个软件包有关的所
;; 有配置写在一个闭包里。你可以去看它的文档，或者抄我下面的用例
```

> `use-package` 也是有不少对位替代品的，但在使用 Emacs 的前五年里你不用关心这个……

{{< admonition note "安装单文件 / git clone软件包" >}}
有时一些软件包以单个文件提供功能（比如 [Dired+](https://www.emacswiki.org/emacs/download/dired+.el)），或者作者还没来得及弄 MELPA 的发布流程（比如 [mix.el](https://github.com/ayrat555/mix.el)）。此时，为了能继续享用
`use-package` 给我们带来的便利，我们要给它提供一个额外的下载器：
`quelpa` 。

```elisp
;; ~/.emacs.d/init.el
;; quelpa - For those packages which are not in MELPA
(use-package quelpa
  :config ; 在 (require) 之后需要执行的表达式
  (use-package quelpa-use-package) ; 把 quelpa 嵌入 use-package 的宏扩展
  (quelpa-use-package-activate-advice)) ; 启用这个 advice

;; 直接 HTTP get 一个 elisp
(use-package dired+
  :quelpa (dired+ :fetcher url :url "https://www.emacswiki.org/emacs/download/dired+.el"))

;; git clone 一个 GitHub repo
(use-package elixir-mode
  :quelpa (elixir-mode :fetcher github :repo "elixir-editors/emacs-elixir"))

;; 只使用 repo 中的某些文件
(use-package mix
  :quelpa (mix.el :fetcher github :repo "ayrat555/mix.el" :files ("mix.el" "LICENSE"))
  :hook ((elixir-mode . mix-minor-mode)))
```
{{< /admonition >}}


#### <span class="section-num">2.2.4</span> `custom.el` {#custom-dot-el}

有一些配置是不需要跟着 `.emacs.d` 这个 git repo 走的，比如我笔记本和台式机用了同一套软件包，但有两三个配置变量不同。

此时 Emacs 自带的 `customize.el` 所提供的「临时配置修改、固化、还原、可视化」功能就十分有用。

它默认把本地配置 append 在 `init.el` 的末尾，对 `.emacs.d` 这个
repo 是个很大的干扰，所以我们把固化配置分出去，单列一个文件：

```elisp
;; ~/.emacs.d/init.el
;; 最好紧跟在 packages 初始化之后

;;; move customize-set-variable out of init.el
(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)  ;; 如果该文件不存在
  (write-region "" nil custom-file)) ;; 写入一个空内容，相当于 touch 一下它
(load custom-file)
```

然后你可以把 `custom.el` 给 `.gitignore` 掉。

接下来可以随意使用 `M-x customize-variable RET` 了。


## <span class="section-num">3</span> 常见需求及对应软件包 {#常见需求及对应软件包}

下面列出的几个常见场景，Emacs 都有至少 3 个软件包能满足要求。我给出的不一定适合你。


### <span class="section-num">3.1</span> Emacs 命令补全引擎 {#emacs-命令补全引擎}

Minibuffer 的使用贯穿 emacs 始终。所以增强 Minibuffer 功能就显得尤为重要。目前有两大阵营打得最火热：


#### <span class="section-num">3.1.1</span> Helm {#helm}

[Helm](https://github.com/emacs-helm/helm) 符合你对「强大」的一切想象。

摒弃 Minibuffer 而使用一个新 buffer 显示丰富的内容。

我正在用。挑不出毛病。

```elisp
(use-package helm
  ;; 等价于 (bind-key "M-x" #'helm-M-x)
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files))
  :config
  ;; 全局启用 Helm minor mode
  (helm-mode 1))

;; 好了。按个 M-x 试试
```

> 虽然 Helm 最近（<span class="timestamp-wrapper"><span class="timestamp">&lt;2020-09-11 Fri&gt;</span></span>）突然被 Archive 了，但我依然对它保持乐观：一方面，围绕 Helm 所建设的工具链生态已经相当靠谱稳定，哪怕 Helm 不更新个三四年也能一样用；另一方面，一定会有人接手
> Fork 的。不慌。


#### <span class="section-num">3.1.2</span> Ivy {#ivy}

[Ivy](https://github.com/abo-abo/swiper) 是 Helm 的实力强劲的竞争对手，注重「短平快」：停留短、内容平、速度快。适合习惯快速精准处理信息的用户。


### <span class="section-num">3.2</span> 文本补全引擎 {#文本补全引擎}

这个似乎没得挑： [company-mode](http://company-mode.github.io/)，支持多 backend。

```elisp
(use-package company
  ;; 等价于 (add-hook 'after-init-hook #'global-company-mode)
  :hook (after-init . global-company-mode)
  :config
  ;; setq 可以像这样连着设置多个变量的值
  (setq company-tooltip-align-annotations t ; 注释贴右侧对齐
        company-tooltip-limit 20            ; 菜单里可选项数量
        company-show-numbers t              ; 显示编号（然后可以用 M-数字 快速选定某一项）
        company-idle-delay .2               ; 延时多少秒后弹出
        company-minimum-prefix-length 1     ; 至少几个字符后开始补全
        ))
```

这时开始，你编写 elisp 文件时应该会减少一点痛苦了……


### <span class="section-num">3.3</span> 错误 / 警告提示引擎 {#错误-警告提示引擎}

目前有两个选择：


#### <span class="section-num">3.3.1</span> `flymake` {#flymake}

这是 Emacs 自带的 minor mode ， `(flymake-mode 1)` 就可以 enable
它。优点和缺点都是功能少。


#### <span class="section-num">3.3.2</span> `flycheck` {#flycheck}

[flycheck](https://www.flycheck.org/en/latest/) 是一个更「现代」的引擎，功能更多，呈现样式更丰富，同时和 `lsp-mode` 结合最好。

```elisp
(use-package flycheck
  :init ;; 在 (require) 之前需要执行的
  (setq flycheck-emacs-lisp-load-path 'inherit)
  :config
  (global-flycheck-mode))
```


### <span class="section-num">3.4</span> 项目管理 {#项目管理}

这个似乎没得挑： [Projectile](https://github.com/bbatsov/projectile) 可以满足对软件项目管理的所有要求：项目列表、项目内文件跳转、 `ag` 、 `make` 等等

```elisp
;; ~/.emacs.d/init.el
(use-package projectile
  :config
  ;; 把它的缓存挪到 ~/.emacs.d/.cache/ 文件夹下，让 gitignore 好做
  (setq projectile-cache-file (expand-file-name ".cache/projectile.cache" user-emacs-directory))
  ;; 全局 enable 这个 minor mode
  (projectile-mode 1)
  ;; 定义和它有关的功能的 leader key
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map))

(use-package helm-projectile
  :if (functionp 'helm) ;; 如果使用了 helm 的话，让 projectile 的选项菜单使用 Helm 呈现
  :config
  (helm-projectile-on))
```

以下是基于此配置，我常用的快捷键：

| 快捷键        | 功能             | 场景                                                                  |
|------------|----------------|---------------------------------------------------------------------|
| `C-c C-p p`   | 在历史项目中切换 | 打开 `~/Projects/elixir/ex_faxtu` 项目的文件列表： `C-c C-p p pro ex fa RET` |
| `C-c C-p f`   | 查找当前项目的文件 | 在上述项目里时， `C-c C-p f test conn RET` 打开 `test/support/conn_case.ex` 文件 |
| `C-c C-p s s` | 用 `ag` 查找项目文件的内容 | 查找所有 `User` 的出现位置： `C-c C-p s s User` 。Helm 可以再搭配 `C-x C-s` 把搜索结果保存起来 |


### <span class="section-num">3.5</span> 版本管理 （git 客户端） {#版本管理-git-客户端}

这里我要吹爆 [Magit](https://magit.vc/)，地表最强 git 客户端，没有之一。

看看[这个视频](https://www.youtube.com/watch?v=rzQEIRRJ2T0)就够了，颠覆你对 git 客户端的一切成见，同时兼顾功能全面、组合自由和 UX 可用性，独一份。

```elisp
;; ~/.emacs.d/init.el
(use-package magit)

;; 没错，好了。
```

我经常使用的工作流：

| 快捷键                    | 功能                                                                 |
|------------------------|--------------------------------------------------------------------|
| `C-c C-p v`               | 让 Projectile 呼出本项目的 magit。当然你也可以用 `M-x magit` 呼出    |
| `s`                       | 当光标停在一个 untracked file 上时， `stage` 这个文件；当停在一个 diff 区块时， `stage` 这个区块 |
| `c c`                     | git commit                                                           |
| `c a`                     | git `commit --amend` （超高频，救命用）                              |
| `f p`                     | git fetch origin                                                     |
| `F p`                     | git pull                                                             |
| `P p`                     | git push                                                             |
| `l b`                     | git log 所有分支                                                     |
| `A`                       | 当光标停在一个 commit 上时， Cherry Pick 它                          |
| `t t`                     | git tag                                                              |
| `Z z`                     | git stash                                                            |
| `Z p`                     | 光标停在一个 stash 上时，git stash pop 它                            |
| `M-x magit-file-dispatch` | 当前文件「时光机」                                                   |
| `M-x magit-blame`         | 顾名思义， `git blame` 当前文件                                      |

可以用 `?` 呼出帮助菜单，告诉你有哪些 Leader key 可以用。
Leader key 敲完后停一会儿也会弹出帮助菜单。


### <span class="section-num">3.6</span> 程序语言服务器 （LSP） {#程序语言服务器-lsp}

微软完成了编辑器行业的大一统，LSP 目前已成为各大语言、各大编辑器的首选通信协议，免去一大堆开发语言支持的麻烦。

目前 Emacs 流行的 LSP 客户端有两个：


#### <span class="section-num">3.6.1</span> `lsp-mode` {#lsp-mode}

[lsp-mode](https://emacs-lsp.github.io/lsp-mode/) 是 Emacs 第一个，也是目前功能最全面的一个 LSP 客户端。它可以把上游语言服务器的结果对接到 [flycheck](#flycheck)、[Company](#文本补全引擎)、[yasnippet](#template)、
[treemacs](https://github.com/Alexander-Miller/treemacs)里。再搭配上 `lsp-ui` 可以实现[相当丰富的内容呈现和互动](https://emacs-lsp.github.io/lsp-mode/page/gallery/)。

启用很简单：在你想用的语言 major mode 里运行 `(lsp)` 即可。

```elisp
(use-package lsp-mode
  ;; 延时加载：仅当 (lsp) 函数被调用时再 (require)
  :commands (lsp)
  ;; 在哪些语言 major mode 下启用 LSP
  :hook (((ruby-mode
           php-mode
           typescript-mode
           ;; ......
           ) . lsp))
  :init ;; 在 (reuqire) 之前执行
  (setq lsp-auto-configure t ;; 尝试自动配置自己
        lsp-auto-guess-root t ;; 尝试自动猜测项目根文件夹
        lsp-idle-delay 0.500 ;; 多少时间idle后向服务器刷新信息
        lsp-session-file "~/.emacs/.cache/lsp-sessions") ;; 给缓存文件换一个位置
  )

;; 内容呈现
(use-package lsp-ui
  ;; 仅在某软件包被加载后再加载
  :after (lsp-mode)
  ;; 延时加载
  :commands (lsp-ui-mode)
  :bind
  (:map lsp-ui-mode-map
        ;; 查询符号定义：使用 LSP 来查询。通常是 M-.
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        ;; 查询符号引用：使用 LSP 来查询。通常是 M-?
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ;; 该文件里的符号列表：类、方法、变量等。前提是语言服务支持本功能。
        ("C-c u" . lsp-ui-imenu))
  ;; 当 lsp 被激活时自动激活 lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :init
  ;; lsp-ui 有相当细致的功能开关。具体参考：
  ;; https://github.com/emacs-lsp/lsp-mode/blob/master/docs/tutorials/how-to-turn-off.md
  (setq lsp-enable-symbol-highlighting t
        lsp-ui-doc-enable t
        lsp-lens-enable t))
```


#### <span class="section-num">3.6.2</span> `eglot` {#eglot}

[eglot](https://github.com/joaotavora/eglot) 是一个注重配置简单、一体化强的客户端。

由于 `lsp-mode` 的一次大更新，把使用方式变成如今调用 `(lsp)` 即可，所以目前 `eglot` 竞争力不强了。


### <span class="section-num">3.7</span> Template {#template}

应该就一个： [yasnippet](https://github.com/joaotavora/yasnippet)。提供非常灵活的动态模板功能（因为模板里可以有 elisp）。

```elisp
(use-package yasnippet
  :config
  ;; 全局启用这个 minor mode
  (yas-global-mode 1))

;; 再装一个通用模板库，省得没 template 用
(use-package yasnippet-snippets
  :after (yasnippet))

;; 模板生成工具，写代码时随手生成一个模板。强烈推荐使用
;; 使用方法： https://github.com/abo-abo/auto-yasnippet#usage
(use-package auto-yasnippet
  :bind
  (("C-c & w" . aya-create)
   ("C-c & y" . aya-expand))
  :config
  (setq aya-persist-snippets-dir (concat user-emacs-directory "my/snippets")))
```


### <span class="section-num">3.8</span> 搜索、跳转和替换 {#搜索-跳转和替换}

这块可能是编辑器最高频调用的功能了，所以 Emacs 这类软件包是遍地开花，保证有一款合你心意。

我会写上我的选择理由（甚至没有理由……），你如果想自己发掘，可以去
[/r/emacs](https://www.reddit.com/r/emacs) 或者 [EmacsChina](https://emacs-china.org/) 之类的社区搜一搜问一问。


#### <span class="section-num">3.8.1</span> 项目内搜索： `helm-ag` {#项目内搜索-helm-ag}

在「[万物皆文本]({{< relref "emacs-intro#万物皆文本" >}})」一章里你已经见识过它了。

```lisp
(use-package helm-ag)
```

然后 `(projectile-ag)` （默认 `C-c C-p s s` ） 会自动调用它。

当然， `ivy` 社区也有对应的工具，一搜就能搜到。


#### <span class="section-num">3.8.2</span> buffer 内搜索： `ctrlf` 和 `helm-swoop` {#buffer-内搜索-ctrlf-和-helm-swoop}

Emacs 自带一个快速关键词搜索： `(search-forward)` （默认 `C-s`&nbsp;[^fn:1]），但功能太简陋了。

`ctrlf` 的出现恰好填补了这个需求的空白：它提供了很多高频常用信息（比如总结果数）和预设功能（比如正则），同时保持原版的快速和非侵占性。

{{< figure src="/ox-hugo/Snipaste_2021-03-11_03-04-56.png" >}}

```elisp
(use-package ctrlf
  :config
  (ctrlf-mode t))
;; 此时 C-s 已经被替换成 ctrlf 版本的了
```

而 `helm-swoop` 则会弹出一个新 buffer，将关键词匹配的所有行呈现出来，你可以用光标上下挑一个。

```elisp
(use-package helm-swoop
  ;; 更多关于它的配置方法: https://github.com/ShingoFukuyama/helm-swoop
  ;; 以下我的配置仅供参考
  :bind
  (("M-i" . helm-swoop)
   ("M-I" . helm-swoop-back-to-last-point)
   ("C-c M-i" . helm-multi-swoop)
   ("C-x M-i" . helm-multi-swoop-all)
   :map isearch-mode-map
   ("M-i" . helm-swoop-from-isearch)
   :map helm-swoop-map
   ("M-i" . helm-multi-swoop-all-from-helm-swoop)
   ("M-m" . helm-multi-swoop-current-mode-from-helm-swoop))
  :config
  ;; 它像 helm-ag 一样，可以直接修改搜索结果 buffer 里的内容并 apply
  (setq helm-multi-swoop-edit-save t)
  ;; 如何给它新开分割窗口
  ;; If this value is t, split window inside the current window
  (setq helm-swoop-split-with-multiple-windows t))
```


#### <span class="section-num">3.8.3</span> 可视范围内跳转： `avy` {#可视范围内跳转-avy}

一种更高频的跳转：你眼睛盯着屏幕上的一个关键词，要把光标跳到那儿去编辑。

{{< figure src="/ox-hugo/Snipaste_2021-03-11_03-19-33.png" >}}

本图里我调用了 `(avy-goto-char-timer)` 并快速键入跳转词 `helm`
，0.3s 后所有可视范围内的 `helm` 都被标注上了编号，我只要继续敲入一个编号 `f` 就可以把光标跳转至那儿。

> 同时原光标位置会成为 `mark` 。请参见「[基本概念]({{< relref "emacs-terms#point-mark-和-region" >}})」一章。

<!--quoteend-->

> 这个命令可以搜索所有「可视范围」，不仅是光标所在 window，它甚至可以跨 frame 跳转光标焦点。
>
> 试想你有两个显示器，分别开了两个 `emacsclient` 。
>
> 有了它，你可以一整天不摸鼠标。

```elisp
(use-package avy
  :bind (("C-'" . avy-goto-char-timer) ;; Control + 单引号
         ;; 复用上一次搜索
         ("C-c C-j" . avy-resume))
  :config
  (setq avy-background t ;; 打关键字时给匹配结果加一个灰背景，更醒目
        avy-all-windows t ;; 搜索所有 window，即所有「可视范围」
        avy-timeout-seconds 0.3)) ;; 「关键字输入完毕」信号的触发时间
```


#### <span class="section-num">3.8.4</span> buffer 内正则替换： `anzu` {#buffer-内正则替换-anzu}

我选它没啥原因，主要是它能「渐进式可视化」 search 正则和 replace
结果。我可以边敲正则边想怎么写……

Emacs 自带的正则替换没有预览，只有按了回车才知道自己打错表达式了……

它已经很久没更新了，但好在一直能用。如果你有其它选择，也欢迎告诉我。

```elisp
(use-package anzu)
;; 我都是手动调用它的，因为使用场景不多，但又不能没有……
;; M-x anzu-query-replace-regexp
```


#### <span class="section-num">3.8.5</span> 多光标： `multiple-cursors` {#多光标-multiple-cursors}

强烈建议看[这个视频](https://www.youtube.com/watch?v=jNa3axo40qM)。三分钟圈粉。

> 每个光标会有 **自己的 kill-ring** ，这使得它在 `dired` 里极其好用。

```elisp
(use-package multiple-cursors
  :bind (("C-S-c" . mc/edit-lines) ;; 每行一个光标
         ("C->" . mc/mark-next-like-this-symbol) ;; 全选光标所在单词并在下一个单词增加一个光标。通常用来启动一个流程
         ("C-M->" . mc/skip-to-next-like-this) ;; 跳过当前单词并跳到下一个单词，和上面在同一个流程里。
         ("C-<" . mc/mark-previous-like-this-symbol) ;; 同样是开启一个多光标流程，但是是「向上找」而不是向下找。
         ("C-M-<" . mc/skip-to-previous-like-this) ;; 跳过当前单词并跳到上一个单词，和上面在同一个流程里。
         ("C-c C->" . mc/mark-all-symbols-like-this))) ;; 直接多选本 buffer 所有这个单词
```


### <span class="section-num">3.9</span> Terminal {#terminal}

目前有两种常用的。


#### <span class="section-num">3.9.1</span> 真 Terminal： `emacs-libvterm` {#真-terminal-emacs-libvterm}

<https://github.com/akermu/emacs-libvterm>

\*nix 用户请直接安装它，它就是你心目中的 terminal，没有任何不使用它的理由。该有的富文本装饰、全屏 TUI 它都支持，不会排版错乱，性能也 OK。

因为它在 Emacs 世界里只是一个能互动的文字 buffer，所以上面那些搜索跳转工具你可以直接拿来用，效率不是其它 GUI terminal emulator
能比的。

唯一一个缺点，Windows 不能用。

```elisp
(use-package vterm
  ;; https://github.com/akermu/emacs-libvterm
  ;; 请务必参照项目 README 作配置，以下不是我的完整配置。
  ;; 比如，如果你要和 shell 双向互动（对，它可以双向互动），
  ;; 那么 shell 需要做一点配置以解析 vterm 传递过来的信号
  :config
  (setq vterm-kill-buffer-on-exit t)) ;; shell 退出时 kill 掉这个 buffer
;; 使用 M-x vterm 新建一个 terminal
;; 在 terminal 中使用 C-c C-t 进入「选择」模式（类似 Tmux 里的 C-b [ ）
```


#### <span class="section-num">3.9.2</span> 模拟 terminal: `eshell` {#模拟-terminal-eshell}

这是 Emacs 自带的，使用纯 elisp 实现的，与系统无关的 shell。

主要好处是两个：

-   Windows 也能用，而且用法和 \*nix 系统一样
-   能直接执行 elisp 表达式

可以用 `M-x eshell` 体验一下。


### <span class="section-num">3.10</span> 窗口、工作空间管理 {#窗口-工作空间管理}

虽然 Emacs 有默认的 [分屏逻辑]({{< relref "emacs-terms#buffer-window-和-frame" >}})，但这不代表我们不能自定义了。


#### <span class="section-num">3.10.1</span> edwina {#edwina}

[edwina](https://github.com/ajgrf/edwina) 是 Emacs 版的「平铺式 WM」。有了它，你 80% 的时间不用操心窗口管理问题。

它把窗口分为「主窗口」和「副窗口」两种，主窗口默认占满左半屏作为你的工作重心，副窗口叠放在右半屏。你可以随时交换主副窗口的位置。

```elisp
(use-package edwina
  :config
  ;; 让所有 display-buffer 动作都新增一个 window （而不是复用已经打开此 buffer 的 window）
  (setq display-buffer-base-action '(display-buffer-below-selected))
  ;; 以下定义会被 (edwina-setup-dwm-keys) 增加 'M-' 修饰。
  ;; 我自定义了一套按键，因为原版会把我很常用的 M-d 覆盖掉。
  (setq edwina-dwm-key-alist
        '(("r" edwina-arrange)
          ("j" edwina-select-next-window)
          ("k" edwina-select-previous-window)
          ("J" edwina-swap-next-window)
          ("K" edwina-swap-previous-window)
          ("h" edwina-dec-mfact)    ;; 主窗口缩窄
          ("l" edwina-inc-mfact)    ;; 主窗口拉宽
          ("D" edwina-dec-nmaster)  ;; 减少主窗口的数量
          ("I" edwina-inc-nmaster)  ;; 增加主窗口的数量
          ("C" edwina-delete-window) ;; 关闭窗口
          ("RET" edwina-zoom t)     ;; 交换「主窗口」和「副窗口」
          ("return" edwina-zoom t)
          ("S-RET" edwina-clone-window t) ;; 复制一个本窗口
          ("S-return" edwina-clone-window t)))
  (edwina-setup-dwm-keys)
  (edwina-mode 1))
```


#### <span class="section-num">3.10.2</span> winum {#winum}

[winum](https://github.com/deb0ch/emacs-winum) 的功能很简单：给每个 window 增加一个数字编号。


#### <span class="section-num">3.10.3</span> eyebrowse {#eyebrowse}

[eyebrowse](https://depp.brause.cc/eyebrowse/) 可以保存和呼出窗口布局。


#### <span class="section-num">3.10.4</span> exwm {#exwm}

这里提到 [exwm](https://github.com/ch11ng/exwm) 其实有点文不对题：它可以让 emacs 变为系统级 window
manager，让你的整个桌面环境享受所有上述 Emacs 工具带来的操作便利性和一致性。搭配下文 [Modal Editing](#modal-editing) 或者本章其它工具有奇效。


### <span class="section-num">3.11</span> 字体 {#字体}

> 显然，本章对 TUI 不 make sense。

Emacs GUI 完整支持 Unicode，对字体定义的精细度极高，颗粒度小到每个 Unicode 码位。正因其自由度太大，实际配置时坑很多。

这里仅介绍常用设置指令和 debug 方法。

> 通过组合以下函数，我[自己写了一套设置字体的流程](https://github.com/nykma/nema/blob/develop/my-sample/font.el) 。


#### <span class="section-num">3.11.1</span> `(font-spec :family "xxx" :height 12)` {#font-spec-family-xxx-height-12}

新建一个字体实例，字体名为 "xxx"，大小为 12 。它不保证这个字体能真正被找到。

具体参见文档 <kbd>C-h f font-spec</kbd>


#### <span class="section-num">3.11.2</span> `(find-font font-spec-instance)` {#find-font-font-spec-instance}

使用 `(font-spec)` 实例的定义查找字体。找到了返回一个
`font-entity` 实例，没找到返回 `nil` 。

具体参见文档 <kbd>C-h f find-font</kbd>

实际例子： Victor Mono

```lisp
(let* ((normal-font (font-spec :family "Victor Mono" :height 12))
       (founded-font (find-font normal-font)))
  (if founded-font
      (message "Font found! %s" founded-font)
    ;; else
    (message "Font not found!")))
```


#### <span class="section-num">3.11.3</span> `(set-fontset-font)` {#set-fontset-font}

使用一个 `(font-spec)` 实例设置某个 Unicode 区段的字体。

参数意义请参考文档 <kbd>C-h f set-fontset-font</kbd>

实际例子：设置 CJK 字符集的字体为 `Sarasa Term Slab SC`

```elisp
(let ((cjk-font (font-spec :family "Sarasa Term Slab SC" :height 12)))
  (if (find-font cjk-font)
      (set-fontset-font t cjk-charset cjk-font nil 'append)
    ;; else
    (message "CJK font not found")))
```


#### <span class="section-num">3.11.4</span> <kbd>C-u C-x =</kbd> {#c-u-c-x}

查看当前光标下的字是什么，以及使用了哪个字体。

{{< figure src="/ox-hugo/2021-03-22_15-50.png" >}}


#### <span class="section-num">3.11.5</span> `(helm-select-xfont)` {#helm-select-xfont}

浏览、搜索所有字体。用 <kbd>C-j</kbd> 可以预览 + 拷贝字体名进 kill ring。

{{< figure src="/ox-hugo/2021-03-22_15-54.png" >}}


#### <span class="section-num">3.11.6</span> `(insert-char)` (<kbd>C-x 8 RET</kbd>) {#insert-char----c-x-8-ret}

从 Unicode 里搜索一个码位并插入 buffer。


### <span class="section-num">3.12</span> 输入法 {#输入法}

Emacs 不仅有输入法，而且做得很好。


#### <span class="section-num">3.12.1</span> 啊？为啥？ {#啊-为啥}

-   用过 vim 的你肯定苦恼过： `NORMAL` 模式下按了 <kbd>jjj</kbd> 发现忘了关输入法
-   输入法不知道你在写代码还是写注释， shift 按到爆炸
-   不同系统下输入法高频词不同，环境不统一，云同步又不放心
-   没装输入法 / 管理员不允许我装输入法


#### <span class="section-num">3.12.2</span> emacs-rime {#emacs-rime}

[emacs-rime](https://github.com/DogLooksGood/emacs-rime) 是 \*nix 系统首选，体验和桌面输入法几乎一致。

优点：

-   C/S 工作模式，速度快
-   如果你用过 Rime，可以直接复用你的配置

缺点：

-   Server 端需要编译
-   需要熟悉 Rime


#### <span class="section-num">3.12.3</span> pyim {#pyim}

[pyim](https://github.com/tumashu/pyim#%E9%85%8D%E7%BD%AE) 是 Emacs 上的老牌中文输入法，纯 Elisp 实现，在无法编译
`emacs-rime` 的场景下十分有用。


### <span class="section-num">3.13</span> 色彩主题 {#色彩主题}

搜索 [emacs theme](https://www.google.com/search?q=emacs%20theme) 并挑一个你喜欢的。

一般来说，引入 Theme 只需要类似以下代码：

```elisp
(use-package srcery-theme
     :config
     (load-theme 'srcery t)
```

有的 theme 会附带其它配置项，请参阅各自的 README 进行配置。

> 这里有一份[我经手过的主题列表](https://github.com/nykma/nema/blob/develop/nema/nema-appearance.el#L72)供你参考。


### <span class="section-num">3.14</span> Mode line {#mode-line}

和主题一样，搜索 [emacs mode line](https://github.com/search?q=emacs+mode+line) 并挑一个你喜欢的。

> 这里有一份[我经手过的 mode line 列表](https://github.com/nykma/nema/blob/develop/nema/nema-appearance.el#L152)供你参考。


### <span class="section-num">3.15</span> Modal Editing {#modal-editing}

你第一个接触的 Modal editor 很可能是 Vim，但 Vim 很可能 ****不是****
最适合你的 modal 方案。

由于我个人不使用 modal，这里就泛泛聊一些我看过的 modal 方案。

> 相信我，你真的不需要 modal。


#### <span class="section-num">3.15.1</span> evil {#evil}

[evil-mode](https://github.com/emacs-evil/evil) 不仅做到了几乎 100% 兼容 Vim，甚至在有些方面能超越 Vim。

优点是有相当多的 emacs 软件包和主题能开箱兼容 `evil` 。

缺点是它太 vim 了。


#### <span class="section-num">3.15.2</span> xah-fly-keys {#xah-fly-keys}

[xah-fly-keys](https://github.com/xahlee/xah-fly-keys) ，程序员四大魔王之一李杀的作品，一个对 Emacs 深度优化后的 vim。


#### <span class="section-num">3.15.3</span> god-mode {#god-mode}

[god-mode](https://github.com/emacsorphanage/god-mode) 的特点是，保留 Emacs 原始按键风味的同时，尽量少按修饰键，减少左手小指的疲劳。


#### <span class="section-num">3.15.4</span> meow {#meow}

[Modular Emacs On Wish](https://github.com/DogLooksGood/meow) 声称其「配置少，集成度优秀，记忆最少键位可以干最多的事」。


#### <span class="section-num">3.15.5</span> modalka {#modalka}

[modalka](https://github.com/mrkkrp/modalka) 只提供 modal 框架，你可以完全设定你自己的 modal 键位，打造专属于你的贴手 modal。


#### <span class="section-num">3.15.6</span> Hydra {#hydra}

[hydra](https://github.com/abo-abo/hydra) 更接近于「功能菜单」：弹出一个「常用功能列表」，你可以用连续击键来连续触发若干个函数。

> 我曾经用它写过一个 [窗口管理工具包](https://github.com/nykma/nema/blob/6a9214f761b611b4722954d5f36875801f3435db/nema/nema-hydra.el) ，但自从用了 [edwina](https://github.com/ajgrf/edwina) 后它就没用了……

[^fn:1]: 向下搜索本 buffer，到 buffer 末尾后会返回开头继续搜。反向动作（向上搜索）是 `(search-backward)` （默认 `C-r` ）
