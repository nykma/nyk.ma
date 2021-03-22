+++
title = "Emacs 自力求生指南 ── 外一则：生态现状"
author = ["Nyk Ma"]
date = 2020-02-11T16:46:00+08:00
lastmod = 2021-03-22T17:52:17+08:00
tags = ["emacs"]
categories = ["tutorial"]
draft = false
+++

[Emacs 101 新手求生指南](https://github.com/emacs-tw/emacs-101-beginner-survival-guide)是我看过的第一个把 emacs 的本质讲清楚的教程，是我入坑最大的勇气来源，也是我写这个系列的初衷。

作者在首页提到了作者已退坑，TA 的选择有自己的时代原因。

我以此为切入，梳理一下 2020 年的今天 Emacs 软件环境的现状。


## <span class="section-num">1</span> 回应 {#回应}

> 跳到 VSCode 三個月後，我的日常工作裡 Emacs 只拿來做寫程式以外的任務（Magit, serial-term 之類的）

同感， [Magit](https://github.com/magit/magit) 过于优秀，作为 git 前端根本无可替代。

> ……就讓我幹譙一下 Emacs，目前在 Emacs 裡面搞過 C++, Python, JavaScript, TypeScript 的自動補全與重構（其實搞過的開發環境不只這幾種語言，但這幾種語言我都有寫了一定的時間比較理解），都很難搞

巧了，这几种我都搞过，无一例外地好搞，因为 LSP 出现了。赞美微软，
LSP 解放了所有编辑器开发者和所有语言开发者，实现了IDE前后端真正的解耦。作者生不逢时，2018 年的 LSP 环境远没有现在这么成熟和开箱即用。

先看配置，[直接摘自我的配置文件](https://github.com/nykma/nema/blob/develop/nema/nema-lsp.el)。这种程度的就足够了：

```elisp
(use-package lsp-mode
:commands (lsp) ;; 懒加载
:hook ((ruby-mode ;; 设置哪些 major mode 里启动 LSP
        php-mode
        ;; python-mode ;; <- handled in nema-python.el
        js-mode typescript-mode js2-mode rjsx-mode
        go-mode
        rust-mode
        ;; c-mode c++-mode objc-mode ;; <- handled in nema-c.el
        ;; swift-mode ;; <- handled in nema-swift.el
        dart-mode
        elixir-mode
        java-mode
        ng2-html ng2-ts
        ;; plain-tex-mode latex-mode ;; <- handled in nema-latex.el
        ) . lsp)
:init
(setq lsp-enable-snippet t    ;;使用 LSP 提供的代码片段
      lsp-auto-guess-root t   ;; 自动猜测项目根目录位置
      lsp-response-timeout 20 ;; 前后端通信的超时时间
      lsp-auto-configure t
      lsp-prefer-flymake nil ;; 我们用 flycheck
      lsp-session-file (expand-file-name ".cache/lsp-sessions" user-emacs-directory) ;; 改个临时文件的保存位置
      )
:config
(use-package lsp-java) ;; Java 的 LSP 是个独立包，额外安装
(require 'lsp-clients)
(dolist (dir '("[/\\\\]builddir$"
               "[/\\\\]\\.elixir_ls$"
               "[/\\\\]_build$"
               "[/\\\\]\\.ccls-cache$"
               "[/\\\\]deps$"))
  (push dir lsp-file-watch-ignored))) ;; 项目根目录的判断依据：找文件夹名
```

然后照着 lsp-mode 的官方文档把[对应语言的 LSP 服务端装起来](https://github.com/emacs-lsp/lsp-mode#supported-languages)就行了，基本上都是一两句话。

再看效果，请看 [lsp-ui](https://github.com/emacs-lsp/lsp-ui) 的截图，是不是已经和 VSCode 差不多了？悬浮文档、引用、跳转、重构、右侧快捷命令、自动补全、模板……应有尽有。

> 而且最終弄出來的效果也都沒有 Qt Creator / PyCharm / VSCode 好

Qt Creator / Xcode / Android Studio 有 UI designer，没啥好比的，
Emacs 是不行。后两个我持保留态度，至少 Emacs 能持平 VSCode，因为
[后端都是微软写的](https://github.com/Microsoft/python-language-server)……

> 就算只是想寫個 web，vue, scss, pug / jade 和 stylus 這幾個需要的 major-mode 我都直接自幹要不然也 hack 過，現有的都超難用要不然就是 bug 滿天飛

哎这里我就要 diss 一下 web 界了，妖怪 DSL 满天飞。

都 2020 年了，谁还用 vue 啊，赶紧换 Angular 啦σ\`∀′) ﾟ∀ﾟ)σ

说到 pug， 这个点挺疼的。Emacs 的 parser 相当弱（低估了未来的语言有多妖），性能也一般，还是同步的。

[tree-sitter](https://tree-sitter.github.io/tree-sitter/) 提供了一套渐进式 parser，不过要把 Emacs自带的 parser
替换成它[还有相当长的一段路要走](https://github.com/ubolonton/emacs-tree-sitter)。如果真的能替换，那么 Emacs 就会真正成为一个语法树编辑器（而不是文本编辑器）。

另外 LSP 还有个姊妹项目叫 [semantic highlighting protocol](https://github.com/microsoft/vscode-languageserver-node/pull/367) ，由 LSP
服务器告诉前端如何高亮代码。也可以期待一下。

> 搞了兩三年發現用 VSCode 隨便滑鼠點一點裝好 plugins 還不用動設定的開發環境都比目前Emacs上各式現有package與自己土炮出來的好用太多

综上，生不逢时……现在编辑 Angular 项目不要太酸爽。

> C/C++ 支援就是沒有 QtCreator 或 VisualStudio 那樣的行雲流水（更別說現在有 clang 語意分析加持的 QtCreator 跟那好用到爆的 UI 搭配）

还是生不逢时。C 项目因为历史遗留原因，per-project 的环境都大不一样。不过鉴于现代项目都用 CMake 了，其实这个配置[也不麻烦](https://github.com/MaskRay/ccls/wiki/Project-Setup#cmake)，只要生成这个
JSON 放在项目根目录里， [ccls](https://github.com/MaskRay/ccls/wiki/Project-Setup#cmake) 就能完美工作了。我的键盘固件、算法题和路由器固件项目都是用 ccls 做 LSP 的，全程如预期。

> 你可能會說這些本來就不是編輯器該做的事情、Emacs 不是 IDE…但畢竟我是要工作寫程式不是在玩遊戲啊，工作效率才是最重要的，我根本不想管 Emacs 定位是編輯器還是 IDE。

综上，Emacs 就是个 IDE。除了 LSP 之外，Emacs 还能对接 Debugger 协议 [DAP](https://github.com/emacs-lsp/dap-mode)。

不过对接 debugger 本来就是 Emacs 的传统强项，比如 [gdb](https://www.gnu.org/software/emacs/manual/html%5Fnode/emacs/GDB-Graphical-Interface.html)……

> 後來試試 VSCode，一裝好，哇好流暢的自動補全啊（Company 實在有夠慢）

如果 Emacs 突然特别吃 CPU ，可以 `M-x profiler-start RET cpu`
，然后执行高负荷操作，然后 `M-x profiler-report` 看哪个函数在吃
CPU 时间。向包作者汇报 Bug 时[是个相当有用的信息](https://github.com/elixir-editors/emacs-elixir/issues/445#issuecomment-547253610)。

> 哇code 不用外掛就可以折疊耶

🤔️说来奇怪，我在用 Vim 的时候也是个重度折叠爱好者，但用了几年
Emacs 之后我丝毫不怀念这个功能……估计是导航太方便了？比如
`lsp-ui-imenu` ：

{{< figure src="/ox-hugo/深度截图_emacs_20200211143954.png" link="/ox-hugo/深度截图_emacs_20200211143954.png" >}}

现在我只会在 `org-mode` 里用折叠。

> 哇 TypeScript 補全超聰明還會自動 import 耶

`lsp-ui` 右侧的快捷操作里有自动 import ，这是 LSP server 给的
suggestion。黄色的字都是可以点击 / 快捷键触发的，相当于 IDE里的「💡️」。

{{< figure src="/ox-hugo/深度截图_st_20200211144641.png" link="/ox-hugo/深度截图_st_20200211144641.png" >}}

> 哇寫 Vue 時編輯器可以直接理解 < script > 裡面的 TypeScript 語意耶

？？[2017 年可以做到啊](https://emacs-china.org/t/topic/4561/5)……

> 媽呀找Reference的UI也太方便了吧……

你说 UI 不好看我同意，至于方便嘛…… `lsp-ui-doc` 能[直接调用 webkit
内核渲染文档](https://github.com/emacs-lsp/lsp-ui#lsp-ui-doc)，和 VSCode 平起平坐了吧至少？


## <span class="section-num">2</span> 现状 {#现状}

上文所抱怨的东西我都理解并经历过，我刚使用 Emacs 是 2017 年，那时
LSP 还不是很成熟，语言的支持并不多，不值得折腾，颇有「编辑文件猛如虎，一进项目二百五」的感觉：JS 有自己的补全包、Ruby 有自己的补全包、
Python 有自己的补全包，一个软件包一套配置，十分费劲不说，项目内互动还不好（当时只有 Java 好些，一是语言比较显式，二是 Eclipse 定下的那套已经成为事实标准了）。

第一次尝到 LSP 甜头是 PHP 的 [intelephense](https://github.com/bmewburn/vscode-intelephense) ，惊为天人，不夸张地说有
phpstorm 八成功力了，甚至还能有有限的类型推理，对一个动态语言来说要啥自行车，遂立即卸载掉残废的 [ac-php](https://github.com/xcwen/ac-php)。也就是在这时（18年？）我才感觉 Emacs 可以作为一个严肃的 IDE 来用了。

所以现状就四个字：能用、好用。
