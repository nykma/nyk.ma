---
title: "Emacs 自力求生指南 ── 基本概念"
author: ["Nyk Ma"]
date: 2020-02-12T18:20:00+08:00
lastmod: 2020-02-12T20:35:40+08:00
tags: ["emacs"]
categories: ["tutorial"]
draft: false
---

## <span class="section-num">1</span> Buffer、Window 和 Frame {#buffer-window-和-frame}

{{< figure src="/ox-hugo/深度截图_emacs_20200212151247.png" link="/ox-hugo/深度截图_emacs_20200212151247.png" >}}

这张图是一个独立的 Emacs 窗口。这个窗口叫 frame[^fn:1]。

这个窗口被均分成了四块小窗口，每个小窗口叫 window。

整个窗口最底下一条空行叫 minibuffer，一般用来输入指令、搜索关键词等。

每个 Window 的最底下有一根「状态栏」，这个状态栏叫 mode line。

Window 里显示的内容是某个 Buffer 的一部分。Buffer 的名字写在 mode
line 里。比如右上角的 Buffer 是 `*scratch*` ，是你冷启动 Emacs 后第一个看到的 Buffer[^fn:2]。

mode line 的内容可以被你或者 Major mode 随意定制，我截图里显示了诸如 window 编号、光标在第几行、文件编码、是否有未保存修改等。
`telega` 额外显示了一些和 Telegram 有关的信息，比如群名、群人数、置顶信息、甚至一个「托盘图标」[^fn:3]。

Buffer 和 Vim 的概念比较接近，但内容物的弹性比 Vim 好得多。比如
[artist-mode](https://www.emacswiki.org/emacs/ArtistMode) 允许你用鼠标在这个 Buffer 的任意地方画画[^fn:4]；再比如这是一个 Image buffer，和普通文件一样使用 `C-x C-f` 打开：

{{< figure src="/ox-hugo/深度截图_st_20200212154417.png" link="/ox-hugo/深度截图_st_20200212154417.png" >}}

关闭 Window 不代表关闭了对应的 Buffer ，因为 window 只是个 view point 而已。

一般不太用在意后台开着的 buffer 。有洁癖？看看 `IBuffer` 吧，杀
buffer 比杀进程还爽。

<details>
<summary>
说说我理解的 Emacs 默认行为：
</summary>
<p class="details">

我刚从 Emacs 切到 vim 的时候，有一点极其痛苦：似乎 Emacs 不鼓励你分割 window。

三年后的现在，我大概理解为什么了：因为 Emacs 不需要频繁地分割
window ，它的工作流不是这样的。

比如一个查文档的场景：很多帮助文档 （比如 `describe-key` 之类的）在显示结果时， **不抢你的 point 焦点** ，只把屏幕平分一半，另一半显示文档。

为什么呢？因为很大可能你只需要「瞄一眼信息」而不用真的把光标跳过去复制什么东西，下面的 Emacs 功能就是为这个场景设计的：

`C-M-v` (`scroll-other-window`)
: 对另一个 window 翻页

`C-x 1` (`delete-other-windows`)
: 关闭其它 window，仅保留 point 所在 window 。

如果你真的要跳转光标，也很方便：

`C-x o` (`other-window`)
: 光标跳转到下一个 window

`C-x 0` (`delete-window`)
: 关闭当前光标所在的 window

你理解了这个场景，就知道为什么最好按的 `1` 被分配这样一个功能了。

Emacs 推荐你只关注一个 window ，其它内容都遵循这个「呼之即来，看完就关」原则，没有移动光标带来的精力开销和多余操作。

为了做到「呼之即来」，Emacs 社区打磨沉淀了相当多的软件和工作流让你方便地跨行、跨文件、跨项目跳转。比如 `M-.`
(`xref-find-definitions`) 用来查找光标下 symbol 在项目里的定义所在，看完了之后 `M-,` (`xref-pop-marker-stack`) 返回。跳过去瞄一眼，再回来，不需要分割 window 的。

当然，如果你不喜欢这个思想，一样完全可以定制[^fn:5]。毕竟 Emacs 没啥不能定制的。比如我自己就写了[一个简单的 hydra](https://github.com/nykma/nema/blob/develop/nema/nema-hydra.el#L8) 用来做 window 操作。
</p>
</details>


## <span class="section-num">2</span> Major mode 和 Minor mode {#major-mode-和-minor-mode}

每个 Buffer 都一定会有一个 Major mode ，通常会搭配若干个 minor mode。

通常来说， Major mode 决定了这个 buffer「用来做什么」，minor mode
用来添加泛用的修饰性功能。

-   比如你写 Python 和 Ruby 时， major mode 分别是 `python-mode` 和
    `ruby-mode` ，因为你在做不同的事。
-   但是写这两个语言时都会用到代码模板 `yasnippet` 和括号上色
    `rainbow-delimiters` ，那么这两个功能由 minor mode 提供。

下图显示该Buffer 的 major mode 是 `Emacs-Lisp` ，minor mode 有 `ElDoc`

{{< figure src="/ox-hugo/深度截图_选择区域_20200212163435.png" link="/ox-hugo/深度截图_选择区域_20200212163435.png" >}}

> 不是所有 minor mode 都会在 mode line 里显示 Indicator （如上图的
`ElDoc` ）的。要查看一个 Buffer 里激活的所有 mode，使用 `C-h m` (
`describe-mode` )

> 不同 major mode 有自己的回调函数列表 `xxx-mode-hook` 和键位绑定
`xxx-mode-map`

> 有的 major mode 有「继承」关系，比如 `php-mode` 继承了 `c-mode`
。效果是，如果你在 `c-mode-hook` 里添加了回调函数，那么 `php-mode`
激活时也会生效。

> 鼠标可以和 mode line 上的组件互动。指针悬停就能看到帮助信息。


## <span class="section-num">3</span> Point、Mark 和 Region {#point-mark-和-region}

输入光标叫 Point。

某些光标的跳转操作，比如 `C-s` (`isearch-forward`) 搜索并跳到关键词，point 跳到了新位置后，原本的光标位置被标记为 Mark 。

Mark 和 Point 之间的部分称为 Region。

很多 `xxx-region` 的操作对象即为此。比如，要删除大段文本，可以用诸如
[avy](https://github.com/abo-abo/avy) 这种快速定位软件来做：

-   用 `avy-goto-char` 跳转到文本开头
-   同样的方法，跳转到文本末尾
    -   此时先前的光标成为了 mark
-   `C-w` ( `kill-region` )

省去了按住 Shift 或者拖左键之类的事。

> Region 不需要「高亮」。 `C-x C-x` ( `exchange-point-and-mark` )
能交换 Point 和 Mark 的位置，顺便高亮出 Region。

> 手动「框选」一段文字： `set-mark-command` (`C-SPC`) 然后移动光标。这流程只有 tmux 里才算有用。Emacs 里用处不大，请活用你的搜索增强工具（比如上文的 `avy` ）。


## <span class="section-num">4</span> Kill ring {#kill-ring}

简单地说，kill ring 是「剪贴板历史」。

Emacs 里常见的删除操作（比如 `C-w` `kill-region` 或者 `C-k`
`kill-line` ）都会把被删除部分放在 kill ring 里，随时可以用 `C-y`
(`yank`) 粘贴回 buffer。按几次 `C-y` 就粘贴几次。

kill ring 是一个链表：在 `C-y` 一次后紧接着按 `M-y` (`yank-pop`)可以粘贴回倒数第二个 kill ring 内容，再按一次 `M-y` 就是倒数第三个内容，以此类推。

> 有些软件（比如 `helm` ）可以帮你可视化 kill ring
  （ `helm-show-kill-ring` ），你可以直接从列表中搜索并挑一个粘贴，如下图是敲入关键字后 kill ring 被 filter 了之后的结果：

{{< figure src="/ox-hugo/深度截图_选择区域_20200212174753.png" link="/ox-hugo/深度截图_选择区域_20200212174753.png" >}}

> Kill ring 功能在 Shell 里也有。熟悉了 Emacs 的操作后，你也可以在
bash 里 `C-k C-a C-y` 。


## <span class="section-num">5</span> Undo 操作 {#undo-操作}

Emacs 的操作历史和 kill ring 类似，也是一个链。

和一般软件不同的是，Undo 操作(`C-/`, `undo`)也是一个操作，也会被记录在树里。举个栗子：

| 操作顺序 | 键盘指令 | 屏幕显示 | 此时的 undo 链                 | 解释                          |
|------|------|------|----------------------------|-----------------------------|
| 1    | `aaa` | `aaa`    | 1                              | 正常打字                      |
| 2    | `bbb` | `aaabbb` | 1 -> 2                         | 正常打字 (与1间隔一点时间)    |
| 3    | `C-/` | `aaa`    | 1 -> 2 -> 2'                   | Undo 一次                     |
| 4    | `C-/` | `(空)`   | 1 -> 2 -> 2' -> 1'             | Undo 两次                     |
| 5    | `C-g` | `(空)`   | 1 -> 2 -> 2' -> 1'             | 打断本次连续 Undo。除了 `C-g` 外移动光标也行 |
| 6    | `C-/` | `aaa`    | 1 -> 2 -> 2' -> 1' -> 4'       | 4' 就是 1'' ，也就是复现 1 （相当于 redo） |
| 7    | `C-/` | `aaabbb` | 1 -> 2 -> 2' -> 1' -> 4' -> 3' | 3' 就是 2'' 相当于复现 2      |

[undo-tree 软件](https://www.emacswiki.org/emacs/UndoTree)能把 undo 历史从链升级为树，同样[也有可视化功能](https://www.emacswiki.org/emacs/UndoTree)：

{{< figure src="/ox-hugo/undo_tree_screenshot.png" link="/ox-hugo/undo_tree_screenshot.png" >}}

[^fn:1]: 我用的窗口管理器不绘制所有窗口的标题栏，所以截图里没有标题栏。
[^fn:2]: 无配置 Emacs 的首屏是欢迎页面。在这个页面按 `q` 可以看到这个 Buffer。一般我们不太需要这个欢迎页，因为 Emacs 不会经常冷启动。
[^fn:3]: Emacs GUI 定制很灵活，Buffer 不仅能渲染 PNG 之类的图片文件，甚至可以直接[用代码「点」出一个位图来](https://github.com/milkypostman/powerline/blob/master/powerline-separators.el#L385)。
[^fn:4]: 比如有人喜欢在注释区画框图。
[^fn:5]: 比如给上面跳转函数包一层 advice 之类的
