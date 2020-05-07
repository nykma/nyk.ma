+++
title = "notmuch 及周边工具配置指南"
author = ["Nyk Ma <i@nyk.ma>"]
date = 2020-05-07
lastmod = 2020-05-07T16:00:26+08:00
tags = ["mail"]
categories = ["tutorial"]
draft = false
+++

> aka 当我收发邮件的时候我在做什么


## You don't need this {#you-don-t-need-this}

如果你搜到了这篇文章，听我一句劝，哪怕你再对标签式邮件数据库、CLI MUA 或者黑客 style 感兴趣，放下你的好奇心，去装个 Thunderbird 用吧。人家要弹性有弹性要易用有易用，什么 edge case 都考虑到了，比自己折腾的强百倍不止……

还要看？那…我给几条理由你对照下：

-   你是 IT，在维护 / 将要搭建公司的邮件系统；
-   你是运维，每天都要处理一大堆告警邮件；
-   你是程序员，你开发的项目使用[邮件列表型开发流程](https://git-send-email.io/)（比如 linux kernel）；
-   你是老学校黑客，订阅了一大堆邮件列表当论坛刷；
-   你是外企文秘，日常工作需要浏览并回复十几条邮件讨论串；

上述理由至少满足两条才有折腾本文的价值，否则去下载 Thunderbird 吧。


## 啥是 email {#啥是-email}

邮件
: 一个文本文件，格式由 [RFC5322](https://tools.ietf.org/html/rfc5322) 定义。

> 简单地说，它有 header 部分和 body 部分，中间用一个空行隔开。
>
> Header 部分是 `Key: Value` ，body 是任意 ASCII。
>
> “附件”会被转成纯文本（比如Base64）内嵌在邮件里（[RFC2046](https://tools.ietf.org/html/rfc2046)）

收
: 现代使用 IMAP 协议（[RFC3501](https://tools.ietf.org/html/rfc3501)）。

> IMAP 是个“同步”型协议，服务器和本地的状态变化会双向同步。下详。

发
: SMTP 协议（[RFC821](https://tools.ietf.org/html/rfc821)）。

> 不仅客户端和服务器之间是 SMTP ，服务器和服务器之间也是 SMTP。
> 比如从 `@gmail.com` 发送到 `@yahoo.com` 的邮件是由 Gmail 和 Yahoo 的邮件服务器之间通信转手的。


## 邮件客户端做了啥 {#邮件客户端做了啥}

以 Thunderbird 为例，先搞清楚一体化方案做了什么，我们才能拆分任务、逐个实现。


### TB：看 {#tb-看}

TB 有 web 引擎以正确地渲染 HTML 正文的邮件。


### TB：写 {#tb-写}

简单地说，只需要一个文本编辑器。不过 Thunderbird 能让我们在 Body 部分 WYSIWYG 地排版 HTML 型邮件。同时 TB 还有地址本功能，方便我们快速补全 `To:`  部分。地址本的数据源来自已有邮件以及 LDAP 服务器（若已配置）。


### TB：管理 {#tb-管理}

TB 通过 IMAP 下载了服务器端的邮件目录结构并压缩到本地的一个个 `.mbox` 文件里。

每个 `.mbox` 文件对应服务器上的一个邮件文件夹。

> 我们不使用这种做法，而采用另一种弹性、安全性更好的 `Maildir` 型管理方法。下详。

另外 TB 建立了所有邮件的全文索引。


### TB：收 {#tb-收}

TB 实现了 IMAP 协议的客户端，定期向服务器同步邮件，下载和上传不同的部分，并归档到 mbox 里。


### TB：发 {#tb-发}

TB 实现了 SMTP 协议的客户端，向 `From:` 里指定的服务器发送邮件请求。


## 那么动手吧 {#那么动手吧}

我们把环节拆分开，每一环都可以单独调试或者替换为其他方案，这是 Unix “组合小工具”哲学的优势，也是我折腾这套方案的原因。


### 写 {#写}

这个最简单。打开一个顺手的编辑器，粘贴以下文本：

```sh
From: Alice <alice@example.com>
To: Bob <bob@example.com>
Subject: Hello from Alice

This is a test mail from Alice. Hello Bob!
```

再保存为一个纯文本文件。恭喜你写好了一封 Email ！

我是认真的。这（几乎）就是一封 valid 的 Email。


### 发 {#发}

先搞发送部分，模块依赖少，测试简单。

在你的系统上安装 `msmtp` 和根证书包（一般叫 `ca-certificates` ？）

然后编写一个 `~/.msmtprc` 配置文件：

复制粘贴前看清楚例子里和你有关的部分，并改动成对你而言 make sense 的样子。

```bash
# 本配置具体内容参照 man 1 msmtp
# 所有 account 共通的设置
defaults
# 服务器需要登录
auth	on
# 需要安全连接
tls	on
# 使用的根证书。根据你自己系统写。
tls_trust_file	/etc/ssl/certs/ca-certificates.crt
# Log
logfile	~/.msmtp.log
# 连接服务器所用的端口号
port 587

# 开始单个帐号配置。alice 是配置名，随意写，能区分即可
account alice
# 匹配邮件里的 From ，如果是以下这个邮箱，使用此登录信息投递。
from alice@example.com
# 连接的服务器地址
host mail.example.com
# 登录用户名
user alice@example.com
# 登录密码
# 如果你需要写明文密码：
# password MyPassw0rd!
# 当然强烈不推荐这么做。我们可以执行命令来动态获取密码。比如使用 gpg ：
passwordeval "gpg2 -d --quiet --for-your-eyes-only --no-tty ~/.password-store/alice_example_com.gpg"

# 如果我有另一个邮箱
account alice-Gmail
from alice@gmail.com
host smtp.gmail.com
# 如有需要，可以覆盖 defaults 里的声明
# port 465
user alice@gmail.com
passwordeval "gpg2 -d --quiet --for-your-eyes-only --no-tty ~/.password-store/alice_gmail_com.gpg"
```

来测试，一行就够了：

```bash
# -a 后跟配置文件里的 account 配置名， 最后的参数表示收件人。邮件本体通过 stdin 灌进去。
$ echo "This is a test mail" | msmtp -a alice alice@example.com
# 然后登录 alice@example.com 看收件箱，应该会有一封来自它自己的无题邮件。
```

当然，如果你想用上上面那个纯文本邮件，先编辑一下 From 和 To 以符合你的情况，然后

```bash
# -t： 从 To: 部分读取收件人
$ cat test_mail.txt | msmtp --read-envelope-from -t
```

这就是发邮件的全部了。


### 收 {#收}

现在 IMAP 协议是唯一的选择，让 POP 躺在博物馆里吧。

收邮件其实是“与服务器同步许多纯文本文件的状态和位置”的过程。

-   纯文本文件：就是你上面手写的那个
-   状态：「是否加星（aka 旗标）」「是否被标记为垃圾邮件」「是否已读」等，与单封信件有关的状态
-   位置：该邮件在邮箱里的哪个「文件夹」下

安装 `isync` ，它提供一个叫 `mbsync` 的可执行。

编写 `~/.mbsyncrc` ，它相对罗嗦一些：

```bash
# 这事儿复杂多了，因为这是个双向通信，server/local 两边都有各自的状态
# 先来看个简单点的例子，让本地的目录结构一比一照抄服务器的

# 定义一个远端服务器配置
IMAPAccount alice-example-com
# 以下信息几乎和上面一样，按自己情况改，没啥好说的
Host mail.example.com
# Port 993
User alice@example.com
PassCmd "gpg2 -q --for-your-eyes-only --no-tty -d ~/.password-store/alice_example_com.gpg"
SSLType IMAPS
CertificateFile /etc/ssl/certs/ca-certificates.crt

# 定义一个远端服务器配置组
IMAPStore alice-remote
# 该组只有一个远端配置，写上配置名
Account alice-example-com

# 远端弄完了，配置本地
# 定义一个本地 Maildir 的存储配置
MaildirStore alice-local
# 子文件夹策略： 字面上的（不替换 / 字符，真的在磁盘上创建子文件夹）
SubFolders Verbatim
# 本邮箱在硬盘上的路径
Path ~/.mail/alice/
# INBOX （默认收件箱）这个文件夹略特殊，可以单独配置它的位置
Inbox ~/.mail/alice/INBOX

# 最后定义一个远端 - 本地联动
Channel alice
# 串联一个远端配置
Master :alice-remote:
# 和一个本地配置
Slave :alice-local:
# 无脑同步所有文件夹。推荐起手这么配置，可以下载到完整的服务器目录结构。接下来可以单独定制
Patterns *
# 把 mbsync 的同步状态 metadata 存在每个文件夹里。详见 man 1 mbsync
SyncState *
```

现在本地文件夹是空的，所以执行“同步”不会弄乱服务器上的东西，是个纯下载行为。
所以调试配置时只要删掉 `~/.mail/alice` ，就可以放心地重新同步。

```bash
# 参数是 Channel 名
mbsync alice
```

`cd ~/.mail/alice` 应该能看到这个邮箱里的所有文件夹了，每个文件夹里有 `cur` 、 `new` 和 `tmp` 。

服务器上找一个有邮件的文件夹，然后本地打开对应的 `cur` 文件夹，你应该能看到很多小文件，每个文件就是一个邮件，你可以 `cat` 任意一个。


### 管理 {#管理}

我们先说说上面这个“每封邮件一个文件”（Maildir）这个本地方案相比 TB 的“每个文件夹一个文件”（mbox）方案好在哪儿：

-   故障范围小。试想如果一个文件的磁盘扇区损坏了，哪个方案受灾大。
-   Maildir 只需要系统级命令就可以管理邮件了：
    -   查看邮件：简单地说 `cat` 就行。但 mbox 不行（因为它近似一个 =.tar.gz=）
    -   移动邮件到新的文件夹： `mv` 。同样 mbox 不行，必须同时修改两个文件。
    -   改邮件的状态：文件重命名。

        <details>
        <summary>
        Σ(っ°д°)っ？？？
        </summary>
        <p class="details">

        假设本地有一封邮件的文件名是 `~/.mail/alice/INBOX/cur/ed114514-037e-11ea-93ce-f49634d6ed04,U=1829:2,S`

        我们只关心 `2,` 之后的部分，这部分标识了邮件的状态。

        现在只有一个 `S` ，表示这封邮件已经 seen （已读）了。

        如果要重设为未读，把 `S` 删掉。

        如果要加旗标（或者叫星标），加一个字母 `F` 。

        改名后跑一次 `mbsync` ，服务器那边这封邮件的状态也会同步为我们所设的。

        详见 [maildir](http://cr.yp.to/proto/maildir.html) 的介绍。很短，很有用，强烈建议看一遍。
        </p>
        </details>

那么，理论上说，邮件管理软件只要能方便地 `mv` 文件就行了。已经有很多软
件能做到了， `mutt` 和 `sup` 都支持 Maildir。如果你看完下面 notmuch 的
介绍后不感兴趣，完全可以换别的软件。

notmuch 和上面这些的思路不同在于，它是一个完全独立于 Maildir 外的
\*\*Tag 数据库\*\*。它是、且只是一个数据库，用来给邮件打 tag，不多不少。

那我为啥要用它呢？类比数据库型音乐管理软件（比如 iTunes）：

| 场景    | iTunes                  | notmuch                     |
|-------|-------------------------|-----------------------------|
| 数据库  | 有自己的数据库，缓存了所有音乐的 ID3 信息 | 有自己的数据库，缓存了所有邮件的metadata和正文 |
| 添加新内容 | 不会动音乐文件（只记录file path） | 不会动信封文件（只记录 File path） |
| 搜索内容 | 很快（不用遍历所有文件） | 很快（不用遍历所有文件）    |
| 把内容加入集合 | 把音乐添加到播放列表不会真的移动音乐文件 | 给邮件加 Tag 不会改动邮件文件本身 |
| 数据种类的弹性 | 可以添加 ID3 标准里没有的字段 | Tag 本身就不是邮件标准的一部分。 |
| 外部工具 | 有很多第三方软件能读写 iTunes 的数据库 | 有很多第三方软件使用了 notmuch 的 API |

简单地说就是“数据库与文件解耦”，可以让你更加专注于内容本身而不用关心文件的状态或位置。

所以 notmuch 的正确用法不是「手动给每个邮件标tag」，而应该使用脚本帮我
们自动打 Tag，以及根据一个邮件 Tag 的状态决定这个邮件应该如何被更名或
移动。notmuch 的弹性使得自动化脚本有实现的可能，自动化脚本使 notmuch
的数据库真正有使用价值。这个自动化脚本叫 `afew` ，下面详述。


#### 建立 Tag 数据库： `notmuch` {#建立-tag-数据库-notmuch}

先把 notmuch 装起来。然后 `notmuch setup` ，此时会有一个简单的 wizard 询问邮件路径在哪儿、你的常用邮件地址等。
这些都可以事后在 `~/.notmuch-config` 里手动改。

如果你使用了我上面的示例收件配置，邮件路径应该是 `~/.mail` 。下面的描述也使用这个配置。

然后执行 `notmuch new` 刷新数据库。

建立好的数据库在 `~/.mail/.notmuch` 里。

notmuch 注重建立 / 刷新数据库的速度。对我而言，上万封邮件大概 2-3s 就可以初始化好。

每次收到新邮件 / 更新邮件文件状态后都要执行一遍 `notmuch new` 增量式地刷新数据库。

如上所说， notmuch 是完全独立于 Maildir 之外的。
任何时候更新数据库（甚至删除 `~/.mail/.notmuch` 并重建数据库）对当前邮件的状态都是无改动的。

这时就可以快速花式搜索邮件了。 `notmuch help search` 给了一些 sample，建议都试一遍，不过它需要搭配一个前端才能更好地发挥价值。

> notmuch 手动增减 tag 的方法：给定一个搜索条件，再给若干个tag增减动作：
>
> `notmuch to:alice@example.com +to-me +important`
> : 发给我的邮件新增两个 tag： `to-me` 和 `important`
>
> `notmuch from:github.com -unread -inbox +github`
> : Github发给我的邮件：标记为已读、删除 `inbox` tag、增加 `github` tag
>
> 记住这个加号和减号的操作方法，这个用法会贯穿所有的使用场景。


#### 自动化 Tag 流程： `afew` {#自动化-tag-流程-afew}

我们知道一个新邮件在被 `notmuch new` 之后，Tag 只有 `new` 一个[^fn:1]。 这显然不方便我们未来的搜索及自动化。

为了让新邮件根据某些规则在入库时就打好 Tag，我们引入 `afew` 。

[安装](https://afew.readthedocs.io/en/latest/installation.html)好后，编写它的配置：

```toml
# ~/.config/afew/config
# 所有新邮件会从上至下经过所有规则

# 邮件所在文件夹是什么名字，就打上什么 tag
[FolderNameFilter]
# 子文件夹分隔符： /
# github/receipt 会被打上两个Tag： +github +receipt
maildir_separator = /
# 以下文件夹不加 tag
folder_blacklist = Archive INBOX
# 所有文件夹名先转小写再打 Tag
folder_lowercases = true

# 被邮件服务器打上 Spam 标记的邮件： +junk
[SpamFilter]
spam_tag = 'junk'

# 有一个极其活跃的讨论串，但内容我不感兴趣
# 因此我给这个串 +killed
# 接下来这个讨论串的新增讨论都会被自动 +killed
[KillThreadsFilter]

# 邮件 Headers 里的 List-Id： +lists/list-id
# 对订阅的邮件列表极其有用
# 比如 emacs-devel 邮件列表会被加上 lists/emacs-devel
[ListMailsFilter]

# 我自己已发送的邮件不打 tag
[ArchiveSentMailsFilter]

# To 给我的邮件： +to-me
[MeFilter]

# 在 INBOX 文件夹里： +inbox
[InboxFilter]

# 其他预置 filter 或自定义 filter：
# https://afew.readthedocs.io/en/latest/filters.html
```

这个流程最好在 `notmuch new` 后立即执行。好在 notmuch 有 hook，我们不用手动执行 afew 了：

```sh
#!/bin/sh
# ~/.mail/.notmuch/hooks/post-new
# See: man 5 notmuch-hooks

# -t: 跑一遍自动打 tag 流程
# -n: 只对 tag:new 跑流程
# See: man 1 afew
/usr/bin/afew -tn
```


#### 自动移动邮件至其它文件夹： `afew` {#自动移动邮件至其它文件夹-afew}

notmuch 标签数据库只能自嗨，你难免会用个手机 / web 邮件客户端之类的。
要是一登上去发现所有邮件还是躺在 INBOX 里，总是感觉不爽的。

这时我们希望根据邮件当前标签移动至不同的 maildir，这样至少能在线上看到
一个干净的 INBOX ……

```toml
# ~/.config/afew/config
# 同样的配置文件，把这段放在上一段后面即可

[MailMover]
# 要列出所有涉案的本地 maildir
folders = 'alice/INBOX' 'alice/Junk' 'alice/Archive' 'alice/Sent'
rename = True

# xx 天之前的邮件不移动
# max_age = 15

# 规则：等号左边 dir 里的邮件，如果满足引号左边的搜索条件，则被移动到冒号右边的 dir
# 注意：如果一个邮件同时符合多个搜索条件，它会被复制多份至所有符合条件的 maildir
# 至于为什么，有过讨论：
# https://github.com/afewmail/afew/issues/242
# 这就是为什么这里写得这么死板
# 当然你可以把它当作一个 feature 加以利用
alice/INBOX = 'tag:junk':alice/Junk 'NOT tag:inbox AND NOT tag:junk':alice/Archive
alice/Junk = 'NOT tag:junk AND tag:inbox':alice/INBOX 'NOT tag:junk':alice/Archive
alice/Archive = 'tag:inbox AND NOT tag:junk':alice/INBOX 'tag:junk':alice/Junk
alice/Sent = 'NOT tag:sent':alice/INBOX
```

使用 `afew -m` 来执行移动操作，再使用上面[收](#收)环节的命令和服务器同步邮件移动的结果。


### 看 {#看}

简单地说，我们需要一个「（至少能）正确渲染邮件纯文本部分的查看器」，如果能无缝衔接 notmuch 的 tag 设计思路是最好。这里提两个前端：


#### `alot` {#alot}

[alot](https://github.com/pazz/alot) 是 `afew` 的姊妹项目（咦），类 vim 操作，用 `\` 呼出 search prompt。


#### `notmuch-emacs` {#notmuch-emacs}

<https://notmuchmail.org/notmuch-emacs/>

得益于 Emacs 强大的文本处理弹性和 [message-mode](https://www.emacswiki.org/emacs/MessageMode) 对邮件场景做的 40 年陈酿贴身优化，使用 Emacs 撰写邮件极其舒服。

`notmuch` 包在 MELPA 里有：

```elisp
(use-package notmuch
  :config
  ;; helm 和 ivy 都有 notmuch 的增强菜单功能。我用的 helm
  ;; M-x helm-notmuch
  (use-package helm-notmuch)
  (setq
   ;; 发送邮件时另存一份到自己的 Sent Maildir
   ;; 有的邮件提供商会自动帮你做这个事；有的邮件提供商不允许你向 Sent 上传新邮件。看情况改。
   ;; C-h v notmuch-fcc-dirs
   notmuch-fcc-dirs '((".*example.com" . "\"alice/Sent\" +sent -new -inbox")
                      ("alice@gmail.com" . "\"Gmail/[Gmail]/已发邮件\" +sent -new -inbox"))
   ;; 发送邮件时调用外部程序的方法。
   ;; SEE ALSO: https://github.com/kzar/davemail
   send-mail-function 'sendmail-send-it
   sendmail-program "/usr/bin/msmtp"
   mail-specify-envelope-from t
   message-sendmail-envelope-from 'header
   mail-envelope-from 'header))
```

然后 `M-x notmuch`

{{< figure src="/ox-hugo/notmuch_emacs.png" link="/ox-hugo/notmuch_emacs.png" >}}

输入搜索条件（或者点击预置的搜索）进入搜索结果页：

1.  按 `l` 可以用额外的搜索条件 filter 可见部分的邮件
2.  `*` 可以对所有可见邮件作批处理。

举个工作流例子：

1.  首页按 `j i` 进入 `tag:inbox` 的搜索结果页
2.  `l from:leetcode.com RET` filter 出广告邮件的发送者
3.  `* -inbox +junk RET` 列表内所有邮件删除 `inbox` 标签并增加 `junk` 标签
4.  按 `g` 刷新此页，发现没有邮件符合条件了（因为 `inbox` 的 tag 没了）
5.  按 `q` 退出 filter 页，回到 inbox 页
6.  按 `g` 刷新 inbox，广告也消失了
7.  选择一封邮件按 `RET` 开始阅读
    -   如果一个邮件的 multipart 有 `text/plain` 部分，则优先显示此纯文本部分
    -   如果没有，使用 `mm-text-html-renderer` 指定的 HTML 渲染器排版 `text/html` 部分
    -   当然[这个策略可以更改](https://notmuchmail.org/pipermail/notmuch/2015/020190.html)
8.  按 `f` 转发， `r` 回复， `R` 回复所有人，或者任意地方按 `m` 新建一封邮件
    -   注意几乎所有东西都是可以编辑的。比如你想增加密送字段，那在 header 部分加一行 `Bcc:` 就行
    -   同时注意确认 `From:` 字段是不是你要使用的身份
    -   附件 `M-x mml-attach-file`
    -   GPG 签名 `M-x mml-secure-message-sign-pgpmime`
    -   发送 `C-c C-c`
    -   撤销 `C-c C-k`


### 我的日常使用流程 {#我的日常使用流程}

1.  移动邮件、与服务器同步、新邮件建立数据库： `afew -m; mbsync -a; notmuch new`

    > 因为上文已经给 `notmuch new` 加了 hook ，这里不需要再 `afew -tn` 了。
    > 其实 `mbsync -a` 和 `afew -m` 环节都可以放到 notmuch 的 hook 里

2.  在 emacs 里读、写、回、改标签
3.  再次执行 1，把改动写回服务器

把上面的配置文件（和 GPG 加密的密码文件）备份下来，换个新系统就可以直接
`mbsync -a; notmuch new` 开始工作了。


## FAQ {#faq}


### 收：我只想收特定文件夹 {#收-我只想收特定文件夹}

看下面一个问题的 `.mbsync` 例子。


### 收：为什么文件夹乱码了？ {#收-为什么文件夹乱码了}

臭名昭著的 UTF-7 编码，所有 \*nix MUA 绕不过去的痛……

不过还好，只要痛一次……

-   先使用「动手收」一章的配置无脑下载所有文件夹
-   找个 utf7 编码转换器让你读懂这个乱码
    -   [我写了一个](https://gist.github.com/nykma/2f6e55f28ae27537de32654e0a1e9e63)
-   编辑 `.mbsyncrc` ，让本地人话文件夹对接远端的乱码文件夹：

<!--listend-->

```bash
Channel Gmail-archive
Master :Gmail-remote:"[Gmail]/&YkBnCZCuTvY-"
Slave :Gmail-local:Archive
Create Slave
SyncState *
```

这样本地文件夹名就是 `Archive` 了


### 收：能更快点嘛？ {#收-能更快点嘛}

理论上来说你可以一口气起多个 `mbsync` 实例同时收不同的 Channel，只要你把 Channel 拆得很碎（比如一个文件夹一个 channel），

> 事实上最佳实践就是「一个文件夹一个 Channel」。不仅颗粒度小易于管理，也是上文「文件夹乱码」所必须的……


### 看：我觉得 `mutt` 更顺手，我能搭配使用 `notmuch` 嘛？ {#看-我觉得-mutt-更顺手-我能搭配使用-notmuch-嘛}

[当然可以](https://notmuchmail.org/notmuch-mutt/)。


### 写：地址簿？ {#写-地址簿}

notmuch 自带一个 [address 补全引擎](https://notmuchmail.org/emacstips/#index13h2)，数据源就是你所有的邮件。


### 管理：我有两台电脑，notmuch 数据库需要同步 {#管理-我有两台电脑-notmuch-数据库需要同步}

> 比如我在台式机上给邮件打的标签，在笔记本上看不到。因为两台电脑数据库是独立的。

请看 [muchsync](http://www.muchsync.org/) 项目。它使用一主多从的设计结构，client 几乎照搬 server 端 `~/.mail` 的状态。


### 收：我还是觉得配置太麻烦，我想更简单点 {#收-我还是觉得配置太麻烦-我想更简单点}

如果你只使用 Gmail ，有一个非常棒的后起之秀： [gauteh/lieer](https://github.com/gauteh/lieer)

-   它使用 API 和 Gmail 通信
-   它将 Gmail 的 tag 系统和 notmuch 的 tag 系统打通

使用它，你可以跳过上面「收」、「发」和「管理」环节。

缺点嘛，只支持 Gmail。而非标准协议是邪恶的，so ……

[^fn:1]: 默认 Tag 可以修改： `~/.notmuch-config` 里面的 `[new] -> tags` 。
