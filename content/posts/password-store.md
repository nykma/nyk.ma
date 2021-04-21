+++
title = "pass : 密码管理本不复杂"
author = ["Nyk Ma"]
date = 2021-04-21T00:14:00+08:00
lastmod = 2021-04-21T15:20:23+08:00
categories = ["software"]
draft = false
+++

## 前言 {#前言}

「密码管理器」五个字看似简单，实则和「甲方爸爸提的需求」一样，涉及很多功能点，坑有点深。

市售的密码管理器五花八门，核心功能点几乎都 cover 了，顶多换换皮。

那么，密码管理器真的很复杂吗？我们真的不能用手头熟悉的工具和概念拼出一个免费、弹性、强大的方案吗？


## 拆解需求 {#拆解需求}

和[折腾邮件收发](https://nyk.ma/posts/notmuch-tutorial)时一样，我们先拆解甲方爸爸的需求，把用户的心理预期列出来先。


### 数据所有权 {#数据所有权}

用户希望数据的所有权归自己（而不是某个云服务商）所有。


### 加密 / 解密 {#加密-解密}

用户希望使用一个久经实战考验、靠得住的加解密方案。


### 防灾能力 / 鲁棒性 {#防灾能力-鲁棒性}

用户希望，当数据出现问题（比如一个扇区坏了）时，受灾面积能降至最低。


### 字段 / 编辑 {#字段-编辑}

用户希望，除了基本的「登录信息」、「密码」字段外，还能有额外的字段供选用。

加分项：不限制字段的名字、种类和数量。


### 生成密码 {#生成密码}

用户希望能方便地生成随机字符串作为密码。


### 搜索 {#搜索}

用户希望，能在不解密数据的情况下，搜索到当前场景所需要的内容。

> 若要解密后才能搜索，解密后的数据势必需要留在内存里。这增加了一丢丢泄密的可能。


### 使用 {#使用}

用户希望能在各个场景方便地使用加密后的数据。


#### 电脑端 {#电脑端}

用户希望有一个全功能、强大的使用介面。

加分项：允许用户写点简单脚本就能批量处理条目。


#### 浏览器端 {#浏览器端}

用户希望有一个能搜索、填充表单、生成密码的浏览器插件。


#### 手机端 {#手机端}

两大手机 OS 都开放了 API 让第三方密码管理软件能成为数据源，用户自然也希望能用上，App 内填写密码更方便。


### 同步 {#同步}

用户希望多台设备（电脑 / 手机）能互相同步密码库的改动。

加分项：不限制设备的数量，且数量多少不影响同步服务的性能。


#### 加分项：冲突处理 {#加分项-冲突处理}

用户希望，同步时若出现了 conflict，有办法手动解决（而不是只能挑一个版本覆盖另一个）。


#### 加分项：团队内共享 {#加分项-团队内共享}

用户希望，若干个条目的可打开、可编辑者不止该用户一个人。


### 审计 / 修改历史 / 撤销 {#审计-修改历史-撤销}

用户希望能查看密码库的修改历史，甚至回滚某个条目的版本。


## 逐个解决 {#逐个解决}


### 加密 / 解密 {#加密-解密}

对称 / 非对称加密和吃饭喝水一样平常。

对称加密相当于一个「解锁密码」， `openssl` CLI 就能做。

非对称加密嘛……最简单易用的就是 GPG 咯，工具链完备，客户端易用，算法可选。

我们选 GPG 好了。在这个场景里，GPG 方案会有一些额外好处。下详。


### 字段 / 编辑 {#字段-编辑}

这个也简单，文本编辑器嘛，谁还没有个趁手的呢。

「字段」？说到底就是个「结构化文本」罢了。JSON 和 YAML 之类的都行吧。这样我想设置什么字段都行了，我自己定规则。

考虑到手动编辑此文件的便利性，就 YAML 好了。YAML 是「手动编辑」和「程序处理」的折衷。

{{< admonition note "为什么一定得是文本文件？" >}}
当然不是「一定」，只是考虑到「密码管理器」场景里 90% 的信息是文本，那把条目 base 在 plain text 格式最自然。
{{< /admonition >}}

{{< admonition note "这样一来字段里不是只能存文本？" >}}
-   整个文件是二进制：直接把整个文件使用 GPG 加密。
-   条目里某个字段是二进制： `base64` 。
{{< /admonition >}}

{{< admonition note "条目的「别名」？" >}}
更简单了，symlink 嘛。
{{< /admonition >}}


### 生成密码 {#生成密码}

`pwgen`


### 搜索 {#搜索}

谁还没用过 `find` 或者 `tree` 呢。

那我们就把「文件」定为我们的「条目单元」好了。

一个条目一个文件，使用其文件 / 路径名作为搜索依据。

路径名不需要解密，使得信息暴露可控，而且速度极快，挺好的。


### 同步 {#同步}

又要「多设备上传下载」又要「解决冲突」又要「历史版本」又要「回滚修改」……

等等，这不就是 `git` 么……

{{< admonition note "git 对此场景的简单适配" >}}
-   怎么 `diff`

    我们都知道 repo 根目录里的 `.gitattribute` 文件可以让 git 使用指定的外部程序来「解包」一个文件。

    ```bash
    *.gpg diff=gpg
    ```
-   怎么解决冲突

    在上面指定了 diff 使用的程序后，你看到的冲突信息就是 gpg 解密后的了。手动编辑 `.gpg` 后 `git add` 即可，熟悉的日常工作流。
-   怎么上传 / 下载

    这个更简单了。

    1.  自建 git 服务
        -   或者使用现成的，如果你不 care [数据所有权](#数据所有权)的话
    2.  给每个设备新建不同的 ssh keypair
        -   虽然麻烦了些，但可以单独控制每个设备
    3.  把它们关连到这个 repo 上，按需分配读写权限
    4.  最后就是刻在 DNA 里的 `git push` 和 `git pull` 了
-   怎么看历史版本 / 回滚

    这……git 客户端你总有一个的吧……？
{{< /admonition >}}


### 加分项的解决 {#加分项的解决}


#### 鲁棒性 {#鲁棒性}

一个条目一个 `.gpg` 文件，受灾面积最小化。


#### 批处理条目 {#批处理条目}

1.  挑一个你最顺手的脚本语言
2.  调用系统 API `list` 出所有 `.gpg` 文件，开始 foreach
3.  调用 `gpg` CLI 解密
4.  过一遍 `YAML.parse()`
5.  该干啥干啥
6.  处理后的数据结构 `YAML.stringify()` 后 `STDIN` 给 `gpg` 加密
7.  加密后的字节流 `File.save()` 回去


#### 团队内共享 {#团队内共享}

<!--list-separator-->

-  上传 / 下载权限

    很多 `git` 服务端支持控制每个账户的读写权限。

    如果需要更精确的控制，可以考虑在密码库 git repo 里开 `submodule`
    。

<!--list-separator-->

-  编辑权限

    很多人不知道， GPG 在加密时可以[指定「多个收件人」](https://stackoverflow.com/a/597200/1490796)，让多个 GPG key
    都可以加解密同一个 `.gpg` 文件。

    这是 GPG 方案的「[额外的好处](#加密-解密)」。


## 封装方案 {#封装方案}

拼装上面工具的程序你不用自己写。事实上，上述方案就是 [pass](https://www.passwordstore.org/) 的核心工作流，而 `pass` 本身只是个 shell 脚本。

`pass` 脚本帮你封装了最高频的操作：

-   本地创建一个密码库（默认 `~/.password-store/` ），关连一个 GPG
    key 并 `git init`
-   所有修改动作（比如创建条目 `pass generate` 、重命名条目 `pass mv`
    、编辑条目 `pass edit` 等）均会自动调用 GPG 并且 `git commit` 一次
-   `pass git push` 等同于 `cd ~/.password-store && git push`


## `pass` 的最佳实践 {#pass-的最佳实践}


### 条目名 {#条目名}

完整路径里体现出 `网站URL/登录用户名` 足矣。

由于 `pass` 使用目录结构管理条目树的增删改查，条目命名的科学性直接影响使用体验。


### 条目结构 {#条目结构}

密码文件的参考结构如下：

```yaml
密码
login: 用户名
url: 网站 URL
# 其它字段
otpauth://xxxxxxx # TOTP URI
```

这个结构几乎被以下所有[生态补充工具](#pass-生态的补充工具)兼容。你可以按需添加其它字段，只要自己记得就行。


## `pass` 生态的补充工具 {#pass-生态的补充工具}

以下工具均可在官网 [Extensions](https://www.passwordstore.org/#extensions) 一章找到。


### 导入外部密码库 {#导入外部密码库}

[pass-import](https://github.com/roddhjav/pass-import#readme) 项目囊括了流行的密码管理软件，迁移 + 批处理十分方便。

我是从 Bitwarden 迁移进 pass 的。


### 批处理 {#批处理}

[pass-update](https://github.com/roddhjav/pass-update#readme) 可以完成上文「[批处理条目](#批处理条目)」的 2、3、6、7 步。


### OTP {#otp}

{{< admonition note "三思！" >}}
把密码和 OTP secret key 保存在一起有悖「二次验证」的目的！
{{< /admonition >}}

[pass-otp](https://github.com/tadfisher/pass-otp#readme) 项目可以读写 `otpauth://` 开头的 URI。


### 桌面前端 {#桌面前端}

我使用官方的 Emacs 前端 [password-store.el](https://git.zx2c4.com/password-store/tree/contrib/emacs) 和 dmenu 数据源 [passmenu](https://git.zx2c4.com/password-store/tree/contrib/dmenu/passmenu)
。另有很多 GUI 可选，详见官网[Compatible Clients](https://www.passwordstore.org/#other)一章。


### 浏览器插件 {#浏览器插件}

Firefox
: [passff](https://github.com/jvenant/passff#readme) 。 C/S 架构。

Chromium-like
: [browserpass](https://github.com/browserpass/browserpass-extension)。C/S 架构。

qutebrowser
: [qute-pass](https://github.com/qutebrowser/qutebrowser/blob/master/misc/userscripts/qute-pass) userscript。单发脚本。


### 手机 App {#手机-app}

iOS
: [Pass for iOS](https://mssun.github.io/passforios/)

    自带 git 和 GPG 功能，只要导入 GPG key、ssh key 并正确设置 repo
    即可使用，全程如预期。

    甚至可以兼容上文的 `pass-otp` ，通过摄像头扫码添加一个 OTP 密钥进已有条目，以及生成 OTP 六位数。

    同样，它的修改动作也会创建 commit。下拉刷新等同于执行 `git pull
               && git push` 。


Android
: [Android-Password-Store](https://github.com/android-password-store/Android-Password-Store#readme)

    它自带 git 功能，GPG 功能通过外部 app （比如 [OpenKeychain](https://play.google.com/store/apps/details?id=org.sufficientlysecure.keychain)）提供，更加灵活。全程如预期。

    它也支持 OTP 。


### `pass` 的其它实现： `gopass` {#pass-的其它实现-gopass}

[gopass](https://github.com/gopasspw/gopass) 是用 golang 实现的 `pass` 的超集，提供了很多增强功能，诸如多 repo、守护进程（API server）用以和浏览器插件通信、更改加密后端、
REPL 的 CLI 等。

我个人感觉没必要， `pass` 的完成度正正好，功能多了就过了。读者自己按需使用吧。


## 结论 {#结论}

密码管理器是复杂，但也没复杂到需要单独做一个 App 的程度。


## 和市售方案的对比 {#和市售方案的对比}

`git` + `gpg` 就解决的事儿，有必要花钱么？


### 云服务（1Password / LastPass 等） {#云服务-1password-lastpass-等}

拿了我的密码还要拿我的钱？有点滑稽。


### Bitwarden / enpass / KeePass {#bitwarden-enpass-keepass}

单数据库文件，容灾、解密后搜索、冲突处理都是个问题。
