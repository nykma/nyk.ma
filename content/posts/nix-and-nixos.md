+++
title = "Nix 和 NixOS：你们安利方法错了"
author = ["Nyk Ma"]
date = 2023-11-21
lastmod = 2024-04-18T15:39:24+08:00
categories = ["software"]
draft = false
+++

## 不是「可复现」 {#不是-可复现}

NixOS 对用户的卖点根本不是什么「可复现」（ `docker` 和 `k8s` 还不够么）。

我知道他们想说什么，但这么表达会让人 Get 不到。

如果你觉得 Nix 是脱裤子放屁，那你就是三个月前的我。我来和你说说我的心路历程。


## 我的安利（aka 状态式系统的真正卖点） {#我的安利-aka-状态式系统的真正卖点}


### 操作系统？一个大号的软件包罢了 {#操作系统-一个大号的软件包罢了}

所谓操作系统，就是一个依赖很多上游软件包的大型软件 project。定义操作系统的过程就是“引入一堆依赖并写一些胶水把它们粘在一起”的过程。

这点[和 Emacs 思路几乎一致](https://nyk.ma/posts/emacs-compare/)。

说起来有点抽象，当你给几个项目添加过 `nix flake init` 开发环境后，你就会发现项目里的 `flake.nix` 和定义你系统的 `configuration.nix` 没什么大区别，是同一种东西。


### 完整的状态解算 {#完整的状态解算}

每次安装、更新系统都在「求解」一次完整的最终系统状态。

「让系统变成某个状态」是解方程，函数式思维。而非「安装 / 删除什么包」的过程式思维。

其它包管理只计算版本号，nix 则会把配置文件（系统全局的、用户的）、
`systemd` 服务、防火墙、shell 注入、GUI 菜单注册之类的全考虑进去，目前几乎做到了「只要系统状态有解，那系统就能正常使用」。这个信心以前只有
Rust 能给我。

读者应该体验过 ubuntu `do-release-upgrade` 升级到一半报错退出了的那种尴尬吧，这时候系统处于非常不稳定的薛定谔态，重启也不是，不重启也不是，再来一遍 `do-release-upgrade` 也不是。在 NixOS 里，别说软件包了，就连内核 module 甚至内核本身我都是想切就切的，成功了说明能用，不成功不会影响现在的系统。

-   例子：Steam 的 `remotePlay.openFirewall = true;`
-   例子：完全搞砸也能救：

> Rerunning the installer will create a new generation but not touch any
> user data. This means you can "undo" the installation by selecting a
> previous generation in the bootloader. To redo the installation
> without changing your root password or changing the version of
> Nixpkgs, run: `nixos-install --no-root-password --no-channel-copy`


### Nix flake：给你完整的“时光机”体验 {#nix-flake-给你完整的-时光机-体验}

开发项目时，只要项目的 `flake.lock` 不更新，其他人在两年后进入这个项目时会回到同一个时间点上，可以使用 ****在那时能用到的**** 软件包。

> NodeJS 里使用 `opencv` 时，对系统 openCV 组件的版本限制非常严格。因此只要把你 node 项目 `flake.lock` 里的 Nixpkgs channel 的版本号锁住，你永远装不了比当时版本更加新的 OpenCV （及其周边生态，比如显卡驱动之类的）。
>
> Nixpkgs 软件包的本体是源码、上游依赖、编译流程的定义（编译成果二进制只是个副产物）。哪怕预编译二进制被镜像站清了，只要源代码还可以 `git
> checkout` / 下载得到（并通过哈希校验），就一定能编译，也几乎一定能编译通过（只要你使用 `stable` channel）。
>
> 反观 Arch 就只能依靠 Archive 了。Archive 什么时候会被清掉可真不好说，毕竟每个二进制体积都不小。Nixpkgs 就没这个问题，现在整个 Nixpkgs
> unstable channel 也就 35MiB 左右，几乎没有保存历史的难度。更何况
> channel 本身是个 git repo。

<!--quoteend-->

> 你很难说一两年后 ubuntu `focal` 软件源里的 `imagemagick` / `ffmpeg` 会不会升了一两个小版本号，导致你的应用突然无法使用、效率下降等。
>
> 所以哪怕你最终要把应用打包成 Docker，我也极力推荐在 `FROM nixos/nix` 的基础上，用 Nix flake 的思路安装依赖。

<!--quoteend-->

> 有人说 NixOS 是 Gentoo 的精神续作。


### 每个项目只 link 自己的依赖，永远没有冲突 {#每个项目只-link-自己的依赖-永远没有冲突}

`asdf` `rtx` `virtualenv` `rustup` 显得很没必要。

开发环境除了打镜像外几乎不需要 `docker` 。用上文的 `nix flake init
--template=xxx` 创建的开发环境比 Docker 好非常多，比如上文说的“时光机”就是一个非常不可替代的 killer feature，这是 Docker 都无法保证的。


### <span class="org-todo todo TODO">TODO</span> 全局计算完整系统状态：残留垃圾最少 {#全局计算完整系统状态-残留垃圾最少}

-   配置文件的残留问题
-   统一软件包依赖导致的重复问题


### 单发应用： `nix-shell` 用完即丢无负担 {#单发应用-nix-shell-用完即丢无负担}

我 iPhone 白苹果了， `nix shell nixpkgs#idevicerestore` 进入一个有救砖工具的 shell 环境，搞好了之后 `C-d` 退出，我就可以忘了这件事。两星期后系统的 GC 会自动把它删掉。


## 还是说说缺点 {#还是说说缺点}

nix 并不完美，但目前看来，它无疑是全 Linux 生态圈里方向最明确、正确的。


### nix 语言过于垃圾 {#nix-语言过于垃圾}

精确地说，是类型系统不行。敲出 `services.` 之后 LSP 根本不知道这里能补全什么。模块系统的隐式行为太多。

如果有人写了 TypeScript 到 Nix 的生成器肯定非常受欢迎。

> 为啥当初不直接用 OCaml / Haskell / Elixir / Lisp / [Unison](https://www.unison-lang.org) 呢，非要自己发明个四不像。
>
> 哪怕用 Ruby 写个 DSL 呢？
>
> 不过还是比 k8s YAML 和 Dockerfile 好不少的（
>
> 插个私货，我觉得 Unison 非常契合 Nix 的思路，给每个函数做哈希的做法像极了 Nix 给每个软件包做哈希。
>
> 另外 Nix 应该有条件把软件包上传到 IPFS 上，或者至少能把软件包哈希成
> IPFS 的 CID。


### NixOS 还是不能完整管理 systemd {#nixos-还是不能完整管理-systemd}

偶尔有些东西还是要手动 `systemctl enable`

-   例子 <https://github.com/nix-community/home-manager/blob/master/modules/services/blueman-applet.nix>

其实现在 systemd 这个位子给 nix 坐是最好，但奈何 systemd 还是相对更容易让传统 linux 用户接受些。


### 每个用户都有打包的义！务！ {#每个用户都有打包的义-务}

小到给你自己的程序项目写 `flake.nix` ，中到 `override` 一个现有软件包以满足你自己需要，大到给社区贡献新 pkgs 定义，你在使用 Nix 的过程中几乎无法避免广义上的“打包”。

不过好在，按照上文这个顺序练级，不算难。在接受了 Nix 的哲学后，理解起来会越来越顺畅。


### 社区对「可复现」的追求过于病态了 {#社区对-可复现-的追求过于病态了}

一个月前起草这篇文时我觉得这是缺点，但现在想想应该是优点。只有这样才能让 Nix 生态在未来保持健壮性。


## 和其它类似物的比较 {#和其它类似物的比较}

其实不是很类似。


### dotfile 管理器 {#dotfile-管理器}

如果只把 Nix 当软件包管理器用，那它是 dotfile manager 的上位替代。

我们先看看 dotfile manager 的问题：

-   如果 dotfile 只负责 `ln` 配置文件，那在不同电脑间共用同一套配置会特别死板
    -   同一个配置文件在两台电脑间需要改动 5%，能怎么改
        -   像 `fish` 那样能把配置拆成 `conf.d/*.fish` 碎片的还好说，不能拆配置的软件怎么办。
    -   新配了一台电脑，你得判断 `ln` 哪几个碎片
-   如果 dotfile 管理器有类似字符串注入 / template 的功能，那问题又来了：它的弹性有多大？
    -   template 内置了哪些控制流函数？能满足我需要吗？
    -   能执行 shell 命令拿返回值再注入吗？
    -   shell 命令能确保在两台电脑都能跑成功吗？

Nix 则完全没有这些问题：

-   信息源很灵活：
    -   你的系统环境（Linux 还是 darwin ，x64 还是 arm64）
    -   对该软件的直接配置 （比如 `programs.atuin.settings.sync_frequency = "5m"` ）
    -   别的软件的配置（见下）
    -   别的软件的注入（比如非常多的 CLI 工具都会注入你 shell 的 completion）
-   所有字段均可动态求值
    -   和 Emacs 优点一样，可以引用别的配置的值来注入或做判断：
        ```nix
        services.gitea.repositoryRoot = "${config.services.gitea.stateDir}/repositories";
        ```
    -   当然更高阶的控制流也不在话下：
        ```nix
        # home-manager/modules/manual.nix
        config = {
          home.packages = mkMerge [ # 也就是 flatMap
            (mkIf cfg.html.enable [ docs.manual.html docs.manual.htmlOpenTool ])
            (mkIf cfg.manpages.enable [ docs.manPages ])
            (mkIf cfg.json.enable [ docs.options.json ])
          ];
        };
        ```
-   几乎不需要执行 shell 拿系统信息，因为系统的状态被当前 Nix 配置树完全定义了，你要做的是找到定义这个信息的 key ，取它的 value 即可继续做操作。
    -   连网卡名字都可以先验地定义。参见：
        -   [networking.usePredictableInterfaceNames](https://search.nixos.org/options?channel=unstable&show=networking.usePredictableInterfaceNames&from=0&size=50&sort=relevance&type=packages&query=usePredictable)
        -   <https://mynixos.com/nixpkgs/option/networking.interfaces.%3Cname%3E.name>
        -   <https://discourse.nixos.org/t/network-interface-names-in-nixos-configuration/15200>
    -   分区也可以先验地定义。参见： <https://github.com/nix-community/disko>
-   作为保底手段，你依然还是可以自己提供全量的配置文件本身。

最后 Nix 把求值后的配置「DOM」给「渲染」成文本的配置文件，打包为一个
Derivation 后， 用类似 OverlayFS 的方式把它放进它该在的地方。

这才是 dotfile 管理该有的样子。未来你有信心无惧部署新机器。


### Docker / podman 容器 {#docker-podman-容器}

上面讲过了，由于“时光机”功能的缺失，在不同的时间点 `docker build` 两遍同样的 `Dockerfile` 可能会产出两个不同的镜像。除非像大公司那样严格控制上游 Base 镜像的版本，否则不算特别可控。一个能用的镜像二进制 `.tar` 有可能会成为传家宝。


### Ansible / Chef {#ansible-chef}

有点接近，但他们没有对“可复现”特别上心（比如他们没有严格限制被部署机器里软件包的版本），也就比纯 shell 脚本在可读性和可组合性上强一点。


### 带 `.lock` 的依赖管理器（ `bundler` / `cargo` / `pnpm` 等） {#带-dot-lock-的依赖管理器-bundler-cargo-pnpm-等}

想象一个比他们更加“语言无关”的包管理器，那就是 Nix 了。

事实上 Nix 确实可以（也已经）把其它程序语言的“包”纳入自己 Nixpkgs 的管辖范围内了，这个功能叫 Package Set。

{{< figure src="/ox-hugo/nix-package-set.png" >}}
