+++
title = "Nix 实战：来几个 use case"
author = ["Nyk Ma"]
date = 2024-12-11T01:06:00+08:00
lastmod = 2024-12-11T01:06:11+08:00
tags = ["nix"]
categories = ["software"]
draft = false
+++

距离我[上篇介绍 Nix 的文章](https://nyk.ma/posts/nix-and-nixos/)有一段时间了，积累了不少 use case 和常见问题，这里总结成文。

{{< admonition tip "一些小建议" >}}
强烈建议没看过上文的 Nix 新用户先去看一眼，否则会跟不上。

另外强烈建议先学一下 [nix 基础语法](https://nixos-cn.org/tutorials/lang/QuickOverview.html)，下面不赘述语法的部分。

语法上它很像 OCaml。
{{< /admonition >}}


## 再详解一些概念 {#再详解一些概念}


### Flake {#flake}

Flake 已经几乎成为 Nix 世界的 de facto 了。其结构大致如下：

```nix
{ # 只列举最常用的字段
  description = "这个项目的文字描述";
  inputs = rec {
    # 所有外部输入。除了这些外部输入外，接下来的所有构建都会被切断网络通信。
    # 由此可以保证最大限度的可复现性（aka 函数式）
    # 外部输入可以是 git repo 或网络文件。
    nixpkgs-master.url = "github:nixos/nixpkgs/master";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    # rec 关键字让你可以在同一个结构体内 ref 自己的字段。
    # 我喜欢用这种方式快速切软件源版本
    nixpkgs = nixpkgs-unstable;
  };
  outputs = {nixpkgs, ...} @ args: {
    # 这里是这个 nix 项目的构建定义。照着这里的流程做就能 build 出成果。
    # 注意 outputs 是一个函数，输入是所有 input 组成的 struct，被解构
    # 赋值了，所以这里能拿到 nixpkgs 字段作为传入变量。没被匹配的字段
    # 就放在 args 变量里了。比如 args.nixpkgs-master.xxxxxx

    # nix fmt 命令会使用这个字段指定的 formatter 来 format Nix 代码。
    # 如下是一个常见的例子：
    formatter = { "x86_64-linux" = nixpkgs.legacyPackages.x86_64-linux.nixpkgs-fmt; };
    # 所有 overlay 。Overlay 是现场修改 input 内容物的方式，最常见的是
    # 修改或者新增某个软件包的定义（而不用 fork 一份 nixpkgs），接下来
    # 使用 input 的部分可以完全无感地继续该怎么用怎么用。
    overlays = { ... };

    # 对于软件项目，下面的字段比较常用：

    # 所有软件包的定义。 nix build 和 nix shell 命令会用这里的定义来build 软件包。
    # 关于如何编写你的软件的 derivation，请翻阅文档或随便在 nixpkgs 里
    # 找一个软件包定义。
    # 下面这个软件可以用 nix build .#software-name 构建。成果会放在
    # ./result 里。
    packages = { x86_64-linux.software-name = myDerivation; };
    # 如果 nix build 命令没有附加包名参数，则此 derivation 会被构建：
    defaultPackage = { x86_64-linux = myDerivation; };
    # nix flake check 命令会执行下面 derivation 定义里的测试流程。
    checks = {  x86_64-linux.software-name = myDerivation; };

    # 开发环境。比如下面一个很简单的例子。
    # 在此项目根目录使用 nix shell 命令可以把你带入这个预装好环境的 shell 中。
    # 你自己项目外的个性化设置（shell、配色、alias、缩写等）不会被覆盖。
    # 不难注意到，你可以定义很多套 shell
    devShells = {
      x86_64-linux.default = nixpkgs.legacyPackages.x86_64-linux.mkShell {
        name = "my-software-dev";
        packages = with nixpkgs.legacyPackages.x86_64-linux; [ nodejs yarn nodePackages.prettier just ];
      };
    };

    # 对于 NixOS 系统构建，下面的字段比较常用：

    # nixos-rebuild 会看这个定义
    # home-manager 的设置会作为它的 module 而被注入
    # my-computer 是电脑的 hostname
    # nixos-rebuild switch 命令若不指定这个名字，则以本机的 hostname 作为 key 在这里查找
    nixosConfigurations = { my-computer = systemDerivation; };
    # nix-darwin 会看这个定义
    # home-manager 也可以注入它
    # 字段名同样是 hostname
    darwinConfigurations = { my-mac = darwinSystemDerivation; };

    # 还有其它的第三方工具会在这里找自己感兴趣的字段
    # 比如 deploy-rs 会要求这么定义：
    deploy = {
      nodes.my-computer = { ... };
    };
  }; # outputs
}
```


### nixpkgs module {#nixpkgs-module}

nixpkgs module 定义了一种函数的输入和输出结构。用在所有和 nixpkgs 有关的系统定义里。它也是会被下文 `imports` 自动 call 的结构。具体参见[中文教程](https://nixos-and-flakes.thiscute.world/zh/nixos-with-flakes/nixos-flake-and-module-system)。


### 经常看到 `pkgs` ，它是啥 {#经常看到-pkgs-它是啥}

我们写 NixOS 配置时，习惯上会这么做：

```nix
{ nixpkgs, ... }: {
  someConfig =
    let
      # 提前把自己系统的 packages 列表做个别名
      pkgs = nixpkgs.legacyPackages."x86_64-linux";
    in
    {
      environment.systemPackages = with pkgs; [ btop ]; # 这样就不用写一大堆了
    }
}
```

之后你看到别人配置里的 `pkgs` ，你就知道它是 `nixpkgs.legacyPackages.${system}` 的别名了。

我也推荐你自己遵照这个约定俗成。


### 相对路径、 `import` 关键字和 `imports` 字段的区别 {#相对路径-import-关键字和-imports-字段的区别}

-   一个文件或目录的相对路径会在 nix 被求值时被拷贝进求值环境，并在此处留下一个绝对路径的字符串（形如 `/nix/store/xxxxxx` ）。这是 Nix 语法那一层的功能。
    ```nix
    # 我们来 build 一个别人写的 C++ 软件包：
    # 这是一个很经典的 legacyPackage 的定义
    { lib, fetchgit, clangStdenv, openssl, ... }:
    clangStdenv.mkDerivation {
      pname = "my-cpp-program";
      version = "0.0.1";
      src = fetchgit {
        url = "https://github.com/nykma/test.git";
        rev = "abcdef123456...";
        hash = "sha256-xxxxxx";
      };
      buildInputs = [ openssl ];
      # 本地测试时我们发现需要给几个源文件打 patch ，否则在 nix 环境下编译不通过
      # 如果我们不想提交这个 patch 至上游，可以这么做：
      patches = [
        ./nixify.patch
        # 这就是一个相对路径的文件，它会在被求值时拷贝进 nix 的环境并被 hash
        # 同时，这个文件必须被 git add 进 staged files 里才能工作，否则会报错文件不存在。
        # 为什么 Nix 不支持引入绝对路径的文件呢？
        # 因为绝对路径定义的文件没法保证在别人的电脑里也存在，也就会失去可复现的优势。
      ];
    }
    ```

-   `import` 是 Nix 语法那一层的概念。其对象只能是 `.nix` 文件或一个文件夹，效果是让内容物好似就写在 `import` 所处位置一样。
    ```nix
    # ./a.nix
    "AAA"

    # ./b.nix
    "BBB"

    # ./c/default.nix
    # 注意这里定义的是一个函数
    input: "CCC ${input}"

    # ./main.nix
    let
      a = import ./a.nix;
      # 上面等效于
      # a = "AAA";
      # 就好似 a.nix 文件的内容写在 import 所处的那个位置一样
      # 记住这个「好似内容写在这里一样」，能帮你节省很多绕弯子的时间
      b = import ./b.nix;
      # import 对象为一个文件夹时，指的是 import 下面的 default.nix 文件
      c = import ./c;
      # 上面等效于：
      # c = input: "CCC ${input}";
    in {
      result = a ++ b ++ (c "wow"); # c 是个函数，所以要 call
    }
    # 对上面求值的结果：
    {
      result = "AAABBBCCC wow";
    }
    ```

-   `imports` 字段为一个 list ，该字段是 nixpkgs module 层级的概念。对其所有 item 求值（若该 item 为函数，则使用当前的 nixpkgs module 环境
    call 它），并将最后结果 ****合并**** 于同结构的对应字段内。
    ```nix
    # ./modules/a.nix
    {
      config.users.users.nyk.isNormalUser = true;
    }
    # ./modules/b.nix
    {
      config.users.users.nyk.home = "/home/NyK";
    }
    # ./modules/c.nix
    # 注意：该文件的顶层不是 struct ，而是一个函数了
    { pkgs, ... }:
    {
      config.users.users.nyk.shell = pkgs.fish;
    }
    # ./main.nix
    { pkgs, ... }: {
      # 为什么 pkgs 需要从外部传入？
      # 因为 imports 列表里的 c 的定义要求这个入参存在
      # imports 将其当前环境内的 pkgs 再传入 c 作为 c 的环境
      imports = [
        # 想象上面说的「好似内容写在这里一样」
        import ./a.nix
        import ./b.nix
        # 注意这个函数被 imports 自动 call 了，并拿到了它的返回值
        import ./c.nix
        # 显然下面一行和 import 是等价的
        { config.users.users.nyk.homeMode = "755"; }
      ];
    }
    # 求值的结果：
    {
      config.users.users.nyk = {
        isNormalUser = true;
        home = "/home/NyK";
        homeMode = "755";
        shell = «derivation /nix/store/c369bf8iifx7ibflwhab98i528mb5gin-fish-3.7.1.drv»;
      };
    }
    ```

{{< admonition question "如果字段重复了怎么办？" >}}
假设 `a.nix` 和 `b.nix` 都定义了 `isNormalUser = ???` ，但一个是 `true` 一个是 `false` 。此时 nix 求值时会报错。

通过设定这些值的优先级可以解决。具体参见 [lib.mkOverride](https://github.com/NixOS/nixpkgs/blob/9dfcba812aa0f4dc374acfe0600d591885f4e274/lib/modules.nix#L646) 。TLDR：优先级数字越小，优先级越高。
{{< /admonition >}}

{{< admonition question "我想在 imports 一个模块时临时外部传入几个参数怎么办？" >}}
假设有个 home manager 的 module ：

```nix
# ./modules/home/x11.nix
#             vvvvvvv 指定参数默认值
{ pkgs, hidpi ? false, ... }:
let
  actualDPI = if hidpi then 144 else 96;
in
{
  home.packages = with pkgs; [ xclip ];
  xresources.properties = { # 修改 ~/.Xresources
    "*.dpi" = actualDPI;
  }
}
```

使用时，我们希望把 `hidpi` 指定为 `true` ：

```nix
# ./home/my-computer.nix
{ pkgs, ... }: # 注意这个外部环境没有 hidpi 这个变量的传入
{
  imports = [
    # 这显然是不行的，值会是默认值 false
    import ./modules/home/x11.nix;
    # 我们不如别让 imports 来 call 这个函数，我们自己来 call
    (import ./modules/home/x11.nix { pkgs = pkgs; hidpi = true; })
    # 再复习一遍「好似内容写在这里一样」。好似在这里定义了一个函数。
    # 再多使用点 nix 的语法糖的话：
    (import ./modules/home/x11.nix { inherit pkgs; hidpi = true; })
    # 有点蠢，但保证能用。如果你想要更工程化的解法，看下面一章，定义你自己的 options
  ];
}
```

你理解了这个例子后，应该有能力自行组合 `imports` 和 `import` 来模块化你的 nixos 配置了。
{{< /admonition >}}


### `config` 和 `options` 的区别 {#config-和-options-的区别}

这是 nixpkgs module 层级的概念。

写 NixOS 配置的时候我们经常会使用诸如
`config.networking.useDHCP=true;` 之类的字段来定义自己的系统状态。

你应该遇到过，写错 `config` 下面的字段会报错「找不到字段定义」之类的。

定义哪些字段有哪些字段没有这件事儿就是在 `options` 字段里做的。

我们也来定义一个自己的模块，通过 `options` 来让模块的调用者动态地决定字段的值：

```nix
# /etc/nixos/modules/my_user.nix
{ lib, pkgs, config, ... }:
let
  # 省得下面打字了
  # 和上面 pkgs 一样，这个 cfg 也是一种约定俗成
  cfg = config.nyk.user;
in
{
  options.nyk.user = {
    # 对于非 bool 的字段，详见 lib.mkOption 的函数定义
    enable = lib.mkEnableOption "Whether to enable user:nyk definition";
  };
  config = lib.mkIf cfg.enable { # 只有当 config.nyk.user.enable = true 时整段定义才会出现
    users.users.nyk = { # 常规的 nixos 配置
      isNormalUser = true;
      shell = pkgs.fish;
    };
  };
}
```

使用它的场景：

```nix
# /etc/nixos/configuration.nix
{
  imports = [
    # 先把模块定义文件给注入进我们的环境
    ./modules/my_user.nix
  ];

  config = {
    nyk = {
      user.enable = true; # 这样上面的 config 注入就能生效了
    };
  };
}
```

所以你在配置 nixos 时写的那些 `config` 都是已经在 `nixpkgs` repo 里被
`options` 定义过了的。具体可以在 [mynixos](https://mynixos.com/nixpkgs/option/networking.firewall.allowPing) 里搜索字段，就能在详情里看到定义的位置。

{{< admonition tips "config 有时是可以省略的" >}}
```nix
{ pkgs, ... }: {
  imports = [
    import ./modules/a.nix
  ];

  users.users.nyk.shell = pkgs.fish;
}
```

上面和下面是等价的：

```nix
{ pkgs, ... }: {
  imports = [
    import ./modules/a.nix
  ];

  config.users.users.nyk.shell = pkgs.fish
}
```

你可以粗糙地认为，若该文件里没有 `options` 顶层字段出现，则所有不是
`imports` 字段的字段都会被归于 `config` 字段下。

但请注意，一旦本文件中 `config` 顶层字段出现了一次，则所有该归于
`config` 下的配置就都不能省略了。
{{< /admonition >}}


## Nix 的优势场景 {#nix-的优势场景}


### 我不要的软件，别出现在我的 `$PATH` 里 {#我不要的软件-别出现在我的-path-里}

试想，你想用 `glacnces` ，然后你开开心心地 `pacman -S glances` ，装好了。

过了三个月，你发现 `$PATH` 里有 `python3` ，而你却完全想不起何时为啥装的了……

Nix 就不会有这样的问题：你要什么软件，就给你 expose 什么程序，其它的依赖会构成有完整层级的树（而非平级或一级）。

```shell
# 用 environment.systemPackages 装，
# 或者在 devShell 里使用也是一样的效果。
# 新建一个有 glances 的临时环境
$ nix shell nixpkgs#glances
$ which glances
/nix/store/xxxxxxx-glances-4.2.0/bin/glances
$ which python3
which: no python3 in (........)
$ exit # 退出那个临时环境
$ which glances
which: no glances in (........)
$ which python3
which: no python3 in (........)

# 以下是在 nix 里试用一个软件最快的方法：
$ nix run nixpkgs#glances
# 退出软件后
$
# 到这里就完全没有这个软件存在过的痕迹了
# 它可能还会在 /nix/store 里留存一段时间，但最终会被 GC 掉
$ which glances
which: no glances in (........)
```

当然，以上用 `environment.systemPackages` 装，或者在 `devShell` 里使用也是一样的效果。

好处？自己依赖自己的依赖，确保对环境（和其它软件包）的影响压缩在最小范围内。

-   我的电脑里同时有 20、22 和 23 的 `nodejs` ，就是不同的软件包依赖不同大版本的结果。每个软件都能开心地用自己版本的依赖，同时我完全无
    `which node` 没结果。
-   每个工作的项目都有自己的依赖，我有几个 C++ 工程依赖 `openssl` 1.1，完全不影响系统中的其它应用使用 `openssl` 3，他们互相不知道对方的存在。

{{< admonition question "这样不会占硬盘？" >}}
-   使用同一个 `python312` 作为依赖的两个不同程序在整个系统被求值（build）的时候，自然会引用到同一个 `python312` 的 derivation 实例上去，实际上不会有多余的硬盘占用。
-   如果上述 `nodejs` 的例子里系统只允许装一个版本的话，就可能会有软件运行不正常了。在不正常和占硬盘之间选哪个不用说了吧。
-   和三四十年前相比，硬盘已经完全不值钱了。现代语言（尤其是 go）已经标配静态链接了，我觉得很对。除非出于商业角度卖私有库的考虑，否则你也最好拥抱静态链接。
{{< /admonition >}}


## FAQ {#faq}


### 如何 debug ？ {#如何-debug}

对 Flake 而言，有 `nix repl` 命令可以让你进入某个名字空间内：

```shell
# 进入 flake.nix 所在的目录
$ pwd
/etc/nixos
# 让你能进入当前系统 config 求值完之后的树内
$ nix --extra-experimental-features repl-flake repl ".#nixosConfigurations.$(hostname)"
nix-repl> config.networking.enableIPv6 # options 当然也能看到
true
nix-repl> config.nix.settings.substituters
[
  "https://mirrors.ustc.edu.cn/nix-channels/store"
  "https://cache.nixos.org/"
]
nix-repl> pkgs.cloudflared.version
"2024.11.0"
```
