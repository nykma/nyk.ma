+++
title = "IIDX 好弟弟排错指南"
author = ["Nyk Ma"]
date = 2023-06-09T16:44:00+08:00
lastmod = 2023-11-21T22:30:27+08:00
categories = ["game"]
draft = false
+++

## 前言 {#前言}

本文会大量使用黑话，因为整个行业本身处于灰色地带。请窒息。

另，不提供任何资源下载，原因同上，请窒息。

> 要想体验 7 键 + 转盘生态，你永远有 BMS 社区这个选项。不追求一比一还原
> Arcade 的话，对于纯练习技术而言完全完全足够了。

<!--quoteend-->

> 以下流程应该至少能适用 26 - 30 版本。


## 通用准备 {#通用准备}

> <span class="timestamp-wrapper"><span class="timestamp">&lt;2023-06-09 五&gt; </span></span> 更新：有个后缀为 `2x` 的小辣椒工具，完美支持目前还在
> N+0 的 30 。另外它为 WASAPI 设置了一些默认参数，能省去你很多手动设置和排错的时间。强烈建议使用。 以下所有准备、安装和排错描述均基于 `2x` 版的小辣椒。


### 显示器 {#显示器}

如果你需要玩雷霆大寺（ `TDJ` 模式），请注意选用一个支持 720p /
1080p（对 30 而言） @ 120Hz 的显示器。

如果你还想搞雷霆大寺触摸副屏，注意这个副屏也得是 120Hz 的。


### 声卡 {#声卡}

`2x` 版小辣椒默认使用 WASAPI，已经不怎么挑声卡了。


### 游戏本体 {#游戏本体}

提示： `nyaa.si` 的 Software 板偶尔有放出。


### 小辣椒 {#小辣椒}

如上所说，在 `2x` 版小辣椒的官方 Github repo 里下个最新的 release。


## 排错 {#排错}

首先的首先，请一定要看 log。

Log 文件在小辣椒程序同目录下，每次启动都会被覆盖。所以建议先备份 log
再问人 / 排错。

在 Log 内搜索 `E:` ，代表 Error 有发生。一般进不去游戏的 coredump 信息出现前，一定会有个地方出现 `E:` 。通过观察这个 Error 的模块名你就能清晰地定位出错的模块是图形部分还是音频部分。

> 一般来说 Warning （ `W:` ）是可以忽略的，不影响正常启动流程，不过也偶有例外。


### 闪退类（没看到游戏画面） {#闪退类-没看到游戏画面}


#### `ACCESS_VIOLATION` 类 {#access-violation-类}

个人经历，这种问题十有八九是老版本辣椒没有正确设置声音设备，
DirectAudio 初始化失败导致设备指针为空，再被 `bm2dx.dll` 里的逻辑强行访问所致。

新版辣椒默认使用 WASAPI，理论上说不需要任何额外配置就能正确初始化音频设备了。如果你还是遇到了这个问题，在 Log 里仔细翻翻音频设备枚举的部分里有没有 `failed` 之类的字样。


#### Omnibus + `9on12` 导致的 {#omnibus-plus-9on12-导致的}

我的 Omnibus 使用了[文件系统注入](https://github.com/mon/ifs_layeredfs)，它使用 `d3d9.dll` 作为注入点，然而我同时选了 `config.exe` 里的 `DX9on12` ，这两个注入冲突了。好在没多久就找到原因了，也是看 log 找到的。


### 显示类 {#显示类}

首先按照[这个教程](https://iidx.org/infinitas_pc#recommended-settings)调整 `64.exe` 的 nVidia 控制面板设置。


#### 自检不通过 {#自检不通过}

一般是刷新率不对，比如 `TDJ` 模式自检时刷新率不是 `120.0000` ，或者是
`LDJ` 模式刷新率过高。先 apply 上面的显卡设置，还是不行就检查自己显示器硬件菜单里的当前分辨率和刷新率。

还有，如果你用窗口模式使用 `TDJ` ，也需要保证整个桌面的刷新率是
120Hz。

不要用辣椒 config 里的自己限制自己帧数功能，会影响游戏内判定。


#### `TDJ` 自检过了，但感觉还是 60Hz 刷新 {#tdj-自检过了-但感觉还是-60hz-刷新}

一年前我在玩 28，买了个 144Hz 高刷显示器，发现 `TDJ` 自检确实是 120Hz，但显示器硬件菜单告诉我当前刷新率是 60Hz。

我另有一台原生 120Hz 显示器是正常的，进游戏后就是 120Hz。

我仔细翻看了下显卡设置里的显示器设置页，发现显示器汇报的“分辨率/刷新率支持列表”里没有 `720p@120Hz` 。

所以我突发奇想，手动加了 `720p@120Hz` 进分辨率列表。并且我先在系统桌面里切到 `720p@120Hz` ，然后再启动游戏。这回不仅自检通过了，而且显示器也是 120Hz 了。

事后分析，因为显示器支持 G-Sync，所以它不用汇报一长串支持列表了。可能我对 `64.exe` 的显卡单独设置不对，导致它没有给显示器正确的 G-Sync 切换信号……？


### 音频类 {#音频类}


#### key 音和 BGM 有越来越大的时差 {#key-音和-bgm-有越来越大的时差}

-   刚进一首曲子是好的
-   打着打着这首曲子的 note 和背景声音差越来越远，一般是背景音越来越超前于 note 。
-   下一首曲子开始时又好了。
-   整体时好时不好，有时一首从头打到尾都没事，有时连着五六首都滞后。

一般和两个有关：垂直同步，系统整体负载。

除了按照上述教程设置 NVidia 控制面板的调整项外，缓解方法只有尽可能关掉后台程序（尤其是杀毒软件）。如果还是不行，你可能需要安装一个干净的
windows（比如 embedded 版或 LTSC 版）。

街机游戏设计处于非常可控的软硬件环境下，因此背景声和 note 的下落没有（也没必要）设计同步机制，背景音乐是自顾自地播，不会“等” note。因此如果后台有进程让 note 的下落卡了几下，比如 120 帧里卡了 5 帧，在 `TDJ`
里玩时肉眼没感觉，但一分钟后你就能听到四分之一秒的时差了，对这种判定精确到帧的游戏而言，十分难受。

追求极致还原的玩家一般都是专机专用的，还要装最精简版本的 windows ，和
Arcade 的环境一致。

> 辣椒设置里有个高精度时钟的选项，可以死马当活马医地试试，一般效果不大。

<!--quoteend-->

> 其实还有个解决方法：玩 INFINITAS。它是为家庭操作系统复杂的运行环境所设计的（