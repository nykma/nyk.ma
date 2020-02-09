---
title: MBP12,1 装 ArchLinux 的要点
categories: ["software"]
author: ["Nyk Ma"]
date: 2015-12-28
draft: false
---

## 0. 前言

> 本文年久失修，说不定本文提到的驱动问题在现在的内核已经解决了。尽管大胆地装吧。

首先，这是可行的，而且就最终效果而言还不错。如果你需要一个深度客制化的系统，还是很值得折腾一下的。

在看以下内容之前，先照着这两个 Wiki 做，本文只讲述这两个 Wiki 里没提到的东西：

- [Beginner's Guide （简体中文） - ArchWiki][1]
- [MacBook - ArchWiki][2]

本文不会涉及太多原理和细节，只给 solution。若有疑问请 [@bgm38][3] 或者自行拿错误信息去 Google。

> 商业系统真特么伟大，把这些琐碎事儿全给你解决了。

## 1. 安装

### 分区

在 OS X 下使用 `Disk Utility.app` 把当前 OS X 分区给「shrink」一块出来，可以先分个 exFAT。接下来照着 *Beginner's Guide* 做，用 `partd` 把这临时分区删了重新划，记得照 *MacBook* 里提到的那样给分区留一点间隙。

### 引导

**跳过教程里安装引导的部分**。安装流程结束后重启进 OS X，安装 [rEFInd][9]，后者会在每次开机时**自动扫描所有分区里可引导的操作系统**，无需任何配置。

## 2. 硬件

### 声卡

在 `/etc/modprobe.d/` 里随便新建一个什么名字的文件，写下这一行：

```conf
options snd_hda_intel index=1 model=intel-mac-auto
```

然后重启。进 `alsamixer` 把 `PCH` 声卡的 Master 取消静音（按`M`）。现在 `speaker-test` 应该会出声儿了。

> 默认声卡顺序把 hdmi 排在了前面，而你电脑上没插 hdmi，自然会报错了。所以你需要手动指定一下 index 把 pch 声卡排在前面。

### 无线网

看上去是开箱即用，但 Broadcom 的驱动有问题：用不了 5G 就算了，开机后第一次连接无线网是正常的，也一直可用，但如果你想切到另外一个网，问题就来了：

```bash
brcmfmac: brcmf_msgbuf_query_dcmd: Timeout on response for query command
brcmfmac: brcmf_cfg80211_get_station: Could not get rate (-52)
```

解决方案是替换掉原生的 firmware：

- [brcmfmac43602-pcie.bin](https://github.com/rsalveti/ubuntu-linux-firmware/blob/wily-brcmfmac/brcm/brcmfmac43602-pcie.bin)
- [brcmfmac43602-pcie.ap.bin](https://github.com/rsalveti/ubuntu-linux-firmware/blob/wily-brcmfmac/brcm/brcmfmac43602-pcie.ap.bin)

把这俩下载下来，覆盖 `/lib/firmware/brcm`里的同名文件，重启就能部分解决问题了。

> 参考： [Bug 100201 - brcmfmac: Can cause constant kernel oops / complete machine freezes (on MacbookPro12,1 2015 edition: Device 43ba 14e4:43ba (rev 01)) ][6]

### 自动连接无线网

> 参考： [自动切换配置 - netctl （简体中文） - ArchWiki][7]

### 键盘

刚装好时可能会把 \` 误认成 >，可以通过在 `/etc/modprobe.d` 里增加一个配置解决：

```bash
options hid_apple iso_layout=0
```

### 多功能键

关键道具是 `xev`（由 `xorg-xev` 包提供）和 `sxhkd` ，前者用来看 keysym，后者用来定义整个 X 范围内的快捷键，在这里就是给多功能键的 keysym 绑定对应的命令或脚本。

`sxhkd` 的配置文件在 `$XDG_CONFIG_HOME/sxhkd/sxhkdrc`，按上面的设置也就是 `~/.config/sxhkd/sxhkdrc`。配置文件的格式很简单：

```bash
# Comment
KEYSYM
  some_command_to_execute
```

然后在 `.xinitrc` 里启动这个 Daemon：

```bash
sxhkd &
```

#### 键盘背光

关键道具是 `kbdlight`，一个封装好的工具省得你 `echo` 一个数字给 `/sys` 了。

```bash
XF86KbdBrightnessDown
  kbdlight down
XF86KbdBrightnessUp
  kbdlight up
```

> `kbdlight` 的功能还挺全的，你另外可以写个脚本让背光“平滑地”变化。

#### 显示器亮度

```bash
XF86MonBrightnessUp
  xbacklight -inc 5
XF86MonBrightnessDown
  xbacklight -dec 5
```

#### 音量控制

```bash
XF86AudioRaiseVolume
  amixer set Master 5%+
XF86AudioLowerVolume
  amixer set Master 5%-
XF86AudioMute
  amixer set Master toggle
```

#### 音乐控制

前提是你用 `mpd` 播放音乐，应该没人不是吧。

```bash
XF86AudioPrev
  mpc prev
XF86AudioNext
  mpc next
XF86AudioPlay
  mpc toggle
```

#### Launchpad 键 和 Exposé 键

自由发挥吧。比如可以把它映射成 `rofi` 嘛：

```bash
# Exposé （F3）
XF86LaunchA
  some_command_to_execute

# Launchpad（F4）
XF86LaunchB
  rofi -show run
```

### 自动调节显示器亮度

Arch wiki 里有一个用 perl 写的脚本，其实原理很简单：

- 读光感计的读数
- 计算目标亮度
- 平滑地增加亮度

其中使用 `xbacklight` 调整亮度已经是平滑的了，那前两个目标也不难了，自己发挥吧，拿脚本语言撸一个。

## 3. 软件

使用 `yaourt` 代替 `pacman`，前者是后者功能的超集。

### X11

Retina 倒是不难解决：

```bash
! ~/.Xresources
Xft.dpi: 192
```

然后就是双显示器怎么办了。以前我的显示器是一台 DELL U2412M，插在笔记本的 DP1 口上，内置和外置的两个显示器的 DPI 不一样。我尝试过以下这句：

```bash
xrandr --output eDP1 --auto --output DP1 --auto --scale=2x2 --left-of eDP1 --output DP2 --off
```

虽然两边 DPI 是正常了，但工作区的大小不正常了。具体来说，鼠标在外置显示器上无法移动到画面边缘。

目前我把内置显示器关了，只使用两台 U2412M 工作，绕过了 HiDPI 问题，感觉也不错：

```bash
xrandr --output DP1 --auto --output DP2 --auto --left-of DP1 --output eDP1 --off
```

也研究过一圈 Wayland ，现有的 Wayland composer 有两个：

- `wlc`，它还不支持多显示器多DPI
- `weston`，它是支持，但没有以它为基础开发的 i3-like WM

综上，等 `wlc` 完善了再用 `sway` 吧……

> 开发 `wlc`、`sway` 和 `orbment` 的家伙都是死宅的感觉……

> 参考： [HiDPI - ArchWiki][5] 、 [Multihead - ArchWiki][8]

### 字体

- 查看所有可用字体列表： `fc-list`
- 刷新字体缓存： `fc-cache`

字体路径在 `fc-list` 里也写了，安装字体就是把你自己的字体拷贝到对应位置，再刷一下缓存就行。

`~/.Xresources` 里可配置的字体，如果带了 `xft:` 前缀就表示这是个 TTF 或者 OTF。写在这儿的字体名称必须要是 `fc-list` 里出现的那个。

> `urxvt` 支持字体回滚，例如： `xft:英文字体名:size=12, xft:中文字体名:size=12` 。
>
> 但就我实验看来，文泉驿微米黑成功了，但思源黑却怎么都没生效，怀疑也和字体有关。孙志贵修改过的“根源黑体”倒是可以用的。
>
> 我的 `urxvt` 字体配置：

```bash
URxvt.font: xft:Monoid HalfTight:style=Retina:size=10, xft:根源黑体 UI Regular:style=Regular:size=12, xft:Icons:size=12
URxvt.boldFont: xft:Monoid HalfTight:style=Bold:size=10, xft:根源黑体 UI:style=Bold:size=12, xft:Icons:size=12
URxvt.italicFont: xft:Monoid HalfTight:style=Italic:size=10, xft:根源黑体 UI:style=Light:size=12, xft:Icons:size=12
! Icons 字体由 ttf-font-icons 提供，将所有图标符号集合于一体。
```

### X11 字体回滚顺序

首先你需要手动定义一下 `XDG_CONFIG_HOME` 在哪儿：

```bash
# ~/.profile
export XDG_CONFIG_HOME="$HOME/.config"
```

然后新建一个 `~/.config/fontconfig/fonts.conf` ：

```xml
<?xml version="1.0"?>
<!DOCTYPE fontconfig SYSTEM "fonts.dtd">
<fontconfig>
  <alias>
    <family>monospace</family>
      <prefer>
        <family>Monoid HalfTight</family>
        <family>Source Han Sans SC</family>
      </prefer>
  </alias>
  <alias>
    <family>sans-serif</family>
      <prefer>
        <family>Fira Sans</family>
        <family>Source Han Sans SC</family>
      </prefer>
  </alias>
</fontconfig>
```

[1]:	https://wiki.archlinux.org/index.php/Beginners'_guide_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87) "Beginners'guide （简体中文）"
[2]:	https://wiki.archlinux.org/index.php/MacBook "MacBook"
[3]:	https://twitter.com/bgm38 "@bgm38"
[4]:	https://twitter.com/7NIaL "@7NIaL"
[5]:	https://wiki.archlinux.org/index.php/HiDPI "HiDPI"
[6]:  https://bugzilla.kernel.org/show_bug.cgi?id=100201#c65
[7]:  https://wiki.archlinux.org/index.php/Netctl_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87)#.E8.87.AA.E5.8A.A8.E5.88.87.E6.8D.A2.E9.85.8D.E7.BD.AE
[8]:  https://wiki.archlinux.org/index.php/Multihead
[9]:  http://www.rodsbooks.com/refind/
