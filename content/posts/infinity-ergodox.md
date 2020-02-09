---
title: Infinity Ergodox
author: ["Nyk Ma"]
categories: ["hardware"]
date: 2016-02-09
---

![Keyboard layout](https://i.imgur.com/PQWDCwa.png)

## 款式及购买

目前Ergodox有三种，PCB和外壳方案都是开源的：

- [ErgoDox](http://ergodox.org/)：元初版。PCB你得自己印，大小物料你得自己买自己焊，外壳你得自己3D打印。MassDrop曾经[开过一次团](https://www.massdrop.com/buy/ergodox)，不过没要开第二次的意思。
- [ErgoDox EZ](https://www.indiegogo.com/projects/ergodox-ez-an-incredible-mechanical-keyboard)：Indiegogo众筹版，找台湾某机械键盘工坊量产的，无需组装还有售后。并且他们改良了外壳的模并添加了一个无级调整高度的垫脚。早结束了别想了。
- [Infinity ErgoDox](https://www.massdrop.com/buy/infinity-ergodox)：[Input:Club](http://input.club)社区威力加强版，外壳PCB固件全部重新设计。提供贴片元件焊接好了的PCB和外壳，你需要自己焊轴、焊灯、组装外壳。开了两次团，也结束了。

我定了 Infinity Ergodox（以下简称IC版），160131结束挣扎下了单，不过要4月底才发货，最快五月才能送到我手上……囧。

如果你想买，最好的选择是[留下你的邮箱](http://input.club/devices/infinity-ergodox)，等下一次 Infinity ErgoDox 开团。

## 款式间区别

Indiegogo版直接使用的元初PCB来量产的。所以区别主要是元初版和IC版的。

- 对键位排布和可定制性来说，二者没有区别，都可以固件级定制。见下。

- 两个版本最大的区别是，IC 版的两个part可以**分别单独**插电脑，元初版只能右手侧。

    > 因为IC版的两个part里分别有一个主控而元初版只有右手侧有主控。
    >
    > 所以IC版的固件要左手右手刷两次。

    > 当你需要一个比Numpad强大得多的左手键盘时，IC版非常有吸引力。还记得十几年前流行过的CS/WoW专用单手游戏键盘么？

- 两个parts互相联通时，IC版的part间通信延迟[可以忽略不计](https://www.massdrop.com/buy/infinity-ergodox/talk/352332)。

    > 已经比键盘和电脑的USB通信频率还高了，所以完全不用担心游戏时slave part的按键时延。

    > 这也是为啥IC版的part间使用 USB 3.0 micro-B 线连接的原因（虽然上面跑的不是USB协议…）。元初版用的是TRRS音频线。
    >
    > 也就是说，IC版只需要一种线：USB3 A公 - USB3 micro-B公

    > 展开说下。IC版的每个part各有一个USB 3.0 A母和一个 USB 3.0 micro-B母。
    >
    > - 当micro-B插电脑时， it acts like a single input device；
    > - 当micro-B连了另一个键盘的A时，说明两个键盘级联工作。此时空出来的一个A母[可以用来充电](https://www.keychatter.com/2015/09/30/interview-part-3-of-3-jacob-alexander-haata-talks-about-future-input-club-plans-ergodox-infinity-development-and-more/)；
    > - 当A连电脑……八成没反应吧。
    >
    > 你固然可以把两个部分的micro-B分别连上电脑，不过这样两个部分就互相独立了，跨part的修饰键可能会失效。

    > 看[I:C的帖子](http://input.club/forums/topic/infinity-ergodox-update)，这键盘居然可以DaisyChain N多个……看来不仅双手，连双脚也可以用来写程序了。
    >
    > 此时 **part** 可以称为 **node** 了。无限的可能性，无愧于Infinity这个名字。

- IC版有背光灯，元初版没有。

    > 准确地说，元初版只有几个状态指示灯。

    > IC版的背光是单色的，没有RGB通道。

    > IC版有单灯珠控制，256级亮度，LED驱动芯片强大到[能以60fps的速度扫描每个灯珠](https://www.keychatter.com/2015/09/30/interview-part-3-of-3-jacob-alexander-haata-talks-about-future-input-club-plans-ergodox-infinity-development-and-more/)。
    >
    > 冲这个我觉得多焊个LED不亏。

- IC有两个带背光的点阵LCD屏。

    > 分辨率 128x32

    > 这背光[是RGB的](http://input.club/forums/topic/infinity-ergodox-update)。
    >
    > 对原厂固件而言，背光颜色代表不同的Layer。每个Layer有自己的主题色，在网页配置界面里有。
    >
    > 讲真，这feature几乎完爆市面上所有能定制Layer的键盘了。

    > 甚至可以[电脑端修改屏幕内容和背光颜色](https://www.keychatter.com/2015/09/30/interview-part-3-of-3-jacob-alexander-haata-talks-about-future-input-club-plans-ergodox-infinity-development-and-more/)！

    > 记得上面说的 **node** 么？不仅按键事件，连屏幕内容和背光状态也是跨node共享的。
    >
    > 写到这儿我后悔只买一套键盘了……早知道买两套了

## BoM

对IC版，在动手焊接之前你应该准备好以下这些东西：

- 焊接好片上元件的 PCB
- 分层塑料壳
- 铝制开关定位板
- 两根 USB 3.0 线
- 76 个键帽，其中
    - 60个 1u
    - 12个 1.5u
    - 4个 2u（拇指区域，带平衡轴的四个大键）
- 76 个开关
- 平衡轴、螺丝螺母若干

### 关于键帽

我选择了DSA键帽，因为它是最容易配的，只要考虑长度就行了。

如果你不习惯纯平的profile，还是选SA或者DCS吧。高度可以参考 [Pulse SA Keycap set](https://www.massdrop.com/buy/pulse-sa-keycap-set) 里 Ergodox Kit 套餐示意图的配备。

## 焊接、组装

> TODO

## 编、写固件

截至目前，算得上可用的IC版固件有两套：

### [kiibohd/controller](https://github.com/kiibohd/controller)

官方固件，没什么好说的。

最大的优点是有一个非常易用的可视化 [Keymap editor](http://configurator.input.club/?layout=MDErgo1-Default)，对轻中度用户来说绰绰有余。

### [fredizzimo/infinity_ergodox](https://github.com/fredizzimo/infinity_ergodox/)

它可以做到所有 [tmk_keyboard](https://github.com/tmk/tmk_keyboard) 能做到的事情：

- 非常灵活的层跳转策略
- 高度可定制的宏
- 能任意改写屏幕文字和背光颜色的helper

截至目前(20160513)它相对于官方固件的缺陷是：

- 没实现node平等。

    > 具体地说，只能左手为master与电脑通信，且不支持多于2块键盘级联。
    > 好消息是，作者正在给整个 `tmk_core` [增加 multiple drivers 的支持](https://github.com/tmk/tmk_core/pull/19)，而且似乎进展不错
