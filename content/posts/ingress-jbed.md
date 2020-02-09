---
title: ingress 在越狱 iOS 上的问题
categories: ["game"]
author: ["Nyk Ma"]
draft: false
date: 2014-07-21
---

## 症状

地图和背包里的东西都能正常读取，但无法与场景互动，包括但不限于：

* Hack
* 插 Resonator
* 使用 XMP
* 收集 XM

## 原因

根据 [reddit 上的一篇 po](http://www.reddit.com/r/Ingress/comments/2amzgv/jailbreak_causing_trouble_with_ios_ingress/) ，这是因为 Ingress.app 会检测系统的越狱状态。

## 一句话方案

把它关回监狱。

## 详细步骤

> 由于我使用日语系统，以下选项和按钮的中文名可能与你看到的不同。

1. 在 Cydia 上安装 tsProtector P （**付费插件**）， reSpring 。
2. 进入**设置** - **tsProtector P** ，将默认动作设置为「允许」（默认是「警告」），取消「在状态栏上显示」。

    > 状态栏提示会挡住经验条。

3. 进入「禁止应用」的列表，选中 **Ingress**
4. 退回上个界面，取消「每个应用一次」。
5. 重启手机。打开 Ingress 。玩。

## 后记

![Imgur](https://i.imgur.com/1hpRG5x.png)

如果你有兴趣，可以在 tsProtector P 中翻看 Ingress 的访问 log 。我不明白他要访问这些东西干什么。除了右边截图里的之外，还有一些：

* /Applications/WinterBoard.app
* /Applications/SBSettings.app
* /Applications/RockApp.app
* /Applications/MxTube.app
* /Applications/IntelliScreen.app
* /Applications/Iny.app
* /Applications/FakeCarrier.app
* /Applications/blackra1n.app
* /Applications/Cydia.app

声明一下以上这些除了 Cydia.app 和 FakeCarrier.app 其它我一个都没装过。Google 这是要搞越狱用户常用插件大统计？

有同学说可能是防止越狱设备伪造 GPS ，但既然机器都越狱了，这种程度的检测手段也是相当容易绕过的。连 root 的安卓设备上都挡不住伪造。
