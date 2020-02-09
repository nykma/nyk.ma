---
title: Gen8 磁盘 IO 卡顿问题及解决
categories: ["hardware"]
author: ["Nyk Ma"]
date: 2017-02-14
draft: false
---

## 环境

Gen8 乞丐版，跑着 HP 版的 ESXi 6.5，只有一个DSM6的虚拟机。

硬盘为一个2T + 一个4T的混合RAID，使用 `vmkfstools -z` 挂载进虚拟机。

## 问题描述

读取磁盘没有任何问题：无论读取速度多么快，系统其它响应均正常，ESXi里观测的磁盘IO最大延迟也正常（不超过500 ms）。

写入磁盘时，一旦写入量达到一个阈值（肉眼估计是100～200 MiB），所有IO请求立刻卡死，最大延迟飙升到至少20 s，HDD灯常亮，听声音发现磁盘实际不活跃。

耐心等的话，写入操作是可以正常结束的。

## 解决历程

首先确定不是硬件的问题：以前DSM是跑在实机上的，一切 as expected。

先尝试更改BIOS的SATA模式。HP服务器有三个模式可选：Legacy、AHCI和智能RAID。其中智能RAID在ESXi里根本看不到任何磁盘，Legacy和AHCI能看到磁盘，但磁盘IO问题都没有解决。

> 奇怪的是，公版的 ESXi 是能看到磁盘的，HP 定制版反而不能看见，什么鬼……

> 但公版ESXi依然存在磁盘IO的问题。

然后想到既然是增加了一层虚拟层后出现的问题， `vmkfstools -z` 又相当于一种「直通」，是不是因为CPU不支持虚拟化导致的。遂上 [ARK](http://ark.intel.com/products/71074/Intel-Celeron-Processor-G1610T-2M-Cache-2_30-GHz?q=G1610T) 找Datasheet，发现 VT-d 一栏一个大大的 **NO**。

……好吧好吧我花钱还不行么。上淘宝买了[E3-1265L V2](http://ark.intel.com/products/65728/Intel-Xeon-Processor-E3-1265L-v2-8M-Cache-2_50-GHz)，点亮后发现原来不能选的 ESXi 硬件直通菜单也能勾选硬件了，但磁盘IO问题依旧。

最后把BIOS的SATA模式更改成AHCI，一切解决。

![最后效果](https://i.imgur.com/zZjsMVA.png)

已经达到千兆以太网的理论传输极限了。無念。


## 结论

**要赚钱**。

解决后回过头去看那些Gen8的上手文，几乎都是一到手就把CPU换了的，所以没人遇到这问题。
