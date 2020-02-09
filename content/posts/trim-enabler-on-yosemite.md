---
title: Trim Enabler 在 Yosemite 上的适配问题
categories: ["software"]
draft: false
author: ["Nyk Ma"]
date: 2014-08-06
---

> 本文年久失修。不说现代 SSD 已经自带主控级 TRIM 了，甚至连 SATA 接口都被 NVMe 淘汰了，因此今天本文没有任何实用价值了。当历史看吧。

# 前言

简而言之，SSD 的性能是会随着已使用容量的变化而改变的，Trim 功能可以让操作系统软件和 SSD 硬件通力合作，尽可能保持新盘的读写效率。

然后，众所周知，Apple 的生态圈是出了名的封闭。比如 Trim 这个 SSD 必备的 feature，OS X 只会在 Apple 御用的 SSD 上打开，而自己买的第三方则不行。

那么，我们需要给 OS X 的硬盘驱动（一个`.kext`文件）打个包，让其误认为现在这块 SSD 是御用的。这个方法在 10.9 以前都没有什么问题。但从 10.10 DP5 开始，OS X 只加载带有信任的签名的驱动。也就是说，10.10 要打开第三方的 trim，不仅要给 kext 打 patch，还要关闭签名验证。

# 症状

今天上午手贱用了一下 Trim Enabler，重启时杯具了：开机进度条到三分之一左右，苹果 LOGO 变成了一个灰色的禁止图标。

# 原因

根据判断，大概是没有正确关闭签名验证。

# 解决方案

- 关机。按住**⌘R**的同时开机，进入安全模式。
- 打开终端，键入以下命令（`YourDisk` 用你的系统分区名代替）：

```bash
nvram boot-args=kext-dev-mode=1
cd /Volumes/YourDisk
touch System/Library/Extensions
kextcache -prelinked-kernel System/Library/Caches/com.apple.kext.caches/Startup/kernelcache\
  -K System/Library/Kernels/kernel System/Library/Extensions
```

- 重启。有一部分人应该能解决问题了。如果没解决，继续往下走。
- 依然进入安全模式，再打一遍上述命令（第一行`nvram`什么的就不需要了），重启。
- 如果还不行，在执行`kextcache`一行时，观察有哪些行有`invalid signature`信息。记下有这些信息对应的 kext 名。
- 进入`/Volumes/YourDisk/System/Library/Extensions`，删除**第一个**签名有问题的 kext （使用`rm -rf`）。重启。
- 我到这儿就解决问题了。如果你还是不行，继续往下删除第二个、第三个带`invalid signature`的 kext。但注意不要删除诸如`IOAHCIBlockstorage.kext`等系统核心驱动。看名字，你会知道哪些能删哪些不能删的。

# 后记

目前，在 OS X 上使用第三方 SSD，**Trim Enabler** 依然是不二选择。据作者说，3.2.5 版已经可以在 10.10 DP5 上正常使用。

关于 kext 的签名机制，我翻译一段 Trim Enabler 开发者的话：

> 显然 Apple 想要屏蔽那些会对 OS X 体验有影响的应用。虽然禁用 kext 签名审查还能让 Trim Enabler 正常工作，我还是希望启用 Trim 的过程能更顺畅些。我第一个想到的是自己写一个内核扩展（kext）来达到目的。但这段详细资料说：
>
>> kext 签名意味着一个合法、被签名的内核扩展只能在给苹果交每年99刀保护费、拿到证书后才能创建。另外，开发者必须填一个表格来解释为什么他们需要这个证书。kext 证书只会在申请批准后发出。
>
> Apple 现在用这种方式有效地控制着什么 kext 能在 OS X 上用，由此来控制开发者（比如我）写的什么新功能可以在 OS X 上实现。既然 Apple 已经花了这么大力气阻止第三方 SSD 拿到 Trim 的功能，我猜如果有个什么人写了这样一个（打开 Trim 功能的） kext 的话，申请证书环节也一定会被 Apple 拒。所以就目前而言，使用 Trim Enabler 3.2.5 或以上版本是在 OS X 上开启 Trim 的最好办法。
>
> 我相信 Apple 对 OS X 的这个新导向是不合适（unfortunate）的，其最终会以讨好初级用户为由大幅限制高级用户的手脚。我认为，「越狱」 Mac 的日子也不远了。


# 参考资料
- [Trim Enabler in Yosemite](http://www.cindori.org/forums/topic/trim-enabler-in-yosemite/)
- [Update on Trim in Yosemite](http://www.cindori.org/update-on-trim-in-yosemite/)
