---
title: 在RPi上使用FAST FW150US
categories: ["soc"]
date: 2014-12-27
author: ["Nyk Ma"]
draft: false
---

> 本文年久失修，说不定现在内核已经有这个驱动了。

# 常规步骤

这个没什么好说的。

首先`lsusb`发现这货使用的Realtek解决方案，硬件ID是`0bda:8179`。

再`lsusb -v -d 0bda:8179`后查明这货的芯片是RTL8188EUS。

Google之，得到一个民间维护的[RTL8188EU驱动](https://github.com/lwfinger/rtl8188eu)。

下载并解压之。尝试直接在RPi下`make`，失败。嘛意料之中。

给RPi接上网线，保险起见先

```bash
sudo apt-get update && sudo apt-get -y upgrade。
```

再回去看Github上的`README.md`，它提到了需要安装header才能编译。好吧那就

```bash
sudo apt-get install build-essential linux-headers-rpi-rpfv
```

再尝试`make`，一切顺利。接着`make install`后就能在下次开机时生效了。着急的话，`sudo modprobe 8188eu`就能立即加载驱动了。

接下来`wpa-supplicant`什么的就不用说了吧，各位比我熟。

----

# 二逼步骤

其实上面`sudo apt-get upgrade`后，我的内核更新到了`3.12.35+`，但重启后才会生效，因此当时的内核是`3.12.28+`。

所以`sudo apt-get install linux-headers-rpi-rpfv`装好后也是35+的headers。

然后我就傻逼兮兮地在28内核下折腾了三遍编译……

这事儿吧本来重启一下就能解决的，我偏偏又去[RaspberryPi的Github](https://github.com/raspberrypi/linux)下把源代码全拖下来把头文件`make`出来后`ln -s`进`/lib/modules`里…

即使这样还没解决：因为28的内核挂载不了35的驱动嘛。

算了算了，也算是对Linux内核编译过程多了点了解吧。今后的教训就是在编译驱动前检查`uname -r`是否和源码里`include/config/kernel.release`里的版本号一致。


### 扩展阅读
- [Raspberry Pi • View topic - cannot found linux-header for 3.2.27](http://www.raspberrypi.org/forums/viewtopic.php?f=71&t=17666)
- [lwfinger/rtl8188eu](https://github.com/lwfinger/rtl8188eu)
