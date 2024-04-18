+++
title = "Proxmox + RouterOS + LEDE 终极折腾教程"
author = ["Nyk Ma"]
lastmod = 2022-11-25T15:33:35+08:00
tags = ["proxmox", "routeros", "openwrt"]
categories = ["network"]
draft = true
+++

{{< admonition warning "注意" >}}
这是个非常扭曲的解决方案。我几乎肯定你和我现状和需求不同。

如果要抄我作业，请选择性地抄。
{{< /admonition >}}

我的 [R86s](https://docs.r86s.net) 到了，不装个软路由浪费了。


## 预期 {#预期}

我已有一个 OpenWRT 的无线路由，但 CPU 不给力，影响乳制品厂出品速度。

我想把它降格成 AP + 交换机，把最核心的路由 + 乳制品功能放到 R86s 上。


## 选型理由 {#选型理由}


### 硬件 {#硬件}

-   1 x 1G + 2 x 2.5G + 2 x SFP+ ，方便我未来折腾 EPON
-   x86，最强生态
-   体积小巧，有主动散热
-   能插 microSD 卡

缺点也有

-   硬盘是 eMMC，不耐磨。
-   发热略大（常年 45+ 度），TF 卡很可能会折寿。


### 软件 {#软件}


#### Proxmox {#proxmox}

-   渐进地把这个网络搭起来时，使用虚拟机调试是最方便的，省得插拔网线了。
-   RouterOS 不支持装到 eMMC 上，因此必须用虚拟机。
-   RouterOS v7 开始似乎不提供 KVM 了，因此要乳制品厂旁路由的话必须上虚拟机。
-   ESXi 似乎也不能装 eMMC 上。


#### RouterOS {#routeros}

-   未来要用 [EPON ONU 模块](https://www.chiphell.com/thread-2184048-1-1.html)，只有 RouterOS 或者 ubnt 之类的支持。
-   功能强大
-   配置直观
-   CLI 的「退路」和 GUI 逻辑一致，很直观
    -   下详
    -   相比之下 openwrt 的 terminal shell 就是标准 Linux，GUI 功能和 CLI
        命令有对不上的可能，很难用 CLI 回滚 GUI 的一次错误修改。


#### LEDE {#lede}

-   没办法，目前最易用、最无人值守的乳制品厂都在 OpenWRT 上。


## 准备 {#准备}

-   R86s
-   一个正常工作的路由器（有乳制品厂最好，下称「既有路由器」）
-   若干根网线
-   一台有网口的电脑
-   显示器和键盘（备用）


## 动手 {#动手}

{{< admonition warning "永远留退路" >}}
网络系统很容易把自己玩崩。

物理显示器和物理键盘永远是你的朋友。

清楚自己在做什么，并清楚如何 reverse 你在做的事。
{{< /admonition >}}

{{< admonition warning "一步一步来" >}}
我会写出每一步的预期表现。

请每做一步，验证一下结果。

不要闷头做一大堆完了发现不对却无从查起。
{{< /admonition >}}

给 R86s 的 `ETH0` 插上既有路由器。

用 U 盘正常引导 ProxmoxVE 安装盘并安装。这一步问题不大。

在既有路由器上查找 PVE 目前的 IP 地址，使用 `https://PVE_IP:8006` 访问 web 界面，用 `root` 登录。
