---
title: 10个让 OS X 更好用的 default write 命令
categories: ["software"]
author: ["Nyk Ma"]
type: post
date: 2012-10-09
---

> 以下内容均由名雪翻译自 [osxdaily.com](http://osxdaily.com/2012/10/09/best-defaults-write-commands-mac-os-x/)

很多OS X偏好设置可以通过一些第三方控制面板来改变。不过背后起作用的核心命令`defaults write`可以做一些只能由命令行调整的改动。以下列举了十个我们认为最棒的`defaults write`命令。即使不是技术宅的你也一定能找到一些令你称心如意的小trick。

## 取消自动隐藏Dock的触发时间

对那些隐藏了Dock的用户来说，把鼠标移动到屏幕下缘呼出Dock有一个短延迟。你可能对此不敏感，不过[移除该延迟](http://osxdaily.com/2012/03/27/remove-auto-hide-dock-delay-mac-os-x/)后会让此非常明显，有一种 Mac 更快了的错觉。

    defaults write com.apple.Dock autohide-delay -float 0 && killall dock

你可以将`-float 0`修改为任意你想要的触发延迟，以秒为单位。

## 加快Mission Control的动画效果
这是另一个让你有 Mac 变快错觉的trick，仅仅是[缩短了Mission Control动画的长度](http://osxdaily.com/2012/02/14/speed-up-misson-control-animations-mac-os-x/)。

    defaults write com.apple.dock expose-animation-duration -float 0.12 && killall Dock

同样的，可以在`-float`后自定义长度，以秒为单位。

> 名雪按：我相反，是把时间拉长了。原先太快的动画总有不流畅的感觉。

## 让Dock中隐藏的程序图标半透明

“隐藏窗口”一直是一个 OS X 的实用功能，不过默认设置下不易区分哪些程序是被隐藏了的。这里使用一个简单的命令开启隐藏程序的[图标半透明](http://osxdaily.com/2010/06/22/make-hidden-application-icons-translucent-in-the-dock/)，这样就便于区别了。

    defaults write com.apple.Dock showhidden -bool YES && killall Dock

## 在Mail中拷贝地址时不使用全名
不论出于何种理由，当你在Mail.app里想拷贝一个email地址时，总是会附上联系人的全名。很讨厌。不过用一个`defaults write`命令可以[关掉](http://osxdaily.com/2012/05/03/stop-pasting-full-names-copy-email-address-mac-os-x-mail/)这功能。

    defaults write com.apple.mail AddressesIncludeNameOnPasteboard -bool false

## 在“快速查看”中开启文本选择功能
空格键一直是OSX最有用的功能之一。并且在快速预览中拥有[文字选择](http://osxdaily.com/2011/11/21/select-text-in-quick-look-windows/)功能似乎也是理所应当的。下面告诉你怎样打开它。

    defaults write com.apple.finder QLEnableTextSelection -bool TRUE;killall Finder

## 在Finder中始终显示隐藏的文件和文件夹
隐藏的文件，不出意外地，在Finder中默认是不显示的。[显示隐藏文件](http://osxdaily.com/2009/02/25/show-hidden-files-in-os-x/)很容易做到，对高玩来说是刚需吧。

    defaults write com.apple.finder AppleShowAllFiles -bool YES && killall Finder

## 完全隐藏桌面图标
如果你桌面上堆了一大坨乱七八糟的文件，[隐藏所有桌面图标](http://osxdaily.com/2009/09/23/hide-all-desktop-icons-in-mac-os-x/)会让你像通了便一样一身轻松。这些文件依然可在`~/Desktop`文件夹访问，你只是不让他们堆在你的萌妹子壁纸上罢了。

    defaults write com.apple.finder CreateDesktop -bool false && killall Finder

## 在登录屏幕上显示系统信息
开启后，你可以在登录界面上查看一些[系统的基本信息](http://osxdaily.com/2011/08/17/show-system-info-mac-os-x-lion-login-screen/)，包括OS X系统版本，主机名及其它。点击右上角的时间即可。适用于系统管理员和蛋疼高玩。

    sudo defaults write /Library/Preferences/com.apple.loginwindow AdminHostInfo HostName

> 名雪按：同理你可以在该命令后再添一些系统变量和自己的字符串啥的

## 改变截图的默认保存地址
如果你经常⌘⇧3或⌘⇧4，你一定明白你的桌面会很快被一堆png淹没。推荐解决方案是在`~/Pictures`或`~/Documents`新建一个文件夹，然后将其设为默认的[屏幕截图保存位置](http://osxdaily.com/2011/01/26/change-the-screenshot-save-file-location-in-mac-os-x/)

    defaults write com.apple.screencapture location ~/Pictures/Screenshots

## 改变截图的默认格式
依然是屏幕截图。你[可以改变](http://osxdaily.com/2010/08/16/change-the-screenshot-capture-file-format/)默认文件格式为JPG或其它的格式。JPG可以提供最佳的图片质量和压缩比。

    defaults write com.apple.screencapture type jpg && killall SystemUIServer

## 附加：显示~/Library文件夹

一个简单的命令能[总是显示用户的~/Library文件夹](http://osxdaily.com/2011/07/04/show-library-directory-in-mac-os-x-lion/)。这虽不是`defaults write`命令，不过对需要经常访问`~/Library/`的人而言会非常方便。

    chflags nohidden ~/Library/


以上大多数命令应该适用于所有 OS X 版本。
