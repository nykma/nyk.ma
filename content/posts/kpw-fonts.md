---
title: 给Kindle Paperwhite添加自定字体
categories: ["software"]
author: ["Nyk Ma"]
tags: ["kindle"]
draft: false
date: 2014-11-06
---

1. 在KPW分区的根目录下

```bash
touch USE_ALT_FONTS
mkdir fonts
```

2. 将你的字体拷进`fonts`内。

    - 我不知道什么格式能用，我的字体都是ttf的。otf应该也行。
    - 字体文件名不重要，含CJK字符也行。字体在系统中显示的名字是该字体的「全名」，包含在字体文件信息里的。
    - 一次不宜添加过多。小于三个吧。

3. 重启。
4. 然后你就可以在阅读文档时，在呼出菜单的「Aa」中选择字体了。
   - 如果点了Aa后系统重启了，说明你一次加入的字体太多了，缓存忙不过来，卡狗（WDT）了。请删掉一些字体，分批添加。
   - 看Aa窗口的大小，应该能添加四个自定义字体。

附赠一个思源黑体的效果，在非衬线中表现很好。

![思源黑体](http://i.imgur.com/OWkqOKY.png)
