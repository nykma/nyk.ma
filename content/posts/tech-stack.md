---
title: 本站技术栈（V2）
categories: ["network"]
draft: false
author: ["Nyk Ma"]
date: 2015-06-28
---

刚刚完成了本站从 Node.js(Express) 到 Ruby(Sinatra) 的重构，本文主要说说重构时遇到的问题及解决思路。

## 器件选型

该烘培机的结构很简单，不需要上RoR之类的庞然大物，一个Sinatra搭配Thin足够了。模版使用Slim，因为其语法和Jade非常相似，方便我从旧站迁移。

该站除了几个静态展示页面外，最复杂的就是这个 /archive 了。我期望它是一个不基于数据库、完全依赖文件结构和文件头信息来组织的博客，类似Jekyll。我接下来的工作相当于重造小半个Jekyll。

## 模版追加内容

这是我遇到的第一个棘手的问题。期望的页面布局大概可分为layout、顶部navbar、底部footer、博客右侧的nav、正文这几块，其中需要重用的部分很多。Slim原生只支持一次注入一个完整的 `.slim` 文件，而如果我想在特定页面“append”一些东西的话就很难办了。

举个例子，这是全站的`layout.slim`：

```slim
/ views/layout.slim
doctype html
html
  == slim :"inc/head"
  body
    == slim :"inc/navbar"
    == yield
    == slim :"inc/footer"
```

你可以看到这里整体注入了很多模版，这和Jade的 `include` 一个意思。现在我们想给footer预留一块给不同的页面填充不同的东西，我们先看看Jade怎么做：

```
/ views/inc/footer.jade
.footer
  p.text-muted &copy; 2011-2015, Nyk Ma.
  block footer
```


```
/ views/index.jade
block footer
  p.text-muted Now in index page.
```


```
/ views/archives.jade
block footer
  p.text-muted Now in archive page.
```

懂了吧？很可惜Slim没有提供强大如`block`的命令，那只能我们自己动手了。

```ruby
# main.rb of Sinatra
# 如果在 Rails 里使用这些方法，它们应该被放在 controller 的 helper 里

Slim::Engine.set_default_options streaming: false # Required!

def content_for(key, &block)
  content_blocks[key.to_sym] << block.call
  ""
end
def content_for?(key)
  content_blocks[key.to_sym].any?
end
def yield_content(key, *args)
  content_blocks[key.to_sym].join
end
def content_blocks
  @content_blocks ||= Hash.new { |h,k| h[k] = [] }
end
```

你明白我意思了：我们自己维护一个 `key:block` 列表并决定什么时候`join`它们。这样一来，调用就会变得和Jade差不多：

``` slim
/ views/inc/footer.slim
.footer
  p.text-muted &copy; 2011-2015, Nyk Ma.
  == yield_content :footer
```

``` slim
/ views/index.slim
== content_for :footer do
  p.text-muted Now in index page.
```

``` slim
/ views/archives.slim
== content_for :footer do
  p.text-muted Now in archive page.
```

然后我一口气移植好了所有静态页面。

## /archive ：基本功能

我们先来搞定文章页面。Raneto规定每篇文章开头由一个特定标示块来记录metadata，之后是正文。结构大致如下：

```markdown
／*
title: 本站技术栈
subtitle: Sinatra / Slim / Kramdown
category: network
published: true
date: 2015-06-28
*／

## 器件选型

该烘培机的结构很简单，不需要……
```

我们要做以下事情：

1. 读`.md`文件
1. 分离meta和正文
2. 正文扔给 markdown render 解析为 HTML
3. meta 扔给 YAML parser 解析为 Ruby Hash
4. 将正文和 meta 扔给模版

### 读文件

Ruby的类和方法的命名相当科学，科学到你用猜就能猜得到的程度。比如这个读取文件的命令，猜一个 `File.read`，进`irb`试一下，果然对了。那这部分就结束了：


```ruby
# main.rb
get "/archives" do
  filename = "./articles/test.md"
  file = File.read(filename)
  # .....
end
```
> 我们先定死一个`filename`，待会儿再想办法用URL指定文件。

### 分离

第一反应是正则。翻翻正则参考手册，拼出来一个能用的： `/\/\*(.*)\*\/m`

> - `\/\*` : 开头的 `／*`，两个均需转义
> - `(.*)` : 中间的所有玩意儿
> - `\*\/` : 结尾的 `*／`
> - `m` : 让Ruby跨行匹配

那好，接下来用 `.scan` 和 `.sub` 来分离 meta 和 body ：

```ruby
# main.rb
get "/archives" do
  filename = "./articles/test.md"
  file = File.read(filename)
  meta = file.scan(/\/\*(.*)\*\//m).flatten[0])
  # String#scan 出来的结果是这个结构： [[""]] ，需要 flatten[0] 把它剥出来
  body = file.sub(/\/\*(.*)\*\//m, "")
end
```

### 处理

两边都很简单：meta扔给`YAML.load`，正文扔给Kramdown。翻了翻Kramdown官网就写出来了：

```ruby
# main.rb
require "kramdown"

get "/archives" do
  filename = "./articles/test.md"
  file = File.read(filename)
  meta = file.scan(/\/\*(.*)\*\//m).flatten[0])
  meta = YAML.load(meta)
  body = file.sub(/\/\*(.*)\*\//m, "")
  body = Kramdown::Document.new(body, input: "GFM").to_html
  # 我文章里的代码块习惯使用GFM风格，所以要指定一个parser
end
```

### 渲染

从controller到view的桥梁是实例变量： `@var`。我们把上面的`meta`和`body`都加上一个`@`，模版里就能读取到它们了。

```ruby
# main.rb
require "slim"

get "archives" do
  # ...假设你这里已经弄好实例变量
  slim :archives # 如此就能开始模版渲染了
end
```

```slim
/ views/archives.slim

h1
  =@meta["title"]
  - if @meta["subtitle"]
    small= " " + @meta["subtitle"]
==@body

== content_for :footer do
  p.text-muted "Written: #{@meta["date"]}"
```

至此，主体功能完成。
