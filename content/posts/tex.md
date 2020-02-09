---
title: LaTeX快速上手
categories: ["software"]
subtitle: 以XJTU本科毕业论文为例
draft: false
author: ["Nyk Ma"]
date: 2014-06-21
---

**论文题目**: LaTeX 快速上手

**学生姓名**: 马名雪

**指导教师**: [Google](https://www.google.com)、[XJTUthesis](https://code.google.com/p/xjtuthesis)

# 摘要

近年来，随着大学生数量的逐年增长和高校扩招规模的逐年增大，基于 Word 的论文排版占用大学生宝贵 DOTA 和看球时间的弊端也越来越显著，学生和高校亟需推广一种高效率的论文排版工具和方法。本文基于国外高校和科学期刊长期使用的 LaTeX 解决方案，结合已有的 xjtuthesis 模板，给国内大学生使用 LaTeX 排版论文提出一些快速上手方法和建议。原型经上电实测，实验数据和图形符合预期。

**关键词**：LaTeX；论文；排版；西安交通大学

# 绪论

## 关于TeX

[TeX](https://zh.wikipedia.org/wiki/TeX)（或者说，目前更通用的[LaTeX](https://zh.wikipedia.org/wiki/LaTeX)）是一个排版引擎。

它要做的事儿就一个：**排版** 。

因此，它最适宜的工作场景是：

- 目标版型复杂
- 内容巨多
- 图文混排多
- 公式多、复杂
- 参考文献多，关系乱
- **已有LaTeX模板可供使用**

看到没，前几点几乎都是论文的特点嘛。因此LaTeX也就天生变得特别适合排论文了。

> 当然还有图书。甚至有geek连presentation都用LaTeX做演示文稿……

TeX**不能**、也**不会**替代诸如Word、Pages之类所见即所得（WYSIWYG）的文稿编辑软件，因其上手曲线过高，且不适用于大部分日常处理文字的场合。

> Word是个很棒的工具，但可惜很多人用的方法都不对，比如狂敲空格和回车来定位内容什么的。

> 再说个题外话，工具本身是**没有对错之分**的。那些 Vim/Emacs、Windows/Mac/Linux、Chrome/Firefox之类的党派斗争没有意义。生产内容的工具，你觉得哪个顺手就用哪个，这东西太主观了。

最后，LaTeX本质上应算一种程序语言。请用你大一时对待C++的态度来学习它。当然它不难学，看完本文，再尝试个半小时就可以开始写自己的内容了。相信我，磨刀不误砍柴工。

*[WYSIWYG]: What you see is what you get.

## 关于本文

- 本文面向有一定程序设计基础、希望排出高质量学术文档且**会用Google**的**中国**大学生（Bachelor / Master / PhD）。

	> 我会尽量给你个思路，但特例总不能面面俱到的。不会的问Google去，我也是搜出来的。

- 本文假设你需要在有限的时间内写出论文（比如，一星期）。在如此之短的时间内，你不可能将LaTeX玩得非常透，但足够输出一个高质量的pdf了。当然我说的是版式，内容靠自己吧。

- 本文以思路作文路，是笔者第一次使用LaTeX时的探索经历，因此是流水账。

# 动笔

## 准备工作

**请先向学校教务处、研究生院等机构确认学校是否能接受LaTeX / pdf格式的论文提交。**

> 意思是，本文不承担任何责任。

另外，你需要查找有没有自己学校的LaTeX模板。

> 没有就别想了，乖乖用word吧。零基础开始弄出一个符合规范的LaTeX模板的时间够你写两篇论文了。

满足以上两个条件后，就开始配置TeX环境吧。[TeXLive](https://www.tug.org/texlive/)是目前最好的一步到位方案（Windows / Linux）。

> Mac用户请用[MacTex](https://www.tug.org/mactex/)

然后下载自己学校的论文模板。

## 阅读主文件

> 除非特别提出，下文均以西安交通大学学位论文LaTeX模板（[XJTUthesis](https://code.google.com/p/xjtuthesis/)）为例进行说明。

通常你会找到一个或多个主tex文件（类似`main.c`一般的存在），编译它才能正确生成文档。如XJTUthesis里有三个主文件：

- `bachelor.tex`
- `master.tex`
- `doctor.tex`

光看名字就知道是干什么的了。接下来点进我们要的本科毕业论文，看看有什么需要自定义的。

跳过一段声明文字，主文件先声明了一个class：

```latex
\documentclass[
    bachelor,
    %bigskip, % sets linespread factor to 1.5
    %truefont, % just turn it on when using Windows
    %nofont, % remember to manally set the fonts
    pdflinks,
    %colorlinks,
    %compact,
]{xjtuthesis}
```
这块确定了文档的全局设置（你看一个完备的注释能节省多少翻代码的时间），想使用哪个功能直接取消注释就行。

- `bigskip`：将行距设置为1.5倍
- `truefont`：使用windows字体（宋体、Times New Roman）
- `nofont`：不使用模板的字体，由自己声明

字体声明部分在`.cls`文件里。在`xjtuthesis.cls`中：

```tex
% Font-family
\ifxjtu@nofont\else
    \ifxjtu@truefont
      \setmainfont[Ligatures=TeX]{Times New Roman}
      \setCJKmainfont{SimSun}

    \else
      \setmainfont[Ligatures=TeX]{FreeSerif}
      \setCJKmainfont{文鼎ＰＬ简报宋}
    \fi
\fi
```

很易懂吧？如果定义了`truefont`则使用宋体和Times New Roman，如果没定义则使用文鼎PL简报宋和FreeSerief。同理，定义`nofont`后就用`\setmainfont`和`\setCJKmainfont`指定字体就好了。

接下来开始描述文档结构了：

```tex
\begin{document}

    % 请修改 meta.tex 中的论文元信息
    \input{meta.tex}

    \xjtucinfopage %中文论文信息
    \xjtueinfopage %英文论文信息
    \xjtutoc %生成目录
    \clearpage %空页

    \input{pages/intro.tex} %绪论
    \input{pages/hardware.tex} %硬件设计
    \input{pages/software.tex} %软件设计
    % 以同样方法添加更多章节
    \input{pages/conclusion.tex} %结论与展望

    % 将你要引用的文献的 BibTeX 放入 bibliography.bib
    % \nocite{*}
    \xjtubib{bibliography} %参考文献

    \xjtuappendix
       \input{pages/appendice.tex} %附录
    \xjtuendappendix

    \xjtuspchapter{致谢}{致\qquad 谢}
        \input{pages/acknowledgements.tex} %致谢

\end{document}
```

很清楚。`\input`的使用使得我们可以很方便地在正文中插入、删除大段内容，以及调整次序。

## 撰写正文

正文中使用的LaTeX语法在`./example/`里已经无所不包了。要使用哪一种，直接在文件里翻找一下，复制粘贴来，改改内容即可。

注意，在论文中，每一个「段落」（paragraph）的内容是相对紧凑、明确的。当叙述内容和方向没有大改变时，不要轻易换段。

## 参考文献

`BibTeX`包可以极大简化参考文献的收集、管理、引用。一个`.bib`即为一个参考文献库。库中每个条目的结构大致为：

```tex
@article{christensen1979solutions,
  title={Solutions for effective shear properties in three phase sphere and cylinder models},
  author={Christensen, RM and Lo, KH},
  journal={Journal of the Mechanics and Physics of Solids},
  volume={27},
  number={4},
  pages={315--330},
  year={1979},
  publisher={Elsevier}
}
```

![Imgur](http://i.imgur.com/25n3Lpm.png)

每个字段的功能都很明确。这个元数据可以直接从[Google scholar](http://scholar.google.com.cn)中生成（如上图红框）。将这个字典一样的元数据全部平文放进`.bib`文件，扔给`.bst`文件渲染后即可生成一个符合格式要求的参考文献页。

> 使用模板文件渲染的好处在于，如果你本来投IEEE的稿件要转投ACM，或者IEEE的格式更改了，只要改一改使用的`.bst`，重新编译一遍就行了。

> 在中国，参考文献的格式应遵循《文后参考文献著录规则》（[GB/T 7714-2005](http://www.bda.edu.cn/wyxb/12-11-26.pdf)）。对应的`.bst`文件为`GBT7714-2005.bst`，可以在网上搜索到。该模板为中文TeX用户自行制作，模板作者不对使用后果负责。

> 在XJTUthesis中，声明渲染模板的地方依然在`xjtuthesis.cls`里：`\bibliographystyle{gbt7714-2005}`

> 如果你使用IDE编译`.tex`，记得在编译选项里把BibTeX勾上。如下图。

![Imgur](http://i.imgur.com/g9SvkrS.png)

使用`\cite{}`在正文引用参考文献。比如，要引用上例文章的话，

```tex
…这是一段废话。\cite{christensen1979solutions}
```

你不需要管`.bib`中条目的出现顺序：谁先被引用了，谁就排前面。数字也是自动按顺序标的。
当然也可以一次引用多篇，大括号中用逗号隔开即可：

	…你问我支持不支持，我说支持，我就明确告诉你这一点。\cite{基本法, 选举法}

> 只有被正文`\cite{}`了的条目会出现在「参考文献」页里。如果使用了`\nocite{*}`，那么不管有没有被引用都会出现在「参考文献」页里。

## 除错

检查这些可以解决很多问题：

- **首先的首先，请读一读报错信息。**

	> 我发现中国学生不喜欢读英文还是怎么的，一些console log写得很清楚的错误原因就是不愿意读，拒绝读、拒绝理解、拒绝搜索，只会说一句话：我程序出问题了。

- 环境配置是否正确。可以先编译一个小文件看看系统能否正确工作。
- 是否编译了正确的`.tex`文件。
- 注意保留字：# $ % ^ & _ { } ~ \

	> 尤其是下划线有时不容易注意到。不管是正文、脚注还是代码区块中，只要出现了下划线就必须用转义表达：`\_`

	> 如果想要在正文中写反斜杠的话：`$\backslash$` 。
	> `\\`不行，它是用来强制换行的。

- 确保系统中有模板指定的字体。
- 检查一些关键文件的位置是否是正确的。

	> 比如`xjtuthesis.cls`、`gbt7714-2005.bst`、`bibliography.bib`之类，如果不确定应该放哪里，那就和主`.tex`文件放一个目录下。

- 如果可能，选择`xelatex`模式。

	> 如果你看到关于fontspec包的报错, 很可能你应该改用xelatex.


# 结论与展望

## 结论

熟悉这套环境后，你可以做到**几乎不停笔地**撰写内容而不用打断思路到内容排版上。这是一个莫大的优势，要知道，你按下编译按钮后，输出的pdf格式一定是正确且美观的。

> 比如，xjtuthesis的作者[@multiple1902](https://twitter.com/multiple1902)用[两天时间](https://twitter.com/multiple1902/status/475490892288389120)写好了自己的本科毕业设计论文。我用了3天。

## 不足与展望

本文的泛用性不高。我假设所有高校论文LaTeX模板都与这个类似的「主文件 + 格式文件 + BibTeX + 子页面 + 附件」结构。如果可能，我会翻阅其它学校的模板，并给出一些编译建议。

另外，某些细节待补充。

# 参考文献

[1] Christensen R M, Lo K H. Solutions for effective shear properties in three phase sphere and cylinder models[J]. Journal of the Mechanics and Physics of Solids, 1979, 27(4): 315-330.

[2] Lamport L. LaTEX: User's Guide & Reference Manual[J]. 1986.

# 致谢


本论文是在导师：Google副教授的悉心指导下完成的。导师渊博的专业知识，严谨的治学态度，精益求精的工作作风，诲人不倦的高尚师德，严以律己、宽以待人的崇高风范，朴实无华、平易近人的人格魅力对我影响深远。不仅使我树立了远大的学术目标、掌握了基本的研究方法，还使我明白了许多待人接物与为人处世的道理。

本论文从选题到完成，每一步都是在导师的指导下完成的，倾注了导师大量的心血。在此，谨向导师表示崇高的敬意和衷心的感谢！本论文的顺利完成，离不开各位老师、同学和朋友的关心和帮助。

在完成论文期间，同时得到了 @multiple1902 同学对模板结构和格式部分的宝贵意见,在此对他深表谢意。
