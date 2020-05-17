+++
title = "Emacs 自力求生指南 ── 初识 Elisp"
author = ["Nyk Ma"]
date = 2020-05-17T14:44:00+08:00
lastmod = 2020-05-17T15:49:36+08:00
tags = ["emacs"]
categories = ["tutorial"]
draft = false
+++

## <span class="section-num">1</span> Emacs Lisp {#emacs-lisp}

Emacs lisp 是一个函数式语言。就我野生的 CS 知识理解，函数式语言的核心是「表达式的构造、求值和保护」。


### <span class="section-num">1.1</span> List {#list}

List: LISt processor 。List 是 lisp 程序的基本指令单元和数据结构。一对括号表示一个 list，内容物可以是任意东西

```elisp
;; 这个 list 有一个元素：一个名为 list-packages 的 symbol
(list-packages)
;; 一个 symbol 和两个字符串
(abc "def" "ghi")
;; symbol、整数、list、list
(+ 1 (* 3 5) (/ 8 2))
;; 没有元素
()
;; symbol 和 list
(quote (+ 1 2))
```


### <span class="section-num">1.2</span> 求值 {#求值}

在 Emacs 里，上面四个表达式，你可以把光标移到右括号的右边按 C-x C-e
对其求值，结果会显示在 minibuffer 里。结果分别为：

1.  显示「软件包列表」buffer
2.  报错： Symbol abc 没有绑定函数
3.  `20`
4.  `nil` ，在 Lisp 世界里表示「假」。[^fn:1]
5.  `(+ 1 2)`

求值：「找到第一个 symbol 所定义的函数，并把 list 里剩下的元素求值后作为参数传入」。第一行有定义函数，且不需要参数； 因为我们没有把
abc 这个 symbol 定义一个函数，所以第二行报错了；求值是递归的，所以第三行里的两个子 list 先被求值再传入 + 函数； 对空 list 的求值永远为空 list，所以第四行不会报错；

第五个结果有点意思，我们本来期望它可能出现 3 的，怎么这个 list 被原样抛出来了呢？


### <span class="section-num">1.3</span> 保护 {#保护}

`(quote)` 可以用来保护一个 List 免于被求值，使其可以保留完整表达。

考虑以下程序：

```elisp
;; (car some-list) ：取出 some-list 的第一个元素
(car (+ 2 3))
```

我们希望这个表达式能返回 `+` 这个 symbol，但它却报错了： `(wrong-type-argument listp 5)` 。数字 5 的出现说明表达式被递归求值了。

那我们怎么抵抗求值呢？像上面例 5 那样保护起来就行了：

```elisp
(car (quote (+ 2 3))) ;; +
```

这命令有什么用呢？简单地说，这个命令 **模糊了指令和数据的界线** ：

-   指令被 `(quote)` 起来就变成了一个普通的 list
-   `(eval)` 一个 list 就变成了指令

写一个函数，把所有传入的表达式都变成加法表达式

```elisp
;; 'expr 等同于 (quote expr)
;; cons: 把所有参数连接为一个 list
;; cdr: 取一个 list 除第一个元素外的部分
(defun change-to-add (expr) ;; 定义函数 函数名 参数列表
  "Change EXPR 's first symbol into `+'" ;; doc ，可省略
  ;; 剩下的全部是函数体。最后一个表达式的返回值即为整个函数的返回值
  (cons '+ (cdr expr))) ;; 在最后一个右括号右侧 C-x C-e 使定义生效

;; 该函数只构造 list
(change-to-add '(* 33 44)) ;; (+ 33 44)
(change-to-add '(/ 5 3)) ;; (+ 5 3)
;; 如果想对 list 求值：
(eval (change-to-add '(* 33 44))) ;; 77
```


### <span class="section-num">1.4</span> 保护内临时展开 {#保护内临时展开}

一个十分常见的场景：我想构造一个 list ，其中一个位置在构造时需要插入外部变量的 **值** 。

比如：

```elisp
(let ((my-value 123)) ;; let: 「局部变量」定义
  '(+ my-value 444))
```

我希望上文能给我 `(+ 123 444)` 这个表达式，但我最终拿到的是 `(+
    my-value 444)` 。这结果显然是不可用的，因为 `let` 的外部没有
`my-value` 的定义。

这时，我们要在 quote 一个表达式时， `unquote` 这个变量，即重新对其求值：

```elisp
(let ((my-value 123))
  `(+ ,my-value 444)) ;; 注意单引号变成了反引号，同时 my-value 前面有个逗号用来标识需要求值的位置
;; 返回： (+ 123 444)
```

这有什么用呢？举一个我实际使用中遇到的例子：

```elisp
;; 很多 Ruby 的软件包只要装起来就行，配置都是固定的，像这样：
(use-package bundler :delight)
(use-package rbenv :delight)
(use-package rubocop :delight)
(use-package rspec-mode :delight)
(use-package ruby-test-mode :delight)
(use-package ruby-refactor :delight)
(use-package ruby-tools :delight)
(use-package rake :delight)

;; 显然这么写太罗嗦了。我们试着循环构造这个 list 并求值
(let ((ruby-packages '(bundler
                      rbenv
                      rubocop
                      rspec-mode
                      ruby-test-mode
                      ruby-refactor
                      ruby-tools
                      rake)))
  (dolist (pkg ruby-packages) ;; 类似 forEach
    (eval `(use-package ,pkg :delight))))
```

有没有体会到「指令即数据，数据即指令」？ (ﾟ∀ﾟ)


## <span class="section-num">2</span> 常用调试命令、快捷键、流程、资料表 {#常用调试命令-快捷键-流程-资料表}

`C-x C-e` (`eval-last-sexp`)
: 最常用的调试方法。由于 Lisp 递归求值的特性，你可以通过移动光标，从内向外逐级调试。

`(message "abc")`
: 在 `*Messages*` buffer 里留下信息，也就是 `printf` 或者 `console.log()`

`(toggle-debug-on-error)`
: 在异常发生时自动弹出调用栈。注意它是 toggle。

`C-h v` (`describe-variable`)
: 寻找某个全局变量的值、文档、定义位置等

`C-h f` (`describe-functionh`)
: 寻找某个函数的文档、定义位置等

`C-h k` (`describe-key`)
: 寻找某个快捷键当前绑定的 function

[基于函数入口的调试法](https://emacs-china.org/t/emacs/11777)
: 在函数被 call 时自动弹出调用栈

[ANSI Common Lisp 手册中文版](https://acl.readthedocs.io/en/latest/zhCN/index.html)
: 用来查找常用控制流和 helper 函数的名字。Elisp 和 Common lisp 区别不大。

[^fn:1]: `nil` 和 `()` 意义相同。「真」是 `t` 。对 `nil` 和 `t` 的求值结果永远为自身。
