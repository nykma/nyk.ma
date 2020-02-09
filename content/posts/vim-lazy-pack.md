---
title: Vim 不够懒的懒人包
subtitle: 写给小一看的
categories: ["software"]
draft: true
date: 2015-04-11
---

> 可能 Vim 并不会成为你的最终选择，但对（几乎）每个程序员来说，体验 Vim 都是一个技能提升过程中必不可少的一环。 —— 名雪

Vim 能风靡二十年不倒的最大原因，我觉得在于**轻量**、**便携**和**高度可扩展**。只消 `tmux`、`vim` 和 `mosh` 三样神器，你就能随时连上服务器开始生产了。

> 另外有个好处是**让老板认为你很忙**，不过这点应该 emacs 更适合 :D

## 前言、准备工作

> 如果还不熟悉 vim 的操作，就跟着 `vimtutor` 做几遍。这个教程常做常新，熟练了之后再回来 skim 一遍也是很有帮助的。

> 高强度使用 vim 的话，大概三四个工作日就能熟练掌握基础操作。
> 接下来你学习的每一个技巧和安装的每一个插件，都会让你的生产效率跃迁一个等级。

> 推荐充分理解并肌肉记忆以下常用操作：
>
> - 习惯使用 `w` 和 `b` 来左右移动光标（有时 `0w` 比 一路 `b` 过去快得多）
> -  `ciw`、`diw`（以及`vim-surround`用到的`ysiw)`等）
> - `C`、`cc`、`A`、`I`、`O`
> - `f`、`:30<CR>`
> - `"*yy`、`"*p`
> - `0`、`<C-R>`、`+`（`-`）

> 你需要用肌肉（而不是大脑）去记忆基础操作的键位。

以下介绍正常使用 Vim 时必不可少的一些配置。 我不会直接把我的 `.vimrc` 文件贴出来，这文件必须是你自己配的才有意义。

- 如果你想干净地重新配置一遍，就 `rm -rf ~/.vim*`

- 先照着[教程](https://github.com/gmarik/Vundle.vim)把 `Vundle` 装起来。没有插件的 vim  说能用也能用，无非蛋疼些。


## 基础

一些对我来说必不可少的东西：

``` viml
set shell=/bin/sh "因为我用了 fish ，所以必须在 vim 里手动指定一个兼容 shell，否则一些插件会报错

" 这里跳过一段 Vundle 的东西

"就是 SublimeText 里的 ⌘P ，你懂的。这个有不少替代，对付小项目是足够了
Plugin 'kien/ctrlp.vim'

"自动补完，比 YouCompleteMe 轻量得多，不需要编译和配置。对付一些不需要静态分析的补完足够了
Plugin 'ervandew/supertab'

"中文的 :help ，包括 vim 本体和一些热门插件的
Plugin 'asins/vimcdoc'

"Git 高度集成插件
Plugin 'airblade/vim-gitgutter'

"我最爱的配色。记得在这个 github repo 里查看如何给你的 Terminal 配置颜色
Plugin 'chriskempson/base16-vim'

" 这里跳过一段 Vundle 的东西

syntax on

set shiftwidth=2
set tabstop=2
set autoindent
set expandtab "都是和缩进有关的

set autoread "文件在外部变动后自动重读
set autowrite "见下文注释

set background=dark
colorscheme base16-eighties "我最爱的配色


set ruler "显示光标当前位置
set number "显示行号
set cursorline "高亮显示当前行
"set cursorcolumn "高亮显示当前列，一般没什么必要


set incsearch "实时搜索，边打字边搜索
set ignorecase "搜索时大小写不敏感
set wildmenu "vim 自己的 : 命令也能用Tab补完了
set hlsearch "高亮显示搜索结果

" 以下这段是用来每次保存时自动修剪掉行末空格的。
" 有人问保存文件时末尾自动留一行有没有，这功能是 vim 默认开启的。
fun! <SID>StripTrailingWhitespaces()
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
    call cursor(l, c)
endfun
autocmd FileType c,cpp,java,php,ruby,python autocmd BufWritePre <buffer> :call <SID>StripTrailingWhitespaces()
```

- 如果你添加了新插件后 `:PluginInstall` 没变化的话，除了 `:w` 外，你还需要 `:source ~/.vimrc` 来重载一下配置文件。要不就 `:q` 退出 vim 后再 `vim +PluginInstall` 也行。


- 关于 `autowrite`， `:help` 里是这么描述的：

    > 自动把内容写回文件：如果文件被修改过，在每个 `:next`、`:rewind`、`:last`、`:first`、`:previous`、`:stop`、`:suspend`、`:tag`、`:!`、`:make`、`CTRL-]` 和 `CTRL-^` 命令时进行；用 `:buffer`、`CTRL-O`、`CTRL-I`、`'{A-Z0-9}` 或 {A-Z0-9} 命令转到别的文件时亦然。

## 我们需要更多的配置


假设你装了我上面写的这些插件。首先设定一下`<Leader>`是什么键：

```vim
let mapleader=';'
```

下面的 `<Leader>` 就是分号了。比如依次按 `;`、`e` 两个键就相当于敲入 `$`，依次按 `;`、`/` 两个键就相当于敲入 `:nohlsearch` `回车`。

> 有关更多 `map` 的细节，请参阅 `:help map.txt`


```vim
nmap <Leader>e $ "到行尾，e 是 end 的意思。行首的 0 已经足够方便就不重新 map 了
nmap <Leader>/ :nohlsearch<CR> "关掉搜索高亮，在搜索结束后特别有用
nmap <Leader>w :w<CR> "保存当前文件，我就是懒

let g:ctrlp_map = '<Leader>p' "不用组合键打开 CtrlP
let g:ctrlp_cmd = 'CtrlP'
map <Leader>P :CtrlPLine<CR> "CtrlP 的文件内搜索模式

set mouse=a "有时候鼠标还是挺方便的
```

## 我们需要更多的插件
行啊。

- 目录树（`scrooloose/nerdtree`）

    > 不过我平常都是 CtrlP 用得多，目录树几乎不打开的。

```vim
map <Leader><tab> <ESC>:NERDTreeToggle<CR> " 使用 分号 Tab 打开和关闭
```

- 注释（`scrooloose/nerdcommenter`）

    使用 <kbd>&lt;Leader&gt;c&lt;Space&gt;</kbd> 来 toggle 一行（或者一段，如果你在 V 模式的话）的注释状态。

- 状态栏（`bling/vim-airline`）

    如其名，这是个非常轻量的状态栏，比 Powerline 轻得多。它能和大多数配色方案和插件完美兼容。

```viml
set laststatus=2 "总是显示状态栏
" let g:airline_powerline_fonts = 1 "使用 PowerLine 定制字体
```

- 语法分析（`scrooloose/syntastic`）

    自动使用各语种流行的语法检查器静态地指出语法错误。可能需要一些配置。

- 多光标（`terryma/vim-multiple-cursors`）

    你最爱的 <kbd>⌘D</kbd>。可以对多块进行 <kbd>c</kbd> 或者 <kbd>x</kbd> 操作。

```vim
let g:multi_cursor_next_key='<C-d>'
let g:multi_cursor_prev_key='<C-p>'
let g:multi_cursor_skip_key='<C-x>'
let g:multi_cursor_quit_key='<Esc>'
```

- 瑞士军刀

    这插件太过强大，能满足你的几乎任何需求。你自己去它 github 页看吧。

    我最常用到 <kbd>&lt;space&gt;y</kbd> 、 <kbd>&lt;space&gt;/</kbd> 、 <kbd>&lt;space&gt;s</kbd> 。

    > 你可以用 `:Unite mapping` 来查看当前所有 map 了的快捷键。

```vim
Plugin 'Shougo/unite.vim'
Plugin 'Shougo/neomru.vim'
" ......
let g:unite_source_history_yank_enable = 1
"nnoremap <Leader>p :Unite file_rec/async -auto-preview<cr>
nnoremap <space>/ :Unite grep:.<cr>
nnoremap <space>y :Unite history/yank<cr>
nnoremap <space>s :Unite -quick-match buffer<cr>
nnoremap <space>b :Unite buffer<cr>
nnoremap <space>a :UniteBookmarkAdd <cr>
nnoremap <space>c :Unite bookmark<CR>
nnoremap <space>m :Unite file_mru<CR>
nnoremap <space>f :UniteWithBufferDir -buffer-name=files file<CR>

autocmd FileType unite call s:unite_my_settings()
function! s:unite_my_settings()
  nmap <buffer> <ESC> <Plug>(unite_exit)
  nnoremap <silent> <buffer> <expr> <C-x> unite#do_action('split')
  inoremap <silent> <buffer> <expr> <C-x> unite#do_action('split')
  nnoremap <silent> <buffer> <expr> <C-v> unite#do_action('vsplit')
  inoremap <silent> <buffer> <expr> <C-v> unite#do_action('vsplit')
  nnoremap <silent> <buffer> <expr> <C-o> unite#do_action('open')
  inoremap <silent> <buffer> <expr> <C-o> unite#do_action('open')
endfunction
```

- tmux 集成

    - `edkolev/tmuxline.vim` 把你的 tmux 状态栏和 vim 的状态栏风格统一化
    - `tmux-plugins/vim-tmux` 给 .tmux.conf 文件加 syntax
    - `tmux-plugins/vim-tmux-focus-events` 在 vim 获得焦点及失去焦点时给 vim 发送信号

- Ruby / Rails 集成（`vim-ruby/vim-ruby`、`tpope/vim-rails`）

    有了这俩，你不仅能享受到 RoR 专属方法的高亮和补完，还能直接调用指令。比如：

    - `:Rake db:migrate` ：常规的 `rake` 命令
    - `:Rails g controller articles` ：常规的 `rails` 命令
    - `:.Rake` ：执行光标所在行的单个测试用例
    - `:Rake` ：如果你在编辑一个 model / controller ，那它会自动执行在 spec 里对应的测试用例；如果你在编辑一个测试用例，那它会执行这个文件本身

    > 也支持 tab 补完，比如你忘了 `db` 里有哪些操作，你可以 `:Rake db:` 再按 <kbd>Tab</kbd>。

    > 这个 rails 插件实在是太强大了。详情请见 `:help rails.txt`

- 异步化（`tpope/vim-dispatch`）

    当你跑了测试 `:Rake spec` 却发现你的编辑界面被覆盖了，很恼人吧。这个插件完美解决问题：

    - 如果你在 tmux 下，它会自动在你 vim 的下部开一个很窄的 window 显示结果，并且把焦点保留在 vim 上

    - 如果你在 iTerm 下，它会开一个静默标签显示结果

    > 我录了一段[演示](https://asciinema.org/a/4gav8gy20tgw5f8rs7xs6a7k0)。

- 快速修复（`Valloric/ListToggle`）

    比如你执行了 `:Rake` 跑测试用例，结果有一条 fail 了。这个插件允许你跳转回 Vim 时保留出错信息、自动高亮信息中的出错行，并帮助你跳转到出错行所在文件的位置。默认键位是 `<Leader>q`

    > 有关更多 QuickFix 的信息，请参阅 `:help quickfix`

-----

因为我不写类 C 语言，所以那些更强大的 `tagbar` 什么的我就不说了。

## 参考资料

如果你装了上面的 `vimcdoc` 的话，那些中文 help 写得都非常非常非常详尽，我就不啰嗦了。感谢汉化者的劳动。

- 不知道从哪儿开始就看 `:help`
- 各种小技巧，请看 `:help tips`
- 关于分裂窗格（`window`）和标签页（`tab`），请看 `:help windows` 和 `:help tab-page`
- 关于按键映射，请看 `:help map.txt`
