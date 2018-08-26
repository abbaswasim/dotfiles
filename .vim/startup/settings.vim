set nocompatible
filetype plugin on
set omnifunc=syntaxcomplete#Complete

filetype indent on
let mapleader = "\<Space>"

set shiftwidth=4
set tabstop=4
set showcmd

set incsearch
set showmatch
set hlsearch

set relativenumber
set number
set confirm

" Don't nag about not saved files
set hidden

set history=1000
set undofile
set undodir=~/.vim/undo     " where to save undo histories
set undolevels=1000         " How many undos
set undoreload=10000        " number of lines to save for undo
set path+=`pwd`/**
set path+=/usr/include/**

set timeout timeoutlen=3000 ttimeoutlen=100

set autoindent
set scrolloff=6
set shiftwidth=4
set expandtab
set softtabstop=4
set cursorline
" set visualbell
set backspace=indent,eol,start

" Always use /g for replace on line, instead of :%s/foo/bar/g use %s/foo/bar/
set gdefault

set tags=./tags

" briefly show matching braces
set showmatch

" Solarized theme
syntax enable
set background=light
set listchars=tab:>~,nbsp:_,trail:·,eol:⏎

set wildignore+=*.a,*.o
set wildignore+=*.bmp,*.git,*.ico,*.jpg,*.png
set wildignore+=.DS_Store,.git,.hg,.svn
set wildignore+=*~,*.swp,*.tmp
set wildignore+=tags

set encoding=utf-8

set list
set wildchar=<Tab> wildmenu wildmode=full

" Always show statusline
set laststatus=2

" Use 256 colours (Use this setting only if your terminal supports 256 colours)
set t_Co=256
let g:solarized_termcolors=256

" ctags setting to look for tags in cwd
set tags=tags;/

" For some reason regexpengine is slowing down vim so the following help
set re=1
set ttyfast
set lazyredraw
set fillchars=diff:·

" if running neovim don't use lusty explorer because it doesn't support ruby
if has('nvim')
    let g:LustyJugglerSuppressRubyWarning = 1

    " neocomplete alternative for neovim
    let g:deoplete#enable_at_startup = 1
endif
