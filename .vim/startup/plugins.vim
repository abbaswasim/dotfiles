set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
"" Vim config
Plugin 'gmarik/Vundle.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-repeat'
Plugin 'airblade/vim-gitgutter'
Plugin 'mhinz/vim-signify'
Plugin 'bling/vim-airline'
Plugin 'mileszs/ack.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'bling/vim-bufferline'
Plugin 'wincent/command-t'
Plugin 'easymotion/vim-easymotion'
Plugin 'ctrlp.vim'
Plugin 'tacahiroy/ctrlp-funky'
Plugin 'xolox/vim-misc'
Plugin 'xolox/vim-session'
" Plugin 'xolox/vim-easytags' " Don't use tags for now
Plugin 'tagbar'
Plugin 'tomtom/tcomment_vim'
Plugin 'sjbach/lusty'
Plugin 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plugin 'junegunn/fzf.vim'
Plugin 'ervandew/supertab'
" Plugin 'spf13/vim-autoclose'
" "Plugin 'Yggdroot/indentLine'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'flazz/vim-colorschemes'

if has('nvim')
	" neocomplete alternative for neovim
	Plugin 'Shougo/deoplete.nvim'
else
	Plugin 'Shougo/neocomplete'
	Plugin 'Shougo/neosnippet'
	Plugin 'Shougo/neosnippet-snippets'
endif

" All of your Plugins must be added before the following line
call vundle#end()            " required
