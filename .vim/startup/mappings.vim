" stop arrow keys
nnoremap <right> :copen<CR>
nnoremap <left> :close<CR>
nnoremap <S-up> ddkP

inoremap <up> <Nop>
inoremap <down> <Nop>
inoremap <right> :copen<CR>
inoremap <left> :close<CR>

vnoremap <up> <Nop>
vnoremap <down> <Nop>
vnoremap <right> :copen<CR>
vnoremap <left> :close<CR>

" NERDTree setup
noremap <C-g> :NERDTreeToggle<CR>

nnoremap <F12> :file<CR>

" Easy motion setup
map <Leader> <Plug>(easymotion-prefix)
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
map <Leader>h <Plug>(easymotion-linebackward)
map <Leader>l <Plug>(easymotion-lineforward)
map <Leader>w <Plug>(easymotion-w)
map <Leader>f <Plug>(easymotion-f)
map <Leader>s <Plug>(easymotion-s)

map <Leader>W <Plug>(easymotion-bd-w)
map <Leader>F <Plug>(easymotion-bd-f)

map  / <Plug>(easymotion-sn)
omap / <Plug>(easymotion-tn)

vnoremap <C-m> %
nnoremap <C-m> %
nnoremap <M-m> viB

nnoremap j gj
nnoremap k gk

" remove all white space from the ends"
nnoremap <leader>A :%s/\s\+$//<cr>:let @/=''<CR>

" Add current path to path
nnoremap <leader>acp :set path+=**

" easymotion plugin setup
let g:EasyMotion_do_mapping = 0 " Disable default mappings
let g:EasyMotion_startofline = 0 " keep cursor column when JK motion
let g:EasyMotion_smartcase = 1

" neo complete setup
let g:neocomplete#enable_at_startup = 1

" Vim-Airline setup
let g:bufferline_echo = 0

if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif

" airline setup
let g:airline_powerline_fonts = 1

" Airline symbols
let g:airline_left_sep = ''
let g:airline_right_sep = ''
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.whitespace = 'Ξ'
let g:airline_left_alt_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''

let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline#extensions#whitespace#enabled = 0
let g:airline#extensions#syntastic#enabled = 1

nnoremap ; :
" "nnoremap : ;
nnoremap <S-tab> :buffers<CR>:buffer<Space>
set wildchar=<Tab> wildmenu wildmode=full
nnoremap <F5> :bp<CR>
nnoremap <F6> :bn<CR>
nnoremap <TAB> :bn<CR>
" nnoremap [ :bp<CR>
" nnoremap ] :bn<CR>
nnoremap <C-w> :bd<CR>
nnoremap ± :!<Space>
inoremap § <Esc>
nnoremap § <Esc>
inoremap jk <ESC>

" These mappings doesn't work
nnoremap <S-Up> :wincmd k<CR>
nnoremap <S-Down> :wincmd j<CR>
nnoremap <S-Left> :wincmd h<CR>
nnoremap <S-Right> :wincmd l<CR>

" Edit files within the same directory with Alt+o
nnoremap ø :edit %:h

noremap <leader>' i''<ESC>i
noremap <leader>" i""<ESC>i
noremap <leader>( i()<ESC>i
noremap <leader>[ i[]<ESC>i
noremap <Leader>{ i{<CR>}<Esc>O

" shortcut comand for writing file as root
" Alt+Shift+r or Alt+R
nnoremap ‰ :write<Space>!sudo<Space>tee<Space>%<Space>><Space>/dev/null

nnoremap <Leader>@ :CtrlPBufTag<CR>

" narrow the list down with a word under cursor
nnoremap <Leader>fU :execute 'CtrlPFunky ' . expand('<cword>')<Cr>

" CtrlP settings
let g:ctrlp_funky_matchtype = 'path'
let g:ctrlp_funky_syntax_highlight = 1

" Vim session setting
let g:session_directory = "~/.vim/session"
let g:session_autoload = "no"
let g:session_autosave = "no"
let g:session_command_aliases = 1

let g:indentLine_char = '︙'


" Use Ag with Ack.vim
if executable('ag')
    let g:ackprg = 'ag --vimgrep'
endif
