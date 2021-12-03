"
" misc. setup
"
set tabstop=2
set softtabstop=2
set shiftwidth=2
set expandtab
set nospell
set nowrap
set modeline

"Turn off highlighting
nnoremap  <F3>     :noh<CR>
"Make latex-suite use latex highlighting
let g:tex_flavor='latex'

" Keybinds for FZF
map <leader>p  :FZF<CR>
map <leader>s  :Rg <C-r>"<CR>
map <leader>ss :Rg<space>
map <leader>w  :Windows<CR>
map <leader>b  :Buffers<CR>

"
" Use a separate directory for swap files and (persistent) undo
"

set directory=$HOME/.vim/swap//
set undofile
set undodir=$HOME/.vim/undo//
