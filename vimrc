" Neobundle config

if has('vim_starting')
  set nocompatible

  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

call neobundle#begin(expand('~/.vim/bundle/'))

NeoBundleFetch 'Shougo/neobundle.vim'

call neobundle#end()

"GitGutter - show diff status when writing
NeoBundle 'airblade/vim-gitgutter.git'
let g:gitgutter_sign_column_always = 1

" You Complete Me - autocompletion goodness!
" We actually only want to use it if we _are not_ using neovim
if !has('nvim')
  NeoBundle 'Valloric/YouCompleteMe'

  NeoBundleCheck
endif

" Custom tabline

function! MyTabLine()
    let s = '  |'
    for i in range(tabpagenr('$'))
        " Select highighting.
        if i + 1 == tabpagenr()
            let s .= '%#TabLineSel#'
        else
            let s .= '%#TabLine#'
        endif

        " Set the tab page number.
        let s .= '%' . (i + 1) . 'T'

        " The label is made by MyTabLabel()
        let s .= ' %{MyTabLabel(' . (i + 1) . ')} '
        let s .= '%#TabLineFill#|'
    endfor

    let s .= '%#TabLineFill#%T'

    return s
endfunction

function! MyTabLabel(n)
    let buflist = tabpagebuflist(a:n)
    let winnr = tabpagewinnr(a:n)
    let file = bufname(buflist[winnr - 1])
    let numBuffers = len(buflist)
    "find any modified buffers
    let mod = 0 "false
    for buf in buflist
        if getbufvar(buf, "&mod")
            let mod = 1 "true
        endif
    endfor
    let mod = getbufvar(buflist[winnr - 1], "&mod")
    "build the tab string.
    let s = '['
    let s .= a:n
    let s .= '] '
    let s .= ((mod)?'*':'') . file
    let s .= ' (' . numBuffers . ')'
    return s
endfunction

set tabline=%!MyTabLine()
set showtabline=2
set laststatus=2

syntax on
""Switch colorscheme based on which terminal is running.
"if $TERM == 'rxvt-unicode-256color'
"    colors solarized
"endif
"if $TERM == 'rxvt-unicode'
"   colors solarized
"endif 
set background=dark
colors distinguished
set fillchars+=vert:\ 
"set encoding
set encoding=utf-8

set tabstop=2
set shiftwidth=2
set expandtab
set number
set relativenumber
set cursorline
" Help the filetype system out a bit
au BufNewFile,BufRead *.frag,*.vert,*.fp,*.vp,*.glsl set syntax=glsl 
au BufNewFile,BufRead *.bcs set syntax=bc
filetype plugin indent on
"set smartindent
au BufNewFile,BufRead *.c,*.cpp,*.h,*.hpp let g:clang_close_preview=1
set nospell
set nowrap
"Turn off highlighting
nnoremap  <F3>     :noh<CR>
"Make latex-suite use latex highlighting
let g:tex_flavor='latex'

"Disable annoying preview buffer
set completeopt-=preview

"We want to fold things syntax style for c files
au BufNewFile,BufRead *.c,*.h,*.cpp,*.hpp,*.cc set foldmethod=syntax
"We don't want things to be autofolded
set foldlevelstart=99
"We also want to save folds when files close
autocmd BufWinLeave *.* mkview
autocmd BufWinEnter *.* silent loadview
"Show when a column slops over
let g:cc_match_group = matchadd('ColorColumn', '\%121v', 100)
"Show trailing spaces
set list
exec "set listchars=tab:\\|\\|,trail:\uF8"
"Do special things for tex files
function! TexSpecialEnables()
  set spell
  call matchdelete(g:cc_match_group)
endfunction

autocmd BufNewFile,BufRead *.tex call TexSpecialEnables()

" open nerdtree
function! StartNerdtree()
  if 0 == argc()
      NERDTree
  end
endfunction

au VimEnter * call StartNerdtree()
autocmd VimEnter * nmap <F3> :NERDTreeToggle<CR>
autocmd VimEnter * imap <F3> <Esc>:NERDTreeToggle<CR>a
let NERDTreeQuitOnOpen=1
let NERDTreeWinSize=35

" disable dumb gentoo word width stuff
autocmd BufNewFile,BufRead * set textwidth=0

" neovim usability stuff
if has('nvim')
  " enable python support
  runtime! plugin/plugin_setup.vim
  set backspace=indent,eol,start
endif
