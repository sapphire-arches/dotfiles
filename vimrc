" Neobundle config

if has('vim_starting')
  set nocompatible

  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

call neobundle#begin(expand('~/.vim/bundle/'))

NeoBundleFetch 'Shougo/neobundle.vim'

call neobundle#end()

NeoBundle 'airblade/vim-gitgutter.git'
NeoBundle 'Rip-Rip/clang_complete.git'
NeoBundle 'flazz/vim-colorschemes.git'
NeoBundle 'Raimondi/delimitMate.git'
NeoBundle 'scrooloose/syntastic.git'
NeoBundle 'dart-lang/dart-vim-plugin.git'
NeoBundleCheck

"GitGutter - show diff status when writing
let g:gitgutter_sign_column_always = 1

"setup status line
set statusline=%t
set statusline+=[%{strlen(&fenc)?&fenc:'none'}, "file encoding
set statusline+=%{&ff}] "file format
set statusline+=%h      "help file flag
set statusline+=%m      "modified flag
set statusline+=%r      "read only flag
set statusline+=%y      "filetype
set statusline+=%=      "left/right separator
set statusline+=%c,     "cursor column
set statusline+=%l/%L   "cursor line/total lines
set statusline+=\ %P    "percent through file"]
"Add syntastic status to the statusline
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

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
colors bubblegum
" colors distinguished
set fillchars+=vert:\ 
"set encoding
set encoding=utf-8

set tabstop=2
set shiftwidth=2
set expandtab
set number
set relativenumber
set cursorline
set visualbell
" Help the filetype system out a bit
au BufNewFile,BufRead *.frag,*.vert,*.fp,*.vp,*.glsl set syntax=glsl 
au BufNewFile,BufRead *.bcs set syntax=bc
filetype plugin indent on
"set smartindent
set nospell
set nowrap
"Turn off highlighting
nnoremap  <F3>     :noh<CR>
"Make latex-suite use latex highlighting
let g:tex_flavor='latex'

function! FileTypeSpecialEnables()
  if &ft == 'c' || &ft == 'cpp'
    "clang_complete - C/C++ completiong using clang
    "  Disable preview buffer, we copen'd already
    " set completeopt-=preview
    "  enable completion automatically
    let g:clang_complete_auto = 1
    nmap <C-m> :call ClangUpdateQuickFix()<CR>
    au BufWrite *.c,*.h,*.cpp,*.hpp :call ClangUpdateQuickFix()

    "delimitMate - expand {<CR> to {<CR>}<ESC>O
    let g:delimitMate_expand_cr=1

    " Use the same config file for syntastic and clang_complete
    let g:syntastic_cpp_config_file='.clang_complete'
    "We want to fold things syntax style for c files
    set foldmethod=syntax
  elseif &ft == 'tex'
    "Do special things for tex files
    set wrap
    set spell
    call matchdelete(g:cc_match_group)
  elseif &ft == 'dart'
    " Disable syntastic autochecking for dart files because dartanalyzer is
    " incredibly slow
    SyntasticToggleMode
  endif
endfunction

"We don't want things to be autofolded
set foldlevelstart=99
" "We also want to save folds when files close
" autocmd BufWinLeave *.* mkview
" autocmd BufWinEnter *.* silent loadview
"Show when a column slops over
let g:cc_match_group = matchadd('ColorColumn', '\%121v', 100)
"Show trailing spaces
set list
exec "set listchars=tab:\\|\\ ,trail:\uF8"

autocmd BufNewFile,BufRead * call FileTypeSpecialEnables()

"  " open nerdtree
"  function! StartNerdtree()
"    if 0 == argc()
"        NERDTree
"    end
"  endfunction
"  
"  au VimEnter * call StartNerdtree()
"  autocmd VimEnter * nmap <F3> :NERDTreeToggle<CR>
"  autocmd VimEnter * imap <F3> <Esc>:NERDTreeToggle<CR>a
"  let NERDTreeQuitOnOpen=1
"  let NERDTreeWinSize=35

" disable dumb gentoo word width stuff
autocmd BufNewFile,BufRead * set textwidth=0

" neovim usability stuff
if has('nvim')
  " enable python support
  runtime! plugin/plugin_setup.vim
  set backspace=indent,eol,start
endif
