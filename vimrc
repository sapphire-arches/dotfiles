"
" Neobundle config
"

if has('vim_starting')
  set nocompatible

  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

call neobundle#begin(expand('~/.vim/bundle/'))

NeoBundleFetch 'Shougo/neobundle.vim'

call neobundle#end()

NeoBundle 'airblade/vim-gitgutter.git'
NeoBundle 'Shougo/neocomplete.vim.git'
NeoBundle 'Rip-Rip/clang_complete'
NeoBundle 'osyo-manga/vim-marching.git'
NeoBundle 'Shougo/vimproc.vim', {
      \ 'build' : {
      \     'windows' : 'tools\\update-dll-mingw',
      \     'cygwin' : 'make -f make_cygwin.mak',
      \     'mac' : 'make -f make_mac.mak',
      \     'linux' : 'make',
      \     'unix' : 'gmake',
      \    },
      \ }
NeoBundle 'osyo-manga/vim-reunions'
NeoBundle 'flazz/vim-colorschemes.git'
NeoBundle 'Raimondi/delimitMate.git'
NeoBundle 'scrooloose/syntastic.git'
NeoBundle 'dart-lang/dart-vim-plugin.git'
NeoBundle 'derekwyatt/vim-scala.git'
NeoBundle 'godlygeek/csapprox'
NeoBundleCheck

"GitGutter - show diff status when writing
let g:gitgutter_sign_column_always = 1

"
" NeoComplete setup
"

" Use neocomplete.
let g:neocomplete#enable_at_startup = 1

if !exists('g:neocomplete#keyword_patterns')
  let g:neocomplete#keyword_patterns = {}
endif
let g:neocomplete#keyword_patterns['default'] = '\h\w*'

if !exists('g:neocomplete#sources#omni#input_patterns')
  let g:neocomplete#sources#omni#input_patterns = {}
endif

"
" Neocomplete <-> clang_complete setup
"
if !exists('g:neocomplete#force_omni_input_patterns')
  let g:neocomplete#force_omni_input_patterns = {}
endif
let g:neocomplete#force_omni_input_patterns.c =
      \ '[^.[:digit:] *\t]\%(\.\|->\)\w*'
let g:neocomplete#force_omni_input_patterns.cpp =
      \ '[^.[:digit:] *\t]\%(\.\|->\)\w*\|\h\w*::\w*'
let g:neocomplete#force_omni_input_patterns.objc =
      \ '\[\h\w*\s\h\?\|\h\w*\%(\.\|->\)'
let g:neocomplete#force_omni_input_patterns.objcpp =
      \ '\[\h\w*\s\h\?\|\h\w*\%(\.\|->\)\|\h\w*::\w*'
let g:clang_complete_auto = 0
let g:clang_auto_select = 0
let g:clang_debug = 1

"
"setup status line
"
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

"
" custom tabline functions
"
function! MyTabLine()
    let s = '|'
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
    let raw_name = fnamemodify(bufname(buflist[winnr - 1]), ':.')
    echo raw_name
    " regex to emulate default vim tabline collapse (path/to/file -> p/t/file)
    let file = substitute(raw_name, '\([^/]\)[^/]\+/', '\1/', 'g')
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

"
" colorsheme selection
"

let g:CSApprox_loaded = 1
set background=dark
let s:colorscheme_choices = split("bubblegum distinguished gruvbox")
let s:colorscheme_choice = s:colorscheme_choices[reltime()[1] % len(s:colorscheme_choices)]
execute 'colors' s:colorscheme_choice
let g:colors_name=s:colorscheme_choice

set fillchars+=vert:\ 
"set encoding
set encoding=utf-8

"
" misc. setup
"
" tab stuff
set tabstop=2
set softtabstop=2
set shiftwidth=2
set expandtab
" current line number and jump numbers
set number
set relativenumber
" highlight background of current line
set cursorline
" BEEEP BEEEP BOOOOOOOP -> *blinky*
set visualbell
" highlight all search matches
set hlsearch
" display what will be tabbed through
set wildmenu

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

"
" custom command to set up an IDE-Like environment for c++
"
function! CPPNew(fname)
  let s:fname = fnamemodify(a:fname, ':r')
  let s:cname = s:fname . '.cpp'
  let s:hname = s:fname . '.hpp'
  echom s:cname
  exe "tabnew " . fnameescape(s:cname)
  exe "vs " . fnameescape(s:hname)
endfunction
command! -nargs=1 -complete=file CPPOpen call CPPNew("<args>")

"
" Open C++ file under cursor
"
function! CPPRead()
  wincmd gf
  let s:opened_fname = bufname("%")
  let s:fname = join(split(s:opened_fname, '\.')[:-2], '.')
  let s:opened_ext = fnamemodify(s:opened_fname, ':e')

  if s:opened_ext == 'cpp'
    exe "vs " . s:fname . '.hpp'
  elseif s:opened_ext == 'hpp'
    exe "vs " . s:fname . '.cpp'
    wincmd r
  endif
endfunction
nnoremap  <F2> :call CPPRead()<CR>

"
" close tab
"
nnoremap  <F10> :tabc<CR>

"
" some filetypes need extra configuration done once plugins have all loaded
"
function! FileTypeSpecialEnables()
  if &ft == 'c' || &ft == 'cpp'
    "  Disable preview buffer, we copen'd already
    set completeopt-=preview

    "delimitMate - expand {<CR> to {<CR>}<ESC>O
    let g:delimitMate_expand_cr=1

    " Use the same config file for syntastic and clang_complete
    let g:syntastic_cpp_config_file='.clang_complete'
    " We want to check header files as well
    let g:syntastic_cpp_check_header=1
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
    let g:syntastic_mode_map = { "mode" : "passive" }
  elseif &ft == 'scala'
    " Disable syntastic autochecking for scala files because scala is
    " incredibly slow
    let g:syntastic_mode_map = { "mode" : "passive" }
  elseif &ft == 'haskell'
    " 4 space tabs
    set ts=4
    set sts=4 " delete complete 'tab' in one keystroke
    set sw=4
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

" disable dumb gentoo word width stuff
autocmd BufNewFile,BufRead * set textwidth=0

" neovim usability stuff
if has('nvim')
  " enable python support
  runtime! plugin/plugin_setup.vim
  set backspace=indent,eol,start
endif

"
" make :W work because releasing shift is hard
"
"

command! W :w
