"
" Random UI tweaks
"

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
"set statusline+=%{SyntasticStatuslineFlag()}
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
    let s .= a:n - 1
    let s .= '] '
    let s .= ((mod)?'*':'') . file
    let s .= ' (' . numBuffers . ')'
    return s
endfunction

set tabline=%!MyTabLine()
set showtabline=2
set laststatus=2

"
" colorsheme selection
"

let g:CSApprox_loaded = 1
let s:colorscheme_choices = split("bubblegum gruvbox")
let s:colorscheme_choice = s:colorscheme_choices[reltime()[1] % len(s:colorscheme_choices)]
execute 'colors' s:colorscheme_choice
" For whatever reason, some colorschemes only pick up the 'dark' setting if you do it after sourcing them
set background=dark
let g:colors_name=s:colorscheme_choice

" Make the vertical seperator not a character
set fillchars+=vert:\ 

"GitGutter - show diff status when writing
let g:gitgutter_sign_column_always = 1

"We don't want things to be autofolded
set foldlevelstart=99
"Show when a column slops over
let g:cc_match_group = matchadd('ColorColumn', '\%121v', 100)
"Show trailing spaces
set list
exec "set listchars=tab:\\|\\ ,trail:\uF8"

syntax on

"set encoding
set encoding=utf-8

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
