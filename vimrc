"
" Run utility stuff (see ~/vim)
"
ru bob_twinkles/bundles.vim
ru bob_twinkles/completion.vim
ru bob_twinkles/ui.vim
ru bob_twinkles/editing.vim

" Help the filetype system out a bit
au BufNewFile,BufRead *.frag,*.vert,*.fp,*.vp,*.glsl set syntax=glsl
au BufNewFile,BufRead *.bcs set syntax=bc
filetype plugin indent on

"
" custom command to open environments where there are 2 important files
" C/C++ header/source files are a good example of thise
"
function! FriendNew(fname)
  " mapping from ending to the setup it should create
  let s:friends = {
    \ 'c' : ['c', 'h'],
    \ 'h' : ['c', 'h'],
    \ 'cpp' : ['cpp', 'hpp'],
    \ 'hpp' : ['cpp', 'hpp'],
    \ 'frag' : ['vert', 'frag'],
    \ 'vert' : ['vert', 'frag'],
    \ }

  let s:fname = fnamemodify(a:fname, ':r')
  let s:fsuffix = fnamemodify(a:fname, ':e')
  if has_key(s:friends, s:fsuffix)
    let s:friendlist = s:friends[s:fsuffix]
    exe "tabnew " . s:fname . '.' . fnameescape(s:friendlist[0])
    for i in s:friendlist[1:]
      exe "vs " . s:fname . '.' . fnameescape(i)
    endfor
  else
    exe "tabnew " . a:fname
  endif
endfunction
command! -nargs=1 -complete=file FriendOpen call FriendNew("<args>")

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

    if has('nvim')
      " map Ycm's GoTo
      nnoremap <leader>jd :YcmCompleter GoTo<cr>
    endif
  elseif &ft == 'tex'
    "Do special things for tex files
    set wrap
    set spell
    call matchdelete(g:cc_match_group)
    "automatically save and 'compile' tex files when we leave insert mode
    " augroup texcompile
    "   autocmd!
    "   autocmd BufWritePost *.tex execute "!texi2pdf --clean %"
    "   autocmd BufWritePost *.tex execute ":redraw!"
    " augroup end
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

autocmd BufNewFile,BufRead * call FileTypeSpecialEnables()

" disable dumb gentoo word width stuff
autocmd BufNewFile,BufRead * set textwidth=0

" haskell setup
let g:haddock_browser = 'firefox'

" neovim usability stuff
if has('nvim')
  set backspace=indent,eol,start

  " use two escapes to exit terminals instead of <c-\><c-n>
  tmap <esc><esc> <c-\><c-n>
endif

"
" make :W work because releasing shift is hard
"
"

command! W :w
