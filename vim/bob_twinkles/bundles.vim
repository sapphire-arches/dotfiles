"
" Neobundle config
"

if has('vim_starting')
  set nocompatible

  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

call neobundle#begin(expand('~/.vim/bundle/'))

NeoBundleFetch 'Shougo/neobundle.vim'

" completion plugins
if has('lua')
  NeoBundle 'Shougo/neocomplete.vim'
endif
let ycm_install_options = '--clang-completer --system-libclang --omnisharp-completer --racer-completer'
NeoBundle 'Valloric/YouCompleteMe', {
     \ 'build' : {
     \     'mac' : './install.sh ' . g:ycm_install_options,
     \     'unix' : './install.sh ' . g:ycm_install_options,
     \     'windows' : './install.sh ' . g:ycm_install_options,
     \     'cygwin' : './install.sh ' . g:ycm_install_options
     \    }
     \ }

" Utility/library bundles
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

"
" plugins for IDE stuff
"
NeoBundle 'Raimondi/delimitMate.git'
NeoBundle 'airblade/vim-gitgutter.git'
NeoBundle 'scrooloose/syntastic.git'

"colorscheme plugins
NeoBundle 'flazz/vim-colorschemes.git'
NeoBundle 'godlygeek/csapprox'

" "
" " assorted language plugins
" "
" NeoBundle 'dart-lang/dart-vim-plugin.git'
" NeoBundle 'derekwyatt/vim-scala.git'
" 
" " haskell pacakges
" NeoBundle 'bitc/vim-hdevtools'
" NeoBundle 'eagletmt/ghcmod-vim'
" NeoBundle 'eagletmt/neco-ghc'
" NeoBundle 'lukerandall/haskellmode-vim'
" 

" postgresql syntax files
NeoBundle 'exu/pgsql.vim'

call neobundle#end()

NeoBundleCheck
