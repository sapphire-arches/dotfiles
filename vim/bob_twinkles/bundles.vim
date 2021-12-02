"
" Neobundle config
"

call plug#begin(expand('~/.vim/bundle/'))

" completion plugins
Plug 'neoclide/coc.nvim', {'branch': 'release'}

" Utility/library bundles
Plug 'osyo-manga/vim-marching'
" Plug 'Shougo/vimproc.vim', {
"       \ 'build' : {
"       \     'windows' : 'tools\\update-dll-mingw',
"       \     'cygwin' : 'make -f make_cygwin.mak',
"       \     'mac' : 'make -f make_mac.mak',
"       \     'linux' : 'make',
"       \     'unix' : 'gmake',
"       \    },
"       \ }
Plug 'osyo-manga/vim-reunions'

"
" plugins for IDE stuff
"
Plug 'Raimondi/delimitMate'
Plug 'airblade/vim-gitgutter'
Plug 'scrooloose/syntastic'

"colorscheme plugins
Plug 'flazz/vim-colorschemes'
Plug 'godlygeek/csapprox'

"
" assorted language plugins
"
Plug 'dart-lang/dart-vim-plugin'
Plug 'ElmCast/elm-vim'
" Plug 'derekwyatt/vim-scala.git'

" " haskell pacakges
" Plug 'bitc/vim-hdevtools'
" Plug 'eagletmt/ghcmod-vim'
" Plug 'eagletmt/neco-ghc'
" Plug 'lukerandall/haskellmode-vim'
" 

" postgresql syntax files
Plug 'exu/pgsql.vim'

" Sort motion
Plug 'christoomey/vim-sort-motion'

" Surround stuff
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'

call plug#end()

" PlugCheck
