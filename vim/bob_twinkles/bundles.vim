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

" completion plugins
NeoBundle 'Shougo/neocomplete.vim'
if has('nvim')
  " use YCM for neovim because neocomplete doesn't worky =(
  NeoBundle 'Valloric/YouCompleteMe', {
       \ 'build' : {
       \     'mac' : './install.sh --clang-completer --system-libclang --omnisharp-completer',
       \     'unix' : './install.sh --clang-completer --system-libclang --omnisharp-completer',
       \     'windows' : './install.sh --clang-completer --system-libclang --omnisharp-completer',
       \     'cygwin' : './install.sh --clang-completer --system-libclang --omnisharp-completer'
       \    }
       \ }
else
  " Otherwise use the lighter-weight clang_complete
  NeoBundle 'Rip-Rip/clang_complete'
endif

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

"
" assorted language plugins
"
NeoBundle 'dart-lang/dart-vim-plugin.git'
NeoBundle 'derekwyatt/vim-scala.git'

" haskell pacakges
NeoBundle 'bitc/vim-hdevtools'
NeoBundle 'eagletmt/ghcmod-vim'
NeoBundle 'eagletmt/neco-ghc'
NeoBundle 'lukerandall/haskellmode-vim'

NeoBundleCheck
