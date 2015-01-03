#######################################
#       CONVIENIENCE FUNCTIONS        #
#######################################
prefix() {
    export WINEPREFIX="$HOME/.local/share/wineprefixes/$1"
    export WINEDEBUG=-all
    export WINEARCH=win32
}

goc() {
    cd $WINEPREFIX/drive_c
}

fun  () { fortune -a | fmt -80 -s | cowsay -$(shuf -n 1 -e b d g p s t w y) -f $(shuf -n 1 -e $(cowsay -l | tail -n +2)) -n; }

lsp() {
    ls $* $HOME/.local/share/wineprefixes
}

unfreeze_ff() {
  pgrep fire | tail -n 1 | xargs kill
}

wired_connection_setup() {
    sudo ip link set eth0 up
    sudo dhcpcd eth0
}

external_monitor_setup() {
    if [ ! -n $3 ]
    then
        echo "Specify display, width, and height"
    fi
    xrandr --newmode $(gtf $2 $3 60 | grep Modeline | cut -d " " -f 4- | sed s/\"//g)
    xrandr --addmode $1 "1680x1050_60.00"
    xrandr --output $1 --mode 1680x1050_60.00 --right-of LVDS1
}

#fix colors on the framebuffer terms.
if [ $TERM = linux ]; then
    echo -en "\e]P0232323" #black
    echo -en "\e]P82B2B2B" #darkgrey
    echo -en "\e]P1D75F5F" #darkred
    echo -en "\e]P9E33636" #red
    echo -en "\e]P287AF5F" #darkgreen
    echo -en "\e]PA98E34D" #green
    echo -en "\e]P3D7AF87" #brown
    echo -en "\e]PBFFD75F" #yellow
    echo -en "\e]P48787AF" #darkblue
    echo -en "\e]PC7373C9" #blue
    echo -en "\e]P5BD53A5" #darkmagenta
    echo -en "\e]PDD633B2" #magenta
    echo -en "\e]P65FAFAF" #darkcyan
    echo -en "\e]PE44C9C9" #cyan
    echo -en "\e]P7E5E5E5" #lightgrey
    echo -en "\e]PFFFFFFF" #white
    clear #for background artifacting
fi

#######################################
#         RANDOM ALIASES              #
#######################################
alias ls='ls -h --color=auto'
alias q='exit'
alias sinktheship='sudo shutdown -h 0'
alias dmake='CC=clang CFLAGS=-g make'
alias l=ls
alias bc='bc -l'

#######################################
#           PREFERENCES               #
#######################################
export MANPAGER=most
export EDITOR='vim'

# Fix Java/XMonad being derpy as hell
export _JAVA_AWT_WM_NONREPARENTING=1

#######################################
#            PATH MODS                #
#######################################

export PYTHONPATH='/home/bob_twinkles/Code/python/thebigdb/thebigdb-python/'
export PATH="$PATH:$HOME/.cabal/bin:$HOME/Scripts"
export PATH="/usr/local/heroku/bin:$PATH"
export PATH="$HOME/Scripts/overrides:$PATH"

# The probability of this causing bad behavior is pretty low
if [ $TERM = 'xterm' ]; then
    export TERM='xterm-256color'
fi
