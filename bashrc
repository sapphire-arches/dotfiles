#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

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

wired_connection_setup() {
    sudo ip link set eth0 up
    sudo dhcpcd eth0
}

external_monitor_setup() {
    if [ ! -n $2 ]
    then
        echo "Specify width and height"
    fi
    xrandr --newmode $(gtf $1 $2 60 | grep Modeline | cut -d " " -f 4- | sed s/\"//g)
    xrandr --addmode VGA1 "1680x1050_60.00"
    xrandr --output VGA1 --mode 1680x1050_60.00 --right-of LVDS1
}

alias ls='ls -h --color=auto'
alias q='exit'
alias sinktheship='sudo shutdown -h 0'
alias minecraft=~/Desktop/tekkit
alias l=ls

export _JAVA_AWT_WM_NONREPARENTING=1
export PYTHONPATH='/home/bob_twinkles/Code/python/thebigdb/thebigdb-python/'

#source /usr/share/git/completion/git-completion.bash
export PATH="$PATH:/home/bob_twinkles/Scripts"
PATH="/usr/local/heroku/bin:$PATH"
export EDITOR='vim'

function prompt {
    export txtblk='\[\e[0;30m\]' # Black - Regular
    export txtred='\[\e[0;31m\]' # Red
    export txtgrn='\[\e[0;32m\]' # Green
    export txtylw='\[\e[0;33m\]' # Yellow
    export txtblu='\[\e[0;34m\]' # Blue
    export txtpur='\[\e[0;35m\]' # Purple
    export txtcyn='\[\e[0;36m\]' # Cyan
    export txtwht='\[\e[0;37m\]' # White
    export bldblk='\[\e[1;30m\]' # Black - Bold
    export bldred='\[\e[1;31m\]' # Red
    export bldgrn='\[\e[1;32m\]' # Green
    export bldylw='\[\e[1;33m\]' # Yellow
    export bldblu='\[\e[1;34m\]' # Blue
    export bldpur='\[\e[1;35m\]' # Purple
    export bldcyn='\[\e[1;36m\]' # Cyan
    export bldwht='\[\e[1;37m\]' # White
    export unkblk='\[\e[4;30m\]' # Black - Underline
    export undred='\[\e[4;31m\]' # Red
    export undgrn='\[\e[4;32m\]' # Green
    export undylw='\[\e[4;33m\]' # Yellow
    export undblu='\[\e[4;34m\]' # Blue
    export undpur='\[\e[4;35m\]' # Purple
    export undcyn='\[\e[4;36m\]' # Cyan
    export undwht='\[\e[4;37m\]' # White
    export bakblk='\[\e[40m\]'   # Black - Background
    export bakred='\[\e[41m\]'   # Red
    export bakgrn='\[\e[42m\]'   # Green
    export bakylw='\[\e[43m\]'   # Yellow
    export bakblu='\[\e[44m\]'   # Blue
    export bakpur='\[\e[45m\]'   # Purple
    export bakcyn='\[\e[46m\]'   # Cyan
    export bakwht='\[\e[47m\]'   # White
    export txtrst='\[\e[0m\]'    # Text Reset
    export PS1="[${txtcyn}\j${txtrst}][${txtgrn}\u${txtrst}@${txtred}\h${txtrst} \w]\$ "
}
prompt

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
#    clear #for background artifacting
fi

if [ $TERM = 'xterm' ]; then
    export TERM='xterm-256color'
fi

export MANPAGER=most
