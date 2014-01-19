#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

. $HOME/.shell_config.sh

#######################################
#          PROMPT STUFF               #
#######################################

function orun_prompt_modify() {
    export PS1="(orun)$PS1"
}

function test_is_orun() {
    if [ $IS_ORUN ]; then
        if [ $IS_ORUN = "true" ]; then
            orun_prompt_modify
            return
        fi
    fi
    for i in $(pgrep optirun)
    do
        if [ $i = $PPID ]; then
            orun_prompt_modify
            export IS_ORUN="true"
            break
        fi
    done
}

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

    test_is_orun
}
prompt
