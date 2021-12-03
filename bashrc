#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

[ ! -z $WINDOWID ] && \
  which transset-df 2>/dev/null >/dev/null && \
  transset-df -i $WINDOWID 0.9 > /dev/null

. $HOME/.shell_config.sh

#######################################
#          PROMPT STUFF               #
#######################################

function __test_is_orun() {
    if [ $IS_ORUN ]; then
        if [ $IS_ORUN = "true" ]; then
            echo "(orun)"
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

function __lastcmd_color {
  if [ $1 != "0" ]
  then
    echo -n ${txtred}
  else
    echo -n ${txtgrn}
  fi
}

function __prompt_cmd {
  # Save exit status before we do anything else
  local EXIT="$?"
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

  # reset prompt
  PS1=""
  # see if we're in an optirun context
  PS1+=$(__test_is_orun)

  # Prompt components
  export pnjobs="${txtcyn}\j${txtrst}"
  export puser="${txtgrn}\u${txtrst}"
  export phost="${txtred}\h${txtrst}"
  export ppath="${txtpur}\w${txtrst}"

  if [[ $(locale charmap) == "UTF-8" ]]
  then
    br='┌'
    tr='└'
    lr='─'
    lterminator='╼'
    # first line
    PS1+="${br} ${pnjobs} ${lr} ${puser}@${phost} ${lr} ${ppath}"
    if [ ! -z ${VIRTUAL_ENV} ]
    then
      venv_relpath=$(python -c "import os.path; print(os.path.relpath('${VIRTUAL_ENV}', '$(pwd)'))")
      PS1+=" (${venv_relpath})"
    fi
    if [ ! -z ${IN_NIX_SHELL} ]
    then
      PS1+=" ${txtcyn}(NIX-SHELL $$|${name})${txtrst}"
    fi
    # Second line
    PS1+="\n${tr}"
    if [ $EXIT != "0" ]
    then
      PS1+="${txtred}${lterminator} ${txtblk}${bakred}$EXIT${txtrst}"
    else
      PS1+="${txtgrn}${lterminator}${txtrst}"
    fi
    PS1+=" \W >> "
  else
    export PS1="[${pnjobs}][${puser}@${phost} ${ppath}] \$ "
  fi
}

if [ -f ~/.nix-profile/share/fzf/key-bindings.bash ]
then
  source ~/.nix-profile/share/fzf/key-bindings.bash
  source ~/.nix-profile/share/fzf/completion.bash
fi

export PROMPT_COMMAND=__prompt_cmd
