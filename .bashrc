#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

function lambda_color {
    branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)
    if [[ $branch == *master* || $branch == *main* ]]
    then
        lambdacolor='[1;31m';
    elif [[ $branch == "" ]]
    then
        lambdacolor='[1;32m';
    else
        lambdacolor='[1;33m';
    fi
}

# Set prompt and window title
inputcolor='[0;37m'
cwdcolor='[0;34m'
gitcolor='[1;31m'
lambda_color

# Setup for window title
export TTYNAME=$$
function settitle() {
  p=$(pwd);
  let l=${#p}-25
  if [ "$l" -gt "0" ]; then
    p=..${p:${l}}
  fi
  t="$TTYNAME $p"
  echo -ne "\e]2;$t\a\e]1;$t\a";
}

function jcurl() {
    curl "$@" | json_pp | pygmentize -l json
}

export jcurl

export EDITOR=vim
export PROMPT_COMMAND='settitle; lambda_color; history -a;'
export PS1='\[\e${lambdacolor}\]λ\[\e${inputcolor}\] '
export PS2=' | '

unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

alias vim='nvim'
alias ls='ls --color=auto'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -ltr'

export NPM_GLOBAL="${HOME}/.npm-global"
export NODE_PATH="$NPM_GLOBAL/lib/node_modules:$NODE_PATH"
export PATH=$PATH:$NPM_GLOBAL/bin:$HOME/.gem/ruby/2.4.0/bin:$HOME/bin:$HOME/.local/bin:$HOME/.cabal/bin

[ -f "/home/bendo/.ghcup/env" ] && source "/home/bendo/.ghcup/env" # ghcup-env

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

