#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# get current git branch name
function git_branch {
    export gitbranch=[$(git rev-parse --abbrev-ref HEAD 2>/dev/null)]
    if [ "$?" -ne 0 ]
      then gitbranch=
    fi
    if [[ "${gitbranch}" == "[]" ]]
      then gitbranch=
    fi
}

function lambda_color {
    branch=$(git status -uno 2>/dev/null)
    if [[ $branch == *master* ]]
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
 
export EDITOR=vim
#export PROMPT_COMMAND='settitle; git_branch; history -a;'
export PROMPT_COMMAND='settitle; lambda_color; history -a;'
#export PS1='\[\e${usercolor}\][\u]\[\e${gitcolor}\]${gitbranch}\[\e${cwdcolor}\][$PWD]\[\e${inputcolor}\] ➤ '
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
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

alias ls='ls --color=auto'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -ltr'
alias f='sudo find / | grep '

export PATH=$HOME/npm-global/bin:$PATH:$HOME/.gem/ruby/2.4.0/bin:$PATH:$HOME/bin:$PATH
