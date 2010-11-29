# Prompt
##RPROMPT='%/'
#PROMPT='%{[36m%}%n%{[35m%}@%{[34m%}%M %{[33m%}%D %T  %{[32m%}%/ 
#%{[31m%}>>%{[m%}'
autoload -U promptinit
promptinit
prompt -s adam2

# History
# number of lines kept in history
export HISTSIZE=10000
# number of lines saved in the history after logout
export SAVEHIST=10000
# location of history
export HISTFILE=~/.zhistory
# append command to history file once executed
setopt INC_APPEND_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_REDUCE_BLANKS

# Disable core dumps
limit coredumpsize 0

# show jobs upon exit
setopt CHECK_JOBS

# Emacs key bindings
bindkey -e

## special keys
#bindkey "\e[3~" delete-char
#bindkey "\e[1~" beginning-of-line
#bindkey "\e[4~" end-of-line
#bindkey "\e[5~" beginning-of-history
#bindkey "\e[6~" end-of-history
#bindkey "\e[3~" delete-char
#bindkey "\e[2~" quoted-insert
#bindkey "\e[5C" forward-word
#bindkey "\eOc" emacs-forward-word
#bindkey "\e[5D" backward-word
#bindkey "\eOd" emacs-backward-word
#bindkey "\e\e[C" forward-word
#bindkey "\e\e[D" backward-word
## for rxvt
#bindkey "\e[8~" end-of-line
#bindkey "\e[7~" beginning-of-line
## for non RH/Debian xterm, can't hurt for RH/Debian xterm
#bindkey "\eOH" beginning-of-line
#bindkey "\eOF" end-of-line
## for freebsd console
#bindkey "\e[H" beginning-of-line
#bindkey "\e[F" end-of-line

## again special
#typeset -g -A key
#bindkey '^?' backward-delete-char
#bindkey '^[[1~' beginning-of-line
#bindkey '^[[5~' up-line-or-history
#bindkey '^[[3~' delete-char
#bindkey '^[[4~' end-of-line
#bindkey '^[[6~' down-line-or-history
#bindkey '^[[A' up-line-or-search
#bindkey '^[[D' backward-char
#bindkey '^[[B' down-line-or-search
#bindkey '^[[C' forward-char 
## completion in the middle of a line
bindkey '^i' expand-or-complete-prefix

# regard them as part of a word
WORDCHARS='*?_-[]~=&;!#$%^(){}<>'

# Auto Completion
autoload -U compinit
compinit

setopt AUTO_LIST
setopt AUTO_MENU
setopt MENU_COMPLETE
# type dirname to enter
setopt AUTO_CD
# cd - [TAB]
setopt AUTO_PUSHD
setopt PUSHD_IGNORE_DUPS
setopt CDABLE_VARS
# cd /a/b/c --> cd /a???/b???/c???
setopt COMPLETE_IN_WORD
setopt EXTENDED_GLOB

# Completion caching
zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path ~/.zcache
zstyle ':completion:*:cd:*' ignore-parents parent pwd

# Completion Options
zstyle ':completion:*:match:*' original only
zstyle ':completion::prefix-1:*' completer _complete
zstyle ':completion:predict:*' completer _complete
zstyle ':completion:incremental:*' completer _complete _correct
zstyle ':completion:*' completer _complete _prefix _correct _prefix _match _approximate

# Path Expansion
zstyle ':completion:*' expand 'yes'
zstyle ':completion:*' squeeze-shlashes 'yes'
zstyle ':completion::complete:*' '\\'

# I do not like the additional "Enter"
# zstyle ':completion:*:*:*:default' menu yes select
zstyle ':completion:*:*:default' force-list always

# enable colors
[ -f /etc/DIR_COLORS ] && eval $(dircolors -b /etc/DIR_COLORS)
export ZLSCOLORS="${LS_COLORS}"
zmodload zsh/complist
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'

zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric

zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always
zstyle ':completion:*:*:*:*:processes' force-list always
zstyle ':completion:*:processes' command 'ps -au$USER'

# Group matches and Describe
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*:descriptions' format $'\e[01;33m -- %d --\e[0m'
zstyle ':completion:*:messages' format $'\e[01;35m -- %d --\e[0m'
zstyle ':completion:*:warnings' format $'\e[01;31m -- No Matches Found --\e[0m'
zstyle ':completion:*:corrections' format $'\e[01;32m -- %d (errors: %e) --\e[0m'

# cd ~
zstyle ':completion:*:-tilde-:*' group-order 'named-directories' 'path-directories' 'users' 'expand'

# alias
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'
alias ls='ls -F --color=auto'
alias ll='ls -l'
alias grep='grep --color=auto'
alias ss='svn status'
alias sd='svn diff | less -R'
alias sdh='svn diff -r HEAD | less -R'
alias en='emacs -nw'

# cd ~xxx
hash -d cwt="$HOME/lbs.svn/branches/cwt"

# misc
setopt CORRECT_ALL
rationalise-dot() {
  if [[ $LBUFFER = *.. ]]; then
    LBUFFER+=/..
  else
    LBUFFER+=.
  fi
}
zle -N rationalise-dot
bindkey . rationalise-dot
