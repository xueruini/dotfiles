export CLICOLOR=1
export PS1='\[\033[34m\]\u\[\033[33m@\[\033[36m\]\h:\[\033[35m\]\w\[\033[32m\]$(__git_ps1 " (%s)")\[\033[31m\]\n\$\[\033[m\] '

alias rm="rm -i"

# brew
export PATH=/usr/local/bin:$PATH
export PATH="$(brew --prefix homebrew/php/php56)/bin:$PATH"

# bash-completion
if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi

# z
. `brew --prefix`/etc/profile.d/z.sh

# aws
complete -C aws_completer aws

## use vim to read man page
#function man() {
#  /usr/bin/man $* | col -b | vim -R -c 'set ft=man nomod nolist' -
#}

man() {
  env \
    LESS_TERMCAP_mb=$(printf "\e[1;31m") \
    LESS_TERMCAP_md=$(printf "\e[1;34m") \
    LESS_TERMCAP_me=$(printf "\e[0m") \
    LESS_TERMCAP_se=$(printf "\e[0m") \
    LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
    LESS_TERMCAP_ue=$(printf "\e[0m") \
    LESS_TERMCAP_us=$(printf "\e[1;31m") \
  man "$@"
}
