export CLICOLOR=1
export PS1='\[\033[34m\]\u\[\033[33m@\[\033[36m\]\h:\[\033[35m\]\w\[\033[32m\]$(__git_ps1 " (%s)")\[\033[31m\]\n\$\[\033[m\] '

alias rm="rm -i"

# brew
export HOMEBREW_NO_ANALYTICS=1

# bash-completion
if [ -f $(brew --prefix)/etc/bash_completion ]; then
  source $(brew --prefix)/etc/bash_completion
fi
complete -C aws_completer aws

# fasd
fasd_cache="$HOME/.fasd-init-bash"
if [ "$(command -v fasd)" -nt "$fasd_cache" -o ! -s "$fasd_cache" ]; then
  fasd --init posix-alias bash-hook bash-ccomp bash-ccomp-install >| "$fasd_cache"
fi
source "$fasd_cache"
unset fasd_cache

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

# GOPATH
export GOPATH=$HOME/Documents/gocode
export GOROOT=$(go env GOROOT)
export PATH=$GOPATH/bin:$PATH

# pip should only run if there is a virtualenv currently activated
# export PIP_REQUIRE_VIRTUALENV=true

# pyenv
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
