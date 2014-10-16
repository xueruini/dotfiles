# for MacPorts
export PATH=/opt/local/bin:/opt/local/sbin:$PATH
export MANPATH=/opt/local/man:$MANPATH

# for X11
export DISPLAY=:0.0
export EDITOR=vi

# git auto completion
source $HOME/bin/git-completion.sh

export PS1='\[\033[1;31m\]\u\[\033[32m\]@\h:\[\033[35m\]\w$(__git_ps1 " (%s)")\[\033[31m\]\n\$\[\033[m\] '

##
# DELUXE-USR-LOCAL-BIN-INSERT
# (do not remove this comment)
##
echo $PATH | grep -q -s "/usr/local/bin"
if [ $? -eq 1 ] ; then
    PATH=$PATH:/usr/local/bin
    export PATH
fi

export CLICOLOR=1
alias ls="ls -Gv"
alias rm="rm -i"

# for luatex
# export OSFONTDIR="{$HOME/Library/Fonts,/Library/Fonts}"

# use vim to read man page
function man() {
  /usr/bin/man $* | col -b | /usr/bin/vim -R -c 'set ft=man nomod nolist' -
}