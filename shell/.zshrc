# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block, everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

## ZSH-specific shell environment flags only relvant to interactive shells
# Variable behaviors
setopt NO_ALL_EXPORT		# Don't export all variables to environment

# Directory changing
setopt AUTO_CD			# cd to a directory if it's given without a command
setopt CDABLE_VARS		# Try to cd to variable value from ~ if no leading slash
setopt NO_AUTO_PUSHD      	# Prevent all directories from being automatically pushed onto the stack
setopt PUSHD_IGNORE_DUPS	# Directory only appears once on the stack
#setopt PUSHD_SILENT		# No non-error messages from pushd
setopt PUSHD_TO_HOME		# pushd with no arguments goes to ~

# Completion
#setopt AUTO_LIST         # (Default) Automatically list ambiguous completion choices
#setopt AUTO_MENU         # (Default) Automatically use menu completion after second completion request
#setopt AUTO_REMOVE_SLASH # (Default) Trailing / in completion is removed
setopt MENU_COMPLETE	  # Cycle through completions by completing in place
setopt NO_LIST_BEEP       # Prevent beeping on ambiguous completion

# Globbing
setopt EXTENDED_GLOB	# Allow globbing qualifiers and other extensions
# cd /a/b/c --> cd /a???/b???/c???
setopt COMPLETE_IN_WORD
setopt GLOB_DOTS	# Patterns may match without leading periods
setopt NOMATCH		# Throw error if a glob fails to match

# History behavior
export HISTSIZE=10000		# number of lines kept in history
export SAVEHIST=10000 		# number of lines saved in the history after logout
setopt INC_APPEND_HISTORY 	# append command to history file once executed
setopt HIST_IGNORE_DUPS
setopt NO_HIST_BEEP       	# Don't beep on failed history lookups
setopt HIST_IGNORE_SPACE	# Do not store lines starting with space
setopt HIST_REDUCE_BLANKS	# Trim multiple insignificant blanks
setopt NO_HIST_VERIFY		# Don't show expanded line for editing
setopt BANG_HIST		# ! style history is allowed
setopt INTERACTIVE_COMMENTS  	# Allow comments to be added; Helpful for history lookups

# Background jobs
setopt AUTO_CONTINUE    # Ensure a stopped job is continued when disowned
setopt NO_BG_NICE	# Don't lower priority of background jobs
setopt CHECK_JOBS       # Report status of background jobs when exitting a shell
setopt LONG_LIST_JOBS	# More verbose listing of jobs
setopt NOTIFY		# Notify of background job changes as soon as they happen

# Miscellaneous
setopt NO_BEEP		# Do not beep on line editor errors
setopt NO_CORRECT	# Don't suggest corrections for misspelled commands
bindkey -e          # Emacs key bindings

# Disable core dumps
limit coredumpsize 0

# # Clever dot expansion
# rationalise-dot() {
#   if [[ $LBUFFER = *.. ]]; then
#     LBUFFER+=/..
#   else
#     LBUFFER+=.
#   fi
# }
# zle -N rationalise-dot
# bindkey . rationalise-dot

# add brew
eval "$(brew shellenv)"
export HOMEBREW_NO_ANALYTICS=1

# antigen via brew
source $(brew --prefix)/share/antigen/antigen.zsh

antigen use oh-my-zsh

antigen bundle common-aliases
antigen bundle colored-man-pages
antigen bundle colorize
antigen bundle command-not-found
antigen bundle history
antigen bundle history-substring-search
antigen bundle macos
antigen bundle vagrant
antigen bundle vagrant-prompt
antigen bundle docker
# antigen bundle docker-compose
antigen bundle brew
antigen bundle brew-cask
# C-x a to expand the alias under the cursor
# antigen bundle globalias
antigen bundle git
antigen bundle git-extras
antigen bundle git-prompt
antigen bundle man
antigen bundle python
antigen bundle pip
# antigen bundle pipenv
# antigen bundle rbenv
# antigen bundle repo
# antigen bundle rsync
# antigen bundle virtualenv
# antigen bundle lein
# antigen bundle golang
antigen bundle sudo
# antigen bundle nvm
antigen bundle npm
antigen bundle node
antigen bundle httpie
antigen bundle tmux
antigen bundle tig
antigen bundle fasd
# antigen bundle zsh_reload
# fzf is better
# antigen bundle zsh-navigation-tools

antigen bundle zsh-users/zsh-completions src
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-autosuggestions
# antigen bundle zsh-users/fizsh

# antigen theme jreese
# antigen theme pygmalion
# antigen theme xueruini/oh-my-zsh-seeker-theme seeker
# antigen theme gnzh
antigen theme romkatv/powerlevel10k

antigen apply

# pip should only run if there is a virtualenv currently activated
# export PIP_REQUIRE_VIRTUALENV=true

## bundle pyenv works
# eval "$(pyenv init -)"
# if which pyenv-virtualenv-init > /dev/null; then
#   eval "$(pyenv virtualenv-init -)"
# fi

# fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
# I love the original ctrl-t
bindkey '^X^T' fzf-file-widget
bindkey '^T' transpose-chars


# # GOPATH
# export GOPATH=$HOME/Documents/go
# export GOROOT=$(go env GOROOT)
# export PATH=$GOPATH/bin:$PATH
# # gvm
# [[ -s "$HOME/.gvm/scripts/gvm" ]] && source "$HOME/.gvm/scripts/gvm"
# gvm use go1.13.5

# nvm
export NVM_DIR=$HOME/.nvm
source $(brew --prefix nvm)/nvm.sh

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
