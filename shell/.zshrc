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
export HOMEBREW_NO_AUTO_UPDATE=1
export HOMEBREW_NO_INSTALL_CLEANUP=1

# zinit via brew
source $(brew --prefix)/opt/zinit/zinit.zsh

# oh-my-zsh cache dir (used by OMZ libs)
export ZSH_CACHE_DIR="${XDG_CACHE_HOME:-$HOME/.cache}/zinit-omz"
[[ -d "$ZSH_CACHE_DIR" ]] || mkdir -p "$ZSH_CACHE_DIR"

# oh-my-zsh libs (replaces `antigen use oh-my-zsh`)
for _omz_lib in functions clipboard completion directories git grep history \
                key-bindings misc spectrum termsupport theme-and-appearance; do
  zinit snippet OMZL::${_omz_lib}.zsh
done
unset _omz_lib

# needed by the fzf bindkeys below, so load it synchronously
zinit snippet OMZP::fzf

# oh-my-zsh plugins (turbo)
zinit wait lucid for \
  OMZP::common-aliases \
  OMZP::colored-man-pages \
  OMZP::command-not-found \
  OMZP::brew \
  OMZP::git \
  OMZP::sudo \
  OMZP::tig \
  OMZP::zoxide

# fzf-tab
zstyle ':completion:*' menu no
zstyle ':completion:*:descriptions' format '[%d]'
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'ls -1 $realpath'

# completions, fzf-tab, highlighting, suggestions (turbo, order matters)
zinit wait lucid for \
  blockf \
    zsh-users/zsh-completions \
  atinit"zicompinit; zicdreplay" \
    Aloxaf/fzf-tab \
  zdharma-continuum/fast-syntax-highlighting \
  atload"!_zsh_autosuggest_start" \
    zsh-users/zsh-autosuggestions \
  atload'bindkey "^[[A" history-substring-search-up; bindkey "^[[B" history-substring-search-down' \
    zsh-users/zsh-history-substring-search

# pip should only run if there is a virtualenv currently activated
export PIP_REQUIRE_VIRTUALENV=true

## fzf
## I love the original ctrl-t
bindkey '^X^T' fzf-file-widget
bindkey '^T' transpose-chars
# source <(fzf --zsh)

# vcpkg
export VCPKG_ROOT="$HOME/vcpkg"

# let git always show engligh
alias git='LANG=en_US.UTF-8 git'

source "$HOME/.local/bin/env"

# starship prompt
eval "$(starship init zsh)"
