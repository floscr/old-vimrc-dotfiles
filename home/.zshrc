#!/bin/zsh
# -----------------------------------------------------------------------------
# ZSHRC
# -----------------------------------------------------------------------------
# Exports & Path Variable
# Oh-my-zsh Settings
# Shell Settings
# Configurations
# Plugins
# -----------------------------------------------------------------------------

# Load path from bashrc
. ./.bashrc

# -----------------------------------------------------------------------------
# Exports & PATH Variable
# -----------------------------------------------------------------------------

export SHELL=zsh

export PATH=$HOME/.gem/bin:$PATH

# Set architecture flags
export ARCHFLAGS="-arch x86_64"

# RVM
export PATH=$PATH:$HOME/.rvm/bin
export PATH="/usr/local/opt/imagemagick@6/bin:$PATH"

# Mysql
export PATH=/usr/local/mysql/bin:$PATH

# Homebrew bins
# Default bins
# export PATH=$PATH:/usr/bin:/bin:/usr/sbin:/sbin
# Ruby bins
# export PATH=$PATH:/usr/local/Cellar/ruby/2.0.0-p0/bin
# Custom Ruby gem path
export GEM_HOME=$HOME/.gem
export PATH=$GEM_HOME/bin:$PATH

# Custom binaries
export PATH=$PATH:$HOME/.dotfiles/bin

# Android Tools
export ANDROID_TOOLS="~/Library/Android/sdk/tools"
export ANDROID_PLATFORM_TOOLS="~/Library/Android/sdk/platform-tools"
export PATH=$PATH:$ANDROID_TOOLS:$ANDROID_PLATFORM_TOOLS

# PHP & Composer
export PATH="/usr/local/opt/php70/bin:$PATH"
export PATH=~/.composer/vendor/bin:$PATH

# Doom
export PATH=$PATH:~/.emacs.d/bin

# MySQL
# export PATH=/usr/local/mysql/bin:$PATH

# Haskell
export PATH=$HOME/.local/bin:$PATH

# Rust
export PATH=$HOME/.cargo/bin:$PATH

# Color Setup
# export COLORTERM=xterm-256color
# export TERM=xterm-256color

# Language
export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# PKG Config
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig/

# Dotfiles
export DOTFILES="$HOME/.dotfiles"
export DOTFILES_PLUGINS="$DOTFILES/plugins"
export DOTFILES_SCRIPTS="$DOTFILES/scripts"
export DOTFILES_TEMP="$DOTFILES/temp"

# Set VIM as editor
export GIT_EDITOR='nvim'
export VISUAL=nvim
export EDITOR="$VISUAL"

# Hide commands from history with two spaces
export HISTIGNORE='  *'

# Grep Colors
export GREP_OPTIONS='--color=always'
export GREP_COLOR='1;35;40'

# Color Profiles
export COLOR_ECI="/Library/Application Support/Adobe/Color/Profiles/Recommended/eciRGB_v2.icc"
export COLOR_SRGB="/Library/Application Support/Adobe/Color/Profiles/Recommended/sRGB Color Space Profile.icm"

# Go
export GOPATH=$HOME/golang
export GOROOT=/usr/local/opt/go/libexec
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:$GOROOT/bin

# Enable a different cursor in NVIM in insert mode
export NVIM_TUI_ENABLE_CURSOR_SHAPE=1

# Enable colors for cheat
export CHEATCOLORS=true

export DEFAULT_CHEAT_DIR=$HOME/Dropbox/Snippets
export CHEATPATH=$HOME/Dropbox/Snippets

# export PATH="/usr/local/opt/mysql@5.7/bin:$PATH"

# -----------------------------------------------------------------------------
# Oh-my-zsh Settings
# -----------------------------------------------------------------------------

# Tab Completion
autoload -U compinit
compinit

# Better Tabbing Styles
zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'

ZSH=$DOTFILES_PLUGINS/oh-my-zsh
ZSH_CUSTOM=$DOTFILES/zsh/custom

# ZSH THEME
# ~/.oh-my-zsh/themes/
ZSH_THEME="floscr"

# ZSH PlUGINS
plugins=(git osx history-substring-search encode64 fasd web-search fast-syntax-highlighting)

# # Lazy load zsh nvm plugin
# ZSH_NVM_PLUGIN=~/.dotfiles/zsh/custom/plugins/zsh-nvm/zsh-nvm.plugin.zsh
# if [[ -f "$ZSH_NVM_PLUGIN" ]]; then
#   export NVM_LAZY_LOAD=true
#   source ~/.dotfiles/zsh/custom/plugins/zsh-nvm/zsh-nvm.plugin.zsh
# fi

# Turn off auto update
# http://stackoverflow.com/questions/11378607/oh-my-zsh-disable-would-you-like-to-check-for-updates-prompt
DISABLE_AUTO_UPDATE=true

# Load up oh-my-zsh
. $ZSH/oh-my-zsh.sh

# Add custom completions to oh-my-zsh
fpath=($DOTFILES/functions/zsh-expansions /usr/local/share/zsh-completions $fpath)

# show completion menu when number of options is at least 2
zstyle ':completion:*' menu select=2

# -----------------------------------------------------------------------------
# Shell Settings
# -----------------------------------------------------------------------------

# Fixes the perl error on uberspace
# Setting for the new UTF-8 terminal support in Lion
LC_CTYPE=en_US.UTF-8
LC_ALL=en_US.UTF-8

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# set editing-mode vi # Set vi mode for zsh
bindkey -v          # Fix vi mode for tmux
export KEYTIMEOUT=1 # Remove the timeout
export DISABLE_AUTO_TITLE=true

bindkey '^P' up-history
bindkey '^N' down-history
bindkey '^?' backward-delete-char
# bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
bindkey '^r' history-incremental-search-backward
bindkey '^A' beginning-of-line
bindkey '^E' end-of-line
bindkey '^[[Z' reverse-menu-complete

setopt    correctall # correct misspelled commands
unsetopt  nomatch # no error if glob fails to expand (scp fix)

# HISTORY
setopt    INC_APPEND_HISTORY  # Append history file immediately
setopt    append_history # Allow multiple terminal sessions to all append to one zsh command history
setopt    HIST_IGNORE_SPACE
# HIST_REDUCE_BLANKS corrupts history.
# https://bugs.launchpad.net/ubuntu/+source/zsh/+bug/1334858
unsetopt  HIST_REDUCE_BLANKS
setopt    HIST_IGNORE_ALL_DUPS
setopt    EXTENDED_HISTORY
setopt    hist_expire_dups_first # when trimming history, lose oldest duplicates first

# Sounds
unsetopt  BEEP                # No beeps on error
unsetopt  HIST_BEEP           # No history beeps
unsetopt  LIST_BEEP           # No list beeps

unsetopt  CHASE_LINKS         # dont resolve symbolic links in cd

# COMPLETION
setopt always_to_end # When completing from the middle of a word, move the cursor to the end of the word
setopt complete_in_word # Allow completion from within a word/phrase

# PROMPT
setopt prompt_subst # Enable parameter expansion, command substitution, and arithmetic expansion in the prompt
unsetopt correct_all

# Edit the current command in vim
# https://jordanelver.co.uk/blog/2015/02/02/editing-the-commandline-in-vim/
# To edit current command: <ESC><v>
# :wq will return you to the command line
autoload edit-command-line
zle -N edit-command-line
bindkey -M vicmd v edit-command-line

# -----------------------------------------------------------------------------
# Configurations
# -----------------------------------------------------------------------------

# Configurations
source $DOTFILES/.aliases       # Aliases
source $DOTFILES/.work          # Work
source $DOTFILES/.zsh-functions # Functions

# -----------------------------------------------------------------------------
# Plugins
# -----------------------------------------------------------------------------

# Init fasd aka z
fasd_cache="$HOME/.fasd-init-bash"
if [ "$(command -v fasd)" -nt "$fasd_cache" -o ! -s "$fasd_cache" ]; then
  fasd --init auto posix-alias zsh-completions zsh-hook zsh-wcomp >| "$fasd_cache"
fi
source "$fasd_cache"
unset fasd_cache

# Init scmpuff
eval "$(scmpuff init -s --aliases=false)"

# Quickly Switch between vim and shell by hitting CTRL-Z
fancy-ctrl-z () {
  if [[ $#BUFFER -eq 0 ]]; then
    BUFFER="fg"
    zle accept-line
  else
    zle push-input
    zle clear-screen
  fi
}
zle -N fancy-ctrl-z
bindkey '^Z' fancy-ctrl-z

# Source fzf zsh extension
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
