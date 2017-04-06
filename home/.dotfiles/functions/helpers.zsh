#!/usr/bin/env bash

# Check if a command exists
# Example
# if _command_exists nvim; then echo 'Nvim exists!'; fi
# http://stackoverflow.com/a/3931779
_command_exists() {
  type "$1" &> /dev/null ;
}

# Echo red error message
# _error_msg "Error message"
_error_msg() {
  RED='\033[0;31m'
  NC='\033[0m' # No Color
  echo "${RED}$@${NC}"
}

# Echo yellow info message
# _error_msg "Error message"
_info_msg() {
  YELLOW='\033[0;33m'
  NC='\033[0m' # No Color
  echo "${YELLOW}ℹ: $@${NC}"
}

# Lazyloading programs so they wont slow down shell startup
# https://gist.github.com/QinMing/364774610afc0e06cc223b467abe83c0
_lazy_load() {
  # Act as a stub to another shell function/command. When first run, it will load the actual function/command then execute it.
  # E.g. This made my zsh load 0.8 seconds faster by loading `nvm` when "nvm", "npm" or "node" is used for the first time
  # $1: space separated list of alias to release after the first load
  # $2: file to source
  # $3: name of the command to run after it's loaded
  # $4+: argv to be passed to $3
  GREY='\033[01;30m'
  NC='\033[0m'
  echo "${GREY}Lazy loading $1 ...${NC}"

  # $1.split(' ') using the s flag. In bash, this can be simply ($1) #http://unix.stackexchange.com/questions/28854/list-elements-with-spaces-in-zsh
  # Single line won't work: local names=("${(@s: :)${1}}"). Due to http://stackoverflow.com/questions/14917501/local-arrays-in-zsh   (zsh 5.0.8 (x86_64-apple-darwin15.0))
  local -a names
  if [[ -n "$ZSH_VERSION" ]]; then
    names=("${(@s: :)${1}}")
  else
    names=($1)
  fi
  unalias "${names[@]}"
  . $2
  shift 2
  $*
}

_group_lazy_load() {
  local script
  script=$1
  shift 1
  for cmd in "$@"; do
    alias $cmd="_lazy_load \"$*\" $script $cmd"
  done
}

